{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API where

import Control.Concurrent (forkIO)
import Control.Lens ((&), (<>~))
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Crypto.JOSE.JWK (JWK)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Database.Persist.Postgresql (ConnectionString)
import Debug.Trace (traceShow)
import GHC.Generics (Generic)
import GHC.TypeLits
  ( ErrorMessage (Text),
    KnownSymbol,
    Symbol,
    TypeError,
    symbolVal,
  )
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant
  ( Application,
    Context (EmptyContext, (:.)),
    Get,
    JSON,
    NoContent,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    err401,
    serveWithContext,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Auth.Server
  ( Auth,
    AuthResult (Authenticated),
    Cookie,
    JWT,
    JWTSettings,
    ThrowAll (throwAll),
    defaultCookieSettings,
    defaultJWTSettings,
    fromSecret,
  )
import Servant.Foreign
  ( Arg (Arg, _argName, _argType),
    HasForeign (..),
    HasForeignType (..),
    HeaderArg (HeaderArg),
    PathSegment (PathSegment),
    reqHeaders,
  )
import Servant.Server
  ( Application,
    Context (EmptyContext, (:.)),
    Server,
    err401,
    serveWithContext,
  )
import System.Environment (getArgs)
import Types
  ( Login,
    RedisConfig,
    Register,
    ReturnToken,
    UserProfile,
    Username,
  )
import Users
  ( fetchUserProfileHandler,
    getLobbyHandler,
    loginHandler,
    registerUserHandler,
  )

type API auths =
  (Servant.Auth.Server.Auth auths Username :> ProtectedUsersAPI)
    :<|> UnprotectedUsersAPI

type UnprotectedUsersAPI =
  "login" :> ReqBody '[JSON] Login :> Post '[JSON] ReturnToken
    :<|> "register" :> ReqBody '[JSON] Register :> Post '[JSON] ReturnToken
    :<|> "lobby" :> Get '[JSON] NoContent

type ProtectedUsersAPI =
  "profile" :> Get '[JSON] UserProfile

api :: Proxy (API '[JWT])
api = Proxy :: Proxy (API '[JWT])

protectedUsersApi :: Proxy ProtectedUsersAPI
protectedUsersApi = Proxy :: Proxy ProtectedUsersAPI

unprotectedUsersApi :: Proxy UnprotectedUsersAPI
unprotectedUsersApi = Proxy :: Proxy UnprotectedUsersAPI

app :: BS.ByteString -> ConnectionString -> RedisConfig -> Application
app secretKey connString redisConfig = addMiddleware $ serveWithAuth secretKey connString redisConfig

type Token = String

type family TokenHeaderName xs :: Symbol where
  TokenHeaderName (Cookie ': xs) = "X-XSRF-TOKEN"
  TokenHeaderName (JWT ': xs) = "Authorization"
  TokenHeaderName (x ': xs) = TokenHeaderName xs
  TokenHeaderName '[] = TypeError (Text "Neither JWT nor cookie auth enabled")

instance
  ( TokenHeaderName auths ~ header,
    KnownSymbol header,
    HasForeignType lang ftype Token,
    HasForeign lang ftype sub
  ) =>
  HasForeign lang ftype (Auth auths a :> sub)
  where
  type Foreign ftype (Auth auths a :> sub) = Foreign ftype sub

  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
    where
      arg =
        Arg
          { _argName = PathSegment . T.pack $ symbolVal @header Proxy,
            _argType = token
          }
      token = typeFor lang (Proxy @ftype) (Proxy @Token)
      subP = Proxy @sub

-- Adds JWT Authentication to our server
serveWithAuth :: BS.ByteString -> ConnectionString -> RedisConfig -> Application
serveWithAuth secretKey c r =
  serveWithContext api cfg (server jwtCfg c r)
  where
    jwk = fromSecret secretKey
    jwtCfg = defaultJWTSettings jwk
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    api = Proxy :: Proxy (API '[JWT]) -- API is a type synonym for our api - type is now concrete

server :: JWTSettings -> ConnectionString -> RedisConfig -> Server (API '[JWT])
server j c r = protectedUsersServer j c r :<|> unprotectedUsersServer j c r

unprotectedUsersServer :: JWTSettings -> ConnectionString -> RedisConfig -> Server UnprotectedUsersAPI
unprotectedUsersServer jwtSettings connString redisConfig =
  loginHandler jwtSettings connString
    :<|> registerUserHandler jwtSettings connString redisConfig
    :<|> getLobbyHandler jwtSettings connString redisConfig

protectedUsersServer :: JWTSettings -> ConnectionString -> RedisConfig -> AuthResult Username -> Server ProtectedUsersAPI
protectedUsersServer j c r (Authenticated username') = fetchUserProfileHandler c username'
protectedUsersServer _ _ _ er = traceShow er (throwAll err401)

type Middleware = Application -> Application

addMiddleware :: Application -> Application
addMiddleware = logStdoutDev . cors (const $ Just policy) . (provideOptions api)
  where
    corsReqHeaders = ["content-type", "Access-Control-Allow-Origin", "POST", "GET", "*"]
    policy = simpleCorsResourcePolicy {corsRequestHeaders = corsReqHeaders}
