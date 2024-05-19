{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Users where

import Control.Monad.Except
  ( MonadError (throwError),
    MonadIO (liftIO),
    runExceptT,
  )
import qualified Crypto.Hash.SHA256 as H
import qualified Crypto.JOSE as Jose
import Crypto.JWT (JWK)
import qualified Crypto.JWT as Jose
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.ByteString.Lazy.UTF8 as BLU (toString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Database
  ( dbGetUserByLogin,
    dbGetUserByUsername,
    dbRegisterUser,
  )
import Database.Persist.Postgresql (ConnectionString)
import Schema
  ( UserEntity
      ( UserEntity,
        userEntityAvailableChips,
        userEntityChipsInPlay,
        userEntityCreatedAt,
        userEntityEmail,
        userEntityPassword,
        userEntityUsername
      ),
  )
import Servant
  ( Handler,
    NoContent (..),
    ServerError (errBody),
    err401,
    err404,
  )
import Servant.Auth.Server (JWTSettings, makeJWT)
import Types
  ( Login (..),
    RedisConfig,
    Register (..),
    ReturnToken (..),
    UserProfile (..),
    Username (..),
  )

fetchUserProfileHandler :: ConnectionString -> Username -> Handler UserProfile
fetchUserProfileHandler connString username' = do
  maybeUser <- liftIO $ dbGetUserByUsername connString username'
  case maybeUser of
    Nothing -> throwError err404
    Just UserEntity {..} ->
      return $
        UserProfile
          { proEmail = userEntityEmail,
            proAvailableChips = userEntityAvailableChips,
            proChipsInPlay = userEntityChipsInPlay,
            proUsername = Username userEntityUsername,
            proUserCreatedAt = userEntityCreatedAt
          }

hashPassword :: Text -> Text
hashPassword password = T.pack $ C.unpack $ H.hash $ encodeUtf8 password

signToken :: JWTSettings -> Username -> Handler ReturnToken
signToken jwtSettings username' = do
  eToken <- liftIO $ makeJWT username' jwtSettings expiryTime
  case eToken of
    Left e -> throwError $ unAuthErr $ BS.pack $ show eToken
    Right token ->
      return $
        ReturnToken
          { access_token = T.pack (BLU.toString token),
            refresh_token = "",
            expiration = 9999999,
            ..
          }
  where
    expiryTime = Nothing
    unAuthErr e = err401 {errBody = e}

loginHandler :: JWTSettings -> ConnectionString -> Login -> Handler ReturnToken
loginHandler jwtSettings connString l@Login {..} = do
  maybeUser <- liftIO $ dbGetUserByLogin connString loginWithHashedPswd
  case maybeUser of
    Nothing -> throwError unAuthErr
    Just u@UserEntity {..} ->
      signToken jwtSettings (Username userEntityUsername)
  where
    unAuthErr = err401 {errBody = "Incorrect email or password"}
    loginWithHashedPswd =
      Login {loginPassword = hashPassword loginPassword, ..}

-- when we register new user we check to see if email and username are already taken
-- if they are then the exception will be propagated to the client
registerUserHandler ::
  JWTSettings ->
  ConnectionString ->
  RedisConfig ->
  Register ->
  Handler ReturnToken
registerUserHandler jwtSettings connString redisConfig Register {..} = do
  currTime <- liftIO getCurrentTime
  let hashedPassword = hashPassword newUserPassword
      (Username username) = newUsername
      newUser =
        UserEntity
          { userEntityUsername = username,
            userEntityEmail = newUserEmail,
            userEntityPassword = hashedPassword,
            userEntityAvailableChips = 3000,
            userEntityChipsInPlay = 0,
            userEntityCreatedAt = currTime
          }
  registrationResult <-
    liftIO $
      runExceptT $
        dbRegisterUser connString redisConfig newUser
  case registrationResult of
    Left err -> throwError $ err401 {errBody = CL.pack $ T.unpack err}
    _ -> signToken jwtSettings newUsername

getLobbyHandler :: JWTSettings -> ConnectionString -> RedisConfig -> Handler NoContent
getLobbyHandler _ _ _ = do
  return NoContent
