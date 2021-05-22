{-# LANGUAGE OverloadedStrings #-}
{-
  Unfortunately jwtSettingsToJwtValidationSettings is directly
  the servant-auth library internals the JWT is signed by the User API
  when a user registers or logs in so essentially this file has
  to currently duplicate the token verification logic that the servant-auth
  library hides internally so we can suthenticate socket connections
-}
{-# LANGUAGE RecordWildCards #-}

module Socket.Auth where

import Control.Monad.Except (runExceptT)
import Crypto.JOSE as Jose (decodeCompact)
import Crypto.JWT
  ( ClaimsSet,
    JWTError,
    decodeCompact,
    defaultJWTValidationSettings,
    verifyClaims,
  )
import Crypto.JWT as Jose
  ( ClaimsSet,
    JWTError,
    JWTValidationSettings,
    decodeCompact,
    defaultJWTValidationSettings,
    verifyClaims,
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (Either)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Postgresql
  ( ConnectionString,
    entityVal,
  )
import qualified Network.WebSockets as WS
import Servant.Auth.Server
  ( IsMatch (DoesNotMatch, Matches),
    JWTSettings (audienceMatches),
    defaultJWTSettings,
    fromSecret,
  )
import Text.Pretty.Simple (pPrint)
import Prelude

verifyJWT :: BS.ByteString -> BL.ByteString -> IO (Either JWTError ClaimsSet)
verifyJWT key jwt = runExceptT $ do
  jwt' <- decodeCompact jwt
  -- decode JWT
  verifyClaims jwtCfg jwk jwt'
  where
    jwk = fromSecret key
    jwtCfg = jwtSettingsToJwtValidationSettings $ defaultJWTSettings jwk

jwtSettingsToJwtValidationSettings :: JWTSettings -> Jose.JWTValidationSettings
jwtSettingsToJwtValidationSettings s =
  defaultJWTValidationSettings
    (toBool <$> audienceMatches s)
  where
    toBool Matches = True
    toBool DoesNotMatch = False
