module Socket.Utils where

import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as X
import qualified Data.Text.Lazy.Encoding as D
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (UTCTime), secondsToDiffTime)
import Socket.Types (Lobby (..), MsgIn, MsgOut, Table, TableName)
import Text.Pretty.Simple (pPrint)
import Prelude

encodeMsgToJSON :: MsgOut -> Text
encodeMsgToJSON a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

encodeMsgX :: MsgIn -> Text
encodeMsgX a = T.pack $ show $ X.toStrict $ D.decodeUtf8 $ encode a

parseMsgFromJSON :: Text -> Maybe MsgIn
parseMsgFromJSON jsonTxt = decode $ C.pack $ T.unpack jsonTxt

parseMsgFromJSON' :: BS.ByteString -> Maybe MsgIn
parseMsgFromJSON' jsonTxt = decode $ C.fromStrict jsonTxt

getTimestamp :: UTCTime
getTimestamp = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

unLobby :: Lobby -> Map TableName Table
unLobby (Lobby lobby) = lobby
