module Network.NatsClient.Protocol.Types where

import Data.Text (Text)
import Network.NatsClient.Protocol.Info (ServerInfo(..))
import Network.NatsClient.Protocol.Connect (ConnectSettings(..))

data ProtocolMessage
  = InfoMessage
      { info :: Maybe ServerInfo,
        error :: Maybe String
      }
  | ConnectMessage
      { settings :: Maybe ConnectSettings,
        error :: Maybe String
      }
  | MsgMessage
      { subject :: String,
        reply :: Maybe String,
        sid :: String,
        len :: Integer,
        payload :: Maybe Text
      }
  | HeaderMsgMessage
      { subject :: String,
        reply :: Maybe String,
        sid :: String,
        headerBytes :: Integer,
        totalBytes :: Integer,
        messageHeaders :: [Header],
        payload :: Maybe Text
      }
  | PubMessage
      { subject :: String,
        reply :: Maybe String,
        len :: Integer,
        payload :: Maybe Text
      }
  | HeaderPubMessage
      { subject :: String,
        reply :: Maybe String,
        headerBytes :: Integer,
        totalBytes :: Integer,
        messageHeaders :: [Header],
        payload :: Maybe Text
      }
  | SubMessage
      { subject :: String,
        queueGroup :: Maybe String,
        sid :: String
      }
  | UnsubMessage
      { sid :: String,
        maxMessages :: Maybe Integer
      }
  | PingMessage
  | PongMessage
  | OkMessage
  | ErrMessage
      { reason :: String
      }
  deriving (Eq, Show)

data Header = Header {key :: String, value :: String} deriving (Eq, Show)
