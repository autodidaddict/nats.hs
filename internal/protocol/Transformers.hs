{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.Transformers where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding (encodeUtf8)
import Protocol.Types (Header (..), ProtocolMessage (..))
import Text.Printf (printf)
import Data.Aeson

class Transformer a where
  transform :: a -> BS.ByteString

instance Transformer ProtocolMessage where
  transform OkMessage = "OK\r\n"
  transform PingMessage = "PING\r\n"
  transform PongMessage = "PONG\r\n"
  transform ConnectMessage { settings } = do
    let f = encode settings
    foldr BS.append "" ["CONNECT ", BS.toStrict f, "\r\n"]
  transform InfoMessage { info } = do
    let f = encode info
    foldr BS.append "" ["INFO ", BS.toStrict f, "\r\n"]
  transform ErrMessage {..} =
    foldr BS.append "" ["-ERR", " ", "'", pack reason, "'\r\n"]
  transform PubMessage {..} =
    foldr
      BS.append
      ""
      [ "PUB",
        " ",
        pack subject,
        " ",
        collapseNothing (fmap pack reply) " ",
        pack $ printf "%v" len,
        "\r\n",
        collapseNothing (encodeUtf8 <$> payload) "",
        "\r\n"
      ]
  transform HeaderPubMessage {..} =
    let headers = headerString messageHeaders
     in foldr
          BS.append
          ""
          [ "HPUB",
            " ",
            pack subject,
            " ",
            collapseNothing (pack <$> reply) " ",
            pack $ printf "%v" headerBytes,
            " ",
            pack $ printf "%v" totalBytes,
            "\r\n",
            headers,
            collapseNothing (encodeUtf8 <$> payload) "",
            "\r\n"
          ]
  transform SubMessage {..} =
    foldr
      BS.append
      ""
      [ "SUB",
        " ",
        collapseNothing (pack <$> queueGroup) " ",
        pack sid,
        "\r\n"
      ]
  transform UnsubMessage {..} =
    foldr
      BS.append
      ""
      [ "UNSUB",
        " ",
        pack sid,
        collapseNothing (pack . printf " %v" <$> maxMessages) "",
        "\r\n"
      ]
  transform MsgMessage {..} =
    foldr
      BS.append
      ""
      [ "MSG",
        " ",
        pack subject,
        " ",
        pack sid,
        " ",
        collapseNothing (pack <$> reply) " ",
        pack $ printf "%v" len,
        "\r\n",
        collapseNothing (encodeUtf8 <$> payload) "",
        "\r\n"
      ]
  transform HeaderMsgMessage {..} =
    let headers = headerString messageHeaders
     in foldr
          BS.append
          ""
          [ "HMSG",
            " ",
            pack subject,
            " ",
            pack sid,
            " ",
            collapseNothing (pack <$> reply) " ",
            pack $ printf "%v" headerBytes,
            " ",
            pack $ printf "%v" totalBytes,
            "\r\n",
            headers,
            collapseNothing (encodeUtf8 <$> payload) "",
            "\r\n"
          ]  

collapseNothing :: Maybe BS.ByteString -> BS.ByteString -> BS.ByteString
collapseNothing mbs suffix = case mbs of
  Just a -> BS.append a suffix
  Nothing -> ""

headerString :: [Header] -> BS.ByteString
headerString hs =
  foldl
    BS.append
    ""
    [ "NATS/1.0\r\n",
      BS.concat . map (\Header {key, value} -> foldr BS.append "" [pack key, ":", pack value, "\r\n"]) $ hs,
      "\r\n"
    ]