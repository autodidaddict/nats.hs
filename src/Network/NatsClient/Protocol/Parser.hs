{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.NatsClient.Protocol.Parser where

import Control.Applicative (optional)
import Control.Monad (guard)
import Data.Aeson (eitherDecode)
import Data.ByteString.Conversion
import Data.Text (Text)
import Data.Void
import Network.NatsClient.Protocol.Types (Header (..), ProtocolMessage (..))
import Text.Megaparsec
  ( MonadParsec (takeP, try),
    Parsec,
    many,
    manyTill,
    some,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    crlf,
    hspace1,
    numberChar,
    printChar,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

integer :: Parser Integer
integer = read <$> some numberChar

parseMessage :: Parser ProtocolMessage
parseMessage =
  pMsgMessage
    <|> pInfoMessage
    <|> pConnectMessage
    <|> pPubMessage
    <|> pSubMessage
    <|> pUnsubMessage
    <|> pHeaderPubMessage
    <|> pHeaderMsgMessage
    <|> pOkMessage
    <|> pErrMessage
    <|> pPingMessage
    <|> pPongMessage

pConnectMessage :: Parser ProtocolMessage
pConnectMessage = do
  _ <- string "CONNECT" <* hspace1
  rest <- many printChar <* crlf
  case eitherDecode $ toByteString rest of
    Right a -> return ConnectMessage {settings = Just a, error = Nothing}
    Left e -> return ConnectMessage {settings = Nothing, error = Just e}

pInfoMessage :: Parser ProtocolMessage
pInfoMessage = do
  _ <- string "INFO" <* hspace1
  rest <- many printChar <* crlf
  case eitherDecode $ toByteString rest of
    Right a -> return InfoMessage {info = Just a, error = Nothing}
    Left e -> return InfoMessage {info = Nothing, error = Just e}

pPingMessage :: Parser ProtocolMessage
pPingMessage = do
  _ <- string "PING" <* crlf
  return PingMessage

pPongMessage :: Parser ProtocolMessage
pPongMessage = do
  _ <- string "PONG" <* crlf
  return PongMessage

pOkMessage :: Parser ProtocolMessage
pOkMessage = do
  _ <- string "+OK" <* crlf
  return OkMessage

pErrMessage :: Parser ProtocolMessage
pErrMessage = do
  _ <- string "-ERR" <* hspace1
  reason <- singleQuotedString
  return ErrMessage {..}

pMsgMessage :: Parser ProtocolMessage
pMsgMessage = do
  _ <- string "MSG" <* hspace1
  subject <- validSubject <* hspace1
  sid <- many alphaNumChar <* hspace1
  reply <- try (fmap Just validSubject <* hspace1) <|> return Nothing
  len <- integer <* crlf
  payload <- optional $ do
    guard (len /= 0)
    takeP (Just "payload byte") (fromIntegral len) <* crlf

  return $ MsgMessage {..}

pPubMessage :: Parser ProtocolMessage
pPubMessage = do
  _ <- string "PUB" <* hspace1
  subject <- validSubject <* hspace1
  reply <- try (fmap Just validSubject <* hspace1) <|> return Nothing
  len <- integer <* crlf
  payload <- optional $ do
    guard (len /= 0)
    takeP (Just "payload byte") (fromIntegral len) <* crlf
  return PubMessage {..}

pSubMessage :: Parser ProtocolMessage
pSubMessage = do
  _ <- string "SUB" <* hspace1
  subject <- validSubject <* hspace1 <?> "subject"
  queueGroup <- try (fmap Just validSubject <* hspace1) <|> return Nothing
  sid <- many alphaNumChar <* crlf
  return SubMessage {..}

pUnsubMessage :: Parser ProtocolMessage
pUnsubMessage = do
  _ <- string "UNSUB" <* hspace1
  sid <- many alphaNumChar
  maxMessages <- try (hspace1 *> fmap Just integer) <|> return Nothing
  _ <- crlf
  return UnsubMessage {..}

pHeaderPubMessage :: Parser ProtocolMessage
pHeaderPubMessage = do
  _ <- string "HPUB" <* hspace1
  subject <- validSubject <* hspace1 <?> "subject"
  (reply, headerBytes, totalBytes) <- try pHeaderMsgTailNoReply <|> pHeaderMsgTailReply
  messageHeaders <- pHeaders
  payload <- optional $ do
    guard (totalBytes /= 0)
    takeP (Just "payload byte") (fromIntegral totalBytes) <* crlf
  return HeaderPubMessage {..}

pHeaderMsgMessage :: Parser ProtocolMessage
pHeaderMsgMessage = do
  _ <- string "HMSG" <* hspace1
  subject <- validSubject <* hspace1 <?> "subject"
  sid <- many alphaNumChar <* hspace1 <?> "sid"
  (reply, headerBytes, totalBytes) <- try pHeaderMsgTailNoReply <|> pHeaderMsgTailReply
  messageHeaders <- pHeaders <?> "headers"
  payload <- optional $ do
    guard (totalBytes /= 0)
    takeP (Just "payload byte") (fromIntegral totalBytes) <* crlf
  return $ HeaderMsgMessage {..}

pHeaders :: Parser [Header]
pHeaders = do
  many pHeader <* crlf

pHeader :: Parser Header
pHeader = do
  key <- validSubject <* char ':'
  value <- validSubject <* crlf
  return Header {..}

pHeaderMsgTailNoReply :: Parser (Maybe string, Integer, Integer)
pHeaderMsgTailNoReply = do
  headerBytes <- integer <* hspace1 <?> "header bytes"
  totalBytes <- integer <* crlf <?> "total bytes"
  return (Nothing, headerBytes, totalBytes)

pHeaderMsgTailReply :: Parser (Maybe String, Integer, Integer)
pHeaderMsgTailReply = do
  reply <- fmap Just validSubject <* hspace1
  headerBytes <- integer <* hspace1 <?> "header bytes"
  totalBytes <- integer <* crlf <?> "total bytes"
  return (reply, headerBytes, totalBytes)

validSubjectCharacter :: Parser Char
validSubjectCharacter = alphaNumChar <|> char '_' <|> char '.' <|> char '>' <|> char '*'

validSubject :: Parser String
validSubject = do
  c <- validSubjectCharacter
  cs <- many validSubjectCharacter
  return (c : cs)

singleQuotedString :: Parser String
singleQuotedString = char '\'' *> manyTill L.charLiteral (char '\'')