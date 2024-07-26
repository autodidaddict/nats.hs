{-# LANGUAGE OverloadedStrings #-}

module PingSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Protocol.Parser (parseMessage)
import Protocol.Transformers (Transformer (transform))
import Protocol.Types (ProtocolMessage (PingMessage))
import Test.Hspec
  ( Spec,
    SpecWith,
    describe,
    it,
    parallel,
    shouldBe,
  )
import Text.Megaparsec (parse)
import Text.Printf (printf)

spec :: Spec
spec = do
  doParserCases
  doTransformerCases

parserCases :: [(Text, ProtocolMessage)]
parserCases = [("PING\r\n", PingMessage)]

transformerCases :: [(ProtocolMessage, Text)]
transformerCases = map (\(a, b) -> (b, a)) parserCases

doParserCases :: SpecWith ()
doParserCases = parallel $ do
  describe "PING parser" $ do
    forM_ parserCases $ \(input, want) -> do
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = parse parseMessage "" input
        output `shouldBe` Right want

doTransformerCases :: SpecWith ()
doTransformerCases = parallel $ do
  describe "PING transformer" $ do
    forM_ transformerCases $ \(input, want) -> do
      it (printf "correctly transforms %s" (show input)) $ do
        transform input `shouldBe` encodeUtf8 want