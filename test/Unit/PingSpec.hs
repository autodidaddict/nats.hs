{-# LANGUAGE OverloadedStrings #-}

module PingSpec (spec) where

import Test.Hspec
import           Control.Monad
import           Text.Printf

import Protocol.Types
import Protocol.Transformers
import Protocol.Parser 
import Text.Megaparsec 
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)


spec :: Spec
spec = do
  parserCases
  transformerCases  

explicitParserCases :: [(Text, ProtocolMessage)]
explicitParserCases = [("PING\r\n", PingMessage)]

explicitTransformerCases :: [(ProtocolMessage, Text)]
explicitTransformerCases = map (\(a,b) -> (b,a)) explicitParserCases

parserCases :: SpecWith ()
parserCases = parallel $ do
  describe "generic parser" $ do
    forM_ explicitParserCases $ \(input, want) -> do
      it (printf "correctly parses explicit case %s" (show input)) $ do
        let output = parse parseMessage "" input
        output `shouldBe` Right want

transformerCases :: SpecWith ()
transformerCases = parallel $ do
  describe "PING transformer" $ do
    forM_ explicitTransformerCases $ \(input, want) -> do
      it (printf"correctly transforms %s" (show input)) $ do
        transform input `shouldBe` encodeUtf8 want