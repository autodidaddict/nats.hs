{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec

spec :: Spec
spec = describe "dummy test" $ do
    it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` (23 :: Int)

