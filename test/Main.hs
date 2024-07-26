module Main (main) where

import Test.HUnit

tests = TestList [
    TestLabel "test2" parsePing
    ]

main :: IO ()
main = runTestTTAndExit tests

parsePing = TestCase $ assertBool "faily" False