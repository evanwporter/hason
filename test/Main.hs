module Main (main) where

import Data.ByteString.Lazy.Char8 (pack)
import Parser (linenumber)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Golden Tests"
    [ goldenVsString
        "LineNumber"
        "test/golden/linenumber.golden"
        (pure $ pack $ show $ linenumber 1 "hello\nworld\n\nThis\nis a thing   \n   \r\n That's happening")
    ]
