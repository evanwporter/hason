module Main (main) where

import Data.ByteString.Lazy.Char8 (pack)
import Parser (linenumber, parseAttrValue, parseAttrs, parseElement)
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
        , goldenVsString
            "ElementParse"
            "test/golden/parseElement.golden"
            (pure $ pack $ show $ parseElement "<book>")
        , goldenVsString
            "ElementParseWithAttrs"
            "test/golden/parseElementWithAttrs.golden"
            (pure $ pack $ show $ parseElement "<book id=\"123\" genre=\"fiction\">")
        , goldenVsString
            "AttrParse"
            "test/golden/parseAttrs.golden"
            (pure $ pack $ show $ parseAttrs "genre=\"fiction\">")
        , goldenVsString
            "AttrValue"
            "test/golden/parseAttrsValue.golden"
            (pure $ pack $ show $ parseAttrValue "\"fiction\">")
        ]
