module Main (main) where

import Data.ByteString.Lazy.Char8 (pack)
import Parser
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Golden Tests"
        [ testGroup
            "Line Number Tests"
            [ goldenVsString
                "LineNumber"
                "test/golden/linenumber.golden"
                (pure $ pack $ show $ linenumber 1 "hello\nworld\n\nThis\nis a thing   \n   \r\n That's happening")
            ]
        , testGroup
            "Parsing Tests"
            [ testGroup
                "Element Parsing Tests"
                [ goldenVsString
                    "ElementParse"
                    "test/golden/parseElement.golden"
                    (pure $ pack $ show $ parseElement "<book></book>")
                , goldenVsString
                    "ElementParseWithAttrs"
                    "test/golden/parseElementWithAttrs.golden"
                    (pure $ pack $ show $ parseElement "<book id=\"123\" genre=\"fiction\"></book>")
                , goldenVsString
                    "ElementParseWithContent"
                    "test/golden/parseElementWithContent.golden"
                    (pure $ pack $ show $ parseElement "<book>Content</book>")
                , goldenVsString
                    "ElementParseWithAttrsAndContent"
                    "test/golden/parseElementWithAttrsAndContent.golden"
                    (pure $ pack $ show $ parseElement "<book id=\"123\" genre=\"fiction\">Content</book>")
                ]
            , testGroup
                "OpenTag Parsing Tests"
                [ goldenVsString
                    "OpenTagParse"
                    "test/golden/parseOpenTag.golden"
                    (pure $ pack $ show $ parseOpenTag "<book>")
                , goldenVsString
                    "OpenTagParseWithAttrs"
                    "test/golden/parseOpenTagWithAttrs.golden"
                    (pure $ pack $ show $ parseOpenTag "<book id=\"123\" genre=\"fiction\">")
                ]
            , testGroup
                "Attr Parsing Tests"
                [ goldenVsString
                    "AttrParse"
                    "test/golden/parseAttrs.golden"
                    (pure $ pack $ show $ parseAttrs "genre=\"fiction\">")
                , goldenVsString
                    "AttrValue"
                    "test/golden/parseAttrsValue.golden"
                    (pure $ pack $ show $ parseAttrValue "\"fiction\">")
                ]
            ]
        ]
