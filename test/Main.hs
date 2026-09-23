module Main (main) where

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import XML.Formatter
import XML.Parser
import XML.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Golden Tests"
        [ testGroup
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
                , goldenVsString
                    "ElementParseNested"
                    "test/golden/parseElementNested.golden"
                    (pure $ pack $ show $ parseElement "<book><ciao></ciao></book>")
                , goldenVsString
                    "ElementParseNestedSiblings"
                    "test/golden/parseElementNestedSiblings.golden"
                    (pure $ pack $ show $ parseElement "<book><ciao></ciao><hello></hello></book>")
                , goldenVsString
                    "ElementParseNestedSiblingsWithContent"
                    "test/golden/parseElementNestedSiblingsWithContent.golden"
                    (pure $ pack $ show $ parseElement "<book><ciao>wassup</ciao><hello>ok</hello></book>")
                , goldenVsString
                    "ElementParseDeeplyNested"
                    "test/golden/parseElementDeeplyNested.golden"
                    (pure $ pack $ show $ parseElement "<book><chapter><section></section></chapter></book>")
                , goldenVsString
                    "ElementParseNestedWithAttrsAndContent"
                    "test/golden/parseElementNestedWithAttrsAndContent.golden"
                    (pure $ pack $ show $ parseElement "<book id=\"123\"><chapter name=\"intro\">Hello</chapter></book>")
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
            , testGroup
                "Whitespace Parsing Tests"
                [ goldenVsString
                    "MultipleSpacesBetweenAttrs"
                    "test/golden/whitespaceMultipleSpacesBetweenAttrs.golden"
                    (pure $ pack $ show $ parseElement "<book id=\"123\"     genre=\"fiction\"></book>")
                , goldenVsString
                    "TabsBetweenAttrs"
                    "test/golden/whitespaceTabsBetweenAttrs.golden"
                    (pure $ pack $ show $ parseElement "<book id=\"123\"\t\tgenre=\"fiction\"></book>")
                , goldenVsString
                    "NewlinesBetweenAttrs"
                    "test/golden/whitespaceNewlinesBetweenAttrs.golden"
                    (pure $ pack $ show $ parseElement "<book id=\"123\"\n    genre=\"fiction\"></book>")
                , goldenVsString
                    "WhitespaceBetweenNestedElements"
                    "test/golden/whitespaceBetweenNestedElements.golden"
                    (pure $ pack $ show $ parseElement "<book>   <chapter></chapter>   </book>")
                , goldenVsString
                    "NewlineIndentationBetweenNestedElements"
                    "test/golden/whitespaceNewlineIndentation.golden"
                    (pure $ pack $ show $ parseElement "<book>\n    <chapter></chapter>\n</book>")
                , goldenVsString
                    "WhitespaceBeforeText"
                    "test/golden/whitespaceBeforeText.golden"
                    (pure $ pack $ show $ parseElement "<book>   Hello</book>")
                , goldenVsString
                    "WhitespaceAfterText"
                    "test/golden/whitespaceAfterText.golden"
                    (pure $ pack $ show $ parseElement "<book>Hello   </book>")
                , goldenVsString
                    "WhitespaceAroundText"
                    "test/golden/whitespaceAroundText.golden"
                    (pure $ pack $ show $ parseElement "<book>   Hello   </book>")
                , goldenVsString
                    "PrettyPrintedNestedElements"
                    "test/golden/whitespacePrettyPrintedNested.golden"
                    (pure $ pack $ show $ parseElement "<book>\n    <chapter>\n        <section></section>\n    </chapter>\n</book>")
                ]
            , testGroup
                "Formatting Tests"
                [ goldenVsString
                    "FormatElement"
                    "test/golden/formatElement.golden"
                    ( pure $
                        pack $
                            TL.unpack $
                                toLazyText $
                                    formatElement $
                                        Element (QName "book") [] (Text "")
                    )
                , goldenVsString
                    "FormatElementWithAttrs"
                    "test/golden/formatElementWithAttrs.golden"
                    ( pure $
                        pack $
                            TL.unpack $
                                toLazyText $
                                    formatElement $
                                        Element
                                            (QName "book")
                                            [ Attr "id" "123"
                                            , Attr "genre" "fiction"
                                            ]
                                            (Text "")
                    )
                , goldenVsString
                    "FormatElements"
                    "test/golden/formatElements.golden"
                    ( pure $
                        pack $
                            TL.unpack $
                                toLazyText $
                                    formatElements $
                                        [ Element
                                            (QName "book")
                                            [Attr "id" "123"]
                                            (Text "ciao")
                                        , Element
                                            (QName "next")
                                            [Attr "id" "123"]
                                            (Text "thing")
                                        ]
                    )
                ]
            ]
        ]
