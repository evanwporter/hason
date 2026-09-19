{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser (
    linenumber,
    parseElement,
    parseOpenTag,
    parseAttrs,
    parseAttrValue,
    LString,
    LChar,
    Line,
    XmlSource (..),
) where

import Data.Char (isAlpha)
import Types

class XmlSource s where
    uncons :: s -> Maybe (Char, s)

instance XmlSource String where
    uncons (c : s) = Just (c, s)
    uncons "" = Nothing

{- | Converts a source string into a list of line-numbered characters.

Starting from the given line number, each character is paired with
its line number. Like so 'ab\nc' (1, 'a'), (1, 'b'), (2, 'c')
-}
linenumber :: (XmlSource s) => Integer -> s -> LString
linenumber n s = case uncons s of
    Nothing -> []
    Just ('\r', s') -> case uncons s' of
        Just ('\n', s'') -> next s''
        _ -> next s'
    Just ('\n', s') -> next s'
    Just (c, s') -> (n, c) : linenumber n s'
  where
    next s' = n' `seq` ((n, '\n') : linenumber n' s') where n' = n + 1

type Line = Integer

type LChar = (Line, Char)

type LString = [LChar]

consumeChar :: Char -> String -> Either String String
consumeChar expected [] =
    Left ("Unexpected end of input. Expected: " ++ [expected])
consumeChar expected (x : xs)
    | x == expected = Right xs
    | otherwise = Left ("Expected " ++ [expected] ++ ", but found " ++ [x])

parseName :: String -> Either String (QName, String)
parseName [] = Left "Unexpected end of input while parsing name"
parseName s =
    let (qname, rest) = span isAlpha s
     in Right (QName{qName = qname}, rest)

parseAttrKey :: String -> Either String (String, String)
parseAttrKey s = Right $ span isAlpha s

parseAttrValue :: String -> Either String (String, String)
parseAttrValue s = do
    rest1 <- consumeChar '"' s -- Start with opening quote
    let (value, rest2) = span (/= '"') rest1 -- Parse until closing quote
    rest3 <- consumeChar '"' rest2 -- Consume closing quote
    return (value, rest3)

parseAttrs :: String -> Either String ([Attr], String)
parseAttrs [] = Left "Unexpected end of input while parsing attributes"
parseAttrs input = do
    (key, rest1) <- parseAttrKey input
    rest2 <-
        case consumeChar '=' rest1 of
            Left err ->
                Left ("Expected '=' after attribute key '" ++ key ++ "': " ++ err)
            Right rest ->
                Right rest
    (value, rest3) <- parseAttrValue rest2
    case rest3 of
        ('>' : _) -> return ([Attr{attrKey = key, attrValue = value}], rest3)
        (' ' : rest4) -> do
            (moreAttrs, rest5) <- parseAttrs rest4
            return (Attr{attrKey = key, attrValue = value} : moreAttrs, rest5)
        _ -> Left "Expected '>' or another attribute"

parseOpenTag :: String -> Either String (OpenTag, String)
parseOpenTag [] = Left "Unexpected end of input while parsing an opening tag"
parseOpenTag input = do
    rest1 <- consumeChar '<' input
    (name, rest2) <- parseName rest1
    case rest2 of
        '>' : rest3 -> do
            return (OpenTag{openTagName = name, openTagAttrs = []}, rest3)
        ' ' : rest3 -> do
            (attrs, rest4) <- parseAttrs rest3
            rest5 <- consumeChar '>' rest4
            return (OpenTag{openTagName = name, openTagAttrs = attrs}, rest5)
        _ -> Left "Expected '>' or another attribute"

parseCloseTag :: String -> Either String (CloseTag, String)
parseCloseTag [] = Left "Unexpected end of input while parsing a closing tag"
parseCloseTag input = do
    rest1 <- consumeChar '<' input
    rest2 <- consumeChar '/' rest1
    (name, rest3) <- parseName rest2
    rest4 <- consumeChar '>' rest3
    return (CloseTag{closeTagName = name}, rest4)

{- | At every element I have the option to:
(1) Parse the next set of characters as an element
  (a) OpenTag
  (b) CloseTag
(2) Parse the next set of characters as a content string
-}
parseElement :: String -> Either String (Element, String)
parseElement [] = Left "Unexpected end of input while parsing an element"
parseElement input = do
    -- TODO: Handle lists of XML elements

    -- OpenTag
    (openTag, rest1) <- parseOpenTag input
    (element, rest2) <- case rest1 of
        '<' : '/' : _ ->
            -- matches a closing tag--presumably the closing tag corresponding to
            -- `openTag`
            return
                ( Element
                    { ename = openTagName openTag
                    , eattrs = openTagAttrs openTag
                    , econtent = Text ""
                    }
                , rest1
                )
        '<' : _ -> do
            -- matches another element
            -- continue parsing from rest1
            (element, rest3) <- parseElement rest1
            let ret =
                    Element
                        { ename = openTagName openTag
                        , eattrs = openTagAttrs openTag
                        , econtent = Elem element
                        }
            return (ret, rest3)
        _ -> do
            -- matches a string
            let (cont, rest3) = span isAlpha rest1
                element =
                    Element
                        { ename = openTagName openTag
                        , eattrs = openTagAttrs openTag
                        , econtent = Text cont
                        }
            return (element, rest3)
    (_, rest3) <- parseCloseTag rest2
    return (element, rest3)
