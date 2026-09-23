{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser (
    parseElement,
    parseOpenTag,
    parseAttrs,
    parseAttrValue,
) where

import Data.Char (isAlpha)
import Types

consumeChar :: Char -> String -> Either String String
consumeChar expected [] =
    Left ("Unexpected end of input. Expected: " ++ [expected])
consumeChar expected (x : xs)
    | x == expected = Right xs
    | otherwise = Left ("Expected " ++ [expected] ++ ", but found " ++ [x])

consumeWhitespace :: String -> String
consumeWhitespace input = dropWhile (\p -> elem p " \t\n\r") input

parseName :: String -> Either String (QName, String)
parseName [] = Left "Unexpected end of input while parsing name"
parseName s =
    -- consume the whitespace from the string then split up the string into
    -- alpha characters and first not alpha character
    let (qname, rest) = span isAlpha $ consumeWhitespace s
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
        (' ' : _) -> do
            -- Consume all the whitespace between the attributes and pass the result
            -- to the `parseAttrs` so we can parse the next attribute
            (moreAttrs, rest5) <- parseAttrs $ consumeWhitespace rest3
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
        ' ' : _ -> do
            -- TODO: Check if I need to match \n\t as valid spacing

            -- Consume all the whitespace between the name and the next attribute
            let rest3 = consumeWhitespace rest2

            -- Parse attributes
            (attrs, rest4) <- parseAttrs rest3
            rest5 <- consumeChar '>' rest4

            -- TODO: Don't strip whitespace if its Text content
            -- Parse all whitespace until next tag or piece of text
            let rest6 = consumeWhitespace rest5
            return (OpenTag{openTagName = name, openTagAttrs = attrs}, rest6)
        _ -> Left "Expected '>' or another attribute"

parseCloseTag :: String -> Either String (CloseTag, String)
parseCloseTag [] = Left "Unexpected end of input while parsing a closing tag"
parseCloseTag input = do
    rest1 <- consumeChar '<' input
    rest2 <- consumeChar '/' rest1
    (name, rest3) <- parseName rest2
    rest4 <- consumeChar '>' rest3
    return (CloseTag{closeTagName = name}, rest4)

parseElementChildren :: String -> Either String ([Element], String)
parseElementChildren input = case input of
    '<' : '/' : _ ->
        return ([], input)
    '<' : _ -> do
        (element, rest1) <- parseElement input
        (elements, rest2) <- parseElementChildren $ consumeWhitespace rest1
        return (element : elements, rest2)
    _ -> Left "Expected child element or closing tag"

parseElementBody :: OpenTag -> String -> Either String (Element, String)
parseElementBody openTag input = case input of
    '<' : '/' : _ ->
        -- matches a closing tag--presumably the closing tag corresponding to
        -- `openTag`
        return
            ( Element
                { ename = openTagName openTag
                , eattrs = openTagAttrs openTag
                , econtent = Text ""
                }
            , input
            )
    '<' : _ -> do
        -- matches another element
        -- continue parsing from input
        (elements, rest1) <- parseElementChildren input
        let ret =
                Element
                    { ename = openTagName openTag
                    , eattrs = openTagAttrs openTag
                    , econtent = Elem elements
                    }
        return (ret, rest1)
    _ -> do
        -- matches a string
        let (cont, rest1) = span isAlpha input
            rest2 = consumeWhitespace rest1
            element =
                Element
                    { ename = openTagName openTag
                    , eattrs = openTagAttrs openTag
                    , econtent = Text cont
                    }
        return (element, rest2)

{- | At every element I have the option to:
(1) Parse the next set of characters as an element
  (a) OpenTag
  (b) CloseTag
(2) Parse the next set of characters as a content string
-}
parseElement :: String -> Either String (Element, String)
parseElement [] = Left "Unexpected end of input while parsing an element"
parseElement input = do
    -- OpenTag
    (openTag, rest1) <- parseOpenTag input

    (element, rest2) <- parseElementBody openTag $ consumeWhitespace rest1

    -- CloseTag
    -- TODO: Check openTag and closeTag match
    (_, rest3) <- parseCloseTag rest2

    return (element, rest3)
