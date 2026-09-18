{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser (linenumber, parseElement, parseAttrs, parseAttrValue, LString, LChar, Line, XmlSource (..)) where

import Data.Char (isAlpha)
import Types (Attr (..), Element (..), QName (..))

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

consumeChar :: Char -> String -> Maybe String
consumeChar _ [] = Nothing
consumeChar expected (x : xs)
    | x == expected = Just xs
    | otherwise = Nothing

parseName :: String -> Maybe (QName, String)
parseName [] = Nothing
parseName s =
    let (name, rest) = span isAlpha s
     in Just (QName{qName = name}, rest)

parseAttrKey :: String -> Maybe (String, String)
parseAttrKey s = Just $ span isAlpha s

parseAttrValue :: String -> Maybe (String, String)
parseAttrValue s = do
    rest1 <- consumeChar '"' s -- Start with opening quote
    let (value, rest2) = span (/= '"') rest1 -- Parse until closing quote
    rest3 <- consumeChar '"' rest2 -- Consume closing quote
    return (value, rest3)

parseAttrs :: String -> Maybe ([Attr], String)
parseAttrs [] = Nothing
parseAttrs input = do
    (key, rest1) <- parseAttrKey input
    rest2 <- consumeChar '=' rest1
    (value, rest3) <- parseAttrValue rest2
    case rest3 of
        ('>' : _) -> return ([Attr{attrKey = key, attrValue = value}], rest3)
        (' ' : rest4) -> do
            (moreAttrs, rest5) <- parseAttrs rest4
            return (Attr{attrKey = key, attrValue = value} : moreAttrs, rest5)
        _ -> Nothing

parseElement :: String -> Maybe (Element, String)
parseElement [] = Nothing
parseElement input = do
    rest1 <- consumeChar '<' input
    (ename, rest2) <- parseName rest1
    case rest2 of
        '>' : rest3 -> return (Element{name = ename, attrs = []}, rest3)
        ' ' : rest3 -> do
            (eattrs, rest4) <- parseAttrs rest3
            rest5 <- consumeChar '>' rest4
            return (Element{name = ename, attrs = eattrs}, rest5)
        _ -> Nothing
