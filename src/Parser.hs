{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser (linenumber, LString, LChar, Line, XmlSource (..)) where

import Types (Element)

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
