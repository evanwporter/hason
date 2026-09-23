{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module XML.Formatter where

import Data.Text.Lazy.Builder
import XML.Types

formatAttrs :: [Attr] -> Builder
formatAttrs [] = mempty
formatAttrs (attr : attrs) =
    singleton ' '
        <> fromString (attrKey attr)
        <> fromString "=\""
        <> fromString (attrValue attr)
        <> formatAttrs attrs

formatElement :: Element -> Builder
formatElement element =
    singleton '<'
        <> fromString (qName $ ename element)
        <> formatAttrs (eattrs element)
        <> singleton '>'
        <> formatContent (econtent element)
        <> fromString "</"
        <> fromString (qName $ ename element)
        <> singleton '>'

formatElements :: [Element] -> Builder
formatElements [] = mempty
formatElements (element : elements) =
    formatElement element <> formatElements elements

formatContent :: Content -> Builder
formatContent content = case content of
    Text text -> fromString text
    Elem elements -> fromString "\n\t" <> formatElements elements <> fromString "\n"
