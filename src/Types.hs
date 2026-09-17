{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types (Element (..)) where

{- | Qualifying Name
TODO: Prefix and URI
-}
data QName = QName
    { qName :: String
    }
    deriving (Show)

{- | Content
<title>My Book</title>
"My Book" is the text content
-}
data Content
    = Elem Element
    | Text String
    deriving (Show)

{- | Attributes
Key-Value pairs
ie: <book id="123" category="fiction">
Key: id, value: 123
-}
data Attr = Attr
    { attrKey :: String
    , attrValue :: String
    }
    deriving (Show)

data Element = Element
    { name :: QName
    , attrs :: [Attr]
    }
    deriving (Show)
