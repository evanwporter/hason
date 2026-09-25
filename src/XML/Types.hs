{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module XML.Types where

-- | Qualifying Name
-- TODO: Prefix and URI
data QName = QName
  { qName :: String
  }
  deriving (Show)

-- | Content
-- <title>My Book</title>
-- "My Book" is the text content
data Content
  = Elem [Element]
  | Text String
  deriving (Show)

-- | Attributes
-- Key-Value pairs
-- ie: <book id="123" category="fiction">
-- Key: id, value: 123
data Attr = Attr
  { attrKey :: String,
    attrValue :: String
  }
  deriving (Show)

data OpenTag = OpenTag
  { openTagName :: QName,
    openTagAttrs :: [Attr]
  }
  deriving (Show)

data CloseTag = CloseTag
  { closeTagName :: QName
  }
  deriving (Show)

-- | XML Element
-- An XML element is everything from (including) the element's start tag to (including) the element's end tag.
-- > <price>29.99</price>
-- https://www.w3schools.com/xmL/xml_elements.asp
data Element = Element
  { ename :: QName,
    eattrs :: [Attr],
    econtent :: Content
  }
  deriving (Show)

data Header = Header
  { hattrs :: [Attr]
  }

data Document = Document
  { dheader :: Header,
    delement :: Element
  }
