-- | Support for parsing, rendering and simple processing of XML
-- content which may be malformed.  The purpose of this module is to
-- form a workaround for malformed content in database which prevent
-- us from migrating column types to XML.
module Text.XML.DirtyContent
  ( XMLContent(..)
  , parseXMLContent
  , renderXMLContent
  , substitute
  , removeTags
  , cdata
  , ToXMLContent(..)
  )
  where

import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text (Text)
import Text.XML (Name)
import qualified Data.Text as T

import qualified Text.XML.Content as C

data XMLContent =
   CleanXMLContent C.XMLContent
 | DirtyXMLContent Text
  deriving (Eq, Ord, Show)

-- | Parses text as XML; removing processing instructions and comments; cleaning up HTML entities
parseXMLContent :: Text -> XMLContent
parseXMLContent s =
  either (const (DirtyXMLContent s)) CleanXMLContent (C.parseXMLContent s)

renderXMLContent :: XMLContent -> Text
renderXMLContent (CleanXMLContent c) = C.renderXMLContent c
renderXMLContent (DirtyXMLContent t) = t

substitute :: Map (Name, Text) C.XMLContent -> XMLContent -> XMLContent
substitute m (CleanXMLContent c) = CleanXMLContent (C.substitute m c)
substitute _ (DirtyXMLContent t) = DirtyXMLContent t

-- | Remove tags, keeping content
removeTags :: XMLContent -> XMLContent
removeTags (CleanXMLContent c) = CleanXMLContent (C.removeTags c)
removeTags (DirtyXMLContent t) = DirtyXMLContent (T.pack (filterTags (T.unpack t)))
  where
    filterTags ('<' : rest) = ' ' : filterTags (drop 1 $ dropWhile (/= '>') rest)
    filterTags (a   : rest) = a : filterTags rest
    filterTags []           = []

-- | Form XMLContent from string literals
instance IsString XMLContent where
  fromString = CleanXMLContent . fromString

-- | Form XMLContent from plain text
cdata :: Text -> XMLContent
cdata = CleanXMLContent . C.cdata

-- | Stuff that can be converted into XML content
class ToXMLContent a where
  toXMLContent :: a -> XMLContent

instance ToXMLContent XMLContent where
  toXMLContent = identity

instance ToXMLContent Text where
  toXMLContent = cdata

instance ToXMLContent String where
  toXMLContent = toXMLContent . T.pack
