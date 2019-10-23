module Text.XML.Writer.Extended (
    module Text.XML.Writer
  , documentA
  ) where

import Data.Default ()
import Data.Text (Text)
import Text.XML
import Text.XML.Writer
import qualified Data.Map as M

-- | Create a simple Document starting with a root element constructed
--   with name, attributes and children.
documentA
  :: Name -- ^ Root node name
  -> [(Name, Text)]
  -> XML  -- ^ Contents
  -> Document
documentA name attrs children = Document
  { documentPrologue = Prologue def def def
  , documentRoot     = Element name (M.fromList attrs) (render children)
  , documentEpilogue = def
  }
