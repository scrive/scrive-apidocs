module Doc.Data.DocumentTag (
    DocumentTag(..)
  , documentTagsSelectors
  ) where

import Data.Function

import DB

data DocumentTag = DocumentTag {
  tagname  :: !String
, tagvalue :: !String
} deriving (Eq, Show)

-- | For 'Set' storage.
instance Ord DocumentTag where
  compare = compare `on` tagname

---------------------------------

documentTagsSelectors :: [SQL]
documentTagsSelectors = [
    "document_tags.name"
  , "document_tags.value"
  ]

type instance CompositeRow DocumentTag = (String, String)

instance PQFormat DocumentTag where
  pqFormat _ = "%document_tag"

instance CompositeFromSQL DocumentTag where
  toComposite (name, value) = DocumentTag {
    tagname = name
  , tagvalue = value
  }
