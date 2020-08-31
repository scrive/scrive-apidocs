module Doc.Types.DocumentTag (
    DocumentTag(..)
  , documentTagsSelectors
  ) where

import Data.Function
import Database.PostgreSQL.PQTypes.Model.CompositeType

import DB
import Doc.Tables

data DocumentTag = DocumentTag
  { tagname  :: Text
  , tagvalue :: Text
  } deriving (Eq, Show)

-- | For 'Set' storage.
instance Ord DocumentTag where
  compare = compare `on` tagname

---------------------------------

documentTagsSelectors :: [SQL]
documentTagsSelectors = ["document_tags.name", "document_tags.value"]

type instance CompositeRow DocumentTag = (Text, Text)

instance PQFormat DocumentTag where
  pqFormat = compositeTypePqFormat ctDocumentTag

instance CompositeFromSQL DocumentTag where
  toComposite (name, value) = DocumentTag { tagname = name, tagvalue = value }
