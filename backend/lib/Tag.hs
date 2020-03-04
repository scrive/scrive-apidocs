{-# LANGUAGE TemplateHaskell #-}
module Tag (
    TagDomain(..)
  , TagOp(..)
  , Tag(Tag)
  , TagUpdate(..)
  , updateTags
) where

import Data.Aeson.Types
import Data.Function
import Data.Unjson
import Database.PostgreSQL.PQTypes.Composite
import Database.PostgreSQL.PQTypes.Format
import Database.PostgreSQL.PQTypes.Model.CompositeType
import Optics.TH
import qualified Data.Set as S

import InspectXML
import Tag.Tables

data Tag = Tag
  { name  :: !Text
  , value :: !Text
  } deriving (Eq, Show)

instance Ord Tag where
  compare = compare `on` name

type instance CompositeRow Tag = (Text, Text)

instance PQFormat Tag where
  pqFormat = compositeTypePqFormat ctTag

instance CompositeFromSQL Tag where
  toComposite (name, value) = Tag { .. }

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \v -> Tag <$> v .: "name" <*> v .: "value"

instance ToJSON Tag where
  toJSON (Tag name val) = object ["name" .= name, "value" .= val]

instance InspectXML Tag

makeFieldLabelsWith noPrefixFieldLabels ''Tag

-- | We support two kinds of tags: internal ones are only accessible through private APIs
-- (such as AdminOnly), while external tags can be exposed to the public.
data TagDomain = Internal | External
  deriving (Eq)

-- Tag updating

data TagOp = SetTo Text | Delete
  deriving (Eq, Ord, Show)

instance ToJSON TagOp where
  toJSON = \case
    SetTo t -> String t
    Delete  -> Null

instance FromJSON TagOp where
  parseJSON = \case
    Null     -> pure $ Delete
    String s -> pure $ SetTo s
    invalid  -> typeMismatch "Expected a string or `null`" invalid

instance Unjson TagOp where
  unjsonDef = unjsonAeson

data TagUpdate = TagUpdate Text TagOp

instance FromJSON TagUpdate where
  parseJSON = withObject "TagUpdate" $ \v -> TagUpdate <$> v .: "name" <*> v .: "value"

instance ToJSON TagUpdate where
  toJSON (TagUpdate name val) = object ["name" .= name, "value" .= val]

instance Unjson TagUpdate where
  unjsonDef = unjsonAeson

updateTag :: [Tag] -> TagUpdate -> [Tag]
updateTag ugts (TagUpdate k op) = case op of
  SetTo v -> (Tag.Tag k v) : deleted
  Delete  -> deleted
  where deleted = filter (\ugt -> ugt ^. #name /= k) ugts

updateTags :: S.Set Tag -> [TagUpdate] -> S.Set Tag
updateTags currentTags = S.fromList . foldl' updateTag (S.toList currentTags)
