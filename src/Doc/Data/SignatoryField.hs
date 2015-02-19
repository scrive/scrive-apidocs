module Doc.Data.SignatoryField (
    FieldType(..)
  , SignatoryFieldValue(..)
  , sfvEncode
  , sfvNull
  , getBinaryField
  , getTextField
  , PlacementAnchor(..)
  , TipSide(..)
  , FieldPlacement(..)
  , SignatoryField(..)
  , getFieldOfType
  , getValueOfType
  , getTextValueOfType
  , signatoryFieldsSelectors
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Data
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Monoid.Utils
import Data.String
import Database.PostgreSQL.PQTypes hiding (def)
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Char8 as BS
import qualified Text.JSON.Gen as J

import DB.Derive
import Doc.SignatoryFieldID
import OurPrelude
import Utils.Image

data FieldType
  = FirstNameFT
  | LastNameFT
  | CompanyFT
  | PersonalNumberFT
  | CompanyNumberFT
  | EmailFT
  | CustomFT String Bool -- label filledbyauthor
  | SignatureFT String
  | CheckboxFT String
  | MobileFT
    deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat FieldType where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL FieldType where
  type PQBase FieldType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return FirstNameFT
      2  -> return LastNameFT
      3  -> return CompanyFT
      4  -> return PersonalNumberFT
      5  -> return CompanyNumberFT
      6  -> return EmailFT
      7  -> return $ CustomFT undefinedField undefinedField
      8  -> return $ SignatureFT undefinedField
      9  -> return $ CheckboxFT undefinedField
      10 -> return MobileFT
      _  -> throwM RangeError {
        reRange = [(1, 10)]
      , reValue = n
      }
    where
      undefinedField :: t
      undefinedField = $unexpectedError "undefined field"

instance ToSQL FieldType where
  type PQDest FieldType = PQDest Int16
  toSQL FirstNameFT      = toSQL (1::Int16)
  toSQL LastNameFT       = toSQL (2::Int16)
  toSQL CompanyFT        = toSQL (3::Int16)
  toSQL PersonalNumberFT = toSQL (4::Int16)
  toSQL CompanyNumberFT  = toSQL (5::Int16)
  toSQL EmailFT          = toSQL (6::Int16)
  toSQL CustomFT{}       = toSQL (7::Int16)
  toSQL SignatureFT{}    = toSQL (8::Int16)
  toSQL CheckboxFT{}     = toSQL (9::Int16)
  toSQL MobileFT         = toSQL (10::Int16)

---------------------------------

-- FIXME: this is bad. we need to integrate this into SignatoryField
-- itself (it's needed anyway as e.g. checkboxes really are of value
-- Bool, signature field should have at most one placement etc.).
data SignatoryFieldValue = BinaryField !BS.ByteString | TextField !String
  deriving (Ord, Eq, Show, Data, Typeable)

instance IsString SignatoryFieldValue where
  fromString = TextField

-- | Encode 'SignatoryFieldValue' to be used in a template
sfvEncode :: FieldType -> SignatoryFieldValue -> String
sfvEncode ft v = case (ft, v) of
  (SignatureFT{}, BinaryField "") -> "" -- FIXME: this case shouldn't be needed
  (SignatureFT{}, BinaryField bs) -> BS.unpack $ imgEncodeRFC2397 bs
  (SignatureFT{}, TextField{})    -> failure
  (_, TextField s)                -> s
  (_, BinaryField{})              -> failure
  where
    failure = error $ "sfvEncode: field of type" <+> show ft <+> "has the following value:" <+> show v <> ", this is not supposed to happen"

-- | Check whether 'FieldValue' is null.
sfvNull :: SignatoryFieldValue -> Bool
sfvNull (TextField s) = null s
sfvNull (BinaryField bs) = BS.null bs

getBinaryField :: SignatoryFieldValue -> Maybe BS.ByteString
getBinaryField (BinaryField bs) = Just bs
getBinaryField _ = Nothing

getTextField :: SignatoryFieldValue -> Maybe String
getTextField (TextField s) = Just s
getTextField _ = Nothing

---------------------------------

data PlacementAnchor = PlacementAnchor {
  placementanchortext  :: !String
, placementanchorindex :: !Int
, placementanchorpages :: ![Int]
} deriving (Eq, Ord, Show, Data, Typeable)

data TipSide = LeftTip | RightTip
  deriving (Eq, Ord, Show, Data, Typeable)

data FieldPlacement = FieldPlacement {
  placementxrel    :: !Double
, placementyrel    :: !Double
, placementwrel    :: !Double
, placementhrel    :: !Double
, placementfsrel   :: !Double
, placementpage    :: !Int
, placementtipside :: !(Maybe TipSide)
, placementanchors :: ![PlacementAnchor]
} deriving (Ord, Show, Data, Typeable)

instance Eq FieldPlacement where
  a == b = and [
      eqByEpsilon placementxrel
    , eqByEpsilon placementyrel
    , eqByEpsilon placementwrel
    , eqByEpsilon placementhrel
    , eqByEpsilon placementfsrel
    , placementpage a == placementpage b
    , placementtipside a == placementtipside b
    , placementanchors a == placementanchors b
    ]
    where
      eqByEpsilon f = abs (f a - f b) < 0.00001

instance PQFormat [FieldPlacement] where
  pqFormat _ = pqFormat (undefined::String)

instance FromSQL [FieldPlacement] where
  type PQBase [FieldPlacement] = PQBase String
  fromSQL = jsonFromSQL' $ fmap nothingToResult $ fromJSValueCustomMany $ do
    xrel <- fromJSValueField "xrel"
    yrel <- fromJSValueField "yrel"
    wrel <- fromJSValueField "wrel"
    hrel <- fromJSValueField "hrel"
    fsrel <- fromJSValueField "fsrel"
    page <- fromJSValueField "page"
    side <- fromJSValueFieldCustom "tip" $ do
      s <- fromJSValue
      case s :: Maybe String of
        Just "left"  -> return $ Just LeftTip
        Just "right" -> return $ Just RightTip
        _ ->            return $ Nothing
    anchors <- fmap (fromMaybe []) <$> fromJSValueFieldCustom "anchors" $ fromJSValueCustomMany $ do
      text <- fromJSValueField "text"
      index <- fromMaybe (Just 1) <$> fromJSValueField "index"
      pages  <- fromJSValueField "pages"
      return (PlacementAnchor <$> text
        <*> index
        <*> pages)
    return (FieldPlacement <$> xrel <*> yrel
      <*> wrel <*> hrel <*> fsrel
      <*> page <*> Just side
      <*> Just anchors)

instance ToSQL [FieldPlacement] where
  type PQDest [FieldPlacement] = PQDest String
  toSQL = jsonToSQL' $ JSArray . map placementJSON
    where
      placementJSON placement = J.runJSONGen $ do
        J.value "xrel" $ placementxrel placement
        J.value "yrel" $ placementyrel placement
        J.value "wrel" $ placementwrel placement
        J.value "hrel" $ placementhrel placement
        J.value "fsrel" $ placementfsrel placement
        J.value "page" $ placementpage placement
        when (not (null (placementanchors placement))) $ do
          J.value "anchors" $ (flip map) (placementanchors placement) $ \anchor -> J.runJSONGen $ do
            J.value "text" (placementanchortext anchor)
            when (placementanchorindex anchor /=1 ) $ do
              J.value "index" (placementanchorindex anchor)
            J.value "pages" (placementanchorpages anchor)
        J.value "tip" $ case (placementtipside placement) of
          Just LeftTip -> Just ("left" :: String)
          Just RightTip -> Just "right"
          _ -> Nothing

---------------------------------

data SignatoryField = SignatoryField {
  sfID                     :: !SignatoryFieldID
, sfType                   :: !FieldType
, sfValue                  :: !SignatoryFieldValue
, sfObligatory             :: !Bool
, sfShouldBeFilledBySender :: !Bool
, sfPlacements             :: ![FieldPlacement]
} deriving (Ord, Show, Data, Typeable)

-- | Do not compare IDs.
instance Eq SignatoryField where
  a == b = and [
      sfType a == sfType b
    , sfValue a == sfValue b
    , sfObligatory a == sfObligatory b
    , sfShouldBeFilledBySender a == sfShouldBeFilledBySender b
    , sfPlacements a == sfPlacements b
    ]

getFieldOfType :: FieldType -> [SignatoryField] -> Maybe SignatoryField
getFieldOfType t = find ((t ==) . sfType)

getValueOfType :: FieldType -> [SignatoryField] -> SignatoryFieldValue
getValueOfType t = fromMaybe def . fmap sfValue . getFieldOfType t
  where
    def = case t of
      SignatureFT{} -> BinaryField BS.empty
      _             -> TextField ""

getTextValueOfType :: FieldType -> [SignatoryField] -> String
getTextValueOfType t = fromMaybe "" . getTextField . getValueOfType t

---------------------------------

signatoryFieldsSelectors :: [SQL]
signatoryFieldsSelectors = [
    "signatory_link_fields.id"
  , "signatory_link_fields.type"
  , "signatory_link_fields.custom_name"
  , "signatory_link_fields.is_author_filled"
  , "signatory_link_fields.value_text"
  , "signatory_link_fields.value_binary"
  , "signatory_link_fields.obligatory"
  , "signatory_link_fields.should_be_filled_by_author"
  , "signatory_link_fields.placements"
  ]

type instance CompositeRow SignatoryField = (SignatoryFieldID, FieldType, String, Bool, Maybe String, Maybe (Binary BS.ByteString), Bool, Bool, [FieldPlacement])

instance PQFormat SignatoryField where
  pqFormat _ = "%signatory_field"

instance CompositeFromSQL SignatoryField where
  toComposite (sfid, ftype, custom_name, is_author_filled, mvalue_text, mvalue_binary, obligatory, should_be_filled_by_sender, placements) = SignatoryField {
    sfID = sfid
  , sfType = case ftype of
    CustomFT{} -> CustomFT custom_name is_author_filled
    CheckboxFT{} -> CheckboxFT custom_name
    SignatureFT{} -> SignatureFT $ if null custom_name then "signature" else custom_name
    _   -> ftype
  , sfValue = case (mvalue_text, mvalue_binary) of
    (Just value_text, Nothing)   -> TextField value_text
    (Nothing, Just value_binary) -> BinaryField $ unBinary value_binary
    _ -> $unexpectedError "can't happen due to the checks in the database"
  , sfObligatory = obligatory
  , sfShouldBeFilledBySender = should_be_filled_by_sender
  , sfPlacements = placements
  }
