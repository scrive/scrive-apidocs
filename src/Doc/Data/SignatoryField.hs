module Doc.Data.SignatoryField (
    FieldType(..)
  , NameOrder(..)
  , PlacementID
  , tempPlacementID
  , PlacementAnchor(..)
  , TipSide(..)
  , FieldPlacement(..)
  , SignatoryField(..)
  , signatoryFieldsSelectors
  , SignatoryNameField(..)
  , SignatoryCompanyField(..)
  , SignatoryPersonalNumberField(..)
  , SignatoryCompanyNumberField(..)
  , SignatoryEmailField(..)
  , SignatoryMobileField(..)
  , SignatoryTextField(..)
  , SignatoryCheckboxField(..)
  , SignatorySignatureField(..)
  ) where

import Control.Monad.Catch
import Data.Data
import Data.Int
import Database.PostgreSQL.PQTypes hiding (def)

import DB.Derive
import Doc.SignatoryFieldID
import File.FileID
import KontraPrelude

newtype NameOrder = NameOrder Int16
  deriving (Eq, Ord, Show)

instance FromSQL NameOrder where
  type PQBase NameOrder = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return $ NameOrder 1
      2  -> return $ NameOrder 2
      _  -> throwM RangeError {
        reRange = [(1,2)]
      , reValue = n
      }

instance ToSQL NameOrder where
  type PQDest NameOrder = PQDest Int16
  toSQL (NameOrder 1) = toSQL (1 :: Int16)
  toSQL (NameOrder 2) = toSQL (2 :: Int16)
  toSQL (NameOrder v) = $unexpectedError $ "Name order " ++ show v ++ " is not supported"

instance PQFormat NameOrder where
  pqFormat = const $ pqFormat ($undefined::Int16)

data FieldType
  = NameFT
  | CompanyFT
  | PersonalNumberFT
  | CompanyNumberFT
  | EmailFT
  | TextFT
  | SignatureFT
  | CheckboxFT
  | MobileFT
    deriving (Eq, Show)

instance PQFormat FieldType where
  pqFormat = const $ pqFormat ($undefined::Int16)

instance FromSQL FieldType where
  type PQBase FieldType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return $ NameFT
      -- Don't reuse value '2' for FieldType. It was used for LastNameFT
      3  -> return CompanyFT
      4  -> return PersonalNumberFT
      5  -> return CompanyNumberFT
      6  -> return EmailFT
      7  -> return TextFT
      8  -> return SignatureFT
      9  -> return CheckboxFT
      10 -> return MobileFT
      _  -> throwM RangeError {
        reRange = [(1,1),(3,10)]
      , reValue = n
      }

instance ToSQL FieldType where
  type PQDest FieldType = PQDest Int16
  toSQL NameFT           = toSQL (1::Int16)
  toSQL CompanyFT        = toSQL (3::Int16)
  toSQL PersonalNumberFT = toSQL (4::Int16)
  toSQL CompanyNumberFT  = toSQL (5::Int16)
  toSQL EmailFT          = toSQL (6::Int16)
  toSQL TextFT           = toSQL (7::Int16)
  toSQL SignatureFT      = toSQL (8::Int16)
  toSQL CheckboxFT       = toSQL (9::Int16)
  toSQL MobileFT         = toSQL (10::Int16)

---------------------------------

newtype PlacementID = PlacementID Int64
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''PlacementID)

instance FromSQL PlacementID where
  type PQBase PlacementID = PQBase Int64
  fromSQL mbase = PlacementID <$> fromSQL mbase
instance ToSQL PlacementID where
  type PQDest PlacementID = PQDest Int64
  toSQL (PlacementID n) = toSQL n

tempPlacementID :: PlacementID
tempPlacementID = PlacementID 0

---------------------------------

data PlacementAnchor = PlacementAnchor {
  placementanchortext  :: !String
, placementanchorindex :: !Int32
} deriving (Eq, Ord, Show)

type instance CompositeRow PlacementAnchor = (String, Int32)

instance PQFormat PlacementAnchor where
  pqFormat = const "%placement_anchor"

instance CompositeFromSQL PlacementAnchor where
  toComposite (text, index) = PlacementAnchor {
      placementanchortext = text
    , placementanchorindex = index
    }

data TipSide = LeftTip | RightTip
  deriving (Eq, Show)

instance PQFormat TipSide where
  pqFormat = const $ pqFormat ($undefined::Int16)

instance FromSQL TipSide where
  type PQBase TipSide = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return LeftTip
      2  -> return RightTip
      _  -> throwM RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL TipSide where
  type PQDest TipSide = PQDest Int16
  toSQL LeftTip  = toSQL (1::Int16)
  toSQL RightTip = toSQL (2::Int16)

data FieldPlacement = FieldPlacement {
  placementid      :: !PlacementID
, placementxrel    :: !Double
, placementyrel    :: !Double
, placementwrel    :: !Double
, placementhrel    :: !Double
, placementfsrel   :: !Double
, placementpage    :: !Int32
, placementtipside :: !(Maybe TipSide)
, placementanchors :: ![PlacementAnchor]
} deriving Show

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

type instance CompositeRow FieldPlacement = (PlacementID, Double, Double, Double, Double, Double, Int32, Maybe TipSide, CompositeArray1 PlacementAnchor)

instance PQFormat FieldPlacement where
  pqFormat = const "%field_placement"

instance CompositeFromSQL FieldPlacement where
  toComposite (pid, xrel, yrel, wrel, hrel, fsrel, page, tip, CompositeArray1 anchors) = FieldPlacement {
      placementid = pid
    , placementxrel = xrel
    , placementyrel = yrel
    , placementwrel = wrel
    , placementhrel = hrel
    , placementfsrel = fsrel
    , placementpage = page
    , placementtipside = tip
    , placementanchors = anchors
    }

---------------------------------

data SignatoryField = SignatoryNameField SignatoryNameField
                    | SignatoryCompanyField SignatoryCompanyField
                    | SignatoryPersonalNumberField SignatoryPersonalNumberField
                    | SignatoryCompanyNumberField SignatoryCompanyNumberField
                    | SignatoryEmailField SignatoryEmailField
                    | SignatoryMobileField SignatoryMobileField
                    | SignatoryTextField SignatoryTextField
                    | SignatoryCheckboxField SignatoryCheckboxField
                    | SignatorySignatureField SignatorySignatureField
  deriving (Show, Typeable)

data SignatoryNameField = NameField {
    snfID                     :: !SignatoryFieldID
  , snfNameOrder              :: !NameOrder
  , snfValue                  :: !String
  , snfObligatory             :: !Bool
  , snfShouldBeFilledBySender :: !Bool
  , snfPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)

data SignatoryCompanyField = CompanyField {
    scfID                     :: !SignatoryFieldID
  , scfValue                  :: !String
  , scfObligatory             :: !Bool
  , scfShouldBeFilledBySender :: !Bool
  , scfPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)

data SignatoryPersonalNumberField = PersonalNumberField {
    spnfID                     :: !SignatoryFieldID
  , spnfValue                  :: !String
  , spnfObligatory             :: !Bool
  , spnfShouldBeFilledBySender :: !Bool
  , spnfPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)

data SignatoryCompanyNumberField = CompanyNumberField {
    scnfID                     :: !SignatoryFieldID
  , scnfValue                  :: !String
  , scnfObligatory             :: !Bool
  , scnfShouldBeFilledBySender :: !Bool
  , scnfPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)

data SignatoryEmailField = EmailField {
    sefID                     :: !SignatoryFieldID
  , sefValue                  :: !String
  , sefObligatory             :: !Bool
  , sefShouldBeFilledBySender :: !Bool
  , sefPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)

data SignatoryMobileField = MobileField {
    smfID                     :: !SignatoryFieldID
  , smfValue                  :: !String
  , smfObligatory             :: !Bool
  , smfShouldBeFilledBySender :: !Bool
  , smfPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)

data SignatoryTextField = TextField {
    stfID                     :: !SignatoryFieldID
  , stfName                   :: !String
  , stfFilledByAuthor         :: !Bool
  , stfValue                  :: !String
  , stfObligatory             :: !Bool
  , stfShouldBeFilledBySender :: !Bool
  , stfPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)

data SignatoryCheckboxField = CheckboxField {
    schfID                     :: !SignatoryFieldID
  , schfName                   :: !String
  , schfValue                  :: !Bool
  , schfObligatory             :: !Bool
  , schfShouldBeFilledBySender :: !Bool
  , schfPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)


data SignatorySignatureField = SignatureField {
    ssfID                     :: !SignatoryFieldID
  , ssfName                   :: !String
  , ssfValue                  :: !(Maybe FileID)
  , ssfObligatory             :: !Bool
  , ssfShouldBeFilledBySender :: !Bool
  , ssfPlacements             :: ![FieldPlacement]
} deriving (Show, Typeable)

---------------------------------

signatoryFieldsSelectors :: [SQL]
signatoryFieldsSelectors = [
    "signatory_link_fields.id"
  , "signatory_link_fields.type"
  , "signatory_link_fields.name_order"
  , "signatory_link_fields.custom_name"
  , "signatory_link_fields.is_author_filled"
  , "signatory_link_fields.value_text"
  , "signatory_link_fields.value_bool"
  , "signatory_link_fields.value_file_id"
  , "signatory_link_fields.obligatory"
  , "signatory_link_fields.should_be_filled_by_author"
  , "ARRAY(" <> placements <> ")"
  ]
  where
    placements = "SELECT (id, xrel, yrel, wrel, hrel, fsrel, page, tip, ARRAY(" <> anchors <> "))::field_placement FROM field_placements WHERE field_placements.signatory_field_id = signatory_link_fields.id ORDER BY field_placements.id"

    anchors = "SELECT (text, index)::placement_anchor FROM placement_anchors WHERE placement_anchors.field_placement_id = field_placements.id ORDER BY placement_anchors.id"

type instance CompositeRow SignatoryField = (SignatoryFieldID, FieldType, Maybe NameOrder, String, Bool, Maybe String, Maybe Bool, Maybe FileID, Bool, Bool, CompositeArray1 FieldPlacement)

instance PQFormat SignatoryField where
  pqFormat = const "%signatory_field"

instance CompositeFromSQL SignatoryField where
  toComposite (sfid, ftype, mname_order, custom_name, is_author_filled, mvalue_text, mvalue_bool, mvalue_file, obligatory, should_be_filled_by_sender, CompositeArray1 placements) =
    case ftype of
      NameFT -> SignatoryNameField $ NameField {
          snfID                     = sfid
        , snfNameOrder              = fromMaybe ($unexpectedError "Name field has NULL as name_order") mname_order
        , snfValue                  = fromMaybe ($unexpectedError "Name field has NULL as value_text") mvalue_text
        , snfObligatory             = obligatory
        , snfShouldBeFilledBySender = should_be_filled_by_sender
        , snfPlacements             = placements
      }
      CompanyFT -> SignatoryCompanyField $ CompanyField {
          scfID                     = sfid
        , scfValue                  = fromMaybe ($unexpectedError "Company field has NULL as value_text") mvalue_text
        , scfObligatory             = obligatory
        , scfShouldBeFilledBySender = should_be_filled_by_sender
        , scfPlacements             = placements
      }
      PersonalNumberFT -> SignatoryPersonalNumberField $ PersonalNumberField {
          spnfID                     = sfid
        , spnfValue                  = fromMaybe ($unexpectedError "Personal number field has NULL as value_text") mvalue_text
        , spnfObligatory             = obligatory
        , spnfShouldBeFilledBySender = should_be_filled_by_sender
        , spnfPlacements             = placements
      }
      CompanyNumberFT -> SignatoryCompanyNumberField $ CompanyNumberField {
          scnfID                     = sfid
        , scnfValue                  = fromMaybe ($unexpectedError "Company number field has NULL as value_text") mvalue_text
        , scnfObligatory             = obligatory
        , scnfShouldBeFilledBySender = should_be_filled_by_sender
        , scnfPlacements             = placements
      }
      EmailFT -> SignatoryEmailField $ EmailField {
          sefID                     = sfid
        , sefValue                  = fromMaybe ($unexpectedError "Email field has NULL as value_text") mvalue_text
        , sefObligatory             = obligatory
        , sefShouldBeFilledBySender = should_be_filled_by_sender
        , sefPlacements             = placements
      }
      MobileFT -> SignatoryMobileField $ MobileField {
          smfID                     = sfid
        , smfValue                  = fromMaybe ($unexpectedError "Mobile field has NULL as value_text") mvalue_text
        , smfObligatory             = obligatory
        , smfShouldBeFilledBySender = should_be_filled_by_sender
        , smfPlacements             = placements
      }
      TextFT -> SignatoryTextField $ TextField {
          stfID                     = sfid
        , stfName                   = custom_name
        , stfFilledByAuthor          = is_author_filled
        , stfValue                  = fromMaybe ($unexpectedError "Text field has NULL as value_text") mvalue_text
        , stfObligatory             = obligatory
        , stfShouldBeFilledBySender = should_be_filled_by_sender
        , stfPlacements             = placements
      }
      CheckboxFT -> SignatoryCheckboxField $ CheckboxField {
          schfID                     = sfid
        , schfName                   = custom_name
        , schfValue                  = fromMaybe ($unexpectedError "Checkbox field has NULL as value_bool") mvalue_bool
        , schfObligatory             = obligatory
        , schfShouldBeFilledBySender = should_be_filled_by_sender
        , schfPlacements             = placements
      }
      SignatureFT -> SignatorySignatureField $ SignatureField {
          ssfID                     = sfid
        , ssfName                   = custom_name
        , ssfValue                  = mvalue_file
        , ssfObligatory             = obligatory
        , ssfShouldBeFilledBySender = should_be_filled_by_sender
        , ssfPlacements             = placements
      }
