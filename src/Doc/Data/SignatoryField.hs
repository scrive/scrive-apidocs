module Doc.Data.SignatoryField (
    FieldType(..)
  , NameOrder(..)
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
import Text.JSON
import Text.JSON.FromJSValue
import qualified Text.JSON.Gen as J

import DB.Derive
import Doc.SignatoryFieldID
import File.FileID
import KontraPrelude

newtype NameOrder = NameOrder Int16
  deriving (Eq, Ord, Show, Data, Typeable)

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
    deriving (Eq, Show, Data, Typeable)

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

data PlacementAnchor = PlacementAnchor {
  placementanchortext  :: !String
, placementanchorindex :: !Int
, placementanchorpages :: !(Maybe [Int])
} deriving (Eq,Ord, Show, Data, Typeable)

data TipSide = LeftTip | RightTip
  deriving (Eq, Show, Data, Typeable)

data FieldPlacement = FieldPlacement {
  placementxrel    :: !Double
, placementyrel    :: !Double
, placementwrel    :: !Double
, placementhrel    :: !Double
, placementfsrel   :: !Double
, placementpage    :: !Int
, placementtipside :: !(Maybe TipSide)
, placementanchors :: ![PlacementAnchor]
} deriving (Show, Data, Typeable)

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
  pqFormat = const $ pqFormat ($undefined::String)

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
      pages  <- fmap Just $ fromJSValueField "pages"
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
            case placementanchorpages anchor of
              Nothing -> return ()
              Just pages -> J.value "pages" pages
        J.value "tip" $ case (placementtipside placement) of
          Just LeftTip -> Just ("left" :: String)
          Just RightTip -> Just "right"
          _ -> Nothing

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
  , stfFilledByAuthor        :: !Bool
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
  , "signatory_link_fields.placements"
  ]

type instance CompositeRow SignatoryField = (SignatoryFieldID, FieldType, Maybe NameOrder, String, Bool, Maybe String, Maybe Bool, Maybe FileID, Bool, Bool, [FieldPlacement])

instance PQFormat SignatoryField where
  pqFormat _ = "%signatory_field"

instance CompositeFromSQL SignatoryField where
  toComposite (sfid, ftype, mname_order, custom_name, is_author_filled, mvalue_text, mvalue_bool, mvalue_file, obligatory, should_be_filled_by_sender, placements) =
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
