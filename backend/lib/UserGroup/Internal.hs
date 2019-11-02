{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module UserGroup.Internal (
    InvoicingType(..)
  , UserGroup(..)
  , fetchUserGroup
  , defaultUserGroup
  , defaultChildUserGroup
  , ugInvoicingType
  , ugPaymentPlan
  , ugwpOnlyParents
  , ugwpPaymentPlan
  , ugwpPaymentPlanWithID
  , ugwpSettings
  , ugwpSettingsWithID
  , ugwpAddress
  , ugwpAddressWithID
  , ugwpFeatures
  , ugwpFeaturesWithID
  , ugwpToList
  , ugwpUG
  , ugwpRoot
  , ugwpAddChild
  , ugrFromUG
  , ugwcToList
  , UserGroupID
  , emptyUserGroupID
  , unsafeUserGroupID
  , fromUserGroupID
  , UserGroupSettings(..)
  , defaultUserGroupSettings
  , UserGroupAddress(..)
  , defaultUserGroupAddress
  , UserGroupUI(..)
  , defaultUserGroupUI
  , UserGroupInvoicing(..)
  , UserGroupWithParents
  , UserGroupRoot(..)
  , ugFromUGRoot
  , UserGroupWithChildren(..)
  ) where

import Data.Aeson
import Data.Int
import Data.Text (Text)
import Data.Unjson
import Database.PostgreSQL.PQTypes.Model.CompositeType
import Happstack.Server
import Optics.TH
import qualified Control.Exception.Lifted as E
import qualified Data.Binary as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import DataRetentionPolicy
import DB
import FeatureFlags.Model
import Folder.Types
import IPAddress
import Log.Identifier
import PadApplication.Types
import SMS.Types
import Theme.ThemeID
import UserGroup.Tables
import UserGroup.Types.PaymentPlan

newtype UserGroupID = UserGroupID Int64
  deriving (Eq, Ord)
deriving newtype instance Read UserGroupID
deriving newtype instance Show UserGroupID
deriving newtype instance TextShow UserGroupID

instance ToJSON UserGroupID where
  toJSON (UserGroupID n) = toJSON $ show n

instance FromJSON UserGroupID where
  parseJSON v = do
    uidStr <- parseJSON v
    case maybeRead uidStr of
      Nothing  -> fail "Could not parse User Group ID"
      Just uid -> return uid

instance PQFormat UserGroupID where
  pqFormat = pqFormat @Int64

instance FromSQL UserGroupID where
  type PQBase UserGroupID = PQBase Int64
  fromSQL mbase = UserGroupID <$> fromSQL mbase

instance ToSQL UserGroupID where
  type PQDest UserGroupID = PQDest Int64
  toSQL (UserGroupID n) = toSQL n

instance FromReqURI UserGroupID where
  fromReqURI = maybeRead . T.pack

unsafeUserGroupID :: Int64 -> UserGroupID
unsafeUserGroupID = UserGroupID

emptyUserGroupID :: UserGroupID
emptyUserGroupID = UserGroupID 0

fromUserGroupID :: UserGroupID -> Int64
fromUserGroupID (UserGroupID ugid) = ugid

instance Identifier UserGroupID where
  idDefaultLabel = "user_group_id"
  idValue (UserGroupID k) = int64AsStringIdentifier k

instance B.Binary UserGroupID where
  put (UserGroupID ugid) = B.put ugid
  get = fmap UserGroupID B.get

instance Unjson UserGroupID where
  unjsonDef = unjsonInvmapR
    ((maybe (fail "Can't parse UserGroupID") return) . maybeRead)
    showt
    unjsonDef

----------------------------------------

data UserGroup = UserGroup
  { id            :: UserGroupID
  , parentGroupID :: Maybe UserGroupID
  , name          :: Text
  -- Folder, where home folders are created for new users
  -- it is a Maybe for slow migration purposes after that
  -- the Maybe will be removed
  -- The Maybe can be re-introduced, when we implement home folder inheritance
  , homeFolderID  :: Maybe FolderID
  , address       :: Maybe UserGroupAddress
  , settings      :: Maybe UserGroupSettings
  , invoicing     :: UserGroupInvoicing
  , ui            :: UserGroupUI
  , features      :: Maybe Features
  } deriving (Show, Eq)

data UserGroupRoot = UserGroupRoot
  { id            :: UserGroupID
  , name          :: Text
  , homeFolderID  :: Maybe FolderID
  , address       :: UserGroupAddress
  , settings      :: UserGroupSettings
  , paymentPlan   :: PaymentPlan  -- user group root always must have Invoice
  , ui            :: UserGroupUI
  , features      :: Features
  } deriving (Show, Eq)

-- UserGroup list is ordered from Leaf to Child of Root)
type UserGroupWithParents = (UserGroupRoot, [UserGroup])

-- UserGroup and all its children down to the bottom
data UserGroupWithChildren = UserGroupWithChildren
  { group :: UserGroup
  , children :: [UserGroupWithChildren]
  } deriving (Eq, Show)

data UserGroupInvoicing =
    None
  | BillItem (Maybe PaymentPlan)
  | Invoice PaymentPlan
  deriving (Show, Eq)

data InvoicingType =
    InvoicingTypeNone
  | InvoicingTypeBillItem
  | InvoicingTypeInvoice
  deriving (Eq, Ord)

data UserGroupSettings = UserGroupSettings
  { ipAddressMaskList         :: [IPAddressWithMask]
  , dataRetentionPolicy       :: DataRetentionPolicy
  , cgiDisplayName            :: Maybe Text
  , cgiServiceID              :: Maybe Text
  , smsProvider               :: SMSProvider
  , padAppMode                :: PadAppMode
  , padEarchiveEnabled        :: Bool
  , legalText                 :: Bool
  , requireBPIDForNewDoc      :: Bool
  , sendTimeoutNotification   :: Bool
  , totpIsMandatory           :: Bool
  , sessionTimeoutSecs        :: Maybe Int32
  , portalUrl                 :: Maybe Text
  } deriving (Show, Eq)

data UserGroupUI = UserGroupUI
  { mailTheme     :: !(Maybe ThemeID)
  , signviewTheme :: !(Maybe ThemeID)
  , serviceTheme  :: !(Maybe ThemeID)
  , browserTitle  :: !(Maybe Text)
  , smsOriginator :: !(Maybe Text)
  , favicon       :: !(Maybe BS.ByteString)
  } deriving (Eq, Ord, Show)

data UserGroupAddress = UserGroupAddress
  { companyNumber :: Text
  , entityName    :: Text
  , address       :: Text
  , zipCode       :: Text
  , city          :: Text
  , country       :: Text
  } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''UserGroup
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupSettings
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupUI
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupAddress
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupRoot
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupWithChildren

-- INVOICING

instance Show InvoicingType where
  show InvoicingTypeNone     = "none"
  show InvoicingTypeBillItem = "billitem"
  show InvoicingTypeInvoice  = "invoice"

instance Read InvoicingType where
  readsPrec _ "none"     = [(InvoicingTypeNone, "")]
  readsPrec _ "billitem" = [(InvoicingTypeBillItem, "")]
  readsPrec _ "invoice"  = [(InvoicingTypeInvoice, "")]
  readsPrec _ _          = []

instance PQFormat InvoicingType where
  pqFormat = pqFormat @Int16

instance FromSQL InvoicingType where
  type PQBase InvoicingType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return InvoicingTypeNone
      2 -> return InvoicingTypeBillItem
      3 -> return InvoicingTypeInvoice
      _ -> E.throwIO $ RangeError { reRange = [(1, 3)], reValue = n }

instance ToSQL InvoicingType where
  type PQDest InvoicingType = PQDest Int16
  toSQL InvoicingTypeNone     = toSQL (1 :: Int16)
  toSQL InvoicingTypeBillItem = toSQL (2 :: Int16)
  toSQL InvoicingTypeInvoice  = toSQL (3 :: Int16)

instance Unjson InvoicingType where
  unjsonDef = unjsonInvmapR
    ((maybe (fail "Can't parse InvoicingType") return) . maybeRead . T.pack)
    show
    unjsonDef

ugInvoicingType :: UserGroup -> InvoicingType
ugInvoicingType ug = case ug ^. #invoicing of
  None         -> InvoicingTypeNone
  (BillItem _) -> InvoicingTypeBillItem
  (Invoice  _) -> InvoicingTypeInvoice

ugPaymentPlan :: UserGroup -> Maybe PaymentPlan
ugPaymentPlan ug = case ug ^. #invoicing of
  None           -> Nothing
  (BillItem mpp) -> mpp
  (Invoice  pp ) -> Just pp

type instance CompositeRow UserGroupInvoicing = (InvoicingType, Maybe PaymentPlan)

instance PQFormat UserGroupInvoicing where
  pqFormat = compositeTypePqFormat ctUserGroupInvoicing

instance CompositeFromSQL UserGroupInvoicing where
  toComposite (invoicing_type, mpayplan) = case (invoicing_type, mpayplan) of
    (InvoicingTypeNone, Nothing) -> None
    (InvoicingTypeBillItem, _) -> BillItem mpayplan
    (InvoicingTypeInvoice, Just payplan) -> Invoice payplan
    _ -> unexpectedError "invalid invoicing row in database"

-- USER GROUP

defaultUserGroup :: UserGroup
defaultUserGroup = ugFromUGRoot $ UserGroupRoot { id = emptyUserGroupID
                                                , name = ""
                                                , homeFolderID = Nothing
                                                , settings = defaultUserGroupSettings
                                                , paymentPlan = FreePlan
                                                , address = defaultUserGroupAddress
                                                , ui = defaultUserGroupUI
                                                , features = defaultFeatures FreePlan
                                                }

defaultChildUserGroup :: UserGroup
defaultChildUserGroup = UserGroup { id            = emptyUserGroupID
                                  , parentGroupID = Nothing
                                  , name          = ""
                                  , homeFolderID  = Nothing
                                  , settings      = Nothing
                                  , invoicing     = None
                                  , address       = Nothing
                                  , ui            = defaultUserGroupUI
                                  , features      = Nothing
                                  }


fetchUserGroup
  :: ( UserGroupID
     , Maybe UserGroupID
     , Text
     , Maybe FolderID
     , Composite UserGroupInvoicing
     , Maybe (Composite UserGroupSettings)
     , Maybe (Composite UserGroupAddress)
     , Composite UserGroupUI
     , Maybe (Composite FeatureFlags)  -- for admins
     , Maybe (Composite FeatureFlags)
     )  -- for regular users
  -> UserGroup
fetchUserGroup (id, parentGroupID, name, homeFolderID, cinvoicing, cinfos, caddresses, cuis, cAdminFeatureFlags, cRegularFeatureFlags)
  = UserGroup
    { settings  = unComposite <$> cinfos
    , invoicing = unComposite cinvoicing
    , address   = unComposite <$> caddresses
    , ui        = unComposite cuis
    , features  = Features
      <$> (unComposite <$> cAdminFeatureFlags)
      <*> (unComposite <$> cRegularFeatureFlags)
    , ..
    }

-- USER GROUP ROOT

ugrFromUG :: UserGroup -> Maybe UserGroupRoot
ugrFromUG ug = do
  -- the root of usergroup tree must have Invoice, Settings, Address, UI
  -- and Feature Flags
  paymentPlan <- case ug ^. #invoicing of
    None        -> Nothing
    BillItem _  -> Nothing         -- the root of usergroup tree must have:
    Invoice  pp -> Just pp         --   Invoice
  settings <- ug ^. #settings  --   Settings
  address  <- ug ^. #address    --   Address
  features <- ug ^. #features  --   Features
  return $ UserGroupRoot
    { id           = ug ^. #id
    , name         = ug ^. #name
    , homeFolderID = ug ^. #homeFolderID
    , ui           = ug ^. #ui
    , ..
    }

ugFromUGRoot :: UserGroupRoot -> UserGroup
ugFromUGRoot ugr = UserGroup { id            = ugr ^. #id
                             , name          = ugr ^. #name
                             , homeFolderID  = ugr ^. #homeFolderID
                             , parentGroupID = Nothing
                             , invoicing     = Invoice $ ugr ^. #paymentPlan
                             , settings      = Just $ ugr ^. #settings
                             , address       = Just $ ugr ^. #address
                             , ui            = ugr ^. #ui
                             , features      = Just $ ugr ^. #features
                             }

-- USER GROUP WITH PARENTS
ugwpOnlyParents :: UserGroupWithParents -> Maybe UserGroupWithParents
ugwpOnlyParents (_   , []                ) = Nothing -- root is leaf, nothing would be left
ugwpOnlyParents (root, (_ : parents_tail)) = Just (root, parents_tail)

ugwpInherit
  :: (UserGroupRoot -> a)
  -> (UserGroup -> Maybe a)
  -> UserGroupWithParents
  -> (UserGroupID, a)
ugwpInherit ugrProperty ugProperty (ug_root, ug_children_path) =
  fromMaybe (ug_root ^. #id, ugrProperty ug_root)
    . listToMaybe
    . catMaybes
    . map (makeIDPropertyTuple)
    $ ug_children_path
  where
    makeIDPropertyTuple ug = case ugProperty ug of
      Nothing   -> Nothing
      Just prop -> Just (ug ^. #id, prop)

ugwpPaymentPlan :: UserGroupWithParents -> PaymentPlan
ugwpPaymentPlan = snd . ugwpInherit (^. #paymentPlan) ugPaymentPlan

ugwpPaymentPlanWithID :: UserGroupWithParents -> (UserGroupID, PaymentPlan)
ugwpPaymentPlanWithID = ugwpInherit (^. #paymentPlan) ugPaymentPlan

ugwpSettings :: UserGroupWithParents -> UserGroupSettings
ugwpSettings = snd . ugwpInherit (^. #settings) (^. #settings)

ugwpSettingsWithID :: UserGroupWithParents -> (UserGroupID, UserGroupSettings)
ugwpSettingsWithID = ugwpInherit (^. #settings) (^. #settings)

ugwpAddress :: UserGroupWithParents -> UserGroupAddress
ugwpAddress = snd . ugwpInherit (^. #address) (^. #address)

ugwpAddressWithID :: UserGroupWithParents -> (UserGroupID, UserGroupAddress)
ugwpAddressWithID = ugwpInherit (^. #address) (^. #address)

ugwpFeatures :: UserGroupWithParents -> Features
ugwpFeatures = snd . ugwpInherit (^. #features) (^. #features)

ugwpFeaturesWithID :: UserGroupWithParents -> (UserGroupID, Features)
ugwpFeaturesWithID = ugwpInherit (^. #features) (^. #features)

ugwpToList :: UserGroupWithParents -> [UserGroup]
ugwpToList (ug_root, ug_children_path) = ug_children_path ++ [ugFromUGRoot ug_root]

ugwpUG :: UserGroupWithParents -> UserGroup
ugwpUG (root, []      ) = ugFromUGRoot root
ugwpUG (_   , (ug : _)) = ug

ugwpRoot :: UserGroupWithParents -> UserGroup
ugwpRoot (root, _) = ugFromUGRoot root

ugwpAddChild :: UserGroup -> UserGroupWithParents -> UserGroupWithParents
ugwpAddChild ug (root, children_path) = (root, ug : children_path)

ugwcToList :: [UserGroupWithChildren] -> [UserGroup]
ugwcToList ugwcs =
  concat . for ugwcs $ \ugwc -> ugwc ^. #group : ugwcToList (ugwc ^. #children)

-- USER GROUP INFO

defaultUserGroupSettings :: UserGroupSettings
defaultUserGroupSettings = UserGroupSettings
  { ipAddressMaskList       = []
  , dataRetentionPolicy     = defaultDataRetentionPolicy
  , cgiDisplayName          = Nothing
  , cgiServiceID            = Nothing
  , smsProvider             = SMSDefault
  , padAppMode              = ListView
  , padEarchiveEnabled      = True
  , legalText               = False
  , requireBPIDForNewDoc    = False
  , sendTimeoutNotification = False
  , totpIsMandatory         = False
  , sessionTimeoutSecs      = Nothing
  , portalUrl               = Nothing
  }

type instance CompositeRow UserGroupSettings
  = ( Maybe Text
    , Maybe Int16
    , Maybe Int16
    , Maybe Int16
    , Maybe Int16
    , Maybe Int16
    , Maybe Int16
    , Bool
    , Maybe Text
    , SMSProvider
    , Maybe Text
    , PadAppMode
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Bool
    , Maybe Int32
    , Maybe Text
    )

instance PQFormat UserGroupSettings where
  pqFormat = compositeTypePqFormat ctUserGroupSettings

instance CompositeFromSQL UserGroupSettings where
  toComposite (ip_address_mask_list, drpIdleDocTimeoutPreparation, drpIdleDocTimeoutClosed, drpIdleDocTimeoutCanceled, drpIdleDocTimeoutTimedout, drpIdleDocTimeoutRejected, drpIdleDocTimeoutError, drpImmediateTrash, cgiDisplayName, smsProvider, cgiServiceID, padAppMode, padEarchiveEnabled, legalText, requireBPIDForNewDoc, sendTimeoutNotification, _useFolderListCalls -- not yet used
                                                                                                                                                                                                                                                                                                                                                                                                        , totpIsMandatory, sessionTimeoutSecs, portalUrl)
    = UserGroupSettings
      { ipAddressMaskList   = maybe [] read ip_address_mask_list
      , dataRetentionPolicy = DataRetentionPolicy { .. }
      , ..
      }

-- UI

defaultUserGroupUI :: UserGroupUI
defaultUserGroupUI = UserGroupUI { mailTheme     = Nothing
                                 , signviewTheme = Nothing
                                 , serviceTheme  = Nothing
                                 , browserTitle  = Nothing
                                 , smsOriginator = Nothing
                                 , favicon       = Nothing
                                 }

type instance CompositeRow UserGroupUI
  = ( Maybe ThemeID
    , Maybe ThemeID
    , Maybe ThemeID
    , Maybe Text
    , Maybe Text
    , Maybe BS.ByteString
    )

instance PQFormat UserGroupUI where
  pqFormat = compositeTypePqFormat ctUserGroupUI

instance CompositeFromSQL UserGroupUI where
  toComposite (mail_theme, signview_theme, service_theme, browser_title, sms_originator, favicon)
    = UserGroupUI { mailTheme     = mail_theme
                  , signviewTheme = signview_theme
                  , serviceTheme  = service_theme
                  , browserTitle  = browser_title
                  , smsOriginator = sms_originator
                  , favicon       = faviconFromBinary favicon
                  }
    where
      faviconFromBinary (Just f) = if (BS.null f) then Nothing else Just f
      -- We should interpret empty logos as no logos.
      faviconFromBinary Nothing  = Nothing

-- ADDRESS

defaultUserGroupAddress :: UserGroupAddress
defaultUserGroupAddress = UserGroupAddress { companyNumber = ""
                                           , entityName    = ""
                                           , address       = ""
                                           , zipCode       = ""
                                           , city          = ""
                                           , country       = ""
                                           }

type instance CompositeRow UserGroupAddress = (Text, Text, Text, Text, Text, Text)

instance PQFormat UserGroupAddress where
  pqFormat = compositeTypePqFormat ctUserGroupAddress

instance CompositeFromSQL UserGroupAddress where
  toComposite (companyNumber, entityName, address, zipCode, city, country) =
    UserGroupAddress {..}
