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
  , UserGroupID(..)
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

data UserGroup = UserGroup {
    ugID            :: UserGroupID
  , ugParentGroupID :: Maybe UserGroupID
  , ugName          :: Text
  -- Folder, where home folders are created for new users
  -- it is a Maybe for slow migration purposes after that
  -- the Maybe will be removed
  -- The Maybe can be re-introduced, when we implement home folder inheritance
  , ugHomeFolderID  :: Maybe FolderID
  , ugAddress       :: Maybe UserGroupAddress
  , ugSettings      :: Maybe UserGroupSettings
  , ugInvoicing     :: UserGroupInvoicing
  , ugUI            :: UserGroupUI
  , ugFeatures      :: Maybe Features
  } deriving (Show, Eq)

data UserGroupRoot = UserGroupRoot
  { ugrID            :: UserGroupID
  , ugrName          :: Text
  , ugrHomeFolderID  :: Maybe FolderID
  , ugrAddress       :: UserGroupAddress
  , ugrSettings      :: UserGroupSettings
  , ugrPaymentPlan   :: PaymentPlan  -- user group root always must have Invoice
  , ugrUI            :: UserGroupUI
  , ugrFeatures      :: Features
  } deriving (Show, Eq)

-- UserGroup list is ordered from Leaf to Child of Root)
type UserGroupWithParents = (UserGroupRoot, [UserGroup])

-- UserGroup and all its children down to the bottom
data UserGroupWithChildren = UserGroupWithChildren
  { ugwcGroup :: UserGroup
  , ugwcChildren :: [UserGroupWithChildren]
  } deriving (Eq, Show)

defaultUserGroup :: UserGroup
defaultUserGroup = ugFromUGRoot $ UserGroupRoot { ugrID = emptyUserGroupID
                                                , ugrName = ""
                                                , ugrHomeFolderID = Nothing
                                                , ugrSettings = defaultUserGroupSettings
                                                , ugrPaymentPlan = FreePlan
                                                , ugrAddress = defaultUserGroupAddress
                                                , ugrUI = defaultUserGroupUI
                                                , ugrFeatures = defaultFeatures FreePlan
                                                }

defaultChildUserGroup :: UserGroup
defaultChildUserGroup = UserGroup { ugID            = emptyUserGroupID
                                  , ugParentGroupID = Nothing
                                  , ugName          = ""
                                  , ugHomeFolderID  = Nothing
                                  , ugSettings      = Nothing
                                  , ugInvoicing     = None
                                  , ugAddress       = Nothing
                                  , ugUI            = defaultUserGroupUI
                                  , ugFeatures      = Nothing
                                  }

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
  { ugsIPAddressMaskList         :: [IPAddressWithMask]
  , ugsDataRetentionPolicy       :: DataRetentionPolicy
  , ugsCGIDisplayName            :: Maybe Text
  , ugsCGIServiceID              :: Maybe Text
  , ugsSMSProvider               :: SMSProvider
  , ugsPadAppMode                :: PadAppMode
  , ugsPadEarchiveEnabled        :: Bool
  , ugsLegalText                 :: Bool
  , ugsRequireBPIDForNewDoc      :: Bool
  , ugsSendTimeoutNotification   :: Bool
  , ugsTotpIsMandatory           :: Bool
  , ugsSessionTimeoutSecs        :: Maybe Int32
  , ugsPortalUrl                 :: Maybe Text
  } deriving (Show, Eq)

defaultUserGroupSettings :: UserGroupSettings
defaultUserGroupSettings = UserGroupSettings
  { ugsIPAddressMaskList       = []
  , ugsDataRetentionPolicy     = defaultDataRetentionPolicy
  , ugsCGIDisplayName          = Nothing
  , ugsCGIServiceID            = Nothing
  , ugsSMSProvider             = SMSDefault
  , ugsPadAppMode              = ListView
  , ugsPadEarchiveEnabled      = True
  , ugsLegalText               = False
  , ugsRequireBPIDForNewDoc    = False
  , ugsSendTimeoutNotification = False
  , ugsTotpIsMandatory         = False
  , ugsSessionTimeoutSecs      = Nothing
  , ugsPortalUrl               = Nothing
  }

data UserGroupUI = UserGroupUI
  { uguiMailTheme     :: !(Maybe ThemeID)
  , uguiSignviewTheme :: !(Maybe ThemeID)
  , uguiServiceTheme  :: !(Maybe ThemeID)
  , uguiBrowserTitle  :: !(Maybe Text)
  , uguiSmsOriginator :: !(Maybe Text)
  , uguiFavicon       :: !(Maybe BS.ByteString)
  } deriving (Eq, Ord, Show)

defaultUserGroupUI :: UserGroupUI
defaultUserGroupUI = UserGroupUI { uguiMailTheme     = Nothing
                                 , uguiSignviewTheme = Nothing
                                 , uguiServiceTheme  = Nothing
                                 , uguiBrowserTitle  = Nothing
                                 , uguiSmsOriginator = Nothing
                                 , uguiFavicon       = Nothing
                                 }

data UserGroupAddress = UserGroupAddress
  { ugaCompanyNumber :: Text
  , ugaEntityName    :: Text
  , ugaAddress       :: Text
  , ugaZip           :: Text
  , ugaCity          :: Text
  , ugaCountry       :: Text
  } deriving (Eq, Ord, Show)

defaultUserGroupAddress :: UserGroupAddress
defaultUserGroupAddress = UserGroupAddress { ugaCompanyNumber = ""
                                           , ugaEntityName    = ""
                                           , ugaAddress       = ""
                                           , ugaZip           = ""
                                           , ugaCity          = ""
                                           , ugaCountry       = ""
                                           }

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
ugInvoicingType ug = case ugInvoicing ug of
  None         -> InvoicingTypeNone
  (BillItem _) -> InvoicingTypeBillItem
  (Invoice  _) -> InvoicingTypeInvoice

ugPaymentPlan :: UserGroup -> Maybe PaymentPlan
ugPaymentPlan ug = case ugInvoicing ug of
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
fetchUserGroup (ugID, ugParentGroupID, ugName, ugHomeFolderID, cinvoicing, cinfos, caddresses, cuis, cAdminFeatureFlags, cRegularFeatureFlags)
  = UserGroup
    { ugSettings  = unComposite <$> cinfos
    , ugInvoicing = unComposite cinvoicing
    , ugAddress   = unComposite <$> caddresses
    , ugUI        = unComposite cuis
    , ugFeatures  = Features
      <$> (unComposite <$> cAdminFeatureFlags)
      <*> (unComposite <$> cRegularFeatureFlags)
    , ..
    }

-- USER GROUP ROOT

ugrFromUG :: UserGroup -> Maybe UserGroupRoot
ugrFromUG ug = do
  -- the root of usergroup tree must have Invoice, Settings, Address, UI
  -- and Feature Flags
  ugrPaymentPlan <- case ugInvoicing ug of
    None        -> Nothing
    BillItem _  -> Nothing         -- the root of usergroup tree must have:
    Invoice  pp -> Just pp         --   Invoice
  ugrSettings <- ugSettings ug  --   Settings
  ugrAddress  <- ugAddress ug    --   Address
  ugrFeatures <- ugFeatures ug  --   Features
  return $ UserGroupRoot
    { ugrID           = ugID ug
    , ugrName         = ugName ug
    , ugrHomeFolderID = ugHomeFolderID ug
    , ugrUI           = ugUI ug
    , ..
    }

ugFromUGRoot :: UserGroupRoot -> UserGroup
ugFromUGRoot ugr = UserGroup { ugID            = ugrID ugr
                             , ugName          = ugrName ugr
                             , ugHomeFolderID  = ugrHomeFolderID ugr
                             , ugParentGroupID = Nothing
                             , ugInvoicing     = Invoice . ugrPaymentPlan $ ugr
                             , ugSettings      = Just $ ugrSettings ugr
                             , ugAddress       = Just $ ugrAddress ugr
                             , ugUI            = ugrUI ugr
                             , ugFeatures      = Just $ ugrFeatures ugr
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
  fromMaybe (ugrID ug_root, ugrProperty ug_root)
    . listToMaybe
    . catMaybes
    . map (makeIDPropertyTuple)
    $ ug_children_path
  where
    makeIDPropertyTuple ug = case ugProperty ug of
      Nothing   -> Nothing
      Just prop -> Just (ugID ug, prop)

ugwpPaymentPlan :: UserGroupWithParents -> PaymentPlan
ugwpPaymentPlan = snd . ugwpInherit ugrPaymentPlan ugPaymentPlan

ugwpPaymentPlanWithID :: UserGroupWithParents -> (UserGroupID, PaymentPlan)
ugwpPaymentPlanWithID = ugwpInherit ugrPaymentPlan ugPaymentPlan

ugwpSettings :: UserGroupWithParents -> UserGroupSettings
ugwpSettings = snd . ugwpInherit ugrSettings ugSettings

ugwpSettingsWithID :: UserGroupWithParents -> (UserGroupID, UserGroupSettings)
ugwpSettingsWithID = ugwpInherit ugrSettings ugSettings

ugwpAddress :: UserGroupWithParents -> UserGroupAddress
ugwpAddress = snd . ugwpInherit ugrAddress ugAddress

ugwpAddressWithID :: UserGroupWithParents -> (UserGroupID, UserGroupAddress)
ugwpAddressWithID = ugwpInherit ugrAddress ugAddress

ugwpFeatures :: UserGroupWithParents -> Features
ugwpFeatures = snd . ugwpInherit ugrFeatures ugFeatures

ugwpFeaturesWithID :: UserGroupWithParents -> (UserGroupID, Features)
ugwpFeaturesWithID = ugwpInherit ugrFeatures ugFeatures

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
  concat . for ugwcs $ \ugwc -> ugwcGroup ugwc : ugwcToList (ugwcChildren ugwc)

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

-- USER GROUP INFO

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
  toComposite (ip_address_mask_list, drpIdleDocTimeoutPreparation, drpIdleDocTimeoutClosed, drpIdleDocTimeoutCanceled, drpIdleDocTimeoutTimedout, drpIdleDocTimeoutRejected, drpIdleDocTimeoutError, drpImmediateTrash, ugsCGIDisplayName, ugsSMSProvider, ugsCGIServiceID, ugsPadAppMode, ugsPadEarchiveEnabled, ugsLegalText, ugsRequireBPIDForNewDoc, ugsSendTimeoutNotification, _useFolderListCalls -- not yet used
                                                                                                                                                                                                                                                                                                                                                                                                                       , ugsTotpIsMandatory, ugsSessionTimeoutSecs, ugsPortalUrl)
    = UserGroupSettings
      { ugsIPAddressMaskList   = maybe [] read ip_address_mask_list
      , ugsDataRetentionPolicy = DataRetentionPolicy { .. }
      , ..
      }

-- UI

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
    = UserGroupUI { uguiMailTheme     = mail_theme
                  , uguiSignviewTheme = signview_theme
                  , uguiServiceTheme  = service_theme
                  , uguiBrowserTitle  = browser_title
                  , uguiSmsOriginator = sms_originator
                  , uguiFavicon       = faviconFromBinary favicon
                  }
    where
      faviconFromBinary (Just f) = if (BS.null f) then Nothing else Just f
      -- We should interpret empty logos as no logos.
      faviconFromBinary Nothing  = Nothing

-- ADDRESS

type instance CompositeRow UserGroupAddress = (Text, Text, Text, Text, Text, Text)

instance PQFormat UserGroupAddress where
  pqFormat = compositeTypePqFormat ctUserGroupAddress

instance CompositeFromSQL UserGroupAddress where
  toComposite (company_number, entity_name, address, zip_code, city, country) =
    UserGroupAddress { ugaCompanyNumber = company_number
                     , ugaEntityName    = entity_name
                     , ugaAddress       = address
                     , ugaZip           = zip_code
                     , ugaCity          = city
                     , ugaCountry       = country
                     }
