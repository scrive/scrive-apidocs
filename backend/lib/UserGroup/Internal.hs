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
    _ugID            :: UserGroupID
  , _ugParentGroupID :: Maybe UserGroupID
  , _ugName          :: Text
  -- Folder, where home folders are created for new users
  -- it is a Maybe for slow migration purposes after that
  -- the Maybe will be removed
  -- The Maybe can be re-introduced, when we implement home folder inheritance
  , _ugHomeFolderID  :: Maybe FolderID
  , _ugAddress       :: Maybe UserGroupAddress
  , _ugSettings      :: Maybe UserGroupSettings
  , _ugInvoicing     :: UserGroupInvoicing
  , _ugUI            :: UserGroupUI
  , _ugFeatures      :: Maybe Features
  } deriving (Show, Eq)

data UserGroupRoot = UserGroupRoot
  { _ugrID            :: UserGroupID
  , _ugrName          :: Text
  , _ugrHomeFolderID  :: Maybe FolderID
  , _ugrAddress       :: UserGroupAddress
  , _ugrSettings      :: UserGroupSettings
  , _ugrPaymentPlan   :: PaymentPlan  -- user group root always must have Invoice
  , _ugrUI            :: UserGroupUI
  , _ugrFeatures      :: Features
  } deriving (Show, Eq)

-- UserGroup list is ordered from Leaf to Child of Root)
type UserGroupWithParents = (UserGroupRoot, [UserGroup])

-- UserGroup and all its children down to the bottom
data UserGroupWithChildren = UserGroupWithChildren
  { _ugwcGroup :: UserGroup
  , _ugwcChildren :: [UserGroupWithChildren]
  } deriving (Eq, Show)

defaultUserGroup :: UserGroup
defaultUserGroup = ugFromUGRoot $ UserGroupRoot {
    _ugrID = emptyUserGroupID
  , _ugrName = ""
  , _ugrHomeFolderID = Nothing
  , _ugrSettings = defaultUserGroupSettings
  , _ugrPaymentPlan = FreePlan
  , _ugrAddress = defaultUserGroupAddress
  , _ugrUI = defaultUserGroupUI
  , _ugrFeatures = defaultFeatures FreePlan
  }

defaultChildUserGroup :: UserGroup
defaultChildUserGroup = UserGroup {
    _ugID = emptyUserGroupID
  , _ugParentGroupID = Nothing
  , _ugName = ""
  , _ugHomeFolderID = Nothing
  , _ugSettings = Nothing
  , _ugInvoicing = None
  , _ugAddress = Nothing
  , _ugUI = defaultUserGroupUI
  , _ugFeatures = Nothing
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

data UserGroupSettings = UserGroupSettings {
    _ugsIPAddressMaskList    :: [IPAddressWithMask]
  , _ugsDataRetentionPolicy  :: DataRetentionPolicy
  , _ugsCGIDisplayName       :: Maybe Text
  , _ugsCGIServiceID         :: Maybe Text
  , _ugsSMSProvider          :: SMSProvider
  , _ugsPadAppMode           :: PadAppMode
  , _ugsPadEarchiveEnabled   :: Bool
  , _ugsLegalText            :: Bool
  , _ugsRequireBPIDForNewDoc :: Bool
  , _ugsSendTimeoutNotification :: Bool
  , _ugsTotpIsMandatory      :: Bool
  } deriving (Show, Eq)

defaultUserGroupSettings :: UserGroupSettings
defaultUserGroupSettings = UserGroupSettings {
    _ugsIPAddressMaskList    = []
  , _ugsDataRetentionPolicy  = defaultDataRetentionPolicy
  , _ugsCGIDisplayName       = Nothing
  , _ugsCGIServiceID         = Nothing
  , _ugsSMSProvider          = SMSDefault
  , _ugsPadAppMode           = ListView
  , _ugsPadEarchiveEnabled   = True
  , _ugsLegalText            = False
  , _ugsRequireBPIDForNewDoc = False
  , _ugsSendTimeoutNotification = False
  , _ugsTotpIsMandatory      = False
  }

data UserGroupUI = UserGroupUI {
    _uguiMailTheme     :: !(Maybe ThemeID)
  , _uguiSignviewTheme :: !(Maybe ThemeID)
  , _uguiServiceTheme  :: !(Maybe ThemeID)
  , _uguiBrowserTitle  :: !(Maybe Text)
  , _uguiSmsOriginator :: !(Maybe Text)
  , _uguiFavicon       :: !(Maybe BS.ByteString)
} deriving (Eq, Ord, Show)

defaultUserGroupUI :: UserGroupUI
defaultUserGroupUI = UserGroupUI {
    _uguiMailTheme     = Nothing
  , _uguiSignviewTheme = Nothing
  , _uguiServiceTheme  = Nothing
  , _uguiBrowserTitle  = Nothing
  , _uguiSmsOriginator = Nothing
  , _uguiFavicon       = Nothing
  }

data UserGroupAddress = UserGroupAddress {
    _ugaCompanyNumber :: Text
  , _ugaAddress       :: Text
  , _ugaZip           :: Text
  , _ugaCity          :: Text
  , _ugaCountry       :: Text
  } deriving (Eq, Ord, Show)

defaultUserGroupAddress :: UserGroupAddress
defaultUserGroupAddress = UserGroupAddress {
    _ugaCompanyNumber = ""
  , _ugaAddress       = ""
  , _ugaZip           = ""
  , _ugaCity          = ""
  , _ugaCountry       = ""
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
  readsPrec _ _  = []

instance PQFormat InvoicingType where
  pqFormat = pqFormat @Int16

instance FromSQL InvoicingType where
  type PQBase InvoicingType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return InvoicingTypeNone
      2  -> return InvoicingTypeBillItem
      3  -> return InvoicingTypeInvoice
      _  -> E.throwIO $ RangeError {
        reRange = [(1, 3)]
      , reValue = n
      }

instance ToSQL InvoicingType where
  type PQDest InvoicingType = PQDest Int16
  toSQL InvoicingTypeNone     = toSQL (1::Int16)
  toSQL InvoicingTypeBillItem = toSQL (2::Int16)
  toSQL InvoicingTypeInvoice  = toSQL (3::Int16)

instance Unjson InvoicingType where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse InvoicingType") return) . maybeRead . T.pack)
                show unjsonDef

ugInvoicingType :: UserGroup -> InvoicingType
ugInvoicingType ug = case _ugInvoicing ug of
  None         -> InvoicingTypeNone
  (BillItem _) -> InvoicingTypeBillItem
  (Invoice _)  -> InvoicingTypeInvoice

ugPaymentPlan :: UserGroup -> Maybe PaymentPlan
ugPaymentPlan ug = case _ugInvoicing ug of
  None           -> Nothing
  (BillItem mpp) -> mpp
  (Invoice pp)   -> Just pp

type instance CompositeRow UserGroupInvoicing = (InvoicingType, Maybe PaymentPlan)

instance PQFormat UserGroupInvoicing where
  pqFormat = compositeTypePqFormat ctUserGroupInvoicing

instance CompositeFromSQL UserGroupInvoicing where
  toComposite (invoicing_type, mpayplan) = case (invoicing_type, mpayplan) of
    (InvoicingTypeNone    , Nothing     ) -> None
    (InvoicingTypeBillItem, _           ) -> BillItem mpayplan
    (InvoicingTypeInvoice , Just payplan) -> Invoice payplan
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
    , Maybe (Composite FeatureFlags)  -- for regular users
    ) -> UserGroup
fetchUserGroup
  ( _ugID
  , _ugParentGroupID
  , _ugName
  , _ugHomeFolderID
  , cinvoicing
  , cinfos
  , caddresses
  , cuis
  , cAdminFeatureFlags
  , cRegularFeatureFlags
  ) =
  UserGroup
  { _ugSettings  = unComposite <$> cinfos
  , _ugInvoicing = unComposite cinvoicing
  , _ugAddress   = unComposite <$> caddresses
  , _ugUI        = unComposite cuis
  , _ugFeatures  = Features
    <$> (unComposite <$> cAdminFeatureFlags)
    <*> (unComposite <$> cRegularFeatureFlags)
  , ..
  }

-- USER GROUP ROOT

ugrFromUG :: UserGroup -> Maybe UserGroupRoot
ugrFromUG ug = do
  -- the root of usergroup tree must have Invoice, Settings, Address, UI
  -- and Feature Flags
  _ugrPaymentPlan <- case _ugInvoicing ug of
    None -> Nothing
    BillItem _ -> Nothing         -- the root of usergroup tree must have:
    Invoice pp -> Just pp         --   Invoice
  _ugrSettings <- _ugSettings ug  --   Settings
  _ugrAddress <- _ugAddress ug    --   Address
  _ugrFeatures <- _ugFeatures ug  --   Features
  return $ UserGroupRoot
    { _ugrID = _ugID ug
    , _ugrName = _ugName ug
    , _ugrHomeFolderID = _ugHomeFolderID ug
    , _ugrUI = _ugUI ug
    , ..
    }

ugFromUGRoot :: UserGroupRoot -> UserGroup
ugFromUGRoot ugr = UserGroup
  { _ugID = _ugrID ugr
  , _ugName = _ugrName ugr
  , _ugHomeFolderID = _ugrHomeFolderID ugr
  , _ugParentGroupID = Nothing
  , _ugInvoicing = Invoice . _ugrPaymentPlan $ ugr
  , _ugSettings = Just $ _ugrSettings ugr
  , _ugAddress = Just $ _ugrAddress ugr
  , _ugUI = _ugrUI ugr
  , _ugFeatures = Just $ _ugrFeatures ugr
  }

-- USER GROUP WITH PARENTS
ugwpOnlyParents :: UserGroupWithParents -> Maybe UserGroupWithParents
ugwpOnlyParents (_,[]) = Nothing -- root is leaf, nothing would be left
ugwpOnlyParents (root, (_:parents_tail)) = Just (root, parents_tail)

ugwpInherit :: (UserGroupRoot -> a) -> (UserGroup -> Maybe a) -> UserGroupWithParents -> (UserGroupID, a)
ugwpInherit ugrProperty ugProperty (ug_root, ug_children_path) =
  fromMaybe (_ugrID ug_root, ugrProperty ug_root) . listToMaybe . catMaybes
    . map (makeIDPropertyTuple) $ ug_children_path
      where
        makeIDPropertyTuple ug = case ugProperty ug of
          Nothing -> Nothing
          Just prop -> Just (_ugID ug, prop)

ugwpPaymentPlan :: UserGroupWithParents -> PaymentPlan
ugwpPaymentPlan = snd . ugwpInherit _ugrPaymentPlan ugPaymentPlan

ugwpPaymentPlanWithID :: UserGroupWithParents -> (UserGroupID, PaymentPlan)
ugwpPaymentPlanWithID = ugwpInherit _ugrPaymentPlan ugPaymentPlan

ugwpSettings :: UserGroupWithParents -> UserGroupSettings
ugwpSettings = snd . ugwpInherit _ugrSettings _ugSettings

ugwpSettingsWithID :: UserGroupWithParents -> (UserGroupID, UserGroupSettings)
ugwpSettingsWithID = ugwpInherit _ugrSettings _ugSettings

ugwpAddress :: UserGroupWithParents -> UserGroupAddress
ugwpAddress = snd . ugwpInherit _ugrAddress _ugAddress

ugwpAddressWithID :: UserGroupWithParents -> (UserGroupID, UserGroupAddress)
ugwpAddressWithID = ugwpInherit _ugrAddress _ugAddress

ugwpFeatures :: UserGroupWithParents -> Features
ugwpFeatures = snd . ugwpInherit _ugrFeatures _ugFeatures

ugwpFeaturesWithID :: UserGroupWithParents -> (UserGroupID, Features)
ugwpFeaturesWithID = ugwpInherit _ugrFeatures _ugFeatures

ugwpToList :: UserGroupWithParents -> [UserGroup]
ugwpToList (ug_root, ug_children_path) =
  ug_children_path ++ [ugFromUGRoot ug_root]

ugwpUG :: UserGroupWithParents -> UserGroup
ugwpUG (root, [])  = ugFromUGRoot root
ugwpUG (_, (ug:_)) = ug

ugwpRoot :: UserGroupWithParents -> UserGroup
ugwpRoot (root, _) = ugFromUGRoot root

ugwpAddChild :: UserGroup -> UserGroupWithParents -> UserGroupWithParents
ugwpAddChild ug (root, children_path) = (root, ug:children_path)

ugwcToList :: [UserGroupWithChildren] -> [UserGroup]
ugwcToList ugwcs = concat . for ugwcs $ \ugwc ->
  _ugwcGroup ugwc : ugwcToList (_ugwcChildren ugwc)

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
      Nothing -> fail "Could not parse User Group ID"
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
  idDefaultLabel          = "user_group_id"
  idValue (UserGroupID k) = int64AsStringIdentifier k

instance B.Binary UserGroupID where
  put (UserGroupID ugid) = B.put ugid
  get = fmap UserGroupID B.get

instance Unjson UserGroupID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse UserGroupID")  return) . maybeRead) showt unjsonDef

-- USER GROUP INFO

type instance CompositeRow UserGroupSettings = (
    Maybe Text
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
  )

instance PQFormat UserGroupSettings where
  pqFormat = compositeTypePqFormat ctUserGroupSettings

instance CompositeFromSQL UserGroupSettings where
  toComposite (
      ip_address_mask_list
    , _drpIdleDocTimeoutPreparation
    , _drpIdleDocTimeoutClosed
    , _drpIdleDocTimeoutCanceled
    , _drpIdleDocTimeoutTimedout
    , _drpIdleDocTimeoutRejected
    , _drpIdleDocTimeoutError
    , _drpImmediateTrash
    , _ugsCGIDisplayName
    , _ugsSMSProvider
    , _ugsCGIServiceID
    , _ugsPadAppMode
    , _ugsPadEarchiveEnabled
    , _ugsLegalText
    , _ugsRequireBPIDForNewDoc
    , _ugsSendTimeoutNotification
    , _useFolderListCalls -- not yet used
    , _ugsTotpIsMandatory
    ) = UserGroupSettings {
      _ugsIPAddressMaskList         = maybe [] read ip_address_mask_list
    , _ugsDataRetentionPolicy = DataRetentionPolicy {..}
    , ..
    }

-- UI

type instance CompositeRow UserGroupUI = (
    Maybe ThemeID
  , Maybe ThemeID
  , Maybe ThemeID
  , Maybe Text
  , Maybe Text
  , Maybe BS.ByteString
  )

instance PQFormat UserGroupUI where
  pqFormat = compositeTypePqFormat ctUserGroupUI

instance CompositeFromSQL UserGroupUI where
  toComposite (
      mail_theme
    , signview_theme
    , service_theme
    , browser_title
    , sms_originator
    , favicon
    ) = UserGroupUI {
      _uguiMailTheme     = mail_theme
    , _uguiSignviewTheme = signview_theme
    , _uguiServiceTheme  = service_theme
    , _uguiBrowserTitle  = browser_title
    , _uguiSmsOriginator = sms_originator
    , _uguiFavicon       = faviconFromBinary favicon
    }
    where
    -- We should interpret empty logos as no logos.
    faviconFromBinary (Just f) = if (BS.null f) then Nothing else Just f
    faviconFromBinary Nothing = Nothing

-- ADDRESS

type instance CompositeRow UserGroupAddress = (
    Text
  , Text
  , Text
  , Text
  , Text
  )

instance PQFormat UserGroupAddress where
  pqFormat = compositeTypePqFormat ctUserGroupAddress

instance CompositeFromSQL UserGroupAddress where
  toComposite (
      company_number
    , address
    , zip_code
    , city
    , country
    ) = UserGroupAddress {
      _ugaCompanyNumber = company_number
    , _ugaAddress       = address
    , _ugaZip           = zip_code
    , _ugaCity          = city
    , _ugaCountry       = country
    }
