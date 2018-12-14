module UserGroup.Internal (
    InvoicingType(..)
  , UserGroup(..)
  , ugInvoicingType
  , ugPaymentPlan
  , ugwpOnlyParents
  , ugwpPaymentPlan
  , ugwpSettings
  , ugwpAddress
  , ugwpToList
  , ugwpUG
  , ugwpAddChild
  , ugrFromUG
  , UserGroupID(..)
  , emptyUserGroupID
  , unsafeUserGroupID
  , fromUserGroupID
  , UserGroupSettings(..)
  , UserGroupAddress(..)
  , UserGroupUI(..)
  , UserGroupInvoicing(..)
  , UserGroupWithParents
  , UserGroupRoot(..)
  , ugFromUGRoot
  ) where

import Data.Binary
import Data.Default
import Data.Int
import Data.Text (Text)
import Data.Unjson
import Happstack.Server
import qualified Control.Exception.Lifted as E
import qualified Data.Binary as B
import qualified Data.ByteString.Char8 as BS

import DataRetentionPolicy
import DB
import IPAddress
import Log.Identifier
import PadApplication.Types
import SMS.Types
import Theme.ThemeID
import UserGroup.Types.PaymentPlan

data UserGroup = UserGroup {
    _ugID            :: UserGroupID
  , _ugParentGroupID :: Maybe UserGroupID
  , _ugName          :: Text
  , _ugAddress       :: Maybe UserGroupAddress
  , _ugSettings      :: Maybe UserGroupSettings
  , _ugInvoicing     :: UserGroupInvoicing
  , _ugUI            :: UserGroupUI
  } deriving (Show, Eq)

data UserGroupRoot = UserGroupRoot
  { _ugrID            :: UserGroupID
  , _ugrName          :: Text
  , _ugrAddress       :: UserGroupAddress
  , _ugrSettings      :: UserGroupSettings
  , _ugrPaymentPlan   :: PaymentPlan  -- user group root always must have Invoice
  , _ugrUI            :: UserGroupUI
  } deriving (Show, Eq)

-- UserGroup list is ordered from Leaf to Child of Root)
type UserGroupWithParents = (UserGroupRoot, [UserGroup])

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
    _ugsIPAddressMaskList   :: [IPAddressWithMask]
  , _ugsDataRetentionPolicy :: DataRetentionPolicy
  , _ugsCGIDisplayName      :: Maybe Text
  , _ugsCGIServiceID        :: Maybe Text
  , _ugsSMSProvider         :: SMSProvider
  , _ugsPadAppMode          :: PadAppMode
  , _ugsPadEarchiveEnabled  :: Bool
  , _ugsLegalText           :: Bool
  } deriving (Show, Eq)

data UserGroupUI = UserGroupUI {
    _uguiMailTheme     :: !(Maybe ThemeID)
  , _uguiSignviewTheme :: !(Maybe ThemeID)
  , _uguiServiceTheme  :: !(Maybe ThemeID)
  , _uguiBrowserTitle  :: !(Maybe Text)
  , _uguiSmsOriginator :: !(Maybe Text)
  , _uguiFavicon       :: !(Maybe BS.ByteString)
} deriving (Eq, Ord, Show)

data UserGroupAddress = UserGroupAddress {
    _ugaCompanyNumber :: Text
  , _ugaAddress       :: Text
  , _ugaZip           :: Text
  , _ugaCity          :: Text
  , _ugaCountry       :: Text
  } deriving (Eq, Ord, Show)

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
                ((maybe (fail "Can't parse InvoicingType") return) . maybeRead)
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
  pqFormat = "%user_group_invoicing"

instance CompositeFromSQL UserGroupInvoicing where
  toComposite (invoicing_type, mpayplan) = case (invoicing_type, mpayplan) of
    (InvoicingTypeNone    , Nothing     ) -> None
    (InvoicingTypeBillItem, _           ) -> BillItem mpayplan
    (InvoicingTypeInvoice , Just payplan) -> Invoice payplan
    _ -> unexpectedError "invalid invoicing row in database"

-- USER GROUP

type instance CompositeRow UserGroup = (
    UserGroupID
  , Maybe UserGroupID
  , Text
  , Composite UserGroupInvoicing
  , Maybe (Composite UserGroupSettings)
  , Maybe (Composite UserGroupAddress)
  , Composite UserGroupUI
  )

instance PQFormat UserGroup where
  pqFormat = "%user_group"

instance CompositeFromSQL UserGroup where
  toComposite
    ( _ugID
    , _ugParentGroupID
    , _ugName
    , cinvoicing
    , cinfos
    , caddresses
    , cuis) =
    UserGroup
    { _ugSettings  = unComposite <$> cinfos
    , _ugInvoicing = unComposite cinvoicing
    , _ugAddress   = unComposite <$> caddresses
    , _ugUI        = unComposite cuis
    , ..
    }

instance Default UserGroup where
  def = UserGroup {
      _ugID = emptyUserGroupID
    , _ugParentGroupID = Nothing
    , _ugName = ""
    , _ugSettings = Just $ UserGroupSettings {
        _ugsIPAddressMaskList   = []
      , _ugsDataRetentionPolicy = defaultDataRetentionPolicy
      , _ugsCGIDisplayName      = Nothing
      , _ugsCGIServiceID        = Nothing
      , _ugsSMSProvider         = SMSDefault
      , _ugsPadAppMode          = ListView
      , _ugsPadEarchiveEnabled  = True
      , _ugsLegalText           = False
      }
    , _ugInvoicing = Invoice FreePlan
    , _ugAddress = Just $ UserGroupAddress {
        _ugaCompanyNumber = ""
      , _ugaAddress       = ""
      , _ugaZip           = ""
      , _ugaCity          = ""
      , _ugaCountry       = ""
      }
    , _ugUI = def
    }

-- USER GROUP ROOT

ugrFromUG :: UserGroup -> Maybe UserGroupRoot
ugrFromUG ug = do
  -- the root of usergroup tree must have Invoice, Settings, Address, UI
  -- and Feature Flags
  _ugrPaymentPlan <- case _ugInvoicing ug of
    None -> Nothing
    BillItem _ -> Nothing
    Invoice pp -> Just pp  -- the root of usergroup tree must have Invoice
  _ugrSettings <- _ugSettings ug  -- the root of usergroup tree must have Settings
  _ugrAddress <- _ugAddress ug  -- the root of usergroup tree must have Address
  return $ UserGroupRoot
    { _ugrID = _ugID ug
    , _ugrName = _ugName ug
    , _ugrUI = _ugUI ug
    , ..
    }

ugFromUGRoot :: UserGroupRoot -> UserGroup
ugFromUGRoot ugr = UserGroup
  { _ugID = _ugrID ugr
  , _ugName = _ugrName ugr
  , _ugParentGroupID = Nothing
  , _ugInvoicing = Invoice . _ugrPaymentPlan $ ugr
  , _ugSettings = Just $ _ugrSettings ugr
  , _ugAddress = Just $ _ugrAddress ugr
  , _ugUI = _ugrUI ugr
  }

-- USER GROUP WITH PARENTS
ugwpOnlyParents :: UserGroupWithParents -> Maybe UserGroupWithParents
ugwpOnlyParents (_,[]) = Nothing -- root is leaf, nothing would be left
ugwpOnlyParents (root, (_:parents_tail)) = Just (root, parents_tail)

ugwpInherit :: (UserGroupRoot -> a) -> (UserGroup -> Maybe a) -> UserGroupWithParents -> a
ugwpInherit ugrProperty ugProperty (ug_root, ug_children_path) =
  fromMaybe (ugrProperty ug_root) . listToMaybe . catMaybes
    . map ugProperty $ ug_children_path

ugwpPaymentPlan :: UserGroupWithParents -> PaymentPlan
ugwpPaymentPlan = ugwpInherit _ugrPaymentPlan ugPaymentPlan

ugwpSettings :: UserGroupWithParents -> UserGroupSettings
ugwpSettings = ugwpInherit _ugrSettings _ugSettings

ugwpAddress :: UserGroupWithParents -> UserGroupAddress
ugwpAddress = ugwpInherit _ugrAddress _ugAddress

ugwpToList :: UserGroupWithParents -> [UserGroup]
ugwpToList (ug_root, ug_children_path) =
  ug_children_path ++ [ugFromUGRoot ug_root]

ugwpUG :: UserGroupWithParents -> UserGroup
ugwpUG (root, [])  = ugFromUGRoot root
ugwpUG (_, (ug:_)) = ug

ugwpAddChild :: UserGroup -> UserGroupWithParents -> UserGroupWithParents
ugwpAddChild ug (root, children_path) = (root, ug:children_path)

newtype UserGroupID = UserGroupID Int64
  deriving (Eq, Ord)
deriving newtype instance Read UserGroupID
deriving newtype instance Show UserGroupID

instance PQFormat UserGroupID where
  pqFormat = pqFormat @Int64

instance FromSQL UserGroupID where
  type PQBase UserGroupID = PQBase Int64
  fromSQL mbase = UserGroupID <$> fromSQL mbase

instance ToSQL UserGroupID where
  type PQDest UserGroupID = PQDest Int64
  toSQL (UserGroupID n) = toSQL n

instance FromReqURI UserGroupID where
  fromReqURI = maybeRead

unsafeUserGroupID :: Int64 -> UserGroupID
unsafeUserGroupID = UserGroupID

emptyUserGroupID :: UserGroupID
emptyUserGroupID = UserGroupID 0

fromUserGroupID :: UserGroupID -> Int64
fromUserGroupID (UserGroupID ugid) = ugid

instance Identifier UserGroupID where
  idDefaultLabel          = "user_group_id"
  idValue (UserGroupID k) = int64AsStringIdentifier k

instance Binary UserGroupID where
  put (UserGroupID ugid) = put ugid
  get = fmap UserGroupID B.get

instance Unjson UserGroupID where
  unjsonDef = unjsonInvmapR ((maybe (fail "Can't parse UserGroupID")  return) . maybeRead) show unjsonDef

-- USER GROUP INFO

type instance CompositeRow UserGroupSettings = (
    Maybe String
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
  )

instance PQFormat UserGroupSettings where
  pqFormat = "%user_group_setting"

instance CompositeFromSQL UserGroupSettings where
  toComposite (
      ip_address_mask_list
    , idle_doc_timeout_preparation
    , idle_doc_timeout_closed
    , idle_doc_timeout_canceled
    , idle_doc_timeout_timedout
    , idle_doc_timeout_rejected
    , idle_doc_timeout_error
    , immediate_trash
    , cgi_display_name
    , sms_provider
    , cgi_service_id
    , pad_app_mode
    , pad_earchive_enabled
    , legal_text
    ) = UserGroupSettings {
      _ugsIPAddressMaskList         = maybe [] read ip_address_mask_list
    , _ugsDataRetentionPolicy = DataRetentionPolicy
        { _drpIdleDocTimeoutPreparation = idle_doc_timeout_preparation
        , _drpIdleDocTimeoutClosed      = idle_doc_timeout_closed
        , _drpIdleDocTimeoutCanceled    = idle_doc_timeout_canceled
        , _drpIdleDocTimeoutTimedout    = idle_doc_timeout_timedout
        , _drpIdleDocTimeoutRejected    = idle_doc_timeout_rejected
        , _drpIdleDocTimeoutError       = idle_doc_timeout_error
        , _drpImmediateTrash            = immediate_trash
        }
    , _ugsCGIDisplayName            = cgi_display_name
    , _ugsCGIServiceID              = cgi_service_id
    , _ugsSMSProvider               = sms_provider
    , _ugsPadAppMode                = pad_app_mode
    , _ugsPadEarchiveEnabled        = pad_earchive_enabled
    , _ugsLegalText                 = legal_text
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
  pqFormat = "%user_group_ui"

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

instance Default UserGroupUI where
  def = UserGroupUI {
      _uguiMailTheme     = Nothing
    , _uguiSignviewTheme = Nothing
    , _uguiServiceTheme  = Nothing
    , _uguiBrowserTitle  = Nothing
    , _uguiSmsOriginator = Nothing
    , _uguiFavicon       = Nothing
    }

-- ADDRESS

type instance CompositeRow UserGroupAddress = (
    Text
  , Text
  , Text
  , Text
  , Text
  )

instance PQFormat UserGroupAddress where
  pqFormat = "%user_group_address"

instance CompositeFromSQL UserGroupAddress where
  toComposite (
      company_number
    , address
    , zip_code
    , city
    , country
    ) =  UserGroupAddress {
      _ugaCompanyNumber = company_number
    , _ugaAddress       = address
    , _ugaZip           = zip_code
    , _ugaCity          = city
    , _ugaCountry       = country
    }
