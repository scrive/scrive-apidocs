module UserGroup.Internal (
    UserGroup(..)
  , ugInvoicingType
  , ugPaymentPlan
  , UserGroupID(..)
  , emptyUserGroupID
  , unsafeUserGroupID
  , fromUserGroupID
  , UserGroupSettings(..)
  , UserGroupAddress(..)
  , UserGroupUI(..)
  , UserGroupInvoicing(..)
  , UserGroupWithParents
  ) where

import Data.Binary
import Data.Default
import Data.Int
import Data.Text
import Data.Unjson
import Happstack.Server
import qualified Control.Exception.Lifted as E
import qualified Data.Binary as B
import qualified Data.ByteString.Char8 as BS

import DataRetentionPolicy
import DB
import IPAddress
import Log.Identifier
import PadApplication.Data
import SMS.Data
import Theme.ThemeID
import UserGroup.Data.PaymentPlan

data UserGroup = UserGroup {
    _ugID            :: UserGroupID
  , _ugParentGroupID :: Maybe UserGroupID
  , _ugName          :: Text
  , _ugAddress       :: UserGroupAddress
  , _ugSettings      :: UserGroupSettings
  , _ugInvoicing     :: UserGroupInvoicing
  , _ugUI            :: UserGroupUI
  } deriving (Show, Eq)

type UserGroupWithParents = (UserGroup, [UserGroup])

data UserGroupInvoicing =
    None
  | BillItem (Maybe PaymentPlan)
  | Invoice PaymentPlan
  deriving (Show, Eq)

data InvoicingType =
    InvoicingTypeNone
  | InvoicingTypeBillItem
  | InvoicingTypeInvoice
  deriving (Read, Show)

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
  , Composite UserGroupSettings
  , Composite UserGroupAddress
  , Composite UserGroupUI
  )

instance PQFormat UserGroup where
  pqFormat = "%user_group"

instance CompositeFromSQL UserGroup where
  toComposite (ugid, mparentgroupid, name, cinvoicing, cinfos, caddresses, cuis) = UserGroup {
      _ugID = ugid
    , _ugParentGroupID = mparentgroupid
    , _ugName = name
    , _ugSettings = unComposite cinfos
    , _ugInvoicing = unComposite cinvoicing
    , _ugAddress = unComposite caddresses
    , _ugUI = unComposite cuis
    }

instance Default UserGroup where
  def = UserGroup {
      _ugID = emptyUserGroupID
    , _ugParentGroupID = Nothing
    , _ugName = ""
    , _ugSettings = UserGroupSettings {
        _ugsIPAddressMaskList   = []
      , _ugsDataRetentionPolicy = def
      , _ugsCGIDisplayName      = Nothing
      , _ugsCGIServiceID        = Nothing
      , _ugsSMSProvider         = SMSDefault
      , _ugsPadAppMode          = ListView
      , _ugsPadEarchiveEnabled  = True
      , _ugsLegalText           = False
      }
    , _ugInvoicing = Invoice FreePlan
    , _ugAddress = UserGroupAddress {
        _ugaCompanyNumber = ""
      , _ugaAddress       = ""
      , _ugaZip           = ""
      , _ugaCity          = ""
      , _ugaCountry       = ""
      }
    , _ugUI = def
    }

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
