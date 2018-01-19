module UserGroup.Internal (
    UserGroup(..)
  , ugInvoicingType
  , ugPaymentPlan
  , UserGroupID
  , emptyUserGroupID
  , unsafeUserGroupID
  , UserGroupInfo(..)
  , UserGroupAddress(..)
  , UserGroupUI(..)
  , UserGroupInvoicing(..)
  , UserGroupWithParents
  ) where

import Data.Aeson
import Data.Int
import Data.Text
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS

import Company.Data.PaymentPlan
import DB
import IPAddress
import Log.Identifier
import PadApplication.Data
import SMS.Data
import Theme.ThemeID

data UserGroup = UserGroup {
    _ugID            :: UserGroupID
  , _ugParentGroupID :: Maybe UserGroupID
  , _ugName          :: Text
  , _ugAddress       :: UserGroupAddress
  , _ugInfo          :: UserGroupInfo
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

data UserGroupInfo = UserGroupInfo {
    _ugiIPAddressMaskList   :: [IPAddressWithMask]
  , _ugiIdleDocTimeout      :: Maybe Int16
  , _ugiCGIDisplayName      :: Maybe Text
  , _ugiCGIServiceID        :: Maybe Text
  , _ugiSMSProvider         :: SMSProvider
  , _ugiPadAppMode          :: PadAppMode
  , _ugiPadEarchiveEnabled  :: Bool
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
  pqFormat = const $ pqFormat (undefined::Int16)

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
  pqFormat _ = "%user_group_invoicing"

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
  , Composite UserGroupInfo
  , Composite UserGroupAddress
  , Composite UserGroupUI
  )

instance PQFormat UserGroup where
  pqFormat _ = "%user_group"

instance CompositeFromSQL UserGroup where
  toComposite (ugid, mparentgroupid, name, cinvoicing, cinfos, caddresses, cuis) = UserGroup {
      _ugID = ugid
    , _ugParentGroupID = mparentgroupid
    , _ugName = name
    , _ugInfo = unComposite cinfos
    , _ugInvoicing = unComposite cinvoicing
    , _ugAddress = unComposite  caddresses
    , _ugUI = unComposite cuis
    }

newtype UserGroupID = UserGroupID Int64
  deriving (Eq, Ord, PQFormat)
deriving newtype instance Read UserGroupID
deriving newtype instance Show UserGroupID

instance FromSQL UserGroupID where
  type PQBase UserGroupID = PQBase Int64
  fromSQL mbase = UserGroupID <$> fromSQL mbase

instance ToSQL UserGroupID where
  type PQDest UserGroupID = PQDest Int64
  toSQL (UserGroupID n) = toSQL n

unsafeUserGroupID :: Int64 -> UserGroupID
unsafeUserGroupID = UserGroupID

emptyUserGroupID :: UserGroupID
emptyUserGroupID = UserGroupID 0

instance Identifier UserGroupID Int64 where
  idDefaultLabel _ = "user_group_id"
  idValue (UserGroupID k) = toJSON k

-- USER GROUP INFO

type instance CompositeRow UserGroupInfo = (
    Maybe String
  , Maybe Int16
  , Maybe Text
  , SMSProvider
  , Maybe Text
  , PadAppMode
  , Bool
  )

instance PQFormat UserGroupInfo where
  pqFormat _ = "%user_group_info"

instance CompositeFromSQL UserGroupInfo where
  toComposite (
      ip_address_mask_list
    , idle_doc_timeout
    , cgi_display_name
    , sms_provider
    , cgi_service_id
    , pad_app_mode
    , pad_earchive_enabled
    ) = UserGroupInfo {
      _ugiIPAddressMaskList   = maybe [] read ip_address_mask_list
    , _ugiIdleDocTimeout      = idle_doc_timeout
    , _ugiCGIDisplayName      = cgi_display_name
    , _ugiCGIServiceID        = cgi_service_id
    , _ugiSMSProvider         = sms_provider
    , _ugiPadAppMode          = pad_app_mode
    , _ugiPadEarchiveEnabled  = pad_earchive_enabled
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
  pqFormat _ = "%user_group_ui"

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
  pqFormat _ = "%user_group_address"

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
