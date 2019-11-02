{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module UserGroup.Internal (
    InvoicingType(..)
  , UserGroup(..)
  , UserGroupID
  , emptyUserGroupID
  , unsafeUserGroupID
  , fromUserGroupID
  , UserGroupSettings(..)
  , UserGroupAddress(..)
  , UserGroupUI(..)
  , UserGroupInvoicing(..)
  , UserGroupWithParents
  , UserGroupRoot(..)
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
  { id            :: !UserGroupID
  , parentGroupID :: !(Maybe UserGroupID)
  , name          :: !Text
  -- Folder, where home folders are created for new users
  -- it is a Maybe for slow migration purposes after that
  -- the Maybe will be removed
  -- The Maybe can be re-introduced, when we implement home folder inheritance
  , homeFolderID  :: !(Maybe FolderID)
  , address       :: !(Maybe UserGroupAddress)
  , settings      :: !(Maybe UserGroupSettings)
  , invoicing     :: !UserGroupInvoicing
  , ui            :: !UserGroupUI
  , features      :: !(Maybe Features)
  } deriving (Show, Eq)

data UserGroupRoot = UserGroupRoot
  { id            :: !UserGroupID
  , name          :: !Text
  , homeFolderID  :: !(Maybe FolderID)
  , address       :: !UserGroupAddress
  , settings      :: !UserGroupSettings
  , paymentPlan   :: !PaymentPlan  -- user group root always must have Invoice
  , ui            :: !UserGroupUI
  , features      :: !Features
  } deriving (Show, Eq)

-- UserGroup list is ordered from Leaf to Child of Root)
type UserGroupWithParents = (UserGroupRoot, [UserGroup])

-- UserGroup and all its children down to the bottom
data UserGroupWithChildren = UserGroupWithChildren
  { group    :: !UserGroup
  , children :: ![UserGroupWithChildren]
  } deriving (Eq, Show)

data UserGroupInvoicing =
    None
  | BillItem (Maybe PaymentPlan)
  | Invoice PaymentPlan
  deriving (Show, Eq)

----------------------------------------

data InvoicingType =
    InvoicingTypeNone
  | InvoicingTypeBillItem
  | InvoicingTypeInvoice
  deriving (Eq, Ord)

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

type instance CompositeRow UserGroupInvoicing = (InvoicingType, Maybe PaymentPlan)

instance PQFormat UserGroupInvoicing where
  pqFormat = compositeTypePqFormat ctUserGroupInvoicing

instance CompositeFromSQL UserGroupInvoicing where
  toComposite (invoicing_type, mpayplan) = case (invoicing_type, mpayplan) of
    (InvoicingTypeNone, Nothing) -> None
    (InvoicingTypeBillItem, _) -> BillItem mpayplan
    (InvoicingTypeInvoice, Just payplan) -> Invoice payplan
    _ -> unexpectedError "invalid invoicing row in database"

----------------------------------------

data UserGroupSettings = UserGroupSettings
  { ipAddressMaskList         :: ![IPAddressWithMask]
  , dataRetentionPolicy       :: !DataRetentionPolicy
  , cgiDisplayName            :: !(Maybe Text)
  , cgiServiceID              :: !(Maybe Text)
  , smsProvider               :: !SMSProvider
  , padAppMode                :: !PadAppMode
  , padEarchiveEnabled        :: !Bool
  , legalText                 :: !Bool
  , requireBPIDForNewDoc      :: !Bool
  , sendTimeoutNotification   :: !Bool
  , totpIsMandatory           :: !Bool
  , sessionTimeoutSecs        :: !(Maybe Int32)
  , portalUrl                 :: !(Maybe Text)
  } deriving (Show, Eq)

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
  toComposite (ip_address_mask_list, drpIdleDocTimeoutPreparation, drpIdleDocTimeoutClosed, drpIdleDocTimeoutCanceled, drpIdleDocTimeoutTimedout, drpIdleDocTimeoutRejected, drpIdleDocTimeoutError, drpImmediateTrash, cgiDisplayName, smsProvider, cgiServiceID, padAppMode, padEarchiveEnabled, legalText, requireBPIDForNewDoc, sendTimeoutNotification, _useFolderListCalls {- not yet used -}
                                                                                                                                                                                                                                                                                                                                                                                , totpIsMandatory, sessionTimeoutSecs, portalUrl)
    = UserGroupSettings
      { ipAddressMaskList   = maybe [] read ip_address_mask_list
      , dataRetentionPolicy = DataRetentionPolicy { .. }
      , ..
      }

----------------------------------------

data UserGroupUI = UserGroupUI
  { mailTheme     :: !(Maybe ThemeID)
  , signviewTheme :: !(Maybe ThemeID)
  , serviceTheme  :: !(Maybe ThemeID)
  , browserTitle  :: !(Maybe Text)
  , smsOriginator :: !(Maybe Text)
  , favicon       :: !(Maybe BS.ByteString)
  } deriving (Eq, Ord, Show)

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

----------------------------------------

data UserGroupAddress = UserGroupAddress
  { companyNumber :: !Text
  , entityName    :: !Text
  , address       :: !Text
  , zipCode       :: !Text
  , city          :: !Text
  , country       :: !Text
  } deriving (Eq, Ord, Show)

type instance CompositeRow UserGroupAddress = (Text, Text, Text, Text, Text, Text)

instance PQFormat UserGroupAddress where
  pqFormat = compositeTypePqFormat ctUserGroupAddress

instance CompositeFromSQL UserGroupAddress where
  toComposite (companyNumber, entityName, address, zipCode, city, country) =
    UserGroupAddress { .. }

makeFieldLabelsWith noPrefixFieldLabels ''UserGroup
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupSettings
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupUI
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupAddress
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupRoot
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupWithChildren
