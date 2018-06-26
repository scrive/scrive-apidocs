module Company.Data (
    module Company.Data.PaymentPlan
  , module Company.CompanyID
  , Company(..)
  , CompanyInfo(..)
  ) where

import Data.Default
import Data.Int (Int16)
import Data.Typeable

import Company.CompanyID
import Company.Data.PaymentPlan
import IPAddress
import PadApplication.Data
import Partner.Partner
import SMS.Data (SMSProvider(..))
import UserGroup.Data

data Company = Company {
    companyid          :: CompanyID
  , companyinfo        :: CompanyInfo
  , companyusergroupid :: UserGroupID
  } deriving (Eq, Ord, Show, Typeable)

data CompanyInfo = CompanyInfo {
    companyname    :: String
  , companynumber  :: String
  , companyaddress :: String
  , companyzip     :: String
  , companycity    :: String
  , companycountry :: String
  , companyipaddressmasklist :: [IPAddressWithMask]
  , companyidledoctimeout :: Maybe Int16
  , companycgidisplayname :: Maybe String
  , companysmsprovider    :: SMSProvider
  , companycgiserviceid   :: Maybe String
  , companypaymentplan    :: PaymentPlan
  , companypartnerid      :: PartnerID
  , companypadappmode     :: PadAppMode
  , companypadearchiveenabled :: Bool
  } deriving (Eq, Ord, Show)

-- @devnote will be no partner ID '0' so we'll remove this in the API endpoints when we
-- encounter it (i.e. when not given explicitly by the API client)
instance Default CompanyInfo where
    def = CompanyInfo
          { companyname                = ""
          , companynumber              = ""
          , companyaddress             = ""
          , companyzip                 = ""
          , companycity                = ""
          , companycountry             = ""
          , companyipaddressmasklist   = []
          , companyidledoctimeout      = Nothing
          , companycgidisplayname      = Nothing
          , companysmsprovider         = SMSDefault
          , companycgiserviceid        = Nothing
          , companypaymentplan         = FreePlan
          , companypartnerid           = unsafePartnerID 0
          , companypadappmode          = ListView
          , companypadearchiveenabled  = True
          }
