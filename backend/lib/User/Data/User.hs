module User.Data.User
    ( composeFullName
    , fetchUser
    , fetchUserWithCompany
    , selectUsersSQL
    , selectUsersSelectors
    , selectUsersSelectorsList
    , selectUsersWithCompaniesSQL
    , User(..)
    , UserInfo(..)
    , UserSettings(..)
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default
import Data.Int (Int16)
import Data.String.Utils (strip)

import BrandedDomain.BrandedDomainID
import Company.Model
import DB
import KontraPrelude
import Log.Identifier
import MinutesTime
import PadApplication.Data
import Partner.Model
import SMS.Data (SMSProvider)
import User.Data.SignupMethod
import User.Email
import User.Lang
import User.Password
import User.UserID
import Util.HasSomeUserInfo

data User = User {
    userid                        :: UserID
  , userpassword                  :: Maybe Password
  , useriscompanyadmin            :: Bool
  , useraccountsuspended          :: Bool
  , userhasacceptedtermsofservice :: Maybe UTCTime
  , usersignupmethod              :: SignupMethod
  , userinfo                      :: UserInfo
  , usersettings                  :: UserSettings
  , usercompany                   :: CompanyID
  , userassociateddomainid        :: BrandedDomainID
  } deriving (Eq, Ord, Show)

instance HasSomeUserInfo User where
  getEmail          = strip . unEmail . useremail . userinfo
  getFirstName      = userfstname         . userinfo
  getLastName       = usersndname         . userinfo
  getPersonalNumber = userpersonalnumber  . userinfo
  getMobile         = userphone          . userinfo

instance Loggable User where
  logValue User{..} = object [
      identifier_ userid
    , "email" .= useremail userinfo
    , "name" .= (userfstname userinfo <> " " <> usersndname userinfo)
    ]
  logDefaultLabel _ = "user"

data UserInfo = UserInfo {
    userfstname         :: String
  , usersndname         :: String
  , userpersonalnumber  :: String
  , usercompanyposition :: String
  , userphone           :: String
  , useremail           :: Email
  } deriving (Eq, Ord, Show)

instance HasSomeUserInfo UserInfo where
  getEmail          = strip . unEmail . useremail
  getFirstName      = userfstname
  getLastName       = usersndname
  getPersonalNumber = userpersonalnumber
  getMobile         = userphone

instance Default User where
    def = User {
    userid                        = unsafeUserID 0
  , userpassword                  = Nothing
  , useriscompanyadmin            = False
  , useraccountsuspended          = False
  , userhasacceptedtermsofservice = Nothing
  , usersignupmethod              = ByAdmin
  , userinfo                      = def
  , usersettings                  = UserSettings LANG_EN
  , usercompany                   = unsafeCompanyID 0
  , userassociateddomainid        = unsafeBrandedDomainID 0
  }

instance Default UserInfo where
    def = UserInfo {
    userfstname         = ""
  , usersndname         = ""
  , userpersonalnumber  = ""
  , usercompanyposition = ""
  , userphone           = ""
  , useremail           = Email ""
  }

data UserSettings  = UserSettings {
    lang                :: Lang
  } deriving (Eq, Ord, Show)

instance HasLang User where
  getLang = getLang . usersettings

instance HasLang UserSettings where
  getLang = lang

selectUsersSelectorsList :: [SQL]
selectUsersSelectorsList =
  [ "id"
  , "password"
  , "salt"
  , "is_company_admin"
  , "account_suspended"
  , "has_accepted_terms_of_service"
  , "signup_method"
  , "company_id"
  , "first_name"
  , "last_name"
  , "personal_number"
  , "company_position"
  , "phone"
  , "email"
  , "lang"
  , "associated_domain_id"
  , "password_algorithm"
  ]

selectUsersSelectors :: SQL
selectUsersSelectors = sqlConcatComma selectUsersSelectorsList

selectUsersSQL :: SQL
selectUsersSQL = "SELECT" <+> selectUsersSelectors <+> "FROM users"

selectUsersWithCompaniesSQL :: SQL
selectUsersWithCompaniesSQL = "SELECT"
  -- User:
  <> "  users.id AS user_id"
  <> ", users.password"
  <> ", users.salt"
  <> ", users.is_company_admin"
  <> ", users.account_suspended"
  <> ", users.has_accepted_terms_of_service"
  <> ", users.signup_method"
  <> ", users.company_id AS user_company_id"
  <> ", users.first_name"
  <> ", users.last_name"
  <> ", users.personal_number"
  <> ", users.company_position"
  <> ", users.phone"
  <> ", users.email"
  <> ", users.lang"
  <> ", users.associated_domain_id"
  -- Company:
  <> ", c.id AS company_id"
  <> ", c.name"
  <> ", c.number"
  <> ", c.address"
  <> ", c.zip"
  <> ", c.city"
  <> ", c.country"
  <> ", c.ip_address_mask_list"
  <> ", c.allow_save_safety_copy"
  <> ", c.idle_doc_timeout"
  <> ", c.cgi_display_name"
  <> ", c.sms_provider"
  <> ", c.cgi_service_id"
  <> ", c.payment_plan"
  <> ", c.partner_id as partner_id"
  <> ", c.pad_app_mode"
  <> ", c.pad_earchive_enabled"
  <> "  FROM users"
  <> "  LEFT JOIN companies c ON users.company_id = c.id"
  <> "  WHERE users.deleted IS NULL"

composeFullName :: (String, String) -> String
composeFullName (fstname, sndname) = if null sndname
  then fstname
  else fstname ++ " " ++ sndname

fetchUser :: (UserID, Maybe ByteString, Maybe ByteString, Bool, Bool, Maybe UTCTime, SignupMethod, CompanyID, String, String, String, String, String, Email, Lang, BrandedDomainID, Maybe Int16) -> User
fetchUser (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, company_id, first_name, last_name, personal_number, company_position, phone, email, lang, associated_domain_id, password_algorithm) = User {
  userid = uid
, userpassword = maybeMkPassword ( password, salt
                                 , int16ToPwdAlgorithm <$> password_algorithm )
, useriscompanyadmin = is_company_admin
, useraccountsuspended = account_suspended
, userhasacceptedtermsofservice = has_accepted_terms_of_service
, usersignupmethod = signup_method
, userinfo = UserInfo {
    userfstname = first_name
  , usersndname = last_name
  , userpersonalnumber = personal_number
  , usercompanyposition = company_position
  , userphone = phone
  , useremail = email
  }
, usersettings = UserSettings { lang = lang }
, usercompany = company_id
, userassociateddomainid = associated_domain_id
}

fetchUserWithCompany :: (UserID, Maybe ByteString, Maybe ByteString, Bool, Bool, Maybe UTCTime, SignupMethod, CompanyID, String, String, String, String, String, Email, Lang, BrandedDomainID, Maybe Int16, Maybe CompanyID, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Bool, Maybe Int16, Maybe String, SMSProvider, Maybe String, PaymentPlan, PartnerID, PadAppMode, Bool) -> (User, Company)
fetchUserWithCompany (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, company_id, first_name, last_name, personal_number, company_position, phone, email, lang, associated_domain_id, password_algorithm, cid, name, number, address, zip', city, country, ip_address_mask, allow_save_safety_copy, idle_doc_timeout, cgi_display_name, sms_provider, cgi_service_id, payment_plan, partner_id, pad_app_mode, pad_earchive_enabled) = (user, company)
  where
    user = User {
      userid = uid
    , userpassword = maybeMkPassword
                     ( password, salt
                     , int16ToPwdAlgorithm <$> password_algorithm )
    , useriscompanyadmin = is_company_admin
    , useraccountsuspended = account_suspended
    , userhasacceptedtermsofservice = has_accepted_terms_of_service
    , usersignupmethod = signup_method
    , userinfo = UserInfo {
        userfstname = first_name
      , usersndname = last_name
      , userpersonalnumber = personal_number
      , usercompanyposition = company_position
      , userphone = phone
      , useremail = email
      }
    , usersettings = UserSettings { lang = lang }
    , usercompany = company_id
    , userassociateddomainid = associated_domain_id
    }
    company = Company {
      companyid = fromJust cid
    , companyinfo = CompanyInfo {
        companyname = fromJust name
      , companynumber = fromJust number
      , companyaddress = fromJust address
      , companyzip = fromJust zip'
      , companycity = fromJust city
      , companycountry = fromJust country
      , companyipaddressmasklist = maybe [] read ip_address_mask
      , companyallowsavesafetycopy = allow_save_safety_copy
      , companyidledoctimeout = idle_doc_timeout
      , companycgidisplayname = cgi_display_name
      , companysmsprovider = sms_provider
      , companycgiserviceid = cgi_service_id
      , companypaymentplan = payment_plan
      , companypartnerid = partner_id
      , companypadappmode = pad_app_mode
      , companypadearchiveenabled = pad_earchive_enabled
      }
    }
