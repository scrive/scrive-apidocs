{-# OPTIONS_GHC -fcontext-stack=50 #-}
{-# LANGUAGE ExistentialQuantification #-}
module User.Model (
    module User.Lang
  , module User.Password
  , module User.UserID
  , InviteType(..)
  , SignupMethod(..)
  , User(..)
  , UserInfo(..)
  , UserSettings(..)
  , UserUsageStats(..)
  , GetUsers(..)
  , GetUserByID(..)
  , GetUserByIDIncludeDeleted(..)
  , GetUserByEmail(..)
  , GetUsersWithCompanies(..)
  , GetCompanyAccounts(..)
  , GetCompanyAdmins(..)
  , GetUsageStats(..)
  , SetUserCompany(..)
  , DeleteUser(..)
  , RemoveInactiveUser(..)
  , AddUser(..)
  , SetUserEmail(..)
  , SetUserPassword(..)
  , SetUserInfo(..)
  , SetUserSettings(..)
  , AcceptTermsOfService(..)
  , SetSignupMethod(..)
  , SetUserCompanyAdmin(..)
  , UserFilter(..)
  , IsUserDeletable(..)
  , composeFullName
  , userFilterToSQL

  , UserOrderBy(..)
  , userOrderByToSQL
  , userOrderByAscDescToSQL
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Char
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Monoid.Space
import Happstack.Server (FromReqURI(..))
import qualified Control.Exception.Lifted as E

import BrandedDomain.BrandedDomainID
import Company.Model
import DB
import Doc.DocStateData (DocumentStatus(..),FieldType(..))
import MinutesTime
import User.Email
import User.Lang
import User.Password
import User.UserID
import Utils.Read

data InviteType = Viral | Admin
  deriving (Eq, Ord, Show)



data SignupMethod = AccountRequest | ViralInvitation | BySigning | ByAdmin | CompanyInvitation
  deriving (Eq, Ord, Show, Read)

instance PQFormat SignupMethod where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL SignupMethod where
  type PQBase SignupMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return AccountRequest
      2 -> return ViralInvitation
      3 -> return BySigning
      4 -> return ByAdmin
      5 -> return CompanyInvitation
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL SignupMethod where
  type PQDest SignupMethod = PQDest Int16
  toSQL AccountRequest    = toSQL (1::Int16)
  toSQL ViralInvitation   = toSQL (2::Int16)
  toSQL BySigning         = toSQL (3::Int16)
  toSQL ByAdmin           = toSQL (4::Int16)
  toSQL CompanyInvitation = toSQL (5::Int16)

instance FromReqURI SignupMethod where
  fromReqURI = maybeRead

data UserUsageStats = UserUsageStats
                    { uusTimeSpan         :: (UTCTime, UTCTime)
                    , uusUser             :: Maybe (UserID, String, String)
                    , uusCompany          :: Maybe (CompanyID, String)
                    , uusDocumentsSent    :: !Int64
                    , uusDocumentsClosed  :: !Int64
                    , uusSignaturesClosed :: !Int64
                    } deriving (Eq, Ord, Show)

-- data structures
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
  , userassociateddomainid        :: Maybe BrandedDomainID
  } deriving (Eq, Ord, Show)

data UserInfo = UserInfo {
    userfstname         :: String
  , usersndname         :: String
  , userpersonalnumber  :: String
  , usercompanyposition :: String
  , userphone           :: String
  , useremail           :: Email
  } deriving (Eq, Ord, Show)

data UserSettings  = UserSettings {
    lang                :: Lang
  } deriving (Eq, Ord, Show)

instance HasLang User where
  getLang = getLang . usersettings

instance HasLang UserSettings where
  getLang = lang


data UserFilter
  = UserFilterByString String             -- ^ Contains the string in name, email or anywhere


userFilterToSQL :: UserFilter -> SQL
userFilterToSQL (UserFilterByString string) =
  sqlConcatAND $ map (\wordpat -> sqlConcatOR [
      "users.first_name ILIKE" <?> wordpat
    , "users.last_name ILIKE" <?> wordpat
    , "users.email ILIKE" <?> wordpat
    , "translate(users.phone,'-+ .,()','') ILIKE translate(" <?> wordpat <+> ",'-+ .,()','')"
    , "translate(users.personal_number,'-+ .,()','') ILIKE translate(" <?> wordpat <+> ",'-+ .,()','')"
    ]) sqlwordpat
  where
      sqlwordpat = map (\word -> "%" ++ concatMap escape word ++ "%") (words string)
      escape '\\' = "\\\\"
      escape '%' = "\\%"
      escape '_' = "\\_"
      escape c = [c]


data UserOrderBy
  = UserOrderByName
  | UserOrderByEmail
  | UserOrderByAccountCreationDate

-- | Convert UserOrderBy enumeration into proper SQL order by statement
userOrderByToSQL :: UserOrderBy -> SQL
userOrderByToSQL UserOrderByName                = "(users.first_name || ' ' || users.last_name)"
userOrderByToSQL UserOrderByEmail               = "users.email"
userOrderByToSQL UserOrderByAccountCreationDate = "users.has_accepted_terms_of_service"

userOrderByAscDescToSQL :: AscDesc UserOrderBy -> SQL
userOrderByAscDescToSQL (Asc x@UserOrderByAccountCreationDate) = userOrderByToSQL x <+> "ASC NULLS FIRST "
userOrderByAscDescToSQL (Desc x@UserOrderByAccountCreationDate) = userOrderByToSQL x <+> "DESC NULLS LAST "
userOrderByAscDescToSQL (Asc x) = userOrderByToSQL x
userOrderByAscDescToSQL (Desc x) = userOrderByToSQL x <+> "DESC"

data GetUsers = GetUsers
instance MonadDB m => DBQuery m GetUsers [User] where
  query GetUsers = do
    runQuery_ $ selectUsersSQL <+> "WHERE deleted IS NULL ORDER BY first_name || ' ' || last_name"
    fetchMany fetchUser

data GetUserByID = GetUserByID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserByID (Maybe User) where
  query (GetUserByID uid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE id =" <?> uid <+> "AND deleted IS NULL"
    fetchMaybe fetchUser

data GetUserByIDIncludeDeleted = GetUserByIDIncludeDeleted UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserByIDIncludeDeleted (Maybe User) where
  query (GetUserByIDIncludeDeleted uid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE id =" <?> uid
    fetchMaybe fetchUser

data GetUserByEmail = GetUserByEmail Email
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserByEmail (Maybe User) where
  query (GetUserByEmail email) = do
    runQuery_ $ selectUsersSQL <+> "WHERE deleted IS NULL AND email =" <?> map toLower (unEmail email)
    fetchMaybe fetchUser

data GetCompanyAccounts = GetCompanyAccounts CompanyID
instance MonadDB m => DBQuery m GetCompanyAccounts [User] where
  query (GetCompanyAccounts cid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE company_id =" <?> cid <+> "AND deleted IS NULL ORDER BY email"
    fetchMany fetchUser

data GetCompanyAdmins = GetCompanyAdmins CompanyID
instance MonadDB m => DBQuery m GetCompanyAdmins [User] where
  query (GetCompanyAdmins cid) = do
    runQuery_ $ selectUsersSQL <+> "WHERE is_company_admin AND company_id =" <?> cid <+> "AND deleted IS NULL ORDER BY email"
    fetchMany fetchUser

data SetUserCompany = SetUserCompany UserID CompanyID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserCompany Bool where
  update (SetUserCompany uid cid) =
    runQuery01 $ sqlUpdate "users" $ do
      sqlSet "company_id" cid
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data IsUserDeletable = IsUserDeletable UserID
instance MonadDB m => DBQuery m IsUserDeletable Bool where
  query (IsUserDeletable uid) = do
    n <- runQuery $ sqlSelect "users" $ do
      sqlWhere "users.deleted IS NULL"
      sqlWhereEq "users.id" uid
      sqlJoinOn "signatory_links" "users.id = signatory_links.user_id"
      sqlWhere "signatory_links.deleted IS NULL"
      sqlWhere "signatory_links.is_author"
      sqlJoinOn "documents" "documents.id = signatory_links.document_id"
      sqlWhereEq "documents.status" Pending
      sqlResult "documents.id"
      sqlLimit 1
    return (n == 0)

-- | Marks a user as deleted so that queries won't return them any more.
data DeleteUser = DeleteUser UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteUser Bool where
  update (DeleteUser uid) = do
    runQuery01 $ sqlUpdate "users" $ do
      sqlSetCmd "deleted" "now()"
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

-- | Removes user who didn't accept TOS from the database
data RemoveInactiveUser = RemoveInactiveUser UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m RemoveInactiveUser Bool where
  update (RemoveInactiveUser uid) = do
    -- There is a chance that a signatory_links gets connected to an
    -- yet not active account the true fix is to not have inactive
    -- accounts, but we are not close to that point yet. Here is a
    -- kludge to get around our own bug.
    runQuery_ $ "SELECT TRUE FROM companyinvites where user_id = " <?> uid
    x :: Maybe Bool <- fetchMaybe unSingle
    if isJust x then
        return False
     else do
       runQuery_ $ "UPDATE signatory_links SET user_id = NULL WHERE user_id = " <?> uid <+> "AND EXISTS (SELECT TRUE FROM users WHERE users.id = " <?> uid <+> " AND users.has_accepted_terms_of_service IS NULL)"
       runQuery01 $ "DELETE FROM users WHERE id = " <?> uid <+> "AND has_accepted_terms_of_service IS NULL"

data AddUser = AddUser (String, String) String (Maybe Password) (CompanyID,Bool) Lang (Maybe BrandedDomainID)
instance (MonadDB m, MonadThrow m) => DBUpdate m AddUser (Maybe User) where
  update (AddUser (fname, lname) email mpwd (cid, admin) l mad) = do
    mu <- query $ GetUserByEmail $ Email email
    case mu of
      Just _ -> return Nothing -- user with the same email address exists
      Nothing -> do
        runQuery_ $ sqlInsert "users" $ do
            sqlSet "password" $ pwdHash <$> mpwd
            sqlSet "salt" $ pwdSalt <$> mpwd
            sqlSet "is_company_admin" admin
            sqlSet "account_suspended" False
            sqlSet "has_accepted_terms_of_service" (Nothing :: Maybe UTCTime)
            sqlSet "signup_method" AccountRequest
            sqlSet "company_id" cid
            sqlSet "first_name" fname
            sqlSet "last_name" lname
            sqlSet "personal_number" ("" :: String)
            sqlSet "company_position" ("" :: String)
            sqlSet "phone" ("" :: String)
            sqlSet "email" $ map toLower email
            sqlSet "lang" l
            sqlSet "deleted" (Nothing :: Maybe UTCTime)
            sqlSet "associated_domain_id" mad
            mapM_ sqlResult selectUsersSelectorsList
        fetchMaybe fetchUser

data SetUserEmail = SetUserEmail UserID Email
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserEmail Bool where
  update (SetUserEmail uid email) = do
    res <- runQuery01 . sqlUpdate "users" $ do
      sqlSet "email" $ map toLower $ unEmail email
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"
    _ <- update $ UpdateDraftsAndTemplatesWithUserData uid
    return res

data SetUserPassword = SetUserPassword UserID Password
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserPassword Bool where
  update (SetUserPassword uid pwd) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "password" $ pwdHash pwd
      sqlSet "salt" $ pwdSalt pwd
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetUserInfo = SetUserInfo UserID UserInfo
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserInfo Bool where
  update (SetUserInfo uid info) = do
    res <- runQuery01 . sqlUpdate "users" $ do
      sqlSet "first_name" $ userfstname info
      sqlSet "last_name" $ usersndname info
      sqlSet "personal_number" $ userpersonalnumber info
      sqlSet "company_position" $ usercompanyposition info
      sqlSet "phone" $ userphone info
      sqlSet "email" $ map toLower $ unEmail $ useremail info
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"
    _ <- update $ UpdateDraftsAndTemplatesWithUserData uid
    return res

data SetUserSettings = SetUserSettings UserID UserSettings
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserSettings Bool where
  update (SetUserSettings uid us) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "lang" $ getLang us
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data AcceptTermsOfService = AcceptTermsOfService UserID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m AcceptTermsOfService Bool where
  update (AcceptTermsOfService uid time) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "has_accepted_terms_of_service" time
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetSignupMethod = SetSignupMethod UserID SignupMethod
instance (MonadDB m, MonadThrow m) => DBUpdate m SetSignupMethod Bool where
  update (SetSignupMethod uid signupmethod) = do
    runQuery01 . sqlUpdate "users" $ do
      sqlSet "signup_method" signupmethod
      sqlWhereEq "id" uid
      sqlWhereIsNULL "deleted"

data SetUserCompanyAdmin = SetUserCompanyAdmin UserID Bool
instance (MonadDB m, MonadThrow m) => DBUpdate m SetUserCompanyAdmin Bool where
  update (SetUserCompanyAdmin uid iscompanyadmin) = do
    runQuery_ $ "SELECT company_id FROM users WHERE id =" <?> uid <+> "AND deleted IS NULL FOR UPDATE"
    mcid <- fetchMaybe unSingle
    case mcid :: Maybe CompanyID of
      Nothing -> return False
      Just _ -> runQuery01 . sqlUpdate "users" $ do
        sqlSet "is_company_admin" iscompanyadmin
        sqlWhereEq "id" uid
        sqlWhereIsNULL "deleted"


fetchUserUsageStats :: (UTCTime, UTCTime, Maybe CompanyID, Maybe String, Maybe UserID, Maybe String, Maybe String, Int64, Int64, Int64) -> UserUsageStats
fetchUserUsageStats (time_begin, time_end, maybe_company_id, maybe_company_name, maybe_user_id, maybe_user_email, maybe_user_name, documents_sent, documents_closed, signatures_closed) = UserUsageStats {
  uusTimeSpan         = (time_begin, time_end)
, uusUser             = (,,) <$> maybe_user_id <*> maybe_user_email <*> maybe_user_name
, uusCompany          = (,) <$> maybe_company_id <*> maybe_company_name
, uusDocumentsSent    = documents_sent
, uusDocumentsClosed  = documents_closed
, uusSignaturesClosed = signatures_closed
}

data GetUsageStats = forall tm. (Show tm, ToSQL tm) => GetUsageStats (Either UserID CompanyID) [(tm, tm)]
instance MonadDB m => DBQuery m GetUsageStats [UserUsageStats] where
  query (GetUsageStats euc timespans) = do
   let (timespans2 :: SQL) = sqlConcatComma $ map (\(beg, end) -> "(" <?> beg <> "::TIMESTAMPTZ, " <?> end <> ":: TIMESTAMPTZ)") timespans
   runQuery_ $ sqlSelect "companies FULL JOIN users ON companies.id = users.company_id" $ do
     sqlFrom $ ", (VALUES" <+> timespans2 <+> ") AS time_spans(b,e)"
     sqlResult "time_spans.b :: TIMESTAMPTZ"
     sqlResult "time_spans.e :: TIMESTAMPTZ"
     sqlResult "companies.id AS \"Company ID\""
     sqlResult "companies.name AS \"Company Name\""
     sqlResult "users.id AS \"User ID\""
     sqlResult "users.email AS \"User Email\""
     sqlResult "users.first_name || ' ' || users.last_name AS \"User Name\""
     sqlResult $ "(SELECT count(*)"
            <+> "   FROM documents"
            <+> "  WHERE EXISTS (SELECT TRUE"
            <+> "                  FROM signatory_links"
            <+> "                 WHERE signatory_links.is_author"
            <+> "                   AND users.id = signatory_links.user_id"
            <+> "                   AND signatory_links.document_id = documents.id"
            <+> "                   AND documents.invite_time BETWEEN time_spans.b AND time_spans.e)"
            <+> ") AS \"Docs sent\""
     sqlResult $ "(SELECT count(*)"
             <+> "   FROM documents"
             <+> "  WHERE EXISTS (SELECT TRUE"
             <+> "                  FROM signatory_links"
             <+> "                 WHERE signatory_links.is_author"
             <+> "                   AND signatory_links.document_id = documents.id"
             <+> "                   AND users.id = signatory_links.user_id"
             <+> "                   AND documents.status = 3" -- Closed
             <+> "                   AND (SELECT max(signatory_links.sign_time)"
             <+> "                          FROM signatory_links"
             <+> "                         WHERE signatory_links.is_partner"
             <+> "                           AND signatory_links.document_id = documents.id) BETWEEN time_spans.b AND time_spans.e)"
             <+> ") AS \"Docs closed\""
     sqlResult $ "(SELECT count(*)"
             <+> "   FROM documents, signatory_links"
             <+> "  WHERE signatory_links.document_id = documents.id"
             <+> "    AND signatory_links.sign_time IS NOT NULL"
             <+> "    AND EXISTS (SELECT TRUE"
             <+> "                  FROM signatory_links"
             <+> "                 WHERE signatory_links.is_author"
             <+> "                   AND signatory_links.document_id = documents.id"
             <+> "                   AND users.id = signatory_links.user_id"
             <+> "                   AND documents.status = 3" -- Closed
             <+> "                   AND (SELECT max(signatory_links.sign_time)"
             <+> "                          FROM signatory_links"
             <+> "                         WHERE signatory_links.is_partner"
             <+> "                           AND signatory_links.document_id = documents.id) BETWEEN time_spans.b AND time_spans.e)"
             <+> ") AS \"Sigs closed\""

     case euc of
       Left  uid -> sqlWhereEq "users.id" uid
       Right cid -> sqlWhereEq "companies.id" cid
     sqlOrderBy "1 DESC, 8 DESC, 9 DESC"
   fetchMany fetchUserUsageStats

-- helpers

composeFullName :: (String, String) -> String
composeFullName (fstname, sndname) = if null sndname
  then fstname
  else fstname ++ " " ++ sndname

selectUsersSQL :: SQL
selectUsersSQL = "SELECT" <+> selectUsersSelectors <+> "FROM users"

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
  ]

selectUsersSelectors :: SQL
selectUsersSelectors = sqlConcatComma selectUsersSelectorsList

fetchUser :: (UserID, Maybe (Binary ByteString), Maybe (Binary ByteString), Bool, Bool, Maybe UTCTime, SignupMethod, CompanyID, String, String, String, String, String, Email, Lang, Maybe BrandedDomainID) -> User
fetchUser (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, company_id, first_name, last_name, personal_number, company_position, phone, email, lang, associated_domain_id) = User {
  userid = uid
, userpassword = maybePassword (password, salt)
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
  <> ", c.sms_originator"
  <> ", c.allow_save_safety_copy"
  <> "  FROM users"
  <> "  LEFT JOIN companies c ON users.company_id = c.id"
  <> "  WHERE users.deleted IS NULL"

fetchUserWithCompany :: (UserID, Maybe (Binary ByteString), Maybe (Binary ByteString), Bool, Bool, Maybe UTCTime, SignupMethod, CompanyID, String, String, String, String, String, Email, Lang, Maybe BrandedDomainID, Maybe CompanyID, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Bool, Maybe Int16) -> (User, Company)
fetchUserWithCompany (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, company_id, first_name, last_name, personal_number, company_position, phone, email, lang, associated_domain_id, cid, name, number, address, zip', city, country, ip_address_mask, sms_originator, allow_save_safety_copy, idle_doc_timeout) = (user, company)
  where
    user = User {
      userid = uid
    , userpassword = maybePassword (password, salt)
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
      , companysmsoriginator = fromJust sms_originator
      , companyallowsavesafetycopy = allow_save_safety_copy
      , companyidledoctimeout = idle_doc_timeout
      }
    }

data GetUsersWithCompanies = GetUsersWithCompanies [UserFilter] [AscDesc UserOrderBy] (Int, Int)
instance MonadDB m => DBQuery m GetUsersWithCompanies [(User, Company)] where
  query (GetUsersWithCompanies filters sorting (offset, limit)) = do
    runQuery_ $ smconcat [
        selectUsersWithCompaniesSQL
      , if null filters
          then mempty
          else "AND" <+> sqlConcatAND (map userFilterToSQL filters)
      , if null sorting
          then mempty
          else "ORDER BY" <+> sqlConcatComma (map userOrderByAscDescToSQL sorting)
      , " OFFSET" <?> (fromIntegral offset :: Int32) <+> "LIMIT" <?> (fromIntegral limit :: Int32)
      ]
    fetchMany fetchUserWithCompany

data UpdateDraftsAndTemplatesWithUserData = UpdateDraftsAndTemplatesWithUserData UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateDraftsAndTemplatesWithUserData () where
 update (UpdateDraftsAndTemplatesWithUserData userid) = do
   muser <- query $ GetUserByID userid
   case muser of
     Nothing -> return ()
     Just user -> do
       -- Update first name
       runQuery_ $ sqlUpdate "signatory_link_fields" $ do
         sqlSet "value" (userfstname $ userinfo user)
         sqlWhereEq "type" FirstNameFT
         whereSignatoryLinkCanBeChanged

       runQuery_ $ sqlUpdate "signatory_link_fields" $ do
         sqlSet "value" (usersndname $ userinfo user)
         sqlWhereEq "type" LastNameFT
         whereSignatoryLinkCanBeChanged

       runQuery_ $ sqlUpdate "signatory_link_fields" $ do
         sqlSet "value" (useremail $ userinfo user)
         sqlWhereEq "type" EmailFT
         whereSignatoryLinkCanBeChanged
  where
    whereSignatoryLinkCanBeChanged =
      sqlWhereExists $ sqlSelect "documents" $ do
        sqlLeftJoinOn "signatory_links" "documents.id = signatory_links.document_id"
        sqlWhere "signatory_links.id = signatory_link_fields.signatory_link_id"
        sqlWhereEq "documents.status" Preparation
        sqlWhereEq "signatory_links.is_author" True
        sqlWhereEq "signatory_links.user_id" userid
