{-# LANGUAGE ExistentialQuantification #-}
module User.Model (
    module User.Lang
  , module User.Password
  , module User.UserID
  , StatsPartition(..)
  , InviteType(..)
  , SignupMethod(..)
  , User(..)
  , UserInfo(..)
  , UserSettings(..)
  , DocumentStats(..)
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

import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Char
import Data.Int
import Happstack.Server (FromReqURI(..))
import qualified Control.Exception.Lifted as E

import BrandedDomain.BrandedDomainID
import Company.Model
import DB
import Doc.DocStateData (DocumentStatus(..),FieldType(..), NameOrder(..))
import KontraPrelude
import MinutesTime
import SMS.Data (SMSProvider)
import User.Email
import User.Lang
import User.Password
import User.UserID

data StatsPartition = PartitionByDay | PartitionByMonth

data InviteType = Viral | Admin
  deriving (Eq, Ord, Show)

data SignupMethod = AccountRequest | ViralInvitation | BySigning | ByAdmin | CompanyInvitation
  deriving (Eq, Ord, Show, Read)

instance PQFormat SignupMethod where
  pqFormat = const $ pqFormat ($undefined::Int16)

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

data DocumentStats = DocumentStats {
    dsDocumentsSent    :: !Int64
  , dsDocumentsClosed  :: !Int64
  , dsSignaturesClosed :: !Int64
  } deriving (Eq, Ord, Show)

instance Monoid DocumentStats where
  mempty = DocumentStats 0 0 0
  ds1 `mappend` ds2 = DocumentStats {
      dsDocumentsSent = dsDocumentsSent ds1 + dsDocumentsSent ds2
    , dsDocumentsClosed = dsDocumentsClosed ds1 + dsDocumentsClosed ds2
    , dsSignaturesClosed = dsSignaturesClosed ds1 + dsSignaturesClosed ds2
    }

data UserUsageStats = UserUsageStats {
    uusTimeWindowStart  :: !UTCTime
  , uusUserEmail        :: !String
  , uusUserName         :: !String
  , uusDocumentStats    :: !DocumentStats
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
  , userassociateddomainid        :: BrandedDomainID
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
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m DeleteUser Bool where
  update (DeleteUser uid) = do
    now <- currentTime
    runQuery01 $ sqlUpdate "users" $ do
      sqlSet "deleted" now
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
    x :: Maybe Bool <- fetchMaybe runIdentity
    if isJust x then
        return False
     else do
       runQuery_ $ "UPDATE signatory_links SET user_id = NULL WHERE user_id = " <?> uid <+> "AND EXISTS (SELECT TRUE FROM users WHERE users.id = " <?> uid <+> " AND users.has_accepted_terms_of_service IS NULL)"
       runQuery01 $ "DELETE FROM users WHERE id = " <?> uid <+> "AND has_accepted_terms_of_service IS NULL"

data AddUser = AddUser (String, String) String (Maybe Password) (CompanyID,Bool) Lang BrandedDomainID
instance (MonadDB m, MonadThrow m) => DBUpdate m AddUser (Maybe User) where
  update (AddUser (fname, lname) email mpwd (cid, admin) l ad) = do
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
            sqlSet "associated_domain_id" ad
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
    mcid <- fetchMaybe runIdentity
    case mcid :: Maybe CompanyID of
      Nothing -> return False
      Just _ -> runQuery01 . sqlUpdate "users" $ do
        sqlSet "is_company_admin" iscompanyadmin
        sqlWhereEq "id" uid
        sqlWhereIsNULL "deleted"

data GetUsageStats = GetUsageStats (Either UserID CompanyID) StatsPartition Interval
instance MonadDB m => DBQuery m GetUsageStats [UserUsageStats] where
  query (GetUsageStats eid statsPartition interval) = do
    -- Fetches relevant documents and then groups them by the
    -- timestamps (trimmed to the precision we want) and users to
    -- achieve desired partitioning. It is also worth noting that it
    -- doesn't return time windows where all numbers would equal 0.
    runQuery_ . sqlSelect "docs_sent FULL JOIN docs_closed USING (time_window, uid) FULL JOIN sigs_closed USING (time_window, uid) JOIN users u ON (uid = u.id)" $ do
      -- Get the number of sent documents per time window / user
      sqlWith "docs_sent" . selectDataWithDateAs "d.invite_time" $ do
        sqlResult "COUNT(*) AS docs_sent"
        sqlGroupBy "time_window"
        sqlGroupBy "uid"
      -- Use intermediate CTE for calculation of both documents and
      -- signatures closed as they are very similar to each other.
      sqlWith "closed" . selectDataWithDateAs maxSignTime $ do
        sqlResult "d.id AS did"
        sqlWhereEq "d.status" Closed
      -- Get the number of closed documents per time window / user.
      sqlWith "docs_closed" . sqlSelect "closed" $ do
        sqlResult "time_window"
        sqlResult "uid"
        sqlResult "COUNT(*) AS docs_closed"
        sqlGroupBy "time_window"
        sqlGroupBy "uid"
      -- Get the number of closed signatures per time window / user.
      sqlWith "sigs_closed" . sqlSelect "closed c" $ do
        sqlJoinOn "signatory_links sl" "c.did = sl.document_id"
        sqlResult "c.time_window"
        sqlResult "c.uid"
        sqlResult "COUNT(*) AS sigs_closed"
        sqlWhereIsNotNULL "sl.sign_time"
        sqlGroupBy "c.time_window"
        sqlGroupBy "c.uid"
      -- Fetch joined data and sort it appropriately.
      sqlResult "time_window"
      sqlResult "u.email"
      sqlResult "u.first_name || ' ' || u.last_name AS name"
      sqlResult "COALESCE(docs_sent, 0) AS docs_sent"
      sqlResult "COALESCE(docs_closed, 0) AS docs_closed"
      sqlResult "COALESCE(sigs_closed, 0) AS sigs_closed"
      sqlOrderBy "time_window DESC"
      sqlOrderBy "docs_sent DESC"
      sqlOrderBy "docs_closed DESC"
      sqlOrderBy "sigs_closed DESC"
      sqlOrderBy "u.email"
    fetchMany fetchUserUsageStats
    where
      maxSignTime :: SQL
      maxSignTime = parenthesize . toSQLCommand . sqlSelect "signatory_links osl" $ do
        sqlResult "max(osl.sign_time)"
        sqlWhere "osl.document_id = d.id"
        sqlWhere "osl.is_partner"

      selectDataWithDateAs :: SQL -> State SqlSelect () -> SqlSelect
      selectDataWithDateAs ts refine = sqlSelect "documents d" $ do
        sqlJoinOn "signatory_links sl" "d.id = sl.document_id"
        sqlJoinOn "users u" "sl.user_id = u.id"
        sqlResult $ dateTrunc ts <+> "AS time_window"
        sqlResult "u.id AS uid"
        sqlWhere "sl.is_author"
        sqlWhere $ dateTrunc ts <+> ">=" <+> dateTrunc ("now() -" <?> interval)
        case eid of
          Left  uid -> sqlWhereEq "u.id" uid
          Right cid -> sqlWhereEq "u.company_id" cid
        refine

      dateTrunc :: SQL -> SQL
      dateTrunc time = "date_trunc('" <> granularity <> "', " <> time <> ")"
        where
          granularity = case statsPartition of
            PartitionByDay   -> "day"
            PartitionByMonth -> "month"

      fetchUserUsageStats :: (UTCTime, String, String, Int64, Int64, Int64) -> UserUsageStats
      fetchUserUsageStats (time_window_start, user_email, user_name, docs_sent, docs_closed, sigs_closed) = UserUsageStats {
          uusTimeWindowStart = time_window_start
        , uusUserEmail       = user_email
        , uusUserName        = user_name
        , uusDocumentStats   = DocumentStats {
            dsDocumentsSent    = docs_sent
          , dsDocumentsClosed  = docs_closed
          , dsSignaturesClosed = sigs_closed
          }
        }

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

fetchUser :: (UserID, Maybe (Binary ByteString), Maybe (Binary ByteString), Bool, Bool, Maybe UTCTime, SignupMethod, CompanyID, String, String, String, String, String, Email, Lang, BrandedDomainID) -> User
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
  <> ", c.allow_save_safety_copy"
  <> ", c.idle_doc_timeout"
  <> ", c.cgi_display_name"
  <> ", c.sms_provider"
  <> "  FROM users"
  <> "  LEFT JOIN companies c ON users.company_id = c.id"
  <> "  WHERE users.deleted IS NULL"

fetchUserWithCompany :: (UserID, Maybe (Binary ByteString), Maybe (Binary ByteString), Bool, Bool, Maybe UTCTime, SignupMethod, CompanyID, String, String, String, String, String, Email, Lang, BrandedDomainID, Maybe CompanyID, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Maybe String, Bool, Maybe Int16, Maybe String, SMSProvider) -> (User, Company)
fetchUserWithCompany (uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service, signup_method, company_id, first_name, last_name, personal_number, company_position, phone, email, lang, associated_domain_id, cid, name, number, address, zip', city, country, ip_address_mask, allow_save_safety_copy, idle_doc_timeout, cgi_display_name, sms_provider) = (user, company)
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
      companyid = $fromJust cid
    , companyinfo = CompanyInfo {
        companyname = $fromJust name
      , companynumber = $fromJust number
      , companyaddress = $fromJust address
      , companyzip = $fromJust zip'
      , companycity = $fromJust city
      , companycountry = $fromJust country
      , companyipaddressmasklist = maybe [] $read ip_address_mask
      , companyallowsavesafetycopy = allow_save_safety_copy
      , companyidledoctimeout = idle_doc_timeout
      , companycgidisplayname = cgi_display_name
      , companysmsprovider = sms_provider
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
         sqlSet "value_text" (userfstname $ userinfo user)
         sqlWhereEq "type" NameFT
         sqlWhereEq "name_order" (NameOrder 1)
         whereSignatoryLinkCanBeChanged

       runQuery_ $ sqlUpdate "signatory_link_fields" $ do
         sqlSet "value_text" (usersndname $ userinfo user)
         sqlWhereEq "type" NameFT
         sqlWhereEq "name_order" (NameOrder 2)
         whereSignatoryLinkCanBeChanged

       runQuery_ $ sqlUpdate "signatory_link_fields" $ do
         sqlSet "value_text" (useremail $ userinfo user)
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
