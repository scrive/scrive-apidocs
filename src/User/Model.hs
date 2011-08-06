module User.Model (
    Email(..)
  , ExternalUserID(..)
  , UserID
  , DesignMode(..)
  , InviteType(..)
  , PaymentMethod(..)
  , SignupMethod(..)
  , InviteInfo(..)
  , LoginInfo(..)
  , User(..)
  , UserInfo(..)
  , UserMailAPI(..)
  , UserSettings(..)
  , UserStats(..)
  , TrustWeaverStorage(..)
  , GetUsers(..)
  , GetUserByID(..)
  , GetUserByEmail(..)
  , GetUsersByFriendUserID(..)
  , GetUserFriends(..)
  , GetCompanyAccounts(..)
  , GetInviteInfo(..)
  , GetUserMailAPI(..)
  , GetUserStats(..)
  , GetUserStatsByUser(..)
  , ExportUsersDetailsToCSV(..)
  , SetUserCompany(..)
  , DeleteUser(..)
  , AddUser(..)
  , SetUserPassword(..)
  , SetInviteInfo(..)
  , SetUserMailAPI(..)
  , SetUserInfo(..)
  , SetUserSettings(..)
  , SetUserPaymentAccountType(..)
  , RecordFailedLogin(..)
  , RecordSuccessfulLogin(..)
  , AddVieverByEmail(..)
  , AcceptTermsOfService(..)
  , SetFreeTrialExpirationDate(..)
  , SetSignupMethod(..)
  , MakeUserCompanyAdmin(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Database.HDBC
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import API.Service.Model
import Company.Model
import DB.Classes
import DB.Derive
import DB.Types
import DB.Utils
import MinutesTime
import Misc
import User.Lang
import User.Password
import User.SystemServer
import User.UserID
import Payments.Model

-- newtypes
newtype Email = Email { unEmail :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''Email)
$(newtypeDeriveUnderlyingReadShow ''Email)

newtype ExternalUserID = ExternalUserID { unExternalUserID :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''ExternalUserID)
$(newtypeDeriveUnderlyingReadShow ''ExternalUserID)

-- enums
data DesignMode = BasicMode | AdvancedMode
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DesignMode)

data InviteType = Viral | Admin
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''InviteType)

data PaymentMethod = CreditCard | Invoice | Undefined
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''PaymentMethod)

data SignupMethod = AccountRequest | ViralInvitation | BySigning
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''SignupMethod)

-- data structures
data InviteInfo = InviteInfo {
    userinviter :: UserID
  , invitetime  :: Maybe MinutesTime
  , invitetype  :: Maybe InviteType
  } deriving (Eq, Ord, Show)

data LoginInfo = LoginInfo {
    lastsuccesstime  :: Maybe MinutesTime
  , lastfailtime     :: Maybe MinutesTime
  , consecutivefails :: Int
  } deriving (Eq, Ord, Show)

data User = User {
    userid                        :: UserID
  , userpassword                  :: Maybe Password
  , useriscompanyadmin            :: Bool
  , useraccountsuspended          :: Bool
  , userhasacceptedtermsofservice :: Maybe MinutesTime
  , userfreetrialexpirationdate   :: Maybe MinutesTime
  , usersignupmethod              :: SignupMethod
  , userinfo                      :: UserInfo
  , usersettings                  :: UserSettings
  , userpaymentaccounttype        :: PaymentAccountType
  -- unused, so we don't fetch it with user for now
  --, userlogininfo                 :: LoginInfo
  , userservice                   :: Maybe ServiceID
  , usercompany                   :: Maybe CompanyID
  , userdeleted                   :: Bool
  } deriving (Eq, Ord, Show)

data UserInfo = UserInfo {
    userfstname         :: BS.ByteString
  , usersndname         :: BS.ByteString
  , userpersonalnumber  :: BS.ByteString
  , usercompanyposition :: BS.ByteString
  , userphone           :: BS.ByteString
  , usermobile          :: BS.ByteString
  , useremail           :: Email
  } deriving (Eq, Ord, Show)

data UserMailAPI = UserMailAPI {
    umapiKey          :: MagicHash
  , umapiDailyLimit   :: Int
  , umapiSentToday    :: Int
  , umapiLastSentDate :: Int
  } deriving (Eq, Ord, Show)

data UserSettings  = UserSettings {
  -- signeddocstorage    :: Maybe TrustWeaverStorage
    userpaymentmethod   :: PaymentMethod
  , preferreddesignmode :: Maybe DesignMode
  , lang                :: Lang
  , systemserver        :: SystemServer
  } deriving (Eq, Ord, Show)

data UserStats = UserStats {
    usercount        :: Int
  , viralinvitecount :: Int
  , admininvitecount :: Int
  } deriving (Eq, Ord, Show)

-- currently unused.
data TrustWeaverStorage = TrustWeaverStorage {
    storagetwenabled       :: Bool
  , storagetwname          :: BS.ByteString
  , storagetwsuperadmin    :: BS.ByteString
  , storagetwsuperadminpwd :: BS.ByteString
  , storagetwsectionpath   :: BS.ByteString
  } deriving (Eq, Ord, Show)

data GetUsers = GetUsers
instance DBQuery GetUsers [User] where
  dbQuery GetUsers = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL ++ " WHERE u.deleted = FALSE ORDER BY ui.first_name || ' ' || ui.last_name DESC"
    _ <- executeRaw st
    fetchUsers st []

data GetUserByID = GetUserByID UserID
instance DBQuery GetUserByID User where
  dbQuery (GetUserByID uid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL ++ " WHERE u.id = ? AND u.deleted = FALSE"
    _ <- execute st [toSql uid]
    us <- fetchUsers st []
    oneObjectReturnedGuard us

data GetUserByEmail = GetUserByEmail (Maybe ServiceID) Email
instance DBQuery GetUserByEmail User where
  dbQuery (GetUserByEmail msid email) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL
      ++ " WHERE u.deleted = FALSE AND ((?::BIGINT IS NULL AND u.service_id IS NULL) OR u.service_id = ?) AND ui.email = ?"
    _ <- execute st [toSql msid, toSql msid, toSql email]
    us <- fetchUsers st []
    oneObjectReturnedGuard us

data GetUsersByFriendUserID = GetUsersByFriendUserID UserID
instance DBQuery GetUsersByFriendUserID [User] where
  dbQuery (GetUsersByFriendUserID uid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL
      ++ " JOIN user_friends uf ON (u.id = uf.user_id) WHERE uf.friend_id = ? AND u.deleted = FALSE ORDER BY ui.email DESC"
    _ <- execute st [toSql uid]
    fetchUsers st []

data GetUserFriends = GetUserFriends UserID
instance DBQuery GetUserFriends [User] where
  dbQuery (GetUserFriends uid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL
      ++ " JOIN user_friends uf ON (u.id = uf.friend_id) WHERE uf.user_id = ? AND u.deleted = FALSE ORDER BY ui.email DESC"
    _ <- execute st [toSql uid]
    fetchUsers st []

data GetCompanyAccounts = GetCompanyAccounts UserID
instance DBQuery GetCompanyAccounts [User] where
  dbQuery (GetCompanyAccounts uid) = wrapDB $ \conn -> do
    st <- prepare conn "SELECT company_id, is_company_admin FROM users WHERE id = ? AND deleted = FALSE"
    _ <- execute st [toSql uid]
    (cid :: Maybe CompanyID, iscompadmin) <- (\[a, b] -> (fromSql a, fromSql b)) <$> (fetchAllRows' st >>= oneObjectReturnedGuard)
    when (isNothing cid) $
      error "User doesn't belong to a company"
    when (not iscompadmin) $
      error "User is not a company admin"
    st' <- prepare conn $ selectUsersSQL ++ " WHERE u.company_id = ?"
    _ <- execute st' [toSql cid]
    fetchUsers st []

data GetInviteInfo = GetInviteInfo UserID
instance DBQuery GetInviteInfo InviteInfo where
  dbQuery (GetInviteInfo uid) = wrapDB $ \conn -> do
    st <- prepare conn "SELECT inviter_id, EXTRACT(EPOCH FROM invite_time), invite_type FROM user_invite_infos WHERE user_id = ?"
    _ <- execute st [toSql uid]
    is <- fetchInviteInfos st []
    oneObjectReturnedGuard is
    where
      fetchInviteInfos st acc = fetchRow st >>= maybe (return acc)
        (\[inviter_id, invite_time, invite_type
         ] -> fetchInviteInfos st $ InviteInfo {
             userinviter = fromSql inviter_id
           , invitetime = fromSql invite_time
           , invitetype = fromSql invite_type
         } : acc)

data GetUserMailAPI = GetUserMailAPI UserID
instance DBQuery GetUserMailAPI UserMailAPI where
  dbQuery (GetUserMailAPI uid) = wrapDB $ \conn -> do
    st <- prepare conn "SELECT key, daily_limit, sent_today, last_sent_date FROM user_mail_apis WHERE user_id = ?"
    _ <- execute st [toSql uid]
    mapis <- fetchUserMailAPIs st []
    oneObjectReturnedGuard mapis
    where
      fetchUserMailAPIs st acc = fetchRow st >>= maybe (return acc)
        (\[key, daily_limit, sent_today, last_sent_date
         ] -> fetchUserMailAPIs st $ UserMailAPI {
             umapiKey = fromSql key
           , umapiDailyLimit = fromSql daily_limit
           , umapiSentToday = fromSql sent_today
           , umapiLastSentDate = fromSql last_sent_date
         } : acc)

data GetUserStats = GetUserStats
instance DBQuery GetUserStats UserStats where
  dbQuery GetUserStats = undefined

data GetUserStatsByUser = GetUserStatsByUser UserID
instance DBQuery GetUserStatsByUser UserStats where
  dbQuery (GetUserStatsByUser _uid) = undefined

data ExportUsersDetailsToCSV = ExportUsersDetailsToCSV
instance DBQuery ExportUsersDetailsToCSV BS.ByteString where
  dbQuery ExportUsersDetailsToCSV = wrapDB $ \conn -> do
    st <- prepare conn "SELECT first_name || ' ' || last_name, email FROM user_infos"
    executeRaw st
    return . toCSV =<< fetchAllRows st
    where
      toCSV = BS.unlines . map (BS.intercalate (BS.pack ", ") . map fromSql)

data SetUserCompany = SetUserCompany UserID CompanyID
instance DBUpdate SetUserCompany () where
  dbUpdate (SetUserCompany uid cid) = wrapDB $ \conn -> do
    st <- prepare conn "UPDATE users SET company_id = ? WHERE id = ? AND deleted = FALSE"
    r <- execute st [toSql cid, toSql uid]
    oneRowAffectedGuard r

data DeleteUser = DeleteUser UserID
instance DBUpdate DeleteUser () where
  dbUpdate (DeleteUser uid) = wrapDB $ \conn -> do
    r <- run conn "DELETE FROM users WHERE id = ?" [toSql uid]
    oneRowAffectedGuard r
    _ <- run conn ("INSERT INTO users ("
      ++ "  id"
      ++ ", is_company_admin"
      ++ ", account_suspended"
      ++ ", signup_method"
      ++ ", deleted) VALUES (?, ?, ?, ?, ?)") [
        toSql uid
      , toSql False
      , toSql False
      , toSql AccountRequest
      , toSql True
      ]
    return ()

data AddUser = AddUser (BS.ByteString, BS.ByteString) BS.ByteString (Maybe Password) Bool (Maybe ServiceID) (Maybe CompanyID) SystemServer
instance DBUpdate AddUser User where
  dbUpdate (AddUser (fname, lname) email mpwd iscompadmin msid mcid ss) = do
    let handle e = case e of
          NoObject -> return Nothing
          _ -> E.throw e
    u <- (Just <$> dbQuery (GetUserByEmail msid $ Email email)) `catchDB` handle
    when (isJust u) $
      error "User with the same email address exists"
    uid <- UserID <$> getUniqueID "users"
    wrapDB $ \conn -> do
      _ <- run conn ("INSERT INTO users ("
        ++ "  id"
        ++ ", password"
        ++ ", salt"
        ++ ", is_company_admin"
        ++ ", account_suspended"
        ++ ", has_accepted_terms_of_service"
        ++ ", free_trial_expiration_date"
        ++ ", signup_method"
        ++ ", service_id"
        ++ ", company_id"
        ++ ", deleted) VALUES (?, decode(?, 'base64'), decode(?, 'base64'), ?, ?, ?, ?, ?, ?, ?, ?)") [
            toSql uid
          , toSql $ pwdHash <$> mpwd
          , toSql $ pwdSalt <$> mpwd
          , toSql iscompadmin
          , toSql False
          , SqlNull
          , SqlNull
          , toSql AccountRequest
          , toSql msid
          , toSql mcid
          , toSql False
          ]
      _ <- run conn ("INSERT INTO user_infos ("
        ++ "  user_id"
        ++ ", first_name"
        ++ ", last_name"
        ++ ", personal_number"
        ++ ", company_position"
        ++ ", phone"
        ++ ", mobile"
        ++ ", email) VALUES (?, ?, ?, ?, ?, ?, ?, ?)") $ [
          toSql uid
        , toSql fname
        , toSql lname
        ] ++ replicate 4 (toSql "")
          ++ [toSql email]
      _ <- run conn ("INSERT INTO user_settings ("
        ++ "  user_id"
        ++ ", payment_method"
        ++ ", preferred_design_mode"
        ++ ", lang"
        ++ ", system_server) VALUES (?, ?, ?, ?, ?)") [
            toSql uid
          , toSql Undefined
          , SqlNull
          , toSql (defaultValue::Lang)
          , toSql ss
          ]
      _ <- run conn ("INSERT INTO user_payment_policies ("
        ++ "  user_id"
        ++ ", account_type) VALUES (?, ?)") [
            toSql uid
          , toSql FreeTrial
          ]
      _ <- run conn ("INSERT INTO user_login_infos ("
        ++ "  user_id"
        ++ ", last_success"
        ++ ", last_fail"
        ++ ", consecutive_fails) VALUES (?, ?, ?, ?)") [
            toSql uid
          , SqlNull
          , SqlNull
          , toSql (0::Int)
          ]
      return ()
    dbQuery $ GetUserByID uid

data SetUserPassword = SetUserPassword UserID Password
instance DBUpdate SetUserPassword () where
  dbUpdate (SetUserPassword uid pwd) = wrapDB $ \conn -> do
    st <- prepare conn $ "UPDATE users SET"
      ++ "  password = decode(?, base64')"
      ++ ", salt = decode(?, 'base64')"
      ++ "  WHERE id = ? AND deleted = FALSE"
    r <- execute st [toSql $ pwdHash pwd, toSql $ pwdSalt pwd, toSql uid]
    oneRowAffectedGuard r

data SetInviteInfo = SetInviteInfo (Maybe UserID) MinutesTime InviteType UserID
instance DBUpdate SetInviteInfo () where
  dbUpdate (SetInviteInfo minviterid invitetime invitetype uid) = do
    checkIfUserExists uid
    wrapDB $ \conn -> case minviterid of
      Just inviterid -> do
        st <- prepare conn "SELECT user_id FROM user_invite_infos WHERE user_id = ?"
        _ <- execute st [toSql uid]
        mrow <- fetchRow st
        r <- case mrow of
          Just _ -> do
            run conn ("UPDATE user_invite_infos SET"
              ++ "  inviter_id = ?"
              ++ ", invite_time = to_timestamp(?)"
              ++ ", invite_type = ?"
              ++ "  WHERE user_id = ?") [
                toSql inviterid
              , toSql invitetime
              , toSql invitetype
              , toSql uid
              ]
          Nothing -> do
            run conn ("INSERT INTO user_invite_infos ("
              ++ "  user_id"
              ++ ", inviter_id"
              ++ ", invite_time"
              ++ ", invite_type) VALUES (?, ?, to_timestamp(?), ?)") [
                toSql uid
              , toSql inviterid
              , toSql invitetime
              , toSql invitetype
              ]
        oneRowAffectedGuard r
      Nothing -> do
        _ <- run conn "DELETE FROM user_invite_infos WHERE user_id = ?" [toSql uid]
        return ()

data SetUserMailAPI = SetUserMailAPI UserID (Maybe UserMailAPI)
instance DBUpdate SetUserMailAPI () where
  dbUpdate (SetUserMailAPI uid musermailapi) = do
    checkIfUserExists uid
    wrapDB $ \conn -> case musermailapi of
      Just mailapi -> do
        st <- prepare conn "SELECT user_id FROM user_mail_apis WHERE user_id = ?"
        _ <- execute st [toSql uid]
        mrow <- fetchRow st
        r <- case mrow of
          Just _ -> do
            run conn ("UPDATE user_mail_apis SET"
              ++ "  key = ?"
              ++ ", daily_limit = ?"
              ++ ", sent_today = ?"
              ++ ", last_sent_date = ?"
              ++ "  WHERE user_id = ?") [
                toSql $ umapiKey mailapi
              , toSql $ umapiDailyLimit mailapi
              , toSql $ umapiSentToday mailapi
              , toSql $ umapiLastSentDate mailapi
              , toSql uid
              ]
          Nothing -> do
            run conn ("INSERT INTO user_mail_apis ("
              ++ "  user_id"
              ++ ", key"
              ++ ", daily_limit"
              ++ ", sent_today"
              ++ ", last_sent_date VALUES (?, ?, ?, ?, ?") [
                toSql uid
              , toSql $ umapiKey mailapi
              , toSql $ umapiDailyLimit mailapi
              , toSql $ umapiSentToday mailapi
              , toSql $ umapiLastSentDate mailapi
              ]
        oneRowAffectedGuard r
      Nothing -> do
        _ <- run conn "DELETE FROM user_mail_apis WHERE user_id = ?" [toSql uid]
        return ()

data SetUserInfo = SetUserInfo UserID UserInfo
instance DBUpdate SetUserInfo () where
  dbUpdate (SetUserInfo uid info) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE user_infos SET"
      ++ "  first_name = ?"
      ++ ", last_name = ?"
      ++ ", personal_number = ?"
      ++ ", company_position = ?"
      ++ ", phone = ?"
      ++ ", mobile = ?"
      ++ ", email = ?"
      ++ "  WHERE user_id = ?") [
        toSql $ userfstname info
      , toSql $ usersndname info
      , toSql $ userpersonalnumber info
      , toSql $ usercompanyposition info
      , toSql $ userphone info
      , toSql $ usermobile info
      , toSql $ useremail info
      , toSql uid
      ]
    oneRowAffectedGuard r

data SetUserSettings = SetUserSettings UserID UserSettings
instance DBUpdate SetUserSettings () where
  dbUpdate (SetUserSettings uid us) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE user_settings SET"
      ++ "  payment_method = ?"
      ++ ", preferred_design_mode = ?"
      ++ ", lang = ?"
      ++ ", system_server = ?"
      ++ "  WHERE user_id = ?") [
        toSql $ userpaymentmethod us
      , toSql $ preferreddesignmode us
      , toSql $ lang us
      , toSql $ systemserver us
      , toSql uid
      ]
    oneRowAffectedGuard r

data SetUserPaymentAccountType = SetUserPaymentAccountType UserID PaymentAccountType
instance DBUpdate SetUserPaymentAccountType () where
  dbUpdate (SetUserPaymentAccountType uid paymentacctype) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE user_payment_policies SET"
      ++ " account_type = ?"
      ++ " WHERE user_id = ?") [
        toSql paymentacctype
      , toSql uid
      ]
    oneRowAffectedGuard r

data RecordFailedLogin = RecordFailedLogin UserID MinutesTime
instance DBUpdate RecordFailedLogin () where
  dbUpdate (RecordFailedLogin uid time) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE user_login_infos SET"
      ++ "  last_fail = to_timestamp(?)"
      ++ ", consecutive_fails = consecutive_fails + 1"
      ++ "  WHERE user_id = ?") [
        toSql time
      , toSql uid
      ]
    oneRowAffectedGuard r

data RecordSuccessfulLogin = RecordSuccessfulLogin UserID MinutesTime
instance DBUpdate RecordSuccessfulLogin () where
  dbUpdate (RecordSuccessfulLogin uid time) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE user_login_infos SET"
      ++ "  last_success = to_timestamp(?)"
      ++ ", consecutive_fails = 0"
      ++ "  WHERE user_id = ?") [
        toSql time
      , toSql uid
      ]
    oneRowAffectedGuard r

-- Note: if this returns Left, errmsg is returned as a flash message. Fix this.
data AddVieverByEmail = AddVieverByEmail UserID Email
instance DBUpdate AddVieverByEmail () where
  dbUpdate (AddVieverByEmail uid email) = wrapDB $ \conn -> do
    st <- prepare conn "SELECT user_id FROM user_infos WHERE email = ?"
    _ <- execute st [toSql email]
    fid <- UserID . fromSql <$> (fetchAllRows' st >>= oneObjectReturnedGuard . join)
    _ <- run conn "INSERT INTO user_friends (user_id, friend_id) VALUES (?, ?)"
      [toSql uid, toSql fid]
    return ()

data AcceptTermsOfService = AcceptTermsOfService UserID MinutesTime
instance DBUpdate AcceptTermsOfService () where
  dbUpdate (AcceptTermsOfService uid time) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE users SET"
      ++ "  has_accepted_terms_of_service = to_timestamp(?)"
      ++ ", free_trial_expiration_date = to_timestamp(?)"
      ++ "  WHERE id = ? AND deleted = FALSE") [
        toSql time
      , toSql $ (60*24*30) `minutesAfter` time
      , toSql uid
      ]
    oneRowAffectedGuard r

data SetFreeTrialExpirationDate = SetFreeTrialExpirationDate UserID (Maybe MinutesTime)
instance DBUpdate SetFreeTrialExpirationDate () where
  dbUpdate (SetFreeTrialExpirationDate uid mtime) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE users SET"
      ++ " free_trial_expiration_date = to_timestamp(?)"
      ++ " WHERE id = ? AND deleted = FALSE") [
        toSql mtime
      , toSql uid
      ]
    oneRowAffectedGuard r

data SetSignupMethod = SetSignupMethod UserID SignupMethod
instance DBUpdate SetSignupMethod () where
  dbUpdate (SetSignupMethod uid signupmethod) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE users SET signup_method = ? WHERE id = ? AND deleted = FALSE")
      [toSql signupmethod, toSql uid]
    oneRowAffectedGuard r

data MakeUserCompanyAdmin = MakeUserCompanyAdmin UserID
instance DBUpdate MakeUserCompanyAdmin () where
  dbUpdate (MakeUserCompanyAdmin uid) = wrapDB $ \conn -> do
    st <- prepare conn "SELECT company_id FROM users WHERE id = ? AND deleted = FALSE"
    _ <- execute st [toSql uid]
    mcid :: Maybe CompanyID <- fromSql <$> (fetchAllRows' st >>= oneObjectReturnedGuard . join)
    case mcid of
      Nothing -> error "User doesn't belong to a company"
      Just _ -> do
        r <- run conn "UPDATE users SET is_company_admin = TRUE WHERE id = ? AND deleted = FALSE" [toSql uid]
        oneRowAffectedGuard r

-- helpers

checkIfUserExists :: UserID -> DB ()
checkIfUserExists uid = wrapDB $ \conn -> do
  st <- prepare conn "SELECT id FROM users WHERE id = ?"
  _ <- execute st [toSql uid]
  _ <- fetchAllRows' st >>= oneObjectReturnedGuard
  return ()

selectUsersSQL :: String
selectUsersSQL = "SELECT u.id, encode(u.password, 'base64'), encode(u.salt, 'base64'), u.is_company_admin, u.account_suspended, EXTRACT(EPOCH FROM u.has_accepted_terms_of_service), EXTRACT(EPOCH FROM u.free_trial_expiration_date), u.signup_method, u.service_id, u.company_id, u.deleted, ui.first_name, ui.last_name, ui.personal_number, ui.company_position, ui.phone, ui.mobile, ui.email, us.payment_method, us.preferred_design_mode, us.lang, us.system_server, upp.account_type FROM users u JOIN user_infos ui ON (u.id = ui.user_id) JOIN user_settings us ON (u.id = us.user_id) JOIN user_payment_policies upp ON (u.id = upp.user_id) "

fetchUsers :: Statement -> [User] -> IO [User]
fetchUsers st acc = fetchRow st >>= maybe (return acc)
  (\[uid, password, salt, is_company_admin, account_suspended, has_accepted_terms_of_service
   , free_trial_expiration_date, signup_method, service_id, company_id, deleted, first_name
   , last_name, personal_number, company_position, phone, mobile, email, payment_method
   , preferred_design_mode, lang, system_server, payment_account_type
   ] -> fetchUsers st $ User {
       userid = fromSql uid
     , userpassword = case (fromSql password, fromSql salt) of
                           (Just pwd, Just salt') -> Just Password {
                               pwdHash = pwd
                             , pwdSalt = salt'
                           }
                           _ -> Nothing
     , useriscompanyadmin = fromSql is_company_admin
     , useraccountsuspended = fromSql account_suspended
     , userhasacceptedtermsofservice = fromSql has_accepted_terms_of_service
     , userfreetrialexpirationdate = fromSql free_trial_expiration_date
     , usersignupmethod = fromSql signup_method
     , userinfo = UserInfo {
         userfstname = fromSql first_name
       , usersndname = fromSql last_name
       , userpersonalnumber = fromSql personal_number
       , usercompanyposition = fromSql company_position
       , userphone = fromSql phone
       , usermobile = fromSql mobile
       , useremail = fromSql email
     }
     , usersettings = UserSettings {
         userpaymentmethod = fromSql payment_method
       , preferreddesignmode = fromSql preferred_design_mode
       , lang = fromSql lang
       , systemserver = fromSql system_server
     }
     , userpaymentaccounttype = fromSql payment_account_type
     , userservice = fromSql service_id
     , usercompany = fromSql company_id
     , userdeleted = fromSql deleted
   } : acc)
