{-# OPTIONS_GHC -fno-warn-orphans -fcontext-stack=50 #-}
module User.Model (
    module User.Lang
  , module User.Region
  , module User.Locale
  , module User.Password
  , module User.UserID
  , Email(..)
  , DesignMode(..)
  , InviteType(..)
  , SignupMethod(..)
  , InviteInfo(..)
  , User(..)
  , UserInfo(..)
  , UserMailAPI(..)
  , UserSettings(..)
  , GetUsers(..)
  , GetUserByID(..)
  , GetUserByEmail(..)
  , GetUsersByFriendUserID(..)
  , GetUserFriends(..)
  , GetCompanyAccounts(..)
  , GetInviteInfo(..)
  , GetUserMailAPI(..)
  , ExportUsersDetailsToCSV(..)
  , SetUserCompany(..)
  , DeleteUser(..)
  , AddUser(..)
  , SetUserEmail(..)
  , SetUserPassword(..)
  , SetInviteInfo(..)
  , SetUserMailAPI(..)
  , SetUserInfo(..)
  , SetUserSettings(..)
  , SetPreferredDesignMode(..)
  , AddViewerByEmail(..)
  , AcceptTermsOfService(..)
  , SetSignupMethod(..)
  , SetUserCompanyAdmin(..)
  , composeFullName
  ) where

import Control.Applicative
import Control.Monad
import Data.Data
import Data.Int
import Database.HDBC
import Happstack.State
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
import User.Locale
import User.Password
import User.Region
import User.Tables
import User.UserID

-- newtypes
newtype Email = Email { unEmail :: BS.ByteString }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''Email)
$(newtypeDeriveUnderlyingReadShow ''Email)

-- enums
data DesignMode = BasicMode | AdvancedMode
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DesignMode)

data InviteType = Viral | Admin
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''InviteType)

data SignupMethod = AccountRequest | ViralInvitation | BySigning
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''SignupMethod)

-- data structures
data InviteInfo = InviteInfo {
    userinviter :: UserID
  , invitetime  :: Maybe MinutesTime
  , invitetype  :: Maybe InviteType
  } deriving (Eq, Ord, Show)

data User = User {
    userid                        :: UserID
  , userpassword                  :: Maybe Password
  , useriscompanyadmin            :: Bool
  , useraccountsuspended          :: Bool
  , userhasacceptedtermsofservice :: Maybe MinutesTime
  , usersignupmethod              :: SignupMethod
  , userinfo                      :: UserInfo
  , usersettings                  :: UserSettings
  , userservice                   :: Maybe ServiceID
  , usercompany                   :: Maybe CompanyID
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
  , umapiDailyLimit   :: Int32
  , umapiSentToday    :: Int32
  } deriving (Eq, Ord, Show)

data UserSettings  = UserSettings {
    preferreddesignmode :: Maybe DesignMode
  , locale              :: Locale
  , customfooter        :: Maybe String
  } deriving (Eq, Ord, Show)

instance HasLocale User where
  getLocale = getLocale . usersettings

instance HasLocale UserSettings where
  getLocale = locale

data GetUsers = GetUsers
instance DBQuery GetUsers [User] where
  dbQuery GetUsers = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL ++ " WHERE u.deleted = FALSE ORDER BY u.first_name || ' ' || u.last_name DESC"
    _ <- executeRaw st
    fetchUsers st

data GetUserByID = GetUserByID UserID
instance DBQuery GetUserByID (Maybe User) where
  dbQuery (GetUserByID uid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL ++ " WHERE u.id = ? AND u.deleted = FALSE"
    _ <- execute st [toSql uid]
    us <- fetchUsers st
    oneObjectReturnedGuard us

data GetUserByEmail = GetUserByEmail (Maybe ServiceID) Email
instance DBQuery GetUserByEmail (Maybe User) where
  dbQuery (GetUserByEmail msid email) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL
      ++ " WHERE u.deleted = FALSE AND ((?::TEXT IS NULL AND u.service_id IS NULL) OR u.service_id = ?) AND u.email = ?"
    _ <- execute st [toSql msid, toSql msid, toSql email]
    us <- fetchUsers st
    oneObjectReturnedGuard us

-- | Name may be confusing, but this just returns a list of
-- users who have user with given id marked as their friend
data GetUsersByFriendUserID = GetUsersByFriendUserID UserID
instance DBQuery GetUsersByFriendUserID [User] where
  dbQuery (GetUsersByFriendUserID uid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL
      ++ " JOIN user_friends uf ON (u.id = uf.user_id) WHERE uf.friend_id = ? AND u.deleted = FALSE ORDER BY u.email DESC"
    _ <- execute st [toSql uid]
    fetchUsers st

data GetUserFriends = GetUserFriends UserID
instance DBQuery GetUserFriends [User] where
  dbQuery (GetUserFriends uid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL
      ++ " JOIN user_friends uf ON (u.id = uf.friend_id) WHERE uf.user_id = ? AND u.deleted = FALSE ORDER BY u.email DESC"
    _ <- execute st [toSql uid]
    fetchUsers st

data GetCompanyAccounts = GetCompanyAccounts CompanyID
instance DBQuery GetCompanyAccounts [User] where
  dbQuery (GetCompanyAccounts cid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUsersSQL ++ " WHERE u.company_id = ? ORDER BY u.email DESC"
    _ <- execute st [toSql cid]
    fetchUsers st

data GetInviteInfo = GetInviteInfo UserID
instance DBQuery GetInviteInfo (Maybe InviteInfo) where
  dbQuery (GetInviteInfo uid) = wrapDB $ \conn -> do
    st <- prepare conn "SELECT inviter_id, EXTRACT(EPOCH FROM invite_time), invite_type FROM user_invite_infos WHERE user_id = ?"
    _ <- execute st [toSql uid]
    is <- fetchInviteInfos st []
    oneObjectReturnedGuard is
    where
      fetchInviteInfos st acc = fetchRow st >>= maybe (return acc) f
        where f [inviter_id, invite_time, invite_type
               ] = fetchInviteInfos st $ InviteInfo {
                   userinviter = fromSql inviter_id
                 , invitetime = fromSql invite_time
                 , invitetype = fromSql invite_type
               } : acc
              f l = error $ "fetchInviteInfos: unexpected row: "++show l


data GetUserMailAPI = GetUserMailAPI UserID
instance DBQuery GetUserMailAPI (Maybe UserMailAPI) where
  dbQuery (GetUserMailAPI uid) = wrapDB $ \conn -> do
    st <- prepare conn "SELECT key, daily_limit, (CASE WHEN last_sent_date = now()::DATE THEN sent_today ELSE 0 END) FROM user_mail_apis WHERE user_id = ?"
    _ <- execute st [toSql uid]
    mapis <- fetchUserMailAPIs st []
    oneObjectReturnedGuard mapis
    where
      fetchUserMailAPIs st acc = fetchRow st >>= maybe (return acc) f
        where f [key, daily_limit, sent_today
                ] = fetchUserMailAPIs st $ UserMailAPI {
                    umapiKey = fromSql key
                  , umapiDailyLimit = fromSql daily_limit
                  , umapiSentToday = fromSql sent_today
                } : acc
              f l = error $ "fetchUserMailAPIs: unexpected row: "++show l

data ExportUsersDetailsToCSV = ExportUsersDetailsToCSV
instance DBQuery ExportUsersDetailsToCSV BS.ByteString where
  dbQuery ExportUsersDetailsToCSV = wrapDB $ \conn -> do
    quickQuery conn "SELECT first_name || ' ' || last_name, email FROM users" []
      >>= return . toCSV
    where
      toCSV = BS.unlines . map (BS.intercalate (BS.pack ", ") . map fromSql)

data SetUserCompany = SetUserCompany UserID (Maybe CompanyID)
instance DBUpdate SetUserCompany Bool where
  dbUpdate (SetUserCompany uid mcid) = wrapDB $ \conn -> do
    case mcid of
      Nothing -> do
        st <- prepare conn "UPDATE users SET company_id = NULL, is_company_admin = FALSE WHERE id = ? AND deleted = FALSE"
        r <- execute st [toSql uid]
        oneRowAffectedGuard r
      Just cid -> do
        st <- prepare conn "UPDATE users SET company_id = ? WHERE id = ? AND deleted = FALSE"
        r <- execute st [toSql cid, toSql uid]
        oneRowAffectedGuard r

data DeleteUser = DeleteUser UserID
instance DBUpdate DeleteUser Bool where
  dbUpdate (DeleteUser uid) = wrapDB $ \conn -> do
    runRaw conn "LOCK TABLE users IN ACCESS EXCLUSIVE MODE"
    -- it removes a user and all its references from database.
    -- however, it'll fail if this user is a service admin.
    r <- run conn "DELETE FROM users WHERE id = ?" [toSql uid]
    res <- oneRowAffectedGuard r
    if res
      then do
        _ <- run conn ("INSERT INTO users ("
          ++ "  id"
          ++ ", deleted"
          ++ ", is_company_admin"
          ++ ", account_suspended"
          ++ ", signup_method"
          ++ ", first_name"
          ++ ", last_name"
          ++ ", personal_number"
          ++ ", company_position"
          ++ ", phone"
          ++ ", mobile"
          ++ ", email"
          ++ ", lang"
          ++ ", region) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)") $ [
            toSql uid
          , toSql True
          , toSql False
          , toSql False
          , toSql AccountRequest
          ] ++ replicate 7 (toSql "") ++ [
            toSql (defaultValue::Lang)
          , toSql (defaultValue::Region)
          ]
        return True
      else return False

data AddUser = AddUser (BS.ByteString, BS.ByteString) BS.ByteString (Maybe Password) Bool (Maybe ServiceID) (Maybe CompanyID) Locale
instance DBUpdate AddUser (Maybe User) where
  dbUpdate (AddUser (fname, lname) email mpwd iscompadmin msid mcid l) = do
    let handle e = case e of
          NoObject{} -> return Nothing
          _ -> E.throw e
    mu <- dbQuery (GetUserByEmail msid $ Email email) `catchDB` handle
    case mu of
      Just _ -> return Nothing -- user with the same email address exists
      Nothing -> do
        wrapDB $ \conn -> runRaw conn "LOCK TABLE users IN ACCESS EXCLUSIVE MODE"
        uid <- UserID <$> getUniqueID tableUsers
        wrapDB $ \conn -> do
          _ <- run conn ("INSERT INTO users ("
            ++ "  id"
            ++ ", password"
            ++ ", salt"
            ++ ", is_company_admin"
            ++ ", account_suspended"
            ++ ", has_accepted_terms_of_service"
            ++ ", signup_method"
            ++ ", service_id"
            ++ ", company_id"
            ++ ", first_name"
            ++ ", last_name"
            ++ ", personal_number"
            ++ ", company_position"
            ++ ", phone"
            ++ ", mobile"
            ++ ", email"
            ++ ", preferred_design_mode"
            ++ ", lang"
            ++ ", region"
            ++ ", deleted) VALUES (?, decode(?, 'base64'), decode(?, 'base64'), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)") $ [
                toSql uid
              , toSql $ pwdHash <$> mpwd
              , toSql $ pwdSalt <$> mpwd
              , toSql iscompadmin
              , toSql False
              , SqlNull
              , toSql AccountRequest
              , toSql msid
              , toSql mcid
              , toSql fname
              , toSql lname
              ] ++ replicate 4 (toSql "")
                ++ [toSql email] ++ [
                SqlNull
              , toSql $ getLang l
              , toSql $ getRegion l
              , toSql False
              ]
          return ()
        dbQuery $ GetUserByID uid

--TODO EM, I don't really understand what I'm doing
--and I need to make sure if this is safe ... check
--with someone
data SetUserEmail = SetUserEmail (Maybe ServiceID) UserID Email
instance DBUpdate SetUserEmail Bool where
  dbUpdate (SetUserEmail msid uid email) = do
    let handle e = case e of
          NoObject{} -> return Nothing
          _ -> E.throw e
    mu <- dbQuery (GetUserByEmail msid email) `catchDB` handle
    case mu of
      Just _ -> return False -- user with the same email address exists
      Nothing -> do
        wrapDB $ \conn -> do
        st <- prepare conn $ "UPDATE users SET"
                ++ "  email = ?"
                ++ "  WHERE id = ? AND deleted = FALSE"
        r <- execute st [toSql $ email, toSql $ uid]
        oneRowAffectedGuard r

data SetUserPassword = SetUserPassword UserID Password
instance DBUpdate SetUserPassword Bool where
  dbUpdate (SetUserPassword uid pwd) = wrapDB $ \conn -> do
    st <- prepare conn $ "UPDATE users SET"
      ++ "  password = decode(?, 'base64')"
      ++ ", salt = decode(?, 'base64')"
      ++ "  WHERE id = ? AND deleted = FALSE"
    r <- execute st [toSql $ pwdHash pwd, toSql $ pwdSalt pwd, toSql uid]
    oneRowAffectedGuard r

data SetInviteInfo = SetInviteInfo (Maybe UserID) MinutesTime InviteType UserID
instance DBUpdate SetInviteInfo Bool where
  dbUpdate (SetInviteInfo minviterid invitetime invitetype uid) = do
    exists <- checkIfUserExists uid
    if exists
      then do
        wrapDB $ \conn -> case minviterid of
          Just inviterid -> do
            runRaw conn "LOCK TABLE user_invite_infos IN ACCESS EXCLUSIVE MODE"
            rec_exists <- quickQuery' conn "SELECT 1 FROM user_invite_infos WHERE user_id = ?" [toSql uid]
              >>= checkIfOneObjectReturned
            r <- if rec_exists
              then do
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
              else do
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
            return True
      else return False

data SetUserMailAPI = SetUserMailAPI UserID (Maybe UserMailAPI)
instance DBUpdate SetUserMailAPI Bool where
  dbUpdate (SetUserMailAPI uid musermailapi) = do
    exists <- checkIfUserExists uid
    if exists
      then do
        wrapDB $ \conn -> case musermailapi of
          Just mailapi -> do
            runRaw conn "LOCK TABLE user_mail_apis IN ACCESS EXCLUSIVE MODE"
            rec_exists <- quickQuery' conn "SELECT 1 FROM user_mail_apis WHERE user_id = ?" [toSql uid]
              >>= checkIfOneObjectReturned
            r <- if rec_exists
              then do
                run conn ("UPDATE user_mail_apis SET"
                  ++ "  key = ?"
                  ++ ", daily_limit = ?"
                  ++ ", sent_today = ?"
                  ++ ", last_sent_date = now()"
                  ++ "  WHERE user_id = ?") [
                    toSql $ umapiKey mailapi
                  , toSql $ umapiDailyLimit mailapi
                  , toSql $ umapiSentToday mailapi
                  , toSql uid
                  ]
              else do
                run conn ("INSERT INTO user_mail_apis ("
                  ++ "  user_id"
                  ++ ", key"
                  ++ ", daily_limit"
                  ++ ", sent_today"
                  ++ ", last_sent_date) VALUES (?, ?, ?, ?, now())") [
                    toSql uid
                  , toSql $ umapiKey mailapi
                  , toSql $ umapiDailyLimit mailapi
                  , toSql $ umapiSentToday mailapi
                  ]
            oneRowAffectedGuard r
          Nothing -> do
            _ <- run conn "DELETE FROM user_mail_apis WHERE user_id = ?" [toSql uid]
            return True
      else return False

data SetUserInfo = SetUserInfo UserID UserInfo
instance DBUpdate SetUserInfo Bool where
  dbUpdate (SetUserInfo uid info) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE users SET"
      ++ "  first_name = ?"
      ++ ", last_name = ?"
      ++ ", personal_number = ?"
      ++ ", company_position = ?"
      ++ ", phone = ?"
      ++ ", mobile = ?"
      ++ ", email = ?"
      ++ "  WHERE id = ?") [
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
instance DBUpdate SetUserSettings Bool where
  dbUpdate (SetUserSettings uid us) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE users SET"
      ++ "  preferred_design_mode = ?"
      ++ ", lang = ?"
      ++ ", region = ?"
      ++ ", customfooter = ?"
      ++ "  WHERE id = ?") [
        toSql $ preferreddesignmode us
      , toSql $ getLang us
      , toSql $ getRegion us
      , toSql $ customfooter us
      , toSql uid
      ]
    oneRowAffectedGuard r

data SetPreferredDesignMode = SetPreferredDesignMode UserID (Maybe DesignMode)
instance DBUpdate SetPreferredDesignMode Bool where
  dbUpdate (SetPreferredDesignMode uid mmode) = wrapDB $ \conn -> do
    r <- run conn "UPDATE users SET preferred_design_mode = ? WHERE id = ?"
      [toSql mmode, toSql uid]
    oneRowAffectedGuard r
    
data AddViewerByEmail = AddViewerByEmail UserID Email
instance DBUpdate AddViewerByEmail Bool where
  dbUpdate (AddViewerByEmail uid email) = wrapDB $ \conn -> do
    mfid <- quickQuery' conn "SELECT id FROM users WHERE service_id IS NULL AND email = ?" [toSql email]
      >>= oneObjectReturnedGuard . join
      >>= return . fmap (UserID . fromSql)
    case mfid of
      Just fid -> do
        runRaw conn "LOCK TABLE user_friends IN ACCESS EXCLUSIVE MODE"
        rec_exists <- quickQuery' conn "SELECT 1 FROM user_friends WHERE user_id = ? AND friend_id = ?" [toSql uid, toSql fid]
          >>= checkIfOneObjectReturned
        if rec_exists
          then return True
          else do
            _ <- run conn "INSERT INTO user_friends (user_id, friend_id) VALUES (?, ?)"
              [toSql uid, toSql fid]
            return True
      Nothing -> return False

data AcceptTermsOfService = AcceptTermsOfService UserID MinutesTime
instance DBUpdate AcceptTermsOfService Bool where
  dbUpdate (AcceptTermsOfService uid time) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE users SET"
      ++ "  has_accepted_terms_of_service = to_timestamp(?)"
      ++ "  WHERE id = ? AND deleted = FALSE") [
        toSql time
      , toSql uid
      ]
    oneRowAffectedGuard r

data SetSignupMethod = SetSignupMethod UserID SignupMethod
instance DBUpdate SetSignupMethod Bool where
  dbUpdate (SetSignupMethod uid signupmethod) = wrapDB $ \conn -> do
    r <- run conn ("UPDATE users SET signup_method = ? WHERE id = ? AND deleted = FALSE")
      [toSql signupmethod, toSql uid]
    oneRowAffectedGuard r

data SetUserCompanyAdmin = SetUserCompanyAdmin UserID Bool
instance DBUpdate SetUserCompanyAdmin Bool where
  dbUpdate (SetUserCompanyAdmin uid iscompanyadmin) = wrapDB $ \conn -> do
    mcid <- quickQuery' conn "SELECT company_id FROM users WHERE id = ? AND deleted = FALSE FOR UPDATE" [toSql uid]
      >>= oneObjectReturnedGuard . join
      >>= return . join . fmap fromSql
    case mcid :: Maybe CompanyID of
      Nothing -> return False
      Just _ -> do
        run conn "UPDATE users SET is_company_admin = ? WHERE id = ? AND deleted = FALSE" [toSql iscompanyadmin, toSql uid]
          >>= oneRowAffectedGuard

-- helpers

composeFullName :: (BS.ByteString, BS.ByteString) -> BS.ByteString
composeFullName (fstname, sndname) =
    if BS.null sndname
       then fstname
       else fstname `BS.append` BS.pack " " `BS.append` sndname

checkIfUserExists :: UserID -> DB Bool
checkIfUserExists uid = wrapDB $ \conn -> do
  quickQuery' conn "SELECT 1 FROM users WHERE id = ? AND deleted = FALSE" [toSql uid]
    >>= checkIfOneObjectReturned

selectUsersSQL :: String
selectUsersSQL = "SELECT "
 ++ "  u.id"
 ++ ", encode(u.password, 'base64')"
 ++ ", encode(u.salt, 'base64')"
 ++ ", u.is_company_admin"
 ++ ", u.account_suspended"
 ++ ", EXTRACT(EPOCH FROM u.has_accepted_terms_of_service)"
 ++ ", u.signup_method"
 ++ ", u.service_id"
 ++ ", u.company_id"
 ++ ", u.first_name"
 ++ ", u.last_name"
 ++ ", u.personal_number"
 ++ ", u.company_position"
 ++ ", u.phone"
 ++ ", u.mobile"
 ++ ", u.email"
 ++ ", u.preferred_design_mode"
 ++ ", u.lang"
 ++ ", u.region"
 ++ ", u.customfooter"
  ++ "  FROM users u"
 ++ " "

fetchUsers :: Statement -> IO [User]
fetchUsers st = do
  fetchValues st decoder
  where decoder uid password salt is_company_admin account_suspended has_accepted_terms_of_service
                signup_method service_id company_id first_name
                last_name personal_number company_position phone mobile email
                preferred_design_mode lang region customfooter
                = (return $ User
                  { userid = uid
                  , userpassword = case (password, salt) of
                                     (Just pwd, Just salt') -> Just Password
                                                               { pwdHash = pwd
                                                               , pwdSalt = salt'
                                                               }
                                     _ -> Nothing
                  , useriscompanyadmin = is_company_admin
                  , useraccountsuspended = account_suspended
                  , userhasacceptedtermsofservice = has_accepted_terms_of_service
                  , usersignupmethod = signup_method
                  , userinfo = UserInfo
                               { userfstname = first_name
                               , usersndname = last_name
                               , userpersonalnumber = personal_number
                               , usercompanyposition = company_position
                               , userphone = phone
                               , usermobile = mobile
                               , useremail = email
                               }
                  , usersettings = UserSettings
                                   { preferreddesignmode = preferred_design_mode
                                   , locale = mkLocale (region) (lang)
                                   , customfooter = customfooter
                                   }
                  , userservice = service_id
                  , usercompany = company_id
                  }) :: Either DBException User

-- this will not be needed when we move documents to pgsql. for now it's needed
-- for document handlers - it seems that types of arguments that handlers take
-- need to be serializable. I don't know wtf, but I'll gladly dispose of these
-- instances when we're done with the migration.

deriving instance Typeable User
deriving instance Typeable UserSettings
deriving instance Typeable UserInfo
deriving instance Typeable SignupMethod
deriving instance Typeable Password
deriving instance Typeable Lang
deriving instance Typeable Region
deriving instance Typeable Locale
deriving instance Typeable DesignMode
deriving instance Typeable Email
deriving instance Typeable Binary

instance Version User
instance Version UserSettings
instance Version UserInfo
instance Version SignupMethod
instance Version Password
instance Version Lang
instance Version Region
instance Version Locale
instance Version DesignMode
instance Version Email
instance Version Binary

$(deriveSerializeFor [
    ''User
  , ''UserSettings
  , ''UserInfo
  , ''SignupMethod
  , ''Password
  , ''Lang
  , ''Region
  , ''Locale
  , ''DesignMode
  , ''Email
  , ''Binary
  ])
