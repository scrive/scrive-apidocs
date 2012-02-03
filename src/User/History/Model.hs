module User.History.Model (
    UserHistory(..)
  , UserHistoryID(..)
  , UserHistoryEvent(..)
  , UserHistoryEventType(..)
  , LogHistoryLoginAttempt(..)
  , LogHistoryLoginSuccess(..)
  , LogHistoryPasswordSetup(..)
  , LogHistoryPasswordSetupReq(..)
  , LogHistoryAccountCreated(..)
  , LogHistoryTOSAccept(..)
  , LogHistoryDetailsChanged(..)
  , LogHistoryUserInfoChanged(..)
  , AddUserHistory(..)
  , GetUserHistoryByID(..)
  , GetUserHistoryByUserID(..)
  ) where

import Control.Applicative
import Data.Data
import Data.Int
import Data.List (intersperse)
import Database.HDBC
import Happstack.State
import qualified Data.ByteString.Char8 as BS
import Text.JSON

import qualified Paths_kontrakcja as Paths
import qualified Data.Version as Ver

import DB.Classes
import DB.Derive
import DB.Fetcher
import DB.Utils

import MinutesTime
import Misc (IPAddress)

import User.History.Tables
import User.UserID
import User.Model

newtype UserHistoryID = UserHistoryID Int64
  deriving (Eq, Typeable)
$(newtypeDeriveConvertible ''UserHistoryID)
$(newtypeDeriveUnderlyingReadShow ''UserHistoryID)
$(deriveSerialize ''UserHistoryID)
instance Version UserHistoryID

data UserHistory = UserHistory {
    uhid               :: UserHistoryID
  , uhuserid           :: UserID
  , uhevent            :: UserHistoryEvent
  , uhip               :: IPAddress
  , uhtime             :: MinutesTime
  , uhsystemversion    :: BS.ByteString
  , uhperforminguserid :: Maybe UserID -- Nothing means system changed it
  }
  deriving (Eq, Show)

data UserHistoryEvent = UserHistoryEvent {
    uheventtype :: UserHistoryEventType
  , uheventdata :: Maybe JSValue
  }
  deriving (Eq, Show)

data UserHistoryEventType = UserLoginAttempt 
                          | UserLoginSuccess 
                          | UserPasswordSetup 
                          | UserPasswordSetupReq 
                          | UserAccountCreated 
                          | UserDetailsChange 
                          | UserTOSAccept
  deriving (Eq, Show)

{- |
  UserPasswordSetup is a successful change but UserPasswordSetupReq is
  only a request, not successful change.
 -}
instance Convertible UserHistoryEventType SqlValue where
  safeConvert UserLoginAttempt     = return . toSql $ (1 :: Int)
  safeConvert UserLoginSuccess     = return . toSql $ (2 :: Int)
  safeConvert UserPasswordSetup    = return . toSql $ (3 :: Int)
  safeConvert UserPasswordSetupReq = return . toSql $ (4 :: Int)
  safeConvert UserAccountCreated   = return . toSql $ (5 :: Int)
  safeConvert UserDetailsChange    = return . toSql $ (6 :: Int)
  safeConvert UserTOSAccept        = return . toSql $ (7 :: Int)

instance Convertible SqlValue UserHistoryEventType where
  safeConvert a = case (fromSql a :: Int) of
                  1 -> return UserLoginAttempt
                  2 -> return UserLoginSuccess
                  3 -> return UserPasswordSetup
                  4 -> return UserPasswordSetupReq
                  5 -> return UserAccountCreated
                  6 -> return UserDetailsChange
                  7 -> return UserTOSAccept
                  s -> Left ConvertError {
                          convSourceValue = show s
                        , convSourceType = "Int"
                        , convDestType = "UserHistoryEventType"
                        , convErrorMessage = "Convertion error: value " 
                                             ++ show s ++ " not mapped"
                        } 

data AddUserHistory = AddUserHistory UserID UserHistoryEvent IPAddress MinutesTime (Maybe UserID) 
instance DBUpdate AddUserHistory (Maybe UserHistory) where
  dbUpdate (AddUserHistory user event ip time mpuser) = do
    uid <- UserHistoryID <$> getUniqueID tableUsersHistory
    wrapDB $ \conn -> do 
      _ <- run conn ("INSERT INTO users_history("
        ++ "  id"
        ++ ", user_id"
        ++ ", event_type"
        ++ ", event_data"
        ++ ", ip"
        ++ ", time"
        ++ ", system_version"
        ++ ", performing_user_id)"
        ++ " VALUES (?, ?, ?, ?, ?, ?, ?, ?)") $ [
            toSql uid
          , toSql user
          , toSql $ uheventtype event
          , toSql $ maybe "" encode $ uheventdata event
          , toSql ip
          , toSql $ time
          , toSql $ concat $ intersperse "." $ Ver.versionTags Paths.version
          , toSql mpuser
          ]
      return ()
    dbQuery $ GetUserHistoryByID uid

data GetUserHistoryByID = GetUserHistoryByID UserHistoryID
instance DBQuery GetUserHistoryByID (Maybe UserHistory) where
  dbQuery (GetUserHistoryByID uid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUserHistorySQL 
            ++ " WHERE id = ?"
    _  <- execute st [toSql uid]
    uh <- fetchUserHistory st
    oneObjectReturnedGuard uh

data GetUserHistoryByUserID = GetUserHistoryByUserID UserID
instance DBQuery GetUserHistoryByUserID (Maybe [UserHistory]) where
  dbQuery (GetUserHistoryByUserID uid) = wrapDB $ \conn -> do
    st <- prepare conn $ selectUserHistorySQL 
            ++ " WHERE user_id = ? ORDER BY time DESC "
    _  <- execute st [toSql uid]
    hist <- fetchUserHistory st
    return $ if null hist then Nothing else Just hist

data LogHistoryLoginAttempt = LogHistoryLoginAttempt UserID IPAddress MinutesTime
instance DBUpdate LogHistoryLoginAttempt (Maybe UserHistory) where
  dbUpdate (LogHistoryLoginAttempt userid ip time) = dbUpdate $ 
    AddUserHistory userid 
                   (UserHistoryEvent {uheventtype = UserLoginAttempt, uheventdata = Nothing})
                   ip
                   time
                   Nothing

data LogHistoryLoginSuccess = LogHistoryLoginSuccess UserID IPAddress MinutesTime
instance DBUpdate LogHistoryLoginSuccess (Maybe UserHistory) where
  dbUpdate (LogHistoryLoginSuccess userid ip time) = dbUpdate $ 
    AddUserHistory userid 
                   (UserHistoryEvent {uheventtype = UserLoginSuccess, uheventdata = Nothing})
                   ip
                   time
                   Nothing

data LogHistoryPasswordSetup = LogHistoryPasswordSetup UserID IPAddress MinutesTime (Maybe UserID)
instance DBUpdate LogHistoryPasswordSetup (Maybe UserHistory) where
  dbUpdate (LogHistoryPasswordSetup userid ip time mpuser) = dbUpdate $ 
    AddUserHistory userid 
                   (UserHistoryEvent {uheventtype = UserPasswordSetup, uheventdata = Nothing})
                   ip
                   time
                   mpuser

data LogHistoryPasswordSetupReq = LogHistoryPasswordSetupReq UserID IPAddress MinutesTime (Maybe UserID)
instance DBUpdate LogHistoryPasswordSetupReq (Maybe UserHistory) where
  dbUpdate (LogHistoryPasswordSetupReq userid ip time mpuser) = dbUpdate $ 
    AddUserHistory userid 
                   (UserHistoryEvent {uheventtype = UserPasswordSetupReq, uheventdata = Nothing})
                   ip
                   time
                   mpuser

data LogHistoryAccountCreated = LogHistoryAccountCreated UserID IPAddress MinutesTime Email (Maybe UserID)
instance DBUpdate LogHistoryAccountCreated (Maybe UserHistory) where
  dbUpdate (LogHistoryAccountCreated userid ip time email mpuser) = dbUpdate $ 
    AddUserHistory userid 
                   (UserHistoryEvent {
                       uheventtype = UserAccountCreated
                     , uheventdata = Just $ JSArray $ [JSObject . toJSObject $ [
                        ("field", JSString $ toJSString "email")
                      , ("oldval", JSString $ toJSString "")
                      , ("newval", JSString $ toJSString $ BS.unpack $ unEmail email)
                      ]]})
                   ip
                   time
                   mpuser

data LogHistoryTOSAccept = LogHistoryTOSAccept UserID IPAddress MinutesTime (Maybe UserID)
instance DBUpdate LogHistoryTOSAccept (Maybe UserHistory) where
  dbUpdate (LogHistoryTOSAccept userid ip time mpuser) = dbUpdate $ 
    AddUserHistory userid 
                   (UserHistoryEvent {
                       uheventtype = UserTOSAccept
                     , uheventdata = Nothing})
                   ip
                   time
                   mpuser

data LogHistoryDetailsChanged = LogHistoryDetailsChanged UserID IPAddress MinutesTime [(String, String, String)] (Maybe UserID)
instance DBUpdate LogHistoryDetailsChanged (Maybe UserHistory) where
  dbUpdate (LogHistoryDetailsChanged userid ip time details mpuser) = dbUpdate $ 
    AddUserHistory userid 
                   (UserHistoryEvent {
                       uheventtype = UserDetailsChange
                     , uheventdata = Just $ JSArray $ map (\(field, oldv, newv) -> 
                                        JSObject . toJSObject $ [
                                            ("field", JSString $ toJSString field)
                                          , ("oldval", JSString $ toJSString oldv)
                                          , ("newval", JSString $ toJSString newv)
                                          ]) details})
                   ip
                   time
                   mpuser

data LogHistoryUserInfoChanged = LogHistoryUserInfoChanged UserID IPAddress MinutesTime UserInfo UserInfo (Maybe UserID)
instance DBUpdate LogHistoryUserInfoChanged (Maybe UserHistory) where
  dbUpdate (LogHistoryUserInfoChanged userid ip time oldinfo newinfo mpuser) = do
    let diff = diffUserInfos oldinfo newinfo
    case diff of 
      [] -> return Nothing
      _  -> dbUpdate $ LogHistoryDetailsChanged userid ip time diff mpuser

diffUserInfos :: UserInfo -> UserInfo -> [(String, String, String)]
diffUserInfos old new = fstNameDiff 
  ++ sndNameDiff 
  ++ personalNumberDiff 
  ++ companyPositionDiff 
  ++ phoneDiff 
  ++ mobileDiff 
  ++ emailDiff
  where
    fstNameDiff = if (userfstname old) /= (userfstname new) 
                    then [("first_name", BS.unpack $ userfstname old, BS.unpack $ userfstname new)] 
                    else []
    sndNameDiff = if (usersndname old) /= (usersndname new) 
                    then [("last_name", BS.unpack $ usersndname old, BS.unpack $ usersndname new)] 
                    else []
    personalNumberDiff = if (userpersonalnumber old) /= (userpersonalnumber new) 
                            then [("personal_number", BS.unpack $ userpersonalnumber old, BS.unpack $ userpersonalnumber new)] 
                            else []
    companyPositionDiff = if (usercompanyposition old) /= (usercompanyposition new) 
                            then [("company_position", BS.unpack $ usercompanyposition old, BS.unpack $ usercompanyposition new)] 
                            else []
    phoneDiff = if (userphone old) /= (userphone new) 
                            then [("phone", BS.unpack $ userphone old, BS.unpack $ userphone new)] 
                            else []
    mobileDiff = if (usermobile old) /= (usermobile new) 
                            then [("mobile", BS.unpack $ usermobile old, BS.unpack $ usermobile new)] 
                            else []
    emailDiff = if (useremail old) /= (useremail new) 
                            then [("email", BS.unpack $ unEmail $ useremail old, BS.unpack $ unEmail $ useremail new)] 
                            else []

selectUserHistorySQL :: String
selectUserHistorySQL = "SELECT "
 ++ "  id"
 ++ ", user_id"
 ++ ", event_type"
 ++ ", event_data"
 ++ ", ip"
 ++ ", time"
 ++ ", system_version"
 ++ ", performing_user_id"
 ++ "  FROM users_history"
 ++ " "

fetchUserHistory :: Statement -> IO [UserHistory]
fetchUserHistory st = fetchValues st decodeRowAsUserHistory

decodeRowAsUserHistory :: UserHistoryID
                       -> UserID
                       -> UserHistoryEventType
                       -> Maybe BS.ByteString
                       -> IPAddress
                       -> MinutesTime
                       -> BS.ByteString
                       -> Maybe UserID
                       -> Either DBException UserHistory
decodeRowAsUserHistory uid userid eventtype meventdata ip time sysver mpuser = (
  Right $ UserHistory {
      uhid = uid
    , uhuserid = userid
    , uhevent = UserHistoryEvent { 
          uheventtype = eventtype
        , uheventdata = maybe Nothing (\d -> case decode $ BS.unpack d of
                                             Ok a -> Just a
                                             _    -> Nothing) meventdata
        }
    , uhip = ip
    , uhtime = time
    , uhsystemversion = sysver
    , uhperforminguserid = mpuser
  }):: Either DBException UserHistory
