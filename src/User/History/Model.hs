module User.History.Model (
    UserHistory(..)
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
  , LogHistoryPadLoginAttempt(..)
  , LogHistoryPadLoginSuccess(..)
  , GetUserHistoryByUserID(..)
  ) where

import Data.List (intersperse)
import Text.JSON

import qualified Paths_kontrakcja as Paths
import qualified Data.Version as Ver

import DB
import IPAddress
import MinutesTime
import Utils.Monoid

import User.History.Tables
import User.UserID
import User.Model

data UserHistory = UserHistory {
    uhuserid           :: UserID
  , uhevent            :: UserHistoryEvent
  , uhip               :: IPAddress
  , uhtime             :: MinutesTime
  , uhsystemversion    :: String
  , uhperforminguserid :: Maybe UserID -- Nothing means no user changed it (like the system)
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
                          | UserPadLoginAttempt
                          | UserPadLoginSuccess
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
  safeConvert UserPadLoginAttempt  = return . toSql $ (8 :: Int)
  safeConvert UserPadLoginSuccess  = return . toSql $ (9 :: Int)
instance Convertible SqlValue UserHistoryEventType where
  safeConvert a = case (fromSql a :: Int) of
    1 -> return UserLoginAttempt
    2 -> return UserLoginSuccess
    3 -> return UserPasswordSetup
    4 -> return UserPasswordSetupReq
    5 -> return UserAccountCreated
    6 -> return UserDetailsChange
    7 -> return UserTOSAccept
    8 -> return UserPadLoginAttempt
    9 -> return UserPadLoginSuccess
    n -> Left ConvertError {
        convSourceValue = show n
      , convSourceType = "Int"
      , convDestType = "UserHistoryEventType"
      , convErrorMessage = "Convertion error: value " ++ show n ++ " not mapped"
    }

data GetUserHistoryByUserID = GetUserHistoryByUserID UserID
instance MonadDB m => DBQuery m GetUserHistoryByUserID [UserHistory] where
  query (GetUserHistoryByUserID uid) = do
    _ <- kRun $ selectUserHistorySQL
      <++> SQL "WHERE user_id = ? ORDER BY time" [toSql uid]
    fetchUserHistory

data LogHistoryLoginAttempt = LogHistoryLoginAttempt UserID IPAddress MinutesTime
instance MonadDB m => DBUpdate m LogHistoryLoginAttempt Bool where
  update (LogHistoryLoginAttempt userid ip time) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserLoginAttempt, uheventdata = Nothing}
    ip
    time
    Nothing

data LogHistoryLoginSuccess = LogHistoryLoginSuccess UserID IPAddress MinutesTime
instance MonadDB m => DBUpdate m LogHistoryLoginSuccess Bool where
  update (LogHistoryLoginSuccess userid ip time) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserLoginSuccess, uheventdata = Nothing}
    ip
    time
    (Just userid)

data LogHistoryPadLoginAttempt = LogHistoryPadLoginAttempt UserID IPAddress MinutesTime
instance MonadDB m => DBUpdate m LogHistoryPadLoginAttempt Bool where
  update (LogHistoryPadLoginAttempt userid ip time) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserPadLoginAttempt, uheventdata = Nothing}
    ip
    time
    Nothing

data LogHistoryPadLoginSuccess = LogHistoryPadLoginSuccess UserID IPAddress MinutesTime
instance MonadDB m => DBUpdate m LogHistoryPadLoginSuccess Bool where
  update (LogHistoryPadLoginSuccess userid ip time) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserPadLoginSuccess, uheventdata = Nothing}
    ip
    time
    (Just userid)

    
data LogHistoryPasswordSetup = LogHistoryPasswordSetup UserID IPAddress MinutesTime (Maybe UserID)
instance MonadDB m => DBUpdate m LogHistoryPasswordSetup Bool where
  update (LogHistoryPasswordSetup userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserPasswordSetup, uheventdata = Nothing}
    ip
    time
    mpuser

data LogHistoryPasswordSetupReq = LogHistoryPasswordSetupReq UserID IPAddress MinutesTime (Maybe UserID)
instance MonadDB m => DBUpdate m LogHistoryPasswordSetupReq Bool where
  update (LogHistoryPasswordSetupReq userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserPasswordSetupReq, uheventdata = Nothing}
    ip
    time
    mpuser

data LogHistoryAccountCreated = LogHistoryAccountCreated UserID IPAddress MinutesTime Email (Maybe UserID)
instance MonadDB m => DBUpdate m LogHistoryAccountCreated Bool where
  update (LogHistoryAccountCreated userid ip time email mpuser) = addUserHistory
    userid
    UserHistoryEvent {
        uheventtype = UserAccountCreated
      , uheventdata = Just $ JSArray $ [JSObject . toJSObject $ [
          ("field", JSString $ toJSString "email")
        , ("oldval", JSString $ toJSString "")
        , ("newval", JSString $ toJSString $ unEmail email)
        ]]
      }
    ip
    time
    mpuser

data LogHistoryTOSAccept = LogHistoryTOSAccept UserID IPAddress MinutesTime (Maybe UserID)
instance MonadDB m => DBUpdate m LogHistoryTOSAccept Bool where
  update (LogHistoryTOSAccept userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserTOSAccept, uheventdata = Nothing}
    ip
    time
    mpuser

data LogHistoryDetailsChanged = LogHistoryDetailsChanged UserID IPAddress MinutesTime [(String, String, String)] (Maybe UserID)
instance MonadDB m => DBUpdate m LogHistoryDetailsChanged Bool where
  update (LogHistoryDetailsChanged userid ip time details mpuser) = addUserHistory
    userid
    UserHistoryEvent {
        uheventtype = UserDetailsChange
      , uheventdata = Just $ JSArray $ map (\(field, oldv, newv) ->
        JSObject . toJSObject $ [
            ("field", JSString $ toJSString field)
          , ("oldval", JSString $ toJSString oldv)
          , ("newval", JSString $ toJSString newv)
          ]) details
      }
    ip
    time
    mpuser

data LogHistoryUserInfoChanged = LogHistoryUserInfoChanged UserID IPAddress MinutesTime UserInfo UserInfo (Maybe UserID)
instance MonadDB m => DBUpdate m LogHistoryUserInfoChanged Bool where
  update (LogHistoryUserInfoChanged userid ip time oldinfo newinfo mpuser) = do
    let diff = diffUserInfos oldinfo newinfo
    case diff of
      [] -> return False
      _  -> update $ LogHistoryDetailsChanged userid ip time diff mpuser

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
      then [("first_name", userfstname old, userfstname new)]
      else []
    sndNameDiff = if (usersndname old) /= (usersndname new) 
      then [("last_name", usersndname old, usersndname new)]
      else []
    personalNumberDiff = if (userpersonalnumber old) /= (userpersonalnumber new) 
      then [("personal_number", userpersonalnumber old, userpersonalnumber new)]
      else []
    companyPositionDiff = if (usercompanyposition old) /= (usercompanyposition new) 
      then [("company_position", usercompanyposition old, usercompanyposition new)]
      else []
    phoneDiff = if (userphone old) /= (userphone new) 
      then [("phone", userphone old, userphone new)]
      else []
    mobileDiff = if (usermobile old) /= (usermobile new) 
      then [("mobile", usermobile old, usermobile new)]
      else []
    emailDiff = if (useremail old) /= (useremail new) 
      then [("email", unEmail $ useremail old, unEmail $ useremail new)]
      else []

addUserHistory :: MonadDB m => UserID -> UserHistoryEvent -> IPAddress -> MinutesTime -> Maybe UserID -> DBEnv m Bool
addUserHistory user event ip time mpuser =
  kRun01 $ mkSQL INSERT tableUsersHistory [
      sql "user_id" user
    , sql "event_type" $ uheventtype event
    , sql "event_data" $ maybe "" encode $ uheventdata event
    , sql "ip" ip
    , sql "time" time
    , sql "system_version" $ concat $ intersperse "." $ Ver.versionTags Paths.version
    , sql "performing_user_id" mpuser
    ]

selectUserHistorySQL :: SQL
selectUserHistorySQL = SQL ("SELECT"
 ++ "  user_id"
 ++ ", event_type"
 ++ ", event_data"
 ++ ", ip"
 ++ ", time"
 ++ ", system_version"
 ++ ", performing_user_id"
 ++ "  FROM users_history"
 ++ " ") []

fetchUserHistory :: MonadDB m => DBEnv m [UserHistory]
fetchUserHistory = foldDB decoder []
  where
    decoder acc userid eventtype meventdata ip time sysver mpuser = UserHistory {
        uhuserid = userid
      , uhevent = UserHistoryEvent {
          uheventtype = eventtype
        , uheventdata = maybe Nothing (\d -> case decode d of
                                        Ok a -> Just a
                                        _    -> Nothing) meventdata
        }
      , uhip = ip
      , uhtime = time
      , uhsystemversion = sysver
      , uhperforminguserid = mpuser
      } : acc
