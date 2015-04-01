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

import Control.Monad.Catch
import Data.Int
import Text.JSON
import Text.JSON.Gen

import DB
import IPAddress
import KontraPrelude
import MinutesTime
import User.Email
import User.Model
import Utils.Prelude
import qualified Version

data UserHistory = UserHistory {
    uhuserid           :: UserID
  , uhevent            :: UserHistoryEvent
  , uhip               :: IPAddress
  , uhtime             :: UTCTime
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
instance PQFormat UserHistoryEventType where
  pqFormat = const $ pqFormat ($undefined::Int32)

instance FromSQL UserHistoryEventType where
  type PQBase UserHistoryEventType = PQBase Int32
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int32 of
      1 -> return UserLoginAttempt
      2 -> return UserLoginSuccess
      3 -> return UserPasswordSetup
      4 -> return UserPasswordSetupReq
      5 -> return UserAccountCreated
      6 -> return UserDetailsChange
      7 -> return UserTOSAccept
      8 -> return UserPadLoginAttempt
      9 -> return UserPadLoginSuccess
      _ -> throwM RangeError {
        reRange = [(1, 9)]
      , reValue = n
      }

instance ToSQL UserHistoryEventType where
  type PQDest UserHistoryEventType = PQDest Int32
  toSQL UserLoginAttempt     = toSQL (1::Int32)
  toSQL UserLoginSuccess     = toSQL (2::Int32)
  toSQL UserPasswordSetup    = toSQL (3::Int32)
  toSQL UserPasswordSetupReq = toSQL (4::Int32)
  toSQL UserAccountCreated   = toSQL (5::Int32)
  toSQL UserDetailsChange    = toSQL (6::Int32)
  toSQL UserTOSAccept        = toSQL (7::Int32)
  toSQL UserPadLoginAttempt  = toSQL (8::Int32)
  toSQL UserPadLoginSuccess  = toSQL (9::Int32)

data GetUserHistoryByUserID = GetUserHistoryByUserID UserID
instance MonadDB m => DBQuery m GetUserHistoryByUserID [UserHistory] where
  query (GetUserHistoryByUserID uid) = do
    runQuery_ $ selectUserHistorySQL
      <+> "WHERE user_id =" <?> uid <+> "ORDER BY time"
    fetchMany fetchUserHistory

data LogHistoryLoginAttempt = LogHistoryLoginAttempt UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryLoginAttempt Bool where
  update (LogHistoryLoginAttempt userid ip time) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserLoginAttempt, uheventdata = Nothing}
    ip
    time
    Nothing

data LogHistoryLoginSuccess = LogHistoryLoginSuccess UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryLoginSuccess Bool where
  update (LogHistoryLoginSuccess userid ip time) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserLoginSuccess, uheventdata = Nothing}
    ip
    time
    (Just userid)

data LogHistoryPadLoginAttempt = LogHistoryPadLoginAttempt UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryPadLoginAttempt Bool where
  update (LogHistoryPadLoginAttempt userid ip time) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserPadLoginAttempt, uheventdata = Nothing}
    ip
    time
    Nothing

data LogHistoryPadLoginSuccess = LogHistoryPadLoginSuccess UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryPadLoginSuccess Bool where
  update (LogHistoryPadLoginSuccess userid ip time) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserPadLoginSuccess, uheventdata = Nothing}
    ip
    time
    (Just userid)


data LogHistoryPasswordSetup = LogHistoryPasswordSetup UserID IPAddress UTCTime (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryPasswordSetup Bool where
  update (LogHistoryPasswordSetup userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserPasswordSetup, uheventdata = Nothing}
    ip
    time
    mpuser

data LogHistoryPasswordSetupReq = LogHistoryPasswordSetupReq UserID IPAddress UTCTime (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryPasswordSetupReq Bool where
  update (LogHistoryPasswordSetupReq userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserPasswordSetupReq, uheventdata = Nothing}
    ip
    time
    mpuser

data LogHistoryAccountCreated = LogHistoryAccountCreated UserID IPAddress UTCTime Email (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryAccountCreated Bool where
  update (LogHistoryAccountCreated userid ip time email mpuser) = addUserHistory
    userid
    UserHistoryEvent {
        uheventtype = UserAccountCreated
      , uheventdata = Just $ JSArray $ [runJSONGen $ do
          value "field" ("email" :: String)
          value "oldval" ("" :: String)
          value "newval" $ unEmail email
        ]
      }
    ip
    time
    mpuser

data LogHistoryTOSAccept = LogHistoryTOSAccept UserID IPAddress UTCTime (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryTOSAccept Bool where
  update (LogHistoryTOSAccept userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent {uheventtype = UserTOSAccept, uheventdata = Nothing}
    ip
    time
    mpuser

data LogHistoryDetailsChanged = LogHistoryDetailsChanged UserID IPAddress UTCTime [(String, String, String)] (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryDetailsChanged Bool where
  update (LogHistoryDetailsChanged userid ip time details mpuser) = addUserHistory
    userid
    UserHistoryEvent {
        uheventtype = UserDetailsChange
      , uheventdata = Just $ JSArray $ for details $ \(field, oldv, newv) -> runJSONGen $ do
          value "field" field
          value "oldval" oldv
          value "newval" newv
      }
    ip
    time
    mpuser

data LogHistoryUserInfoChanged = LogHistoryUserInfoChanged UserID IPAddress UTCTime UserInfo UserInfo (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryUserInfoChanged Bool where
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
    emailDiff = if (useremail old) /= (useremail new)
      then [("email", unEmail $ useremail old, unEmail $ useremail new)]
      else []

addUserHistory :: (MonadDB m, MonadThrow m) => UserID -> UserHistoryEvent -> IPAddress -> UTCTime -> Maybe UserID -> m Bool
addUserHistory user event ip time mpuser =
  runQuery01 $ sqlInsert "users_history" $ do
    sqlSet "user_id" user
    sqlSet "event_type" $ uheventtype event
    sqlSet "event_data" $ maybe "" encode $ uheventdata event
    sqlSet "ip" ip
    sqlSet "time" time
    sqlSet "system_version" $ Version.versionID
    sqlSet "performing_user_id" mpuser

selectUserHistorySQL :: SQL
selectUserHistorySQL = "SELECT"
  <> "  user_id"
  <> ", event_type"
  <> ", event_data"
  <> ", ip"
  <> ", time"
  <> ", system_version"
  <> ", performing_user_id"
  <> "  FROM users_history"

fetchUserHistory :: (UserID, UserHistoryEventType, Maybe String, IPAddress, UTCTime, String, Maybe UserID) -> UserHistory
fetchUserHistory (userid, eventtype, meventdata, ip, time, sysver, mpuser) = UserHistory {
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
}
