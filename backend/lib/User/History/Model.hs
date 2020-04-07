module User.History.Model (
    UserHistory(..)
  , UserHistoryEvent(..)
  , UserHistoryEventType(..)
  , LogHistoryLoginFailure(..)
  , LogHistoryLoginTOTPFailure(..)
  , LogHistoryLoginSuccess(..)
  , LogHistoryPasswordSetup(..)
  , LogHistoryPasswordSetupReq(..)
  , LogHistoryTOTPEnable(..)
  , LogHistoryTOTPDisable(..)
  , LogHistoryAccountCreated(..)
  , LogHistoryAccountDeleted(..)
  , LogHistoryTOSAccept(..)
  , LogHistoryDetailsChanged(..)
  , LogHistoryUserInfoChanged(..)
  , LogHistoryPadLoginFailure(..)
  , LogHistoryPadLoginSuccess(..)
  , LogHistoryAPIGetPersonalTokenFailure(..)
  , LogHistoryAPIGetPersonalTokenSuccess(..)
  , GetUserHistoryByUserID(..)
  , GetUserRecentAuthFailureCount(..)
  ) where

import Control.Monad.Catch
import Control.Monad.Time
import Data.Int
import Data.Time.Clock
import Text.JSON
import Text.JSON.Gen
import qualified Data.Text as T

import DB
import IPAddress
import User.Email
import User.Types.User
import User.UserID
import qualified VersionTH

data UserHistory = UserHistory {
    uhuserid           :: UserID
  , uhevent            :: UserHistoryEvent
  , uhip               :: IPAddress
  , uhtime             :: UTCTime
  , uhsystemversion    :: Text
  , uhperforminguserid :: Maybe UserID -- Nothing means no user changed it (like the system)
  }
  deriving (Eq, Show)

data UserHistoryEvent = UserHistoryEvent {
    uheventtype :: UserHistoryEventType
  , uheventdata :: Maybe JSValue
  }
  deriving (Eq, Show)

data UserHistoryEventType = UserLoginFailure
                          | UserLoginTOTPFailure
                          | UserLoginSuccess
                          | UserPasswordSetup
                          | UserPasswordSetupReq
                          | UserTOTPEnable
                          | UserTOTPDisable
                          | UserAccountCreated
                          | UserDetailsChange
                          | UserTOSAccept
                          | UserPadLoginFailure
                          | UserPadLoginSuccess
                          | UserAPIGetPersonalTokenFailure
                          | UserAPIGetPersonalTokenSuccess
                          | UserAccountDeleted
  deriving (Eq, Show)

{- |
  UserPasswordSetup is a successful change but UserPasswordSetupReq is
  only a request, not successful change.
 -}
instance PQFormat UserHistoryEventType where
  pqFormat = pqFormat @Int32

instance FromSQL UserHistoryEventType where
  type PQBase UserHistoryEventType = PQBase Int32
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int32 of
      1  -> return UserLoginFailure
      2  -> return UserLoginSuccess
      3  -> return UserPasswordSetup
      4  -> return UserPasswordSetupReq
      5  -> return UserAccountCreated
      6  -> return UserDetailsChange
      7  -> return UserTOSAccept
      8  -> return UserPadLoginFailure
      9  -> return UserPadLoginSuccess
      10 -> return UserAPIGetPersonalTokenFailure
      11 -> return UserAPIGetPersonalTokenSuccess
      12 -> return UserLoginTOTPFailure
      13 -> return UserTOTPEnable
      14 -> return UserTOTPDisable
      15 -> return UserAccountDeleted
      _  -> throwM RangeError { reRange = [(1, 15)], reValue = n }

instance ToSQL UserHistoryEventType where
  type PQDest UserHistoryEventType = PQDest Int32
  toSQL UserLoginFailure               = toSQL (1 :: Int32)
  toSQL UserLoginSuccess               = toSQL (2 :: Int32)
  toSQL UserPasswordSetup              = toSQL (3 :: Int32)
  toSQL UserPasswordSetupReq           = toSQL (4 :: Int32)
  toSQL UserAccountCreated             = toSQL (5 :: Int32)
  toSQL UserDetailsChange              = toSQL (6 :: Int32)
  toSQL UserTOSAccept                  = toSQL (7 :: Int32)
  toSQL UserPadLoginFailure            = toSQL (8 :: Int32)
  toSQL UserPadLoginSuccess            = toSQL (9 :: Int32)
  toSQL UserAPIGetPersonalTokenFailure = toSQL (10 :: Int32)
  toSQL UserAPIGetPersonalTokenSuccess = toSQL (11 :: Int32)
  toSQL UserLoginTOTPFailure           = toSQL (12 :: Int32)
  toSQL UserTOTPEnable                 = toSQL (13 :: Int32)
  toSQL UserTOTPDisable                = toSQL (14 :: Int32)
  toSQL UserAccountDeleted             = toSQL (15 :: Int32)

newtype GetUserHistoryByUserID = GetUserHistoryByUserID UserID
instance MonadDB m => DBQuery m GetUserHistoryByUserID [UserHistory] where
  dbQuery (GetUserHistoryByUserID uid) = do
    runQuery_ $ selectUserHistorySQL <+> "WHERE user_id =" <?> uid <+> "ORDER BY time"
    fetchMany fetchUserHistory

newtype GetUserRecentAuthFailureCount = GetUserRecentAuthFailureCount UserID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBQuery m GetUserRecentAuthFailureCount Int64 where
  dbQuery (GetUserRecentAuthFailureCount uid) = do
    now <- currentTime
    runQuery_ . sqlSelect "users_history" $ do
      sqlWhereEq "user_id" uid
      sqlWhereIn
        "event_type"
        [ UserLoginFailure
        , UserLoginTOTPFailure
        , UserPadLoginFailure
        , UserAPIGetPersonalTokenFailure
        ]
      sqlWhere $ "time >= (" <?> now <+> "- interval '10 minutes')"
      sqlWhere $ "time >= (" <> toSQLCommand passResetTimeQuery <> ")"
      sqlResult "COUNT(*)"
    fetchOne runIdentity
    where
      passResetTimeQuery = sqlSelect "users_history" $ do
        sqlWhereEq "user_id"    uid
        sqlWhereEq "event_type" UserPasswordSetup
        sqlResult "coalesce(max(time), to_timestamp(0))"

data LogHistoryLoginFailure = LogHistoryLoginFailure UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryLoginFailure Bool where
  dbUpdate (LogHistoryLoginFailure userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserLoginFailure, uheventdata = Nothing }
    ip
    time
    Nothing

data LogHistoryLoginTOTPFailure = LogHistoryLoginTOTPFailure UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryLoginTOTPFailure Bool where
  dbUpdate (LogHistoryLoginTOTPFailure userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserLoginTOTPFailure, uheventdata = Nothing }
    ip
    time
    Nothing

data LogHistoryLoginSuccess = LogHistoryLoginSuccess UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryLoginSuccess Bool where
  dbUpdate (LogHistoryLoginSuccess userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserLoginSuccess, uheventdata = Nothing }
    ip
    time
    (Just userid)

data LogHistoryPadLoginFailure = LogHistoryPadLoginFailure UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryPadLoginFailure Bool where
  dbUpdate (LogHistoryPadLoginFailure userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserPadLoginFailure, uheventdata = Nothing }
    ip
    time
    Nothing

data LogHistoryPadLoginSuccess = LogHistoryPadLoginSuccess UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryPadLoginSuccess Bool where
  dbUpdate (LogHistoryPadLoginSuccess userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserPadLoginSuccess, uheventdata = Nothing }
    ip
    time
    (Just userid)

data LogHistoryAPIGetPersonalTokenFailure = LogHistoryAPIGetPersonalTokenFailure UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryAPIGetPersonalTokenFailure Bool where
  dbUpdate (LogHistoryAPIGetPersonalTokenFailure userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserAPIGetPersonalTokenFailure
                     , uheventdata = Nothing
                     }
    ip
    time
    Nothing

data LogHistoryAPIGetPersonalTokenSuccess = LogHistoryAPIGetPersonalTokenSuccess UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryAPIGetPersonalTokenSuccess Bool where
  dbUpdate (LogHistoryAPIGetPersonalTokenSuccess userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserAPIGetPersonalTokenSuccess
                     , uheventdata = Nothing
                     }
    ip
    time
    (Just userid)


data LogHistoryPasswordSetup = LogHistoryPasswordSetup UserID IPAddress UTCTime (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryPasswordSetup Bool where
  dbUpdate (LogHistoryPasswordSetup userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserPasswordSetup, uheventdata = Nothing }
    ip
    time
    mpuser

data LogHistoryPasswordSetupReq = LogHistoryPasswordSetupReq UserID IPAddress UTCTime (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryPasswordSetupReq Bool where
  dbUpdate (LogHistoryPasswordSetupReq userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserPasswordSetupReq, uheventdata = Nothing }
    ip
    time
    mpuser

data LogHistoryTOTPEnable = LogHistoryTOTPEnable UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryTOTPEnable Bool where
  dbUpdate (LogHistoryTOTPEnable userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserTOTPEnable, uheventdata = Nothing }
    ip
    time
    Nothing

data LogHistoryTOTPDisable = LogHistoryTOTPDisable UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryTOTPDisable Bool where
  dbUpdate (LogHistoryTOTPDisable userid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserTOTPDisable, uheventdata = Nothing }
    ip
    time
    Nothing

data LogHistoryAccountCreated = LogHistoryAccountCreated UserID IPAddress UTCTime Email (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryAccountCreated Bool where
  dbUpdate (LogHistoryAccountCreated userid ip time email mpuser) = addUserHistory
    userid
    UserHistoryEvent
      { uheventtype = UserAccountCreated
      , uheventdata = Just $ JSArray
                        [ runJSONGen $ do
                            value "field"  ("email" :: String)
                            value "oldval" ("" :: String)
                            value "newval" $ unEmail email
                        ]
      }
    ip
    time
    mpuser

data LogHistoryTOSAccept = LogHistoryTOSAccept UserID IPAddress UTCTime (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryTOSAccept Bool where
  dbUpdate (LogHistoryTOSAccept userid ip time mpuser) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserTOSAccept, uheventdata = Nothing }
    ip
    time
    mpuser

data LogHistoryDetailsChanged = LogHistoryDetailsChanged
  UserID
  IPAddress
  UTCTime
  [(Text, Text, Text)]
  (Maybe UserID)

instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryDetailsChanged Bool where
  dbUpdate (LogHistoryDetailsChanged userid ip time details mpuser) = addUserHistory
    userid
    UserHistoryEvent
      { uheventtype = UserDetailsChange
      , uheventdata = Just . JSArray $ for
                        details
                        (\(field, oldv, newv) -> runJSONGen $ do
                          value "field"  field
                          value "oldval" oldv
                          value "newval" newv
                        )
      }
    ip
    time
    mpuser

data LogHistoryUserInfoChanged = LogHistoryUserInfoChanged UserID IPAddress UTCTime UserInfo UserInfo (Maybe UserID)
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryUserInfoChanged Bool where
  dbUpdate (LogHistoryUserInfoChanged userid ip time oldinfo newinfo mpuser) = do
    let diff = diffUserInfos oldinfo newinfo
    case diff of
      [] -> return False
      _  -> dbUpdate $ LogHistoryDetailsChanged userid ip time diff mpuser

data LogHistoryAccountDeleted = LogHistoryAccountDeleted UserID UserID IPAddress UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m LogHistoryAccountDeleted Bool where
  dbUpdate (LogHistoryAccountDeleted userid deletinguserid ip time) = addUserHistory
    userid
    UserHistoryEvent { uheventtype = UserAccountDeleted, uheventdata = Nothing }
    ip
    time
    (Just deletinguserid)

diffUserInfos :: UserInfo -> UserInfo -> [(Text, Text, Text)]
diffUserInfos old new =
  fstNameDiff
    ++ sndNameDiff
    ++ personalNumberDiff
    ++ companyPositionDiff
    ++ phoneDiff
    ++ emailDiff
  where
    fstNameDiff = if (old ^. #firstName) /= (new ^. #firstName)
      then [("first_name", old ^. #firstName, new ^. #firstName)]
      else []
    sndNameDiff = if (old ^. #lastName) /= (new ^. #lastName)
      then [("last_name", old ^. #lastName, new ^. #lastName)]
      else []
    personalNumberDiff = if (old ^. #personalNumber) /= (new ^. #personalNumber)
      then [("personal_number", old ^. #personalNumber, new ^. #personalNumber)]
      else []
    companyPositionDiff = if (old ^. #companyPosition) /= (new ^. #companyPosition)
      then [("company_position", old ^. #companyPosition, new ^. #companyPosition)]
      else []
    phoneDiff = if (old ^. #phone) /= (new ^. #phone)
      then [("phone", old ^. #phone, new ^. #phone)]
      else []
    emailDiff = if (old ^. #email) /= (new ^. #email)
      then [("email", unEmail $ old ^. #email, unEmail $ new ^. #email)]
      else []

addUserHistory
  :: (MonadDB m, MonadThrow m)
  => UserID
  -> UserHistoryEvent
  -> IPAddress
  -> UTCTime
  -> Maybe UserID
  -> m Bool
addUserHistory user event ip time mpuser = runQuery01 . sqlInsert "users_history" $ do
  sqlSet "user_id" user
  sqlSet "event_type" $ uheventtype event
  sqlSet "event_data" . maybe "" encode $ uheventdata event
  sqlSet "ip"                 ip
  sqlSet "time"               time
  sqlSet "system_version"     VersionTH.versionID
  sqlSet "performing_user_id" mpuser

selectUserHistorySQL :: SQL
selectUserHistorySQL =
  "SELECT"
    <> "  user_id"
    <> ", event_type"
    <> ", event_data"
    <> ", ip"
    <> ", time"
    <> ", system_version"
    <> ", performing_user_id"
    <> "  FROM users_history"

fetchUserHistory
  :: (UserID, UserHistoryEventType, Maybe Text, IPAddress, UTCTime, Text, Maybe UserID)
  -> UserHistory
fetchUserHistory (userid, eventtype, meventdata, ip, time, sysver, mpuser) = UserHistory
  { uhuserid           = userid
  , uhevent            = UserHistoryEvent
                           { uheventtype = eventtype
                           , uheventdata = (\d -> case decode $ T.unpack d of
                                             Ok a -> Just a
                                             _    -> Nothing
                                           )
                                             =<< meventdata
                           }
  , uhip               = ip
  , uhtime             = time
  , uhsystemversion    = sysver
  , uhperforminguserid = mpuser
  }
