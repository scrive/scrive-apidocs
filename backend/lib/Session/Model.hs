module Session.Model (
    getNonTempSessionID
  , getCurrentSession
  , updateSession
  , getUserFromSession
  , getPadUserFromSession
  , getSession
  , startNewSession
  , terminateAllUserSessionsExceptCurrent
  , TerminateAllButOneUserSessions(..)
  , DeleteExpiredSessions(..)
  , PurgeExpiredTemporaryLoginTokens(..)
  , NewTemporaryLoginToken(..)
  -- Exported for tests
  , GetSession(..)
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Crypto.RNG
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime, nominalDay)
import Happstack.Server hiding (Session)
import Log

import Context
import DB
import KontraError
import KontraMonad
import Log.Identifier
import MagicHash
import MinutesTime
import Session.Cookies
import Session.SessionID as SessionID
import Session.Types
import User.Model
import Utils.HTTP

-- | Get a non-temporary session ID from Context. If the current
-- session ID is temporary, insert a new empty session into the
-- database and return its ID (needed when the document ticket/eleg
-- transaction needs to be inserted into the database, but current
-- session is temporary), also modifying Context to carry modified ID.
getNonTempSessionID :: ( CryptoRNG m, KontraMonad m, MonadDB m
                       , MonadThrow m, MonadTime m, ServerMonad m )
                    => m SessionID
getNonTempSessionID = do
  sid <- get ctxsessionid <$> getContext
  if sid == SessionID.tempSessionID
    then do
      new_sid <- sesID <$> insertEmptySession
      modifyContext $ set ctxsessionid new_sid
      return new_sid
    else return sid
  where
    insertEmptySession = do
      sesToken     <- random
      sesCSRFToken <- random
      sesExpires   <- sessionNowModifier <$> currentTime
      sesDomain    <- currentDomain

      update . CreateSession $ Session
        { sesID        = SessionID.tempSessionID
        , sesUserID    = Nothing
        , sesPadUserID = Nothing
        , ..
        }

-- | Get the current session based on cookies set.
-- If no session is available, return a new empty session.

-- IE 10 is sending cookies for both domain and subdomain (scrive.com
-- & nj.scrive.com) We need to read them both, since we have no idea
-- which is the right one.
getCurrentSession :: ( CryptoRNG m, MonadDB m, MonadThrow m
                     , ServerMonad m, MonadLog m, FilterMonad Response m, MonadIO m)
                  => m Session
getCurrentSession = do
  bxt <- isXTokenCookieBroken
  if bxt
    then do
      emptySession
    else do
      cookieSessions <- currentSessionInfoCookies
      getSessionFromCookies cookieSessions
  where
    getSessionFromCookies (cs:css) = do
      domain <- currentDomain
      mses   <- getSession (cookieSessionID cs) (cookieSessionToken cs) domain
      case mses of
        Just ses -> return ses
        Nothing  -> getSessionFromCookies css
    getSessionFromCookies [] = emptySession

startNewSession :: ( FilterMonad Response m, ServerMonad m
                   , MonadDB m, MonadLog m, MonadThrow m
                   , MonadIO m, MonadBase IO m )
                => Session -> Maybe UserID -> Maybe UserID -> m Session
startNewSession _       Nothing Nothing    = internalError
startNewSession session mnewuid mnewpaduid = do
  let uid = fromJust $ (mnewuid `mplus` mnewpaduid)
  deleteSuperfluousUserSessions uid
  expires <- sessionNowModifier <$> currentTime
  dbUpdate . CreateSession $ session {
      sesExpires   = expires
    , sesUserID    = mnewuid
    , sesPadUserID = mnewpaduid
    }

updateSession :: ( FilterMonad Response m, ServerMonad m
                 , MonadDB m, MonadLog m, MonadThrow m
                 , MonadIO m, MonadBase IO m )
              => Session -> SessionID -> (Maybe UserID) -> (Maybe UserID)
              -> m ()
updateSession old_ses new_ses_id new_muser new_mpad_user = do
  case new_ses_id == SessionID.tempSessionID of
    -- We have no session and we want to log in some user
    True | (isJust new_muser || isJust new_mpad_user) -> do
      startNewSession old_ses new_muser new_mpad_user >>= startSessionCookie
    -- We have no session and we don't want to log in some user
    -- We do however remove cookies for expired session etc.
    True -> fixSessionCookiesIfBrokenOrSessionExpired
    -- We are updating existing session
    False | sesID old_ses == new_ses_id -> do
      when (sesUserID old_ses /= new_muser
            || sesPadUserID old_ses /= new_mpad_user) $ do
        success <- dbUpdate . UpdateSession $
          old_ses { sesUserID = new_muser, sesPadUserID = new_mpad_user}
        unless success $
          logInfo_ $ "UpdateSession didn't update session\
                     \ when it should have (existing session)"
        -- We are logging out, remove session cookie
        when (new_muser == Nothing && new_mpad_user == Nothing) $
          stopSessionCookie
    -- We got new session - now we need to generate a cookie for
    -- it. We also update it with users logged in during this
    -- handler.
    False | sesID old_ses /= new_ses_id -> do
      mses <- dbQuery $ GetSession new_ses_id
      case mses of
        Nothing   ->
          logInfo_ "updateSession failed while trying to switch session"
        Just sess -> do
          let new_sess = sess { sesUserID = new_muser
                              , sesPadUserID = new_mpad_user }
          success <- dbUpdate . UpdateSession $ new_sess
          startSessionCookie new_sess
          unless success $
            logInfo_
            "UpdateSession didn't update session when it should have had"
    _ -> return ()

getUserFromSession :: (MonadDB m, MonadThrow m) => Session -> m (Maybe User)
getUserFromSession Session{sesUserID} = case sesUserID of
  Just uid -> dbQuery $ GetUserByID uid
  Nothing  -> return Nothing

getPadUserFromSession :: (MonadDB m, MonadThrow m) => Session -> m (Maybe User)
getPadUserFromSession Session{sesPadUserID} = case sesPadUserID of
  Just uid -> dbQuery $ GetUserByID uid
  Nothing  -> return Nothing

sessionNowModifier :: UTCTime -> UTCTime
sessionNowModifier = (720 `minutesAfter`)

sessionExpirationDelay :: NominalDiffTime
sessionExpirationDelay = 2 * (nominalDay / 24) -- 2 hours

getSession :: (MonadDB m, MonadThrow m, MonadTime m)
           => SessionID -> MagicHash -> Text -> m (Maybe Session)
getSession sid token domain = runMaybeT $ do
  Just ses@Session{..} <- dbQuery $ GetSession sid
  guard $ sesToken == token
  guard $ sesDomain == domain
  now <- currentTime
  -- Updating 'expires' on every access is costly and results in
  -- quite a lot of database races for a single row in database, at
  -- least for user sessions.
  -- So we decided to update 'expires' only when at least 10% of the
  -- sessionExpirationDelay is consumed
  case diffUTCTime sesExpires now < 0.9 * sessionExpirationDelay of
    True -> do
      let sesextended = ses {
            sesExpires = sessionExpirationDelay `addUTCTime` now
            }
      void . dbUpdate $ UpdateSession sesextended
      return sesextended
    False ->
      return ses

-- If there is session-id cookie but no xtoken cookie or session has expired
-- but we still get cookie, then lets just clean cookies.

fixSessionCookiesIfBrokenOrSessionExpired ::
  ( MonadDB m, MonadThrow m, ServerMonad m, MonadLog m
  , FilterMonad Response m, MonadIO m)
  => m ()
fixSessionCookiesIfBrokenOrSessionExpired = do
  brokenXTokenCookie <- isXTokenCookieBroken
  cookieSessions <- currentSessionInfoCookies
  let someSessionCookieExists = not $ null cookieSessions
  allSessionsExpiredOrDropped  <- and <$> forM cookieSessions (\cs -> isExpiredOrDroppedSession (cookieSessionID cs))
  when (brokenXTokenCookie || (someSessionCookieExists && allSessionsExpiredOrDropped)) $ do
    stopSessionCookie
  where
    isExpiredOrDroppedSession :: (MonadDB m, MonadThrow m, MonadTime m)
              => SessionID -> m Bool
    isExpiredOrDroppedSession sid = isNothing <$> dbQuery (GetSession sid)

terminateAllUserSessionsExceptCurrent ::
  ( CryptoRNG m, MonadDB m, MonadThrow m, ServerMonad m
  , MonadLog m , FilterMonad Response m, MonadIO m)
  => UserID -> m ()
terminateAllUserSessionsExceptCurrent uid = do
  cs <- getCurrentSession
  dbUpdate $ TerminateAllButOneUserSessions uid (sesID cs)

selectSessionSelectorsList :: [SQL]
selectSessionSelectorsList = [ "id", "user_id", "pad_user_id"
                             , "expires", "token", "csrf_token", "domain" ]

data DeleteExpiredSessions = DeleteExpiredSessions
instance (MonadDB m, MonadThrow m, MonadTime m) =>
  DBUpdate m DeleteExpiredSessions () where
  update DeleteExpiredSessions = do
    now <- currentTime
    runQuery_ . sqlDelete "sessions" $
      sqlWhere $ "expires <" <?> now

data TerminateAllButOneUserSessions = TerminateAllButOneUserSessions UserID SessionID
instance (MonadDB m, MonadThrow m, MonadTime m) =>
  DBUpdate m TerminateAllButOneUserSessions () where
  update (TerminateAllButOneUserSessions uid sid) = do
    runQuery_ . sqlDelete "sessions" $ do
      sqlWhereAny [
          sqlWhere $ "user_id =" <?> uid
        , sqlWhere $ "pad_user_id =" <?> uid
        ]
      sqlWhere $ "id <>" <?> sid

data GetSession = GetSession SessionID
instance (MonadDB m, MonadThrow m, MonadTime m) =>
  DBQuery m GetSession (Maybe Session) where
  query (GetSession sid) = do
    now <- currentTime
    runQuery_ $ sqlSelect "sessions" $ do
      mapM_ sqlResult selectSessionSelectorsList
      sqlWhereEq "id" sid
      sqlWhere $ "expires >=" <?> now
    fetchMaybe fetchSession

data CreateSession = CreateSession Session
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateSession Session where
  update (CreateSession Session{..}) = do
    runQuery_ $ sqlInsert "sessions" $ do
      sqlSet "user_id" sesUserID
      sqlSet "pad_user_id" sesPadUserID
      sqlSet "token" sesToken
      sqlSet "csrf_token" sesCSRFToken
      sqlSet "domain" sesDomain
      sqlSet "expires" sesExpires
      mapM_ sqlResult selectSessionSelectorsList
    fetchOne fetchSession

data UpdateSession = UpdateSession Session
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateSession Bool where
  update (UpdateSession Session{..}) = do
    runQuery01 $ sqlUpdate "sessions" $ do
      sqlSet "user_id" sesUserID
      sqlSet "pad_user_id" sesPadUserID
      sqlSet "token" sesToken
      sqlSet "csrf_token" sesCSRFToken
      sqlSet "domain" sesDomain
      sqlSet "expires" sesExpires
      sqlWhereEq "id" sesID

fetchSession
  :: ( SessionID, Maybe UserID, Maybe UserID
     , UTCTime, MagicHash, MagicHash, Text )
  -> Session
fetchSession ( sesID, sesUserID, sesPadUserID
             , sesExpires, sesToken, sesCSRFToken, sesDomain ) =
  Session {..}

data PurgeExpiredTemporaryLoginTokens = PurgeExpiredTemporaryLoginTokens
instance (MonadDB m, MonadTime m) => DBUpdate m PurgeExpiredTemporaryLoginTokens () where
  update _ = do
    -- Expired tokens should remain in the DB for 12h to provide better error messages
    purgeTime <- ((12 * 60) `minutesBefore`) <$> currentTime
    runSQL_ $ "DELETE FROM temporary_login_tokens WHERE expiration_time <=" <?> purgeTime

data NewTemporaryLoginToken = NewTemporaryLoginToken UserID UTCTime
instance (CryptoRNG m, MonadDB m) => DBUpdate m NewTemporaryLoginToken MagicHash where
  update (NewTemporaryLoginToken uid expiryTime) = do
    hash <- random
    runQuery_ . sqlInsert "temporary_login_tokens" $ do
      sqlSet "hash" hash
      sqlSet "user_id" uid
      sqlSet "expiration_time" expiryTime
    return hash

-- | We allow for at most 51 sessions with the same user_id, so if there
-- are more, just delete the oldest ones. Note: only 50 sessions are left
-- because we do deletion BEFORE inserting new session. This is better
-- because this way we can be sure that newest session will always end
-- up in the database.
deleteSuperfluousUserSessions :: (MonadDB m, MonadLog m) => UserID -> m ()
deleteSuperfluousUserSessions uid = do
  n <- runQuery $
       "DELETE FROM sessions WHERE id IN (SELECT id FROM sessions WHERE user_id ="
       <?> uid <+> "ORDER BY expires DESC OFFSET 50)"
  logInfo "Superfluous sessions of user removed from the database" $ object
    [ identifier uid
    , "sessions" .= n
    ]
