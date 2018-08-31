module Session.Model (
    getNonTempSessionID
  , getCurrentSession
  , updateSession
  , getUserFromSession
  , getPadUserFromSession
  , getSession
  , startNewSession
  , DeleteExpiredSessions(..)
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
import Session.Data
import User.Model
import Utils.HTTP

-- | Gets non temporary session id from Context. If current session id
-- is temporary, it inserts new, empty session into database and returns
-- its id (needed when document ticket/eleg transaction needs to be
-- inserted into the database, but current session is temporary), also
-- modifying Context to carry modified id.

getNonTempSessionID :: ( CryptoRNG m, KontraMonad m, MonadDB m
                       , MonadThrow m, MonadTime m, ServerMonad m )
                    => m SessionID
getNonTempSessionID = do
  sid <- (get ctxsessionid) `liftM` getContext
  if sid == tempSessionID
    then do
      new_sid <- insertEmptySession
      modifyContext $ set ctxsessionid new_sid
      return new_sid
    else return sid
  where
    insertEmptySession = do
      token <- random
      csrf_token <- random
      expires <- sessionNowModifier `liftM` currentTime
      domain <- currentDomain
      session <- update $ CreateSession $
        Session tempSessionID Nothing Nothing expires token csrf_token domain
      return $ sesID session

-- | Get current session based on cookies set.
-- If no session is available, return new, empty session.

-- IE 10 is sending cookies for both domain and subdomain (scrive.com
-- & nj.scrive.com) We need to read them both, since we have no idea
-- which is the right one.
getCurrentSession :: ( CryptoRNG m, MonadDB m, MonadThrow m
                     , ServerMonad m, MonadLog m )
                  => m Session
getCurrentSession = currentSessionInfoCookies >>= getSessionFromCookies
  where
    getSessionFromCookies (cs:css) = do
      domain <- currentDomain
      mses <- getSession (cookieSessionID cs) (cookieSessionToken cs) domain
      case mses of
        Just ses -> return ses
        Nothing  -> getSessionFromCookies css
    getSessionFromCookies [] = emptySession

startNewSession :: ( FilterMonad Response m, MonadLog m, MonadDB m
                   , MonadThrow m, ServerMonad m, MonadIO m, MonadBase IO m)
                => Session -> Maybe UserID -> Maybe UserID -> m Session
startNewSession _       Nothing Nothing    = internalError
startNewSession session mnewuid mnewpaduid = do
  let uid = fromJust $ (mnewuid `mplus` mnewpaduid)
  n <- deleteSuperfluousUserSessions uid
  logInfo "Superfluous sessions of user removed from the database" $ object [
      identifier uid
    , "sessions" .= n
    ]
  expires <- sessionNowModifier <$> currentTime
  dbUpdate . CreateSession $ session {
      sesExpires   = expires
    , sesUserID    = mnewuid
    , sesPadUserID = mnewpaduid
    }

updateSession :: ( FilterMonad Response m, MonadLog m, MonadDB m, MonadThrow m
                 , ServerMonad m, MonadIO m, MonadBase IO m )
              => Session -> SessionID -> (Maybe UserID) -> (Maybe UserID) -> m ()
updateSession old_ses new_ses_id new_muser new_mpad_user = do
  case new_ses_id == tempSessionID of
    -- We have no session and we want to log in some user
    True | (isJust new_muser || isJust new_mpad_user) -> do
      startNewSession old_ses new_muser new_mpad_user >>= startSessionCookie
    -- We have no session and we don't want to log in some user
    True -> return ()
    -- We are updating existing session
    False | sesID old_ses == new_ses_id -> do
      when (sesUserID old_ses /= new_muser
            || sesPadUserID old_ses /= new_mpad_user) $ do
        success <- dbUpdate . UpdateSession $
          old_ses { sesUserID = new_muser, sesPadUserID = new_mpad_user}
        when (not success) $
          logInfo_ $ "UpdateSession didn't update session"
          <> " where it should have to (existing session)"
        -- We are logging out, remove session cookie
        when (new_muser == Nothing && new_mpad_user == Nothing) $
          stopSessionCookie
    -- We got new session - now we need to generate a cookie for
    -- it. We also update it with users logged in durring this
    -- handler.
    False | sesID old_ses /= new_ses_id -> do
      mses <- dbQuery $ GetSession new_ses_id
      case mses of
        Nothing -> logInfo_ "updateSession failed while trying to switch session"
        Just sess -> do
          let new_sess = sess { sesUserID = new_muser
                              , sesPadUserID = new_mpad_user }
          success <- dbUpdate . UpdateSession $ new_sess
          startSessionCookie new_sess
          when (not success) $
            logInfo_ "UpdateSession didn't update session where it should have to"
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
           => SessionID -> MagicHash -> String -> m (Maybe Session)
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
  :: (SessionID, Maybe UserID, Maybe UserID, UTCTime, MagicHash, MagicHash, String)
  -> Session
fetchSession (sid, m_user_id, m_pad_user_id, expires, token, csrf_token, domain) =
  Session
    { sesID = sid
    , sesUserID = m_user_id
    , sesPadUserID = m_pad_user_id
    , sesExpires = expires
    , sesToken = token
    , sesCSRFToken = csrf_token
    , sesDomain = domain
    }

-- | We allow for at most 51 sessions with the same user_id, so if there
-- are more, just delete the oldest ones. Note: only 50 sessions are left
-- because we do deletion BEFORE inserting new session. This is better
-- because this way we can be sure that newest session will always end
-- up in the database.
deleteSuperfluousUserSessions :: MonadDB m => UserID -> m Int
deleteSuperfluousUserSessions uid = do
  runQuery $
    "DELETE FROM sessions WHERE id IN (SELECT id FROM sessions WHERE user_id ="
    <?> uid <+> "ORDER BY expires DESC OFFSET 50)"
