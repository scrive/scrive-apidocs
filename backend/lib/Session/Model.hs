module Session.Model (
    getNonTempSessionID
  , getCurrentSession
  , updateSession
  , getUserFromSession
  , getPadUserFromSession
  , getSession
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Crypto.RNG
import Happstack.Server hiding (Session)
import Log

import ActionQueue.Core
import Context
import DB
import KontraMonad
import KontraPrelude
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

getNonTempSessionID :: (CryptoRNG m, KontraMonad m, MonadDB m, MonadThrow m, MonadTime m, ServerMonad m)
                    => m SessionID
getNonTempSessionID = do
  sid <- ctxsessionid `liftM` getContext
  if sid == tempSessionID
    then do
      new_sid <- insertEmptySession
      modifyContext $ \ctx -> ctx { ctxsessionid = new_sid }
      return new_sid
    else return sid
  where
    insertEmptySession = do
      token <- random
      csrf_token <- random
      expires <- sessionNowModifier `liftM` currentTime
      domain <- currentDomain
      update (NewAction session expires (Nothing, Nothing, token, csrf_token, domain))
        >>= return . sesID

-- | Get current session based on cookies set.
-- If no session is available, return new, empty session.
getCurrentSession :: (CryptoRNG m, MonadDB m, MonadThrow m, ServerMonad m, MonadLog m) => m Session
getCurrentSession = currentSessionInfoCookies >>= getSessionFromCookies
  where
    getSessionFromCookies (cs:css) = do
      domain <- currentDomain
      mses <- getSession (cookieSessionID cs) (cookieSessionToken cs) domain
      case mses of
        Just ses -> return ses
        Nothing  -> getSessionFromCookies css
    getSessionFromCookies [] = emptySession

updateSession :: (FilterMonad Response m, MonadLog m, MonadDB m, MonadThrow m, ServerMonad m, MonadIO m) => Session -> SessionID -> (Maybe UserID) -> (Maybe UserID) -> m ()
updateSession old_ses new_ses_id new_muser new_mpad_user = do
  case new_ses_id == tempSessionID of
    -- We have no session and we want to log in some user
    True | (isJust new_muser || isJust new_mpad_user) -> do
      let uid = fromJust $ (new_muser `mplus` new_mpad_user)
      n <- deleteSuperfluousUserSessions uid
      logInfo "Superfluous sessions of user removed from the database" $ object [
          identifier_ uid
        , "sessions" .= n
        ]
      expires <- sessionNowModifier `liftM` currentTime
      dbUpdate (NewAction session expires (new_muser, new_mpad_user, sesToken old_ses, sesCSRFToken old_ses, sesDomain old_ses))
        >>= startSessionCookie
    -- We have no session and we don't want to log in some user
    True -> return ()
    -- We are updating existing session
    False | sesID old_ses == new_ses_id -> do
      when (sesUserID old_ses /= new_muser || sesPadUserID old_ses /= new_mpad_user) $ do
        success <- dbUpdate $ UpdateAction session old_ses { sesUserID = new_muser, sesPadUserID = new_mpad_user}
        when (not success) $
          logInfo_ "UpdateAction didn't update session where it should have to (existing session)"
        -- We are logging out, remove session cookie
        when (new_muser == Nothing && new_mpad_user == Nothing) $
          stopSessionCookie
    -- We got new session - now we need to generate a cookie for it. We also update it with users logged in durring this handler.
    False | sesID old_ses /= new_ses_id -> do
      mses <- dbQuery $ GetAction session new_ses_id
      case mses of
        Nothing -> logInfo_ "updateSession failed while trying to switch session"
        Just sess -> do
          let new_sess = sess { sesUserID = new_muser, sesPadUserID = new_mpad_user}
          success <- dbUpdate $ UpdateAction session new_sess
          startSessionCookie new_sess
          when (not success) $
            logInfo_ "UpdateAction didn't update session where it should have to"
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

getSession :: (MonadDB m, MonadThrow m, MonadTime m) => SessionID -> MagicHash -> String -> m (Maybe Session)
getSession sid token domain = runMaybeT $ do
  Just ses@Session{sesToken,sesDomain} <- dbQuery $ GetAction session sid
  guard $ sesToken == token
  guard $ sesDomain == domain
  return ses

-- | We allow for at most 5 sessions with the same user_id, so if there
-- are more, just delete the oldest ones. Note: only 4 sessions are left
-- because we do deletion BEFORE inserting new session. This is better
-- because this way we can be sure that newest session will always end
-- up in the database.
deleteSuperfluousUserSessions :: MonadDB m => UserID -> m Int
deleteSuperfluousUserSessions uid = do
  runQuery $ "DELETE FROM sessions WHERE id IN (SELECT id FROM sessions WHERE user_id =" <?> uid <+> "ORDER BY expires DESC OFFSET 4)"
