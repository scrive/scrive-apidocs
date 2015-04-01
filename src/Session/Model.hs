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
import Happstack.Server hiding (Session)

import ActionQueue.Core
import Context
import Crypto.RNG
import DB
import KontraMonad
import KontraPrelude
import MagicHash
import MinutesTime
import Session.Cookies
import Session.Data
import User.Model
import Utils.HTTP
import qualified Log

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
getCurrentSession :: (CryptoRNG m, HasRqData m, MonadDB m, MonadThrow m, MonadPlus m, ServerMonad m, Log.MonadLog m)
                  => m Session
getCurrentSession = withDataFn currentSessionInfoCookies $ getSessionFromCookies
  where
    getSessionFromCookies (cs:css) = do
      domain <- currentDomain
      mses <- getSession (cookieSessionID cs) (cookieSessionToken cs) domain
      case mses of
        Just ses -> return ses
        Nothing  -> getSessionFromCookies css
    getSessionFromCookies [] = emptySession

updateSession :: (FilterMonad Response m, Log.MonadLog m, MonadDB m, MonadThrow m, ServerMonad m, MonadIO m)
              => Session -> Session -> m ()
updateSession old_ses ses = do
  case sesID ses == tempSessionID of
    -- if session id is temporary, but its data is not empty, insert it into db
    True | not (isSessionEmpty ses) -> do
      when (isNothing (sesUserID old_ses) && isJust (sesUserID ses)) $ do
        let uid = $fromJust $ sesUserID ses
        n <- deleteSuperfluousUserSessions uid
        Log.mixlog_ $ show n ++ " superfluous sessions of user with id = " ++ show uid ++ " removed from the database"
      expires <- sessionNowModifier `liftM` currentTime
      let Session{..} = ses
      dbUpdate (NewAction session expires (sesUserID, sesPadUserID, sesToken, sesCSRFToken, sesDomain))
        >>= startSessionCookie
    _ | old_ses /= ses -> do
      success <- dbUpdate $ UpdateAction session ses
      -- if below condition is met, that means empty session was inserted
      -- into the database (with getNonTempSessionID) to allow inserting
      -- document token/eid transaction, but cookie wasn't created, so
      -- we need to create it here.
      when (sesID old_ses == tempSessionID && sesID ses /= tempSessionID) $
        startSessionCookie ses
      when (not success) $
        Log.mixlog_ "UpdateAction didn't update session where it should have to"
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
sessionNowModifier = (120 `minutesAfter`)

isSessionEmpty :: Session -> Bool
isSessionEmpty Session{..} = isNothing sesUserID && isNothing sesPadUserID

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
