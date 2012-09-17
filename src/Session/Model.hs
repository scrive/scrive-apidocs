module Session.Model (
    getCurrentSession
  , updateSession
  , getUserFromSession
  , getPadUserFromSession
  ) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import Happstack.Server hiding (Session)

import ActionQueue.Core
import Crypto.RNG
import DB
import MagicHash
import MinutesTime
import Session.Cookies
import Session.Data
import User.Model
import qualified Log

-- | Get current session based on cookies set.
-- If no session is available, return new, empty session.
getCurrentSession :: (CryptoRNG m, HasRqData m, MonadDB m, MonadPlus m, ServerMonad m)
                  => m Session
getCurrentSession = withDataFn currentSessionInfoCookie $ \msc -> case msc of
  Just cs -> do
    mses <- getSession (cookieSessionID cs) (cookieSessionToken cs)
    case mses of
      Just ses -> return ses
      Nothing  -> emptySession
  Nothing -> emptySession

updateSession :: (FilterMonad Response m, MonadDB m, ServerMonad m)
              => Session -> Session -> m ()
updateSession old_ses ses = do
  case sesID ses == tempSessionID of
    True | not (isSessionEmpty ses) -> do
      when (isNothing (sesUserID old_ses) && isJust (sesUserID ses)) $ do
        let uid = fromJust $ sesUserID ses
        n <- deleteSuperfluousUserSessions uid
        Log.debug $ show n ++ " superfluous sessions of user with id = " ++ show uid ++ " removed from the database"
      expires <- (120 `minutesAfter`) `liftM` getMinutesTime
      let Session{..} = ses
      dbUpdate (NewAction session expires (sesUserID, sesPadUserID, sesToken, sesCSRFToken))
        >>= startSessionCookie
    _ | old_ses /= ses -> do
      success <- dbUpdate $ UpdateAction session ses
      when (not success) $
        Log.debug "UpdateAction didn't update session where it should have to"
    _ -> return ()

getUserFromSession :: MonadDB m => Session -> m (Maybe User)
getUserFromSession Session{sesUserID} = case sesUserID of
  Just uid -> dbQuery $ GetUserByID uid
  Nothing  -> return Nothing

getPadUserFromSession :: MonadDB m => Session -> m (Maybe User)
getPadUserFromSession Session{sesPadUserID} = case sesPadUserID of
  Just uid -> dbQuery $ GetUserByID uid
  Nothing  -> return Nothing

isSessionEmpty :: Session -> Bool
isSessionEmpty Session{..} = isNothing sesUserID && isNothing sesPadUserID

getSession :: MonadDB m => SessionID -> MagicHash -> m (Maybe Session)
getSession sid token = runMaybeT $ do
  Just ses@Session{sesToken} <- dbQuery $ GetAction session sid
  guard $ sesToken == token
  return ses

deleteSuperfluousUserSessions :: MonadDB m => UserID -> m Integer
deleteSuperfluousUserSessions uid = runDBEnv $ do
  kRun $ SQL "DELETE FROM sessions WHERE id IN (SELECT id FROM sessions WHERE user_id = ? ORDER BY expires DESC OFFSET 5)" [toSql uid]
