module Callback.Model
  ( callbackNotificationChannel
  , scheduleNewCallback
  , scheduleExistingCallback
  , scheduleDependentCallbacks
  ) where

import Control.Monad.Catch
import Data.Aeson
import Data.Foldable

import Callback.Types
import DB
import MinutesTime

-- | Notification channel for the 'callbackConsumer'.
callbackNotificationChannel :: Channel
callbackNotificationChannel = "callback"

----------------------------------------

-- | Schedule a single callback for immediate execution.
scheduleNewCallback
  :: (MonadDB m, MonadThrow m, ToJSON payload)
  => Text
  -> AuthMethod
  -> payload
  -> m CallbackID
scheduleNewCallback url authMethod payload = do
  runQuery_ . sqlInsert "callbacks" $ do
    sqlSet "run_at"      unixEpoch
    sqlSet "url"         url
    sqlSet "payload"     (JSONB $ encode payload)
    sqlSet "auth_method" authMethod
    sqlResult "id"
  cid <- fetchOne runIdentity
  notify callbackNotificationChannel ""
  return cid

-- | Schedule a callback that already exists in the database for execution.
scheduleExistingCallback :: MonadDB m => CallbackID -> m ()
scheduleExistingCallback cid = do
  runQuery_ . sqlUpdate "callbacks" $ do
    sqlSet "run_at" unixEpoch
    sqlWhereEq "id" cid
  notify callbackNotificationChannel ""

-- | Schedule a list of dependent callbacks. They will be executed in the order
-- as given and each will start only after the one before it has succeeded.
scheduleDependentCallbacks
  :: (MonadDB m, MonadThrow m, ToJSON payload)
  => [(Text, AuthMethod, payload)]
  -> m [CallbackID]
scheduleDependentCallbacks callbacks = do
  -- Put the chain of callbacks into the database where Nth one references the
  -- id of the (N+1)th one in the "next" field.
  cids <- foldrM putDependentCallback [] callbacks
  -- If the input list was non-empty, schedule the first one to start the chain.
  whenJust (listToMaybe cids) scheduleExistingCallback
  return cids
  where
    putDependentCallback (url, authMethod, payload) cids = do
      runQuery_ . sqlInsert "callbacks" $ do
        sqlSet "url"         url
        sqlSet "payload"     (JSONB $ encode payload)
        sqlSet "auth_method" authMethod
        sqlSet "next"        (listToMaybe cids)
        sqlResult "id"
      (: cids) <$> fetchOne runIdentity
