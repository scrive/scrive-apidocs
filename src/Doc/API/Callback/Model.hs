module Doc.API.Callback.Model (
    DocumentAPICallback
  , documentAPICallback
  , triggerAPICallbackIfThereIsOne
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Int

import ActionQueue.Scheduler
import DB
import Doc.API.Callback.Data
import Doc.API.Callback.Execute
import Doc.DocStateData
import Doc.DocumentID
import JobQueue.Config
import KontraPrelude
import MinutesTime
import User.CallbackScheme.Model
import Util.SignatoryLinkUtils
import qualified Log

documentAPICallback :: (MonadIO m, MonadBase IO m, Log.MonadLog m, MonadMask m)
  => (forall r. Scheduler r -> m r)
  -> ConsumerConfig m CallbackID DocumentAPICallback
documentAPICallback runExecute = ConsumerConfig {
  ccJobsTable = "document_api_callbacks"
, ccConsumersTable = "document_api_callback_consumers"
, ccJobSelectors = ["id", "document_id", "url", "attempts"]
, ccJobFetcher = \(cid, did, url, attempts) -> DocumentAPICallback {
    dacID = cid
  , dacDocumentID = did
  , dacURL = url
  , dacAttempts = attempts
  }
, ccJobIndex = dacID
, ccNotificationChannel = Just apiCallbackNotificationChannel
, ccNotificationTimeout = 60 * 1000000
, ccMaxRunningJobs = 32
, ccProcessJob = \dac@DocumentAPICallback{..} -> runExecute $ execute dac >>= \case
  True  -> return $ Ok Remove
  False -> dbQuery (CheckQueuedCallbacksFor dacDocumentID) >>= \case
    True -> do
      Log.mixlog_ $ "Callback for document" <+> show dacDocumentID <+> "failed and there are more queued, discarding."
      return $ Failed Remove
    False -> Failed <$> onFailure dacAttempts
, ccOnException = onFailure . dacAttempts
}
  where
    onFailure attempts = case attempts of
      1 -> return . RetryAfter $ iminutes 5
      2 -> return . RetryAfter $ iminutes 10
      3 -> return . RetryAfter $ iminutes 30
      4 -> return . RetryAfter $ ihours 1
      5 -> return . RetryAfter $ ihours 2
      6 -> return . RetryAfter $ ihours 4
      7 -> return . RetryAfter $ ihours 4
      8 -> return . RetryAfter $ ihours 4
      9 -> return . RetryAfter $ ihours 8
      _ -> do
        Log.mixlog_ "10th call attempt failed, discarding."
        return Remove

triggerAPICallbackIfThereIsOne :: (MonadDB m, MonadCatch m, Log.MonadLog m)
  => Document -> m ()
triggerAPICallbackIfThereIsOne doc@Document{..} = case documentstatus of
  Preparation -> return () -- We don't trigger callbacks for Drafts
  _ -> case documentapicallbackurl of
    Just url -> addAPICallback url
    Nothing -> case (maybesignatory =<< getAuthorSigLink doc) of
      -- FIXME: this should be modified so it's not Maybe
      Just userid -> do
        mcallbackschema <- dbQuery $ GetUserCallbackSchemeByUserID userid
        case mcallbackschema of
          Just (ConstantUrlScheme url) -> addAPICallback url
          _ -> return () -- No callback defined for document nor user.
      Nothing -> $unexpectedErrorM $ "Document" <+> show documentid <+> "has no author"

  where
    addAPICallback url = do
      Log.mixlog_ $ "Triggering API callback for document " ++ show documentid
      dbUpdate $ MergeAPICallback documentid url

----------------------------------------

apiCallbackNotificationChannel :: Channel
apiCallbackNotificationChannel = "api_callback"

data CheckQueuedCallbacksFor = CheckQueuedCallbacksFor DocumentID
instance (MonadDB m, MonadCatch m) => DBQuery m CheckQueuedCallbacksFor Bool where
  query (CheckQueuedCallbacksFor did) = do
    runSQL01_ $ "SELECT EXISTS (SELECT TRUE FROM document_api_callbacks WHERE document_id =" <?> did <+> "AND reserved_by IS NULL)"
    fetchOne runIdentity

data MergeAPICallback = MergeAPICallback DocumentID String
instance (MonadDB m, MonadCatch m, Log.MonadLog m) => DBUpdate m MergeAPICallback () where
  update (MergeAPICallback did url) = do
    -- If callbacks are queued, but not being processed, replace them.
    -- There will be only 1 queued callback majority of times, but it
    -- doesn't have to be the case. Consider the following:
    --  * Callback #1 is scheduled and run.
    --  * Callback #2 is scheduled and no callback is queued (#1 runs),
    --    so it's also scheduled and run.
    --  * Both #1 and #2 fail to execute and each of them checks for
    --    other queued callbacks before the other one is released, so
    --    they don't see each other and both end up being postponed.
    --  * Callback #3 is scheduled and below query updates both #1 and #2.
    --
    -- Replacing one callback using SELECT ... LIMIT 1 FOR UPDATE doesn't
    -- work reliably here either. Consider the following:
    --  * Callbacks #1 and #2 are queued, we attempt to schedule callback #3.
    --  * We try to select id of #1 for update, but it was just selected for
    --    update by the job queue, so our select waits.
    --  * Job queue sets the flag reserved_by and releases the lock on the row.
    --  * The select proceeeds, but the row now doesn't satisfy WHERE clause,
    --    so the select returns no rows.
    --  * We end up inserting #3 as a separate callback, even though we could
    --    replace #2 instead.
    updated <- runQuery . sqlUpdate "document_api_callbacks" $ do
      setFields
      sqlWhereEq "document_id" did
      sqlWhere "reserved_by IS NULL"
    when (updated == 0) $ do
      -- Otherwise insert a new one.
      Log.mixlog_ $ "Inserting callback for document" <+> show did
      runQuery_ $ sqlInsert "document_api_callbacks" setFields
    notify apiCallbackNotificationChannel ""
    Log.mixlog_ $ "Callback for document" <+> show did <+> "merged"
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "document_id" did
        sqlSet "run_at" unixEpoch
        sqlSet "url" url
        sqlSet "attempts" (0::Int32)
