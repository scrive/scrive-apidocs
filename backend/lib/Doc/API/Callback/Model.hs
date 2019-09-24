module Doc.API.Callback.Model (
    DocumentAPICallback
  , documentAPICallback
  , triggerAPICallbackIfThereIsOne
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG (CryptoRNG)
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log

import API.APIVersion
import CronEnv
import DB
import Doc.API.Callback.Execute
import Doc.API.Callback.Types
import Doc.DocStateData
import Doc.DocumentID
import Doc.Logging
import File.Storage
import Log.Identifier
import MinutesTime
import User.CallbackScheme.Model
import Util.SignatoryLinkUtils

documentAPICallback
  :: ( MonadIO m, MonadBase IO m, MonadLog m, MonadMask m
     , MonadFileStorage cronenv, CryptoRNG cronenv, MonadBaseControl IO cronenv
     , MonadDB cronenv, MonadIO cronenv, MonadLog cronenv
     , MonadReader CronEnv cronenv, MonadMask cronenv )
  => (forall r. cronenv r -> m r)
  -> Int
  -> ConsumerConfig m CallbackID DocumentAPICallback
documentAPICallback runExecute maxRunningJobs =
  ConsumerConfig {
      ccJobsTable = "document_api_callbacks"
    , ccConsumersTable = "document_api_callback_consumers"
    , ccJobSelectors = ["id", "document_id", "api_version", "url", "attempts"]
    , ccJobFetcher = \ (cid, did, apiVersion, url, attempts) ->
        DocumentAPICallback {
            dacID = cid
          , dacDocumentID = did
          , dacApiVersion = apiVersion
          , dacURL = url
          , dacAttempts = attempts
          }
    , ccJobIndex = dacID
    , ccNotificationChannel = Just apiCallbackNotificationChannel
    , ccNotificationTimeout = 60 * 1000000 -- 1 minute
    , ccMaxRunningJobs = maxRunningJobs
    , ccProcessJob = \ dac@DocumentAPICallback {..} -> logDocument dacDocumentID . runExecute $ do
        execute dac >>= \ case
          True  -> return $ Ok Remove
          False -> dbQuery (CheckQueuedCallbacksFor dacDocumentID) >>= \ case
            True  -> do
              logInfo_ "Callback for document failed and there are more queued, discarding"
              return $ Failed Remove
            False -> Failed <$> onFailure dac
    , ccOnException = const onFailure
    }
      where
        onFailure DocumentAPICallback {..} = logDocument dacDocumentID $
          case dacAttempts of
            1 -> return . RerunAfter $ iminutes 5
            2 -> return . RerunAfter $ iminutes 10
            3 -> return . RerunAfter $ iminutes 30
            4 -> return . RerunAfter $ ihours 1
            5 -> return . RerunAfter $ ihours 2
            6 -> return . RerunAfter $ ihours 4
            7 -> return . RerunAfter $ ihours 4
            8 -> return . RerunAfter $ ihours 4
            9 -> return . RerunAfter $ ihours 8
            _ -> do
              logInfo_ "10th call attempt failed, discarding"
              return Remove

triggerAPICallbackIfThereIsOne :: (MonadDB m, MonadCatch m, MonadLog m)
  => Document -> m ()
triggerAPICallbackIfThereIsOne doc@Document{..} = logDocument documentid $ case documentstatus of
  Preparation -> return () -- We don't trigger callbacks for Drafts
  -- NOTE: v2 has priority over v1 callbacks, because v2 calls don't expose v1 callbacks
  _ -> case (documentapiv2callbackurl, documentapiv1callbackurl) of
    (Just url,_) -> addAPICallback url V2
    (_,Just url) -> addAPICallback url V1
    _ -> case (maybesignatory =<< getAuthorSigLink doc) of
      -- FIXME: this should be modified so it's not Maybe
      Just userid -> do
        mcallbackschema <- dbQuery $ GetUserCallbackSchemeByUserID userid
        case mcallbackschema of
          Just (ConstantUrlScheme url) -> addAPICallback url V1
          Just (ConstantUrlSchemeV2 url) -> addAPICallback url V2
          _ -> return () -- No callback defined for document nor user.
      Nothing -> do
        logAttention "Cant trigger user API callback for doc wihout author" $ object [
            identifier documentid
          ]
        return () -- skipping

  where
    addAPICallback url apiVersion = do
      logInfo "Triggering API callback for document with api version" $
        object [ identifier apiVersion]
      dbUpdate $ MergeAPICallback documentid url apiVersion

----------------------------------------

apiCallbackNotificationChannel :: Channel
apiCallbackNotificationChannel = "api_callback"

data CheckQueuedCallbacksFor = CheckQueuedCallbacksFor DocumentID
instance (MonadDB m, MonadCatch m) => DBQuery m CheckQueuedCallbacksFor Bool where
  query (CheckQueuedCallbacksFor did) = do
    runSQL01_ $ "SELECT EXISTS (SELECT TRUE FROM document_api_callbacks WHERE document_id =" <?> did <+> "AND reserved_by IS NULL)"
    fetchOne runIdentity

data MergeAPICallback = MergeAPICallback DocumentID Text APIVersion
instance (MonadDB m, MonadCatch m, MonadLog m) => DBUpdate m MergeAPICallback () where
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
  update (MergeAPICallback did url apiVersion) = logDocument did $ do
    updated <- runQuery . sqlUpdate "document_api_callbacks" $ do
      setFields
      sqlWhereEq "document_id" did
      sqlWhere "reserved_by IS NULL"
    when (updated == 0) $ do
      -- Otherwise insert a new one.
      logInfo_ "Inserting callback for document"
      runQuery_ $ sqlInsert "document_api_callbacks" setFields
    notify apiCallbackNotificationChannel ""
    logInfo_ "Callback for document merged"
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "document_id" did
        sqlSet "api_version" apiVersion
        sqlSet "run_at" unixEpoch
        sqlSet "url" url
        sqlSet "attempts" (0::Int32)
