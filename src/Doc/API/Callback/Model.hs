module Doc.API.Callback.Model (
    DocumentAPICallback
  , documentAPICallback
  , triggerAPICallbackIfThereIsOne
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Int
import Data.Monoid.Utils

import ActionQueue.Scheduler
import DB
import Doc.API.Callback.Data
import Doc.API.Callback.Execute
import Doc.DocStateData
import Doc.DocumentID
import JobQueue.Config
import MinutesTime
import OurPrelude
import User.CallbackScheme.Model
import Util.SignatoryLinkUtils
import qualified Log

documentAPICallback :: (MonadIO m, MonadBase IO m, Log.MonadLog m, MonadMask m)
  => (forall r. Scheduler r -> m r)
  -> ConsumerConfig m DocumentID DocumentAPICallback
documentAPICallback runExecute = ConsumerConfig {
  ccJobsTable = "document_api_callbacks"
, ccConsumersTable = "document_api_callback_consumers"
, ccJobSelectors = ["id", "url", "attempts"]
, ccJobFetcher = \(did, url, attempts) -> DocumentAPICallback {
    dacID = did
  , dacURL = url
  , dacAttempts = attempts
  }
, ccJobIndex = dacID
, ccNotificationChannel = Just apiCallbackNotificationChannel
, ccNotificationTimeout = 60 * 1000000
, ccMaxRunningJobs = 32
, ccProcessJob = \dac -> runExecute (execute dac) >>= \case
  True -> return $ Ok Remove
  False -> Failed <$> onFailure (dacAttempts dac)
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
triggerAPICallbackIfThereIsOne doc@Document{..} = case documentapicallbackurl of
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

data MergeAPICallback = MergeAPICallback DocumentID String
instance (MonadDB m, MonadCatch m) => DBUpdate m MergeAPICallback () where
  update (MergeAPICallback did url) = loopOnUniqueViolation $ do
    updated <- runQuery01 . sqlUpdate "document_api_callbacks" $ do
      setFields
      sqlWhereEq "id" did
    when (not updated) $ do
      runQuery_ $ sqlInsert "document_api_callbacks" setFields
    notify apiCallbackNotificationChannel ""
    where
      setFields :: (MonadState v n, SqlSet v) => n ()
      setFields = do
        sqlSet "id"  did
        sqlSet "run_at" unixEpoch
        sqlSet "url" url
        sqlSet "attempts" (0::Int32)
