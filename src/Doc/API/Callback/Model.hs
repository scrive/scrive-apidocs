module Doc.API.Callback.Model (
    DocumentAPICallback(..)
  , documentAPICallback
  , requestCallback
  ) where

import Control.Applicative
import Data.Int
import Data.Monoid
import Data.Typeable
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)

import ActionQueue.Core
import ActionQueue.Scheduler
import DB
import Doc.API.Callback.Tables
import Doc.DocumentID
import Utils.IO
import MinutesTime
import qualified Log

data DocumentAPICallback = DocumentAPICallback {
    dacDocumentID :: DocumentID
  , dacExpires :: MinutesTime
  , dacURL :: String
  , dacAttempt :: Int32
  } deriving (Show, Typeable)

documentAPICallback :: Action DocumentID DocumentAPICallback (DocumentID, String) Scheduler
documentAPICallback = Action {
    qaTable = tableDocumentApiCallbacks
  , qaFields = \(did, url) -> [
      sql "document_id" did
    , sql "url" url
    , sql "attempt" (1::Int)
    ]
  , qaSelectFields = ["document_id", "expires", "url", "attempt"]
  , qaIndexField = "document_id"
  , qaExpirationDelay = "5 minutes" -- not really needed
  , qaDecode = foldDB decoder []
  , qaUpdateSQL = \DocumentAPICallback{..} -> mkSQL UPDATE tableDocumentApiCallbacks [
      sql "expires" dacExpires
    , sql "url" dacURL
    , sql "attempt" dacAttempt
    ] <> SQL ("WHERE " ++ qaIndexField documentAPICallback ++ " = ?") [
      toSql dacDocumentID
    ]
  , qaEvaluateExpired = evaluateDocumentCallback
  }
  where
    decoder acc document_id expires url attempt = DocumentAPICallback {
        dacDocumentID = document_id
      , dacExpires = expires
      , dacURL = url
      , dacAttempt = attempt
      } : acc

    evaluateDocumentCallback dac@DocumentAPICallback{..} = do
      Log.debug $ "Calling " ++ show dacURL ++ " (document_id = " ++ show dacDocumentID ++ ")..."
      (exitcode, stdout, stderr) <- readProcessWithExitCode' "curl" [
          "-X", "POST"
        , "-d", "document_id=" ++ show dacDocumentID
        , dacURL
        ] BSL.empty
      Log.debug $ "Call result: " ++ BSL.toString stdout
      case exitcode of
        ExitSuccess -> deleteAction
        ExitFailure _ -> do
          Log.debug $ "Call failed: " ++ BSL.toString stderr
          case dacAttempt of
            1 -> evaluateAgainAfter 5
            2 -> evaluateAgainAfter 10
            3 -> evaluateAgainAfter 30
            4 -> evaluateAgainAfter 60
            _ -> do
              Log.debug "5th call attempt failed, discarding."
              deleteAction
      where
        deleteAction = do
          _ <- dbUpdate $ DeleteAction documentAPICallback dacDocumentID
          return ()
        evaluateAgainAfter n = do
          Log.debug $ "Deferring call for " ++ show n ++ " minutes"
          expires <- minutesAfter n <$> getMinutesTime
          _ <- dbUpdate . UpdateAction documentAPICallback $ dac {
              dacExpires = expires
            , dacAttempt = dacAttempt + 1
          }
          return ()

requestCallback :: MonadDB m => DocumentID -> m Bool
requestCallback did = do
  mcallback <- dbQuery $ GetAction documentAPICallback did
  case mcallback of
    Just _ -> return True -- if request's already in there, do nothing
    Nothing -> do
      murl <- runDBEnv . getOne $ SQL "SELECT api_callback_url FROM documents WHERE id = ?" [toSql did]
      case murl of
        Nothing -> return False -- no url, no callback
        Just url -> do
          -- FIXME: obvious race condition here. to be fixed after
          -- dev-andrzej-noacid is merged as it contains appropriate
          -- tools for resolving it.
          now <- getMinutesTime
          _ <- dbUpdate $ NewAction documentAPICallback now (did, url)
          return True
