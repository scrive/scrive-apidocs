module Doc.API.Callback.Model (
    DocumentAPICallback(..)
  , documentAPICallback
  , triggerAPICallbackIfThereIsOne 
  ) where

import Control.Applicative
import Data.Int
import Data.Typeable
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)

import ActionQueue.Core
import ActionQueue.Scheduler
import DB
import DB.SQL2
import Doc.API.Callback.Tables
import Doc.DocumentID
import Doc.DocStateData

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
  , qaDecode = kFold decoder []
  , qaUpdateSQL = \DocumentAPICallback{..} -> toSQLCommand $ sqlUpdate "document_api_callbacks" $ do
      sqlSet "expires" dacExpires
      sqlSet "url" dacURL
      sqlSet "attempt" dacAttempt
      sqlWhereEq (qaIndexField documentAPICallback) dacDocumentID
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
      Log.debug $ "Calling " ++ show dacURL ++ " (documentid = " ++ show dacDocumentID ++ ")..."
      (exitcode, stdout, stderr) <- readCurl [
          "-X", "POST"
        , "-d", "documentid=" ++ show dacDocumentID
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


triggerAPICallbackIfThereIsOne :: MonadDB m => Document -> m ()
triggerAPICallbackIfThereIsOne doc = do
      case (documentapicallbackurl doc) of
        Nothing -> return () 
        Just url -> do
          -- Race condition. Andrzej said that he can fix it later.
          Log.debug $ "Triggering API callback for document " ++ show (documentid doc)
          now <- getMinutesTime   
          _ <- dbUpdate $ DeleteAction documentAPICallback (documentid doc)
          _ <- dbUpdate $ NewAction documentAPICallback now (documentid doc, url)
          return ()
          