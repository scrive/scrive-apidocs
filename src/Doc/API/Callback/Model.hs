module Doc.API.Callback.Model (
    DocumentAPICallback
  , documentAPICallback
  , triggerAPICallbackIfThereIsOne
  ) where

import Control.Applicative
import ActionQueue.Core
import ActionQueue.Scheduler
import Data.Int
import DB
import Doc.API.Callback.Tables
import Doc.DocumentID
import Doc.DocStateData
import MinutesTime
import qualified Log
import Doc.API.Callback.DocumentAPICallback
import Doc.API.Callback.Execute
import User.CallbackScheme.Model
import Util.SignatoryLinkUtils
import Control.Monad

documentAPICallback :: Action DocumentID DocumentAPICallback (DocumentID, String) Scheduler
documentAPICallback = Action {
    qaTable = tableDocumentApiCallbacks
  , qaSetFields = \(did, url) -> do
      sqlSet "document_id" did
      sqlSet "url" url
      sqlSet "attempt" (1::Int32)
  , qaSelectFields = ["document_id", "expires", "url", "attempt"]
  , qaIndexField = "document_id"
  , qaExpirationDelay = "5 minutes" -- not really needed
  , qaDecode = \(document_id, expires, url, attempt) -> DocumentAPICallback {
      dacDocumentID = document_id
    , dacExpires = expires
    , dacURL = url
    , dacAttempt = attempt
    }
  , qaUpdateSQL = \DocumentAPICallback{..} -> toSQLCommand $ sqlUpdate "document_api_callbacks" $ do
      sqlSet "expires" dacExpires
      sqlSet "url" dacURL
      sqlSet "attempt" dacAttempt
      sqlWhereEq (qaIndexField documentAPICallback) dacDocumentID
  , qaEvaluateExpired = evaluateDocumentCallback
  }
  where
    evaluateDocumentCallback dac@DocumentAPICallback{..} = do
      res <- execute dac
      case res of
        True -> deleteAction
        False -> do
          case dacAttempt of
            1 -> evaluateAgainAfter 5
            2 -> evaluateAgainAfter 10
            3 -> evaluateAgainAfter 30
            4 -> evaluateAgainAfter 60
            5 -> evaluateAgainAfter 120
            6 -> evaluateAgainAfter 240
            7 -> evaluateAgainAfter 240
            8 -> evaluateAgainAfter 240
            9 -> evaluateAgainAfter 480
            _ -> do
              Log.mixlog_ "10th call attempt failed, discarding."
              deleteAction
      where
        deleteAction = do
          _ <- dbUpdate $ DeleteAction documentAPICallback dacDocumentID
          return ()
        evaluateAgainAfter n = do
          Log.mixlog_ $ "Deferring call for " ++ show n ++ " minutes"
          expires <- minutesAfter n <$> getMinutesTime
          _ <- dbUpdate . UpdateAction documentAPICallback $ dac {
              dacExpires = expires
            , dacAttempt = dacAttempt + 1
          }
          return ()


triggerAPICallbackIfThereIsOne :: (MonadDB m, Log.MonadLog m) => Document -> m ()
triggerAPICallbackIfThereIsOne doc = do
      case (documentapicallbackurl doc) of
        Nothing -> do
          case (join $ maybesignatory <$> getAuthorSigLink doc) of
            Nothing -> return () --This should never happend
            Just userid -> do
             mcallbackschema <- dbQuery $ GetUserCallbackSchemeByUserID userid
             case mcallbackschema of
                 Just (ConstantUrlScheme url) -> addAPICallback url
                 _ -> return () -- No callback defined for document and for user
        Just url -> addAPICallback url
  where
    addAPICallback url = do
          -- Race condition. Andrzej said that he can fix it later.
          Log.mixlog_ $ "Triggering API callback for document " ++ show (documentid doc)
          now <- getMinutesTime
          _ <- dbUpdate $ DeleteAction documentAPICallback (documentid doc)
          _ <- dbUpdate $ NewAction documentAPICallback now (documentid doc, url)
          return ()
