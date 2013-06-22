module Doc.API.Callback.Model (
    DocumentAPICallback
  , documentAPICallback
  , triggerAPICallbackIfThereIsOne
  ) where

import Control.Applicative
import ActionQueue.Core
import ActionQueue.Scheduler
import DB
import DB.SQL2
import Doc.API.Callback.Tables
import Doc.DocumentID
import Doc.DocStateData
import Control.Monad.IO.Class
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
  , qaFields = \(did, url) -> [
      ("document_id", toSql did)
    , ("url", toSql url)
    , ("attempt", toSql (1::Int))
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
      res <- execute dac
      case res of
        True -> deleteAction
        False -> do
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


triggerAPICallbackIfThereIsOne :: (MonadDB m, MonadIO m) => Document -> m ()
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
          Log.debug $ "Triggering API callback for document " ++ show (documentid doc)
          now <- getMinutesTime
          _ <- dbUpdate $ DeleteAction documentAPICallback (documentid doc)
          _ <- dbUpdate $ NewAction documentAPICallback now (documentid doc, url)
          return ()

