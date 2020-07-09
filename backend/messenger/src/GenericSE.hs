{-# LANGUAGE ExtendedDefaultRules #-}

module GenericSE (
    handleGenericSEEvents
  ) where

import Control.Concurrent.MVar
import Control.Exception.Lifted as E
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Happstack.Server
import Log
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.Text as T

import DB
import Messenger
import SMS.Model
import SMS.Types

data GenericSEStatus = Sending
                     | Sent
                     | Delivered
                     | Failed T.Text

parseGenericSEStatus :: ReaderT JSValue Messenger (Maybe GenericSEStatus)
parseGenericSEStatus = do
  mstatus <- fromJSValueField "Status"
  case mstatus of
    Just "Sending"   -> return $ Just Sending
    Just "Sent"      -> return $ Just Sent
    Just "Delivered" -> return $ Just Delivered
    Just "Failed"    -> do
      mreason <- fromJSValueField "Reason"
      case mreason of
        Just reason -> return $ Just $ Failed reason
        _           -> return Nothing
    _ -> return Nothing

handleInterestingEvent
  :: (MonadLog m, MonadDB m, MonadThrow m) => JSValue -> T.Text -> Maybe T.Text -> m ()
handleInterestingEvent json genericSEID mError = do
  msms <- dbQuery $ GetSMSByRemoteID genericSEID
  case msms of
    Nothing -> logAttention "GenericSE callback for nonexisting sms"
      $ object ["request" .= show json]
    Just (ShortMessage {..}) -> do
      let deliverage = case mError of
            Nothing  -> SMSDelivered
            Just err -> SMSUndelivered err
          event = SMSEvent smMSISDN deliverage
      res <- dbUpdate $ UpdateWithSMSEvent smID event
      logInfo "UpdateWithSMSEventForGenericSE returned"
        $ object ["messageId" .= genericSEID, "event" .= show event, "result" .= res]

handleGenericSEEvents :: Messenger Response
handleGenericSEEvents =
  localDomain "handleGenericSEEvents"
    . flip
        E.catch
        (\(e :: SomeException) ->
          logInfo "Logging exception" (object ["exception" .= show e]) >> throwIO e
        )
    $ do
        logInfo_ "Processing GenericSE events"
        rqVar <- rqBody <$> askRq
        rq    <- liftIO $ fmap unBody <$> tryTakeMVar rqVar
        case decode . BS.toString <$> rq of
          Just (Ok v@(JSObject callbackJSON)) -> do
            logInfo "GenericSE JSON with events parsed"
              $ object ["json" .= show callbackJSON]
            callbackData <- withJSValue v $ do
              mid     <- fromJSValueField "MessageId"
              mstatus <- parseGenericSEStatus
              return (mid, mstatus)
            case callbackData of
              (Nothing, _) -> logAttention "Couldn't parse GenericSE id "
                $ object ["request" .= show rq]
              (Just genericSEID, Nothing) ->
                logAttention "Couldn't parse GenericSE event"
                  $ object ["request" .= show rq, "batch_id" .= genericSEID]
              (Just genericSEID, Just status) -> case status of
                Sending       -> return () -- not interesting to us
                Sent          -> return () -- not interesting to us
                Delivered     -> handleInterestingEvent v genericSEID Nothing
                Failed reason -> handleInterestingEvent v genericSEID $ Just reason
          _ -> do
            logAttention "Couldn't parse GenericSE callback JSON"
              $ object ["request" .= show rq]
        ok $ toResponse ("Thanks!" :: String)
