{-# LANGUAGE ExtendedDefaultRules #-}

module Mblox (
    handleMbloxEvents
  ) where

import Control.Concurrent.MVar
import Control.Exception.Lifted as E
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Happstack.Server
import Log
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.UTF8 as BS

import DB
import Messenger
import SMS.Model
import SMS.Types

handleMbloxEvents :: Messenger Response
handleMbloxEvents =
  localDomain "handleMbloxEvents"
    . flip
        E.catch
        (\(e :: SomeException) ->
          logInfo "Logging exception" (object ["exception" .= show e]) >> throwIO e
        )
    $ do
        logInfo_ "Processing Mblox events"
        rqVar <- rqBody <$> askRq
        rq    <- liftIO $ fmap unBody <$> tryTakeMVar rqVar
        case decode . BS.toString <$> rq of
          Just (Ok v@(JSObject callbackJSON)) -> do
            logInfo "Mblox JSON with events parsed" $ object ["json" .= show callbackJSON]
            callbackData <- withJSValue v $ do
              mid    <- fromJSValueField "batch_id"
              mevent <- mbloxEventFromJSValue
              return (mid, mevent)
            case callbackData of
              (Nothing, _) ->
                logAttention "Couldn't parse Mblox id " $ object ["request" .= show rq]
              (Just mbloxID, (False, _)) -> logAttention "Couldn't parse Mblox event"
                $ object ["request" .= show rq, "batch_id" .= mbloxID]
              (Just _      , (True, Nothing)   ) -> return () -- Everything parsed, just event is not interesting
              (Just mbloxID, (True, Just event)) -> do
                res <- dbUpdate $ UpdateWithSMSEventForMbloxID mbloxID event
                logInfo "UpdateWithSMSEventForMblox returned" $ object
                  ["batch_id" .= mbloxID, "event" .= show event, "result" .= res]
          _ -> do
            logAttention "Couldn't parse Mblox callback JSON"
              $ object ["request" .= show rq]
        ok $ toResponse ("Thanks!" :: String)

mbloxEventFromJSValue :: ReaderT JSValue Messenger (Bool, Maybe SMSEvent)
mbloxEventFromJSValue = do
  mrecipient <- fromJSValueField "recipient"
  mstatus    <- fromJSValueField "status"
  case (mrecipient, mstatus) of
    (Just _     , Just "Queued"    ) -> return (True, Nothing)
    (Just _     , Just "Dispatched") -> return (True, Nothing)
    (Just msisdn, Just "Delivered" ) -> return (True, Just $ SMSEvent msisdn SMSDelivered)
    (Just msisdn, Just "Aborted") ->
      return (True, Just . SMSEvent msisdn $ SMSUndelivered "Aborted")
    (Just msisdn, Just "Rejected") ->
      return (True, Just . SMSEvent msisdn $ SMSUndelivered "Rejected")
    (Just msisdn, Just "Failed") ->
      return (True, Just . SMSEvent msisdn $ SMSUndelivered "Failed")
    (Just msisdn, Just "Expired") ->
      return (True, Just . SMSEvent msisdn $ SMSUndelivered "Expired")
    (Just _, Just "Unknown") -> return (True, Nothing)
    _ -> return (False, Nothing)
