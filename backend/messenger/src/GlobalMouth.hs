{-# LANGUAGE ExtendedDefaultRules #-}
module GlobalMouth (
    handleGlobalMouthEvents
  ) where

import Control.Exception.Lifted as E
import Happstack.Server
import Log

import DB
import Happstack.Fields
import KontraPrelude
import Log.Identifier
import Messenger
import SMS.Data
import SMS.Model

handleGlobalMouthEvents :: Messenger Response
handleGlobalMouthEvents = localDomain "handleGlobalMouthEvents" . flip E.catch (\(e :: SomeException) -> logInfo "Logging exception" (object ["exception" .= show e]) >> throwIO e) $ do
  logInfo_ "Processing a globalmounth event"
  xtype       <- getField' "type"          -- dlr, delivery report
  xref        <- readField "ref"           -- scrive reference
  xmsisdn     <- getField' "msisdn"        -- full number
  xtimestamp  <- getField' "timestamp"     -- timestamp
  xdelivered  <- getField' "delivered"     -- true or false
  xreason     <- getField' "reason"        -- optional text message what went wrong if not delivered
  logInfo "Logging information" $ object [
      "type" .= xtype
    , identifier_ xref
    , "msisdn" .= xmsisdn
    , "timestamp" .= xtimestamp
    , "delivered" .= xdelivered
    , "reason" .= xreason
    ]
  let event = case (xdelivered, xreason) of
        ("true", _) -> SMSDelivered
        -- If GM throttles our smses because we send too many for our quotas,
        -- they return this event, but we can safely ignore it, because GM
        -- will continue attempts to deliver it once our quotas allow it.
        ("false", "Throttling error (ESME has exceeded allowed message limits)") -> SMSDelivered
        ("false", _) -> SMSUndelivered xreason
        _ -> $unexpectedError "unknown report"

  case xref of
    Just xref' -> do
      let ev = SMSEvent xmsisdn event
          updateEvent = do
            res <- dbUpdate $ UpdateWithSMSEvent xref' ev
            logInfo "UpdateWithSMSEvent returned" $ object [
                identifier_ xref'
              , "event" .= show ev
              , "result" .= res
              ]
          handleMissingSMS (e :: DBException) = do
            logInfo "UpdateWithSMSEvent failed (probably original SMS got purged)" $ object [
                identifier_ xref'
              , "event" .= show ev
              , "exception" .= show e
              ]
      updateEvent `E.catch` handleMissingSMS
      ok $ toResponse "Thanks"
    Nothing -> do
      logInfo "UpdateWithSMSEvent not run, invalid id" $ object [
          identifier_ xref
        ]
      ok $ toResponse "Could not read ShortMessageID"
