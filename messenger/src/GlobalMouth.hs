{-# LANGUAGE ExtendedDefaultRules #-}
module GlobalMouth (
    handleGlobalMouthEvents
  ) where

import Control.Exception.Lifted as E
import Happstack.Server

import DB
import Happstack.Fields
import KontraPrelude
import Log
import Messenger
import SMS.Data
import SMS.Model

handleGlobalMouthEvents :: Messenger Response
handleGlobalMouthEvents = flip E.catch (\(e :: SomeException) -> logInfo_ (show e) >> throwIO e) $ do
  logInfo_ $ "Processing some globalmounth event"
  xtype       <- getField' "type"          -- dlr, delivery report
  xref        <- readField "ref"           -- scrive reference
  xmsisdn     <- getField' "msisdn"        -- full number
  xtimestamp  <- getField' "timestamp"     -- timestamp
  xdelivered  <- getField' "delivered"     -- true or false
  xreason     <- getField' "reason"        -- optional text message what went wrong if not delivered
  logInfo_ $ "   xtype=" ++ xtype
  logInfo_ $ "   xref=" ++ show xref
  logInfo_ $ "   xmsisdn=" ++ xmsisdn
  logInfo_ $ "   xtimestamp=" ++ xtimestamp
  logInfo_ $ "   xdelivered=" ++ xdelivered
  logInfo_ $ "   xreason=" ++ xreason

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
              logInfo_ $ "UpdateWithSMSEvent" <+> show xref' <+> show ev
              res <- dbUpdate (UpdateWithSMSEvent xref' ev)
              logInfo_ $ "UpdateWithSMSEvent" <+> show xref' <+> show ev <+> "=>" <+> show res
              ok $ toResponse "Thanks"
    Nothing -> do
              logInfo_ $ "UpdateWithSMSEvent not run due to xref=" ++ show xref
              ok $ toResponse "Could not read ShortMessageID"
