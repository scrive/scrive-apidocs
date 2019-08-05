{-# LANGUAGE ExtendedDefaultRules #-}
module TeliaCallGuide (
    handleTeliaCallGuideEvents
  ) where

import Control.Exception.Lifted as E
import Happstack.Server
import Log

import DB
import Happstack.Fields
import Messenger
import SMS.Model
import SMS.Types

handleTeliaCallGuideEvents :: Messenger Response
handleTeliaCallGuideEvents =
  localDomain "handleTeliaCallGuideEvents" .
  flip E.catch (\(e :: SomeException) -> do
                   logInfo "Logging exception" $ object ["exception" .= show e]
                   throwIO e) $ do
  logInfo_ "Processing a Telia CallGuide event"
  -- Refer to: Telia CallGuide SMS Interface Extended Interface Specification
  -- Should be in this folder: https://drive.google.com/drive/folders/0B8akyOlg6VShRnJOYVI1N2FaNU0
  -- Otherwise search Google Drive or contact Telia for documentation

  xID                <- getField' "MessageId"
                        -- The message ID of the MT message that this
                        -- report corresponds to.
  xMSISDN            <- getField' "DestinationAddress"
                        -- The consumer’s MSISDN
  xStatusCode        <- readField "StatusCode"
                        -- Status code indicates the status of the MT message:
                        -- 0 = Delivered. 2 = Deleted (reason code applies)
  xTimestamp         <- getField' "TimeStamp"
                        -- Time indicating when the delivery report
                        -- was received by Telia. The time zone of the
                        -- timestamp is CET or CEST (with summer time
                        -- as defined for the EU).
  xOperator          <- getField' "Operator"
                        -- Optional: The name of the consumer’s mobile operator
  xReasonCode        <- getField' "ReasonCode"
                        -- Optional: Operator specific error code.
  xOperatorTimestamp <- getField' "OperatorTimestamp"
                        -- Optional: Time indicating when the report
                        -- was triggered in the operator’s SMS-C (if
                        -- provided by the operator). The time zone of
                        -- the timestamp is CET or CEST (with summer
                        -- time as defined for the EU).
  xStatusText        <- getField' "StatusText"
                        -- Optional: Placeholder for additional
                        -- information, such as a clear text
                        -- description of the status/reason.

  logInfo "Telia SMS Callback Logging information" $ object
    [ "message_id"          .= xID
    , "destination_address" .= xMSISDN
    , "status_code"         .= xStatusCode
    , "timestamp"           .= xTimestamp
    , "operator"            .= xOperator
    , "reason_code"         .= xReasonCode
    , "operator_timestamp"  .= xOperatorTimestamp
    , "status_text"         .= xStatusText
    ]

  let mEvent = case xStatusCode of
                 Just 0 -> Just $ SMSEvent xMSISDN SMSDelivered
                 Just 2 -> Just $ SMSEvent xMSISDN (SMSUndelivered xStatusText)
                 _ -> Nothing
  case mEvent of
    Just event -> do
      let updateEvent = do
            res <- dbUpdate $ UpdateWithSMSEventForTeliaID xID event
            logInfo "UpdateWithSMSEventForTeliaID returned" $ object [
                "telia_id" .= xID
              , "event"    .= show event
              , "result"   .= res
              ]
      let handleMissingSMS (e :: DBException) = do
            logInfo "UpdateWithSMSEventForTeliaID failed \
                    \(probably original SMS got purged)" $ object [
                "telia_id"  .= xID
              , "event"     .= show event
              , "exception" .= show e
              ]
      updateEvent `E.catch` handleMissingSMS
      ok $ toResponse "<DeliveryResponse ack=\"true\"/>"
    Nothing -> do
      logInfo "UpdateWithSMSEventForTeliaID not run: \
              \could not read StatusCode" $ object [
        "status_code" .= xStatusCode
        ]
      ok $ toResponse "Could not read StatusCode"
