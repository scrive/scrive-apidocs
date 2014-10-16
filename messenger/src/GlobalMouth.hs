{-# LANGUAGE ExtendedDefaultRules #-}
module GlobalMouth (
    handleGlobalMouthEvents
  ) where

import Control.Exception.Lifted as E
import Happstack.Server

import DB
import Happstack.Fields
import Messenger
import SMS.Data
import SMS.Model
import qualified Log

handleGlobalMouthEvents :: Messenger Response
handleGlobalMouthEvents = flip E.catch (\(e :: SomeException) -> Log.mixlog_ (show e) >> throwIO e) $ do
  Log.mixlog_ $ "Processing some globalmounth event"
  xtype       <- getField' "type"          -- dlr, delivery report
  xref        <- readField "ref"           -- scrive reference
  xmsisdn     <- getField' "msisdn"        -- full number
  xtimestamp  <- getField' "timestamp"     -- timestamp
  xdelivered  <- getField' "delivered"     -- true or false
  xreason     <- getField' "reason"        -- optional text message what went wrong if not delivered
  Log.mixlog_ $ "   xtype=" ++ xtype
  Log.mixlog_ $ "   xref=" ++ show xref
  Log.mixlog_ $ "   xmsisdn=" ++ xmsisdn
  Log.mixlog_ $ "   xtimestamp=" ++ xtimestamp
  Log.mixlog_ $ "   xdelivered=" ++ xdelivered
  Log.mixlog_ $ "   xreason=" ++ xreason

  let event = case xdelivered of
                "true" -> SMSDelivered
                "false" -> SMSUndelivered xreason
                _ -> error "Unknown report"

  case xref of
    Just xref' -> do
              let ev = SMSEvent xmsisdn event
              Log.mixlog_ $ "UpdateWithSMSEvent " ++ show xref' ++ " " ++ show ev
              res <- dbUpdate (UpdateWithSMSEvent xref' ev)
              Log.mixlog_ $ "UpdateWithSMSEvent " ++ show xref' ++ " " ++ show ev ++ " => " ++ show res
              ok $ toResponse "Thanks"
    Nothing -> do
              Log.mixlog_ $ "UpdateWithSMSEvent not run due to xref=" ++ show xref
              ok $ toResponse "Could not read ShortMessageID"
