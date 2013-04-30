{-# LANGUAGE ExtendedDefaultRules #-}
module GlobalMouth (
    handleGlobalMouthEvents
  ) where

import Happstack.Server
import Happstack.Fields
import Messenger
import qualified Log (messengerServer)
import SMS.Data
import SMS.Model
import DB
import Control.Exception.Lifted as E

handleGlobalMouthEvents :: Messenger Response
handleGlobalMouthEvents = flip E.catch (\(e :: SomeException) -> Log.messengerServer (show e) >> throwIO e) $ do
  Log.messengerServer $ "Processing some globalmounth event"
  xtype       <- getField' "type"          -- dlr, delivery report
  xref        <- readField "ref"           -- scrive reference
  xmsisdn     <- getField' "msisdn"        -- full number
  xtimestamp  <- getField' "timestamp"     -- timestamp
  xdelivered  <- getField' "delivered"     -- true or false
  xreason     <- getField' "reason"        -- optional text message what went wrong if not delivered
  Log.messengerServer $ "   xtype=" ++ xtype
  Log.messengerServer $ "   xref=" ++ show xref
  Log.messengerServer $ "   xmsisdn=" ++ xmsisdn
  Log.messengerServer $ "   xtimestamp=" ++ xtimestamp
  Log.messengerServer $ "   xdelivered=" ++ xdelivered
  Log.messengerServer $ "   xreason=" ++ xreason

  let event = case xdelivered of
                "true" -> GM_Delivered
                "false" -> GM_Undelivered xreason
                _ -> error "Unknown report"

  case xref of
    Just xref' -> do
              let ev = GlobalMouthEvent xmsisdn event
              Log.messengerServer $ "UpdateWithSMSEvent " ++ show xref' ++ " " ++ show ev
              res <- dbUpdate (UpdateWithSMSEvent xref' ev)
              Log.messengerServer $ "UpdateWithSMSEvent " ++ show xref' ++ " " ++ show ev ++ " => " ++ show res
              ok $ toResponse "Thanks"
    Nothing -> do
              Log.messengerServer $ "UpdateWithSMSEvent not run due to xref=" ++ show xref
              ok $ toResponse "Could not read ShortMessageID"

