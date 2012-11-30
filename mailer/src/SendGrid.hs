module SendGrid (
    handleSendGridEvents
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import Happstack.Server
import qualified Control.Exception.Lifted as E

import DB
import Happstack.Fields
import Mails.Model
import Mailer
import qualified Log (mailingServer)

handleSendGridEvents :: Mailer Response
handleSendGridEvents = do
  logMsg $ "Processing some sendgrid event"
  mident <- (,) <$> readField "email_id" <*> readField "email_token"
  logMsg $ "For email: " ++ show mident
  case mident of
    (Just mid, Just token) -> do
      logMsg $ "Getting mail for DB" 
      mmail <- dbQuery $ GetEmail mid token
      logMsg $ "Checking it"
      case mmail of
        Nothing -> logMsg $ "Email with id = " ++ show mid ++ ", token = " ++ show token ++ " doesn't exist."
        Just Mail{..} -> do
          logMsg $ "Checking SMTP params"
          let attrs = fromXSMTPAttrs mailXSMTPAttrs
          fields <- forM attrs $ \(name,_) -> do
            logMsg $ "Reading field " ++ show name
            fvalue <- getField name
            logMsg $ "Value is " ++ show fvalue
            return (name, fvalue)
          logMsg $ "Fields are " ++ show fields 
          if fields /= map (second Just) attrs
            then logMsg $ "Expected X-SMTP data (" ++ show attrs ++ ") doesn't match delivered one: " ++ show fields
            else do
              logMsg $ "Reading event type "
              mevent <- readEventType =<< getField "event"
              case mevent of
                Nothing -> logMsg "No event object received"
                Just event -> do
                  logMsg $ "Reading rest of event"
                  email <- fromMaybe "" <$> getField "email"
                  category <- fromMaybe "" <$> getField "category"
                  let ev = SendGridEvent email event category
                  logMsg $ "Doing final update"
                  res <- dbUpdate (UpdateWithEvent mailID ev) `E.catch` \(e::SQLError) -> do
                    logMsg $ "SQLError thrown while executing UpdateWithEvent: " ++ show e
                    dbRollback
                    return False
                  logMsg $ if not res
                    then "UpdateWithEvent didn't update anything"
                    else "Event '" ++ show event ++ "' for email #" ++ show mailID ++ " received."
    (mid, token) -> logMsg $ "Invalid id (" ++ show mid ++ ") or token (" ++ show token ++ ") received."
  ok $ toResponse ("Thanks" :: String)

logMsg :: String -> Mailer ()
logMsg msg = Log.mailingServer $ "handleSendgridEvents: " ++ msg

readEventType :: Maybe String -> Mailer (Maybe SendGridEvent)
readEventType (Just "processed") = return $ Just SG_Processed
readEventType (Just "open") = return $ Just SG_Opened
readEventType (Just "dropped") = getField "reason" >>= return . fmap SG_Dropped
readEventType (Just "deferred") = do
  response <- getField "response"
  attempt  <- readField "attempt"
  return (SG_Deferred <$> response <*> attempt)
readEventType (Just "delivered") = getField "response" >>= return . fmap SG_Delivered
readEventType (Just "bounce") = do
  status <- getField "status"
  reason <- getField "reason"
  btype  <- getField "type"
  return (SG_Bounce <$> status <*> reason <*> btype)
readEventType (Just "spamreport") = return $ Just SG_SpamReport
readEventType (Just "unsubscribe") = return $ Just SG_Unsubscribe
readEventType _ = return Nothing
