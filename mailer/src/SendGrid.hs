module SendGrid (
      handleSendGridEventsV1
    , handleSendGridEventsV3
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Data.Maybe
import Happstack.Server
import qualified Data.ByteString.Lazy.UTF8 as BS
import Text.JSON

import DB
import Happstack.Fields
import Mails.Model
import Mailer
import qualified Log (MonadLog, mailingServer)
import Text.JSON.FromJSValue
import Utils.Read
import Control.Monad.Trans.Reader

handleSendGridEventsV3 :: Mailer Response
handleSendGridEventsV3= do
  logMsg $ "Processing some sendgrid events (new interface)"
  rqVar <- rqBody <$> askRq
  rq <- liftIO $ fmap unBody <$> tryTakeMVar rqVar
  case (decode <$> BS.toString <$> rq) of
       Just (Ok (JSArray a)) -> do
         logMsg $ "Event json parsed : we got " ++ (show $ length a) ++ " events."
         mapM_ processSendGridEventV3 a
         ok $ toResponse ("Thanks" :: String)
       _ -> do
         logError $ "Parse problem on " ++ show rq
         badRequest $ toResponse ("Invalid response" :: String)


processSendGridEventV3 :: JSValue -> Mailer ()
processSendGridEventV3 js = do
  withJSValue js $ do
    mmid <- join <$> fmap maybeRead <$> fromJSValueField "email_id"
    mmtk <- join <$> fmap maybeRead <$> fromJSValueField "email_token"
    case (mmid,mmtk) of
      (Just mid, Just token) -> do
        mmail <- dbQuery $ GetEmail mid token
        case mmail of
          Nothing -> logError $ "Error: Email with id = " ++ show mid ++ ", token = " ++ show token ++ " doesn't exist."
          Just Mail{..} -> do
              let attrs = fromXSMTPAttrs mailXSMTPAttrs
              fields <- forM attrs $ \(name,_) -> do
                fvalue <- fromJSValueField name
                return (name, fvalue)
              if fields /= map (second Just) attrs
                then logError $ "Expected X-SMTP data (" ++ show attrs ++ ") doesn't match delivered one: " ++ show fields ++ ". JSON " ++ encode js
                else do
                  mevent <- sendgridEventFromJSValueM
                  case mevent of
                    Nothing -> logError $ "We could not parse event type:"  ++ encode js
                    Just event -> do
                        logMsg $ "For email with id = " ++ show mid ++ ", token = " ++ show token ++ " we got event: " ++ show event
                        email <- fromMaybe "" <$> fromJSValueField "email"
                        category <- fromMaybe "" <$> fromJSValueField "category"
                        let ev = SendGridEvent email event category
                        logMsg $ "Updating database"
                        res <- dbUpdate (UpdateWithEvent mailID ev)
                        if not res
                          then logError $ "UpdateWithEvent didn't update anything for mail #" ++ show mailID ++ " event " ++ show ev
                          else logMsg $ "Event '" ++ show event ++ "' for email #" ++ show mailID ++ " recorded."
      _ -> logError $ "Recieved event - but can't determine mail " ++ encode js
  return ()


handleSendGridEventsV1 :: Mailer Response
handleSendGridEventsV1 = do
  logMsg $ "Processing some sendgrid event (old interface)"
  mident <- (,) <$> readField "email_id" <*> readField "email_token"
  logMsg $ "For email: " ++ show mident
  case mident of
    (Just mid, Just token) -> do
      logMsg $ "Getting mail for DB"
      mmail <- dbQuery $ GetEmail mid token
      logMsg $ "Checking it"
      case mmail of
        Nothing -> logError $ "Email with id = " ++ show mid ++ ", token = " ++ show token ++ " doesn't exist."
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
            then logError $ "Expected X-SMTP data (" ++ show attrs ++ ") doesn't match delivered one: " ++ show fields
            else do
              logMsg $ "Reading event type "
              mevent <- readEventType =<< getField "event"
              case mevent of
                Nothing -> logError "No event object received"
                Just event -> do
                  logMsg $ "Reading rest of event"
                  email <- fromMaybe "" <$> getField "email"
                  category <- fromMaybe "" <$> getField "category"
                  let ev = SendGridEvent email event category
                  logMsg $ "Doing final update"
                  res <- dbUpdate (UpdateWithEvent mailID ev)
                  logMsg $ if not res
                    then "UpdateWithEvent didn't update anything"
                    else "Event '" ++ show event ++ "' for email #" ++ show mailID ++ " received."
    (mid, token) -> logError $ "Invalid id (" ++ show mid ++ ") or token (" ++ show token ++ ") received."
  ok $ toResponse ("Thanks" :: String)

logMsg :: (Log.MonadLog m) => String -> m ()
logMsg msg = Log.mailingServer $ "handleSendgridEvents: " ++ msg

logError :: (Log.MonadLog m) => String -> m ()
logError msg = Log.mailingServer $ "handleSendgridEvents: Error " ++ msg

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


sendgridEventFromJSValueM :: ReaderT JSValue Mailer (Maybe SendGridEvent)
sendgridEventFromJSValueM = do
    (event :: Maybe String) <- fromJSValueField "event"
    case event of
      (Just "processed")   -> return $ Just SG_Processed
      (Just "open")        -> return $ Just SG_Opened
      (Just "dropped")     -> fromJSValueField "reason" >>= return . fmap SG_Dropped
      (Just "delivered")   -> fromJSValueField "response" >>= return . fmap SG_Delivered
      (Just "bounce")      -> do
                              status <- fromJSValueField "status"
                              reason <- fromJSValueField "reason"
                              btype  <- fromJSValueField "type"
                              return (SG_Bounce <$> status <*> reason <*> btype)
      (Just "deferred")    -> do
                              response <- fromJSValueField "response"
                              (attempt :: Maybe Int)  <- fromJSValueField "attempt"
                              return (SG_Deferred <$> response <*> (fromIntegral <$> attempt))
      (Just "spamreport")  -> return $ Just SG_SpamReport
      (Just "unsubscribe") -> return $ Just SG_Unsubscribe
      _ -> return Nothing
