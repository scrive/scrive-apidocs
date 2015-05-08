module SendGrid (
      handleSendGridEventsV1
    , handleSendGridEventsV3
  ) where

import Control.Arrow (second)
import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Happstack.Server
import Log
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.UTF8 as BS

import DB
import Happstack.Fields
import KontraPrelude
import Mailer
import Mails.Model
import Utils.Read

handleSendGridEventsV3 :: Mailer Response
handleSendGridEventsV3= do
  logMsg' $ "Processing some sendgrid events (new interface)"
  rqVar <- rqBody <$> askRq
  rq <- liftIO $ fmap unBody <$> tryTakeMVar rqVar
  case (decode <$> BS.toString <$> rq) of
       Just (Ok (JSArray a)) -> do
         logMsg' $ "Event json parsed : we got" <+> show (length a) <+> "events."
         mapM_ processSendGridEventV3 a
         ok $ toResponse ("Thanks" :: String)
       _ -> do
         logAttention' $ "Parse problem on" <+> show rq
         badRequest $ toResponse ("Invalid response" :: String)


processSendGridEventV3 :: JSValue -> Mailer ()
processSendGridEventV3 js = do
  withJSValue js $ do
    mmid <- join . fmap maybeRead <$> fromJSValueField "email_id"
    mmtk <- join . fmap maybeRead <$> fromJSValueField "email_token"
    case (mmid, mmtk) of
      (Just mid, Just token) -> do
        mmail <- dbQuery $ GetEmail mid token
        case mmail of
          Nothing -> logAttention' $ "Error: Email with id =" <+> show mid ++ ", token =" <+> show token <+> "doesn't exist."
          Just Mail{..} -> do
              let attrs = fromXSMTPAttrs mailXSMTPAttrs
              fields <- forM attrs $ \(name,_) -> do
                fvalue <- fromJSValueField name
                return (name, fvalue)
              if fields /= map (second Just) attrs
                then logAttention' $ "Expected X-SMTP data (" ++ show attrs ++ ") doesn't match delivered one:" <+> show fields ++ ". JSON" <+> encode js
                else do
                  mevent <- sendgridEventFromJSValueM
                  case mevent of
                    Nothing -> logAttention' $ "We could not parse event type:"  ++ encode js
                    Just event -> do
                        logMsg' $ "For email with id =" <+> show mid ++ ", token =" <+> show token <+> "we got event:" <+> show event
                        email <- fromMaybe "" <$> fromJSValueField "email"
                        category <- fromMaybe "" <$> fromJSValueField "category"
                        let ev = SendGridEvent email event category
                        logMsg' $ "Updating database"
                        res <- dbUpdate (UpdateWithEvent mailID ev)
                        if not res
                          then logAttention' $ "UpdateWithEvent didn't update anything for email" <+> show mailID <+> "event" <+> show ev
                          else logMsg' $ "Event '" ++ show event ++ "' for email" <+> show mailID <+> "recorded."
      _ -> logAttention' $ "Received event - but can't determine email:" <+> encode js
  return ()


handleSendGridEventsV1 :: Mailer Response
handleSendGridEventsV1 = do
  logMsg' $ "Processing some sendgrid event (old interface)"
  mident <- (,) <$> readField "email_id" <*> readField "email_token"
  logMsg' $ "For email:" <+> show mident
  case mident of
    (Just mid, Just token) -> do
      logMsg' $ "Getting mail for DB"
      mmail <- dbQuery $ GetEmail mid token
      logMsg' $ "Checking it"
      case mmail of
        Nothing -> logAttention' $ "Email with id =" <+> show mid ++ ", token =" <+> show token <+> "doesn't exist."
        Just Mail{..} -> do
          logMsg' $ "Checking SMTP params"
          let attrs = fromXSMTPAttrs mailXSMTPAttrs
          fields <- forM attrs $ \(name,_) -> do
            logMsg' $ "Reading field" <+> show name
            fvalue <- getField name
            logMsg' $ "Value is" <+> show fvalue
            return (name, fvalue)
          logMsg' $ "Fields are" <+> show fields
          if fields /= map (second Just) attrs
            then logAttention' $ "Expected X-SMTP data (" ++ show attrs ++ ") doesn't match delivered one:" <+> show fields
            else do
              logMsg' $ "Reading event type "
              mevent <- readEventType =<< getField "event"
              case mevent of
                Nothing -> logAttention' "No event object received"
                Just event -> do
                  logMsg' $ "Reading rest of event"
                  email <- fromMaybe "" <$> getField "email"
                  category <- fromMaybe "" <$> getField "category"
                  let ev = SendGridEvent email event category
                  logMsg' $ "Doing final update"
                  res <- dbUpdate (UpdateWithEvent mailID ev)
                  logMsg' $ if not res
                    then "UpdateWithEvent didn't update anything"
                    else "Event '" ++ show event ++ "' for email" <+> show mailID <+> "received."
    (mid, token) -> logAttention' $ "Invalid id (" ++ show mid ++ ") or token (" ++ show token ++ ") received."
  ok $ toResponse ("Thanks" :: String)

logMsg' :: (MonadLog m) => String -> m ()
logMsg' msg = logInfo_ $ "handleSendgridEvents:" <+> msg

logAttention' :: (MonadLog m) => String -> m ()
logAttention' msg = logInfo_ $ "handleSendgridEvents: Error" <+> msg

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
