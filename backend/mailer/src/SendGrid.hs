module SendGrid (handleSendGridEvents) where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Happstack.Server
import Log
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.UTF8 as BS

import DB
import KontraPrelude
import Log.Identifier
import Mailer
import Mails.Model
import Text.JSON.Convert

handleSendGridEvents :: Mailer Response
handleSendGridEvents = localDomain "handleSendGridEvents" $ do
  logInfo_ "Processing sendgrid events"
  rqVar <- rqBody <$> askRq
  rq <- liftIO $ fmap unBody <$> tryTakeMVar rqVar
  case (decode <$> BS.toString <$> rq) of
    Just (Ok (JSArray a)) -> do
      logInfo "JSON with events parsed" $ object [
          "events" .= length a
        ]
      mapM_ processSendGridEvent a
      ok $ toResponse ("Thanks" :: String)
    _ -> do
      logAttention "Couldn't parse HTTP request" $ object [
          "request" .= show rq
        ]
      badRequest $ toResponse ("Invalid response" :: String)


processSendGridEvent :: JSValue -> Mailer ()
processSendGridEvent js = do
  withJSValue js $ do
    mmid <- join . fmap maybeRead <$> fromJSValueField "email_id"
    mmtk <- join . fmap maybeRead <$> fromJSValueField "email_token"
    case (mmid, mmtk) of
      (Just mid, Just token) -> localData [identifier_ mid] $ do
        mmail <- dbQuery $ GetEmail mid token
        case mmail of
          Nothing -> logAttention "Email doesn't exist" $ object [
              "token" .= show token
            ]
          Just Mail{..} -> do
            mevent <- sendgridEventFromJSValueM
            case mevent of
              Nothing -> logAttention "Couldn't parse event type" $ object [
                  "event" .= jsonToAeson js
                ]
              Just event -> do
                  logInfo "Even parsed successfully" $ object [
                      "token" .= show token
                    , "event" .= show event
                    ]
                  email <- fromMaybe "" <$> fromJSValueField "email"
                  category <- fromMaybe "" <$> fromJSValueField "category"
                  let ev = SendGridEvent email event category
                  logInfo_ "Updating database"
                  res <- dbUpdate (UpdateWithEvent mailID ev)
                  if not res
                    then logAttention "UpdateWithEvent didn't update anything" $ object [
                        "event" .= show ev
                      ]
                    else logInfo "Event recorded" $ object [
                        "event" .= show ev
                      ]
      _ -> logAttention "Received event but couldn't determine email and/or token" $ object [
          "event" .= jsonToAeson js
        , identifier_ mmid
        , "token" .= fmap show mmtk
        ]

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
                              attempt <- join <$> fmap maybeRead <$> fromJSValueField "attempt"  -- SG returns ints as strings for this field
                              return (SG_Deferred <$> response <*> (fromIntegral <$> attempt))
      (Just "spamreport")  -> return $ Just SG_SpamReport
      (Just "unsubscribe") -> return $ Just SG_Unsubscribe
      _ -> return Nothing
