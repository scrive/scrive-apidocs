module MailJet (
    handleMailJetEvents
  ) where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Happstack.Server
import Log
import Text.JSON
import Text.JSON.FromJSValue
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.Text as T

import DB
import Log.Identifier
import Mailer
import Mails.Model
import Text.JSON.Convert

handleMailJetEvents :: Mailer Response
handleMailJetEvents = localDomain "handleMailJetEvents" $ do
  rqVar <- rqBody <$> askRq
  rq <- liftIO $ fmap unBody <$> tryTakeMVar rqVar
  case (decode <$> BS.toString <$> rq) of
    Just (Ok (a :: JSValue)) -> do
      logInfo "JSON with event parsed" $ object [
                    "event" .= jsonToAeson a
                  ]
      processMailJetEvent a
      ok $ toResponse ("Thanks" :: String)
    _ -> do
      logAttention "Couldn't parse HTTP request" $ object [
          "request" .= show rq
        ]
      badRequest $ toResponse ("Invalid response" :: String)


processMailJetEvent :: JSValue -> Mailer ()
processMailJetEvent js = do
  withJSValue js $ do
    cid <- fromJSValueField "CustomID"
    case (T.split (== '-') <$> cid) of
      Just [messageIdMID,messageIdToken] -> case (maybeRead messageIdMID, maybeRead messageIdToken) of
        (Just mid, Just token) -> localData [identifier mid] $ do
          mmail <- dbQuery $ GetEmail mid token
          case mmail of
            Nothing -> logInfo "Email doesn't exist" $ object [
                "token" .= show token
              ]
            Just Mail{..} -> localData [identifier mailID] $ do
              mevent <- mailjetEventFromJSValueM
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
                    let ev = MailJetEvent email event
                    logInfo_ "Updating database"
                    res <- dbUpdate (UpdateWithEvent mailID ev)
                    if not res
                      then logAttention "UpdateWithEvent didn't update anything" $ object [
                          "event" .= show ev
                        ]
                      else logInfo "Event recorded" $ object [
                          "event" .= show ev
                        ]
        (mid, token) -> logInfo "Invalid id or token received" $ object [
            identifier mid
          , "token" .= fmap show token
          ]
      _ -> logInfo "Invalid CustomID received" $ object ["custom_id" .= fmap show cid ]

mailjetEventFromJSValueM :: ReaderT JSValue Mailer (Maybe MailJetEvent)
mailjetEventFromJSValueM = do
  (event :: Maybe String) <- fromJSValueField "event"
  case event of
    (Just "sent")   -> return $ Just MJ_Sent
    (Just "open")        -> return $ Just MJ_Open
    (Just "click")     -> return $ Just MJ_Click
    (Just "bounce")   -> do
      hard <- fromJSValueField "hard_bounce"
      case hard of
        Just True -> return $ Just MJ_Bounce_Hard
        _ -> return $ Just MJ_Bounce_Soft
    (Just "blocked")     -> return $ Just MJ_Blocked
    (Just "spam")     -> return $ Just MJ_Spam
    (Just "unsub")     -> return $ Just MJ_Unsub
    _ -> return Nothing
