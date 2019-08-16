{-# LANGUAGE ExtendedDefaultRules #-}
module MailGun (
    handleMailGunEvents
  ) where

import Happstack.Server
import Log
import qualified Control.Exception.Lifted as E

import DB
import Happstack.Fields
import Log.Identifier
import Mailer
import Mails.Model

-- Note: This function is almost the same as the one used for handling sendgrid
-- events, yet slightly different. I could abstract away some parts of them, but
-- I'm not sure if it's worth it, because in the future we can add support for
-- next mailing service which may require different handling and all will be for
-- nothing, so I would rather leave it the way it is for now.

handleMailGunEvents :: Mailer Response
handleMailGunEvents = localDomain "handleMailGunEvents" $ do
  logInfo_ "Processing mailgun event"
  mident <- (,) <$> readField "email_id" <*> readField "email_token"
  case mident of
    (Just mid, Just token) -> localData [identifier mid] $ do
      mmail <- dbQuery $ GetEmail mid token
      case mmail of
        Nothing -> logInfo "Email doesn't exist" $ object [
            "token" .= show token
          ]
        Just Mail{..} -> localData [identifier mailID] $ do
          eventField <- getField "event"
          mevent <- readEventType eventField
          case mevent of
            Nothing -> logInfo_ "No event object received"
            Just event -> do
              email <- fromMaybe "" <$> getField "recipient"
              let ev = MailGunEvent email event
              res <- dbUpdate (UpdateWithEvent mailID ev) `E.catch` \(e::DBException) -> do
                logInfo "DBException thrown while executing UpdateWithEvent" $ object [
                    "exception" .= show e
                  ]
                rollback
                return False
              if not res
                then logInfo_ "UpdateWithEvent didn't update anything"
                else logInfo "Event received" $ object [
                    "event" .= show event
                  ]
    (mid, token) -> logInfo "Invalid id or token received" $ object [
        identifier mid
      , "token" .= fmap show token
      ]
  ok $ toResponse "Thanks"

readEventType :: Maybe Text -> Mailer (Maybe MailGunEvent)
readEventType (Just "opened") = return $ Just MG_Opened
readEventType (Just "delivered") = return $ Just MG_Delivered
readEventType (Just "clicked") = getField "url" >>= return . fmap MG_Clicked
readEventType (Just "unsubscribed") = getField "domain" >>= return . fmap MG_Unsubscribed
readEventType (Just "complained") = getField "domain" >>= return . fmap MG_Complained
readEventType (Just "bounced") = do
  domain <- getField "domain"
  code   <- getField "code"
  err    <- getField "error"
  return (MG_Bounced <$> domain <*> code <*> err)
readEventType (Just "dropped") = getField "reason" >>= return . fmap MG_Dropped
readEventType _ = return Nothing
