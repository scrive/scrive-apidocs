{-# LANGUAGE ExtendedDefaultRules #-}
module MailGun (
    handleMailGunEvents
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

-- Note: This function is almost the same as the one used for handling sendgrid
-- events, yet slightly different. I could abstract away some parts of them, but
-- I'm not sure if it's worth it, because in the future we can add support for
-- next mailing service which may require different handling and all will be for
-- nothing, so I would rather leave it the way it is for now.

handleMailGunEvents :: Mailer Response
handleMailGunEvents = do
  mident <- (,) <$> readField "email_id" <*> readField "email_token"
  case mident of
    (Just mid, Just token) -> do
      mmail <- dbQuery $ GetEmail mid token
      case mmail of
        Nothing -> logMsg $ "Email with id = " ++ show mid ++ ", token = " ++ show token ++ " doesn't exist."
        Just Mail{..} -> do
          let attrs = fromXSMTPAttrs mailXSMTPAttrs
          fields <- forM attrs $ \(name,_) -> do
            fvalue <- getField name
            return (name, fvalue)
          if fields /= map (second Just) attrs
            then logMsg $ "Expected X-SMTP data (" ++ show attrs ++ ") doesn't match delivered one: " ++ show fields
            else do
              mevent <- readEventType =<< getField "event"
              case mevent of
                Nothing -> logMsg "No event object received"
                Just event -> do
                  email <- fromMaybe "" <$> getField "recipient"
                  let ev = MailGunEvent email event
                  res <- dbUpdate (UpdateWithEvent mailID ev) `E.catch` \(e::SQLError) -> do
                    logMsg $ "SQLError thrown while executing UpdateWithEvent: " ++ show e
                    dbRollback
                    return False
                  logMsg $ if not res
                    then "UpdateWithEvent didn't update anything"
                    else "Event '" ++ show event ++ "' for email #" ++ show mailID ++ " received."
    (mid, token) -> logMsg $ "Invalid id (" ++ show mid ++ ") or token (" ++ show token ++ ") received."
  ok $ toResponse "Thanks"

logMsg :: String -> Mailer ()
logMsg msg = Log.mailingServer $ "handleMailGunEvents: " ++ msg

readEventType :: Maybe String -> Mailer (Maybe MailGunEvent)
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
