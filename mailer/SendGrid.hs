{-# LANGUAGE RecordWildCards #-}
module SendGrid (
    handleSendgridEvents
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Happstack.Server

import DB.Classes
import Mails.Model
import Mailer
import Misc
import qualified AppLogger as Log (mailingServer)

handleSendgridEvents :: Mailer Response
handleSendgridEvents = do
  mident <- (,) <$> readField "id" <*> readField "token"
  case mident of
    (Just mid, Just token) -> do
      mmail <- runDBQuery $ GetEmail mid token
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
                Nothing -> logMsg "No event object provided"
                Just event -> do
                  res <- runDBUpdate $ UpdateWithEvent mailID $ SendGridEvent event
                  logMsg $ if not res
                    then "UpdateWithEvent didn't update anything"
                    else "Event '" ++ show event ++ "' for email #" ++ show mailID ++ " received."
    (mid, token) -> logMsg $ "Invalid id (" ++ show mid ++ " and token (" ++ show token ++ "provided."
  ok $ toResponse "Thanks"

logMsg :: String -> Mailer ()
logMsg msg = Log.mailingServer $ "handleSendgridEvents: " ++ msg

readEventType :: Maybe String -> Mailer (Maybe SendGridEvent)
readEventType (Just "processed") = return $ Just Processed
readEventType (Just "open") = return $ Just Opened
readEventType (Just "dropped") = do
  attr <- getField "reason"
  return $ case attr of
    Just reason -> Just $ Dropped reason
    Nothing     -> Nothing
readEventType (Just "deferred") = do
  attrs <- pairMaybe <$> getField "response" <*> readField "attempt"
  return $ case attrs of
    Just (response, attempt) -> Just $ Deferred response attempt
    Nothing                  -> Nothing
readEventType (Just "delivered") = do
  attr <- getField "response"
  return $ case attr of
    Just response -> Just $ Delivered response
    Nothing       -> Nothing
readEventType (Just "bounce") = do
  attrs <- pairMaybe3 <$> getField "status" <*> getField "reason" <*> getField "type"
  return $ case attrs of
    Just (status, reason, btype) -> Just $ Bounce status reason btype
    _                            -> Nothing
readEventType _ = return Nothing
