{-# LANGUAGE ExtendedDefaultRules #-}

module SocketLabs (
    handleSocketLabsEvents
  ) where

import Happstack.Server
import Log
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T

import DB
import Happstack.Fields
import Log.Identifier
import Mailer
import MailingServerConf
import Mails.Model

handleSocketLabsEvents :: MailingServerConf -> Mailer Response
handleSocketLabsEvents conf = localDomain "handleSocketLabsEvents" $ do
  logInfo_ "Processing SocketLabs event"
  withCallbackValidation (callbackValidationsFromConfig conf) $ do
    messageId <- getField "MessageId"
    case (T.split (== '-') <$> messageId) of
      Just [messageIdMID,messageIdToken] -> case (maybeRead messageIdMID, maybeRead messageIdToken) of
        (Just mid, Just token) -> localData [identifier mid] $ do
          mmail <- dbQuery $ GetEmail mid token
          case mmail of
            Nothing -> logInfo "Email doesn't exist" $ object [
                "token" .= show token
              ]
            Just Mail{..} -> localData [identifier mailID] $ do
              typeField <- getField "Type"
              mevent <- readEventType $ typeField
              case mevent of
                Nothing -> logInfo_ "No event object received"
                Just event -> do
                  email <- fromMaybe "" <$> getField "Address"
                  let ev = SocketLabsEvent email event
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
      _ -> do
        getField "Type" >>= \case
          Just "Validation" -> logInfo_ "Validation request received"
          _ -> logInfo "Invalid MessageId received" $ object ["message_id" .= fmap show messageId ]

readEventType :: Maybe Text -> Mailer (Maybe SocketLabsEvent)
readEventType (Just "Delivered") = return $ Just SL_Delivered
readEventType (Just "Failed") = do
  ft <- readField "FailureType"
  fc <- readField "FailureCode"
  return $ SL_Failed <$> ft <*> fc
readEventType (Just "Tracking") = do
  tt <- getField "TrackingType"
  case tt of
    Just "0" -> return $ Just SL_Clicked
    Just "1" -> return $ Just SL_Opened
    Just "2" -> return $ Just SL_Unsubscribed
    _ -> return $ Nothing
readEventType (Just "Complaint") = return $ Just SL_Complained
readEventType _ = return Nothing

-- Whenever we get a callback with SocketLabs - we should check secret key,
-- and response with matching validation key
withCallbackValidation :: [CallbackValidationKeys] -> Mailer () -> Mailer Response
withCallbackValidation cvks handler = do
  msc <- getField "SecretKey"
  case msc of
    Nothing -> do
      logAttention_ "SecretKey missing for SocketLabs callback"
      badRequest $ toResponse "SecretKey missing "
    Just sc -> case findValidationKey cvks sc of
      Nothing -> do
        logAttention "SecretKey missing for SocketLabs callback" $ object [
            "secret_key" .= show sc
          ]
        badRequest $ toResponse "SecretKey invalid"
      Just validationKey -> do
       handler
       ok $ toResponse validationKey

-- Callback validation keys can located in multiple places in config file (master sender, slave sender, dedicated sended)
-- We need to get all possible validation keys, and then find a one that matches secret key of current request
callbackValidationsFromConfig :: MailingServerConf -> [CallbackValidationKeys]
callbackValidationsFromConfig conf =
  (cvFromSender (mailerMasterSender conf)) <> (concat $ maybeToList (cvFromSender <$> mailerSlaveSender conf))
  where
    cvFromSender (SMTPSender{smtpUser,smtpDedicatedUsers}) =
      catMaybes ((callbackValidationKeys smtpUser) :  (callbackValidationKeys <$> smtpDedicatedUser <$> smtpDedicatedUsers))
    cvFromSender _ = []

findValidationKey :: [CallbackValidationKeys] -> Text -> Maybe Text
findValidationKey (cvk:cvks) sc = if (callbackValidationSecretKey cvk == sc)
                                     then Just $ callbackValidationValidationKey cvk
                                     else findValidationKey cvks sc
findValidationKey _ _ = Nothing
