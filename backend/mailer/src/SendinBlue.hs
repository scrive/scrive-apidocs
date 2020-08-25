module SendinBlue (handleSendinBlueEvents) where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Happstack.Server
import Log
import Text.JSON
import Text.JSON.FromJSValue
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.Text as T

import DB
import Log.Identifier
import Mailer
import Mails.Model

handleSendinBlueEvents :: Mailer Response
handleSendinBlueEvents = localDomain "handleSendinBlueEvents" $ do
  logInfo_ "Processing sendinblue events"
  rqVar <- rqBody <$> askRq
  rq    <- liftIO $ fmap unBody <$> tryTakeMVar rqVar
  case (decode <$> BS.toString <$> rq) of
    Just (Ok v@(JSObject callbackJSON)) -> do
      logInfo "SendinBlue JSON with events parsed" $ object ["json" .= show callbackJSON]
      withJSValue v $ do
        mevent <- sendinBlueEventFromJSValueM
        case mevent of
          Nothing ->
            logInfo "SendinBlue event object not found" $ object ["callback" .= show v]
          Just event -> do
            xMailinCustom <- fromJSValueField "X-Mailin-custom"
            case (T.splitOn "-" <$> xMailinCustom) of
              Just [messageIdMID, messageIdToken] ->
                case (maybeRead messageIdMID, maybeRead messageIdToken) of
                  (Just mid, Just token) -> localData [identifier mid] $ do
                    mmail <- dbQuery $ GetEmail mid token
                    case mmail of
                      Nothing ->
                        logInfo "Email doesn't exist" $ object ["token" .= show token]
                      Just Mail {..} -> localData [identifier mailID] $ do
                        email <- fromMaybe "" <$> fromJSValueField "email"
                        let ev = SendinBlueEvent email event
                        res <-
                          dbUpdate (UpdateWithEvent mailID ev)
                            `E.catch` \(e :: DBException) -> do
                                        logInfo
                                            "DBException thrown while executing UpdateWithEvent"
                                          $ object ["exception" .= show e]
                                        rollback
                                        return False
                        if not res
                          then logInfo_ "UpdateWithEvent didn't update anything"
                          else logInfo "SendinBlue event received and processed"
                            $ object ["event" .= show event]
                  (mid, token) -> logInfo "Invalid id or token received from SendinBlue"
                    $ object [identifier mid, "token" .= fmap show token]
              _ -> logInfo "Invliad X-Mailin-custom received from SendinBlue"
                $ object ["X-mailin-custom" .= xMailinCustom]
    _ -> do
      logAttention "Couldn't parse SendinBlue callback JSON"
        $ object ["request" .= show rq]
  ok $ toResponse ("Thanks!" :: String)

sendinBlueEventFromJSValueM :: ReaderT JSValue Mailer (Maybe SendinBlueEvent)
sendinBlueEventFromJSValueM = do
  (event :: Maybe Text) <- fromJSValueField "event"
  (reason :: Text     ) <- fromMaybe "" <$> fromJSValueField "reason"
  case event of
    (Just "request") -> return $ Just SiB_Request
    (Just "delivered") -> return $ Just SiB_Delivered
    (Just "opened") -> return $ Just SiB_Opened
    (Just "click") -> return $ Just SiB_Click
    (Just "hard_bounce") -> return . Just $ SiB_HardBounce reason
    (Just "soft_bounce") -> return . Just $ SiB_SoftBounce reason
    (Just "blocked") -> return . Just $ SiB_Blocked reason
    (Just "spam") -> return . Just $ SiB_Spam reason
    (Just "invalid_email") -> return . Just $ SiB_InvalidEmail reason
    (Just "deferred") -> return . Just $ SiB_Deferred reason
    _ -> return Nothing
