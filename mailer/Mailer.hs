{-# LANGUAGE RecordWildCards, NoImplicitPrelude, TemplateHaskell #-}
module Mailer (
    Mailer(..)
  , createMailer
  ) where

import Control.Monad.IO.Class
import Data.List hiding (head)
import System.Directory
import System.Exit
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU

import Assembler
import MailingServerConf
import Mails.Model
import Misc
import OurPrelude
import qualified AppLogger as Log (mailingServer)

newtype Mailer = Mailer { sendMail :: MonadIO m => Mail -> m Bool }

createMailer :: MailsConfig -> Mailer
createMailer mc = case mc of
  MailsSendgrid{} -> createSendGridMailer mc
  MailsSendmail   -> createSendmailMailer
  MailsLocalOpen  -> createLocalOpenMailer

createExternalMailer :: String -> (Mail -> [String]) -> Mailer
createExternalMailer program createargs = Mailer { sendMail = reallySend }
  where
    reallySend :: MonadIO m => Mail -> m Bool
    reallySend mail@Mail{..} = liftIO $ do
      content <- assembleContent mail
      (code, _, bsstderr) <- readProcessWithExitCode' program (createargs mail) content
      case code of
        ExitFailure retcode -> do
          Log.mailingServer $ "Error while sending email #" ++ show mailID ++ ", cannot execute " ++ program ++ " to send email (code " ++ show retcode ++ ") stderr: \n" ++ BSLU.toString bsstderr
          return False
        ExitSuccess -> do
          Log.mailingServer $ "Email #" ++ show mailID ++ " sent correctly to: " ++ intercalate ", " (map addrEmail mailTo)
          --Log.mailContent $
          --  "Subject: " ++ BS.toString title ++ "\n" ++
          --  "To: " ++ createMailTos mail ++ "\n" ++
          --  "Attachments: " ++ show (length attachments) ++ "\n" ++
          --  "\n" ++ htmlToTxt (BS.toString mailContent)
          return True

createSendGridMailer :: MailsConfig -> Mailer
createSendGridMailer config = createExternalMailer "curl" createargs
  where
    mailRcpt addr = [
        "--mail-rcpt"
      , "<" ++ addrEmail addr ++ ">"
      ]
    createargs Mail{mailTo} = [
        "--user"
      , sendgridUser config ++ ":" ++ sendgridPassword config
      , sendgridSMTP config
      , "-k", "--ssl"
      ] ++ concatMap mailRcpt mailTo

createSendmailMailer :: Mailer
createSendmailMailer = createExternalMailer "sendmail" createargs
  where
    createargs _ = [
        "-t" -- get the addresses from the content
      , "-i" -- ignore single dots in input
      ]

createLocalOpenMailer :: Mailer
createLocalOpenMailer = Mailer { sendMail = sendToTempFile }
  where
    sendToTempFile :: MonadIO m => Mail -> m Bool
    sendToTempFile mail@Mail{..} = liftIO $ do
      tmp <- getTemporaryDirectory
      let filename = tmp ++ "/Email-" ++ addrEmail ($(head) mailTo) ++ "-" ++ show mailID ++ ".eml"
      content <- assembleContent mail
      BSL.writeFile filename content
      Log.mailingServer $ "Email #" ++ show mailID ++ " saved to file " ++ filename
      openDocument filename
      return True
