{-# LANGUAGE RecordWildCards, NoImplicitPrelude, TemplateHaskell #-}
module Sender (
    Sender(..)
  , createSender
  ) where

import Control.Monad.IO.Class
import Data.List hiding (head)
import System.Exit
import System.Process
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU

import Assembler
import MailingServerConf
import Mails.Model
import Misc
import OurPrelude
import qualified AppLogger as Log (mailingServer, mailContent)

newtype Sender = Sender { sendMail :: MonadIO m => Mail -> m Bool }

createSender :: MailsConfig -> Sender
createSender mc = case mc of
  MailsSendgrid{} -> createSendGridSender mc
  MailsSendmail   -> createSendmailSender
  MailsLocal{}    -> createLocalOpenSender mc

createExternalSender :: String -> (Mail -> [String]) -> Sender
createExternalSender program createargs = Sender { sendMail = send }
  where
    send :: MonadIO m => Mail -> m Bool
    send mail@Mail{..} = liftIO $ do
      content <- assembleContent mail
      (code, _, bsstderr) <- readProcessWithExitCode' program (createargs mail) content
      let receivers = intercalate ", " (map addrEmail mailTo)
      case code of
        ExitFailure retcode -> do
          Log.mailingServer $ "Error while sending email #" ++ show mailID ++ ", cannot execute " ++ program ++ " to send email (code " ++ show retcode ++ ") stderr: \n" ++ BSLU.toString bsstderr
          return False
        ExitSuccess -> do
          let subject = filter (/= '\n') mailTitle
          Log.mailingServer $ "Email #" ++ show mailID ++ " with subject '" ++ subject ++ "' sent correctly to: " ++ receivers
          Log.mailContent $ unlines [
              "Subject: " ++ subject
            , "To: " ++ intercalate ", " (map addrEmail mailTo)
            , "Attachments: " ++ show (length mailAttachments)
            , htmlToTxt mailContent
            ]
          return True

createSendGridSender :: MailsConfig -> Sender
createSendGridSender config = createExternalSender "curl" createargs
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

createSendmailSender :: Sender
createSendmailSender = createExternalSender "sendmail" createargs
  where
    createargs _ = [
        "-t" -- get the addresses from the content
      , "-i" -- ignore single dots in input
      ]

createLocalOpenSender :: MailsConfig -> Sender
createLocalOpenSender config = Sender { sendMail = send }
  where
    send :: MonadIO m => Mail -> m Bool
    send mail@Mail{..} = liftIO $ do
      let filename = localDirectory config ++ "/Email-" ++ addrEmail ($(head) mailTo) ++ "-" ++ show mailID ++ ".eml"
      content <- assembleContent mail
      BSL.writeFile filename content
      Log.mailingServer $ "Email #" ++ show mailID ++ " saved to file " ++ filename
      case localOpenCommand config of
        Nothing  -> return ()
        Just cmd -> do
          _ <- createProcess (proc cmd [filename]) {
              std_in  = Inherit
            , std_out = Inherit
            , std_err = Inherit
          }
          return ()
      return True
