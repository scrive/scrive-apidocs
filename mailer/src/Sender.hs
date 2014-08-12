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
import Crypto.RNG (CryptoRNG)
import MailingServerConf
import Mails.Model
import Utils.IO
import OurPrelude
import qualified Log
import DB
import qualified Amazon as AWS
import Data.Maybe (fromMaybe)

data Sender = Sender {
    senderName :: String
  , sendMail   :: (CryptoRNG m, MonadDB m, Log.MonadLog m, AWS.AmazonMonad m) => Mail -> m Bool
  }

instance Show Sender where
  show Sender{senderName} = senderName

instance Eq Sender where
  Sender name _ == Sender name' _ = name == name'

createSender :: SenderConfig -> Sender
createSender mc = case mc of
  SMTPSender{}   -> createSMTPSender mc
  LocalSender{}  -> createLocalSender mc
  NullSender     -> createNullSender mc

createExternalSender :: String -> String -> (Mail -> [String]) -> Sender
createExternalSender name program createargs = Sender { senderName = name, sendMail = send }
  where
    send :: (CryptoRNG m, MonadDB m, Log.MonadLog m, AWS.AmazonMonad m) => Mail -> m Bool
    send mail@Mail{..} = do
      content <- assembleContent mail
      liftIO $ do
        (code, _, bsstderr) <- readProcessWithExitCode' program (createargs mail) content
        let receivers = intercalate ", " (map addrEmail mailTo)
        case code of
          ExitFailure retcode -> do
            Log.mixlog_ $ "Error while sending email #" ++ show mailID ++ ", cannot execute " ++ program ++ " to send email (code " ++ show retcode ++ ") stderr: \n" ++ BSLU.toString bsstderr
            return False
          ExitSuccess -> do
            let subject = filter (not . (`elem` "\r\n")) mailTitle
            Log.mixlog_ $ "Email #" ++ show mailID ++ " with subject '" ++ subject ++ "' sent correctly to: " ++ receivers
            Log.mixlog_ $ unlines [
                "Subject: " ++ subject
              , "To: " ++ intercalate ", " (map addrEmail mailTo)
              , case mailReplyTo of
                     Just addr -> "Reply-To: " ++ addrEmail addr
                     Nothing   -> ""
              , "Attachments: " ++ show (length mailAttachments)
              , htmlToTxt mailContent
              ]
            return True

createSMTPSender :: SenderConfig -> Sender
createSMTPSender config = createExternalSender (serviceName config) "curl" createargs
  where
    mailRcpt addr = [
        "--mail-rcpt"
      , "<" ++ addrEmail addr ++ ">"
      ]
    createargs Mail{mailFrom, mailTo} =
      let smtpUserForThisMail = fromMaybe (smtpUser config) $ 
                                  fmap smtpDedicatedUser $ 
                                    find (\du -> smtpFromDedicatedAddress du == addrEmail mailFrom) (smtpDedicatedUsers config)
      in
      [ "-s", "-S"                   -- show no progress information but show error messages
      , "-k", "--ssl"                -- use SSL but do not fret over self-signed or outdated certifcate
      , "-T", "."                    -- input from stdin. Else curl goes into interactive mode, tries to do VRFY, etc.
      ] ++ (if null (smtpAccount smtpUserForThisMail) && null (smtpPassword smtpUserForThisMail)
           then [] else
           [ "--user"
           , smtpAccount smtpUserForThisMail ++ ":" ++ smtpPassword smtpUserForThisMail
           ]) ++
      [ smtpAddr config
      , "--mail-from", "<" ++ addrEmail mailFrom ++ ">"
      ] ++ concatMap mailRcpt mailTo

createLocalSender :: SenderConfig -> Sender
createLocalSender config = Sender { senderName = "localSender", sendMail = send }
  where
    send :: (CryptoRNG m, MonadDB m, Log.MonadLog m, AWS.AmazonMonad m) => Mail -> m Bool
    send mail@Mail{..} = do
      content <- assembleContent mail
      let filename = localDirectory config ++ "/Email-" ++ addrEmail ($(head) mailTo) ++ "-" ++ show mailID ++ ".eml"
      liftIO $ do
        BSL.writeFile filename content
        Log.mixlog_ $ "Email #" ++ show mailID ++ " saved to file " ++ filename
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

createNullSender :: SenderConfig -> Sender
createNullSender _ = Sender { senderName = "nullSender", sendMail = const (return True) }
