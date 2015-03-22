{-# LANGUAGE RecordWildCards, NoImplicitPrelude, TemplateHaskell #-}
module Sender (
    Sender(..)
  , createSender
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.List hiding (head)
import Data.Monoid
import Data.Monoid.Utils
import System.Exit
import System.Process
import qualified  Data.Foldable as F
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU

import Assembler
import Crypto.RNG (CryptoRNG)
import DB
import MailingServerConf
import Mails.Model
import OurPrelude
import Utils.IO
import qualified Amazon as AWS
import qualified Log

data Sender = Sender {
  senderName :: String
, sendMail   :: (CryptoRNG m, MonadMask m, MonadBase IO m, Log.MonadLog m, AWS.AmazonMonad m) => Mail -> m Bool
}

instance Show Sender where
  show Sender{senderName} = senderName

instance Eq Sender where
  Sender name _ == Sender name' _ = name == name'

createSender :: ConnectionSource -> SenderConfig -> Sender
createSender cs mc = case mc of
  SMTPSender{}  -> createSMTPSender cs mc
  LocalSender{} -> createLocalSender cs mc
  NullSender    -> createNullSender

----------------------------------------

createExternalSender :: ConnectionSource -> String -> String -> (Mail -> [String]) -> Sender
createExternalSender cs name program createArgs = Sender {
  senderName = name
, sendMail = \mail@Mail{..} -> do
  content <- runDBT cs ts $ assembleContent mail
  (code, _, bsstderr) <- liftBase $ readProcessWithExitCode' program (createArgs mail) content
  let receivers = intercalate ", " (map addrEmail mailTo)
  case code of
    ExitFailure retcode -> do
      Log.mixlog_ $ "Error while sending email" <+> show mailID <> ", cannot execute" <+> program <+> "to send email (code" <+> show retcode <> ") stderr:\n" <> BSLU.toString bsstderr
      return False
    ExitSuccess -> do
      let subject = filter (not . (`elem` "\r\n")) mailTitle
      Log.mixlog_ $ "Email" <+> show mailID <+> "with subject '" <> subject <> "' sent correctly to:" <+> receivers
      Log.mixlog_ $ unlines [
          "Subject:" <+> subject
        , "To:" <+> intercalate ", " (map addrEmail mailTo)
        , case mailReplyTo of
            Just addr -> "Reply-To:" <+> addrEmail addr
            Nothing   -> ""
        , "Attachments:" <+> show (length mailAttachments)
        , htmlToTxt mailContent
        ]
      return True
}

createSMTPSender :: ConnectionSource -> SenderConfig -> Sender
createSMTPSender cs config =
  createExternalSender cs (serviceName config) "curl" createArgs
  where
    mailRcpt addr = [
        "--mail-rcpt"
      , "<" ++ addrEmail addr ++ ">"
      ]

    createArgs Mail{mailFrom, mailTo} =
      let smtpUserForThisMail = fromMaybe (smtpUser config) $
                                  fmap smtpDedicatedUser $
                                    find (\du -> smtpFromDedicatedAddress du == addrEmail mailFrom) (smtpDedicatedUsers config)
      in
      [ "-s", "-S"                   -- show no progress information but show error messages
      , "-k", "--ssl"                -- use SSL but do not fret over self-signed or outdated certifcate
      , "-T", "-"                    -- input from stdin. Else curl goes into interactive mode, tries to do VRFY, etc.
      ] ++ (if null (smtpAccount smtpUserForThisMail) && null (smtpPassword smtpUserForThisMail)
           then [] else
           [ "--user"
           , smtpAccount smtpUserForThisMail ++ ":" ++ smtpPassword smtpUserForThisMail
           ]) ++
      [ smtpAddr config
      , "--mail-from", "<" ++ addrEmail mailFrom ++ ">"
      ] ++ concatMap mailRcpt mailTo

createLocalSender :: ConnectionSource -> SenderConfig -> Sender
createLocalSender cs config = Sender {
  senderName = "localSender"
, sendMail = \mail@Mail{..} -> do
  content <- runDBT cs ts $ assembleContent mail
  let filename = localDirectory config ++ "/Email-" ++ addrEmail ($head mailTo) ++ "-" ++ show mailID ++ ".eml"
  liftBase $ BSL.writeFile filename content
  Log.mixlog_ $ "Email" <+> show mailID <+> "saved to file" <+> filename
  liftBase $ F.forM_ (localOpenCommand config) $ \cmd -> createProcess (proc cmd [filename]) {
    std_in  = Inherit
  , std_out = Inherit
  , std_err = Inherit
  }
  return True
}

createNullSender :: Sender
createNullSender = Sender { senderName = "nullSender", sendMail = const (return True) }

----------------------------------------

ts :: TransactionSettings
ts = def {
  tsIsolationLevel = ReadCommitted
, tsPermissions = ReadOnly
}
