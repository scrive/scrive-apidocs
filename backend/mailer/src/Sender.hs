module Sender (
    Sender(..)
  , createSender
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.List hiding (head)
import Log
import System.Exit
import System.Process hiding (readProcessWithExitCode)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified  Data.Foldable as F
import qualified Data.ByteString.Lazy as BSL

import Assembler
import Crypto.RNG (CryptoRNG)
import DB
import KontraPrelude
import Log.Identifier
import Log.Utils
import MailingServerConf
import Mails.Model
import qualified Amazon as AWS

data Sender = Sender {
  senderName :: String
, sendMail   :: forall m. (CryptoRNG m, MonadMask m, MonadBaseControl IO m, MonadLog m, AWS.AmazonMonad m) => Mail -> m Bool
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
, sendMail = \mail@Mail{..} -> localData [identifier_ mailID] $ do
  content <- runDBT cs ts $ assembleContent mail
  (code, _, bsstderr) <- liftBase $ readProcessWithExitCode program (createArgs mail) content
  case code of
    ExitFailure retcode -> do
      logInfo "Error while sending email, execution of external program failed" $ object [
          "program" .= program
        , "code" .= retcode
        , "stderr" `equalsExternalBSL` bsstderr
        ]
      return False
    ExitSuccess -> do
      let subject = filter (not . (`elem` ("\r\n"::String))) mailTitle
      logInfo "Email sent correctly" $ object [
          "subject" .= subject
        , "to" .= map addrEmail mailTo
        , "attachments" .= length mailAttachments
        , "reply-to" .= case mailReplyTo of
            Just addr -> addrEmail addr
            Nothing   -> ""
        , "content" .= htmlToTxt mailContent
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
, sendMail = \mail@Mail{..} -> localData [identifier_ mailID] $ do
  content <- runDBT cs ts $ assembleContent mail
  let filename = localDirectory config ++ "/Email-" ++ addrEmail ($head mailTo) ++ "-" ++ show mailID ++ ".eml"
  liftBase $ BSL.writeFile filename content
  logInfo "Email saved to file" $ object [
      "file" .= filename
    ]
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
  tsPermissions = ReadOnly
}
