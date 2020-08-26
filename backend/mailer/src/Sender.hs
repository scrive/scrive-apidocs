module Sender (
    Sender(..)
  , createSender
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Crypto.RNG (CryptoRNG)
import Log
import System.Exit
import System.Process hiding (readProcessWithExitCode)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Text as T

import AddressUtils
import Assembler
import DB
import DB.PostgreSQL
import File.Storage
import Log.Identifier
import Log.Utils
import MailingServerConf
import Mails.Model

data Sender = Sender {
  senderName :: Text
, sendMail   :: forall m. (CryptoRNG m, MonadIO m, MonadMask m, MonadBaseControl IO m, MonadLog m, MonadFileStorage m) => Mail -> m Bool
}

instance Show Sender where
  show Sender { senderName } = T.unpack senderName

instance Eq Sender where
  Sender name _ == Sender name' _ = name == name'

createSender :: TrackedConnectionSource -> SenderConfig -> Sender
createSender cs mc = case mc of
  SMTPSender{}  -> createSMTPSender cs mc
  LocalSender{} -> createLocalSender cs mc
  NullSender    -> createNullSender

----------------------------------------

createExternalSender
  :: TrackedConnectionSource -> Text -> Text -> (Mail -> [Text]) -> Sender
createExternalSender (ConnectionSource pool) name program createArgs = Sender
  { senderName = name
  , sendMail   =
    \mail@Mail {..} -> localData [identifier mailID] $ do
      content             <- runDBT pool ts $ assembleContent mail
      (code, _, bsstderr) <- liftBase $ readProcessWithExitCode
        (T.unpack program)
        (T.unpack <$> createArgs mail)
        content
      case code of
        ExitFailure retcode -> do
          logInfo "Error while sending email, execution of external program failed"
            $ object
                [ "program" .= program
                , "code" .= retcode
                , "stderr" `equalsExternalBSL` bsstderr
                ]
          return False
        ExitSuccess -> do
          logInfo "Email sent correctly" $ logObject_ mail
          return True
  }

createSMTPSender :: TrackedConnectionSource -> SenderConfig -> Sender
createSMTPSender cs config = createExternalSender cs
                                                  (serviceName config)
                                                  "curl"
                                                  createArgs
  where
    mailRcpt addr = ["--mail-rcpt", "<" <> punyEncode (addrEmail addr) <> ">"]

    createArgs Mail { mailFrom, mailTo } =
      let
        smtpUserForThisMail = maybe
          (smtpUser config)
          smtpDedicatedUser
          (find (\du -> smtpFromDedicatedAddress du == addrEmail mailFrom)
                (smtpDedicatedUsers config)
          )
        -- this path changes HELO/EHLO hostname
        -- it is fine to be hardcoded and used on every service
        smtpAddress = smtpAddr config <> "/scrive.com"
      in
        [ "-s"
        , "-S"                   -- show no progress information but show error messages
        , "-k"
        , "--ssl"                -- use SSL but do not fret over self-signed or outdated certifcate
        , "-T"
        , "-"                    -- input from stdin. Else curl goes into interactive mode, tries to do VRFY, etc.
        ]
        <> (if T.null (smtpAccount smtpUserForThisMail)
               && T.null (smtpPassword smtpUserForThisMail)
            then
              []
            else
              [ "--user"
              , smtpAccount smtpUserForThisMail <> ":" <> smtpPassword smtpUserForThisMail
              ]
           )
        <> [smtpAddress, "--mail-from", "<" <> addrEmail mailFrom <> ">"]
        <> concatMap mailRcpt mailTo

createLocalSender :: TrackedConnectionSource -> SenderConfig -> Sender
createLocalSender (ConnectionSource pool) config = Sender
  { senderName = "localSender"
  , sendMail   = \mail@Mail {..} -> localData [identifier mailID] $ do
                   content <- runDBT pool ts $ assembleContent mail
                   let filename :: FilePath =
                         localDirectory config
                           <> "/Email-"
                           <> T.unpack (addrEmail (head mailTo))
                           <> "-"
                           <> show mailID
                           <> ".eml"
                   liftBase $ BSL.writeFile filename content
                   logInfo "Email saved to file" $ object ["file" .= filename]
                   liftBase . F.forM_ (localOpenCommand config) $ \cmd -> createProcess
                     (proc (T.unpack cmd) [filename]) { std_in  = Inherit
                                                      , std_out = Inherit
                                                      , std_err = Inherit
                                                      }
                   return True
  }

createNullSender :: Sender
createNullSender = Sender { senderName = "nullSender", sendMail = const (return True) }

----------------------------------------

ts :: TransactionSettings
ts = defaultTransactionSettings { tsPermissions = ReadOnly }
