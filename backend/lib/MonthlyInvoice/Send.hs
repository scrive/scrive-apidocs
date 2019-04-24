module MonthlyInvoice.Send (
    sendMailWithMonthlyInvoice
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Crypto.RNG
import Log
import System.Directory (listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode(..))
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.Text as T

import Control.Monad.Trans.Instances ()
import DB
import Log.Utils
import Mails.MailsData
import Mails.SendMail
import MonthlyInvoice.Config

sendMailWithMonthlyInvoice :: (MonadDB m, MonadThrow m, MonadIO m, CryptoRNG m, MonadLog m) => T.Text -> MonthlyInvoiceConf -> m ()
sendMailWithMonthlyInvoice dbConfig invoiceConf = do
  let script       = scriptPath invoiceConf
      name         = recipientName invoiceConf
      emailAddress = recipientEmail invoiceConf
      reportsDir   = "monthly-report"
      args         =
        [
          T.unpack $ dbConfig
          , "-f", script
          , "-v", "report_dir=" ++ reportsDir
        ]
  (code, stdout, stderr) <- liftIO $ readProcessWithExitCode "psql" args BSL.empty
  void $ case (code == ExitSuccess) of
    False ->
      logAttention "Running monthly-invoice psql script has failed" $ object [
          "exit_code" .= show code
        , "stdout" `equalsExternalBSL` stdout
        , "stderr" `equalsExternalBSL` stderr
        ]
    True -> do
      runActuallSendout reportsDir name emailAddress
      liftIO $ removeDirectoryRecursive reportsDir

runActuallSendout
  :: (MonadDB m, MonadThrow m, MonadIO m, CryptoRNG m, MonadLog m)
  => FilePath -> String -> String
  -> m ()
runActuallSendout dir name emailAddress = do
  files <- liftIO $ listDirectory dir
  attachments <- liftIO $ sequence $ map (\f -> do
          fileContent <- BS.readFile $ dir ++ "/" ++ f
          return (f, Left fileContent)) files
  let mail = emptyMail
        {
          title       = "Monthly invoice"
        , to          = [MailAddress name emailAddress]
        , attachments = attachments
        }
  scheduleEmailSendout mail
