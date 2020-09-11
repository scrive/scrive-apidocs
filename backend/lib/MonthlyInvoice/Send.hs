module MonthlyInvoice.Send (
    sendMailWithMonthlyInvoice
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Crypto.RNG
import Log
import System.Directory
  ( getCurrentDirectory, listDirectory, removeDirectoryRecursive
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Process (CreateProcess(..), proc)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.Text as T

import Control.Monad.Trans.Instances ()
import DB
import Log.Utils
import Mails.MailsData
import Mails.SendMail
import MonthlyInvoice.Config

sendMailWithMonthlyInvoice
  :: (MonadDB m, MonadThrow m, MonadIO m, CryptoRNG m, MonadLog m)
  => Text
  -> MonthlyInvoiceConf
  -> m ()
sendMailWithMonthlyInvoice dbConfig invoiceConf = do
  currentDir <- liftIO getCurrentDirectory
  reportsDir <- liftIO $ createTempDirectory "/tmp" "monthly-report"
  let script        = currentDir </> T.unpack (scriptPath invoiceConf)
      name          = recipientName invoiceConf
      emailAddress  = recipientEmail invoiceConf
      args          = [T.unpack dbConfig, "-f", script, "-v", "report_dir=" <> reportsDir]
      createProcess = (proc "psql" args) { cwd = Just reportsDir }
  (code, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode createProcess BSL.empty
  if code == ExitSuccess
    then do
      runActuallSendout reportsDir name emailAddress
      liftIO $ removeDirectoryRecursive reportsDir
    else logAttention "Running monthly-invoice psql script has failed" $ object
      [ "exit_code" .= show code
      , "stdout" `equalsExternalBSL` stdout
      , "stderr" `equalsExternalBSL` stderr
      ]

runActuallSendout
  :: (MonadDB m, MonadThrow m, MonadIO m, CryptoRNG m, MonadLog m)
  => FilePath
  -> Text
  -> Text
  -> m ()
runActuallSendout dir name emailAddress = do
  files       <- liftIO $ listDirectory dir
  attachments <- liftIO $ mapM
    (\f -> do
      fileContent <- BS.readFile $ dir </> f
      return (T.pack f, Left fileContent)
    )
    files
  let mail = emptyMail { title       = "Monthly invoice"
                       , to          = [MailAddress name emailAddress]
                       , attachments = attachments
                       }
  scheduleEmailSendout mail
