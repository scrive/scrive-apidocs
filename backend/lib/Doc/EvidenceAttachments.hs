module Doc.EvidenceAttachments
  ( extractAttachmentsList
  , extractAttachment
  -- Exported for tests
  , extractAttachmentsListFromFileContent
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Log
import System.Exit
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Text as T

import DB (MonadDB)
import Doc.Data.MainFile
import Doc.DocStateData (Document(..), documentsealedfile)
import File.Storage (getFileIDContents)
import KontraPrelude
import Log.Utils
import Utils.Directory
import qualified Amazon as AWS

extractAttachmentsList :: (MonadLog m, MonadDB m, MonadMask m, MonadBaseControl IO m, AWS.AmazonMonad m) => Document -> m [T.Text]
extractAttachmentsList doc = do
  case mainfileid <$> documentsealedfile doc of
    Nothing -> do
      logAttention_ "No sealed file found"
      return []
    Just fid -> do
      content <- getFileIDContents fid
      extractAttachmentsListFromFileContent content

extractAttachment :: (MonadLog m, MonadDB m, MonadMask m, MonadBaseControl IO m, AWS.AmazonMonad m) => Document -> T.Text -> m (Maybe BSL.ByteString)
extractAttachment doc aname = do
  case mainfileid <$> documentsealedfile doc of
    Nothing -> do
      logAttention_ "No sealed file found"
      return Nothing
    Just fid -> do
      content <- getFileIDContents fid
      extractAttachmentFromFileContent aname content

extractAttachmentsListFromFileContent :: (MonadLog m, MonadBaseControl IO m) => BS.ByteString -> m [T.Text]
extractAttachmentsListFromFileContent content = withSystemTempDirectory' ("extract-attachments-") $ \tmppath -> do
        let tmpin = tmppath ++ "/input.pdf"
        liftBase $ BS.writeFile tmpin content
        (code, stdout, stderr) <- liftBase $ do
          readProcessWithExitCode "pdfdetach" ["-list", tmpin] (BSL.empty)
        case code of
          ExitSuccess -> do
            let list = decodePdfDetachList $ T.pack $ BSL.toString stdout
            logInfo "Extracted attachments list" $ object ["list_length" .= show (length list)]
            return list
          ExitFailure err -> do
            logAttention "Failed to extract list of attachments" $ object [
                "exit_code" .= err
              , "stderr" `equalsExternalBSL` stderr
              ]
            return []

extractAttachmentFromFileContent :: (MonadLog m, MonadBaseControl IO m) => T.Text -> BS.ByteString -> m (Maybe BSL.ByteString)
extractAttachmentFromFileContent name content = withSystemTempDirectory' ("extract-attachment-") $ \tmppath -> do
        mIndex <- extractAttachmentIndex name content
        case (mIndex) of
          Nothing -> do
            logAttention "Attachment is no in PDF" $ object ["attachment_name" .= name]
            return Nothing
          Just attachmentIndex -> do
            let tmpin =  tmppath ++ "/input.pdf"
            let outfile = tmppath ++ "/out"
            liftBase $ BS.writeFile tmpin content
            (code, _, stderr) <- liftBase $ do
              readProcessWithExitCode "pdfdetach" ["-save", show (attachmentIndex + 1) , "-o", outfile, tmpin] (BSL.empty)
            case code of
              ExitSuccess -> Just <$> (liftBase $ BSL.readFile outfile)
              ExitFailure err -> do
                logAttention "Failed to extract attachment" $ object [
                    "exit_code" .= err
                  , "attachment_name" .= name
                  , "stderr" `equalsExternalBSL` stderr
                  ]
                return Nothing

{- Output format from pdfdetach is
   Some description
   1: Name of first attachment.html
   2: Name of second attachment.html
   ....

-}
decodePdfDetachList :: T.Text -> [T.Text]
decodePdfDetachList s = decodeLines 1 $ T.lines s
  where
    decodeLines n (l:r) = let prefix = (T.pack $ show n) `T.append` ": "
                          in if (prefix `T.isPrefixOf` l)
                            then (T.drop (T.length prefix) l) : (decodeLines (n + 1) r)
                            else (decodeLines n r)
    decodeLines _ [] = []

extractAttachmentIndex :: (MonadBaseControl IO m, MonadLog m) => T.Text -> BS.ByteString -> m (Maybe Int)
extractAttachmentIndex name content = do
  list <- extractAttachmentsListFromFileContent content
  return $ elemIndex name list
