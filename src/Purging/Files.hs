module Purging.Files (
    MarkOrphanFilesForPurgeAfter(..)
  , purgeOrphanFile
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Log

import Amazon
import DB
import File.Conditions
import File.Model
import KontraPrelude
import Log.Identifier

data MarkOrphanFilesForPurgeAfter = MarkOrphanFilesForPurgeAfter Int Interval
instance (MonadDB m, MonadThrow m) => DBUpdate m MarkOrphanFilesForPurgeAfter [FileID] where
  update (MarkOrphanFilesForPurgeAfter limit interval) = do
    -- Check if the database still looks similar to what the code below was written for
    runSQL_ $ "SELECT table_name::text, column_name::text"
        <+> "  FROM information_schema.key_column_usage"
        <+> " WHERE constraint_name IN (SELECT constraint_name"
        <+> "                             FROM information_schema.referential_constraints"
        <+> "                            WHERE unique_constraint_name = 'pk__files')"
    refs :: [(String, String)] <- fetchMany id
    let expected_refs =
           [ ("attachments",           "file_id")
           , ("author_attachments",    "file_id")
           , ("main_files",            "file_id")
           , ("mail_attachments",      "file_id")
           , ("signatory_attachments", "file_id")
           , ("signatory_screenshots", "file_id")
           , ("signatory_link_fields", "value_file_id")
           ]

    when (sort expected_refs /= sort refs) $
      $unexpectedErrorM $ "PurgeFile: database layout has changed, update PurgeFile.expected_refs and check the code: " ++ show refs

    runSQL_ $ "UPDATE files"
        <+> "SET purge_at = now() +" <?> interval
        <+> "WHERE id IN ("
        <+> " SELECT id FROM files"
        <+> " WHERE purge_at IS NULL AND purged_time IS NULL"
                -- Case 1:"
                -- File is connected as a file to a document that is still available to somebody."
        <+> "   AND NOT EXISTS ("
        <+> "       SELECT TRUE"
        <+> "         FROM documents"
        <+> "         JOIN main_files ON main_files.document_id = documents.id"
        <+> "        WHERE files.id = main_files.file_id"
        <+> "          AND documents.purged_time IS NULL"
        <+> "       )"
                -- Case 3:"
                -- File is connected as a signatory attachment to a document that is available to somebody."
        <+> "   AND NOT EXISTS ("
        <+> "       SELECT TRUE"
        <+> "         FROM documents"
        <+> "         JOIN signatory_links ON signatory_links.document_id = documents.id"
        <+> "         JOIN signatory_attachments ON signatory_attachments.signatory_link_id = signatory_links.id"
        <+> "        WHERE signatory_attachments.file_id = files.id"
        <+> "          AND documents.purged_time IS NULL"
        <+> "       )"
                -- Case 4:"
                -- File is connected as an author attachment to a document that is available to somebody."
        <+> "   AND NOT EXISTS ("
        <+> "       SELECT TRUE"
        <+> "         FROM documents"
        <+> "         JOIN signatory_links ON signatory_links.document_id = documents.id"
        <+> "         JOIN author_attachments ON author_attachments.document_id = documents.id"
        <+> "        WHERE author_attachments.file_id = files.id"
        <+> "          AND documents.purged_time IS NULL"
        <+> "       )"
                -- Case 5:"
                -- There is an email with this file as an attachment"
        <+> "   AND NOT EXISTS ("
        <+> "       SELECT TRUE"
        <+> "         FROM mail_attachments"
        <+> "        WHERE mail_attachments.file_id = files.id"
        <+> "       )"
                -- Case 6:"
                -- There is a screenshot useful for a non-deleted document"
        <+> "   AND NOT EXISTS ("
        <+> "       SELECT TRUE"
        <+> "         FROM documents"
        <+> "         JOIN signatory_links ON signatory_links.document_id = documents.id"
        <+> "         JOIN signatory_screenshots ON signatory_screenshots.signatory_link_id = signatory_links.id"
        <+> "        WHERE signatory_screenshots.file_id = files.id"
        <+> "          AND documents.purged_time IS NULL"
        <+> "       )"
                -- Case 7:
                -- There is an attachment with this file referenced.
        <+> "   AND NOT EXISTS ("
        <+> "       SELECT TRUE"
        <+> "         FROM attachments"
        <+> "        WHERE attachments.file_id = files.id"
        <+> "          AND NOT attachments.deleted"
        <+> "       )"
                -- Case 8:
                -- There is a signature (in signatory_link_fields) with this file referenced. On document purge reference is dropped.
        <+> "   AND NOT EXISTS ("
        <+> "       SELECT TRUE"
        <+> "         FROM signatory_link_fields"
        <+> "        WHERE signatory_link_fields.value_file_id = files.id"
        <+> "       )"
        <+> "  LIMIT" <?> limit <+> "FOR UPDATE)"
        <+> "  RETURNING id"
    fetchMany runIdentity

purgeOrphanFile :: forall m. (MonadDB m, MonadThrow m, MonadLog m, MonadIO m, AmazonMonad m) => m Bool
purgeOrphanFile = do
  runQuery_ . sqlSelect "files" $ do
    sqlResult "id"
    sqlResult "amazon_bucket"
    sqlResult "amazon_url"
    sqlResult "content IS NULL"
    sqlWhereFileWasNotPurged
    sqlWhere "purge_at >= now()"
    sqlOrderBy "purge_at"
    sqlLimit 1
  fetchMaybe id >>= \case
    Nothing   -> return False
    Just file -> do
      purge file
      return True
  where
    purge :: (FileID, Maybe String, Maybe String, Bool) -> m ()
    purge (fid, mamazonBucket, mamazonUrl, isOnAmazon) = do
      purgedFromOtherSystems <- case (mamazonBucket, mamazonUrl, isOnAmazon) of
        (Just amazonBucket, Just amazonUrl, True) -> do
          conf <- getAmazonConfig
          deleteFile (mkAWSAction $ amazonConfig conf) amazonBucket amazonUrl
        _ -> return True
      if purgedFromOtherSystems
        then do
          dbUpdate $ PurgeFile fid
          commit
        else do
          logAttention "Purging file failed, it couldn't be removed from Amazon" $ object [
              identifier_ fid
            ]
          rollback
