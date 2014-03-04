
module Purging.Files
    ( FindFilesForPurging(..)
    , purgeSomeFiles
    ) where

import Control.Monad
import Data.Int
import Data.Monoid.Space

import DB
import File.FileID
import Data.List (sort)
import Amazon
import Control.Monad.IO.Class
import File.Model
import qualified Log
import Control.Concurrent

data FindFilesForPurging = FindFilesForPurging Int
instance MonadDB m => DBQuery m FindFilesForPurging [(FileID,Maybe String,Maybe String,Bool)] where
  query (FindFilesForPurging limit) = do
    -- lets check if the database still looks similar to what the code
    -- below was written for

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
           ]

    when (sort expected_refs /= sort refs) $
      error $ "PurgeFile: database layout has changed, update PurgeFile.expected_refs and check the code: " ++ show refs

    runSQL_ $ "SELECT id, amazon_bucket, amazon_url, content IS NULL"
        <+> "  FROM files"
        <+> " WHERE purged_time IS NULL"
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
        <+> "   LIMIT" <?> (fromIntegral limit :: Int32)
    fetchMany id

purgeSomeFiles :: (MonadDB m, Log.MonadLog m, MonadIO m, AmazonMonad m) => m ()
purgeSomeFiles = do
  someFiles <- dbQuery $ FindFilesForPurging 10
  mapM_ purge someFiles
  where
    purge (_id',_amazon_bucket,_amazon_url,  isonamazon) | not isonamazon = return ()
    purge (id', Just amazon_bucket, Just amazon_url, _isonamazon) = do
      conf <- getAmazonConfig
      success <- deleteFile (mkAWSAction $ amazonConfig conf) amazon_bucket amazon_url
      if success
        then do
          dbUpdate $ PurgeFile id'
          commit
        else do
          rollback
          Log.mixlog_ "Purging from Amazon failed, sleeping for 5 minutes."
          liftIO $ threadDelay $ 5 * 60 * 1000000
    purge (_id', _amazon_bucket, _amazon_url, _isonamazon) = return ()
