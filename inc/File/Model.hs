module File.Model (
      module File.FileID
    , FileMovedToAWS(..)
    , GetFileByFileID(..)
    , GetFileThatShouldBeMovedToAmazon(..)
    , NewFile(..)
    , PurgeFile(..)
    ) where

import Control.Applicative
import Control.Monad
import Database.HDBC

import Crypto
import Crypto.RNG
import DB
import DB.SQL2
import File.File
import File.FileID
import File.Conditions
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA1 as SHA1
import Data.List (sort)

data GetFileByFileID = GetFileByFileID FileID
instance MonadDB m => DBQuery m GetFileByFileID File where
  query (GetFileByFileID fid) = do
    kRunAndFetch1OrThrowWhyNot fetchFilesDecoder $ sqlSelect "files" $ do
      mapM_ (sqlResult . raw) filesSelectors
      sqlWhereFileIDIs fid
      sqlWhereFileWasNotPurged

data NewFile = NewFile String Binary
instance (Applicative m, CryptoRNG m, MonadDB m) => DBUpdate m NewFile FileID where
  update (NewFile filename content) = do
    kRun_ $ sqlInsert "files" $ do
        sqlSet "name" filename
        sqlSet "content" $ content
        sqlSet "checksum" $ SHA1.hash `binApp` content
        sqlSet "size" $ BS.length $ unBinary content
        sqlResult "id"
    let fetchIDs = kFold decoder []
        decoder acc fid = fid : acc
    fetchIDs >>= exactlyOneObjectReturnedGuard

data FileMovedToAWS = FileMovedToAWS FileID String String AESConf
instance MonadDB m => DBUpdate m FileMovedToAWS () where
  update (FileMovedToAWS fid bucket url aes) =
    kRun_ $ sqlUpdate "files" $ do
        sqlSet "content" SqlNull
        sqlSet "amazon_bucket" bucket
        sqlSet "amazon_url" url
        sqlSet "aes_key" $ aesKey aes
        sqlSet "aes_iv" $ aesIV aes
        sqlWhereFileIDIs fid
        sqlWhereFileWasNotPurged

data GetFileThatShouldBeMovedToAmazon = GetFileThatShouldBeMovedToAmazon
instance MonadDB m => DBQuery m GetFileThatShouldBeMovedToAmazon (Maybe File) where
  query GetFileThatShouldBeMovedToAmazon = do
    kRun_ $ sqlSelect "files" $ do
      sqlWhere "content IS NOT NULL"
      sqlLimit 1
      mapM_ (sqlResult . raw) filesSelectors
    fetchFiles >>= oneObjectReturnedGuard

data PurgeFile = PurgeFile
instance MonadDB m => DBUpdate m PurgeFile [(FileID,Maybe String,Maybe String,Bool)] where
  update (PurgeFile) = do
    -- lets check if the database still looks similar to what the code
    -- below was written for

    kRun_ $ ("SELECT table_name, column_name" :: SQL)
        <+> "  FROM information_schema.key_column_usage"
        <+> " WHERE constraint_name IN (SELECT constraint_name"
        <+> "                             FROM information_schema.referential_constraints"
        <+> "                            WHERE unique_constraint_name = 'pk__files')"
    refs <- kFold (\acc table_name column_name -> (table_name :: String, column_name::String) : acc) []
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

    kRun_ $ ("SELECT id, amazon_bucket, amazon_url, content IS NULL" :: SQL)
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
        <+> "   LIMIT 1"
    result <- kFold (\acc id' bucket url isonamazon -> (id',bucket,url,isonamazon) : acc) []
    when (not (null result)) $ do
      kRun_ $ "UPDATE files"
          <+> "   SET purged_time = now()"
          <+> "     , content = NULL"
          <+> " WHERE files.id IN " <+> parenthesize (sqlConcatComma (map (sqlParam . (\(a,_,_,_) -> a)) result))
    return result


filesSelectors :: [RawSQL]
filesSelectors = [
    "id"
  , "name"
  , "content"
  , "amazon_bucket"
  , "amazon_url"
  , "checksum"
  , "aes_key"
  , "aes_iv"
  ]

fetchFiles :: MonadDB m => m [File]
fetchFiles = kFold fetchFilesDecoder []

fetchFilesDecoder :: [File]
                  -> FileID
                  -> String
                  -> Maybe Binary
                  -> Maybe String
                  -> Maybe String
                  -> Maybe Binary
                  -> Maybe Binary
                  -> Maybe Binary
                  -> [File]
fetchFilesDecoder acc fid fname content amazon_bucket
      amazon_url checksum maes_key maes_iv = File {
        fileid = fid
      , filename = fname
      , filestorage =
        -- Here we need to support the following cases:
        --
        -- * plain data in the database: just return content
        -- * encrypted data in the database (backward compatibility only):
        --      decrypt and return content
        -- * encrypted data in Amazon S3: return (bucket, url, aes)
        -- * invalid AES key: error out at this place
        --
        -- Binary data in database is temporary: next cron run should
        -- move it to Amazon.  Encrypted data in database is backward
        -- compatibility only: we are not going to put entrypted data
        -- anymore, but we need to handle some leftovers that may be
        -- lingering there.
        case content of
          Just (Binary mem) -> case eaes of
            Nothing          -> FileStorageMemory mem
            Just (Right aes) -> FileStorageMemory (aesDecrypt aes mem)
            Just (Left msg)  -> err msg
          Nothing -> case (amazon_bucket, amazon_url, eaes) of
            (Just bucket, Just url, Just (Right aes)) -> FileStorageAWS bucket url aes
            (Just _,      Just _,   Just (Left msg))  -> err msg
            d                                  -> error $ "Invalid AWS data for file with id = " ++ show fid ++ ": " ++ show d
      , filechecksum = unBinary `fmap` checksum
    } : acc
      where
        err :: String -> FileStorage
        err msg = error $ "File with id = " ++ show fid ++ " has invalid aes/iv pair: " ++ msg
        eaes = case (maes_key, maes_iv) of
                 (Just aes_key, Just aes_iv) -> Just $ mkAESConf (unBinary aes_key) (unBinary aes_iv)
                 _ -> Nothing
