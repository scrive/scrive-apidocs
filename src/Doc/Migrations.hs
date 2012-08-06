{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.Migrations where

import Control.Monad
import Data.Int
import Database.HDBC
import Text.JSON

import DB
import Doc.Tables
import qualified Log
import Doc.DocumentID
import Doc.DocStateData
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS
import EvidenceLog.Model
import Version
import MinutesTime

$(jsonableDeriveConvertible [t| [SignatoryField] |])

addForeignKeyToDocumentTags :: MonadDB m => Migration m
addForeignKeyToDocumentTags = Migration {
    mgrTable = tableDocumentTags
  , mgrFrom = 1
  , mgrDo = kRunRaw $ "ALTER TABLE document_tags"
      ++ " ADD CONSTRAINT fk_document_tags_document_id FOREIGN KEY(document_id)"
      ++ " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
}

deprecateDocFunctionalityCol :: MonadDB m => Migration m
deprecateDocFunctionalityCol = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 5
  , mgrDo = do
    kRunRaw "ALTER TABLE documents DROP COLUMN functionality"
}

setCascadeOnSignatoryAttachments :: MonadDB m => Migration m
setCascadeOnSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 3
  , mgrDo = do
    -- this is supposed to aid in the signatory_links renumeration step that follows
    kRunRaw $ "ALTER TABLE signatory_attachments"
              ++ " DROP CONSTRAINT fk_signatory_attachments_signatory_links,"
              ++ " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(document_id,signatory_link_id)"
              ++ " REFERENCES signatory_links(document_id,id) ON DELETE RESTRICT ON UPDATE CASCADE"
              ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

renumerateSignatoryLinkIDS :: MonadDB m => Migration m
renumerateSignatoryLinkIDS = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 6
  , mgrDo = do
    kRunRaw $ "UPDATE signatory_links"
              ++ " SET id = DEFAULT"
              ++ " FROM signatory_links AS sl2"
              ++ " WHERE signatory_links.id = sl2.id"
              ++ " AND signatory_links.document_id <>"
              ++ " sl2.document_id"
  }

dropSLForeignKeyOnSignatoryAttachments :: MonadDB m => Migration m
dropSLForeignKeyOnSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 4
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_attachments"
           ++ " DROP CONSTRAINT fk_signatory_attachments_signatory_links"
  }

setSignatoryLinksPrimaryKeyToIDOnly :: MonadDB m => Migration m
setSignatoryLinksPrimaryKeyToIDOnly = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 7
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_links"
              ++ " DROP CONSTRAINT pk_signatory_links,"
              ++ " ADD CONSTRAINT pk_signatory_links PRIMARY KEY (id)"
  }

setSignatoryAttachmentsForeignKeyToSLIDOnly :: MonadDB m => Migration m
setSignatoryAttachmentsForeignKeyToSLIDOnly = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 5
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_attachments"
      ++ " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id)"
      ++ " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
  }

dropDocumentIDColumntFromSignatoryAttachments :: MonadDB m => Migration m
dropDocumentIDColumntFromSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 6
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_attachments"
      ++ " DROP COLUMN document_id"
  }


{-
- migrate padqueue - set fk referencing signatory_links to ON UPDATE CASCADE
- migrate signatory_attachments - set fk referencing signatory_links to ON UPDATE CASCADE
- migrate signatory_links - renumerate ids (references in padqueue/signatory_attachments are properly updated if necessary)
- migrate padqueue - drop fk referencing signatory_links
- migrate signatory_attachments - drop fk referencing signatory_links
- migrate signatory_links - change primary key
- migrate padqueue - add new fk referencing signatory_links
- migrate signatory_attachments - add new fk referencing signatory_links
-}
             
moveSignatoryLinkFieldsToSeparateTable :: MonadDB m => Migration m
moveSignatoryLinkFieldsToSeparateTable = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 8
  , mgrDo = do
    _ <- kRun $ SQL "SELECT id, fields FROM signatory_links WHERE fields <> '' AND fields <> '[]'" [];
    values <- foldDB fetch []
    forM_ values $ \(slid, fields) -> do
      forM_ fields $ \field -> do
        let (xtypestr :: String, custom_name :: String, is_author_filled :: Bool) =
              case lookup "sfType" field of
                Just (JSString x_sfType) -> (fromJSString x_sfType,"",False)
                Just obj@(JSObject xcustom) ->
                  case lookup "CustomFT" (fromJSObject xcustom) of
                    Just (JSArray [JSString custname, JSBool authorfilled]) ->
                      ("CustomFT", fromJSString custname, authorfilled)
                    _ -> error $ "Custom field has unrecognized format: " ++ encode obj
                Just x -> error $ "Field type must be either string or object, found: " ++ encode x
                Nothing -> error $ "Field definition does not have sfType, whole def: " ++ encode field

            Just (JSString x_sfValue) = lookup "sfValue" field
            Just placement = lookup "sfPlacements" field
            (xtype :: Int) = case xtypestr of
                      "FirstNameFT"      -> 1
                      "LastNameFT"       -> 2
                      "CompanyFT"        -> 3
                      "PersonalNumberFT" -> 4
                      "CompanyNumberFT"  -> 5
                      "EmailFT"          -> 6
                      "CustomFT"         -> 7
                      "SignatureFT"      -> 8
                      _                  -> error $ "Unknown field type: " ++ xtypestr

        _ <- kRun $ mkSQL INSERT tableSignatoryLinkFields
           [ sql "type" xtype
           , sql "value" $ fromJSString x_sfValue
           , sql "signatory_link_id" slid
           , sql "is_author_filled" is_author_filled
           , sql "custom_name" custom_name
           , sql "placements" $ encode placement
           ]
        return ()
      return ()
    kRunRaw $ "ALTER TABLE signatory_links DROP COLUMN fields"
  }
  where
    fetch acc slid fieldsstr = (slid :: Int64, fields) : acc
      where
        Ok (JSArray arr) = decode fieldsstr
        fromJSVal (JSObject obj) = fromJSObject obj
        fromJSVal x =
          error $ "moveSignatoryLinkFieldsToSeparateTable: expected valid object, got: " ++ encode x
        fields = map fromJSVal arr

moveDocumentTagsFromDocumentsTableToDocumentTagsTable :: MonadDB m => Migration m
moveDocumentTagsFromDocumentsTableToDocumentTagsTable = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 4
  , mgrDo = do
    _ <- kRun $ SQL "SELECT id, tags FROM documents WHERE tags <> '' AND tags <> '[]'" [];
    values <- foldDB fetch []
    forM_ values $ \(docid, tags) -> do
      forM_ tags $ \tag -> do
        let Just (JSString tagname) = lookup "tagname" tag
            Just (JSString tagvalue) = lookup "tagvalue" tag
        kRun $ mkSQL INSERT tableDocumentTags
           [ sql "name" $ fromJSString tagname
           , sql "value" $ fromJSString tagvalue
           , sql "document_id" docid
           ]
      return ()
    kRunRaw $ "ALTER TABLE documents DROP COLUMN tags"
  }
  where
    fetch acc docid tagsstr = (docid :: Int64, tags) : acc
      where
        Ok (JSArray arr) = decode tagsstr
        fromJSVal (JSObject obj) = fromJSObject obj
        fromJSVal x =
          error $ "moveDocumentTagsFromDocumentsTableToDocumentTagsTable: expected {tagname:'',tagvalue:''}, got: " ++ encode x
        tags = map fromJSVal arr

updateDocumentStatusAfterRemovingAwaitingAuthor :: MonadDB m => Migration m
updateDocumentStatusAfterRemovingAwaitingAuthor = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 3
  , mgrDo = do
    -- change AwaitingAuthor to Pending
    kRunRaw "UPDATE documents SET status = 2 WHERE status = 7"
    -- update DocumentError so it has proper value
    kRunRaw "UPDATE documents SET status = 7 WHERE status = 8"
  }

removeOldSignatoryLinkIDFromCancelationReason :: MonadDB m => Migration m
removeOldSignatoryLinkIDFromCancelationReason = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 1
  , mgrDo = do
    _ <- kRun $ SQL "SELECT id, cancelation_reason FROM documents WHERE cancelation_reason LIKE ?" [toSql "{\"ELegDataMismatch%"]
    values <- foldDB fetch []
    forM_ values $ \(slid, params) -> do
      let (x : JSObject link : xs) = params
          [("unSignatoryLinkID", newlink)] = fromJSObject link
          newparams = toJSObject [("ELegDataMismatch", x : newlink : xs)]
      1 <- kRun $ SQL "UPDATE documents SET cancelation_reason = ? WHERE id = ?"
        [toSql $ encode newparams, toSql slid]
      return ()
  }
  where
    fetch acc slid reason = (slid :: Int64, params) : acc
      where
        Ok (JSObject o) = decode reason
        [("ELegDataMismatch", JSArray params)] = fromJSObject o

addColumnToRecordInternalInsertionOrder :: MonadDB m => Migration m
addColumnToRecordInternalInsertionOrder =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 2
  , mgrDo = do
      kRunRaw "CREATE SEQUENCE signatory_links_internal_insert_order_seq"
      kRunRaw $ "ALTER TABLE signatory_links"
        ++ " ADD COLUMN internal_insert_order BIGINT NOT NULL DEFAULT nextval('signatory_links_internal_insert_order_seq')"
      return ()
  }

addDocumentIdIndexOnSignatoryLinks :: MonadDB m => Migration m
addDocumentIdIndexOnSignatoryLinks =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 3
  , mgrDo = do
      kRunRaw $ "CREATE INDEX idx_signatory_links_document_id ON signatory_links(document_id)"
      return ()
  }

addIdSerialOnSignatoryLinks :: MonadDB m => Migration m
addIdSerialOnSignatoryLinks =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 4
  , mgrDo = do
      -- create the sequence
      _ <- kRunRaw $ "CREATE SEQUENCE signatory_links_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      Just n <- getOne $ SQL "SELECT setval('signatory_links_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM signatory_links))" []
      Log.debug $ "Table signatory_links has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      _ <- kRunRaw $ "ALTER TABLE signatory_links ALTER id SET DEFAULT nextval('signatory_links_id_seq')"
      return ()
  }

addIdSerialOnDocuments :: MonadDB m => Migration m
addIdSerialOnDocuments =
  Migration {
    mgrTable = tableDocuments
  , mgrFrom = 2
  , mgrDo = do
      -- create the sequence
      _ <- kRunRaw $ "CREATE SEQUENCE documents_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      Just n <- getOne $ SQL "SELECT setval('documents_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM documents))" []
      Log.debug $ "Table documents has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      _ <- kRunRaw $ "ALTER TABLE documents ALTER id SET DEFAULT nextval('documents_id_seq')"
      return ()
  }

addNameColumnInSignatoryAttachments :: MonadDB m => Migration m
addNameColumnInSignatoryAttachments =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE signatory_attachments ADD COLUMN name TEXT NOT NULL DEFAULT ''"
  }

addCSVUploadDataFromDocumentToSignatoryLink :: MonadDB m => Migration m
addCSVUploadDataFromDocumentToSignatoryLink =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw $ "ALTER TABLE signatory_links"
        ++ " ADD COLUMN csv_title TEXT NULL,"
        ++ " ADD COLUMN csv_contents TEXT NULL,"
        ++ " ADD COLUMN csv_signatory_index INTEGER NULL"
  }

addSignatoryLinkIdToSignatoryAttachment :: MonadDB m => Migration m
addSignatoryLinkIdToSignatoryAttachment =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 2
  , mgrDo = do
    kRunRaw $ "ALTER TABLE signatory_attachments"
      ++ " ADD COLUMN signatory_link_id BIGINT NOT NULL DEFAULT 0"
    -- set the new column signatory_link_id from signatory_links that have the same email and document_id
    kRunRaw $ "UPDATE signatory_attachments "
      ++ "SET signatory_link_id = sl.id "
      ++ "FROM signatory_links sl "
      ++ "WHERE sl.document_id = signatory_attachments.document_id "
      ++ "AND regexp_replace(sl.fields, '^.*EmailFT\",\"sfValue\":\"([a-zA-Z0-9@-_.]+)\".*$', E'\\\\1') = signatory_attachments.email"
    kRunRaw $ "ALTER TABLE signatory_attachments DROP CONSTRAINT pk_signatory_attachments"
    -- delete attachments which have emails and document_id that don't exist in signatory_links
    logAndDeleteBadAttachments
    kRunRaw $ "ALTER TABLE signatory_attachments DROP COLUMN email"
    kRunRaw $ "ALTER TABLE signatory_attachments ADD CONSTRAINT pk_signatory_attachments PRIMARY KEY (document_id, signatory_link_id, name)"
    kRunRaw $ "ALTER TABLE signatory_attachments"
      ++ " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id, document_id)"
      ++ " REFERENCES signatory_links(id, document_id) ON DELETE CASCADE ON UPDATE RESTRICT"
      ++ " DEFERRABLE INITIALLY IMMEDIATE"
    kRunRaw $ "CREATE INDEX idx_signatory_attachments_signatory_link_id ON signatory_attachments(signatory_link_id)"
  }
  where
    logAndDeleteBadAttachments = do
      kRunRaw $ "SELECT document_id, name, email, description FROM signatory_attachments WHERE signatory_link_id = 0"
      atts <- foldDB decoder []
      kRunRaw $ "DELETE FROM signatory_attachments WHERE signatory_link_id = 0"
      mapM_ (\(d, n, e, s) ->
        Log.debug $ "Deleted bad attachment: document_id = " ++ show d
                 ++ ", name = " ++ show n
                 ++ ", email = " ++ show e
                 ++ ", description = " ++ show s) atts
      return ()
      where
        decoder acc docid name email desc = (docid :: Int64, name :: BS.ByteString, email :: BS.ByteString, desc :: BS.ByteString) : acc


fixSignatoryLinksSwedishChars :: MonadDB m => Migration m
fixSignatoryLinksSwedishChars =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 5
  , mgrDo = do
     _ <- kRun $ SQL "SELECT id, document_id, fields FROM signatory_links" []
     sls <- foldDB decoder []
     forM_ sls $ \(sid,did,fields) -> do
       let fixedfields = fixSwedishChars fields
       when (fields /= fixedfields) $ do
         _ <- kRun $ SQL "UPDATE signatory_links SET fields = ? WHERE id = ? AND document_id = ?"
                [ toSql fixedfields
                , toSql sid
                , toSql did
                ]
         return ()

  }
    where
        decoder :: [(SignatoryLinkID, DocumentID, [SignatoryField])] ->  SignatoryLinkID ->  DocumentID -> [SignatoryField] -> [(SignatoryLinkID, DocumentID, [SignatoryField])]
        decoder !acc sid did fields = (sid,did,fields) : acc
        fixSwedishChars :: [SignatoryField] ->  [SignatoryField]
        fixSwedishChars = map fixSwedishCharsForAField
        fixSwedishCharsForAField :: SignatoryField -> SignatoryField
        fixSwedishCharsForAField f = f { sfType = fixSwedishCharsForAFieldType (sfType f)
                                       , sfValue = fixSwedishCharsForAString (sfValue f)
                                       }
        fixSwedishCharsForAFieldType (CustomFT s b) = CustomFT (fixSwedishCharsForAString s) b
        fixSwedishCharsForAFieldType a = a
        fixSwedishCharsForAString :: String -> String
        fixSwedishCharsForAString s =
          let value = BS.toString $ BSC.pack s
          in if value /= s && BS.replacement_char `notElem` value
             then value
             else s

addOCSPResponse :: MonadDB m => Migration m
addOCSPResponse =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 9
  , mgrDo = kRunRaw $ "ALTER TABLE signatory_links ADD COLUMN signinfo_ocsp_response VARCHAR NULL DEFAULT NULL"
  } 


moveAttachmentsFromDocumentsToAttachments :: MonadDB m => Migration m
moveAttachmentsFromDocumentsToAttachments =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 6
  , mgrDo = do
      inserted <- kRun $ SQL ("INSERT INTO attachments(title,file_id,deleted,shared,ctime,mtime, user_id)"
                              ++ " SELECT title, file_id, signatory_links.deleted, sharing=2, ctime, mtime, user_id"
                              ++ " FROM documents JOIN signatory_links ON document_id = documents.id AND (roles&2)<>0"
                              ++ " WHERE type = 3") []
      deleted <- kRun $ SQL ("DELETE FROM documents WHERE type = 3") []
      when (deleted /= inserted) $
           error $ "migration from documents to attachments lost some files: " ++ show inserted ++ "/" ++ show deleted
  }

removeOldDocumentLog :: MonadDB m => Migration m
removeOldDocumentLog =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 7
  , mgrDo = do
      now <- getMinutesTime
      _ <- kRun $ SQL ("INSERT INTO evidence_log(document_id,time,text,event_type,version_id)"
                              ++ " SELECT id, ?, log, ? , ? FROM documents") [toSql now ,  toSql OldDocumentHistory, toSql versionID]
      kRunRaw "ALTER TABLE documents DROP COLUMN log"
  }
