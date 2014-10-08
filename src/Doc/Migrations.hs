{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.Migrations where

import Control.Monad
import Control.Monad.Catch
import Data.Int
import Data.Monoid
import Data.Monoid.Space
import Data.Maybe
import Text.JSON
import Text.JSON.FromJSValue

import DB
import DB.Checks
import Doc.Tables
import qualified Log
import Doc.DocumentID
import Doc.SealStatus (SealStatus(..))
import Utils.Default
import Doc.DocStateData
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS
import EvidenceLog.Model
import Version
import Doc.SignatoryLinkID
import MinutesTime
import Control.Applicative
import Utils.Prelude

instance PQFormat [SignatoryField] where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL [SignatoryField] where
  type PQBase [SignatoryField] = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL [SignatoryField] where
  type PQDest [SignatoryField] = PQDest String
  toSQL = jsonToSQL

signatoryLinksChangeVarcharColumnsToText :: MonadDB m => Migration m
signatoryLinksChangeVarcharColumnsToText = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 23
  , mgrDo = do
    runSQL_ "ALTER TABLE signatory_links ALTER COLUMN signinfo_ocsp_response TYPE TEXT"
    runSQL_ "ALTER TABLE signatory_links ALTER COLUMN sign_redirect_url TYPE TEXT"
    runSQL_ "ALTER TABLE signatory_links ALTER COLUMN reject_redirect_url TYPE TEXT"
}

addConfirmationDeliveryMethodToSignatoryLinks :: MonadDB m => Migration m
addConfirmationDeliveryMethodToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 24
  , mgrDo = do
      runSQL_ $   "ALTER TABLE signatory_links"
              <+> "ADD COLUMN confirmation_delivery_method SMALLINT NOT NULL DEFAULT 1"

      runQuery_ $ "UPDATE signatory_links SET confirmation_delivery_method =" <?> (MobileConfirmationDelivery)
        <+> "WHERE delivery_method = " <?> MobileDelivery

      runQuery_ $ "UPDATE signatory_links SET confirmation_delivery_method =" <?> (EmailAndMobileConfirmationDelivery)
        <+> "WHERE delivery_method = " <?> EmailAndMobileDelivery
}

addSealStatusToDocument :: MonadDB m => Migration m
addSealStatusToDocument = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 23
  , mgrDo = runSQL_ "ALTER TABLE documents ADD COLUMN seal_status SMALLINT NULL"
}

setMandatoryExpirationTimeInDocument :: (MonadDB m, MonadThrow m) => Migration m
setMandatoryExpirationTimeInDocument = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 12
  , mgrDo = do
    -- Fix documents that don't have days to sign or timeout set:
    --   Pending  => 90 days to sign, timeout_time to 90 days from time of migration
    --   Draft => default days to sign
    --   All other documents => set days to sign to 0
    let pendingDaysToSign = 90
    timeout <- (pendingDaysToSign `daysAfter`) `liftM` currentTime
    runQuery_ $ "UPDATE documents SET days_to_sign =" <?> documentdaystosign defaultValue
        <+> "WHERE status =" <?> Preparation <+> "AND days_to_sign IS NULL"
    runQuery_ $ "UPDATE documents SET days_to_sign =" <?> (fromIntegral pendingDaysToSign :: Int32)
                           <+> ", timeout_time =" <?> timeout
        <+> "WHERE status =" <?> Pending <+> "AND timeout_time IS NULL"
    runSQL_ "UPDATE documents SET days_to_sign = 0 WHERE days_to_sign IS NULL"
    runSQL_ "ALTER TABLE documents ALTER days_to_sign SET NOT NULL"
}

removeDeletedFromDocuments :: MonadDB m => Migration m
removeDeletedFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 14
  , mgrDo = do
    runSQL_ "ALTER TABLE documents DROP COLUMN deleted"
}

removeSignatoryLinksInternalInsertOrder :: MonadDB m => Migration m
removeSignatoryLinksInternalInsertOrder = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 14
  , mgrDo = do
      runSQL_ $ "ALTER TABLE signatory_link_fields "
             <> "DROP CONSTRAINT fk_signatory_link_fields_signatory_links, "
             <> "ADD CONSTRAINT fk_signatory_link_fields_signatory_links FOREIGN KEY (signatory_link_id) "
             <> "REFERENCES signatory_links (id) MATCH SIMPLE "
             <> "ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY IMMEDIATE"

      runSQL_ $ "ALTER TABLE signatory_attachments "
             <> "DROP CONSTRAINT fk_signatory_attachments_signatory_links,  "
             <> "ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY (signatory_link_id)  "
             <> "REFERENCES signatory_links (id) MATCH SIMPLE  "
             <> "ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY IMMEDIATE"

      -- This gigantic recursive query has to collect all
      -- signatory_links that are problematic and need to get a new
      -- id. Signatory links is problematic either because it itself
      -- is in wrong order or because some other link after it gets
      -- new id will make this link problematic. We iterativelly find
      -- all such links and put them in the table
      -- problematic_signatory_links. Then we assign new ids to
      -- problematic_signatory_links sorted according to
      -- internal_insert_order because this is the order we want to
      -- mimic. At the very end we use this new ids for
      -- signatory_links.
      --
      -- There is no way to make it in one swoop because UPDATE has
      -- unspecified order of updates and it cannot be made
      -- deterministic. Generator nextval('') has to be used in a
      -- SELECT query.
      runSQL_ $ "WITH RECURSIVE "
             <> "problematic_signatory_links(id,internal_insert_order,document_id) AS ( "
             <> "    SELECT signatory_links.id, signatory_links.internal_insert_order, signatory_links.document_id "
             <> "      FROM signatory_links, signatory_links AS sl2 "
             <> "     WHERE sl2.document_id = signatory_links.document_id "
             <> "       AND signatory_links.id < sl2.id "
             <> "       AND signatory_links.internal_insert_order > sl2.internal_insert_order "
             <> "UNION "
             <> "    SELECT signatory_links.id, signatory_links.internal_insert_order, signatory_links.document_id "
             <> "      FROM signatory_links, problematic_signatory_links "
             <> "     WHERE problematic_signatory_links.document_id = signatory_links.document_id "
             <> "       AND signatory_links.id > problematic_signatory_links.id "
             <> "       AND signatory_links.internal_insert_order > problematic_signatory_links.internal_insert_order "
             <> ") "
             <> "UPDATE signatory_links "
             <> "   SET id = problematic_signatory_links_with_new_ids.new_id "
             <> "  FROM (SELECT sorted_problematic_signatory_links.*, nextval('signatory_links_id_seq') AS new_id "
             <> "          FROM (SELECT * "
             <> "                  FROM problematic_signatory_links "
             <> "                 ORDER BY internal_insert_order) "
             <> "                    AS sorted_problematic_signatory_links) "
             <> "            AS problematic_signatory_links_with_new_ids "
             <> " WHERE signatory_links.id = problematic_signatory_links_with_new_ids.id "

      runSQL_ $ "ALTER TABLE signatory_links "
             <> "DROP COLUMN internal_insert_order"
}

removeSignatoryRoles :: MonadDB m => Migration m
removeSignatoryRoles = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 11
  , mgrDo = do
    runSQL_ "ALTER TABLE signatory_links ADD COLUMN is_author BOOL NULL"
    runSQL_ "ALTER TABLE signatory_links ADD COLUMN is_partner BOOL NULL"
    runSQL_ $ "UPDATE signatory_links SET"
      <> "  is_author  = (roles & 2)::BOOL"
      <> ", is_partner = (roles & 1)::BOOL"
    runSQL_ "ALTER TABLE signatory_links DROP COLUMN roles"
    runSQL_ "ALTER TABLE signatory_links ALTER is_author SET NOT NULL"
    runSQL_ "ALTER TABLE signatory_links ALTER is_partner SET NOT NULL"
}

addApiCallbackUrlToDocument :: MonadDB m => Migration m
addApiCallbackUrlToDocument = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 10
  , mgrDo = runSQL_ "ALTER TABLE documents ADD COLUMN api_callback_url TEXT NULL"
}

addUnsavedDraftToDocument :: MonadDB m => Migration m
addUnsavedDraftToDocument = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 15
  , mgrDo = do
      runSQL_ "ALTER TABLE documents ADD COLUMN unsaved_draft BOOL NOT NULL DEFAULT FALSE"
      runSQL_ "UPDATE documents SET unsaved_draft = true WHERE (title ILIKE 'Namnlös%' OR title ILIKE  'Untitled%') AND type = 1 AND status = 1"
}

dropTrustWeaverReferenceFromDocuments :: MonadDB m => Migration m
dropTrustWeaverReferenceFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 16
  , mgrDo = do
      runSQL_ "ALTER TABLE documents DROP COLUMN trust_weaver_reference"
}

moveRejectionInfoFromDocumentsToSignatoryLinks :: MonadDB m => Migration m
moveRejectionInfoFromDocumentsToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 16
  , mgrDo = do
      runSQL_ $  "ALTER TABLE signatory_links"
              <+> "ADD COLUMN rejection_time    TIMESTAMPTZ,"
              <+> "ADD COLUMN rejection_reason  TEXT"
      runSQL_ $   "UPDATE signatory_links"
              <+> "   SET rejection_time = documents.rejection_time,"
              <+> "       rejection_reason = documents.rejection_reason"
              <+> "FROM documents"
              <+> "WHERE documents.rejection_signatory_link_id = signatory_links.id"
}

dropRejectionInfoFromDocuments :: MonadDB m => Migration m
dropRejectionInfoFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 17
  , mgrDo = do
      runSQL_ $ "ALTER TABLE documents"
              <+> "DROP COLUMN rejection_time,"
              <+> "DROP COLUMN rejection_reason,"
              <+> "DROP COLUMN rejection_signatory_link_id"
}

moveAuthenticationMethodFromDocumentsToSignatoryLinks :: MonadDB m => Migration m
moveAuthenticationMethodFromDocumentsToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 17
  , mgrDo = do
      runSQL_ $   "ALTER TABLE signatory_links"
              <+> "ADD COLUMN authentication_method         SMALLINT     NULL"
      runSQL_ $   "UPDATE signatory_links"
              <+> "   SET authentication_method = (SELECT authentication_method FROM documents WHERE documents.id = signatory_links.document_id)"
      runSQL_ $   "ALTER TABLE signatory_links"
              <+> "ALTER COLUMN authentication_method SET NOT NULL"
}

dropAuthenticationMethodFromDocuments :: MonadDB m => Migration m
dropAuthenticationMethodFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 18
  , mgrDo = do
      runSQL_ $ "ALTER TABLE documents"
              <+> "DROP COLUMN authentication_method"
}

moveDeliveryMethodFromDocumentsToSignatoryLinks :: MonadDB m => Migration m
moveDeliveryMethodFromDocumentsToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 19
  , mgrDo = do
      runSQL_ $   "ALTER TABLE signatory_links"
              <+> "ADD COLUMN delivery_method         SMALLINT     NULL"
      runSQL_ $   "UPDATE signatory_links"
              <+> "   SET delivery_method = (SELECT delivery_method FROM documents WHERE documents.id = signatory_links.document_id)"
      runSQL_ $   "ALTER TABLE signatory_links"
              <+> "ALTER COLUMN delivery_method SET NOT NULL"
}

dropDeliveryMethodFromDocuments :: MonadDB m => Migration m
dropDeliveryMethodFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 21
  , mgrDo = do
      runSQL_ $ "ALTER TABLE documents"
              <+> "DROP COLUMN delivery_method"
}

addObjectVersionToDocuments :: MonadDB m => Migration m
addObjectVersionToDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 22
  , mgrDo = do
      runSQL_ $   "ALTER TABLE documents"
              <+> "ADD COLUMN object_version BIGINT NOT NULL DEFAULT 0"
}


moveCancelationReasonFromDocumentsToSignatoryLinks :: (MonadDB m, Log.MonadLog m) => Migration m
moveCancelationReasonFromDocumentsToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 18
  , mgrDo = do
      runSQL_ $ "ALTER TABLE signatory_links"
              <+> "ADD COLUMN eleg_data_mismatch_message          TEXT     NULL,"
              <+> "ADD COLUMN eleg_data_mismatch_first_name       TEXT     NULL,"
              <+> "ADD COLUMN eleg_data_mismatch_last_name        TEXT     NULL,"
              <+> "ADD COLUMN eleg_data_mismatch_personal_number  TEXT     NULL"
      runSQL_ "SELECT id, cancelation_reason FROM documents WHERE cancelation_reason LIKE '%ELegDataMismatch%'"

      let fetch (docid, fieldsstr) = v
             where
               Ok value = decode fieldsstr
               v = fromJSValue1 (value :: JSValue)
               fromJSValue1 = do
                 g <- fromJSValueField "ELegDataMismatch"
                 case g of
                   Just (JSArray [ JSString message
                                 , slid
                                 , JSString first_name
                                 , JSString last_name
                                 , JSString personal_number
                                 ]) -> do
                                -- this should be array of 5 elements:
                                -- [message, slid, first_name, last_name, personal_number]
                                return ( docid
                                       , fromJust (fromJSValue slid), fromJSString message
                                       , fromJSString first_name, fromJSString last_name
                                       , fromJSString personal_number)
                   _ -> error $ "Could not parse what is in ELegDataMismatch: " ++ fieldsstr ++ ", value is " ++ show g

      values <- fetchMany fetch
      forM_ values $ \v@( did :: Int64, slid :: Int64, message :: String
                        , first_name :: String, last_name :: String
                        , personal_number :: String) -> do
        r <- runQuery . sqlUpdate "signatory_links" $ do
          sqlSet "eleg_data_mismatch_message" message
          sqlSet "eleg_data_mismatch_first_name" first_name
          sqlSet "eleg_data_mismatch_last_name" last_name
          sqlSet "eleg_data_mismatch_personal_number" personal_number
          sqlWhereEq "id" slid
          sqlWhereEq "document_id" did
        when (r /= 1) $
          Log.mixlog_ $ "Migration failed at " ++ show v
      runQuery_ $ sqlUpdate "documents" $ do
        sqlSetCmd "cancelation_reason" "NULL"
        sqlWhere "cancelation_reason = '\"ManualCancel\"'"
      runQuery_ $ sqlUpdate "documents" $ do
        sqlSetCmd "cancelation_reason" "NULL"
        sqlWhereExists $ sqlSelect "signatory_links" $ do
          sqlWhere "signatory_links.document_id = documents.id"
          sqlWhere "signatory_links.eleg_data_mismatch_message IS NOT NULL"
}

dropCancelationReasonFromDocuments :: (MonadDB m, Log.MonadLog m) => Migration m
dropCancelationReasonFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 19
  , mgrDo = do
      runQuery_ $ sqlSelect "documents" $ do
                 sqlResult "id, title, cancelation_reason"
                 sqlWhere "cancelation_reason IS NOT NULL"
      values :: [(Int64, String, String)] <- fetchMany id
      mapM_ (\(a,b,c) -> Log.mixlog_ $ "ID: " ++ show a ++ " (" ++ b ++ "): " ++ c) $ values

      --when (not (null values)) $
      --     error "There are some useful cancelation_reason fields in documents still"

      runSQL_ $ "ALTER TABLE documents"
              <+> "DROP COLUMN cancelation_reason"
}

dropMailFooterFromDocuments :: MonadDB m => Migration m
dropMailFooterFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 20
  , mgrDo = do
      runSQL_ $ "ALTER TABLE documents"
              <+> "DROP COLUMN mail_footer"
}

dropCSVSignatoryIndexFromSignatoryLinks :: MonadDB m => Migration m
dropCSVSignatoryIndexFromSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 15
  , mgrDo = do
      runSQL_ "ALTER TABLE signatory_links DROP COLUMN csv_signatory_index"
}

addSequenceOwnerToDocumentsId :: MonadDB m => Migration m
addSequenceOwnerToDocumentsId = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 11
  , mgrDo = runSQL_ "ALTER SEQUENCE documents_id_seq OWNED BY documents.id"
}

addSequenceOwnerToSignatoryLinks :: MonadDB m => Migration m
addSequenceOwnerToSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 12
  , mgrDo = do
      runSQL_ "ALTER SEQUENCE signatory_links_internal_insert_order_seq OWNED BY signatory_links.internal_insert_order"
      runSQL_ "ALTER SEQUENCE signatory_links_id_seq OWNED BY signatory_links.id"
}

removeCompanyIdFromSignatoryLinks :: MonadDB m => Migration m
removeCompanyIdFromSignatoryLinks = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 13
  , mgrDo = do
      runSQL_ "ALTER TABLE signatory_links DROP COLUMN company_id"
}

removeServiceIDFromDocuments :: MonadDB m => Migration m
removeServiceIDFromDocuments = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 9
  , mgrDo = do
    -- check if service_id field is empty for all documents
    runSQL_ "SELECT DISTINCT service_id IS NULL FROM documents"
    check <- fetchMany unSingle
    case check of
      []     -> return () -- no records, ok
      [True] -> return () -- only nulls, ok
      _      -> error "Documents have rows with non-null service_id"
    runSQL_ "ALTER TABLE documents DROP CONSTRAINT fk_documents_services"
    runSQL_ "DROP INDEX idx_documents_service_id"
    runSQL_ "ALTER TABLE documents DROP COLUMN service_id"
}

addShouldBeFilledBySenderColumnToSignatoryLinkFields :: MonadDB m => Migration m
addShouldBeFilledBySenderColumnToSignatoryLinkFields = Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 3
  , mgrDo = runSQL_ "ALTER TABLE signatory_link_fields ADD COLUMN should_be_filled_by_author BOOL NOT NULL DEFAULT FALSE"
  }

splitIdentificationTypes :: MonadDB m => Migration m
splitIdentificationTypes = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 8
  , mgrDo = do
    runSQL_ "ALTER TABLE documents ADD COLUMN authentication_method SMALLINT NULL"
    runSQL_ "ALTER TABLE documents ADD COLUMN delivery_method SMALLINT NULL"
    runQuery_ $ mconcat [
        "UPDATE documents SET"
      , "  authentication_method = (CASE WHEN allowed_id_types = 0 THEN " <?> StandardAuthentication <> " WHEN allowed_id_types = 1 THEN " <?> StandardAuthentication <> " WHEN allowed_id_types = 2 THEN " <?> ELegAuthentication <> " WHEN allowed_id_types = 4 THEN " <?> StandardAuthentication <> " END)::SMALLINT"
      , ", delivery_method = (CASE WHEN allowed_id_types = 0 THEN " <?> EmailDelivery <> " WHEN allowed_id_types = 1 THEN " <?> EmailDelivery <> " WHEN allowed_id_types = 2 THEN " <?> EmailDelivery <> " WHEN allowed_id_types = 4 THEN " <?> PadDelivery <> " END)::SMALLINT"
      ]
    runSQL_ "ALTER TABLE documents ALTER authentication_method SET NOT NULL"
    runSQL_ "ALTER TABLE documents ALTER delivery_method SET NOT NULL"
    runSQL_ "ALTER TABLE documents DROP COLUMN allowed_id_types"
}

addForeignKeyToDocumentTags :: MonadDB m => Migration m
addForeignKeyToDocumentTags = Migration {
    mgrTable = tableDocumentTags
  , mgrFrom = 1
  , mgrDo = runSQL_ $ "ALTER TABLE document_tags"
      <> " ADD CONSTRAINT fk_document_tags_document_id FOREIGN KEY(document_id)"
      <> " REFERENCES documents(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
}

deprecateDocFunctionalityCol :: MonadDB m => Migration m
deprecateDocFunctionalityCol = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 5
  , mgrDo = do
    runSQL_ "ALTER TABLE documents DROP COLUMN functionality"
}

setCascadeOnSignatoryAttachments :: MonadDB m => Migration m
setCascadeOnSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 3
  , mgrDo = do
    -- this is supposed to aid in the signatory_links renumeration step that follows
    runSQL_ $ "ALTER TABLE signatory_attachments"
              <> " DROP CONSTRAINT fk_signatory_attachments_signatory_links,"
              <> " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(document_id,signatory_link_id)"
              <> " REFERENCES signatory_links(document_id,id) ON DELETE RESTRICT ON UPDATE CASCADE"
              <> " DEFERRABLE INITIALLY IMMEDIATE"
  }

renumerateSignatoryLinkIDS :: MonadDB m => Migration m
renumerateSignatoryLinkIDS = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 6
  , mgrDo = do
    runSQL_ $ "UPDATE signatory_links"
              <> " SET id = DEFAULT"
              <> " FROM signatory_links AS sl2"
              <> " WHERE signatory_links.id = sl2.id"
              <> " AND signatory_links.document_id <>"
              <> " sl2.document_id"
  }

dropSLForeignKeyOnSignatoryAttachments :: MonadDB m => Migration m
dropSLForeignKeyOnSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 4
  , mgrDo = do
    runSQL_ $ "ALTER TABLE signatory_attachments"
           <> " DROP CONSTRAINT fk_signatory_attachments_signatory_links"
  }

setSignatoryLinksPrimaryKeyToIDOnly :: MonadDB m => Migration m
setSignatoryLinksPrimaryKeyToIDOnly = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 7
  , mgrDo = do
    runSQL_ $ "ALTER TABLE signatory_links"
              <> " DROP CONSTRAINT pk_signatory_links,"
              <> " ADD CONSTRAINT pk_signatory_links PRIMARY KEY (id)"
  }

setSignatoryAttachmentsForeignKeyToSLIDOnly :: MonadDB m => Migration m
setSignatoryAttachmentsForeignKeyToSLIDOnly = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 5
  , mgrDo = do
    runSQL_ $ "ALTER TABLE signatory_attachments"
      <> " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id)"
      <> " REFERENCES signatory_links(id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
  }

dropDocumentIDColumntFromSignatoryAttachments :: MonadDB m => Migration m
dropDocumentIDColumntFromSignatoryAttachments = Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 6
  , mgrDo = do
    runSQL_ $ "ALTER TABLE signatory_attachments"
      <> " DROP COLUMN document_id"
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
    runSQL_ "SELECT id, fields FROM signatory_links WHERE fields <> '' AND fields <> '[]'"
    values <- fetchMany fetch
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
            (xtype :: Int16) = case xtypestr of
                      "FirstNameFT"      -> 1
                      "LastNameFT"       -> 2
                      "CompanyFT"        -> 3
                      "PersonalNumberFT" -> 4
                      "CompanyNumberFT"  -> 5
                      "EmailFT"          -> 6
                      "CustomFT"         -> 7
                      "SignatureFT"      -> 8
                      _                  -> error $ "Unknown field type: " ++ xtypestr

        runQuery_ . sqlInsert "signatory_link_fields" $ do
                sqlSet "type" xtype
                sqlSet "value" $ fromJSString x_sfValue
                sqlSet "signatory_link_id" slid
                sqlSet "is_author_filled" is_author_filled
                sqlSet "custom_name" custom_name
                sqlSet "placements" $ encode placement

        return ()
      return ()
    runSQL_ $ "ALTER TABLE signatory_links DROP COLUMN fields"
  }
  where
    fetch (slid, fieldsstr) = (slid :: Int64, fields)
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
    runSQL_ "SELECT id, tags FROM documents WHERE tags <> '' AND tags <> '[]'"
    values <- fetchMany fetch
    forM_ values $ \(docid, tags) -> do
      forM_ tags $ \tag -> do
        let Just (JSString tagname) = lookup "tagname" tag
            Just (JSString tagvalue) = lookup "tagvalue" tag
        runQuery . sqlInsert "document_tags" $ do
                   sqlSet "name" $ fromJSString tagname
                   sqlSet "value" $ fromJSString tagvalue
                   sqlSet "document_id" docid
      return ()
    runSQL_ $ "ALTER TABLE documents DROP COLUMN tags"
  }
  where
    fetch (docid, tagsstr) = (docid :: Int64, tags)
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
    runSQL_ "UPDATE documents SET status = 2 WHERE status = 7"
    -- update DocumentError so it has proper value
    runSQL_ "UPDATE documents SET status = 7 WHERE status = 8"
  }

removeOldSignatoryLinkIDFromCancelationReason :: MonadDB m => Migration m
removeOldSignatoryLinkIDFromCancelationReason = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 1
  , mgrDo = do
    runSQL_ $ "SELECT id, cancelation_reason FROM documents WHERE cancelation_reason LIKE" <?> ("{\"ELegDataMismatch%"::String)
    values <- fetchMany fetch
    forM_ values $ \(slid, params) -> do
      let (x : JSObject link : xs) = params
          [("unSignatoryLinkID", newlink)] = fromJSObject link
          newparams = toJSObject [("ELegDataMismatch", x : newlink : xs)]
      1 <- runQuery $ "UPDATE documents SET cancelation_reason = " <?> encode newparams <> " WHERE id = " <?> slid
      return ()
  }
  where
    fetch (slid, reason) = (slid :: Int64, params)
      where
        Ok (JSObject o) = decode reason
        [("ELegDataMismatch", JSArray params)] = fromJSObject o

addColumnToRecordInternalInsertionOrder :: MonadDB m => Migration m
addColumnToRecordInternalInsertionOrder =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 2
  , mgrDo = do
      runSQL_ "CREATE SEQUENCE signatory_links_internal_insert_order_seq"
      runSQL_ $ "ALTER TABLE signatory_links"
        <> " ADD COLUMN internal_insert_order BIGINT NOT NULL DEFAULT nextval('signatory_links_internal_insert_order_seq')"
      return ()
  }

addDocumentIdIndexOnSignatoryLinks :: MonadDB m => Migration m
addDocumentIdIndexOnSignatoryLinks =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 3
  , mgrDo = do
      runSQL_ $ "CREATE INDEX idx_signatory_links_document_id ON signatory_links(document_id)"
      return ()
  }

addIdSerialOnSignatoryLinks :: (MonadDB m, MonadThrow m, Log.MonadLog m) => Migration m
addIdSerialOnSignatoryLinks =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 4
  , mgrDo = do
      -- create the sequence
      runSQL_ "CREATE SEQUENCE signatory_links_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      runSQL_ "SELECT setval('signatory_links_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM signatory_links))"
      n <- fetchOne unSingle
      Log.mixlog_ $ "Table signatory_links has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      runSQL_ "ALTER TABLE signatory_links ALTER id SET DEFAULT nextval('signatory_links_id_seq')"
  }

addIdSerialOnDocuments :: (MonadDB m, MonadThrow m, Log.MonadLog m) => Migration m
addIdSerialOnDocuments =
  Migration {
    mgrTable = tableDocuments
  , mgrFrom = 2
  , mgrDo = do
      -- create the sequence
      runSQL_ "CREATE SEQUENCE documents_id_seq"
      -- set start value to be one more than maximum already in the table or 1000 if table is empty
      runSQL_ "SELECT setval('documents_id_seq',(SELECT COALESCE(max(id)+1,1000) FROM documents))"
      n <- fetchOne unSingle
      Log.mixlog_ $ "Table documents has yet " ++ show (maxBound - n :: Int64) ++ " values to go"
      -- and finally attach serial default value to files.id
      runSQL_ $ "ALTER TABLE documents ALTER id SET DEFAULT nextval('documents_id_seq')"
  }

addNameColumnInSignatoryAttachments :: MonadDB m => Migration m
addNameColumnInSignatoryAttachments =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE signatory_attachments ADD COLUMN name TEXT NOT NULL DEFAULT ''"
  }

addCSVUploadDataFromDocumentToSignatoryLink :: MonadDB m => Migration m
addCSVUploadDataFromDocumentToSignatoryLink =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ $ "ALTER TABLE signatory_links"
        <> " ADD COLUMN csv_title TEXT NULL,"
        <> " ADD COLUMN csv_contents TEXT NULL,"
        <> " ADD COLUMN csv_signatory_index INTEGER NULL"
  }

addSignatoryLinkIdToSignatoryAttachment :: (MonadDB m, Log.MonadLog m) => Migration m
addSignatoryLinkIdToSignatoryAttachment =
  Migration {
    mgrTable = tableSignatoryAttachments
  , mgrFrom = 2
  , mgrDo = do
    runSQL_ $ "ALTER TABLE signatory_attachments"
      <> " ADD COLUMN signatory_link_id BIGINT NOT NULL DEFAULT 0"
    -- set the new column signatory_link_id from signatory_links that have the same email and document_id
    runSQL_ $ "UPDATE signatory_attachments "
      <> "SET signatory_link_id = sl.id "
      <> "FROM signatory_links sl "
      <> "WHERE sl.document_id = signatory_attachments.document_id "
      <> "AND regexp_replace(sl.fields, '^.*EmailFT\",\"sfValue\":\"([a-zA-Z0-9@-_.]+)\".*$', E'\\\\1') = signatory_attachments.email"
    runSQL_ $ "ALTER TABLE signatory_attachments DROP CONSTRAINT pk_signatory_attachments"
    -- delete attachments which have emails and document_id that don't exist in signatory_links
    logAndDeleteBadAttachments
    runSQL_ $ "ALTER TABLE signatory_attachments DROP COLUMN email"
    runSQL_ $ "ALTER TABLE signatory_attachments ADD CONSTRAINT pk_signatory_attachments PRIMARY KEY (document_id, signatory_link_id, name)"
    runSQL_ $ "ALTER TABLE signatory_attachments"
      <> " ADD CONSTRAINT fk_signatory_attachments_signatory_links FOREIGN KEY(signatory_link_id, document_id)"
      <> " REFERENCES signatory_links(id, document_id) ON DELETE CASCADE ON UPDATE RESTRICT"
      <> " DEFERRABLE INITIALLY IMMEDIATE"
    runSQL_ $ "CREATE INDEX idx_signatory_attachments_signatory_link_id ON signatory_attachments(signatory_link_id)"
  }
  where
    logAndDeleteBadAttachments = do
      runSQL_ $ "SELECT document_id, name, email, description FROM signatory_attachments WHERE signatory_link_id = 0"
      atts :: [(Int64, BS.ByteString, BS.ByteString, BS.ByteString)] <- fetchMany id
      runSQL_ $ "DELETE FROM signatory_attachments WHERE signatory_link_id = 0"
      mapM_ (\(d, n, e, s) ->
        Log.mixlog_ $ "Deleted bad attachment: document_id = " ++ show d
                 ++ ", name = " ++ show n
                 ++ ", email = " ++ show e
                 ++ ", description = " ++ show s) atts
      return ()

fixSignatoryLinksSwedishChars :: MonadDB m => Migration m
fixSignatoryLinksSwedishChars =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 5
  , mgrDo = do
     runSQL_ "SELECT id, document_id, fields FROM signatory_links"
     sls :: [(SignatoryLinkID, DocumentID, [SignatoryField])] <- fetchMany id
     forM_ sls $ \(sid,did,fields) -> do
       let fixedfields = fixSwedishChars fields
       when (fields /= fixedfields) $ do
         runQuery_ . sqlUpdate "signatory_links" $ do
           sqlSet "fields" fixedfields
           sqlWhereEq "id" sid
           sqlWhereEq "document_id" did
  }
    where
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
  , mgrDo = runSQL_ $ "ALTER TABLE signatory_links ADD COLUMN signinfo_ocsp_response VARCHAR NULL DEFAULT NULL"
  }

addSignRedirectURL :: MonadDB m => Migration m
addSignRedirectURL =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 10
  , mgrDo = do
      runSQL_ $ "ALTER TABLE signatory_links ADD COLUMN sign_redirect_url VARCHAR NULL DEFAULT NULL"
  }

moveAttachmentsFromDocumentsToAttachments :: (MonadDB m, Log.MonadLog m) => Migration m
moveAttachmentsFromDocumentsToAttachments =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 6
  , mgrDo = do
      inserted <- runSQL $ "INSERT INTO attachments(title,file_id,deleted,shared,ctime,mtime, user_id)"
                              <> " SELECT title, file_id, signatory_links.deleted, sharing=2, ctime, mtime, user_id"
                              <> " FROM documents JOIN signatory_links ON document_id = documents.id AND (roles&2)<>0 AND (documents.file_id IS NOT NULL)"
                              <> " WHERE type = 3"
      deleted <- runSQL "DELETE FROM documents WHERE type = 3"
      when (deleted /= inserted) $
         Log.mixlog_  $ "Migration from documents to attachments done. Migrated: " ++ show inserted ++ ". Lost attachments due to missing files: " ++ show (deleted - inserted)
  }

removeOldDocumentLog :: (MonadDB m, MonadThrow m) => Migration m
removeOldDocumentLog =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 7
  , mgrDo = do
      now <- currentTime
      runSQL_ $ "INSERT INTO evidence_log(document_id,time,text,event_type,version_id)"
        <> " SELECT id, " <?> now <> ", log, " <?> Obsolete OldDocumentHistory <> ", " <?> versionID <> " FROM documents"
      runSQL_ "ALTER TABLE documents DROP COLUMN log"
  }

changeRegionToLang :: MonadDB m => Migration m
changeRegionToLang =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 13
  , mgrDo = runSQL_ "ALTER TABLE documents RENAME COLUMN region TO lang"
  }

removeStatsTables :: MonadDB m => Migration m
removeStatsTables =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 24
  , mgrDo = runSQL_ "DROP TABLE doc_stat_events, sign_stat_events, user_stat_events CASCADE"
  }

removeProcessFromDocuments :: MonadDB m => Migration m
removeProcessFromDocuments =
  Migration
  { mgrTable = tableDocuments
  , mgrFrom = 25
  , mgrDo = runSQL_ "ALTER TABLE documents DROP COLUMN process"
  }

moveBinaryDataForSignatoryScreenshotsToFilesTable :: (MonadDB m, Log.MonadLog m) => Migration m
moveBinaryDataForSignatoryScreenshotsToFilesTable =
  Migration
  { mgrTable = tableSignatoryScreenshots
  , mgrFrom = 1
  , mgrDo = do
      runSQL_ "ALTER TABLE signatory_screenshots DROP COLUMN mimetype"
      runSQL_ "ALTER TABLE signatory_screenshots ADD COLUMN file_id BIGINT"
      Log.mixlog_ $ "This is a long running migration with O(n^2) complexity. Please wait!"
      runSQL_ "CREATE INDEX ON signatory_screenshots((digest(image,'sha1')))"
      filesInserted <- runQuery . sqlInsertSelect "files" "signatory_screenshots" $ do
          sqlSetCmd "content" "signatory_screenshots.image"
          sqlSetCmd "name" "signatory_screenshots.type || '_screenshot.jpeg'"
          sqlSetCmd "size" "octet_length(signatory_screenshots.image)"
          sqlSetCmd "checksum" "digest(signatory_screenshots.image,'sha1')"
          sqlDistinct
      screenshotsUpdated <- runQuery . sqlUpdate "signatory_screenshots" $ do

        sqlSetCmd "file_id" "(SELECT id FROM files WHERE content = signatory_screenshots.image AND name=signatory_screenshots.type || '_screenshot.jpeg' LIMIT 1)"

      runSQL_ "ALTER TABLE signatory_screenshots DROP COLUMN image"
      Log.mixlog_ $ "Moved " ++ show screenshotsUpdated ++ " into " ++ show filesInserted ++ " files (removing duplicates)"
  }

migrateSignatoryLinksDeletedTime :: MonadDB m => Migration m
migrateSignatoryLinksDeletedTime =
  Migration {
      mgrTable = tableSignatoryLinks
    , mgrFrom = 20
    , mgrDo = do
       _ <- runSQL_ $ "ALTER TABLE signatory_links"
                  <+> "ALTER deleted DROP NOT NULL,"
                  <+> "ALTER deleted DROP DEFAULT,"
                  <+> "ALTER deleted TYPE TIMESTAMPTZ USING (CASE WHEN deleted THEN now() ELSE NULL END),"
                  <+> "ALTER really_deleted DROP NOT NULL,"
                  <+> "ALTER really_deleted DROP DEFAULT,"
                  <+> "ALTER really_deleted TYPE TIMESTAMPTZ USING (CASE WHEN really_deleted THEN now() ELSE NULL END)"
       return ()
    }

createMainFilesTable :: MonadDB m => Migration m
createMainFilesTable =
  Migration {
      mgrTable = tableMainFiles
    , mgrFrom = 0
    , mgrDo = do
        createTable $ tblTable {
                        tblName = "main_files"
                      , tblVersion = 1
                      , tblColumns = [
                         tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
                        , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
                        , tblColumn { colName = "file_id", colType = BigIntT, colNullable = False }
                        , tblColumn { colName = "document_status", colType = SmallIntT, colNullable = False }
                        , tblColumn { colName = "seal_status", colType = SmallIntT }
                        ]
                      , tblPrimaryKey = pkOnColumn "id"
                      , tblForeignKeys = [
                         (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
                        , fkOnColumn "file_id" "files" "id"
                        ]
                      , tblIndexes = [
                         indexOnColumn "document_id"
                        , indexOnColumn "file_id"
                        ]
                      }
}

migrateSeparateDeliveryStatuses :: MonadDB m => Migration m
migrateSeparateDeliveryStatuses =
  Migration {
      mgrTable = tableSignatoryLinks
    , mgrFrom = 21
    , mgrDo = do
       _ <- runSQL_ $ "ALTER TABLE signatory_links ADD COLUMN mail_invitation_delivery_status SMALLINT NOT NULL DEFAULT 3"
       _ <- runSQL_ $ "ALTER TABLE signatory_links ADD COLUMN sms_invitation_delivery_status  SMALLINT NOT NULL DEFAULT 3"
       _ <- runSQL_ $ "UPDATE signatory_links "
             <> "      SET mail_invitation_delivery_status = invitation_delivery_status "
             <> "      WHERE delivery_method = 1 OR delivery_method = 5"
       _ <- runSQL_ $ "UPDATE signatory_links "
             <> "      SET sms_invitation_delivery_status = invitation_delivery_status "
             <> "      WHERE delivery_method = 4 OR delivery_method = 5"
       _ <- runSQL_ $ "ALTER TABLE signatory_links DROP COLUMN invitation_delivery_status"
       return ()
    }

removeCSVStuffFromDocuments  :: MonadDB m => Migration m
removeCSVStuffFromDocuments =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 26
    , mgrDo = do
       _ <- runSQL_ $ "ALTER TABLE documents DROP COLUMN csv_title, DROP COLUMN csv_contents, DROP COLUMN csv_signatory_index"
       return ()
    }

migrateDocumentsAddPurgedTime :: MonadDB m => Migration m
migrateDocumentsAddPurgedTime =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 27
    , mgrDo = do
       _ <- runSQL_ $ "ALTER TABLE documents"
                  <+> "ADD COLUMN purged_time TIMESTAMPTZ"
       return ()
    }

addRejectRedirectURL :: MonadDB m => Migration m
addRejectRedirectURL =
  Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 22
  , mgrDo = do
      runSQL_ $ "ALTER TABLE signatory_links ADD COLUMN reject_redirect_url VARCHAR NULL DEFAULT NULL"
  }

migrateDocumentsMoveFilesToMainFilesTable :: (MonadDB m, MonadThrow m) => Migration m
migrateDocumentsMoveFilesToMainFilesTable =
  Migration
    { mgrTable = tableDocuments
    , mgrFrom = 28
    , mgrDo = do
        docsWithFile <- runQuery . sqlInsertSelect "main_files" "documents" $ do
          sqlSetCmd "document_id" "documents.id"
          sqlSetCmd "file_id" "documents.file_id"
          sqlSet "document_status" Preparation
          --sqlSetCmd "seal_status" "NULL"
          sqlWhere "documents.file_id IS NOT NULL"
        docsWithSealedFile <- runQuery . sqlInsertSelect "main_files" "documents" $ do
          sqlSetCmd "document_id" "documents.id"
          sqlSetCmd "file_id" "documents.sealed_file_id"
          sqlSet "document_status" Closed
          sqlSetCmd "seal_status" "documents.seal_status"
          sqlWhere "documents.sealed_file_id IS NOT NULL"

        -- Sanity checks: for each documents.file_id, there should be
        -- a matching element in main_files, and similar for documents.sealed_file_id

        runQuery_ . sqlSelect "main_files" $ do
          sqlResult "count(*)"
          sqlWhereEq "document_status" Preparation
          sqlWhereExists $ sqlSelect "documents" $ do
            sqlWhere "documents.id = main_files.document_id"
            sqlWhere "documents.file_id = main_files.file_id"
        migratedFiles <- (fromIntegral :: Int64 -> Int) <$> fetchOne unSingle
        runQuery_ . sqlSelect "main_files" $ do
          sqlResult "count(*)"
          sqlWhereEq "document_status" Closed
          sqlWhereExists $ sqlSelect "documents" $ do
            sqlWhere "documents.id = main_files.document_id"
            sqlWhere "documents.sealed_file_id = main_files.file_id"
            sqlWhere "documents.seal_status IS NOT DISTINCT FROM main_files.seal_status"
        migratedSealedFiles <- (fromIntegral :: Int64 -> Int) <$> fetchOne unSingle
        when (migratedFiles /= docsWithFile) $ do
          fail $ "Doc.Migrations.migrateDocumentsRemoveFiles: #migratedFiles is not #docsWithFile: " ++ show (migratedFiles, docsWithFile)
        when (migratedSealedFiles /= docsWithSealedFile) $ do
          fail $ "Doc.Migrations.migrateDocumentsRemoveFiles: #migratedSealedFiles is not #docsWithSealedFile: " ++ show (migratedSealedFiles, docsWithSealedFile)

        -- Drop redundant columns

        runSQL_ $ "ALTER TABLE documents DROP COLUMN file_id"
        runSQL_ $ "ALTER TABLE documents DROP COLUMN sealed_file_id"
        runSQL_ $ "ALTER TABLE documents DROP COLUMN seal_status"
    }

migrateDocumentsAddDaysToRemind :: MonadDB m => Migration m
migrateDocumentsAddDaysToRemind =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 29
    , mgrDo = do
       _ <- runSQL_ $ "ALTER TABLE documents"
                  <+> "ADD COLUMN days_to_remind INTEGER NULL DEFAULT NULL"
       return ()
    }

migrateDocumentsAddSignviewSettings :: MonadDB m => Migration m
migrateDocumentsAddSignviewSettings =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 30
    , mgrDo = do
        runSQL_ "ALTER TABLE documents ADD COLUMN show_header BOOL NOT NULL DEFAULT TRUE"
        runSQL_ "ALTER TABLE documents ADD COLUMN show_pdf_download BOOL NOT NULL DEFAULT TRUE"
        runSQL_ "ALTER TABLE documents ADD COLUMN show_reject_option BOOL NOT NULL DEFAULT TRUE"
        runSQL_ "ALTER TABLE documents ADD COLUMN show_footer BOOL NOT NULL DEFAULT TRUE"

        -- PadDelivery used to mean "don't show reject option, don't show pdf download",
        -- but this is now decoupled.
        runQuery_ $ sqlUpdate "documents" $ do
          sqlSet "show_reject_option" False
          sqlSet "show_pdf_download" False
          sqlWhereExists $ sqlSelect "signatory_links" $ do
            sqlWhere "signatory_links.document_id = documents.id"
            sqlWhereEq "signatory_links.delivery_method" PadDelivery

        return ()
      }

migrateDocumentsAddDocumentToken :: MonadDB m => Migration m
migrateDocumentsAddDocumentToken =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 31
    , mgrDo = do
        runQuery_ $ sqlAlterTable "documents"
                  [ sqlAddColumn $ tblColumn { colName = "token"
                                             , colType = BigIntT
                                             , colNullable = True }
                  ]
        -- We need to generate random tokens here. Postgresql random function
        -- returns double precision number in the range [0;1], but there are
        -- about 31 bits of significant random data in there. Putting those two
        -- together it gives 62 bits and that should be enough.
        --
        -- We do two runs over documents table so that both random samples are
        -- independent from each other.
        runSQL_ "update documents set token = (random() * 2147483647) :: BIGINT"
        runSQL_ "update documents set token = ((random() * 2147483647) :: BIGINT << 32) # token"
        runQuery_ $ sqlAlterTable "documents"
                  [ sqlAlterColumn "token" "set not null"
                  ]
        return ()
      }


addConfirmTextToDocuments :: MonadDB m => Migration m
addConfirmTextToDocuments =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 32
    , mgrDo = do
        runSQL_ "ALTER TABLE documents ADD COLUMN confirm_text TEXT NOT NULL DEFAULT ''"
        return ()
      }

addTimeZoneNameToDocuments :: MonadDB m => Migration m
addTimeZoneNameToDocuments =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 33
    , mgrDo = do
        runSQL_ "ALTER TABLE documents ADD COLUMN time_zone_name TEXT NOT NULL DEFAULT 'Europe/Stockholm'"
        return ()
      }

addAPIVersionToDocument :: MonadDB m => Migration m
addAPIVersionToDocument =
  Migration {
      mgrTable = tableDocuments
    , mgrFrom = 34
    , mgrDo = do
        runSQL_ "ALTER TABLE documents ADD COLUMN api_version SMALLINT NOT NULL DEFAULT 1"
        return ()
      }

fixSignatureFieldsWithAnySize :: MonadDB m => Migration m
fixSignatureFieldsWithAnySize =
  Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 4
  , mgrDo =  do
    runQuery_ $ sqlSelect "signatory_link_fields" $ do
                 sqlResult "id, placements"
                 sqlWhere "placements ILIKE '%\"wrel\":0,%'"
                 sqlWhereEq "type" (SignatureFT undefined)
    values :: [(Int64, [FieldPlacement])] <- fetchMany id
    forM_ values $ \(fid,  placements) -> do
      let placements' = for placements (\p -> if (placementwrel p == 0 || placementhrel p == 0)
                                                then  p {placementwrel = 260 / 943, placementhrel = 102 / 1335 }
                                                else p )

      runQuery_ $ sqlUpdate "signatory_link_fields" $ do
                  sqlSet "placements" placements'
                  sqlWhereEq "id" fid
  }


-- Personal number used to be obligatory, but we didn't asked about it in extra details section
changeSomeStandardFieldsToOptional :: MonadDB m => Migration m
changeSomeStandardFieldsToOptional=
  Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 5
  , mgrDo =  do
    runQuery_ $ sqlUpdate "signatory_link_fields" $ do
                 sqlSet "obligatory" False
                 sqlWhereInSql "id" $ do
                   sqlSelect "documents as d, signatory_links as s, signatory_link_fields as f" $ do
                     sqlResult "f.id"
                     sqlWhereEq "f.placements" ("[]"::String)
                     sqlWhereEq "f.value" (""::String)
                     sqlWhereEqSql "f.signatory_link_id" "s.id"
                     sqlWhereEqSql "d.id" "s.document_id"
                     sqlWhereNotEq "d.status" Closed
                     sqlWhereAny
                       [ do
                         sqlWhereEq "f.type" PersonalNumberFT
                         sqlWhereNotEq "s.authentication_method" ELegAuthentication
                       , do
                         sqlWhereEq "f.type" (MobileFT)
                         sqlWhereNotIn "s.delivery_method" [MobileDelivery,EmailAndMobileDelivery]
                         sqlWhereNotIn "s.confirmation_delivery_method" [MobileConfirmationDelivery,EmailAndMobileConfirmationDelivery]
                       ]
  }

-- Personal number used to be obligatory, but we didn't asked about it in extra details section
addUniqueContraintsTypeOnFields :: (MonadDB m,Log.MonadLog m) => Migration m
addUniqueContraintsTypeOnFields=
  Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 6
  , mgrDo = do
       -- We have a large number of duplicated signature fields (10K+) - they have same value and same name, but only one of them has placement
       Log.mixlog_ "Migration of fields started"
       -- We create index first so that rest of the migration runs on somewhat ok speed

       let migrateOnce = do
             n1 <- runQuery $ sqlDelete "signatory_link_fields" $ do
               sqlFrom "signatory_link_fields as s2"
               -- We select fields for same signatory, with same values and names
               sqlWhere "signatory_link_fields.signatory_link_id = s2.signatory_link_id"
               sqlWhere "signatory_link_fields.type = s2.type"
               sqlWhere "signatory_link_fields.custom_name = s2.custom_name"
               sqlWhereEq "signatory_link_fields.type" (8::Int16) -- SignatureFT
               -- We need to compare MD5, since simple value comparision is very slow
               sqlWhere "md5(signatory_link_fields.value) = md5(s2.value)"
               -- We select one without placement that has a signature
               -- in order lower or a signature with placements
               -- (anywhere in order).
               sqlWhere "(signatory_link_fields.id > s2.id OR s2.placements <> '[]')"
               sqlWhereEq "signatory_link_fields.placements" ("[]" :: String)
             Log.mixlog_ $ "Migration (unique fields): " ++ show n1 ++" duplicated signature fields removed"
       migrateOnce
       -- We are expecting to have less then 500 other fields, that
       -- need to be fixed. So it should be ok to do a separate update
       -- for each of them
       Log.mixlog_ "About to rename unmergeable custom fields"
       renameUnmergeableFields
       Log.mixlog_ "About to fix other fields"
       fixOtherFields
       Log.mixlog_ "Fields fixed, creating indexes"
       -- When we fixed all fields, we can introduce a contraint
       runQuery_ $ sqlDropIndex "signatory_link_fields" (indexOnColumn "signatory_link_id")
       runQuery_ $ sqlCreateIndex "signatory_link_fields" (uniqueIndexOnColumns ["signatory_link_id","type","custom_name"])
       Log.mixlog_ "Migration of fields ended"
  }
  where
       renameUnmergeableFields = do
         n <- runQuery $ sqlUpdate "signatory_link_fields" $ do

           -- Calculate number of fields that have the same name but
           -- are before this one in order.
           let calc = "SELECT count(*)"
                  <+>   "FROM signatory_link_fields s3"
                  <+>  "WHERE signatory_link_fields.signatory_link_id = s3.signatory_link_id"
                  <+>    "AND signatory_link_fields.type = s3.type"
                  <+>    "AND signatory_link_fields.custom_name = s3.custom_name"
                  <+>    "AND s3.id < signatory_link_fields.id"
           -- Append as many '_' as there are fields before this one
           sqlSetCmd "custom_name" ("custom_name || rpad('', (" <> calc <> ") :: INT, '_')")
           -- We fix only custom fields this way
           sqlWhereIn "type" [SignatureFT undefined, CustomFT undefined undefined, CheckboxFT undefined]
           -- Make sure that we alter only the problemtic
           -- fields. Otherwise Postgresql will happily reqwrite whole
           -- table.
           sqlWhereExists $ sqlSelect "signatory_link_fields as s3" $ do
             sqlWhere "signatory_link_fields.signatory_link_id = s3.signatory_link_id"
             sqlWhere "signatory_link_fields.type = s3.type"
             sqlWhere "signatory_link_fields.custom_name = s3.custom_name"
             sqlWhere "s3.id < signatory_link_fields.id"
         Log.mixlog_ $ "Renamed " ++ show n ++ " unmergeable fields"

       fixOtherFields = do
         runQuery_ $ sqlSelect "signatory_link_fields" $ do
                       sqlWhereExists $ sqlSelect "signatory_link_fields as s2" $ do
                         sqlWhere   "signatory_link_fields.signatory_link_id = s2.signatory_link_id"
                         sqlWhere   "signatory_link_fields.type = s2.type"
                         sqlWhere   "signatory_link_fields.custom_name = s2.custom_name"
                         sqlWhere   "signatory_link_fields.id <> s2.id"
                       sqlResult  "signatory_link_fields.id, signatory_link_fields.signatory_link_id, signatory_link_fields.type, signatory_link_fields.custom_name, signatory_link_fields.value, signatory_link_fields.placements"
         values :: [(Int64, Int64, FieldType, String, String, [FieldPlacement])] <- fetchMany id
         fixUniqField values

       fixUniqField ((fid1,fslid1,ft1,fcn1,fv1,fp1) : (fid2,fslid2,ft2,fcn2,fv2,fp2) : rest) |
         ft1==ft2 && fslid1==fslid2 && fcn1==fcn2 = do
           fixStandardField (fid1,fv1,fp1,fid2,fv2,fp2)
           fixUniqField ( (fid1,fslid1,ft1,fcn1,(if (null fv1) then fv2 else fv1),fp1 ++ fp2) : rest)
       fixUniqField (_ : rest)  = fixUniqField rest
       fixUniqField [] = return ()

       fixStandardField (fid1,fv1,fp1,fid2,fv2,fp2) = do
         Log.mixlog_ $ "Migration (unique fields): Merging standard fields that should be joined: " ++ show fid1 ++ " " ++ show fid2 ++ ".\n" ++
                       "Values that are merged are: '" ++ fv1 ++"' and '" ++ fv2 ++ "'. Value '"++(if (null fv1) then fv2 else fv1)++"' will be used."
         runQuery_ $ sqlUpdate "signatory_link_fields" $ do
           sqlWhereEq "id" fid1
           sqlSet "value" (if (null fv1) then fv2 else fv1)
           sqlSet "placements" (fp1 ++ fp2)
         runQuery_ $ sqlDelete "signatory_link_fields" $ do
           sqlWhereEq "id" fid2


makeSealStatusNonNullInMainFiles  :: MonadDB m => Migration m
makeSealStatusNonNullInMainFiles =
  Migration {
      mgrTable = tableMainFiles
    , mgrFrom = 1
    , mgrDo = do
       runQuery_ $ sqlUpdate "main_files" $ do
         sqlSet "seal_status" UnknownSealStatus
         sqlWhereIsNULL "seal_status"
         sqlWhereEq "document_status" Closed
       runQuery_ $ sqlUpdate "main_files" $ do
         sqlSet "seal_status" Missing
         sqlWhereIsNULL "seal_status"
         sqlWhereEq "document_status" Preparation
       _ <- runSQL_ $ "ALTER TABLE main_files ALTER seal_status SET NOT NULL"
       return ()
    }
