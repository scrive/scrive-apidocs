{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.Migrations where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.Char
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Monoid.Utils
import Data.String.Utils
import Text.HTML.TagSoup.Entity
import Text.JSON
import Text.JSON.FromJSValue
import Text.XML.HaXml(render)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty(content)
import qualified Data.ByteString as BS
import qualified Text.XML.HaXml.Types as XML

import DB
import DB.Checks
import Doc.DocStateData
import Doc.SealStatus (SealStatus(..))
import Doc.Tables
import EvidenceLog.Model
import MinutesTime
import Utils.Default
import Utils.Prelude
import Utils.String
import Version
import qualified Log

moveSignaturesToFilesAndAddBoolValueForFields :: MonadDB m => Migration m
moveSignaturesToFilesAndAddBoolValueForFields = Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 9
  , mgrDo = do
    runQuery_ $ sqlAlterTable "signatory_link_fields" [
        sqlDropCheck $ Check "check_signatory_link_fields_text_fields_well_defined" ""
      , sqlDropCheck $ Check "check_signatory_link_fields_signature_well_defined" ""
      , sqlDropCheck $ Check "check_signatory_link_fields_text_fields_name_order_well_defined" ""
      ]
    runSQL_ "ALTER TABLE signatory_link_fields ADD COLUMN value_bool BOOL NULL"
    runSQL_ "UPDATE signatory_link_fields SET value_bool = value_text <> '', value_text = NULL WHERE type = 9"

    runSQL_ "ALTER TABLE signatory_link_fields ADD COLUMN value_file_id BIGINT NULL"
    runSQL_ "ALTER TABLE files ADD COLUMN tmp_signatory_link_fields_id BIGINT NULL"

    runQuery_ $ sqlInsertSelect "files" "signatureFields" $ do
      sqlWith "signatureFields" . sqlSelect "signatory_link_fields" $ do
        sqlWhereEq "signatory_link_fields.type" (8 :: Int16)
        sqlWhere "signatory_link_fields.value_binary IS NOT NULL AND signatory_link_fields.value_binary <> ''"
        sqlResult "signatory_link_fields.id AS signature_field_id"
        sqlResult "signatory_link_fields.value_binary AS value_binary"

      sqlSetCmd "name" "'signature.png'"
      sqlSetCmd "content" "value_binary"
      sqlSetCmd "size" "octet_length(value_binary)"
      sqlSetCmd "checksum" "digest(value_binary,'sha1')"
      sqlSetCmd "tmp_signatory_link_fields_id" "signature_field_id"

    runSQL_ $ "UPDATE signatory_link_fields"
      <+> "SET value_file_id = f.id"
      <+> "FROM (SELECT id, tmp_signatory_link_fields_id FROM files WHERE tmp_signatory_link_fields_id IS NOT NULL) AS f"
      <+> "WHERE signatory_link_fields.id = f.tmp_signatory_link_fields_id"

    runSQL_ "ALTER TABLE files DROP COLUMN tmp_signatory_link_fields_id"
    runSQL_ "ALTER TABLE signatory_link_fields DROP COLUMN value_binary"

    runQuery_ $ sqlAlterTable "signatory_link_fields" [
        sqlAddCheck $ Check "check_signatory_link_fields_name_fields_are_well_defined"
          "type = 1 AND name_order IS NOT NULL AND value_bool IS NULL AND value_file_id IS NULL AND value_text IS NOT NULL OR type <> 1"
      , sqlAddCheck $ Check "check_signatory_link_fields_checkboxes_are_well_defined"
          "type = 9 AND name_order IS NULL AND value_bool IS NOT NULL AND value_file_id IS NULL AND value_text IS NULL OR type <> 9"
      , sqlAddCheck $ Check "check_signatory_link_fields_signatures_are_well_defined"
          "type = 8 AND name_order IS NULL AND value_bool IS NULL AND value_text IS NULL OR type <> 8"
      , sqlAddCheck $  Check "check_signatory_link_fields_other_text_fields_are_well_defined"
          "(type = ANY (ARRAY[3, 4, 5, 6, 7, 10])) AND name_order IS NULL AND value_bool IS NULL AND value_file_id IS NULL AND value_text IS NOT NULL OR NOT (type = ANY (ARRAY[3, 4, 5, 6, 7, 10]))"
      , sqlAddFK "signatory_link_fields" $ fkOnColumn "value_file_id" "files" "id"
      ]


}


addNameOrderToFieldsAndMigrateNames :: MonadDB m => Migration m
addNameOrderToFieldsAndMigrateNames = Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 8
  , mgrDo = do
    runQuery_ $ sqlDropIndex "signatory_link_fields" (uniqueIndexOnColumns ["signatory_link_id","type","custom_name"])
    runSQL_ "ALTER TABLE signatory_link_fields ADD COLUMN name_order SMALLINT NULL"
    runSQL_ "UPDATE signatory_link_fields SET type=1, name_order=1 WHERE type=1"
    runSQL_ "UPDATE signatory_link_fields SET type=1, name_order=2 WHERE type=2"
    runQuery_ $ sqlAlterTable "signatory_link_fields" [
      sqlAddCheck $ Check "check_signatory_link_fields_text_fields_name_order_well_defined"
        "type = 1 AND name_order IS NOT NULL OR type <> 1 AND name_order IS NULL"
      ]
    runQuery_ $ sqlCreateIndex "signatory_link_fields" (uniqueIndexOnColumns ["signatory_link_id","type","name_order","custom_name"])

}


dropHTMLFromInvitationAndConfirmationMessages :: MonadDB m => Migration m
dropHTMLFromInvitationAndConfirmationMessages = Migration {
  mgrTable = tableDocuments
, mgrFrom = 36
, mgrDo = do
    -- Fix invitation custom message
    runSQL_ "SELECT id, invite_text FROM documents WHERE invite_text IS NOT NULL AND invite_text <> ''"
    (invite_messages :: [(Int64,String)]) <- fetchMany id
    forM_ invite_messages $ \(did, invite_message) -> do
      runQuery_ . sqlUpdate "documents" $ do
           sqlSet "invite_text" $ fixMessage $ invite_message
           sqlWhereEq "id" did

    -- Fix confirmation custom message
    runSQL_ "SELECT id, confirm_text FROM documents WHERE confirm_text IS NOT NULL AND confirm_text <> ''"
    (confirm_messages:: [(Int64,String)]) <- fetchMany id
    forM_ confirm_messages $ \(did, confirm_message) -> do
      runQuery_ . sqlUpdate "documents" $ do
           sqlSet "confirm_text" $ fixMessage $ confirm_message
           sqlWhereEq "id" did
}
  where
    fixMessage :: String -> String
    fixMessage xs = strip $ replace "\160" " " $ case xmlParse' "Migration-documents-drop html" $ "<span>" ++ xs ++ "</span>" of
       (Right (XML.Document _ _ (XML.Elem _ _ cs) _)) -> (concatMap fixContent cs)
       _ -> let xsWithFixedBRs = replace "<BR>" "<BR/>" $ replace "<br>" "<br/>" xs
            in if xsWithFixedBRs /= xs
               then fixMessage xsWithFixedBRs
               else unescapeHTML $ justText xs
    fixContent :: XML.Content Posn -> String
    fixContent (XML.CElem (XML.Elem (XML.N "div") _ cs) _) = (concatMap fixContent cs) ++ " \n"
    fixContent (XML.CElem (XML.Elem (XML.N "p") _ cs) _)   = (concatMap fixContent cs) ++ " \n"
    fixContent (XML.CElem (XML.Elem (XML.N "br") _ cs) _)  = (concatMap fixContent cs) ++ " \n"
    fixContent (XML.CElem (XML.Elem (XML.N _) _ cs) _)     = (concatMap fixContent cs)
    fixContent x@(XML.CString _ _ _)                       = render $ content x
    fixContent (XML.CRef (XML.RefEntity ent) _)            = fromMaybe "" $ lookupEntity ent
    fixContent (XML.CRef (XML.RefChar i) _)                = [chr i]
    fixContent _ = ""
    justText ('<':cs) = justText $ drop 1 $ dropWhile (/= '>') cs
    justText (c:cs) = c : justText cs
    justText [] = []

addMtimeStatusIndexes :: MonadDB m => Migration m
addMtimeStatusIndexes = Migration {
  mgrTable = tableDocuments
, mgrFrom = 35
, mgrDo = do
  runQuery_ . sqlCreateIndex "documents" $ indexOnColumn "mtime"
  runQuery_ . sqlCreateIndex "documents" $ indexOnColumn "status"
}

signatoryLinksMoveSignatures :: MonadDB m => Migration m
signatoryLinksMoveSignatures = Migration {
  mgrTable = tableSignatoryLinks
, mgrFrom = 25
, mgrDo = do
  -- NULLify empty certificate fields (set for mobile bank id)
  runQuery_ . sqlUpdate "signatory_links" $ do
    sqlSet "signinfo_certificate" (Nothing :: Maybe String)
    sqlWhere "signinfo_certificate = ''"

  -- move signature data to eid_signatures
  runQuery_ . sqlInsertSelect "eid_signatures" "signatory_links sl" $ do
    sqlSetCmd "signatory_link_id" "sl.id"
    sqlSetCmd "provider" "sl.signinfo_provider"
    sqlSetCmd "data" "sl.signinfo_text"
    sqlSetCmd "signature" "decode(sl.signinfo_signature, 'base64')"
    sqlSetCmd "certificate" "decode(sl.signinfo_certificate, 'base64')"
    sqlSetCmd "ocsp_response" "decode(sl.signinfo_ocsp_response, 'base64')"
    sqlWhere "sl.signinfo_provider IS NOT NULL"

  -- delete data from signatory_links table
  runQuery_ . sqlAlterTable "signatory_links" $ [
      sqlDropColumn "signinfo_text"
    , sqlDropColumn "signinfo_signature"
    , sqlDropColumn "signinfo_certificate"
    , sqlDropColumn "signinfo_provider"
    , sqlDropColumn "signinfo_first_name_verified"
    , sqlDropColumn "signinfo_last_name_verified"
    , sqlDropColumn "signinfo_personal_number_verified"
    , sqlDropColumn "signinfo_ocsp_response"
    , sqlDropColumn "eleg_data_mismatch_message"
    , sqlDropColumn "eleg_data_mismatch_first_name"
    , sqlDropColumn "eleg_data_mismatch_last_name"
    , sqlDropColumn "eleg_data_mismatch_personal_number"
    ]
}

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

setMandatoryExpirationTimeInDocument :: (MonadDB m, MonadThrow m, MonadTime m) => Migration m
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
    check <- fetchMany runIdentity
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

removeOldDocumentLog :: (MonadDB m, MonadThrow m, MonadTime m) => Migration m
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

migrateSignatoryLinksDeletedTime :: (MonadDB m, MonadTime m) => Migration m
migrateSignatoryLinksDeletedTime = Migration {
  mgrTable = tableSignatoryLinks
, mgrFrom = 20
, mgrDo = do
  now <- currentTime
  runSQL_ $ smconcat [
      "ALTER TABLE signatory_links"
    , "ALTER deleted DROP NOT NULL,"
    , "ALTER deleted DROP DEFAULT,"
    , "ALTER deleted TYPE TIMESTAMPTZ USING (CASE WHEN deleted THEN" <?> now <+> "ELSE NULL END),"
    , "ALTER really_deleted DROP NOT NULL,"
    , "ALTER really_deleted DROP DEFAULT,"
    , "ALTER really_deleted TYPE TIMESTAMPTZ USING (CASE WHEN really_deleted THEN" <?> now <+> "ELSE NULL END)"
    ]
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
        migratedFiles <- (fromIntegral :: Int64 -> Int) <$> fetchOne runIdentity
        runQuery_ . sqlSelect "main_files" $ do
          sqlResult "count(*)"
          sqlWhereEq "document_status" Closed
          sqlWhereExists $ sqlSelect "documents" $ do
            sqlWhere "documents.id = main_files.document_id"
            sqlWhere "documents.sealed_file_id = main_files.file_id"
            sqlWhere "documents.seal_status IS NOT DISTINCT FROM main_files.seal_status"
        migratedSealedFiles <- (fromIntegral :: Int64 -> Int) <$> fetchOne runIdentity
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
                 sqlWhereEq "type" (8 :: Int16)
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
                     sqlWhereEqSql "f.signatory_link_id" ("s.id"::SQL)
                     sqlWhereEqSql "d.id" ("s.document_id"::SQL)
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

signatoryLinkFieldsAddBinaryValue :: MonadDB m => Migration m
signatoryLinkFieldsAddBinaryValue = Migration {
  mgrTable = tableSignatoryLinkFields
, mgrFrom = 7
, mgrDo = do
  let alter_table = sqlAlterTable "signatory_link_fields"
  -- add new field according to the table definition
  runQuery_ $ alter_table ["RENAME value TO value_text"]
  runQuery_ $ alter_table [
      sqlAlterColumn "value_text" "DROP DEFAULT"
    , sqlAlterColumn "value_text" "DROP NOT NULL"
    , sqlAddColumn tblColumn { colName = "value_binary", colType = BinaryT }
    ]

  -- move empty signatures to bytea
  runQuery_ . sqlUpdate "signatory_link_fields" $ do
    sqlSet "value_text" (Nothing :: Maybe String)
    sqlSet "value_binary" $ Binary BS.empty
    sqlWhereEq "type" (8::Int16) -- SignatureFT
    sqlWhereEq "value_text" (""::String)

  -- move signatures stored as base64 to bytea. if regex matching fails, substring
  -- returns NULL and therefore the next query setting constraint checks will fail,
  -- because the field will not have value_text nor value_binary set as NOT NULL.
  runQuery_ . sqlUpdate "signatory_link_fields" $ do
    sqlSet "value_text" (Nothing :: Maybe String)
    sqlSetCmd "value_binary" "decode(substring(value_text from 'data:(?:[a-z/-]*;base64){0,1},(.*)'), 'base64')"
    sqlWhereEq "type" (8::Int16) -- SignatureFT
    sqlWhereIsNULL "value_binary"

  runQuery_ $ alter_table [
      sqlAddCheck $ Check "check_signatory_link_fields_signature_well_defined"
        "type = 8 AND value_binary IS NOT NULL AND value_text IS NULL OR type <> 8"
    , sqlAddCheck $ Check "check_signatory_link_fields_text_fields_well_defined"
        "type = 8 OR type <> 8 AND value_binary IS NULL AND value_text IS NOT NULL"
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
           sqlWhereIn "type" ([7, 8, 9]::[Int16]) -- Text,Signature and Checkbox
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
