module Doc.Migrations where

import Data.Char
import Data.Int
import Data.String.Utils
import Text.HTML.TagSoup.Entity
import Text.XML.HaXml(render)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty(content)
import qualified Data.ByteString as BS
import qualified Text.XML.HaXml.Types as XML

import DB
import Doc.Tables
import KontraPrelude
import Utils.String

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
      , sqlAddCheck $ Check "check_signatory_link_fields_signatures_are_well_defined"
          "type = 8 AND name_order IS NULL AND value_bool IS NULL AND value_text IS NULL OR type <> 8"
      , sqlAddCheck $ Check "check_signatory_link_fields_checkboxes_are_well_defined"
          "type = 9 AND name_order IS NULL AND value_bool IS NOT NULL AND value_file_id IS NULL AND value_text IS NULL OR type <> 9"
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

addFileNameToMainFiles :: MonadDB m  => Migration m
addFileNameToMainFiles = Migration {
      mgrTable = tableMainFiles
    , mgrFrom = 2
    , mgrDo = return () -- Empty migration to mark change in ctMainFile :: CompositeType
    }

addFileNameToAuthorAttachments :: MonadDB m  => Migration m
addFileNameToAuthorAttachments = Migration {
      mgrTable = tableAuthorAttachments
    , mgrFrom = 1
    , mgrDo = return () -- Empty migration to mark change in ctAuthorAttachment :: CompositeType

    }


dropErrorTextFromDocuments :: MonadDB m  => Migration m
dropErrorTextFromDocuments = Migration {
      mgrTable = tableDocuments
    , mgrFrom = 37
    , mgrDo = runSQL_ "ALTER TABLE documents DROP COLUMN error_text"
              -- Also this migration will update composite type
    }

dropCSVTitleFromSignatories :: MonadDB m  => Migration m
dropCSVTitleFromSignatories = Migration {
      mgrTable = tableSignatoryLinks
    , mgrFrom = 26
    , mgrDo = runSQL_ "ALTER TABLE signatory_links DROP COLUMN csv_title"
              -- Also this migration will update composite type
    }

dropAPIVersionFromDocuments :: MonadDB m => Migration m
dropAPIVersionFromDocuments = Migration {
      mgrTable = tableDocuments
    , mgrFrom = 38
    , mgrDo = runSQL_ "ALTER TABLE documents DROP COLUMN api_version"
              -- Also this migration will update composite type
    }

addAPIV2CallbackAndRenameExisting :: MonadDB m => Migration m
addAPIV2CallbackAndRenameExisting = Migration {
      mgrTable = tableDocuments
    , mgrFrom = 39
    , mgrDo = do
        runSQL_ "ALTER TABLE documents RENAME api_callback_url TO api_v1_callback_url"
        runSQL_ "ALTER TABLE documents ADD COLUMN api_v2_callback_url TEXT NULL"
    }

fixPurgedPendingDocumentsAndAddConstraint :: MonadDB m => Migration m
fixPurgedPendingDocumentsAndAddConstraint = Migration {
      mgrTable = tableDocuments
    , mgrFrom = 40
    , mgrDo = do
        runSQL_ "UPDATE documents SET status = 5 WHERE status = 2 and purged_time IS NOT NULL" -- Timeout pending purged documents
        runQuery_ $ sqlAlterTable "documents" [
          sqlAddCheck $ Check "check_documents_pending_are_not_purged"
            "status <> 2 OR purged_time IS NULL"
          ]
    }



addSignatoryAuthenticationToView :: MonadDB m => Migration m
addSignatoryAuthenticationToView = Migration {
      mgrTable = tableSignatoryLinks
    , mgrFrom = 27
    , mgrDo = do
      runSQL_ "ALTER TABLE signatory_links RENAME COLUMN authentication_method TO authentication_to_sign_method"
      runSQL_ "ALTER TABLE signatory_links ADD COLUMN authentication_to_view_method SMALLINT NOT NULL DEFAULT 1" -- StandartAuthenticationToView
      runSQL_ "ALTER TABLE signatory_links ALTER COLUMN authentication_to_view_method DROP DEFAULT"
    }

addAuthenticatedToViewToCompositeType :: MonadDB m => Migration m
addAuthenticatedToViewToCompositeType = Migration {
      mgrTable = tableSignatoryLinks
    , mgrFrom = 28
    , mgrDo = return () -- Empty migration to mark change in ctSignatoryLink :: CompositeType
    }
