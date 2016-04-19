module Doc.Migrations where

import Control.Monad.Catch
import Data.Char
import Data.Int
import Data.String.Utils
import Log
import Text.HTML.TagSoup.Entity
import Text.XML.HaXml(render)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty(content)
import qualified Data.ByteString as BS
import qualified Text.XML.HaXml.Types as XML

import DB
import DB.Checks
import Doc.Tables
import KontraPrelude
import Utils.String

addUniqueConstraintForAuthorCheck :: MonadDB m => Migration m
addUniqueConstraintForAuthorCheck = Migration {
    mgrTable = tableSignatoryLinks
  , mgrFrom = 29
  , mgrDo = do
      let tname = tblName tableSignatoryLinks
      runQuery_ $ sqlAlterTable tname [sqlDropColumn "is_author"]
      runQuery_ . sqlCreateIndex tname $ uniqueIndexOnColumns ["id", "document_id"]

      -- Add consistency foreign key to documents.
      let tdocs = tblName tableDocuments
      runQuery_ $ sqlAlterTable tdocs [
          sqlAddFK tdocs $ (fkOnColumns ["id", "author_id"] "signatory_links" ["document_id", "id"]) { fkDeferred = True }
        ]
  }

documentsAddAuthorID :: (MonadDB m, MonadLog m, MonadThrow m) => Migration m
documentsAddAuthorID = Migration {
    mgrTable = tableDocuments
  , mgrFrom = 41
  , mgrDo = do
      let tname = tblName tableDocuments
      runQuery_ $ sqlAlterTable tname [sqlAddColumn tblColumn { colName = "author_id", colType = BigIntT, colNullable = False, colDefault = Just "0" }]

      let linksCount = parenthesize $ smconcat [
              "SELECT COUNT(*)"
            , "  FROM signatory_links sl"
            , " WHERE sl.document_id = d.id"
            ]
          authorsCount = parenthesize $ smconcat [
              "SELECT COUNT(*)"
            , "  FROM signatory_links sl"
            , " WHERE sl.document_id = d.id"
            , "   AND sl.is_author"
            ]

      -- For purged documents with zero authors pick the link with the
      -- lowest id to be the author...
      runQuery_ . sqlUpdate "signatory_links sl" $ do
        sqlWith "zero_authors" . sqlSelect "documents d" $ do
          sqlResult "d.id"
          sqlWhere "d.purged_time IS NOT NULL"
          sqlWhere $ linksCount <+> "> 0"
          sqlWhere $ authorsCount <+> "= 0"
        sqlSetCmd "is_author" . parenthesize $ "sl.id =" <+> parenthesize (smconcat [
            "SELECT MIN(osl.id)"
          , "  FROM signatory_links osl"
          , " WHERE osl.document_id = sl.document_id"
          ])
        sqlWhere "document_id IN (SELECT id FROM zero_authors)"

      -- ...or create one if there are no signatory links.
      runQuery_ . sqlInsertSelect "signatory_links" "zero_authors" $ do
        sqlWith "zero_authors" . sqlSelect "documents d" $ do
          sqlResult "d.id"
          sqlWhere "d.purged_time IS NOT NULL"
          sqlWhere $ linksCount <+> "= 0"
        sqlSetCmd "document_id" "zero_authors.id"
        sqlSet "token" (1::Int64)
        sqlSet "is_author" True
        sqlSet "is_partner" True
        sqlSet "delivery_method" (1::Int16)
        sqlSet "authentication_to_sign_method" (1::Int16)
        sqlSet "authentication_to_view_method" (1::Int16)

      -- For purged documents with more than one author pick the one
      -- with the lowest id.
      runQuery_ . sqlUpdate "signatory_links sl" $ do
        sqlWith "multiple_authors" . sqlSelect "documents d" $ do
          sqlResult "d.id"
          sqlWhere "d.purged_time IS NOT NULL"
          sqlWhere $ authorsCount <+> "> 1"
        sqlSetCmd "is_author" . parenthesize $ "sl.id =" <+> parenthesize (smconcat [
            "SELECT MIN(osl.id)"
          , "  FROM signatory_links osl"
          , " WHERE osl.document_id = sl.document_id"
          , "   AND osl.is_author"
          ])
        sqlWhere "document_id IN (SELECT id FROM multiple_authors)"

      -- If there are any remaining (non-purged) documents with zero
      -- or multiple authors, bail out as they need to be corrected
      -- manually.
      badDocuments <- runQuery . sqlSelect "documents d" $ do
        sqlResult "d.id"
        sqlResult authorsCount
        sqlWhere $ authorsCount <+> "<> 1"
      when (badDocuments > 0) $ do
        mapDB_ $ \(did::Int64, authors::Int64) -> do
          logInfo "Document has invalid number of authors" $ object [
              "document_id" .= did
            , "authors" .= authors
            ]
        $unexpectedErrorM "Migration failed"

      -- Import authors from signatory links table.
      runQuery_ . sqlUpdate "documents d" $ do
        sqlSetCmd "author_id" . parenthesize $ smconcat [
            "SELECT sl.id"
          , "FROM signatory_links sl"
          , "WHERE sl.document_id = d.id"
          , "  AND sl.is_author"
          ]
        sqlWhere "TRUE"

      -- Create index on author_id.
      runQuery_ $ sqlCreateIndex tname $ uniqueIndexOnColumn "author_id"
      -- Drop default value.
      runQuery_ $ sqlAlterTable tname [sqlAlterColumn "author_id" "DROP DEFAULT"]
  }

createIndexOnEmailFields :: MonadDB m => Migration m
createIndexOnEmailFields = Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 11
  , mgrDo = do
      let tname = tblName tableSignatoryLinkFields
      runQuery_ . sqlCreateIndex tname $ (indexOnColumn "value_text") { idxWhere = Just "type = 6" }
  }

createTablePlacementAnchors :: MonadDB m => Migration m
createTablePlacementAnchors = Migration {
    mgrTable = tablePlacementAnchors
  , mgrFrom = 0
  , mgrDo = createTable True tblTable {
      tblName = "placement_anchors"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "field_placement_id", colType = BigIntT, colNullable = False }
      , tblColumn { colName = "text", colType = TextT, colNullable = False }
      , tblColumn { colName = "index", colType = IntegerT, colNullable = False }
      ]
    , tblPrimaryKey = pkOnColumn "id"
    -- no foreigh keys, created later
    }
  }

createTableFieldPlacements :: MonadDB m => Migration m
createTableFieldPlacements = Migration {
    mgrTable = tableFieldPlacements
  , mgrFrom = 0
  , mgrDo = createTable True tblTable {
      tblName = "field_placements"
    , tblVersion = 1
    , tblColumns = [
        tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "signatory_field_id", colType = BigIntT, colNullable = False }
      , tblColumn { colName = "xrel", colType = DoubleT, colNullable = False }
      , tblColumn { colName = "yrel", colType = DoubleT, colNullable = False }
      , tblColumn { colName = "wrel", colType = DoubleT, colNullable = False }
      , tblColumn { colName = "hrel", colType = DoubleT, colNullable = False }
      , tblColumn { colName = "fsrel", colType = DoubleT, colNullable = False }
      , tblColumn { colName = "page", colType = IntegerT, colNullable = False }
      , tblColumn { colName = "tip", colType = SmallIntT }
      -- temporary column for staged data migration
      , tblColumn { colName = "anchors", colType = JsonT }
      ]
    , tblPrimaryKey = pkOnColumn "id"
    -- no foreigh keys, created later
    }
  }

unjsonFieldPlacements :: MonadDB m => Migration m
unjsonFieldPlacements = Migration {
    mgrTable = tableSignatoryLinkFields
  , mgrFrom = 10
  , mgrDo = do
    -- Note: tested on production data, took ~200 seconds to complete.
    -- Stage 1: copy placements over to a separate table with anchors as json.
    runSQL_ $ smconcat [
        "WITH placements AS ("
      , "  SELECT id, json_array_elements(placements::json) AS pl"
      , "  FROM signatory_link_fields"
      , ")"
      , "INSERT INTO field_placements (signatory_field_id, xrel, yrel, wrel, hrel, fsrel, page, tip, anchors)"
      , "(SELECT id"
      , ", (pl->>'xrel')::float8 AS xrel"
      , ", (pl->>'yrel')::float8 AS yrel"
      , ", (pl->>'wrel')::float8 AS wrel"
      , ", (pl->>'hrel')::float8 AS hrel"
      , ", (pl->>'fsrel')::float8 AS fsrel"
      , ", (pl->>'page')::int4 AS page"
      , ", CASE WHEN (pl->>'tip') IS NULL THEN NULL WHEN (pl->>'tip') = 'left' THEN 1 WHEN (pl->>'tip') = 'right' THEN 2 ELSE 65536 END AS tip"
      , ", (pl->>'anchors')::json AS anchors"
      , "FROM placements)"
      ]

    -- Add foreign key and index after copy as it's faster that way.
    let fpName = "field_placements"
    runQuery_ $ sqlAlterTable fpName [sqlAddFK fpName $ (fkOnColumn "signatory_field_id" "signatory_link_fields" "id") { fkOnDelete = ForeignKeyCascade }]
    runQuery_ . sqlCreateIndex fpName $ indexOnColumn "signatory_field_id"

    -- Stage 2: copy anchors to another table.
    runSQL_ $ smconcat [
        "WITH anchors AS ("
      , "  SELECT id, json_array_elements(anchors) AS anchor"
      , "FROM field_placements"
      , ")"
      , "INSERT INTO placement_anchors (field_placement_id, text, index)"
      , "(SELECT id"
      , ", anchor->>'text' AS text"
      , ", CASE WHEN (anchor->>'index') IS NULL THEN 1 ELSE (anchor->>'index')::int4 END AS index"
      , "FROM anchors)"
      ]

    -- Add foreign key and index once again after copy.
    let paName = "placement_anchors"
    runQuery_ $ sqlAlterTable paName [sqlAddFK paName $ (fkOnColumn "field_placement_id" "field_placements" "id") { fkOnDelete = ForeignKeyCascade }]
    runQuery_ . sqlCreateIndex paName $ indexOnColumn "field_placement_id"

    -- Stage 3: remove migrated json data
    runQuery_ $ sqlAlterTable fpName [sqlDropColumn "anchors"]
    runQuery_ $ sqlAlterTable (tblName tableSignatoryLinkFields) [sqlDropColumn "placements"]
  }

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

addRequiredAndNameToAuthorAttachments :: MonadDB m  => Migration m
addRequiredAndNameToAuthorAttachments = Migration {
      mgrTable = tableAuthorAttachments
    , mgrFrom = 2
    , mgrDo = do
        runQuery_ $ sqlAlterTable "author_attachments" [
            sqlAddColumn tblColumn { colName = "name", colType = TextT, colNullable = False, colDefault = Just "''::text"}
          , sqlAddColumn tblColumn { colName = "required", colType = BoolT, colNullable = False, colDefault = Just "false"}
          ]
        runSQL_ $ "UPDATE author_attachments"
          <+> "SET name = f.name"
          <+> "FROM (SELECT id,name FROM files) AS f"
          <+> "WHERE f.id = author_attachments.file_id"
        runQuery_ $ sqlAlterTable "author_attachments" [
            sqlAlterColumn "required" "DROP DEFAULT"
          , sqlAlterColumn "name" "DROP DEFAULT"
          ]
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

addFileNameToSignatoryAttachment :: MonadDB m  => Migration m
addFileNameToSignatoryAttachment = Migration {
      mgrTable = tableSignatoryAttachments
    , mgrFrom = 7
    , mgrDo = return () -- Empty migration to mark change in ctSignatoryAttachment :: CompositeType
    }

allowManyCopiesOfAuthorAttachmentForSameDocument :: MonadDB m => Migration m
allowManyCopiesOfAuthorAttachmentForSameDocument = Migration {
      mgrTable = tableAuthorAttachments
    , mgrFrom = 3
    , mgrDo = do
      runQuery_ $ "ALTER TABLE author_attachments " <> sqlDropPK (tblName tableAuthorAttachments)
      runQuery_ $ "ALTER TABLE author_attachments " <> sqlAddPK (tblName tableAuthorAttachments) ($fromJust $ pkOnColumns ["file_id", "document_id", "name"])
    }

addAddedToSealedFileToAuthorAttachment :: MonadDB m => Migration m
addAddedToSealedFileToAuthorAttachment = Migration {
      mgrTable = tableAuthorAttachments
    , mgrFrom = 4
    , mgrDo = do
        runQuery_ $ sqlAlterTable "author_attachments" [
          sqlAddColumn tblColumn { colName = "add_to_sealed_file", colType = BoolT, colNullable = False, colDefault = Just "true"}
          ]
        runQuery_ $ sqlAlterTable "author_attachments" [
          sqlAlterColumn "add_to_sealed_file" "DROP DEFAULT"
          ]
    }
