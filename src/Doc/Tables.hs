module Doc.Tables where

import DB

tableDocuments :: Table
tableDocuments = tblTable {
    tblName = "documents"
  , tblVersion = 25
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "file_id", colType = BigIntT }
    , tblColumn { colName = "sealed_file_id", colType = BigIntT }
    , tblColumn { colName = "title", colType = TextT, colNullable = False }
    , tblColumn { colName = "status", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "error_text", colType = TextT }
    , tblColumn { colName = "type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "process", colType = SmallIntT }
    , tblColumn { colName = "ctime", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "mtime", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "days_to_sign", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "timeout_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "invite_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "invite_ip", colType = IntegerT }
    , tblColumn { colName = "invite_text", colType = TextT, colNullable = False }
    , tblColumn { colName = "csv_title", colType = TextT }
    , tblColumn { colName = "csv_contents", colType = TextT }
    , tblColumn { colName = "csv_signatory_index", colType = IntegerT }
    , tblColumn { colName = "sharing", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "lang", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "api_callback_url", colType = TextT }
    , tblColumn { colName = "unsaved_draft", colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "object_version", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "seal_status", colType = SmallIntT }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      tblForeignKeyColumn "file_id" "files" "id"
    , tblForeignKeyColumn "sealed_file_id" "files" "id"
    ]
  }

tableAuthorAttachments :: Table
tableAuthorAttachments = tblTable {
    tblName = "author_attachments"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "file_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["file_id", "document_id"]
  , tblForeignKeys = [
      tblForeignKeyColumn "file_id" "files" "id"
    , (tblForeignKeyColumn "document_id" "documents" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  }

tableSignatoryAttachments :: Table
tableSignatoryAttachments = tblTable {
    tblName = "signatory_attachments"
  , tblVersion = 7
  , tblColumns = [
      tblColumn { colName = "file_id", colType = BigIntT }
    , tblColumn { colName = "description", colType = TextT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False, colDefault = Just "0" }
    ]
  , tblPrimaryKey = ["name", "signatory_link_id"]
  , tblForeignKeys = [
      tblForeignKeyColumn "file_id" "files" "id"
    , (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  , tblIndexes = [tblIndexOnColumn "signatory_link_id"]
  }

tableSignatoryLinks :: Table
tableSignatoryLinks = tblTable {
    tblName = "signatory_links"
  , tblVersion = 20
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "user_id", colType = BigIntT }
    , tblColumn { colName = "sign_order", colType = IntegerT, colNullable = False, colDefault = Just "1" }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "sign_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "sign_ip", colType = IntegerT }
    , tblColumn { colName = "seen_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "seen_ip", colType = IntegerT }
    , tblColumn { colName = "read_invitation", colType = TimestampWithZoneT }
    , tblColumn { colName = "invitation_delivery_status", colType = SmallIntT, colNullable = False, colDefault = Just "3" }
    , tblColumn { colName = "signinfo_text", colType = TextT }
    , tblColumn { colName = "signinfo_signature", colType = TextT }
    , tblColumn { colName = "signinfo_certificate", colType = TextT }
    , tblColumn { colName = "signinfo_provider", colType = SmallIntT }
    , tblColumn { colName = "signinfo_first_name_verified", colType = BoolT }
    , tblColumn { colName = "signinfo_last_name_verified", colType = BoolT }
    , tblColumn { colName = "signinfo_personal_number_verified", colType = BoolT }
    , tblColumn { colName = "deleted", colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "really_deleted", colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "csv_title", colType = TextT }
    , tblColumn { colName = "csv_contents", colType = TextT }
    , tblColumn { colName = "signinfo_ocsp_response", colType = TextT }
    , tblColumn { colName = "sign_redirect_url", colType = TextT }
    , tblColumn { colName = "is_author", colType = BoolT, colNullable = False }
    , tblColumn { colName = "is_partner", colType = BoolT, colNullable = False }
    , tblColumn { colName = "rejection_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "rejection_reason", colType = TextT }
    , tblColumn { colName = "authentication_method", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "eleg_data_mismatch_message", colType = TextT }
    , tblColumn { colName = "eleg_data_mismatch_first_name", colType = TextT }
    , tblColumn { colName = "eleg_data_mismatch_last_name", colType = TextT }
    , tblColumn { colName = "eleg_data_mismatch_personal_number", colType = TextT }
    , tblColumn { colName = "delivery_method", colType = SmallIntT, colNullable = False }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "document_id" "documents" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    , tblForeignKeyColumn "user_id" "users" "id"
    ]
  , tblIndexes = [
      tblIndexOnColumn "user_id"
    , tblIndexOnColumn "document_id"
    ]
  }

tableDocumentTags :: Table
tableDocumentTags = tblTable {
    tblName = "document_tags"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "value", colType = TextT, colNullable = False }
    ]
  , tblForeignKeys = [
      (tblForeignKeyColumn "document_id" "documents" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  , tblIndexes = [tblIndexOnColumn "document_id"]
  }

tableSignatoryLinkFields :: Table
tableSignatoryLinkFields = tblTable {
    tblName = "signatory_link_fields"
  , tblVersion = 4
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "custom_name", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "value", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "is_author_filled", colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "placements", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "obligatory", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "should_be_filled_by_author", colType = BoolT, colNullable = False, colDefault = Just "false" }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  , tblIndexes = [ tblIndexOnColumn "signatory_link_id" ]
  }

tableSignatoryScreenshots :: Table
tableSignatoryScreenshots = tblTable {
    tblName = "signatory_screenshots"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "type", colType = TextT, colNullable = False }
    , tblColumn { colName = "time", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "mimetype", colType = TextT, colNullable = False }
    , tblColumn { colName = "image", colType = BinaryT, colNullable = False }
    ]
  , tblPrimaryKey = ["id"]
  , tblForeignKeys = [
      (tblForeignKeyColumn "signatory_link_id" "signatory_links" "id") {
        fkOnDelete = ForeignKeyCascade
      }
    ]
  , tblIndexes = [tblIndexOnColumn "signatory_link_id"]
  }
