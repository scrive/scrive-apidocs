module Doc.Tables where

import DB
import KontraPrelude

tableDocuments :: Table
tableDocuments = tblTable {
    tblName = "documents"
  , tblVersion = 44
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "title", colType = TextT, colNullable = False }
    , tblColumn { colName = "status", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "ctime", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "mtime", colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "days_to_sign", colType = IntegerT, colNullable = False }
    , tblColumn { colName = "timeout_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "invite_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "invite_ip", colType = IntegerT }
    , tblColumn { colName = "invite_text", colType = TextT, colNullable = False }
    , tblColumn { colName = "sharing", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "lang", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "api_v1_callback_url", colType = TextT }
    , tblColumn { colName = "unsaved_draft", colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "object_version", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "purged_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "days_to_remind", colType = IntegerT  }
    , tblColumn { colName = "show_header", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "show_pdf_download", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "show_reject_option", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "show_footer", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "token", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "confirm_text", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "time_zone_name", colType = TextT, colNullable = False, colDefault = Just "'Europe/Stockholm'::text" }
    , tblColumn { colName = "api_v2_callback_url", colType = TextT }
    , tblColumn { colName = "author_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "allow_reject_reason", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "is_receipt", colType = BoolT, colNullable = False, colDefault = Just "false" }

    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
        -- Consistency check - author of a document needs to reference
        -- it back (deferred to break cyclic dependency).
        (fkOnColumns ["id", "author_id"] "signatory_links" ["document_id", "id"]) {
            fkDeferred = True
          }
      ]
  , tblIndexes = [
      -- for list of documents in adminonly
      indexOnColumn "mtime"
      -- for filtering by status in archive
    , indexOnColumn "status"
      -- for joining with signatory links
    , uniqueIndexOnColumn "author_id"
    ]
  , tblChecks = [
        Check "check_documents_pending_are_not_purged"
          "status <> 2 OR purged_time IS NULL"
    ]
  }

ctDocument :: CompositeType
ctDocument = CompositeType {
  ctName = "document"
, ctColumns = [
    CompositeColumn { ccName = "id", ccType = BigIntT }
  , CompositeColumn { ccName = "title", ccType = TextT }
  , CompositeColumn { ccName = "signatory_links", ccType = ArrayT $ CustomT "signatory_link" }
  , CompositeColumn { ccName = "main_files", ccType = ArrayT $ CustomT "main_file" }
  , CompositeColumn { ccName = "status", ccType = SmallIntT }
  , CompositeColumn { ccName = "type", ccType = SmallIntT }
  , CompositeColumn { ccName = "ctime", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "mtime", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "days_to_sign", ccType = IntegerT }
  , CompositeColumn { ccName = "days_to_remind", ccType = IntegerT }
  , CompositeColumn { ccName = "timeout_time", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "expiration_date", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "invite_time", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "invite_ip", ccType = IntegerT }
  , CompositeColumn { ccName = "invite_text", ccType = TextT }
  , CompositeColumn { ccName = "confirm_text", ccType = TextT }
  , CompositeColumn { ccName = "show_header", ccType = BoolT }
  , CompositeColumn { ccName = "show_pdf_download", ccType = BoolT }
  , CompositeColumn { ccName = "show_reject_option", ccType = BoolT }
  , CompositeColumn { ccName = "allow_reject_reason", ccType = BoolT }
  , CompositeColumn { ccName = "show_footer", ccType = BoolT }
  , CompositeColumn { ccName = "is_receipt", ccType = BoolT }
  , CompositeColumn { ccName = "lang", ccType = SmallIntT }
  , CompositeColumn { ccName = "sharing", ccType = SmallIntT }
  , CompositeColumn { ccName = "tags", ccType = ArrayT $ CustomT "document_tag" }
  , CompositeColumn { ccName = "author_attachments", ccType = ArrayT $ CustomT "author_attachment" }
  , CompositeColumn { ccName = "api_v1_callback_url", ccType = TextT }
  , CompositeColumn { ccName = "api_v2_callback_url", ccType = TextT }
  , CompositeColumn { ccName = "unsaved_draft", ccType = BoolT }
  , CompositeColumn { ccName = "object_version", ccType = BigIntT }
  , CompositeColumn { ccName = "token", ccType = BigIntT }
  , CompositeColumn { ccName = "time_zone_name", ccType = TextT }
  , CompositeColumn { ccName = "author_company_id", ccType = BigIntT }
  , CompositeColumn { ccName = "status_class", ccType = SmallIntT }
  ]
}

---------------------------------

tableMainFiles :: Table
tableMainFiles = tblTable {
    tblName = "main_files"
  , tblVersion = 3
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "file_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "document_status", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "seal_status", colType = SmallIntT, colNullable = False }
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

ctMainFile :: CompositeType
ctMainFile = CompositeType {
  ctName = "main_file"
, ctColumns = [
    CompositeColumn { ccName = "file_id", ccType = BigIntT }
  , CompositeColumn { ccName = "document_status", ccType = SmallIntT }
  , CompositeColumn { ccName = "seal_status", ccType = SmallIntT }
  , CompositeColumn { ccName = "file_name", ccType = TextT }
  ]
}

---------------------------------

tableAuthorAttachments :: Table
tableAuthorAttachments = tblTable {
    tblName = "author_attachments"
  , tblVersion = 5
  , tblColumns = [
      tblColumn { colName = "file_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "required", colType = BoolT, colNullable = False }
    , tblColumn { colName = "add_to_sealed_file", colType = BoolT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumns ["file_id", "document_id", "name"]
  , tblForeignKeys = [
      fkOnColumn "file_id" "files" "id"
    , (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [
      indexOnColumn "document_id"
    , indexOnColumn "file_id"
    ]
  }

ctAuthorAttachment :: CompositeType
ctAuthorAttachment = CompositeType {
  ctName = "author_attachment"
, ctColumns = [
    CompositeColumn { ccName = "name", ccType = TextT }
  , CompositeColumn { ccName = "required", ccType = BoolT }
  , CompositeColumn { ccName = "add_to_sealed_file", ccType = BoolT }
  , CompositeColumn { ccName = "file_id", ccType = BigIntT }

  ]
}

---------------------------------

tableSignatoryAttachments :: Table
tableSignatoryAttachments = tblTable {
    tblName = "signatory_attachments"
  , tblVersion = 8
  , tblColumns = [
      tblColumn { colName = "file_id", colType = BigIntT }
    , tblColumn { colName = "description", colType = TextT, colNullable = False }
    , tblColumn { colName = "name", colType = TextT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False, colDefault = Just "0" }
    ]
  , tblPrimaryKey = pkOnColumns ["signatory_link_id", "name"]
  , tblForeignKeys = [
      fkOnColumn "file_id" "files" "id"
    , (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [
      indexOnColumn "file_id"
    , indexOnColumn "signatory_link_id"
    ]
  }

ctSignatoryAttachment :: CompositeType
ctSignatoryAttachment = CompositeType {
  ctName = "signatory_attachment"
, ctColumns = [
    CompositeColumn { ccName = "file_id", ccType = BigIntT }
  , CompositeColumn { ccName = "filename", ccType = TextT }
  , CompositeColumn { ccName = "description", ccType = TextT }
  , CompositeColumn { ccName = "name", ccType = TextT }
  ]
}

---------------------------------

tableSignatoryLinks :: Table
tableSignatoryLinks = tblTable {
    tblName = "signatory_links"
  , tblVersion = 31
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
    , tblColumn { colName = "deleted", colType = TimestampWithZoneT }
    , tblColumn { colName = "really_deleted", colType = TimestampWithZoneT }
    , tblColumn { colName = "csv_contents", colType = TextT }
    , tblColumn { colName = "sign_redirect_url", colType = TextT }
    , tblColumn { colName = "is_partner", colType = BoolT, colNullable = False }
    , tblColumn { colName = "rejection_time", colType = TimestampWithZoneT }
    , tblColumn { colName = "rejection_reason", colType = TextT }
    , tblColumn { colName = "authentication_to_sign_method", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "delivery_method", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "mail_invitation_delivery_status", colType = SmallIntT, colNullable = False, colDefault = Just "3" }
    , tblColumn { colName = "sms_invitation_delivery_status", colType = SmallIntT, colNullable = False, colDefault = Just "3" }
    , tblColumn { colName = "reject_redirect_url", colType = TextT }
    , tblColumn { colName = "confirmation_delivery_method", colType = SmallIntT, colNullable = False, colDefault = Just "1" }
    , tblColumn { colName = "authentication_to_view_method", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "allows_highlighting", colType = BoolT, colNullable = False, colDefault = Just "false" }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "document_id" "documents" "id") {fkOnDelete = ForeignKeyCascade }
    , fkOnColumn "user_id" "users" "id"
    ]
  , tblIndexes = [
      indexOnColumn "user_id"
    , indexOnColumn "document_id"
      -- Needed for the corresponding foreign key in documents table
      -- (PostgreSQL whimsy).
    , uniqueIndexOnColumns ["id", "document_id"]
    ]
  }

ctSignatoryLink :: CompositeType
ctSignatoryLink = CompositeType {
  ctName = "signatory_link"
, ctColumns = [
    CompositeColumn { ccName = "id", ccType = BigIntT }
  , CompositeColumn { ccName = "signatory_fields", ccType = ArrayT $ CustomT "signatory_field" }
  , CompositeColumn { ccName = "is_author", ccType = BoolT }
  , CompositeColumn { ccName = "is_partner", ccType = BoolT }
  , CompositeColumn { ccName = "sign_order", ccType = IntegerT }
  , CompositeColumn { ccName = "token", ccType = BigIntT }
  , CompositeColumn { ccName = "user_id", ccType = BigIntT }
  , CompositeColumn { ccName = "sign_time", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "sign_ip", ccType = IntegerT }
  , CompositeColumn { ccName = "seen_time", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "seen_ip", ccType = IntegerT }
  , CompositeColumn { ccName = "read_invitation", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "mail_invitation_delivery_status", ccType = SmallIntT }
  , CompositeColumn { ccName = "sms_invitation_delivery_status", ccType = SmallIntT }
  , CompositeColumn { ccName = "deleted", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "really_deleted", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "csv_contents", ccType = TextT }
  , CompositeColumn { ccName = "attachments", ccType = ArrayT $ CustomT "signatory_attachment" }
  , CompositeColumn { ccName = "highlighted_pages", ccType = ArrayT $ CustomT "highlighted_page" }
  , CompositeColumn { ccName = "sign_redirect_url", ccType = TextT }
  , CompositeColumn { ccName = "reject_redirect_url", ccType = TextT }
  , CompositeColumn { ccName = "rejection_time", ccType = TimestampWithZoneT }
  , CompositeColumn { ccName = "rejection_reason", ccType = TextT }
  , CompositeColumn { ccName = "authentication_to_view_method", ccType = SmallIntT }
  , CompositeColumn { ccName = "authentication_to_sign_method", ccType = SmallIntT }
  , CompositeColumn { ccName = "delivery_method", ccType = SmallIntT }
  , CompositeColumn { ccName = "confirmation_delivery_method", ccType = SmallIntT }
  , CompositeColumn { ccName = "allows_highlighting", ccType = BoolT}
  , CompositeColumn { ccName = "has_identified_to_view", ccType = BoolT }
  ]
}

---------------------------------

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
      (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [indexOnColumn "document_id"]
  }

ctDocumentTag :: CompositeType
ctDocumentTag = CompositeType {
  ctName = "document_tag"
, ctColumns = [
    CompositeColumn { ccName = "name", ccType = TextT }
  , CompositeColumn { ccName = "value", ccType = TextT }
  ]
}

---------------------------------

tableFieldPlacements :: Table
tableFieldPlacements = tblTable {
    tblName = "field_placements"
  , tblVersion = 2
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
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "signatory_field_id" "signatory_link_fields" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [
      indexOnColumn "signatory_field_id"
    ]
  }

ctFieldPlacement :: CompositeType
ctFieldPlacement = CompositeType {
    ctName = "field_placement"
  , ctColumns = [
      CompositeColumn { ccName = "id", ccType = BigIntT }
    , CompositeColumn { ccName = "xrel", ccType = DoubleT }
    , CompositeColumn { ccName = "yrel", ccType = DoubleT }
    , CompositeColumn { ccName = "wrel", ccType = DoubleT }
    , CompositeColumn { ccName = "hrel", ccType = DoubleT }
    , CompositeColumn { ccName = "fsrel", ccType = DoubleT }
    , CompositeColumn { ccName = "page", ccType = IntegerT }
    , CompositeColumn { ccName = "tip", ccType = SmallIntT }
    , CompositeColumn { ccName = "anchors", ccType = ArrayT $ CustomT "placement_anchor" }
    ]
  }

---------------------------------

tablePlacementAnchors :: Table
tablePlacementAnchors = tblTable {
    tblName = "placement_anchors"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "field_placement_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "text", colType = TextT, colNullable = False }
    , tblColumn { colName = "index", colType = IntegerT, colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "field_placement_id" "field_placements" "id") { fkOnDelete = ForeignKeyCascade }
    ]
  , tblIndexes = [
      indexOnColumn "field_placement_id"
    ]
  }

ctPlacementAnchor :: CompositeType
ctPlacementAnchor = CompositeType {
    ctName = "placement_anchor"
  , ctColumns = [
      CompositeColumn { ccName = "text", ccType = TextT }
    , CompositeColumn { ccName = "index", ccType = IntegerT }
    ]
  }

---------------------------------

tableSignatoryLinkFields :: Table
tableSignatoryLinkFields = tblTable {
    tblName = "signatory_link_fields"
  , tblVersion = 12
  , tblColumns = [
      tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "custom_name", colType = TextT, colNullable = False, colDefault = Just "''::text" }
    , tblColumn { colName = "value_text", colType = TextT }
    , tblColumn { colName = "is_author_filled", colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "obligatory", colType = BoolT, colNullable = False, colDefault = Just "true" }
    , tblColumn { colName = "should_be_filled_by_author", colType = BoolT, colNullable = False, colDefault = Just "false" }
    , tblColumn { colName = "name_order", colType = SmallIntT }
   ,  tblColumn { colName = "value_bool", colType = BoolT }
   ,  tblColumn { colName = "value_file_id", colType = BigIntT }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblChecks = [
        Check "check_signatory_link_fields_name_fields_are_well_defined"
          "type = 1 AND name_order IS NOT NULL AND value_bool IS NULL AND value_file_id IS NULL AND value_text IS NOT NULL OR type <> 1"
      , Check "check_signatory_link_fields_signatures_are_well_defined"
          "type = 8 AND name_order IS NULL AND value_bool IS NULL AND value_text IS NULL OR type <> 8"
      , Check "check_signatory_link_fields_checkboxes_are_well_defined"
          "type = 9 AND name_order IS NULL AND value_bool IS NOT NULL AND value_file_id IS NULL AND value_text IS NULL OR type <> 9"
      , Check "check_signatory_link_fields_other_text_fields_are_well_defined"
          "(type = ANY (ARRAY[3, 4, 5, 6, 7, 10])) AND name_order IS NULL AND value_bool IS NULL AND value_file_id IS NULL AND value_text IS NOT NULL OR NOT (type = ANY (ARRAY[3, 4, 5, 6, 7, 10]))"
    ]
  , tblForeignKeys = [
        (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
      , (fkOnColumn "value_file_id" "files" "id")
    ]
  , tblIndexes = [
      uniqueIndexOnColumns ["signatory_link_id", "type", "name_order", "custom_name"]
    -- For efficient connection of documents to a new user.
    , (indexOnColumn "value_text") {
          idxWhere = Just "type = 6" -- email
        }
    ]
  }

ctSignatoryField :: CompositeType
ctSignatoryField = CompositeType {
  ctName = "signatory_field"
, ctColumns = [
    CompositeColumn { ccName = "id", ccType = BigIntT }
  , CompositeColumn { ccName = "type", ccType = SmallIntT }
  , CompositeColumn { ccName = "name_order", ccType = SmallIntT }
  , CompositeColumn { ccName = "custom_name", ccType = TextT }
  , CompositeColumn { ccName = "is_author_filled", ccType = BoolT }
  , CompositeColumn { ccName = "value_text", ccType = TextT }
  , CompositeColumn { ccName = "value_bool", ccType = BoolT }
  , CompositeColumn { ccName = "value_file_id", ccType = BigIntT }
  , CompositeColumn { ccName = "obligatory", ccType = BoolT }
  , CompositeColumn { ccName = "should_be_filled_by_author", ccType = BoolT }
  , CompositeColumn { ccName = "placements", ccType = ArrayT $ CustomT "field_placement" }
  ]
}

---------------------------------

tableSignatoryScreenshots :: Table
tableSignatoryScreenshots = tblTable {
    tblName = "signatory_screenshots"
  , tblVersion = 2
  , tblColumns = [
      tblColumn { colName = "id",                colType = BigSerialT,         colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT,            colNullable = False }
    , tblColumn { colName = "type",              colType = TextT,              colNullable = False }
    , tblColumn { colName = "time",              colType = TimestampWithZoneT, colNullable = False }
    , tblColumn { colName = "file_id",           colType = BigIntT,            colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
    , fkOnColumn "file_id" "files" "id"
    ]
  , tblIndexes = [
      indexOnColumn "signatory_link_id"
    , indexOnColumn "file_id"
    ]
  }

---------------------------------

tableHighlightedPages :: Table
tableHighlightedPages = tblTable {
    tblName = "highlighted_pages"
  , tblVersion = 1
  , tblColumns = [
      tblColumn { colName = "id",                colType = BigSerialT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT,    colNullable = False }
    , tblColumn { colName = "page",              colType = IntegerT,  colNullable = False }
    , tblColumn { colName = "file_id",           colType = BigIntT,    colNullable = False }
    ]
  , tblPrimaryKey = pkOnColumn "id"
  , tblForeignKeys = [
      (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade }
    , fkOnColumn "file_id" "files" "id"

    ]
  , tblIndexes = [
      uniqueIndexOnColumns ["signatory_link_id", "page"]
    , indexOnColumn "signatory_link_id"
    , indexOnColumn "file_id"
    ]
  }

---------------------------------

ctHighlightedPage:: CompositeType
ctHighlightedPage = CompositeType {
    ctName = "highlighted_page"
  , ctColumns = [
      CompositeColumn { ccName = "page", ccType = IntegerT }
    , CompositeColumn { ccName = "file_id", ccType = BigIntT }
    ]
  }

---------------------------------
