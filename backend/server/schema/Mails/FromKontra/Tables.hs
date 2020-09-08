module Mails.FromKontra.Tables
  ( tableKontraInfoForMails
  , ctKontraForMailAggregate
  ) where

import DB

tableKontraInfoForMails :: Table
tableKontraInfoForMails = tblTable
  { tblName        = "kontra_info_for_mails"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "mail_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "mail_type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = True }
    ]
  , tblPrimaryKey  = pkOnColumns ["mail_id", "document_id"]
  , tblForeignKeys =
    [ (fkOnColumn "mail_id" "mails" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  , tblIndexes     = [ indexOnColumn "mail_id"
                     , indexOnColumn "document_id"
                     , indexOnColumn "signatory_link_id"
                     ]
  }

ctKontraForMailAggregate :: CompositeType
ctKontraForMailAggregate = CompositeType
  { ctName    = "kontra_for_mail_aggregate_c1"
  , ctColumns = [ CompositeColumn { ccName = "mail_type", ccType = SmallIntT }
                , CompositeColumn { ccName = "document_id", ccType = BigIntT }
                , CompositeColumn { ccName = "signatory_link_id", ccType = BigIntT }
                ]
  }
