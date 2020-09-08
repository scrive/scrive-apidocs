module SMS.FromKontra.Tables
  ( tableKontraInfoForSMSes
  , ctKontraForSMSAggregate
  ) where

import DB

tableKontraInfoForSMSes :: Table
tableKontraInfoForSMSes = tblTable
  { tblName        = "kontra_info_for_smses"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "sms_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "sms_type", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "document_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = True }
    ]
  , tblPrimaryKey  = pkOnColumns ["sms_id", "document_id"]
  , tblForeignKeys =
    [ (fkOnColumn "sms_id" "smses" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "document_id" "documents" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  , tblIndexes     = [ indexOnColumn "sms_id"
                     , indexOnColumn "document_id"
                     , indexOnColumn "signatory_link_id"
                     ]
  }

ctKontraForSMSAggregate :: CompositeType
ctKontraForSMSAggregate = CompositeType
  { ctName    = "kontra_for_sms_aggregate_c1"
  , ctColumns = [ CompositeColumn { ccName = "sms_type", ccType = SmallIntT }
                , CompositeColumn { ccName = "document_id", ccType = BigIntT }
                , CompositeColumn { ccName = "signatory_link_id", ccType = BigIntT }
                ]
  }
