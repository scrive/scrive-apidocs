module EID.Nets.Tables where

import DB

-- | Nets signing transaction is unique for a given signatory. We do
-- | not really need to bind
-- | transaction to the session, but it gives us garbage collection
-- | of old transactions for free.

tableNetsSignOrders :: Table
tableNetsSignOrders = tblTable
  { tblName        = "nets_sign_orders"
  , tblVersion     = 3
  , tblColumns     =
    [ tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "text_to_be_signed", colType = TextT, colNullable = False }
    , tblColumn { colName = "order_id", colType = TextT, colNullable = False }
    , tblColumn { colName     = "deadline"
                , colType     = TimestampWithZoneT
                , colNullable = False
                }
    , tblColumn { colName = "is_canceled", colType = BoolT, colNullable = False }
    , tblColumn { colName = "provider", colType = SmallIntT, colNullable = False }
    , tblColumn { colName = "ssn", colType = TextT, colNullable = True }
    ]
-- only one authentication per signatory. can be relaxed later if necessary.
  , tblPrimaryKey  = pkOnColumn "signatory_link_id"
  , tblForeignKeys =
    [ (fkOnColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  , tblIndexes     = [ indexOnColumn "session_id"
                     , (indexOnColumn "order_id") { idxUnique = True }
                     ]
  , tblChecks      =
    [ tblCheck
        { chkName      = "check_nets_sign_orders_ssn_is_well_defined"
        , chkCondition =
        -- Norwegian Nets eSigning does not need SSN, but Danish Nets eSigning
        -- does.
          "provider = 1 AND ssn IS \  \NULL \
        \OR provider = 2 AND ssn IS NOT NULL"
        }
    ]
  }
