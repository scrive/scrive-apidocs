module EID.Nets.Migrations (
    createNetsSignOrdersTable
  , netsSignOrdersDropSSN
  , netsSignOrdersAddProviderAndSSN
  ) where

import Database.PostgreSQL.PQTypes.Checks

import DB
import EID.Nets.Tables

createNetsSignOrdersTable :: MonadDB m => Migration m
createNetsSignOrdersTable = Migration
  { mgrTableName = tblName tableNetsSignOrders
  , mgrFrom      = 0
  , mgrAction    =
    StandardMigration $ createTable
      True
      tblTable
        { tblName        = "nets_sign_orders"
        , tblVersion     = 1
        , tblColumns     =
          [ tblColumn { colName     = "signatory_link_id"
                      , colType     = BigIntT
                      , colNullable = False
                      }
          , tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
          , tblColumn { colName     = "text_to_be_signed"
                      , colType     = TextT
                      , colNullable = False
                      }
          , tblColumn { colName = "order_id", colType = TextT, colNullable = False }
          , tblColumn { colName = "ssn", colType = TextT, colNullable = False }
          , tblColumn { colName     = "deadline"
                      , colType     = TimestampWithZoneT
                      , colNullable = False
                      }
          , tblColumn { colName = "is_canceled", colType = BoolT, colNullable = False }
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
        }
  }

netsSignOrdersDropSSN :: MonadDB m => Migration m
netsSignOrdersDropSSN = Migration
  { mgrTableName = tblName tableNetsSignOrders
  , mgrFrom      = 1
  , mgrAction    = StandardMigration $ runQuery_ $ sqlAlterTable
                     (tblName tableNetsSignOrders)
                     [sqlDropColumn "ssn"]
  }

netsSignOrdersAddProviderAndSSN :: MonadDB m => Migration m
netsSignOrdersAddProviderAndSSN = Migration
  { mgrTableName = tblName tableNetsSignOrders
  , mgrFrom      = 2
  , mgrAction    =
    StandardMigration $ do
      runQuery_ $ sqlAlterTable
        (tblName tableNetsSignOrders)
        [ sqlAddColumn
          $ tblColumn { colName = "provider", colType = SmallIntT, colNullable = True }
        , sqlAddColumn
          $ tblColumn { colName = "ssn", colType = TextT, colNullable = True }
        ]
      -- set the provider for current orders, which are all Norwegian
      runSQL_ $ "UPDATE nets_sign_orders SET provider = 1"
      runQuery_ $ sqlAlterTable
        (tblName tableNetsSignOrders)
        [ sqlAlterColumn "provider" "SET NOT NULL"
        , sqlAddValidCheck $ tblCheck
          { chkName      = "check_nets_sign_orders_ssn_is_well_defined"
          , chkCondition =
              -- Norwegian Nets eSigning does not need SSN,
              -- but Danish Nets eSigning does.
            "provider = 1 AND ssn IS \  \NULL \
            \OR provider = 2 AND ssn IS NOT NULL"
          }
        ]
  }
