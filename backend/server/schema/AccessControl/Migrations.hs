module AccessControl.Migrations where

import Database.PostgreSQL.PQTypes.Checks

import AccessControl.Tables
import DB

createTableAccessControl :: MonadDB m => Migration m
createTableAccessControl =
  Migration
  {
    mgrTableName = tblName tableAccessControl
  , mgrFrom = 0
  , mgrAction = StandardMigration . createTable True $ tblTable
    { tblName = "access_control"
    , tblVersion = 1
    , tblColumns =
      [ tblColumn { colName = "id", colType = BigSerialT, colNullable = False }
      , tblColumn { colName = "role", colType = SmallIntT, colNullable = False }
      , tblColumn { colName = "src_user_id", colType = BigIntT, colNullable = True }
      , tblColumn { colName = "src_user_group_id", colType = BigIntT, colNullable = True }
      , tblColumn { colName = "trg_user_id", colType = BigIntT, colNullable = True }
      , tblColumn { colName = "trg_user_group_id", colType = BigIntT, colNullable = True }
      ]
    , tblPrimaryKey = pkOnColumn "id"
    , tblIndexes =
        [ indexOnColumn "src_user_id"
        , indexOnColumn "src_user_group_id"
        , indexOnColumn "trg_user_id"
        , indexOnColumn "trg_user_group_id"
        ]
    , tblForeignKeys =
        [ (fkOnColumn "src_user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
        , (fkOnColumn "src_user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
        , (fkOnColumn "trg_user_id" "users" "id") { fkOnDelete = ForeignKeyCascade }
        , (fkOnColumn "trg_user_group_id" "user_groups" "id") { fkOnDelete = ForeignKeyCascade }
        ]
    , tblChecks =
      [
        -- exactly 1 src is NOT NULL
        Check "check_access_control_exactly_one_src"
              "src_user_id IS NOT NULL AND src_user_group_id IS \  \NULL \
          \OR src_user_id IS \  \NULL AND src_user_group_id IS NOT NULL"
        -- exactly 1 trg is NOT NULL
      , Check "check_access_control_exactly_one_trg"
              "trg_user_id IS NOT NULL AND trg_user_group_id IS \  \NULL \
          \OR trg_user_id IS \  \NULL AND trg_user_group_id IS NOT NULL"
        -- UserAR has UserID trg
      , Check "check_access_control_valid_user_ar"
              "role = 0 AND trg_user_id IS NOT NULL OR role <> 0"
        -- UserGroupMemberAR has UserGroupID trg
      , Check "check_access_control_valid_user_group_member_ar"
              "role = 1 AND trg_user_group_id IS NOT NULL OR role <> 1"
        -- UserAdminAR has UserGroupID trg
      , Check "check_access_control_valid_user_admin_ar"
              "role = 2 AND trg_user_group_id IS NOT NULL OR role <> 2"
        -- UserGroupAdminAR has UserGroupID trg
      , Check "check_access_control_valid_user_group_admin_ar"
              "role = 3 AND trg_user_group_id IS NOT NULL OR role <> 3"
      ]
    }
  }

migratePartnerAdmins :: MonadDB m => Migration m
migratePartnerAdmins = Migration
  { mgrTableName = tblName tableAccessControl
  , mgrFrom = 1
  , mgrAction = StandardMigration $ do
      runSQL_ $ "INSERT INTO access_control \
                  \(role, src_user_id, trg_user_group_id) \
                \SELECT 3, pa.user_id, p.user_group_id \
                  \FROM partner_admins as pa \
                  \JOIN partners as p \
                  \ON p.id = pa.partner_id"
  }

accesscontrolBumpVersionAfterDropingPartnerAdmins :: MonadDB m => Migration m
accesscontrolBumpVersionAfterDropingPartnerAdmins = Migration {
    mgrTableName = tblName tableAccessControl
  , mgrFrom = 2
  , mgrAction = StandardMigration $ return ()
  }
