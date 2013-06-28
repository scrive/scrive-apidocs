module User.CallbackScheme.Tables where

import DB

tableUsersCallbackScheme :: Table
tableUsersCallbackScheme = tblTable {
     tblName = "user_callback_scheme"
   , tblVersion = 1
   , tblCreateOrValidate = \desc -> case desc of
        [  ("user_id", SqlColDesc {colType = SqlBigIntT, colNullable = Just False})
         , ("callback_scheme", SqlColDesc {colType = SqlVarCharT, colNullable = Just False})
         ] -> return TVRvalid
        [] -> do
          kRunRaw $ "CREATE TABLE user_callback_scheme ("
            <> "  user_id BIGINT NOT NULL"
            <> ", callback_scheme TEXT NOT NULL "
            <> ", CONSTRAINT user_callback_scheme_callback_scheme PRIMARY KEY (user_id)"
            <> ")"
          return TVRcreated
        _ -> return TVRinvalid
  , tblIndexes = [ ]
  , tblForeignKeys = [ (tblForeignKeyColumn "user_id" "users" "id")
                       { fkOnDelete = ForeignKeyCascade }
                     ]
  }
