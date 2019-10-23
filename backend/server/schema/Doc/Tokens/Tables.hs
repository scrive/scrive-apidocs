module Doc.Tokens.Tables where

import DB

tableDocumentSessionTokens :: Table
tableDocumentSessionTokens = tblTable
  { tblName        = "document_session_tokens"
  , tblVersion     = 2
  , tblColumns     =
    [ tblColumn { colName = "session_id", colType = BigIntT, colNullable = False }
    , tblColumn { colName = "signatory_link_id", colType = BigIntT, colNullable = False }
    ]
  , tblPrimaryKey  = pkOnColumns ["session_id", "signatory_link_id"]
  , tblForeignKeys =
    [ (fkOnColumn "session_id" "sessions" "id") { fkOnDelete = ForeignKeyCascade }
    , (fkOnColumn "signatory_link_id" "signatory_links" "id") { fkOnDelete = ForeignKeyCascade
                                                              }
    ]
  , tblIndexes     = [indexOnColumn "session_id", indexOnColumn "signatory_link_id"]
  }
