
module Doc.Process where

import Doc.DocStateData
--import Auth.DocumentPrivilege
import DB.Classes
import MinutesTime
import Database.HDBC
--import Data.Semantic
--import Control.Monad
import DB.Utils
import File.FileID
--import Data.List
import SQL.Builder
--import Auth.DocumentAuthorization
import Auth.DocumentAuthorization
import Auth.DocumentPrivilege

data DocumentAuthorization a => DocumentsUpdate a = 
  DocumentsUpdate { docUpdateFields :: [SetField],
                    docPrivilege    :: DocumentPrivilege,
                    docUpdateWhere  :: Maybe WhereClause,
                    docUpdateTime   :: MinutesTime,
                    docUpdateID     :: DocumentID,
                    docAuth         :: a
                  }
  
instance DocumentAuthorization a => DBUpdate (DocumentsUpdate a) Bool where
  dbUpdate a = wrapDB $ \conn -> do
    let (s, vs) = stringFromUpdateStatement $ 
                  UpdateStatement { usTableName = "documents"
                                  , usSetFields = (SetField "mtime = ?" (toSql $ docUpdateTime a)):(docUpdateFields a)
                                  , usWhereClause = combineWhereAnd 
                                                    (Just $ WhereSimple "id = ?" [toSql $ docUpdateID a]) $
                                                    combineWhereAnd
                                                    (Just $ docSqlWhere (docAuth a) (docPrivilege a) (docUpdateID a) (docUpdateTime a))
                                                    (docUpdateWhere a)
                                  }
    r <- run conn s vs
    oneRowAffectedGuard r

setDocumentTitle :: DocumentAuthorization a => DocumentID -> String -> MinutesTime -> a -> DocumentsUpdate a
setDocumentTitle did title mt a = DocumentsUpdate 
                                  [SetField "title = ?" (toSql title)]
                                  DocumentEdit
                                  Nothing
                                  mt
                                  did
                                  a
  
attachDocumentFile :: DocumentAuthorization a => DocumentID -> FileID -> MinutesTime -> a -> DocumentsUpdate a
attachDocumentFile did fid mt a = DocumentsUpdate 
                                  [SetField "file_id = ?" (toSql fid)]
                                  DocumentEdit
                                  (Just $ WhereSimple "NULL file_id" [])
                                  mt
                                  did
                                  a
  
data SignatoriesUpdate = SignatoriesUpdate { sigUpdateFields :: [SetField],
                                             sigUpdateWhere  :: Maybe WhereClause,
                                             sigUpdateTime   :: MinutesTime,
                                             sigUpdateID     :: DocumentID,
                                             sigUpdateSigID  :: SignatoryLinkID
                                           }

instance DBUpdate SignatoriesUpdate Bool where
  dbUpdate a = wrapDB $ \conn -> do
    let (s1, vs1) = stringFromUpdateStatement $
                    UpdateStatement { usTableName = "signatory_links"
                                    , usSetFields = sigUpdateFields a
                                    , usWhereClause = combineWhereAnd (Just $ WhereSimple "id = ?" [toSql $ sigUpdateSigID a]) $
                                                      combineWhereAnd (Just $ WhereSimple "document_id = ?" [toSql $ sigUpdateID a]) $ 
                                                      sigUpdateWhere a
                                  }
    let (s2, vs2) = stringFromUpdateStatement $ 
                    UpdateStatement { usTableName = "documents"
                                    , usSetFields = [SetField "mtime = ?" (toSql $ sigUpdateTime a)]
                                    , usWhereClause = Just $ WhereSimple "id = ?" [toSql $ sigUpdateID a]
                                    }
    r1 <- run conn s1 vs1
    r <- oneRowAffectedGuard r1
    if r
      then oneRowAffectedGuard =<< run conn s2 vs2
      else return False

setSignatoryRoles :: DocumentID -> SignatoryLinkID -> [SignatoryRole] -> MinutesTime -> SignatoriesUpdate
setSignatoryRoles did slid srs mt = SignatoriesUpdate
                                    [SetField "roles = ?" $ toSql srs]
                                    Nothing
                                    mt
                                    did
                                    slid
