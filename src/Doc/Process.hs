
module Doc.Process where

import Doc.DocStateData
import Auth.DocumentPrivilege
import DB.Classes
import MinutesTime
import Database.HDBC
import Data.Semantic
import Control.Monad
import DB.Utils
import File.FileID
import Data.List
--import Auth.DocumentAuthorization


data WhereClause = WhereSimple String [SqlValue]
                 | WhereAnd WhereClause WhereClause
                 | WhereOr WhereClause WhereClause

stringFromWhere :: WhereClause -> (String, [SqlValue])
stringFromWhere (WhereSimple s vs) = (s, vs)
stringFromWhere (WhereAnd a b) = 
  let (s1, vs1) = stringFromWhere a
      (s2, vs2) = stringFromWhere b
  in ("(" ++ s1 ++ " AND " ++ s2 ++ ")", vs1 ++ vs2)
stringFromWHere (WhereOr  a b) = 
  let (s1, vs1) = stringFromWhere a
      (s2, vs2) = stringFromWhere b
  in ("(" ++ s1 ++ " OR  " ++ s2 ++ ")", vs1 ++ vs2)


data UpdateStatement = UpdateStatement { usTableName :: String
                                       , usSetFields :: [SetField]
                                       , usWhereClause :: Maybe WhereClause
                                       }
                       
stringFromUpdateStatement :: UpdateStatement -> (String, [SqlValue])
stringFromUpdateStatement (UpdateStatement{ usTableName, usSetFields, usWhereClause }) =
  let (sfs, sfvs) = unzip $ map stringFromSetField usSetFields
      (ws, wvs) = stringFromWhere usWhereClause
  in (" UPDATE " ++ usTableName ++ 
      " SET " ++ intercalate ", " sfs ++
      " WHERE " ++ ws,
      sfvs ++ wvs)

data SetField = SetField String SqlValue

stringFromSetField :: SetField -> (String, SqlValue)
stringFromSetField (SetField r v) = (r, v)

combineWhereAnd :: Maybe WhereClause -> Maybe WhereClause -> Maybe WhereClause
combineWhereAnd Nothing Nothing = Nothing
combineWhereAnd Nothing a = a
combineWhereAnd a Nothing = a
combineWhereAnd (Just a) (Just b) = Just $ WhereAnd a b
  
class DocumentsUpdate a where
  docUpdateFields :: a -> [SetField]
  docUpdateWhere  :: a -> Maybe WhereClause
  docUpdateTime   :: a -> MinutesTime  
  docUpdateID     :: a -> DocumentID
  
instance DocumentsUpdate a => DBUpdate a Bool where
  dbUpdate a = wrapDB $ \conn -> do
    let (s, vs) = stringFromUpdateStatement $ 
                  UpdateStatement { usTableName = "documents"
                                  , usSetFields = (SetField "mtime = to_timestamp(?)" (toSql $ docUpdateTime a)):(docUpdateFields a)
                                  , usWhereClause = combineWhereAnd (Just $ WhereSimple "id = ?" (toSql $ docUpdateID a)) (docUpdateWhere a)
                                  }
    r <- run conn s vs
    oneRowAffectedGuard r

data SetDocumentTitle = SetDocumentTitle DocumentID String MinutesTime
instance DocumentsUpdate SetDocumentTitle where
  docUpdateFields (SetDocumentTitle _ t _) = [SetField "title = ?" (toSql t)]
  docUpdateWhere _ = Nothing
  docUpdateTime (SetDocumentTitle _ _ mt) = mt
  docUpdateID (SetDocumentTitle did _ _) = did
  
data AttachDocumentFile = AttachDocumentFile DocumentID FileID MinutesTime
instance DocumentsUpdate AttachDocumentFile where
  docUpdateFields (AttachDocumentFile _ f _) = [SetField "file_id = ?" (toSql f)]
  docUpdateWhere _ = Just $ WhereSimple "NULL file_id" []
  docUpdateTime (AttachDocumentFile _ _ mt) = mt
  docUpdateID (AttachDocumentFile did _ _) = did
  
class SignatoriesUpdate a where
  sigUpdateFields :: a -> [SetField]
  sigUpdateWhere :: a -> Maybe WhereClause
  sigUpdateTime :: a -> MinutesTime
  sigUpdateID :: a -> DocumentID
  sigUpdateSigID :: a -> SignatoryLinkID

instance SignatoriesUpdate a => DBUpdate a Bool where
  dbUpdate a = wrapDB $ \conn -> do
    let (s1, vs1) = stringFromUpdateStatement $
                  UpdateStatement { usTableName = "signatory_links"
                                  , usSetFields = sigUpdateFields a
                                  , usWhereClause = combineWhereAnd (Just $ WhereSimple "id = ?" (toSql $ sigUpdateSigID)) $
                                                    combineWhereAnd (Just $ WhereSimple "document_id = ?" (toSql $ sigUpdateID))
                                                                    sigUpdateWhere a
                                  }
    let (s2, vs2) = stringFromUpdateStatement $ 
                  UpdateStatement { usTableName = "documents"
                                  , usSetFields = [SetField "mtime = to_timestamp(?)" (toSql $ sigUpdateTime a)]
                                  , usWhereClause = Just $ WhereSimple "id = ?" (toSql $ sigUpdateID a)
                                  }
    r1 <- run conn s1 vs1
    r <- oneRowAffectedGuard r1
    if r
      then oneRowAffectedGuard =<< run conn s2 vs2
      else return False

data SetSignatoryRoles = SetSignatoryRoles DocumentID SignatoryLinkID [SignatoryRole] MinutesTime
instance SignatoriesUpdate SetSignatoryRoles where
  sigUpdateFields (SetSignatoryRoles _ _ rs _) = [SetField "roles = ?" $ toSql rs]
  sigUpdateWhere _ = Nothing
  sigUpdateTime (SetSignatoryRoles _ _ _ mt) = mt
  sigUpdateID (SetSignatoryRoles did _ _ _) = did
  sigUpdateSigID (SetSignatoryRoles _ sid _ _) = sid

 -- examples
{-
data SetDocumentTitle a = SetDocumentTitle DocumentID String MinutesTime a

instance DocumentOperation (SetDocumentTitle a) Bool where
  docOp (SetDocumentTitle _ _ _ _) = do
    undefined -- do your sql; you can assume locks on documents and signatory_links
  docOpDocumentID  (SetDocumentTitle did _ _ _) = did
  docOpMinutesTime (SetDocumentTitle _ _ mt _ ) = mt
  docOpPrivilege _ = DocumentEdit
  
instance DocumentAuthorization a => TakesDocumentAuthorization (SetDocumentTitle a) a where
  getDocumentAuthorization (SetDocumentTitle _ _ _ a) = a
  
instance HasTypeStatusMatcher (SetDocumentTitle a) DocumentStatus where
  getTypeStatusMatcher _ = Preparation -- only allow this 

-- A small "DSL" to match documenttypes and documentstatuses

class TypeStatusMatcher a where
  tsMatch :: a -> DocumentType -> DocumentStatus -> Bool
  
instance TypeStatusMatcher DocumentStatus where
  tsMatch ds _ s = ds == s
  
instance TypeStatusMatcher DocumentType where
  tsMatch ts t _ = ts == t
  
instance (TypeStatusMatcher a, TypeStatusMatcher b) => TypeStatusMatcher (And a b) where
  tsMatch (And a b) t s = tsMatch a t s && tsMatch b t s
  
instance (TypeStatusMatcher a, TypeStatusMatcher b) => TypeStatusMatcher (Or a b) where
  tsMatch (Or a b) t s = tsMatch a t s || tsMatch b t s

instance (TypeStatusMatcher a) => TypeStatusMatcher (Not a) where
  tsMatch (Not a) t s = not $ tsMatch a t s

instance TypeStatusMatcher (DocumentType -> Bool) where  
  tsMatch p t _ = p t
  
instance TypeStatusMatcher (DocumentStatus -> Bool) where
  tsMatch p _ s = p s

-- define three new type classes

class DocumentAuthorization b => TakesDocumentAuthorization a b | a -> b where
  getDocumentAuthorization :: a -> b
  
class TypeStatusMatcher b => HasTypeStatusMatcher a b | a -> b where
  getTypeStatusMatcher :: a -> b
  
class DocumentOperation a b | a -> b where
  docOp :: a -> DB b -- the Database stuff; you can assume a lock on documents && signatorylinks
  docOpDocumentID  :: a -> DocumentID        -- the documentid to act on
  docOpMinutesTime :: a -> MinutesTime       -- the time of the operation
  docOpPrivilege   :: a -> DocumentPrivilege -- the privilege the operation falls under
  
-- we can then define a higher-level abstraction on DBUpdate
-- and now we make an instance for DBUpdate which performs the check
-- automatically
instance (HasTypeStatusMatcher a d, TakesDocumentAuthorization a c, DocumentOperation a b) => DBUpdate a b where
  dbUpdate a = do
    wrapDB $ \conn -> runRaw conn "LOCK TABLE documents IN ACCESS EXCLUSIVE MODE"
    wrapDB $ \conn -> runRaw conn "LOCK TABLE signatory_links IN ACCESS EXCLUSIVE MODE"
    -- replace the next line with SQL code to get the signatorylinks    
    -- siglinks <- getSigLinks $ docOpDocumentID a
    let siglinks :: [SignatoryLink] = []
    unless (canDocument (getDocumentAuthorization a) siglinks (docOpMinutesTime a) (docOpPrivilege a)) $
      error "Not authorized"
    -- replace next two lines with SQL code to get the status/type
    -- based on getDocumentID a
    -- (docstatus, doctype) <- getStatusAndType $ docOpDocumentID a
    let docstatus = Preparation
        doctype = Signable Contract
    unless (tsMatch (getTypeStatusMatcher a) doctype docstatus) $
      error "Operation is not allowed."
    docOp a

-}
