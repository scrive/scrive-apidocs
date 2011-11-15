{-# LANGUAGE FunctionalDependencies #-}
module Doc.Process where

import Doc.DocStateData
import Auth.DocumentPrivilege
import DB.Classes
import MinutesTime
import Database.HDBC
import Data.Semantic
import Control.Monad
import Auth.DocumentAuthorization

-- examples

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

