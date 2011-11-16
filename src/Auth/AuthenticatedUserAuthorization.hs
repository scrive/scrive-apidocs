module Auth.AuthenticatedUserAuthorization where

import User.Model
import Util.SignatoryLinkUtils
import Auth.DocumentPrivilege
import Auth.UserPrivilege
import Data.Semantic

-- | An authorization wrapper for a user's base permissions
data AuthenticatedUserAuthorization = AuthenticatedUserAuthorization UserID

instance DocumentAuthorization AuthenticatedUserAuthorization where
  canDocument (AuthenticatedUserAuthorization uid) doc _ DocumentSend = 
    hasSigLinkFor (And SignatoryAuthor uid) doc
  canDocument (AuthenticatedUserAuthorization uid) doc _ DocumentSign =
    -- is this correct?
    hasSigLinkFor (And SignatoryPartner uid) doc
  canDocument (AuthenticatedUserAuthorization uid) doc _ DocumentView =
    hasSigLinkFor uid doc
    
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentSend did =
    ("(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND user_id = ? AND document_id = ?)", 
     [toSql [SignatoryAuthor]
     ,toSql [SignatoryAuthor, SignatoryPartner]
     ,toSql uid
     ,toSql did])
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentSign did = 
    ("(SELECT count(*) FROM signatory_links WHERE roles = ? AND user_id = ? AND document_id = ?)", 
     [toSql [SignatoryPartner]
     ,toSql uid
     ,toSql did])
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentView did = 
    ("(SELECT count(*) FROM signatory_links WHERE user_id = ? AND document_id = ?)", 
     [toSql uid,
      toSql did])
  
instance UserAuthorization AuthenticatedUserAuthorization where
  canUser (AuthenticatedUserAuthorization uid) uid2 _ _ = uid == uid2
