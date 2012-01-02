module Auth.AuthenticatedUserAuthorization where

import User.Model
import Auth.DocumentPrivilege
import Auth.SignatoryLinkPrivilege
--import Auth.UserPrivilege
import Database.HDBC
import SQL.Builder
import Doc.DocState
import Auth.DocumentAuthorization
import Auth.SignatoryLinkAuthorization

-- | An authorization wrapper for a user's base permissions
data AuthenticatedUserAuthorization = AuthenticatedUserAuthorization UserID

instance DocumentAuthorization AuthenticatedUserAuthorization where
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentSend did _ =
    WhereReason (
      WhereSimple 
      "(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND user_id = ? AND document_id = ?)"
      [toSql [SignatoryAuthor]
      ,toSql [SignatoryAuthor, SignatoryPartner]
      ,toSql uid
      ,toSql did]
      )
    ("Authenticated User does not have author permissions to do DocumentSend; user: " ++ show uid ++ "; document: " ++ show did)
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentEdit did _ =
    WhereReason (
      WhereSimple 
      "(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND user_id = ? AND document_id = ?)"
      [toSql [SignatoryAuthor]
      ,toSql [SignatoryAuthor, SignatoryPartner]
      ,toSql uid
      ,toSql did]
      )
    ("Authenticated User does not have author permissions to do DocumentEdit; user: " ++ show uid ++ "; document: " ++ show did)
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentView did _ = 
    WhereReason (
      WhereSimple
      "(SELECT count(*) FROM signatory_links WHERE user_id = ? AND document_id = ?)"
      [toSql uid
      ,toSql did]
      )
    ("Authenticated User is not in signatory_links list to do DocumentView; user: " ++ show uid ++ "; document: " ++ show did)
  
instance SignatoryLinkAuthorization AuthenticatedUserAuthorization where
  sigSqlWhere (AuthenticatedUserAuthorization uid) DocumentSign sid did _ = 
    WhereReason (
      WhereSimple "(roles = ? OR roles = ? AND user_id = ?)" 
      [toSql [SignatoryPartner], toSql [SignatoryPartner, SignatoryAuthor], toSql uid])
    ("Cannot Sign Document because user is not a Partner; user: " ++ show uid ++ "; siglink: " ++ show sid ++ "; document: " ++ show did)
  sigSqlWhere (AuthenticatedUserAuthorization uid) SignatoryLinkEdit sid did _ =
    WhereReason (
      WhereSimple "user_id = ? OR (SELECT count(*) FROM signatory_link WHERE user_id = ? AND (roles = ? OR roles = ?))"
      [toSql uid, toSql uid, toSql [SignatoryPartner, SignatoryAuthor], toSql [SignatoryAuthor]])
    ("Cannot Edit Signatory Link because user is not the author or signatory is not a user; user: " ++ show uid ++ "; siglink: " ++ show sid ++ "; document: " ++ show did)
    
--instance UserAuthorization AuthenticatedUserAuthorization where
--  canUser (AuthenticatedUserAuthorization uid) uid2 _ _ = uid == uid2

