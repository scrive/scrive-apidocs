module Auth.AuthenticatedUserAuthorization where

import User.Model
import Auth.DocumentPrivilege
--import Auth.UserPrivilege
import Database.HDBC
import SQL.Builder
import Doc.DocState
import Auth.DocumentAuthorization

-- | An authorization wrapper for a user's base permissions
data AuthenticatedUserAuthorization = AuthenticatedUserAuthorization UserID

instance DocumentAuthorization AuthenticatedUserAuthorization where
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentSend did _ =
    WhereSimple 
    "(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND user_id = ? AND document_id = ?)"
    [toSql [SignatoryAuthor]
    ,toSql [SignatoryAuthor, SignatoryPartner]
    ,toSql uid
    ,toSql did]
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentEdit did _ =
    WhereSimple 
    "(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND user_id = ? AND document_id = ?)"
    [toSql [SignatoryAuthor]
    ,toSql [SignatoryAuthor, SignatoryPartner]
    ,toSql uid
    ,toSql did]
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentView did _ = 
    WhereSimple
    "(SELECT count(*) FROM signatory_links WHERE user_id = ? AND document_id = ?)"
    [toSql uid
    ,toSql did]
  
instance SignatoryLinkAuthorization AuthenticatedUserAuthorization where
  sigSqlWhere (AuthenticatedUserAuthorization uid) DocumentSign sid did _ = 
    WhereSimple "(roles = ? OR roles = ? AND user_id = ?)" 
    [toSql [SignatoryPartner] ,toSql [SignatoryPartner, SignatoryAuthor], toSql uid]
  sigSqlWhere (AuthenticatedUserAuthorization uid) DocumentSign sid did _ =
    WhereSimple "user_id = ? OR (SELECT count(*) FROM signatory_link WHERE roles = ? OR roles = ?)"
    [toSql [SignatoryPartner], toSql [SignatoryPartner, SignatoryAuthor], toSql [SignatoryAuthor]]

--instance UserAuthorization AuthenticatedUserAuthorization where
--  canUser (AuthenticatedUserAuthorization uid) uid2 _ _ = uid == uid2

