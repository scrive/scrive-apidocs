module Auth.SigLinkMagicHashAuthorization where

import Auth.DocumentPrivilege
import DB.Types
import Doc.DocStateData
import Auth.DocumentAuthorization
--import Auth.UserAuthorization
import Database.HDBC
import SQL.Builder

-- | Authenticated with SignatoryLinkID and MagicHash combo
data SigLinkMagicHashAuthorization = SigLinkMagicHashAuthorization SignatoryLinkID MagicHash

instance DocumentAuthorization SigLinkMagicHashAuthorization where
  docSqlWhere (SigLinkMagicHashAuthorization sid mh) DocumentSend did _ =
    WhereSimple 
    "(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND id = ? AND token = ? AND document_id = ?)"
    [toSql [SignatoryAuthor]
    ,toSql [SignatoryAuthor, SignatoryPartner]
    ,toSql sid
    ,toSql mh
    ,toSql did]
  docSqlWhere (SigLinkMagicHashAuthorization sid mh) DocumentEdit did _ =
    WhereSimple 
    "(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND id = ? AND token = ? AND document_id = ?)"
    [toSql [SignatoryAuthor]
    ,toSql [SignatoryAuthor, SignatoryPartner]
    ,toSql sid
    ,toSql mh
    ,toSql did]
  {-docSqlWhere (SigLinkMagicHashAuthorization sid mh) DocumentSign did _ = 
    WhereSimple
    "(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND id = ? AND token = ? AND document_id = ?)"
    [toSql [SignatoryPartner]
    ,toSql [SignatoryAuthor, SignatoryPartner]
    ,toSql sid
    ,toSql mh
    ,toSql did]-}
  docSqlWhere (SigLinkMagicHashAuthorization sid mh) DocumentView did _ = 
    WhereSimple
    "(SELECT count(*) FROM signatory_links WHERE id = ? AND token = ? AND document_id = ?)"
    [toSql sid,
     toSql mh,
     toSql did]
  
  
--instance UserAuthorization SigLinkMagicHashAuthorization where
--  canUser _ _ _ _ = False
