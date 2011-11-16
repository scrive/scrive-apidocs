module Auth.SigLinkMagicHashAuthorization where

import Auth.DocumentPrivilege
import DB.Types
import Doc.DocStateData
import Auth.DocumentAuthorization
import Auth.UserAuthorization
import Data.Semantic

-- | Authenticated with SignatoryLinkID and MagicHash combo
data SigLinkMagicHashAuthorization = SigLinkMagicHashAuthorization SignatoryLinkID MagicHash

instance DocumentAuthorization SigLinkMagicHashAuthorization where
  canDocument (SigLinkMagicHashAuthorization sid mh) doc _ DocumentView =
    hasSigLinkFor (And sid mh) doc
  canDocument (SigLinkMagicHashAuthorization sid mh) doc _ DocumentSign =
    hasSigLinkFor (And SignatoryPartner (And sid mh)) doc
  canDocument _ _ _ _ = False
  
  docSqlWhere (SigLinkMagicHashAuthorization sid mh) DocumentSend did =
    ("(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND user_id = ? AND document_id = ?)", 
     [toSql [SignatoryAuthor]
     ,toSql [SignatoryAuthor, SignatoryPartner]
     ,toSql uid
     ,toSql did])
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentSign did = 
    ("(SELECT count(*) FROM signatory_links WHERE (roles = ? OR roles = ?) AND user_id = ? AND document_id = ?)", 
     [toSql [SignatoryPartner]
     ,toSql [SignatoryAuthor, SignatoryPartner]
     ,toSql uid
     ,toSql did])
  docSqlWhere (AuthenticatedUserAuthorization uid) DocumentView did = 
    ("(SELECT count(*) FROM signatory_links WHERE user_id = ? AND document_id = ?)", 
     [toSql uid,
      toSql did])
  
  
instance UserAuthorization SigLinkMagicHashAuthorization where
  canUser _ _ _ _ = False
