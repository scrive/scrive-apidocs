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
  
instance UserAuthorization SigLinkMagicHashAuthorization where
  canUser _ _ _ _ = False
