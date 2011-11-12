module Auth.Internal where

import Misc
import User.Model
import Doc.Model
import Data.Maybe
import Util.SignatoryLinkUtils
import MinutesTime
import DB.Types
import Data.Pairs
import Data.Int
import DB.Derive

-- | Privilege tokens; actions should fall into one of these
-- Privileges
data Privilege = DocumentPrivilege DocumentPrivilege
               | UserPrivilege     UserPrivilege
                 deriving (Show, Eq)
                          
data DocumentPrivilege = DocumentSend
                       | DocumentSign
                       | DocumentView
               deriving (Show, Eq)

data UserPrivilege = DocumentCreate
               deriving (Show, Eq)
                        
-- An informal standard of User Privileges being 2xx, doc privileges
-- in 1xx
instance SafeEnum Privilege where
  fromSafeEnum (UserPrivilege   DocumentCreate) = 200
  fromSafeEnum (DocumentPrivilege DocumentSend) = 100
  fromSafeEnum (DocumentPrivilege DocumentSign) = 101
  fromSafeEnum (DocumentPrivilege DocumentView) = 102
  
  toSafeEnum 200 = Just (UserPrivilege   DocumentCreate)
  toSafeEnum 100 = Just (DocumentPrivilege DocumentSend)
  toSafeEnum 101 = Just (DocumentPrivilege DocumentSign)
  toSafeEnum 102 = Just (DocumentPrivilege DocumentView)
  toSafeEnum _   = Nothing

class DocumentAuthorization a where
  canDocument :: a -> Document -> MinutesTime -> DocumentPrivilege -> Bool
  canDocument _ _ _ _ = False

class UserAuthorization a where
  canUser :: a -> UserID -> MinutesTime -> UserPrivilege -> Bool
  canUser _ _ _ _ = False

data UserSessionAuthorization = UserSessionAuthorization UserID

instance DocumentAuthorization UserSessionAuthorization where
  canDocument (UserSessionAuthorization uid) doc _ DocumentSend = 
    maybe False ((==) (Just uid) . maybesignatory) (getAuthorSigLink doc)
  canDocument (UserSessionAuthorization uid) doc _ DocumentSign =
    isJust (getSigLinkFor doc uid)
  canDocument (UserSessionAuthorization uid) doc _ DocumentView =
    isJust (getSigLinkFor doc uid)
  
instance UserAuthorization UserSessionAuthorization where
  canUser (UserSessionAuthorization uid) uid2 _ _ = uid == uid2
  
-- | Authenticated with SignatoryLinkID and MagicHash combo
data SigLinkMagicHashAuthorization = SigLinkMagicHashAuthorization SignatoryLinkID MagicHash

instance DocumentAuthorization SigLinkMagicHashAuthorization where
  canDocument (SigLinkMagicHashAuthorization sid mh) doc _ DocumentView =
    maybe False ((==) mh . signatorymagichash) (getSigLinkFor doc sid)
  canDocument (SigLinkMagicHashAuthorization sid mh) doc _ DocumentSign =
    maybe False ((==) mh . signatorymagichash) (getSigLinkFor doc sid)
  canDocument _ _ _ _ = False
  
instance UserAuthorization SigLinkMagicHashAuthorization where
  canUser _ _ _ _ = False

data AccessTokenAuthorization = AccessTokenAuthorization MinutesTime UserID [Privilege]

instance DocumentAuthorization AccessTokenAuthorization where
  canDocument (AccessTokenAuthorization ex _ _ ) _ now _ | ex <= now                        = False
  canDocument (AccessTokenAuthorization _ _ ps ) _ _ p   | DocumentPrivilege p `notElem` ps = False
  canDocument (AccessTokenAuthorization _ uid _) doc _ DocumentView =
    isJust (getSigLinkFor doc uid)
  canDocument (AccessTokenAuthorization _ uid _) doc _ DocumentSign =
    isJust (getSigLinkFor doc uid)
  canDocument (AccessTokenAuthorization _ uid _) doc _ DocumentSend =
    maybe False ((==) (Just uid) . maybesignatory) (getAuthorSigLink doc)

instance UserAuthorization AccessTokenAuthorization where
  canUser (AccessTokenAuthorization ex _ _  ) _ now _  | ex <= now                    = False
  canUser (AccessTokenAuthorization _ _ ps  ) _ _ p    | UserPrivilege p `notElem` ps = False
  canUser (AccessTokenAuthorization _ uid _ ) uid2 _ _ | uid /= uid2                  = False
  canUser (AccessTokenAuthorization _ _ _   ) _ _ DocumentCreate                      = True

instance DocumentAuthorization a => DocumentAuthorization (Maybe a) where
  canDocument Nothing _ _ _ = False
  canDocument (Just a) x y z = canDocument a x y z
  
instance UserAuthorization a => UserAuthorization (Maybe a) where
  canUser Nothing _ _ _ = False
  canUser (Just a) x y z = canUser a x y z
  
instance DocumentAuthorization a => DocumentAuthorization (Either x a) where
  canDocument (Left _) _ _ _ = False
  canDocument (Right a) x y z = canDocument a x y z
  
instance UserAuthorization a => UserAuthorization (Either x a) where
  canUser (Left _) _ _ _ = False
  canUser (Right a) x y z = canUser a x y z

instance (DocumentAuthorization a, DocumentAuthorization b) => DocumentAuthorization (Or a b) where
  canDocument (Or a b) x y z = canDocument a x y z || canDocument b x y z
  
instance (UserAuthorization a, UserAuthorization b) => UserAuthorization (Or a b) where
  canUser (Or a b) x y z = canUser a x y z || canUser b x y z




newtype AccessToken = AccessToken { unAccessToken :: Int64 }
                    deriving (Eq, Ord)
$(newtypeDeriveConvertible ''AccessToken)
$(newtypeDeriveUnderlyingReadShow ''AccessToken)

newtype APIToken = APIToken { unAPIToken :: Int64 }
                    deriving (Eq, Ord)
$(newtypeDeriveConvertible ''APIToken)
$(newtypeDeriveUnderlyingReadShow ''APIToken)

newtype APISecret = APISecret { unAPISecret :: Int64 }
                    deriving (Eq, Ord)
$(newtypeDeriveConvertible ''APISecret)
$(newtypeDeriveUnderlyingReadShow ''APISecret)

data APITokenStatus = APITokenActive
                    | APITokenDeleted

instance SafeEnum APITokenStatus where
  fromSafeEnum APITokenActive  = 1
  fromSafeEnum APITokenDeleted = 0
  
  toSafeEnum 1 = Just APITokenActive
  toSafeEnum 0 = Just APITokenDeleted
  toSafeEnum _ = Nothing
