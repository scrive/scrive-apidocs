module Auth.Internal where

import Misc
import User.Model
import Doc.Model
import Data.Maybe
import Util.SignatoryLinkUtils
import MinutesTime
import DB.Types

data Privilege = DocumentCreate
               | DocumentSend
               | DocumentSign
               | DocumentView
               deriving (Show, Eq)
                        
instance SafeEnum Privilege where
  fromSafeEnum DocumentCreate = 200
  fromSafeEnum DocumentSend   = 100
  fromSafeEnum DocumentSign   = 101
  fromSafeEnum DocumentView   = 102
  
  toSafeEnum 200 = Just DocumentCreate
  toSafeEnum 100 = Just DocumentSend
  toSafeEnum 101 = Just DocumentSign
  toSafeEnum 102 = Just DocumentView
  toSafeEnum _   = Nothing
  
documentPrivileges :: [Privilege]
documentPrivileges = [DocumentSend, DocumentSign, DocumentView]

userPrivileges :: [Privilege]
userPrivileges = [DocumentCreate]

class DocumentAuthorization a where
  canDocument :: a -> Document -> MinutesTime -> Privilege -> Bool
  canDocument _ _ _ _ = False

class UserAuthorization a where
  canUser :: a -> UserID -> MinutesTime -> Privilege -> Bool
  canUser _ _ _ _ = False

data UserSessionAuthorization = UserSessionAuthorization UserID

instance DocumentAuthorization UserSessionAuthorization where
  canDocument (UserSessionAuthorization uid) doc _ DocumentSend = 
    maybe False ((==) (Just uid) . maybesignatory) (getAuthorSigLink doc)
  canDocument (UserSessionAuthorization uid) doc _ DocumentSign =
    isJust (getSigLinkFor doc uid)
  canDocument (UserSessionAuthorization uid) doc _ DocumentView =
    isJust (getSigLinkFor doc uid)
  canDocument _ _ _ _ = False
  
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
  canDocument (AccessTokenAuthorization ex _ _) _ now _ | ex <= now = False
  canDocument (AccessTokenAuthorization _ _ _) _ _ p | p `notElem` documentPrivileges = False
  canDocument (AccessTokenAuthorization _ _ ps) _ _ p | p `notElem` ps = False
  canDocument (AccessTokenAuthorization _ uid _) doc _ DocumentView =
    isJust (getSigLinkFor doc uid)
  canDocument (AccessTokenAuthorization _ uid _) doc _ DocumentSign =
    isJust (getSigLinkFor doc uid)
  canDocument (AccessTokenAuthorization _ uid _) doc _ DocumentSend =
    maybe False ((==) (Just uid) . maybesignatory) (getAuthorSigLink doc)
  canDocument _ _ _ _ = False

instance UserAuthorization AccessTokenAuthorization where
  canUser (AccessTokenAuthorization ex _ _  ) _ now _  | ex <= now                   = False
  canUser (AccessTokenAuthorization _ _ _   ) _ _ p    | p `notElem` userPrivileges  = False
  canUser (AccessTokenAuthorization _ _ ps  ) _ _ p    | p `notElem` ps              = False
  canUser (AccessTokenAuthorization _ uid _ ) uid2 _ _ | uid /= uid2                 = False
  canUser (AccessTokenAuthorization _ _ _   ) _ _ DocumentCreate                     = True
  canUser _ _ _ _                                                                    = False

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

data Union a b = Union a b

instance (DocumentAuthorization a, DocumentAuthorization b) => DocumentAuthorization (Union a b) where
  canDocument (Union a b) x y z = canDocument a x y z || canDocument b x y z
  
instance (UserAuthorization a, UserAuthorization b) => UserAuthorization (Union a b) where
  canUser (Union a b) x y z = canUser a x y z || canUser b x y z

