module Auth.Internal where

import Misc
import User.Model
import Doc.Model
import Data.Maybe
import Util.SignatoryLinkUtils
import MinutesTime
import DB.Types
import Data.Semantic
import Data.Int
import DB.Derive

-- | Privilege tokens; actions should fall into one of these
-- Privileges. Please think about granularity before adding a new one.
data Privilege = DocumentPrivilege DocumentPrivilege
               | UserPrivilege     UserPrivilege
                 deriving (Show, Eq)
                          
-- | Privileges that have to do with a specific, existing document
data DocumentPrivilege = DocumentSend -- ^ Send invitations/reminders/etc
                       | DocumentSign -- ^ Sign a document
                       | DocumentView -- ^ View a document
               deriving (Show, Eq)

-- | Privileges specific to a User (Creating docs, changing settings,
-- etc).
data UserPrivilege = DocumentCreate -- ^ Create a new document
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

-- | Instances of this class determine whether an Authorization can
-- act on the document
class DocumentAuthorization a where
  -- | Can this authorization act on this document at this time and
  -- privilege the action falls under
  canDocument :: a -> Document -> MinutesTime -> DocumentPrivilege -> Bool
  canDocument _ _ _ _ = False

-- | Instances of this class determine whether an Authorization can
-- act on the User
class UserAuthorization a where
  canUser :: a -> UserID -> MinutesTime -> UserPrivilege -> Bool
  canUser _ _ _ _ = False

-- | An authorization wrapper for a user's base permissions
data AuthenticatedUserAuthorization = AuthenticatedUserAuthorization UserID

instance DocumentAuthorization AuthenticatedUserAuthorization where
  canDocument (AuthenticatedUserAuthorization uid) doc _ DocumentSend = 
    isJust $ getSigLinkFor doc $ And SignatoryAuthor uid
  canDocument (AuthenticatedUserAuthorization uid) doc _ DocumentSign =
    -- is this correct?
    isJust $ getSigLinkFor doc $ And SignatoryPartner uid
  canDocument (AuthenticatedUserAuthorization uid) doc _ DocumentView =
    isJust $ getSigLinkFor doc uid
  
instance UserAuthorization AuthenticatedUserAuthorization where
  canUser (AuthenticatedUserAuthorization uid) uid2 _ _ = uid == uid2
  
-- | Authenticated with SignatoryLinkID and MagicHash combo
data SigLinkMagicHashAuthorization = SigLinkMagicHashAuthorization SignatoryLinkID MagicHash

instance DocumentAuthorization SigLinkMagicHashAuthorization where
  canDocument (SigLinkMagicHashAuthorization sid mh) doc _ DocumentView =
    isJust $ getSigLinkFor doc (And sid mh)
  canDocument (SigLinkMagicHashAuthorization sid mh) doc _ DocumentSign =
    isJust $ getSigLinkFor doc (And SignatoryPartner (And sid mh))
  canDocument _ _ _ _ = False
  
instance UserAuthorization SigLinkMagicHashAuthorization where
  canUser _ _ _ _ = False

-- | API Access Token
data AccessTokenAuthorization = AccessTokenAuthorization MinutesTime UserID [Privilege]

instance DocumentAuthorization AccessTokenAuthorization where
  canDocument (AccessTokenAuthorization ex _ _ ) _ now _ | ex <= now                        = False
  canDocument (AccessTokenAuthorization _ _ ps ) _ _ p   | DocumentPrivilege p `notElem` ps = False
  canDocument (AccessTokenAuthorization _ uid _) doc now p =
    canDocument (AuthenticatedUserAuthorization uid) doc now p 

instance UserAuthorization AccessTokenAuthorization where
  canUser (AccessTokenAuthorization ex _ _  ) _ now _  | ex <= now                    = False
  canUser (AccessTokenAuthorization _ _ ps  ) _ _ p    | UserPrivilege p `notElem` ps = False
  canUser (AccessTokenAuthorization _ uid _ ) uid2 _ _ | uid /= uid2                  = False
  canUser (AccessTokenAuthorization _ uid _ ) uid2 now p =
    canUser (AuthenticatedUserAuthorization uid) uid2 now p

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

-- | AccessToken for OAuth2 API calls
newtype AccessToken = AccessToken { unAccessToken :: Int64 }
                    deriving (Eq, Ord)
$(newtypeDeriveConvertible ''AccessToken)
$(newtypeDeriveUnderlyingReadShow ''AccessToken)

-- | APIToken for API calls
newtype APIToken = APIToken { unAPIToken :: Int64 }
                    deriving (Eq, Ord)
$(newtypeDeriveConvertible ''APIToken)
$(newtypeDeriveUnderlyingReadShow ''APIToken)

-- | APISecret (like the password) for API calls
newtype APISecret = APISecret { unAPISecret :: Int64 }
                    deriving (Eq, Ord)
$(newtypeDeriveConvertible ''APISecret)
$(newtypeDeriveUnderlyingReadShow ''APISecret)

-- | APITokens have a status
data APITokenStatus = APITokenActive   -- | The API Token can be used to get an AccessToken
                    | APITokenDisabled -- | The API Token cannot be used to get an AccessToken

instance SafeEnum APITokenStatus where
  fromSafeEnum APITokenActive  = 1
  fromSafeEnum APITokenDisabled = 0
  
  toSafeEnum 1 = Just APITokenActive
  toSafeEnum 0 = Just APITokenDisabled
  toSafeEnum _ = Nothing
