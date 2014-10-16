module Util.Actor (
  Actor(..)
  , authorActor
  , signatoryActor
  , systemActor
  , mailSystemActor
  , userActor
  , adminActor
  , mkAuthorActor
  , mkAdminActor
  , apiActor
  ) where

import Control.Monad
import Data.Typeable

import Context
import Doc.DocStateData (SignatoryLink(..))
import Doc.DocumentMonad (DocumentMonad)
import Doc.SignatoryLinkID
import IPAddress
import MinutesTime
import User.Model
import Util.HasSomeUserInfo

mkAuthorActor :: Context -> Maybe Actor
mkAuthorActor ctx = case (ctxmaybeuser ctx) `mplus` (ctxmaybepaduser ctx) of
  Just user -> Just $ authorActor ctx user
  Nothing   -> Nothing

mkAdminActor :: Context -> Maybe Actor
mkAdminActor ctx = case ctxmaybeuser ctx of
  Just user -> Just $ adminActor ctx user
  Nothing   -> Nothing

-- | Actor describes who is performing an action and when
data Actor = Actor {
    -- | the time the action is taken
    actorTime      :: UTCTime
    -- | the client time the action is taken
  , actorClientTime :: Maybe UTCTime
    -- | the client name
  , actorClientName :: Maybe String
    -- | If the action is originated on another machine, its IP
  , actorIP        :: Maybe IPAddress
    -- | If the action is originated by a logged in user
  , actorUserID    :: Maybe UserID
    -- | If the action is originated by a person with email address
  , actorEmail     :: Maybe String
    -- | If the action is originated by a signatory on the document being acted on
  , actorSigLinkID :: Maybe SignatoryLinkID
    -- | If the action is originated by an api call, a string to describe it
  , actorAPIString :: Maybe String
    -- | A textual string describing this actor, used for building evidence strings
  , actorWho       :: String
  } deriving (Eq, Ord, Show, Typeable)

contextActor :: Context -> Actor
contextActor ctx = Actor
  { actorTime = ctxtime ctx
  , actorClientTime = ctxclienttime ctx
  , actorClientName = ctxclientname ctx
  , actorIP = Just (ctxipnumber ctx)
  , actorUserID = Nothing
  , actorEmail = Nothing
  , actorSigLinkID = Nothing
  , actorAPIString = Nothing
  , actorWho = ""
  }

systemActor :: UTCTime -> Actor
systemActor time = Actor {
    actorTime = time
  , actorClientTime = Nothing
  , actorClientName = Nothing
  , actorIP = Nothing
  , actorUserID = Nothing
  , actorEmail = Nothing
  , actorSigLinkID = Nothing
  , actorAPIString = Nothing
  , actorWho = "the Scrive system"
}

-- | For an action that requires an operation on a document and an
-- author to be logged in
authorActor :: Context -> User -> Actor
authorActor ctx u = (userActor ctx u) {
    actorWho = "the author " ++ getIdentifier u
}

-- | For an action requiring a signatory with siglinkid and token (such as signing)
signatoryActor :: DocumentMonad m => Context -> SignatoryLink -> m Actor
signatoryActor ctx s = do
  sid <- getSignatoryIdentifier s
  return $ (contextActor ctx){
        actorUserID = maybesignatory s
      , actorEmail = Just (getEmail s)
      , actorSigLinkID = Just (signatorylinkid s)
      , actorWho = "the signatory " ++ sid
      }

-- | For delivery/reading notifications from the mail system
mailSystemActor :: UTCTime -> Maybe UserID -> String -> SignatoryLinkID -> Actor
mailSystemActor time muid email slid = Actor {
    actorTime = time
  , actorIP = Nothing
  , actorClientTime = Nothing
  , actorClientName = Nothing
  , actorUserID = muid
  , actorEmail = Just email
  , actorSigLinkID = Just slid
  , actorAPIString = Nothing
  , actorWho = "the email subsystem"
}

-- | For actions performed by logged in user
userActor :: Context -> User -> Actor
userActor ctx u = (contextActor ctx) {
    actorUserID = Just (userid u)
  , actorEmail = Just (getEmail u)
  , actorWho = "the user " ++ getIdentifier u
}

-- | For actions performed by an admin
adminActor :: Context -> User -> Actor
adminActor ctx u = (userActor ctx u) {
    actorWho = "the admin (" ++ (getEmail u) ++ ")"
}

apiActor :: Context -> User -> String -> Actor
apiActor ctx u apistring = (userActor ctx u) {
    actorAPIString = Just apistring
  , actorWho = "the user (" ++ show (getEmail u) ++ ") (using the API)"
  }
