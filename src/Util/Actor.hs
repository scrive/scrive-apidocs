module Util.Actor (
  Actor(actorTime, actorIP, actorUserID, actorEmail, actorSigLinkID, actorAPIString, actorWho)
  , authorActor
  , signatoryActor
  , systemActor
  , mailAPIActor
  , mailSystemActor
  , userActor
  , adminActor
  , mkAuthorActor
  , mkAdminActor
  , apiActor
  ) where

import Util.HasSomeUserInfo
import Context
import IPAddress
import MinutesTime
import User.Model
import Doc.SignatoryLinkID
import Data.Typeable
import Control.Monad

mkAuthorActor :: Context -> Maybe Actor
mkAuthorActor ctx = case (ctxmaybeuser ctx) `mplus` (ctxmaybepaduser ctx) of
  Just user -> Just $ authorActor (ctxtime ctx) (ctxipnumber ctx) (userid user) (getEmail user)
  Nothing   -> Nothing

mkAdminActor :: Context -> Maybe Actor
mkAdminActor ctx = case ctxmaybeuser ctx of
  Just user -> Just $ adminActor (ctxtime ctx) (ctxipnumber ctx) (userid user) (getEmail user)
  Nothing   -> Nothing

-- | Actor describes who is performing an action and when
data Actor = Actor {
    -- | the time the action is taken
    actorTime      :: MinutesTime
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

systemActor :: MinutesTime -> Actor
systemActor time = Actor {
    actorTime = time
  , actorIP = Nothing
  , actorUserID = Nothing
  , actorEmail = Nothing
  , actorSigLinkID = Nothing
  , actorAPIString = Nothing
  , actorWho = "the Scrive system"
}

-- | For an action that requires an operation on a document and an
-- author to be logged in
authorActor :: MinutesTime -> IPAddress -> UserID -> String -> Actor
authorActor time ip uid email = Actor {
    actorTime = time
  , actorIP = Just ip
  , actorUserID = Just uid
  , actorEmail = Just email
  , actorSigLinkID = Nothing
  , actorAPIString = Nothing
  , actorWho = "the author" ++ if not (null email) then " (" ++ email ++ ")" else ""
}

-- | For an action requiring a signatory with siglinkid and token (such as signing)
signatoryActor :: MinutesTime -> IPAddress -> Maybe UserID -> String -> SignatoryLinkID -> Actor
signatoryActor time ip muid email slid = Actor {
    actorTime = time
  , actorIP = Just ip
  , actorUserID = muid
  , actorEmail = Just email
  , actorSigLinkID = Just slid
  , actorAPIString = Nothing
  , actorWho = "the signatory (" ++ email ++ ")"
}

-- | For documents created using mailapi/scrivebymail
mailAPIActor :: MinutesTime -> UserID -> String -> Actor
mailAPIActor time uid email = Actor {
    actorTime = time
  , actorIP = Nothing
  , actorUserID = Just uid
  , actorEmail = Just email
  , actorSigLinkID = Nothing
  , actorAPIString = Just "Mail API"
  , actorWho = "the user (" ++ email ++ ") (using the Mail API)"
}

-- | For delivery/reading notifications from the mail system
mailSystemActor :: MinutesTime -> Maybe UserID -> String -> SignatoryLinkID -> Actor
mailSystemActor time muid email slid = Actor {
    actorTime = time
  , actorIP = Nothing
  , actorUserID = muid
  , actorEmail = Just email
  , actorSigLinkID = Just slid
  , actorAPIString = Nothing
  , actorWho = "the email subsystem"
}

-- | For actions performed by logged in user
userActor :: MinutesTime -> IPAddress -> UserID -> String -> Actor
userActor time ip uid email = Actor {
    actorTime = time
  , actorIP = Just ip
  , actorUserID = Just uid
  , actorEmail = Just email
  , actorSigLinkID = Nothing
  , actorAPIString = Nothing
  , actorWho = "the user (" ++ email ++ ")"
}

-- | For actions performed by an admin
adminActor :: MinutesTime -> IPAddress -> UserID -> String -> Actor
adminActor time ip uid email = Actor {
    actorTime = time
  , actorIP = Just ip
  , actorUserID = Just uid
  , actorEmail = Just email
  , actorSigLinkID = Nothing
  , actorAPIString = Nothing
  , actorWho = "the admin (" ++ email ++ ")"
}

apiActor :: MinutesTime -> IPAddress -> UserID -> String -> String -> Actor
apiActor time ip uid email apistring = Actor {
    actorTime = time
  , actorIP = Just ip
  , actorUserID = Just uid
  , actorEmail = Just email
  , actorSigLinkID = Nothing
  , actorAPIString = Just apistring
  , actorWho = "the user (" ++ show email ++ ") (using the API)" 
  }