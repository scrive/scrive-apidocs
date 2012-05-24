module Util.Actor (
  Actor(actorTime, actorIP, actorUserID, actorEmail, actorSigLinkID, actorAPIString, actorWho)
  , authorActor
  , signatoryActor
  , systemActor
  , mailAPIActor
  , mailSystemActor
  , integrationAPIActor
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
import API.Service.Model
import Doc.DocStateData
import Data.Typeable
import qualified Data.ByteString.UTF8 as BS
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
  , actorWho = "the author (" ++ email ++ ")"
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
  , actorWho = "the signatory with email \"" ++ email ++ "\""
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
  , actorWho = "the user with email \"" ++ email ++ "\" using the Mail API"
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
  , actorWho = "the signatory with email \"" ++ email ++ "\" (reported by the Mail subsystem)"
}

-- | For actions originating from the integration api
integrationAPIActor :: MinutesTime -> IPAddress -> ServiceID -> Maybe String -> Actor
integrationAPIActor time ip sid mcompany = Actor {
    actorTime = time
  , actorIP = Just ip
  , actorUserID = Nothing
  , actorEmail = Nothing
  , actorSigLinkID = Nothing
  , actorAPIString = Just $ BS.toString $ unServiceID sid
  , actorWho = case mcompany of
      Just company -> "the company \"" ++ company ++ "\" using the Integration API"
      Nothing -> "the Integration API"
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
  , actorWho = "the user with email \"" ++ email ++ "\""
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
  , actorWho = "the admin with email \"" ++ email ++ "\""
}

apiActor :: MinutesTime -> IPAddress -> UserID -> String -> String -> Actor
apiActor time ip uid email apistring = Actor {
    actorTime = time
  , actorIP = Just ip
  , actorUserID = Just uid
  , actorEmail = Just email
  , actorSigLinkID = Nothing
  , actorAPIString = Just apistring
  , actorWho = "the user with email " ++ show email ++ " using the API" 
  }