module Util.Actor (
    Actor(..)
  , authorActor
  , signatoryActor
  , recreatedSignatoryActor
  , systemActor
  , mailSystemActor
  , userActor
  , mkAuthorActor
  , mkAdminActor
  , apiActor
  , contextActor
  ) where

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
mkAuthorActor ctx = case getContextUser ctx of
  Just user -> Just $ authorActor ctx user
  Nothing   -> Nothing

mkAdminActor :: Context -> Maybe Actor
mkAdminActor ctx = case get ctxmaybeuser ctx of
  Just user -> Just $ adminActor ctx user
  Nothing   -> Nothing

-- | Actor describes who is performing an action and when
data Actor = Actor {
    -- | the time the action is taken
    actorTime      :: UTCTime
    -- | the client time the action is taken
  , actorClientTime :: Maybe UTCTime
    -- | the client name
  , actorClientName :: Maybe Text
    -- | If the action is originated on another machine, its IP
  , actorIP        :: Maybe IPAddress
    -- | If the action is originated by a logged in user
  , actorUserID    :: Maybe UserID
    -- | If the action is originated by a person with email address
  , actorEmail     :: Maybe Text
    -- | If the action is originated by a signatory on the document being acted on
  , actorSigLinkID :: Maybe SignatoryLinkID
    -- | If the action is originated by an api call, a string to describe it
  , actorAPIString :: Maybe Text
    -- | A textual string describing this actor, used for building evidence strings
  , actorWho       :: Text
  } deriving (Eq, Ord, Show, Typeable)

contextActor :: Context -> Actor
contextActor ctx = Actor { actorTime       = get ctxtime ctx
                         , actorClientTime = get ctxclienttime ctx
                         , actorClientName = get ctxclientname ctx
                         , actorIP         = Just (get ctxipnumber ctx)
                         , actorUserID     = Nothing
                         , actorEmail      = Nothing
                         , actorSigLinkID  = Nothing
                         , actorAPIString  = Nothing
                         , actorWho        = ""
                         }

systemActor :: UTCTime -> Actor
systemActor time = Actor { actorTime       = time
                         , actorClientTime = Nothing
                         , actorClientName = Nothing
                         , actorIP         = Nothing
                         , actorUserID     = Nothing
                         , actorEmail      = Nothing
                         , actorSigLinkID  = Nothing
                         , actorAPIString  = Nothing
                         , actorWho        = "the Scrive system"
                         }

-- | For an action that requires an operation on a document and an
-- author to be logged in
authorActor :: Context -> User -> Actor
authorActor ctx u = (userActor ctx u)
  { actorWho = "the author " <> getFullName u <> " (" <> getEmail u <> ")"
  }

-- | For an action requiring a signatory with siglinkid and token (such as signing)
signatoryActor :: DocumentMonad m => Context -> SignatoryLink -> m Actor
signatoryActor ctx s = return $ toSignatoryActor s (contextActor ctx)

-- | Used if we are performing some action in background, but we want
-- to pretend like if it was performed by signatory.
recreatedSignatoryActor
  :: DocumentMonad m
  => UTCTime
  -> Maybe UTCTime
  -> Maybe Text
  -> IPAddress
  -> SignatoryLink
  -> m Actor
recreatedSignatoryActor time mctime mcname mipaddress s =
  return $ toSignatoryActor s $ Actor { actorTime       = time
                                      , actorClientTime = mctime
                                      , actorClientName = mcname
                                      , actorIP         = Just mipaddress
                                      , actorUserID     = Nothing
                                      , actorEmail      = Nothing
                                      , actorSigLinkID  = Nothing
                                      , actorAPIString  = Nothing
                                      , actorWho        = ""
                                      }

toSignatoryActor :: SignatoryLink -> Actor -> Actor
toSignatoryActor s a = a
  { actorUserID    = maybesignatory s
  , actorEmail     = Just (getEmail s)
  , actorSigLinkID = Just (signatorylinkid s)
  , actorWho       = "the signatory " <> getFullName s <> " (" <> getEmail s <> ")"
  }

-- | For delivery/reading notifications from the mail system
mailSystemActor :: UTCTime -> Maybe UserID -> Text -> SignatoryLinkID -> Actor
mailSystemActor time muid email slid = Actor { actorTime       = time
                                             , actorIP         = Nothing
                                             , actorClientTime = Nothing
                                             , actorClientName = Nothing
                                             , actorUserID     = muid
                                             , actorEmail      = Just email
                                             , actorSigLinkID  = Just slid
                                             , actorAPIString  = Nothing
                                             , actorWho        = "the email subsystem"
                                             }

-- | For actions performed by logged in user
userActor :: Context -> User -> Actor
userActor ctx u = (contextActor ctx)
  { actorUserID = Just (userid u)
  , actorEmail  = Just (getEmail u)
  , actorWho    = "the user " <> getFullName u <> " (" <> getEmail u <> ")"
  }

-- | For actions performed by an admin
adminActor :: Context -> User -> Actor
adminActor ctx u = (userActor ctx u)
  { actorWho = "the admin " <> getFullName u <> " (" <> getEmail u <> ")"
  }

apiActor :: Context -> User -> Text -> Actor
apiActor ctx u apistring = a { actorAPIString = Just apistring
                             , actorWho       = actorWho a <> " (using the API)"
                             }
  where a = userActor ctx u
