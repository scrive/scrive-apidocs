module EvidenceLog.Model 
       (
         EvidenceEventType(..),
         InsertEvidenceEvent(..),
         Actor(..),
         AuthorActor(..),
         SignatoryActor(..),
         SystemActor(..),
         MailAPIActor(..)       
       )
       
       where

import Doc.DocStateData
import User.UserID
import DB.Derive
import DB.Nexus

import DB.Classes
import DB.Utils
import MinutesTime

import Database.HDBC
import Misc

import Version

class Actor a where
  actorTime      :: a -> MinutesTime
  actorIP        :: a -> Maybe IPAddress
  actorIP _      = Nothing
  actorUserID    :: a -> Maybe UserID
  actorUserID _  = Nothing
  actorEmail     :: a -> Maybe String
  actorEmail _   = Nothing
  actorSigLinkID :: a -> Maybe SignatoryLinkID
  actorSigLinkID _ = Nothing

data SystemActor = SystemActor MinutesTime
                   deriving (Eq, Ord, Show)
instance Actor SystemActor where
  actorTime (SystemActor t) = t

data AuthorActor = AuthorActor MinutesTime IPAddress UserID String
                   deriving (Eq, Ord, Show)
instance Actor AuthorActor where
  actorTime   (AuthorActor t _ _ _) = t
  actorIP     (AuthorActor _ i _ _) = Just i
  actorUserID (AuthorActor _ _ u _) = Just u
  actorEmail  (AuthorActor _ _ _ e) = Just e

data SignatoryActor = SignatoryActor MinutesTime IPAddress (Maybe UserID) String SignatoryLinkID
                   deriving (Eq, Ord, Show)
instance Actor SignatoryActor where
  actorTime      (SignatoryActor t _ _ _ _) = t
  actorIP        (SignatoryActor _ i _ _ _) = Just i
  actorUserID    (SignatoryActor _ _ u _ _) = u
  actorEmail     (SignatoryActor _ _ _ e _) = Just e
  actorSigLinkID (SignatoryActor _ _ _ _ s) = Just s
  
data MailAPIActor = MailAPIActor MinutesTime UserID String
                   deriving (Eq, Ord, Show)
instance Actor MailAPIActor where
  actorTime   (MailAPIActor t _ _) = t
  actorUserID (MailAPIActor _ u _) = Just u
  actorEmail  (MailAPIActor _ _ e) = Just e
                    
insertEvidenceEvent :: Actor a 
                       => EvidenceEventType
                       -> String       -- text
                       -> Maybe DocumentID 
                       -> a
                       -> Nexus
                       -> IO Bool
insertEvidenceEvent event text mdid actor conn = do
  st <- prepare conn $ "INSERT INTO evidence_log ("
        ++ "  document_id"
        ++ ", time"
        ++ ", text"
        ++ ", event_type"
        ++ ", version_id"
        ++ ", user_id"
        ++ ", email"
        ++ ", request_ip_v4"
        ++ ", signatory_link_id"
        ++ ") VALUES (?,?,?,?,?,?,?,?,?)"
  res <- execute st [toSql mdid
                    ,toSql $ actorTime      actor
                    ,toSql text
                    ,toSql event
                    ,toSql versionID
                    ,toSql $ actorUserID    actor
                    ,toSql $ actorEmail     actor
                    ,toSql $ actorIP        actor
                    ,toSql $ actorSigLinkID actor
                    ]
  oneRowAffectedGuard res

data Actor a => InsertEvidenceEvent a = InsertEvidenceEvent 
                                        EvidenceEventType       -- A code for the event
                                        String                  -- Text for evidence
                                        (Maybe DocumentID)      -- The documentid if this event is about a document
                                        a
instance Actor a => DBUpdate (InsertEvidenceEvent a) Bool where
  dbUpdate (InsertEvidenceEvent tp txt mdid a) = 
    wrapDB (insertEvidenceEvent tp txt mdid a)

-- | A machine-readable event code for different types of events.
data EvidenceEventType = DocumentCreateEvidence
                       | SignatoryAddInformationEvidence
                       | AuthorAddInformationEvidence
                       | UserAccountAddInformationEvidence
                       | InvitationSentEvidence
                       | InvitationReceivedEvidence
                       | InvitationOpenedEvidence
                       | ClickSecretLinkEvidence
                       | ClickSignEvidence
                       | ClickRejectEvidence
                       | ElegConfirmEvidence
                       | DocumentClosedEvidence
                       | DocumentCancelEvidence
                       | ReminderSendEvidence
                       | CustomInviteTextEvidence
                       | AuthorRequestAttachmentEvidence
                       | AuthorAddAttachmentEvidence
                       | AuthorSetsDueDateEvidence
                       | SystemSetsDueDateEvidence
                       | AuthorAddsViewerEvidence
                       | AuthorAddsSignatoryEvidence
                       | AuthorSetsToSecretaryEvidence
                       | AuthorSetsToSignatoryEvidence
                       | AuthorUsesCSVEvidence
                       | AuthorChoosesSignLastEvidence
                       | DocSignedByTrustweaverEvidence
                       | ClosedDocDeliveredEvidence
                       | SignatoryUploadAttachmentEvidence
                       | CancelDocBadElegEvidence
                       | AuthorChangeSigEmailEvidence
                       | AttachSealedFileEvidence
                       | PreparationToPendingEvidence
                       | DeleteSigAttachmentEvidence
                       | ErrorDocumentEvidence
                       deriving (Eq, Show, Read, Ord)
$(enumDeriveConvertible ''EvidenceEventType)

