module EvidenceLog.Model 
       (
         EvidenceEventType(..),
         InsertEvidenceEvent(..),
         Actor(..),
         actorTime
       )
       
       where

import Doc.DocStateData
import User.UserID
import DB.Derive

import DB.Classes
import DB.Utils
import MinutesTime

import Database.HDBC
import Misc

import Database.HDBC.PostgreSQL
import Version

data Actor = SystemActor    MinutesTime
           | AuthorActor    MinutesTime IPAddress        UserID  String SignatoryLinkID
           | SignatoryActor MinutesTime IPAddress (Maybe UserID) String SignatoryLinkID
           | MailAPIActor   MinutesTime                  UserID  String SignatoryLinkID
           deriving (Show, Eq, Ord)
                    
actorTime :: Actor -> MinutesTime
actorTime (SystemActor time) = time
actorTime (AuthorActor time _ _ _ _) = time
actorTime (SignatoryActor time _ _ _ _) = time
actorTime (MailAPIActor time _ _ _ ) = time

insertEvidenceEvent :: EvidenceEventType
                       -> String       -- text
                       -> Maybe DocumentID 
                       -> Actor
                       -> Connection
                       -> IO Bool
insertEvidenceEvent event text mdid (SystemActor time) conn = do
  st <- prepare conn $ "INSERT INTO evidence_log ("
        ++ "  document_id"
        ++ ", time"
        ++ ", text"
        ++ ", event_type"
        ++ ", version_id"
        ++ ") VALUES (?,?,?,?,?)"
  res <- execute st [toSql mdid
                    ,toSql time
                    ,toSql text
                    ,toSql event
                    ,toSql versionID
                    ]
  oneRowAffectedGuard res
insertEvidenceEvent event text mdid (AuthorActor time ip uid eml slid) conn = do
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
        ++ ") VALUES (?,?,?,?,?,?,?,?,?,?,?)"
  res <- execute st [toSql mdid
                    ,toSql time
                    ,toSql text
                    ,toSql event
                    ,toSql versionID
                    ,toSql $ Just uid
                    ,toSql $ Just eml
                    ,toSql ip
                    ,toSql $ Just slid
                    ]
  oneRowAffectedGuard res
insertEvidenceEvent event text mdid (SignatoryActor time ip muid eml slid) conn = do
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
                    ,toSql time
                    ,toSql text
                    ,toSql event
                    ,toSql versionID
                    ,toSql muid
                    ,toSql $ Just eml
                    ,toSql ip
                    ,toSql $ Just slid
                    ]
  oneRowAffectedGuard res
insertEvidenceEvent event text mdid (MailAPIActor time uid eml slid) conn = do
  st <- prepare conn $ "INSERT INTO evidence_log ("
        ++ "  document_id"
        ++ ", time"
        ++ ", text"
        ++ ", event_type"
        ++ ", version_id"
        ++ ", user_id"
        ++ ", email"
        ++ ", signatory_link_id"
        ++ ", api_user"
        ++ ") VALUES (?,?,?,?,?,?,?,?,?)"
  res <- execute st [toSql mdid
                    ,toSql time
                    ,toSql text
                    ,toSql event
                    ,toSql versionID
                    ,toSql $ Just uid
                    ,toSql $ Just eml
                    ,toSql $ Just slid
                    ,toSql $ Just "mailapi"
                    ]
  oneRowAffectedGuard res

data InsertEvidenceEvent = InsertEvidenceEvent 
                             EvidenceEventType       -- A code for the event
                             String                  -- Text for evidence
                             (Maybe DocumentID)      -- The documentid if this event is about a document
                             Actor
instance DBUpdate InsertEvidenceEvent Bool where
  dbUpdate (InsertEvidenceEvent tp txt mdid md) = 
    wrapDB (insertEvidenceEvent tp txt mdid md)

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

