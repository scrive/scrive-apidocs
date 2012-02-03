module EvidenceLog.Model 
       (
         EvidenceEventType(..),
         InsertEvidenceEvent(..),
         Actor(..),
         AuthorActor(..),
         SignatoryActor(..),
         SystemActor(..),
         MailAPIActor(..),  
         MailSystemActor(..),
         IntegrationAPIActor(..),
         UserActor(..),
         AdminActor(..),
         GetEvidenceLog(..),
         DocumentEvidenceEvent(..),
         copyEvidenceLogToNewDocument
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
  actorAPIString :: a -> Maybe String
  actorAPIString _ = Nothing
  actorWho       :: a -> String

data SystemActor = SystemActor MinutesTime
                   deriving (Eq, Ord, Show)
instance Actor SystemActor where
  actorTime (SystemActor t) = t
  actorWho _ = "the Scrive system"

data AuthorActor = AuthorActor MinutesTime IPAddress UserID String
                   deriving (Eq, Ord, Show)
instance Actor AuthorActor where
  actorTime   (AuthorActor t _ _ _) = t
  actorIP     (AuthorActor _ i _ _) = Just i
  actorUserID (AuthorActor _ _ u _) = Just u
  actorEmail  (AuthorActor _ _ _ e) = Just e
  actorWho    (AuthorActor _ _ _ e) = "the author (" ++ e ++ ")"

data SignatoryActor = SignatoryActor MinutesTime IPAddress (Maybe UserID) String SignatoryLinkID
                   deriving (Eq, Ord, Show)
instance Actor SignatoryActor where
  actorTime      (SignatoryActor t _ _ _ _) = t
  actorIP        (SignatoryActor _ i _ _ _) = Just i
  actorUserID    (SignatoryActor _ _ u _ _) = u
  actorEmail     (SignatoryActor _ _ _ e _) = Just e
  actorSigLinkID (SignatoryActor _ _ _ _ s) = Just s
  actorWho       (SignatoryActor _ _ _ e _) = "the signatory with email " ++ show e
  
data MailAPIActor = MailAPIActor MinutesTime UserID String
                   deriving (Eq, Ord, Show)
instance Actor MailAPIActor where
  actorTime   (MailAPIActor t _ _) = t
  actorUserID (MailAPIActor _ u _) = Just u
  actorEmail  (MailAPIActor _ _ e) = Just e
  actorWho    (MailAPIActor _ _ e) = "the user with email " ++ show e ++ " using the Mail API"
  actorAPIString _ = Just "Mail API"
                    
data MailSystemActor = MailSystemActor MinutesTime (Maybe UserID) String SignatoryLinkID
                   deriving (Eq, Ord, Show)
instance Actor MailSystemActor where
  actorTime      (MailSystemActor t _ _ _) = t
  actorUserID    (MailSystemActor _ u _ _) = u
  actorEmail     (MailSystemActor _ _ e _) = Just e
  actorSigLinkID (MailSystemActor _ _ _ s) = Just s
  actorWho       (MailSystemActor _ _ e _) = "the signatory with email " ++ show e ++ " (reported by the Mail subsystem)"

data IntegrationAPIActor = IntegrationAPIActor MinutesTime IPAddress String (Maybe String)
                         deriving (Eq, Ord, Show)
instance Actor IntegrationAPIActor where
  actorTime      (IntegrationAPIActor t _ _ _) = t
  actorIP        (IntegrationAPIActor _ i _ _) = Just i
  actorAPIString (IntegrationAPIActor _ _ a _) = Just a
  actorWho       (IntegrationAPIActor _ _ _ c) = 
    case c of
      Just c' -> "the company " ++ show c' ++ " using the Integration API"
      Nothing -> "the Integration API" 
  
data UserActor = UserActor MinutesTime IPAddress UserID String
               deriving (Eq, Ord, Show)
instance Actor UserActor where
  actorTime      (UserActor t _ _ _) = t
  actorIP        (UserActor _ i _ _) = Just i
  actorUserID    (UserActor _ _ u _) = Just u
  actorEmail     (UserActor _ _ _ e) = Just e
  actorWho       (UserActor _ _ _ e) = "the user with email " ++ show e
  
data AdminActor = AdminActor MinutesTime IPAddress UserID String
                  deriving (Eq, Ord, Show)
instance Actor AdminActor where
  actorTime      (AdminActor t _ _ _) = t
  actorIP        (AdminActor _ i _ _) = Just i
  actorUserID    (AdminActor _ _ u _) = Just u
  actorEmail     (AdminActor _ _ _ e) = Just e
  actorWho       (AdminActor _ _ _ e) = "the admin with email " ++ show e

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
        ++ ", api_user"
        ++ ") VALUES (?,?,?,?,?,?,?,?,?,?)"
  res <- execute st [toSql mdid
                    ,toSql $ actorTime      actor
                    ,toSql text
                    ,toSql event
                    ,toSql versionID
                    ,toSql $ actorUserID    actor
                    ,toSql $ actorEmail     actor
                    ,toSql $ actorIP        actor
                    ,toSql $ actorSigLinkID actor
                    ,toSql $ actorAPIString actor
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
    
data DocumentEvidenceEvent = DocumentEvidenceEvent { evDocumentID :: DocumentID
                                                   , evTime       :: MinutesTime
                                                   , evText       :: String
                                                   , evType       :: EvidenceEventType
                                                   , evVersionID  :: String
                                                   , evEmail      :: Maybe String
                                                   , evUserID     :: Maybe UserID
                                                   , evIP4        :: Maybe IPAddress
                                                   , evIP6        :: Maybe IPAddress
                                                   , evSigLinkID  :: Maybe SignatoryLinkID
                                                   , evAPI        :: Maybe String
                                                   }
    
fetchEvidenceLog :: Statement -> [DocumentEvidenceEvent] -> IO [DocumentEvidenceEvent]
fetchEvidenceLog st acc = fetchRow st >>= maybe (return acc) f
  where f [did,tm,txt,tp,vid,uid,eml,ip4,ip6,slid,api] =
          fetchEvidenceLog st $ DocumentEvidenceEvent { evDocumentID = fromSql did
                                                      , evTime       = fromSql tm
                                                      , evText       = fromSql txt
                                                      , evType       = fromSql tp
                                                      , evVersionID  = fromSql vid
                                                      , evUserID     = fromSql uid
                                                      , evEmail      = fromSql eml
                                                      , evIP4        = fromSql ip4
                                                      , evIP6        = fromSql ip6
                                                      , evSigLinkID  = fromSql slid
                                                      , evAPI        = fromSql api
                                                      } : acc
        f l = error $ "fetchEvidenceLog: unexpected row: " ++ show l

data GetEvidenceLog = GetEvidenceLog DocumentID
instance DBQuery GetEvidenceLog [DocumentEvidenceEvent] where
  dbQuery (GetEvidenceLog did) = wrapDB $ \conn -> do
    st <- prepare conn $ "SELECT "
        ++ "  document_id"
        ++ ", time"
        ++ ", text"
        ++ ", event_type"
        ++ ", version_id"
        ++ ", user_id"
        ++ ", email"
        ++ ", request_ip_v4"
        ++ ", request_ip_v6"        
        ++ ", signatory_link_id"
        ++ ", api_user"
        ++ " FROM evidence_log "
        ++ " WHERE document_id = ?"
        ++ " ORDER BY time"
    _ <- execute st [toSql did]
    fetchEvidenceLog st []

copyEvidenceLogToNewDocument :: DocumentID -> DocumentID -> DB ()
copyEvidenceLogToNewDocument fromdoc todoc =
  wrapDB $ \conn -> do
    st <- prepare conn $ "INSERT INTO evidence_log ("
        ++ "  document_id"
        ++ ", time"
        ++ ", text"
        ++ ", event_type"
        ++ ", version_id"
        ++ ", user_id"
        ++ ", email"
        ++ ", request_ip_v4"
        ++ ", request_ip_v6"        
        ++ ", signatory_link_id"
        ++ ", api_user"
        ++ ") SELECT "
        ++ "  ?"
        ++ ", time"
        ++ ", text"
        ++ ", event_type"
        ++ ", version_id"
        ++ ", user_id"
        ++ ", email"
        ++ ", request_ip_v4"
        ++ ", request_ip_v6"        
        ++ ", signatory_link_id"
        ++ ", api_user"
        ++ " FROM evidence_log "
        ++ " WHERE document_id = ?"
    _ <- execute st [toSql todoc, toSql fromdoc]
    return ()

-- | A machine-readable event code for different types of events.
data EvidenceEventType =
  AddSigAttachmentEvidence                    |
  RemoveSigAttachmentsEvidence                |
  RemoveDocumentAttachmentEvidence            |
  AddDocumentAttachmentEvidence               |
  PendingToAwaitingAuthorEvidence             |
  UpdateFieldsEvidence                        |
  SetElegitimationIdentificationEvidence      |
  SetEmailIdentificationEvidence              |
  TimeoutDocumentEvidence                     |
  SignDocumentEvidence                        |
  SetInvitationDeliveryStatusEvidence         |
  SetDocumentUIEvidence                       |
  SetDocumentLocaleEvidence                   |
  SetDocumentTitleEvidence                    |
  SetDocumentAdvancedFunctionalityEvidence    |
  RemoveDaysToSignEvidence                    |
  SetDaysToSignEvidence                       |
  SetInvitationTextEvidence                   |
  RemoveSignatoryUserEvidence                 |
  SetSignatoryUserEvidence                    |
  RemoveSignatoryCompanyEvidence              |
  SetSignatoryCompanyEvidence                 |
  SetDocumentTagsEvidence                     |
  SaveSigAttachmentEvidence                   |
  SaveDocumentForUserEvidence                 |
  RestartDocumentEvidence                     |
  ReallyDeleteDocumentEvidence                |
  NewDocumentEvidence                         |
  MarkInvitationReadEvidence                  |
  CloseDocumentEvidence                       |
  ChangeSignatoryEmailWhenUndeliveredEvidence |
  ChangeMainfileEvidence                      |
  CancelDocumenElegEvidence                   |
  CancelDocumentEvidence                      |
  AttachFileEvidence                          |
  AttachSealedFileEvidence                    |
  PreparationToPendingEvidence                |
  DeleteSigAttachmentEvidence                 |
  AuthorUsesCSVEvidence                       |
  ErrorDocumentEvidence                       |
  MarkDocumentSeenEvidence                    |
  RejectDocumentEvidence                      |
  SetDocumentInviteTimeEvidence               |
  SetDocumentTimeoutTimeEvidence              |
  RestoreArchivedDocumentEvidence             |
  InvitationEvidence                          |
  SignableFromDocumentIDWithUpdatedAuthorEvidence |
  ArchiveDocumentEvidence                     |
  ResetSignatoryDetailsEvidence |
  AdminOnlySaveForUserEvidence |
  SignableFromDocumentEvidence |
  TemplateFromDocumentEvidence |
  AttachCSVUploadEvidence
  deriving (Eq, Show, Read, Ord)
$(enumDeriveConvertible ''EvidenceEventType)

