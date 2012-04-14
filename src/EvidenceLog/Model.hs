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
         copyEvidenceLogToNewDocument,
         mkAuthorActor,
         mkAdminActor,
         htmlDocFromEvidenceLog
       )
       where

import API.Service.Model
import Context
import DB
import Doc.DocStateData
import EvidenceLog.Tables
import IPAddress
import MinutesTime
import Misc
import User.Model
import Util.HasSomeUserInfo
import Version
import qualified Data.ByteString.UTF8 as BS
import Templates.Templates
import qualified Templates.Fields as F

-- | Actor describes who is performing an action and when
class Actor a where
  -- | the time the action is taken
  actorTime      :: a -> MinutesTime
  -- | If the action is originated on another machine, its IP
  actorIP        :: a -> Maybe IPAddress
  actorIP _      = Nothing
  -- | If the action is originated by a logged in user
  actorUserID    :: a -> Maybe UserID
  actorUserID _  = Nothing
  -- | If the action is originated by a person with email address
  actorEmail     :: a -> Maybe String
  actorEmail _   = Nothing
  -- | If the action is originated by a signatory on the document being acted on
  actorSigLinkID :: a -> Maybe SignatoryLinkID
  actorSigLinkID _ = Nothing
  -- | If the action is originated by an api call, a string to describe it
  actorAPIString :: a -> Maybe String
  actorAPIString _ = Nothing
  -- | A textual string describing this actor, used for building evidence strings
  actorWho       :: a -> String

-- | The Scrive System acting on its own
data SystemActor = SystemActor MinutesTime
                   deriving (Eq, Ord, Show)
instance Actor SystemActor where
  actorTime (SystemActor t) = t
  actorWho _ = "the Scrive system"

mkAuthorActor :: Context -> Maybe AuthorActor
mkAuthorActor ctx = case ctxmaybeuser ctx of
  Just user -> Just $ AuthorActor (ctxtime ctx) (ctxipnumber ctx) (userid user) (getEmail user)
  Nothing   -> Nothing

mkAdminActor :: Context -> Maybe AdminActor
mkAdminActor ctx = case ctxmaybeuser ctx of
  Just user -> Just $ AdminActor (ctxtime ctx) (ctxipnumber ctx) (userid user) (getEmail user)
  Nothing   -> Nothing

-- | For an action that requires an operation on a document and an
-- author to be logged in
data AuthorActor = AuthorActor 
                   MinutesTime 
                   IPAddress 
                   UserID 
                   String -- ^ Email address
                   deriving (Eq, Ord, Show)
instance Actor AuthorActor where
  actorTime   (AuthorActor t _ _ _) = t
  actorIP     (AuthorActor _ i _ _) = Just i
  actorUserID (AuthorActor _ _ u _) = Just u
  actorEmail  (AuthorActor _ _ _ e) = Just e
  actorWho    (AuthorActor _ _ _ e) = "the author (" ++ e ++ ")"

-- | For an action requiring a signatory with siglinkid and token (such as signing)
data SignatoryActor = SignatoryActor 
                      MinutesTime 
                      IPAddress 
                      (Maybe UserID) 
                      String -- ^ Email address
                      SignatoryLinkID
                   deriving (Eq, Ord, Show)
instance Actor SignatoryActor where
  actorTime      (SignatoryActor t _ _ _ _) = t
  actorIP        (SignatoryActor _ i _ _ _) = Just i
  actorUserID    (SignatoryActor _ _ u _ _) = u
  actorEmail     (SignatoryActor _ _ _ e _) = Just e
  actorSigLinkID (SignatoryActor _ _ _ _ s) = Just s
  actorWho       (SignatoryActor _ _ _ e _) = "the signatory with email " ++ show e
  
-- | For documents created using mailapi/scrivebymail
data MailAPIActor = MailAPIActor 
                    MinutesTime 
                    UserID 
                    String -- ^ Email address
                   deriving (Eq, Ord, Show)
instance Actor MailAPIActor where
  actorTime   (MailAPIActor t _ _) = t
  actorUserID (MailAPIActor _ u _) = Just u
  actorEmail  (MailAPIActor _ _ e) = Just e
  actorWho    (MailAPIActor _ _ e) = "the user with email " ++ show e ++ " using the Mail API"
  actorAPIString _ = Just "Mail API"
                    
-- | For delivery/reading notifications from the mail system
data MailSystemActor = MailSystemActor 
                       MinutesTime 
                       (Maybe UserID) 
                       String -- ^ Email address
                       SignatoryLinkID
                   deriving (Eq, Ord, Show)
instance Actor MailSystemActor where
  actorTime      (MailSystemActor t _ _ _) = t
  actorUserID    (MailSystemActor _ u _ _) = u
  actorEmail     (MailSystemActor _ _ e _) = Just e
  actorSigLinkID (MailSystemActor _ _ _ s) = Just s
  actorWho       (MailSystemActor _ _ e _) = "the signatory with email " ++ show e ++ " (reported by the Mail subsystem)"

-- | For actions originating from the integration api
data IntegrationAPIActor = IntegrationAPIActor 
                           MinutesTime 
                           IPAddress 
                           ServiceID 
                           (Maybe String) -- ^ Company name
                         deriving (Eq, Ord, Show)
instance Actor IntegrationAPIActor where
  actorTime      (IntegrationAPIActor t _ _ _) = t
  actorIP        (IntegrationAPIActor _ i _ _) = Just i
  actorAPIString (IntegrationAPIActor _ _ a _) = Just $ BS.toString $ unServiceID a
  actorWho       (IntegrationAPIActor _ _ _ c) = 
    case c of
      Just c' -> "the company " ++ show c' ++ " using the Integration API"
      Nothing -> "the Integration API" 
  
-- | For actions performed by logged in user
data UserActor = UserActor 
                 MinutesTime 
                 IPAddress 
                 UserID 
                 String -- ^ email address
               deriving (Eq, Ord, Show)
instance Actor UserActor where
  actorTime      (UserActor t _ _ _) = t
  actorIP        (UserActor _ i _ _) = Just i
  actorUserID    (UserActor _ _ u _) = Just u
  actorEmail     (UserActor _ _ _ e) = Just e
  actorWho       (UserActor _ _ _ e) = "the user with email " ++ show e
  
-- | For actions performed by an admin
data AdminActor = AdminActor 
                  MinutesTime 
                  IPAddress 
                  UserID 
                  String -- ^ Email address
                  deriving (Eq, Ord, Show)
instance Actor AdminActor where
  actorTime      (AdminActor t _ _ _) = t
  actorIP        (AdminActor _ i _ _) = Just i
  actorUserID    (AdminActor _ _ u _) = Just u
  actorEmail     (AdminActor _ _ _ e) = Just e
  actorWho       (AdminActor _ _ _ e) = "the admin with email " ++ show e

data Actor a => InsertEvidenceEvent a = InsertEvidenceEvent 
                                        EvidenceEventType       -- A code for the event
                                        String                  -- Text for evidence
                                        (Maybe DocumentID)      -- The documentid if this event is about a document
                                        a                       -- Actor
instance (Actor a, MonadDB m) => DBUpdate m (InsertEvidenceEvent a) Bool where
  update (InsertEvidenceEvent event text mdid actor) =
    kRun01 $ mkSQL INSERT tableEvidenceLog [
      sql "document_id" mdid
    , sql "time" $ actorTime actor
    , sql "text" text
    , sql "event_type" event
    , sql "version_id" versionID
    , sql "user_id" $ actorUserID actor
    , sql "email" $ actorEmail actor
    , sql "request_ip_v4" $ actorIP actor
    , sql "signatory_link_id" $ actorSigLinkID actor
    , sql "api_user" $ actorAPIString actor
    ]

data DocumentEvidenceEvent = DocumentEvidenceEvent {
    evDocumentID :: DocumentID
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

htmlDocFromEvidenceLog :: TemplatesMonad m => String -> [DocumentEvidenceEvent] -> m String
htmlDocFromEvidenceLog title elog = do
  renderTemplate "htmlevidencelog" $ do
    F.value "documenttitle" title
    F.objects "entries" $ for elog $ \entry -> do
      F.value "time" $ formatMinutesTimeUTC (evTime entry) ++ " UTC"
      F.value "ip"   $ show $ evIP4 entry
      F.value "text" $ evText entry

data GetEvidenceLog = GetEvidenceLog DocumentID
instance MonadDB m => DBQuery m GetEvidenceLog [DocumentEvidenceEvent] where
  query (GetEvidenceLog docid) = do
    _ <- kRun $ SQL ("SELECT "
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
      ++ "  FROM evidence_log "
      ++ "  WHERE document_id = ?"
      ++ "  ORDER BY time DESC") [
        toSql docid
      ]
    foldDB fetchEvidenceLog []
    where
      fetchEvidenceLog acc did' tm txt tp vid uid eml ip4 ip6 slid api =
        DocumentEvidenceEvent {
            evDocumentID = did'
          , evTime       = tm
          , evText       = txt
          , evType       = tp
          , evVersionID  = vid
          , evUserID     = uid
          , evEmail      = eml
          , evIP4        = ip4
          , evIP6        = ip6
          , evSigLinkID  = slid
          , evAPI        = api
          } : acc

copyEvidenceLogToNewDocument :: MonadDB m => DocumentID -> DocumentID -> DBEnv m ()
copyEvidenceLogToNewDocument fromdoc todoc = do
  _ <- kRun $ SQL ("INSERT INTO evidence_log ("
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
    ++ " WHERE document_id = ?") [
      toSql todoc
    , toSql fromdoc
    ]
  return ()

-- | A machine-readable event code for different types of events.
data EvidenceEventType =
  AddSigAttachmentEvidence                        |
  RemoveSigAttachmentsEvidence                    |
  RemoveDocumentAttachmentEvidence                |
  AddDocumentAttachmentEvidence                   |
  PendingToAwaitingAuthorEvidence                 |
  UpdateFieldsEvidence                            |
  SetElegitimationIdentificationEvidence          |
  SetEmailIdentificationEvidence                  |
  TimeoutDocumentEvidence                         |
  SignDocumentEvidence                            |
  SetInvitationDeliveryStatusEvidence             |
  SetDocumentUIEvidence                           |
  SetDocumentLocaleEvidence                       |
  SetDocumentTitleEvidence                        |
  SetDocumentAdvancedFunctionalityEvidence        |
  RemoveDaysToSignEvidence                        |
  SetDaysToSignEvidence                           |
  SetInvitationTextEvidence                       |
  RemoveSignatoryUserEvidence                     |
  SetSignatoryUserEvidence                        |
  RemoveSignatoryCompanyEvidence                  |
  SetSignatoryCompanyEvidence                     |
  SetDocumentTagsEvidence                         |
  SaveSigAttachmentEvidence                       |
  SaveDocumentForUserEvidence                     |
  RestartDocumentEvidence                         |
  ReallyDeleteDocumentEvidence                    |
  NewDocumentEvidence                             |
  MarkInvitationReadEvidence                      |
  CloseDocumentEvidence                           |
  ChangeSignatoryEmailWhenUndeliveredEvidence     |
  ChangeMainfileEvidence                          |
  CancelDocumenElegEvidence                       |
  CancelDocumentEvidence                          |
  AttachFileEvidence                              |
  AttachSealedFileEvidence                        |
  PreparationToPendingEvidence                    |
  DeleteSigAttachmentEvidence                     |
  AuthorUsesCSVEvidence                           |
  ErrorDocumentEvidence                           |
  MarkDocumentSeenEvidence                        |
  RejectDocumentEvidence                          |
  SetDocumentInviteTimeEvidence                   |
  SetDocumentTimeoutTimeEvidence                  |
  RestoreArchivedDocumentEvidence                 |
  InvitationEvidence                              |
  SignableFromDocumentIDWithUpdatedAuthorEvidence |
  ArchiveDocumentEvidence                         |
  ResetSignatoryDetailsEvidence                   |
  AdminOnlySaveForUserEvidence                    |
  SignableFromDocumentEvidence                    |
  TemplateFromDocumentEvidence                    |
  AttachCSVUploadEvidence                         |
  SendToPadDevice                                 |
  RemovedFromPadDevice
  deriving (Eq, Show, Read, Ord)

instance Convertible EvidenceEventType Int where
  safeConvert AddSigAttachmentEvidence                        = return 1
  safeConvert RemoveSigAttachmentsEvidence                    = return 2
  safeConvert RemoveDocumentAttachmentEvidence                = return 3
  safeConvert AddDocumentAttachmentEvidence                   = return 4
  safeConvert PendingToAwaitingAuthorEvidence                 = return 5
  safeConvert UpdateFieldsEvidence                            = return 6
  safeConvert SetElegitimationIdentificationEvidence          = return 7
  safeConvert SetEmailIdentificationEvidence                  = return 8
  safeConvert TimeoutDocumentEvidence                         = return 9
  safeConvert SignDocumentEvidence                            = return 10
  safeConvert SetInvitationDeliveryStatusEvidence             = return 11
  safeConvert SetDocumentUIEvidence                           = return 12
  safeConvert SetDocumentLocaleEvidence                       = return 13
  safeConvert SetDocumentTitleEvidence                        = return 14
  safeConvert SetDocumentAdvancedFunctionalityEvidence        = return 15
  safeConvert RemoveDaysToSignEvidence                        = return 16
  safeConvert SetDaysToSignEvidence                           = return 17
  safeConvert SetInvitationTextEvidence                       = return 18
  safeConvert RemoveSignatoryUserEvidence                     = return 19
  safeConvert SetSignatoryUserEvidence                        = return 20
  safeConvert RemoveSignatoryCompanyEvidence                  = return 21
  safeConvert SetSignatoryCompanyEvidence                     = return 22
  safeConvert SetDocumentTagsEvidence                         = return 23
  safeConvert SaveSigAttachmentEvidence                       = return 24
  safeConvert SaveDocumentForUserEvidence                     = return 25
  safeConvert RestartDocumentEvidence                         = return 26
  safeConvert ReallyDeleteDocumentEvidence                    = return 27
  safeConvert NewDocumentEvidence                             = return 28
  safeConvert MarkInvitationReadEvidence                      = return 29
  safeConvert CloseDocumentEvidence                           = return 30
  safeConvert ChangeSignatoryEmailWhenUndeliveredEvidence     = return 31
  safeConvert ChangeMainfileEvidence                          = return 32
  safeConvert CancelDocumenElegEvidence                       = return 33
  safeConvert CancelDocumentEvidence                          = return 34
  safeConvert AttachFileEvidence                              = return 35
  safeConvert AttachSealedFileEvidence                        = return 36
  safeConvert PreparationToPendingEvidence                    = return 37
  safeConvert DeleteSigAttachmentEvidence                     = return 38
  safeConvert AuthorUsesCSVEvidence                           = return 39
  safeConvert ErrorDocumentEvidence                           = return 40
  safeConvert MarkDocumentSeenEvidence                        = return 41
  safeConvert RejectDocumentEvidence                          = return 42
  safeConvert SetDocumentInviteTimeEvidence                   = return 43
  safeConvert SetDocumentTimeoutTimeEvidence                  = return 44
  safeConvert RestoreArchivedDocumentEvidence                 = return 45
  safeConvert InvitationEvidence                              = return 46
  safeConvert SignableFromDocumentIDWithUpdatedAuthorEvidence = return 47
  safeConvert ArchiveDocumentEvidence                         = return 48
  safeConvert ResetSignatoryDetailsEvidence                   = return 49
  safeConvert AdminOnlySaveForUserEvidence                    = return 50
  safeConvert SignableFromDocumentEvidence                    = return 51
  safeConvert TemplateFromDocumentEvidence                    = return 52
  safeConvert AttachCSVUploadEvidence                         = return 53
  safeConvert SendToPadDevice                                 = return 54
  safeConvert RemovedFromPadDevice                            = return 55
  
instance Convertible Int EvidenceEventType where
    safeConvert 1  = return AddSigAttachmentEvidence
    safeConvert 2  = return RemoveSigAttachmentsEvidence
    safeConvert 3  = return RemoveDocumentAttachmentEvidence
    safeConvert 4  = return AddDocumentAttachmentEvidence
    safeConvert 5  = return PendingToAwaitingAuthorEvidence
    safeConvert 6  = return UpdateFieldsEvidence
    safeConvert 7  = return SetElegitimationIdentificationEvidence
    safeConvert 8  = return SetEmailIdentificationEvidence
    safeConvert 9  = return TimeoutDocumentEvidence
    safeConvert 10 = return SignDocumentEvidence
    safeConvert 11 = return SetInvitationDeliveryStatusEvidence
    safeConvert 12 = return SetDocumentUIEvidence
    safeConvert 13 = return SetDocumentLocaleEvidence
    safeConvert 14 = return SetDocumentTitleEvidence
    safeConvert 15 = return SetDocumentAdvancedFunctionalityEvidence
    safeConvert 16 = return RemoveDaysToSignEvidence
    safeConvert 17 = return SetDaysToSignEvidence
    safeConvert 18 = return SetInvitationTextEvidence
    safeConvert 19 = return RemoveSignatoryUserEvidence
    safeConvert 20 = return SetSignatoryUserEvidence
    safeConvert 21 = return RemoveSignatoryCompanyEvidence
    safeConvert 22 = return SetSignatoryCompanyEvidence
    safeConvert 23 = return SetDocumentTagsEvidence
    safeConvert 24 = return SaveSigAttachmentEvidence
    safeConvert 25 = return SaveDocumentForUserEvidence
    safeConvert 26 = return RestartDocumentEvidence
    safeConvert 27 = return ReallyDeleteDocumentEvidence
    safeConvert 28 = return NewDocumentEvidence
    safeConvert 29 = return MarkInvitationReadEvidence
    safeConvert 30 = return CloseDocumentEvidence
    safeConvert 31 = return ChangeSignatoryEmailWhenUndeliveredEvidence
    safeConvert 32 = return ChangeMainfileEvidence
    safeConvert 33 = return CancelDocumenElegEvidence
    safeConvert 34 = return CancelDocumentEvidence
    safeConvert 35 = return AttachFileEvidence
    safeConvert 36 = return AttachSealedFileEvidence
    safeConvert 37 = return PreparationToPendingEvidence
    safeConvert 38 = return DeleteSigAttachmentEvidence
    safeConvert 39 = return AuthorUsesCSVEvidence
    safeConvert 40 = return ErrorDocumentEvidence
    safeConvert 41 = return MarkDocumentSeenEvidence
    safeConvert 42 = return RejectDocumentEvidence
    safeConvert 43 = return SetDocumentInviteTimeEvidence
    safeConvert 44 = return SetDocumentTimeoutTimeEvidence
    safeConvert 45 = return RestoreArchivedDocumentEvidence
    safeConvert 46 = return InvitationEvidence
    safeConvert 47 = return SignableFromDocumentIDWithUpdatedAuthorEvidence
    safeConvert 48 = return ArchiveDocumentEvidence
    safeConvert 49 = return ResetSignatoryDetailsEvidence
    safeConvert 50 = return AdminOnlySaveForUserEvidence
    safeConvert 51 = return SignableFromDocumentEvidence
    safeConvert 52 = return TemplateFromDocumentEvidence
    safeConvert 53 = return AttachCSVUploadEvidence
    safeConvert 54 = return SendToPadDevice
    safeConvert 55 = return RemovedFromPadDevice
    safeConvert s  = Left ConvertError { convSourceValue = show s
                                       , convSourceType = "Int"
                                       , convDestType = "EvidenceEventType"
                                       , convErrorMessage = "Convertion error: value " ++ show s ++ " not mapped"
                                       }

instance Convertible EvidenceEventType SqlValue where
  safeConvert e = fmap toSql (safeConvert e :: Either ConvertError Int)

instance Convertible SqlValue EvidenceEventType where
  safeConvert s = safeConvert (fromSql s :: Int)
