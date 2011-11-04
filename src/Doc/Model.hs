{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE CPP #-}

module Doc.Model
  ( module File.File
  , isTemplate -- fromUtils
  , isShared -- fromUtils
  , isDeletableDocument -- fromUtils
  , anyInvitationUndelivered
  , undeliveredSignatoryLinks
  , AuthorAttachment(..)
  , DocumentID(..)
  , SignatoryAttachment(..)
  , SignatoryLinkID(..)
  , SignOrder(..)
  , CancelationReason(..)
  , DocumentFunctionality(..)
  , DocumentLogEntry(..)
  , DocumentProcess(..)
  , DocumentStatus(..)
  , DocumentSharing(..)
  , DocumentType(..)
  , FieldType(..)
  , IdentificationType(..)
  , SignatoryRole(..)
  , SignatureProvider(..)
  , CSVUpload(..)
  , DocumentTag(..)
  , DocumentUI(..)
  , FieldPlacement(..)
  , SignatoryDetails(..)
  , SignatoryField(..)
  , SignatureInfo(..)
  , SignInfo(..)
  , SignatoryLink(..)
  , Document(..)
  , DocStats(..)

  , ArchiveDocuments(..)
  , ArchiveDocumentForAll(..)
  , ArchiveDocumentForAuthor(..)
  , RestoreArchivedDocuments(..)
  , ReallyDeleteDocuments(..)
  , DeleteDocumentRecordIfRequired(..)
  , AttachFile(..)
  , AttachSealedFile(..)
  , ChangeMainfile(..)
  , RejectDocument(..)
  , GetDocumentByDocumentID(..)
  , GetDocumentStats(..)
  , GetDocumentStatsByUser(..)
  , GetDocuments(..)
  , GetDocumentsByAuthor(..)
  , GetDocumentsBySignatory(..)
  , GetDocumentsByCompany(..)
  , GetDocumentsSharedInCompany(..)
  , GetDocumentsByUser(..)
  , GetDeletedDocumentsByCompany(..)
  , GetDeletedDocumentsByUser(..)
  , GetNumberOfDocumentsOfUser(..)
  , GetTimeoutedButPendingDocuments(..)
  , MarkDocumentSeen(..)
  , MarkInvitationRead(..)
  , SetInvitationDeliveryStatus(..)
  , NewDocument(..)
  , SaveDocumentForUser(..)
  , SetDocumentTimeoutTime(..)
  , SetDocumentTags(..)
  , SetDocumentUI(..)
  , GetDocumentsByCompanyAndTags(..)
  , ShareDocument(..)
  , SetDocumentTitle(..)
  , SignDocument(..)
  , TimeoutDocument(..)
  , UpdateDocumentSimple(..)
  , AttachCSVUpload(..)
  , GetDocumentsByDocumentID(..)
  , AddDocumentAttachment(..)
  , RemoveDocumentAttachment(..)
  , CloseDocument(..)
  , CancelDocument(..)
  , RestartDocument(..)
  , ChangeSignatoryEmailWhenUndelivered(..)
  , GetUniqueSignatoryLinkID(..)
  , GetMagicHash(..)
  , ErrorDocument(..)
  , TemplateFromDocument(..)
  , SignableFromDocument(..)
  , SignableFromDocumentIDWithUpdatedAuthor(..)
  , DocumentFromSignatoryData(..)
  , UpdateSigAttachments(..)
  , SaveSigAttachment(..)
  , PreparationToPending(..)
  , AddInvitationEvidence(..)
  , UpdateFields(..)
  , PendingToAwaitingAuthor(..)
  , SetSignatoryCompany(..)
  , RemoveSignatoryCompany(..)
  , SetSignatoryUser(..)
  , RemoveSignatoryUser(..)
  , SetInviteText(..)
  , SetDaysToSign(..)
  , RemoveDaysToSign(..)
  , SetDocumentFunctionality(..)
  , SetCSVSigIndex(..)
  , SetEmailIdentification(..)
  , SetElegitimationIdentification(..)
  , ResetSignatoryDetails(..)
  , SetDocumentLocale(..)
  --, MigrateDocumentSigAccounts(..)
  , MigrateDocumentSigLinkCompanies(..)
  -- , FixBug510ForDocument(..)
  , StoreDocumentForTesting(..)
  , SignLinkFromDetailsForTest(..)
  , DeleteSigAttachment(..)
  , GetSignatoryLinkIDs(..)
  , AdminOnlySaveForUser(..)
  ) where

import API.Service.Model
import DB.Classes
--import DB.Derive
import DB.Types
--import DB.Utils
import File.File
import File.FileID
--import User.Region
import Doc.DocUtils
import User.UserID
import User.Model
import Company.Model
import MinutesTime
import Doc.DocStateData
import Data.Data
--import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as BS
import qualified Mails.MailsUtil as Mail

unimplemented :: String -> a
unimplemented msg = error ("Unimplemented in Doc/Model: " ++ msg)

data AdminOnlySaveForUser = AdminOnlySaveForUser DocumentID User
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AdminOnlySaveForUser (Either String Document) where
  dbUpdate (AdminOnlySaveForUser docid user) = wrapDB $ \conn -> do
    unimplemented "AdminOnlySaveForUser"

data ArchiveDocumentForAll = ArchiveDocumentForAll DocumentID
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocumentForAll (Either String Document) where
  dbUpdate (ArchiveDocumentForAll docid) = wrapDB $ \conn -> do
    unimplemented "ArchiveDocumentForAll"

data ArchiveDocumentForAuthor = ArchiveDocumentForAuthor DocumentID
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocumentForAuthor (Either String Document) where
  dbUpdate (ArchiveDocumentForAuthor docid) = wrapDB $ \conn -> do
    unimplemented "ArchiveDocumentForAuthor"

data ArchiveDocuments = ArchiveDocuments User [DocumentID] deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocuments (Either String [Document]) where
  dbUpdate (ArchiveDocuments user docids) = wrapDB $ \conn -> do
    unimplemented "ArchiveDocuments"

data AttachCSVUpload = AttachCSVUpload DocumentID CSVUpload
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachCSVUpload (Either String Document) where
  dbUpdate (AttachCSVUpload docid csvupload) = wrapDB $ \conn -> do
    unimplemented "AttachCSVUpload"

data AttachFile = AttachFile DocumentID FileID MinutesTime
                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachFile (Either String Document) where
  dbUpdate (AttachFile docid fid time) = wrapDB $ \conn -> do
    unimplemented "AttachFile"

data AttachSealedFile = AttachSealedFile DocumentID FileID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachSealedFile (Either String Document) where
  dbUpdate (AttachSealedFile docid fid) = wrapDB $ \conn -> do
    unimplemented "AttachSealedFile"

data AuthorSendDocument = AuthorSendDocument DocumentID MinutesTime Word32 (Maybe SignatureInfo)
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AuthorSendDocument (Either String Document) where
  dbUpdate (AuthorSendDocument docid mtime ipaddress msiginfo) = wrapDB $ \conn -> do
    unimplemented "AuthorSendDocument"

data AuthorSignDocument = AuthorSignDocument DocumentID MinutesTime Word32 (Maybe SignatureInfo)
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AuthorSignDocument (Either String Document) where
  dbUpdate (AuthorSignDocument docid mtime ipaddress msiginfo) = wrapDB $ \conn -> do
    unimplemented "AuthorSignDocument"

data CancelDocument = CancelDocument DocumentID CancelationReason MinutesTime Word32
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CancelDocument (Either String Document) where
  dbUpdate (CancelDocument docid reason mtime ipaddress) = wrapDB $ \conn -> do
    unimplemented "CancelDocument"

data ChangeMainfile = ChangeMainfile DocumentID FileID
instance DBUpdate ChangeMainfile (Either String Document) where
  dbUpdate (ChangeMainfile did fid) = wrapDB $ \conn -> do
    unimplemented "ChangeMainfile"

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) BS.ByteString
                                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ChangeSignatoryEmailWhenUndelivered (Either String Document) where
  dbUpdate (ChangeSignatoryEmailWhenUndelivered docid siglinkid muser email) = wrapDB $ \conn -> do
    unimplemented "ChangeSignatoryEmailWhenUndelivered"


data PreparationToPending = PreparationToPending DocumentID MinutesTime
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PreparationToPending (Either String Document) where
  dbUpdate (PreparationToPending docid time) = wrapDB $ \conn -> do
    unimplemented "PreparationToPending"

data CloseDocument = CloseDocument DocumentID MinutesTime Word32
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CloseDocument (Either String Document) where
  dbUpdate (CloseDocument docid time ipaddress) = wrapDB $ \conn -> do
    unimplemented "CloseDocument"

data DeleteDocumentRecordIfRequired = DeleteDocumentRecordIfRequired DocumentID [User]
                                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DeleteDocumentRecordIfRequired (Either String Document) where
  dbUpdate (DeleteDocumentRecordIfRequired docid users) = wrapDB $ \conn -> do
    unimplemented "DeleteDocumentRecordIfRequired"

data DeleteSigAttachment = DeleteSigAttachment DocumentID BS.ByteString FileID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DeleteSigAttachment (Either String Document) where
  dbUpdate (DeleteSigAttachment docid email fid) = wrapDB $ \conn -> do
    unimplemented "DeleteSigAttachment"

data DocumentFromSignatoryData = DocumentFromSignatoryData DocumentID Int BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString [BS.ByteString]
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DocumentFromSignatoryData (Either String Document) where
  dbUpdate (DocumentFromSignatoryData docid sigindex fstname sndname email company personalnumber companynumber fieldvalues) = wrapDB $ \conn -> do
    unimplemented "DocumentFromSignatoryData"

data ErrorDocument = ErrorDocument DocumentID String
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ErrorDocument (Either String Document) where
  dbUpdate (ErrorDocument docid errmsg) = wrapDB $ \conn -> do
    unimplemented "ErrorDocument"

data FileModTime = FileModTime FileID deriving (Eq, Ord, Show, Typeable)
instance DBQuery FileModTime MinutesTime where
  dbQuery (FileModTime fid) = wrapDB $ \conn -> do
    unimplemented "FileModTime"

data GetDeletedDocumentsByCompany = GetDeletedDocumentsByCompany User
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByCompany [Document] where
  dbQuery (GetDeletedDocumentsByCompany uid) = wrapDB $ \conn -> do
    unimplemented "GetDeletedDocumentsByCompany"

data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser User
                                 deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByUser [Document] where
  dbQuery (GetDeletedDocumentsByUser uid) = wrapDB $ \conn -> do
    unimplemented "GetDeletedDocumentsByUser"

data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentByDocumentID (Maybe Document) where
  dbQuery (GetDocumentByDocumentID did) = wrapDB $ \conn -> do
    unimplemented "GetDocumentByDocumentID"


data GetDocumentStats = GetDocumentStats
                        deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentStats DocStats where
  dbQuery (GetDocumentStats) = wrapDB $ \conn -> do
    unimplemented "GetDocumentStats"

data GetDocumentStatsByUser = GetDocumentStatsByUser User MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentStatsByUser DocStats where
  dbQuery (GetDocumentStatsByUser user time) = wrapDB $ \conn -> do
    unimplemented "GetDocumentStatsByUser"

data GetDocuments = GetDocuments (Maybe ServiceID)
                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocuments [Document] where
  dbQuery (GetDocuments mserviceid) = wrapDB $ \conn -> do
    unimplemented "GetDocuments"

data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
                            deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByAuthor [Document] where
  dbQuery (GetDocumentsByAuthor userid) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByAuthor"

data GetDocumentsByCompany = GetDocumentsByCompany User
                             deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompany [Document] where
  dbQuery (GetDocumentsByCompany userid) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByCompany"

data GetDocumentsByCompanyAndTags = GetDocumentsByCompanyAndTags (Maybe ServiceID) CompanyID [DocumentTag]
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompanyAndTags [Document] where
  dbQuery (GetDocumentsByCompanyAndTags mservice companyid doctags) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByCompanyAndTags"

data GetDocumentsByDocumentID = GetDocumentsByDocumentID [DocumentID]
                                deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByDocumentID [Document] where
  dbQuery (GetDocumentsByDocumentID docids) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByDocumentID"


data GetDocumentsBySignatory = GetDocumentsBySignatory User
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsBySignatory [Document] where
  dbQuery (GetDocumentsBySignatory user) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsBySignatory"

data GetDocumentsByUser = GetDocumentsByUser User
                          deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByUser [Document] where
  dbQuery (GetDocumentsByUser user) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsByUser"

data GetDocumentsSharedInCompany = GetDocumentsSharedInCompany User
                                   deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsSharedInCompany [Document] where
  dbQuery (GetDocumentsSharedInCompany user) = wrapDB $ \conn -> do
    unimplemented "GetDocumentsSharedInCompany"

data GetMagicHash = GetMagicHash
                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetMagicHash MagicHash where
  dbQuery (GetMagicHash) = wrapDB $ \conn -> do
    unimplemented "GetMagicHash"

data GetNumberOfDocumentsOfUser = GetNumberOfDocumentsOfUser User
                                  deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetNumberOfDocumentsOfUser Int where
  dbQuery (GetNumberOfDocumentsOfUser user) = wrapDB $ \conn -> do
    unimplemented "GetNumberOfDocumentsOfUser"

data GetSignatoryLinkIDs = GetSignatoryLinkIDs
                           deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetSignatoryLinkIDs [SignatoryLinkID] where
  dbQuery (GetSignatoryLinkIDs) = wrapDB $ \conn -> do
    unimplemented "GetSignatoryLinkIDs"

data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments MinutesTime
                                       deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetTimeoutedButPendingDocuments [Document] where
  dbQuery (GetTimeoutedButPendingDocuments mtime) = wrapDB $ \conn -> do
    unimplemented "GetTimeoutedButPendingDocuments"

data GetUniqueSignatoryLinkID = GetUniqueSignatoryLinkID
                                deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetUniqueSignatoryLinkID SignatoryLinkID where
  dbQuery (GetUniqueSignatoryLinkID) = wrapDB $ \conn -> do
    unimplemented "GetUniqueSignatoryLinkID"

data MarkDocumentSeen = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash MinutesTime Word32
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkDocumentSeen (Either String Document) where
  dbUpdate (MarkDocumentSeen documentid signatorylinkid1 mh time ipnumber) = wrapDB $ \conn -> do
    unimplemented "MarkDocumentSeen"

data AddInvitationEvidence = AddInvitationEvidence DocumentID SignatoryLinkID MinutesTime Word32
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AddInvitationEvidence (Either String Document) where
  dbUpdate (AddInvitationEvidence docid slid time ipnumber) = wrapDB $ \conn -> do
    unimplemented "AddInvitationEvidence"

data MarkInvitationRead = MarkInvitationRead DocumentID SignatoryLinkID MinutesTime
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkInvitationRead (Either String Document) where
  dbUpdate (MarkInvitationRead documentid linkid time) = wrapDB $ \conn -> do
    unimplemented "MarkInvitationRead"

data MigrateDocumentSigLinkCompanies = MigrateDocumentSigLinkCompanies DocumentID [User]
                                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MigrateDocumentSigLinkCompanies (Either String Document) where
  dbUpdate (MigrateDocumentSigLinkCompanies docid sigusers) = wrapDB $ \conn -> do
    unimplemented "MigrateDocumentSigLinkCompanies"

data NewDocument = NewDocument User (Maybe Company) BS.ByteString DocumentType MinutesTime
                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate NewDocument (Either String Document) where
  dbUpdate (NewDocument user mcompany title documenttype ctime) = wrapDB $ \conn -> do
    unimplemented "NewDocument"

data ReallyDeleteDocuments = ReallyDeleteDocuments User [(DocumentID, [User])]
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ReallyDeleteDocuments (Either String [Document]) where
  dbUpdate (ReallyDeleteDocuments deletinguser docidsAndUsers) = wrapDB $ \conn -> do
    unimplemented "ReallyDeleteDocuments"

data RejectDocument = RejectDocument DocumentID SignatoryLinkID MinutesTime Word32 (Maybe BS.ByteString)
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RejectDocument (Either String Document) where
  dbUpdate (RejectDocument documentid signatorylinkid1 time ipnumber customtext) = wrapDB $ \conn -> do
    unimplemented "RejectDocument"

data RestartDocument = RestartDocument Document User MinutesTime Word32
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestartDocument (Either String Document) where
  dbUpdate (RestartDocument doc user time ipnumber) = wrapDB $ \conn -> do
    unimplemented "RestartDocument"

data RestoreArchivedDocuments = RestoreArchivedDocuments User [DocumentID]
                                deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestoreArchivedDocuments (Either String [Document]) where
  dbUpdate (RestoreArchivedDocuments user docids) = wrapDB $ \conn -> do
    unimplemented "RestoreArchivedDocuments"

data SaveDocumentForUser = SaveDocumentForUser DocumentID User SignatoryLinkID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveDocumentForUser (Either String Document) where
  dbUpdate (SaveDocumentForUser documentid User{userid, usercompany} signatorylinkid1) = wrapDB $ \conn -> do
    unimplemented "SaveDocumentForUser"

data SaveSigAttachment = SaveSigAttachment DocumentID BS.ByteString BS.ByteString FileID
                         deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveSigAttachment (Either String Document) where
  dbUpdate (SaveSigAttachment docid name email fid) = wrapDB $ \conn -> do
    unimplemented "SaveSigAttachment"


data SetDocumentTags = SetDocumentTags DocumentID [DocumentTag]
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTags (Either String Document) where
  dbUpdate (SetDocumentTags docid doctags) = wrapDB $ \conn -> do
    unimplemented "SetDocumentTags"


data SetDocumentTimeoutTime = SetDocumentTimeoutTime DocumentID MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTimeoutTime (Either String Document) where
  dbUpdate (SetDocumentTimeoutTime documentid timeouttime) = wrapDB $ \conn -> do
    unimplemented "SetDocumentTimeoutTime"

data SetSignatoryCompany = SetSignatoryCompany DocumentID SignatoryLinkID CompanyID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetSignatoryCompany (Either String Document) where
  dbUpdate (SetSignatoryCompany documentid slid cid) = wrapDB $ \conn -> do
    unimplemented "SetSignatoryCompany"

data RemoveSignatoryCompany = RemoveSignatoryCompany DocumentID SignatoryLinkID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveSignatoryCompany (Either String Document) where
  dbUpdate (RemoveSignatoryCompany documentid slid) = wrapDB $ \conn -> do
    unimplemented "RemoveSignatoryCompany"

data SetSignatoryUser = SetSignatoryUser DocumentID SignatoryLinkID UserID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetSignatoryUser (Either String Document) where
  dbUpdate (SetSignatoryUser documentid slid uid) = wrapDB $ \conn -> do
    unimplemented "SetSignatoryUser"

data RemoveSignatoryUser = RemoveSignatoryUser DocumentID SignatoryLinkID
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveSignatoryUser (Either String Document) where
  dbUpdate (RemoveSignatoryUser documentid slid) = wrapDB $ \conn -> do
    unimplemented "RemoveSignatoryUser"

data SetInviteText = SetInviteText DocumentID BS.ByteString MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInviteText (Either String Document) where
  dbUpdate (SetInviteText docid text time) = wrapDB $ \conn -> do
    unimplemented "SetInviteText"

data SetDaysToSign = SetDaysToSign DocumentID Int MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDaysToSign (Either String Document) where
  dbUpdate (SetDaysToSign docid days time) = wrapDB $ \conn -> do
    unimplemented "SetDaysToSign"

data RemoveDaysToSign = RemoveDaysToSign DocumentID MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveDaysToSign (Either String Document) where
  dbUpdate (RemoveDaysToSign docid time) = wrapDB $ \conn -> do
    unimplemented "RemoveDaysToSign"

data SetDocumentFunctionality = SetDocumentFunctionality DocumentID DocumentFunctionality MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentFunctionality (Either String Document) where
  dbUpdate (SetDocumentFunctionality did docfunc time) = wrapDB $ \conn -> do
    unimplemented "SetDocumentFunctionality"


data SetDocumentTitle = SetDocumentTitle DocumentID BS.ByteString MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTitle (Either String Document) where
  dbUpdate (SetDocumentTitle docid doctitle mtime) = wrapDB $ \conn -> do
    unimplemented "SetDocumentTitle"

data SetDocumentLocale = SetDocumentLocale DocumentID Locale MinutesTime
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentLocale (Either String Document) where
  dbUpdate (SetDocumentLocale did locale time) = wrapDB $ \conn -> do
    unimplemented "SetDocumentLocale"

data SetDocumentTrustWeaverReference = SetDocumentTrustWeaverReference DocumentID String
                                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTrustWeaverReference (Either String Document) where
  dbUpdate (SetDocumentTrustWeaverReference documentid reference) = wrapDB $ \conn -> do
    unimplemented "SetDocumentTrustWeaverReference"

data SetDocumentUI = SetDocumentUI DocumentID DocumentUI
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentUI (Either String Document) where
  dbUpdate (SetDocumentUI documentid documentui) = wrapDB $ \conn -> do
    unimplemented "SetDocumentUI"


data SetInvitationDeliveryStatus = SetInvitationDeliveryStatus DocumentID SignatoryLinkID Mail.MailsDeliveryStatus
                                   deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInvitationDeliveryStatus (Either String Document) where
  dbUpdate (SetInvitationDeliveryStatus docid siglnkid status) = wrapDB $ \conn -> do
    unimplemented "SetInvitationDeliveryStatus"


data ShareDocument = ShareDocument DocumentID
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ShareDocument (Either String Document) where
  dbUpdate (ShareDocument docid) = wrapDB $ \conn -> do
    unimplemented "ShareDocument"


data SignDocument = SignDocument DocumentID SignatoryLinkID MagicHash MinutesTime Word32 (Maybe SignatureInfo)
                    deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignDocument (Either String Document) where
  dbUpdate (SignDocument documentid signatorylinkid1 mh time ipnumber msiginfo) = wrapDB $ \conn -> do
    unimplemented "SignDocument"


data ResetSignatoryDetails = ResetSignatoryDetails DocumentID [(SignatoryDetails, [SignatoryRole])] MinutesTime
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ResetSignatoryDetails (Either String Document) where
  dbUpdate (ResetSignatoryDetails documentid signatories time) = wrapDB $ \conn -> do
    unimplemented "ResetSignatoryDetails"


data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest SignatoryDetails [SignatoryRole]
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignLinkFromDetailsForTest SignatoryLink where
  dbUpdate (SignLinkFromDetailsForTest details roles) = wrapDB $ \conn -> do
    unimplemented "SignLinkFromDetailsForTest"

data SignableFromDocument = SignableFromDocument Document
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocument (Either String Document) where
  dbUpdate (SignableFromDocument document) = wrapDB $ \conn -> do
    unimplemented "SignableFromDocument"


data SignableFromDocumentIDWithUpdatedAuthor = SignableFromDocumentIDWithUpdatedAuthor User (Maybe Company) DocumentID
                                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocumentIDWithUpdatedAuthor (Either String Document) where
  dbUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid) = wrapDB $ \conn -> do
    unimplemented "SignableFromDocumentIDWithUpdatedAuthor"


data StoreDocumentForTesting = StoreDocumentForTesting Document
                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate StoreDocumentForTesting DocumentID where
  dbUpdate (StoreDocumentForTesting document) = wrapDB $ \conn -> do
    unimplemented "StoreDocumentForTesting"

data TemplateFromDocument = TemplateFromDocument DocumentID
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TemplateFromDocument (Either String Document) where
  dbUpdate (TemplateFromDocument documentid) = wrapDB $ \conn -> do
    unimplemented "TemplateFromDocument"

data TimeoutDocument = TimeoutDocument DocumentID MinutesTime
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TimeoutDocument (Either String Document) where
  dbUpdate (TimeoutDocument documentid time) = wrapDB $ \conn -> do
    unimplemented "TimeoutDocument"

data UpdateDocument = UpdateDocument MinutesTime DocumentID BS.ByteString [(SignatoryDetails,[SignatoryRole])] (Maybe Int)
                      BS.ByteString (SignatoryDetails, [SignatoryRole], UserID, Maybe CompanyID) [IdentificationType]
                      (Maybe Int) DocumentFunctionality
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocument (Either String Document) where
  dbUpdate (UpdateDocument time documentid docname signatories daystosign invitetext (authordetails, authorroles, authorid, mcompanyid) idtypes mcsvsigindex docfunctionality) = wrapDB $ \conn -> do
    unimplemented "UpdateDocument"


data SetCSVSigIndex = SetCSVSigIndex DocumentID Int MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetCSVSigIndex (Either String Document) where
  dbUpdate (SetCSVSigIndex did csvsigindex time) = wrapDB $ \conn -> do
    unimplemented "SetCSVSigIndex"

data SetEmailIdentification = SetEmailIdentification DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetEmailIdentification (Either String Document) where
  dbUpdate (SetEmailIdentification did time) = wrapDB $ \conn -> do
    unimplemented "SetEmailIdentification"

data SetElegitimationIdentification = SetElegitimationIdentification DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetElegitimationIdentification (Either String Document) where
  dbUpdate (SetElegitimationIdentification did time) = wrapDB $ \conn -> do
    unimplemented "SetElegitimationIdentification"


data UpdateFields  = UpdateFields DocumentID SignatoryLinkID [(BS.ByteString, BS.ByteString)]
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateFields (Either String Document) where
  dbUpdate (UpdateFields docid slid fields) = wrapDB $ \conn -> do
    unimplemented "UpdateFields"

data PendingToAwaitingAuthor = PendingToAwaitingAuthor DocumentID MinutesTime
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate PendingToAwaitingAuthor (Either String Document) where
  dbUpdate (PendingToAwaitingAuthor docid time) = wrapDB $ \conn -> do
    unimplemented "PendingToAwaitingAuthor"


data UpdateDocumentAttachments = UpdateDocumentAttachments DocumentID [DocumentID] [FileID]
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocumentAttachments (Either String Document) where
  dbUpdate (UpdateDocumentAttachments docid idstoadd idstoremove) = wrapDB $ \conn -> do
    unimplemented "UpdateDocumentAttachments"

data AddDocumentAttachment = AddDocumentAttachment DocumentID FileID
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AddDocumentAttachment (Either String Document) where
  dbUpdate (AddDocumentAttachment docid fid) = wrapDB $ \conn -> do
    unimplemented "AddDocumentAttachment"

data RemoveDocumentAttachment = RemoveDocumentAttachment DocumentID FileID 
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RemoveDocumentAttachment (Either String Document) where
  dbUpdate (RemoveDocumentAttachment docid fid) = wrapDB $ \conn -> do
    unimplemented "RemoveDocumentAttachment"


data UpdateDocumentSimple = UpdateDocumentSimple DocumentID (SignatoryDetails, User) [SignatoryDetails]
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocumentSimple (Either String Document) where
  dbUpdate (UpdateDocumentSimple did (authordetails,author) signatories) = wrapDB $ \conn -> do
    unimplemented "UpdateDocumentSimple"


data UpdateSigAttachments = UpdateSigAttachments DocumentID [SignatoryAttachment] MinutesTime
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateSigAttachments (Either String Document) where
  dbUpdate (UpdateSigAttachments docid sigatts time) = wrapDB $ \conn -> do
    unimplemented "UpdateSigAttachments"
