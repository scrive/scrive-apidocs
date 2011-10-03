{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches#-}

module Doc.Model (
    module Doc.File
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
  , MailsDeliveryStatus(..)
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
  ) where

import API.Service.Model
import DB.Classes
import DB.Derive
import DB.Types
--import DB.Utils
import Doc.File
import User.Region
import User.UserID
import User.Model
import Company.Model
import MinutesTime

import Data.Data
import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as BS

-- newtypes
newtype AuthorAttachment = AuthorAttachment { authorattachmentfile :: File }
  deriving (Eq, Ord, Show)

newtype DocumentID = DocumentID { unDocumentID :: Int64 }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''DocumentID)

newtype SignatoryAttachment = SignatoryAttachment { signatoryattachmentfile :: File }
  deriving (Eq, Ord, Show)

newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int64 }
  deriving (Eq, Ord, Data, Typeable)
$(newtypeDeriveUnderlyingReadShow ''SignatoryLinkID)

newtype SignOrder = SignOrder { unSignOrder :: Int32 }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''SignOrder)

-- enums
data CancelationReason = ManualCancel
                        -- The data returned by ELeg server
                        -- msg sl fn ln num
                        | ELegDataMismatch String SignatoryLinkID BS.ByteString BS.ByteString BS.ByteString
                          deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| CancelationReason |])

data DocumentFunctionality = BasicFunctionality | AdvancedFunctionality
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocumentFunctionality)

data DocumentLogEntry = DocumentLogEntry MinutesTime BS.ByteString
  deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| [DocumentLogEntry] |])

data DocumentProcess = Contract | Offer | Order
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocumentProcess)

data DocumentStatus = Preparation
                    | Pending
                    | Closed
                    | Canceled
                    | Timedout
                    | Rejected
                    | AwaitingAuthor
                    | DocumentError String
                      deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| DocumentStatus |])

data DocumentSharing = Private | Shared
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocumentSharing)

data DocumentType = Signable | Template | Attachment | AttachmentTemplate
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''DocumentType)

data FieldType =
    FirstNameFT | LastNameFT | CompanyFT | PersonalNumberFT
  | CompanyNumberFT | EmailFT | CustomFT BS.ByteString Bool -- label filledbyauthor
    deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| FieldType |])

data IdentificationType = EmailIdentification
                        | ELegitimationIdentification
                          deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''IdentificationType)

data MailsDeliveryStatus = Delivered | Undelivered | Unknown | Deferred
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''MailsDeliveryStatus)

data SignatoryRole = SignatoryPartner | SignatoryAuthor
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''SignatoryRole)

data SignatureProvider = BankIDProvider | TeliaProvider | NordeaProvider
  deriving (Eq, Ord, Show)
$(enumDeriveConvertible ''SignatureProvider)

-- data structures
data CSVUpload = CSVUpload {
    csvtitle          :: BS.ByteString
  , csvcontents       :: [[BS.ByteString]]
  , csvsignatoryindex :: Int32
  } deriving (Eq, Ord, Show)

data DocumentTag = DocumentTag {
    tagname  :: BS.ByteString
  , tagvalue :: BS.ByteString
  } deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| [DocumentTag] |])

data DocumentUI = DocumentUI {
    documentmailfooter :: Maybe BS.ByteString
  } deriving (Eq, Ord, Show)

data FieldPlacement = FieldPlacement {
    placementx          :: Int32
  , placementy          :: Int32
  , placementpage       :: Int32
  , placementpagewidth  :: Int32
  , placementpageheight :: Int32
  } deriving (Eq, Ord, Show, Data, Typeable)

data SignatoryDetails = SignatoryDetails {
    signatorysignorder :: SignOrder
  , signatoryfields    :: [SignatoryField]
  } deriving (Eq, Ord, Show)

data SignatoryField = SignatoryField {
    sfType       :: FieldType
  , sfValue      :: BS.ByteString
  , sfPlacements :: [FieldPlacement]
  } deriving (Eq, Ord, Show, Data, Typeable)
$(jsonableDeriveConvertible [t| [SignatoryField] |])

data SignatureInfo = SignatureInfo {
    signatureinfotext        :: String
  , signatureinfosignature   :: String
  , signatureinfocertificate :: String
  , signatureinfoprovider    :: SignatureProvider
  , signaturefstnameverified :: Bool
  , signaturelstnameverified :: Bool
  , signaturepersnumverified :: Bool
  } deriving (Eq, Ord, Show)

data SignInfo = SignInfo {
    signtime :: MinutesTime
  , signipnumber :: Word32
  } deriving (Eq, Ord, Show)

data SignatoryLink = SignatoryLink {
    signatorylinkid            :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
  , signatorydetails           :: SignatoryDetails    -- ^ details of this person as filled in invitation
  , signatorymagichash         :: MagicHash           -- ^ authentication code
  , maybesignatory             :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
  , maybecompany               :: Maybe CompanyID     -- ^ if this document has been saved to a company account this is the companyid
  , maybesigninfo              :: Maybe SignInfo      -- ^ when a person has signed this document
  , maybeseeninfo              :: Maybe SignInfo      -- ^ when a person has first seen this document
  , maybereadinvite            :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read
  , invitationdeliverystatus   :: MailsDeliveryStatus -- ^ status of email delivery
  , signatorysignatureinfo     :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
  , signatoryroles             :: [SignatoryRole]
  , signatorylinkdeleted       :: Bool -- ^ when true sends the doc to the recycle bin for that sig
  , signatorylinkreallydeleted :: Bool -- ^ when true it means that the doc has been removed from the recycle bin
  } deriving (Eq, Ord, Show)

data Document = Document {
    documentid                     :: DocumentID
  , documenttitle                  :: BS.ByteString
  , documentsignatorylinks         :: [SignatoryLink]
  , documentfiles                  :: [File]
  , documentsealedfiles            :: [File]
  , documentstatus                 :: DocumentStatus
  , documentprocess                :: DocumentProcess
  , documenttype                   :: DocumentType
  , documentfunctionality          :: DocumentFunctionality
  , documentctime                  :: MinutesTime
  , documentmtime                  :: MinutesTime
  , documentdaystosign             :: Maybe Int
  , documenttimeouttime            :: Maybe MinutesTime
  , documentinvitetime             :: Maybe SignInfo
  , documentlog                    :: [DocumentLogEntry]  -- to be made into plain text
  , documentinvitetext             :: BS.ByteString
  , documenttrustweaverreference   :: Maybe BS.ByteString
  , documentallowedidtypes         :: [IdentificationType]
  , documentcsvupload              :: Maybe CSVUpload
  , documentcancelationreason      :: Maybe CancelationReason -- When a document iscancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
  , documentsharing                :: DocumentSharing
  , documentrejectioninfo          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
  , documenttags                   :: [DocumentTag]
  , documentservice                :: Maybe ServiceID
  , documentdeleted                :: Bool -- set to true when doc is deleted - the other fields will be cleared too, so it is really truely deleting, it's just we want to avoid re-using the docid.
  , documentauthorattachments      :: [AuthorAttachment]
  , documentsignatoryattachments   :: [SignatoryAttachment]
  , documentregion                 :: Region
  , documentui                     :: DocumentUI
  } deriving (Eq, Ord, Show)



data AdminOnlySaveForUser = AdminOnlySaveForUser DocumentID User 
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AdminOnlySaveForUser Document where
  dbUpdate (AdminOnlySaveForUser docid user) = wrapDB $ \conn -> error ""

data ArchiveDocumentForAll = ArchiveDocumentForAll DocumentID 
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocumentForAll Document where
  dbUpdate (ArchiveDocumentForAll docid) = wrapDB $ \conn -> error ""

data ArchiveDocuments = ArchiveDocuments User [DocumentID] deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ArchiveDocuments [Document] where
  dbUpdate (ArchiveDocuments user docids) = wrapDB $ \conn -> error ""

data AttachCSVUpload = AttachCSVUpload DocumentID CSVUpload
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachCSVUpload Document where
  dbUpdate (AttachCSVUpload docid csvupload) = wrapDB $ \conn -> error ""

data AttachFile = AttachFile DocumentID BS.ByteString BS.ByteString
                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachFile Document where
  dbUpdate (AttachFile docid filename content) = wrapDB $ \conn -> error ""

data AttachSealedFile = AttachSealedFile DocumentID BS.ByteString BS.ByteString
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AttachSealedFile Document where
  dbUpdate (AttachSealedFile docid filename content) = wrapDB $ \conn -> error ""

data AuthorSendDocument = AuthorSendDocument DocumentID MinutesTime Word32 (Maybe SignatureInfo)
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AuthorSendDocument Document where
  dbUpdate (AuthorSendDocument docid mtime ipaddress msiginfo) = wrapDB $ \conn -> error ""

data AuthorSignDocument = AuthorSignDocument DocumentID MinutesTime Word32 (Maybe SignatureInfo)
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate AuthorSignDocument Document where
  dbUpdate (AuthorSignDocument docid mtime ipaddress msiginfo) = wrapDB $ \conn -> error ""

data CancelDocument = CancelDocument DocumentID CancelationReason MinutesTime Word32
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CancelDocument Document where
  dbUpdate (CancelDocument docid reason mtime ipaddress) = wrapDB $ \conn -> error ""

data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered DocumentID SignatoryLinkID (Maybe User) BS.ByteString
                                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ChangeSignatoryEmailWhenUndelivered Document where
  dbUpdate (ChangeSignatoryEmailWhenUndelivered docid siglinkid muser email) = wrapDB $ \conn -> error ""

data CloseDocument = CloseDocument DocumentID MinutesTime Word32 (Maybe SignatureInfo)
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate CloseDocument Document where
  dbUpdate (CloseDocument docid time ipaddress msiginfo) = wrapDB $ \conn -> error ""

data DeleteDocumentRecordIfRequired = DeleteDocumentRecordIfRequired DocumentID [User]
                                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DeleteDocumentRecordIfRequired Document where
  dbUpdate (DeleteDocumentRecordIfRequired docid users) = wrapDB $ \conn -> error ""

data DeleteSigAttachment = DeleteSigAttachment DocumentID BS.ByteString FileID
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DeleteSigAttachment Document where
  dbUpdate (DeleteSigAttachment docid email fid) = wrapDB $ \conn -> error ""

data DocumentFromSignatoryData = DocumentFromSignatoryData DocumentID Int BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString BS.ByteString [BS.ByteString]
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate DocumentFromSignatoryData Document where
  dbUpdate (DocumentFromSignatoryData docid sigindex fstname sndname email company personalnumber companynumber fieldvalues) = wrapDB $ \conn -> error ""

data ErrorDocument = ErrorDocument DocumentID String
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ErrorDocument Document where
  dbUpdate (ErrorDocument docid errmsg) = wrapDB $ \conn -> do
                                              error ""

data FileModTime = FileModTime FileID deriving (Eq, Ord, Show, Typeable)
instance DBQuery FileModTime MinutesTime where
  dbQuery (FileModTime fid) = wrapDB $ \conn -> do
                                             error ""

data FileMovedToAWS = FileMovedToAWS FileID BS.ByteString BS.ByteString
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate FileMovedToAWS Document where
  dbUpdate (FileMovedToAWS fid bucket url) = wrapDB $ \conn -> do
                                              error ""

data FileMovedToDisk = FileMovedToDisk FileID FilePath
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate FileMovedToDisk Document where
  dbUpdate (FileMovedToDisk fid filepath) = wrapDB $ \conn -> do
                                              error ""

data GetDeletedDocumentsByCompany = GetDeletedDocumentsByCompany User
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByCompany [Document] where
  dbQuery (GetDeletedDocumentsByCompany uid) = wrapDB $ \conn -> do
                                             error ""

data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser User
                                 deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDeletedDocumentsByUser [Document] where
  dbQuery (GetDeletedDocumentsByUser uid) = wrapDB $ \conn -> do
                                             error ""

data GetDocumentByDocumentID = GetDocumentByDocumentID DocumentID
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentByDocumentID (Maybe Document) where
  dbQuery (GetDocumentByDocumentID did) = wrapDB $ \conn -> do
                                             error ""

-- FIXME: this is wrong, FileID may be in many documents
data GetDocumentByFileID = GetDocumentByFileID FileID
                           deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentByFileID Document where
  dbQuery (GetDocumentByFileID fid) = wrapDB $ \conn -> do
                                             error ""


data GetDocumentStats = GetDocumentStats
                        deriving (Eq, Ord, Show, Typeable)

{-
instance DBQuery GetDocumentStats DocStats where
  dbQuery (GetDocumentStats) = wrapDB $ \conn -> do
                                             error ""
-}

data GetDocumentStatsByUser = GetDocumentStatsByUser User MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
{-
instance DBQuery GetDocumentStatsByUser DocStats where
  dbQuery (GetDocumentStatsByUser user) = wrapDB $ \conn -> do
                                             error ""
-}

data GetDocuments = GetDocuments (Maybe ServiceID)
                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocuments [Document] where
  dbQuery (GetDocuments mserviceid) = wrapDB $ \conn -> do
                                             error ""

data GetDocumentsByAuthor = GetDocumentsByAuthor UserID
                            deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByAuthor [Document] where
  dbQuery (GetDocumentsByAuthor userid) = wrapDB $ \conn -> do
                                             error ""

data GetDocumentsByCompany = GetDocumentsByCompany User
                             deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompany [Document] where
  dbQuery (GetDocumentsByCompany userid) = wrapDB $ \conn -> do
                                             error ""

data GetDocumentsByCompanyAndTags = GetDocumentsByCompanyAndTags (Maybe ServiceID) CompanyID [DocumentTag]
                                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByCompanyAndTags [Document] where
  dbQuery (GetDocumentsByCompanyAndTags mservice companyid doctags) = wrapDB $ \conn -> do
                                             error ""

data GetDocumentsByDocumentID = GetDocumentsByDocumentID [DocumentID]
                                deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByDocumentID [Document] where
  dbQuery (GetDocumentsByDocumentID docids) = wrapDB $ \conn -> do
                                             error ""


data GetDocumentsBySignatory = GetDocumentsBySignatory User
                               deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsBySignatory [Document] where
  dbQuery (GetDocumentsBySignatory user) = wrapDB $ \conn -> do
                                             error ""

data GetDocumentsByUser = GetDocumentsByUser User
                          deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsByUser [Document] where
  dbQuery (GetDocumentsByUser user) = wrapDB $ \conn -> do
                                             error ""

data GetDocumentsSharedInCompany = GetDocumentsSharedInCompany User
                                   deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetDocumentsSharedInCompany [Document] where
  dbQuery (GetDocumentsSharedInCompany user) = wrapDB $ \conn -> do
                                             error ""

data GetFilesThatShouldBeMovedToAmazon = GetFilesThatShouldBeMovedToAmazon
                                         deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetFilesThatShouldBeMovedToAmazon [File] where
  dbQuery (GetFilesThatShouldBeMovedToAmazon) = wrapDB $ \conn -> do
                                             error ""

data GetMagicHash = GetMagicHash
                    deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetMagicHash MagicHash where
  dbQuery (GetMagicHash) = wrapDB $ \conn -> do
                                             error ""

data GetNumberOfDocumentsOfUser = GetNumberOfDocumentsOfUser User
                                  deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetNumberOfDocumentsOfUser Int where
  dbQuery (GetNumberOfDocumentsOfUser user) = wrapDB $ \conn -> do
                                             error ""

data GetSignatoryLinkIDs = GetSignatoryLinkIDs
                           deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetSignatoryLinkIDs Int where
  dbQuery (GetSignatoryLinkIDs) = wrapDB $ \conn -> do
                                             error ""

data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments MinutesTime
                                       deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetTimeoutedButPendingDocuments [Document] where
  dbQuery (GetTimeoutedButPendingDocuments mtime) = wrapDB $ \conn -> do
                                             error ""

data GetUniqueSignatoryLinkID = GetUniqueSignatoryLinkID
                                deriving (Eq, Ord, Show, Typeable)
instance DBQuery GetUniqueSignatoryLinkID SignatoryLinkID where
  dbQuery (GetUniqueSignatoryLinkID) = wrapDB $ \conn -> do
                                             error ""

data MarkDocumentSeen = MarkDocumentSeen DocumentID SignatoryLinkID MagicHash MinutesTime Word32
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkDocumentSeen Document where
  dbUpdate (MarkDocumentSeen documentid signatorylinkid1 mh time ipnumber) = wrapDB $ \conn -> do
                                              error ""

data MarkInvitationRead = MarkInvitationRead DocumentID SignatoryLinkID MinutesTime
                          deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MarkInvitationRead Document where
  dbUpdate (MarkInvitationRead documentid linkid time) = wrapDB $ \conn -> do
                                              error ""

data MigrateDocumentSigLinkCompanies = MigrateDocumentSigLinkCompanies DocumentID [User]
                                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate MigrateDocumentSigLinkCompanies Document where
  dbUpdate (MigrateDocumentSigLinkCompanies docid sigusers) = wrapDB $ \conn -> do
                                              error ""

data NewDocument = NewDocument User (Maybe Company) BS.ByteString DocumentType MinutesTime
                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate NewDocument Document where
  dbUpdate (NewDocument user mcompany title documenttype ctime) = wrapDB $ \conn -> do
                                              error ""

data ReallyDeleteDocuments = ReallyDeleteDocuments User [(DocumentID, [User])]
                             deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ReallyDeleteDocuments [Document] where
  dbUpdate (ReallyDeleteDocuments deletinguser docidsAndUsers) = wrapDB $ \conn -> do
                                              error ""

data RejectDocument = RejectDocument DocumentID SignatoryLinkID MinutesTime Word32 (Maybe BS.ByteString)
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RejectDocument Document where
  dbUpdate (RejectDocument documentid signatorylinkid1 time ipnumber customtext) = wrapDB $ \conn -> do
                                              error ""

data RestartDocument = RestartDocument Document User MinutesTime Word32
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestartDocument Document where
  dbUpdate (RestartDocument doc user time ipnumber) = wrapDB $ \conn -> do
                                              error ""

data RestoreArchivedDocuments = RestoreArchivedDocuments User [DocumentID]
                                deriving (Eq, Ord, Show, Typeable)
instance DBUpdate RestoreArchivedDocuments [Document] where
  dbUpdate (RestoreArchivedDocuments user docids) = wrapDB $ \conn -> do
                                              error ""

data SaveDocumentForUser = SaveDocumentForUser DocumentID User SignatoryLinkID 
                           deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveDocumentForUser Document where
  dbUpdate (SaveDocumentForUser documentid User{userid, usercompany} signatorylinkid1) = wrapDB $ \conn -> do
                                              error ""

data SaveSigAttachment = SaveSigAttachment DocumentID BS.ByteString BS.ByteString BS.ByteString
                         deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SaveSigAttachment Document where
  dbUpdate (SaveSigAttachment docid name email content) = wrapDB $ \conn -> do
                                              error ""


data SetDocumentTags = SetDocumentTags DocumentID [DocumentTag]
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTags Document where
  dbUpdate (SetDocumentTags docid doctags) = wrapDB $ \conn -> do
                                              error ""


data SetDocumentTimeoutTime = SetDocumentTimeoutTime DocumentID MinutesTime
                              deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTimeoutTime Document where
  dbUpdate (SetDocumentTimeoutTime documentid timeouttime) = wrapDB $ \conn -> do
                                              error ""


data SetDocumentTitle = SetDocumentTitle DocumentID BS.ByteString
                        deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTitle Document where
  dbUpdate (SetDocumentTitle docid doctitle) = wrapDB $ \conn -> do
                                              error ""

data SetDocumentTrustWeaverReference = SetDocumentTrustWeaverReference DocumentID String
                                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentTrustWeaverReference Document where
  dbUpdate (SetDocumentTrustWeaverReference documentid reference) = wrapDB $ \conn -> do
                                              error ""

data SetDocumentUI = SetDocumentUI DocumentID DocumentUI
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetDocumentUI Document where
  dbUpdate (SetDocumentUI documentid documentui) = wrapDB $ \conn -> do
                                              error ""


data SetInvitationDeliveryStatus = SetInvitationDeliveryStatus DocumentID SignatoryLinkID MailsDeliveryStatus
                                   deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SetInvitationDeliveryStatus Document where
  dbUpdate (SetInvitationDeliveryStatus docid siglnkid status) = wrapDB $ \conn -> do
                                              error ""


data ShareDocument = ShareDocument DocumentID 
                     deriving (Eq, Ord, Show, Typeable)
instance DBUpdate ShareDocument Document where
  dbUpdate (ShareDocument docid) = wrapDB $ \conn -> do
                                              error ""


data SignDocument = SignDocument DocumentID SignatoryLinkID MinutesTime Word32 (Maybe SignatureInfo) [(BS.ByteString, BS.ByteString)]
                    deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignDocument Document where
  dbUpdate (SignDocument documentid signatorylinkid1 time ipnumber msiginfo fields) = wrapDB $ \conn -> do
                                              error ""


data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest SignatoryDetails [SignatoryRole]
                                  deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignLinkFromDetailsForTest SignatoryLink where
  dbUpdate (SignLinkFromDetailsForTest details roles) = wrapDB $ \conn -> do
                                              error ""

data SignableFromDocument = SignableFromDocument Document 
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocument Document where
  dbUpdate (SignableFromDocument document) = wrapDB $ \conn -> do
                                              error ""


data SignableFromDocumentIDWithUpdatedAuthor = SignableFromDocumentIDWithUpdatedAuthor User (Maybe Company) DocumentID
                                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate SignableFromDocumentIDWithUpdatedAuthor Document where
  dbUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany docid) = wrapDB $ \conn -> do
                                              error ""


data StoreDocumentForTesting = StoreDocumentForTesting Document 
                               deriving (Eq, Ord, Show, Typeable)
instance DBUpdate StoreDocumentForTesting DocumentID where
  dbUpdate (StoreDocumentForTesting document) = wrapDB $ \conn -> do
                                              error ""

data TemplateFromDocument = TemplateFromDocument DocumentID 
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TemplateFromDocument Document where
  dbUpdate (TemplateFromDocument documentid) = wrapDB $ \conn -> do
                                              error ""

data TimeoutDocument = TimeoutDocument DocumentID MinutesTime
                       deriving (Eq, Ord, Show, Typeable)
instance DBUpdate TimeoutDocument Document where
  dbUpdate (TimeoutDocument documentid time) = wrapDB $ \conn -> do
                                              error ""

data UpdateDocument = UpdateDocument MinutesTime DocumentID BS.ByteString [(SignatoryDetails,[SignatoryRole])] (Maybe Int)
                      BS.ByteString (SignatoryDetails, [SignatoryRole], UserID, Maybe CompanyID) [IdentificationType]
                      (Maybe Int) DocumentFunctionality
                      deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocument Document where
  dbUpdate (UpdateDocument time documentid docname signatories daystosign invitetext (authordetails, authorroles, authorid, mcompanyid) idtypes mcsvsigindex docfunctionality) = wrapDB $ \conn -> do
                                              error ""

data UpdateDocumentAttachments = UpdateDocumentAttachments DocumentID [DocumentID] [FileID]
                                 deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocumentAttachments Document where
  dbUpdate (UpdateDocumentAttachments docid idstoadd idstoremove) = wrapDB $ \conn -> do
                                              error ""

data UpdateDocumentSimple = UpdateDocumentSimple DocumentID (SignatoryDetails, User) [SignatoryDetails]
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateDocumentSimple Document where
  dbUpdate (UpdateDocumentSimple did (authordetails,author) signatories) = wrapDB $ \conn -> do
                                              error ""


data UpdateSigAttachments = UpdateSigAttachments DocumentID [SignatoryAttachment]
                            deriving (Eq, Ord, Show, Typeable)
instance DBUpdate UpdateSigAttachments Document where
  dbUpdate (UpdateSigAttachments docid sigatts) = wrapDB $ \conn -> do
                                              error ""
