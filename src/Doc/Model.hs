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
import DB.Derive
import DB.Types
import Doc.File
import User.Region
import User.UserID
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



data AdminOnlySaveForUser = AdminOnlySaveForUser deriving (Eq, Ord, Show, Typeable)
data ArchiveDocumentForAll = ArchiveDocumentForAll deriving (Eq, Ord, Show, Typeable)
data ArchiveDocuments = ArchiveDocuments deriving (Eq, Ord, Show, Typeable)
data AttachCSVUpload = AttachCSVUpload deriving (Eq, Ord, Show, Typeable)
data AttachFile = AttachFile deriving (Eq, Ord, Show, Typeable)
data AttachSealedFile = AttachSealedFile deriving (Eq, Ord, Show, Typeable)
data AuthorSendDocument = AuthorSendDocument deriving (Eq, Ord, Show, Typeable)
data AuthorSignDocument = AuthorSignDocument deriving (Eq, Ord, Show, Typeable)
data CancelDocument = CancelDocument deriving (Eq, Ord, Show, Typeable)
data ChangeSignatoryEmailWhenUndelivered = ChangeSignatoryEmailWhenUndelivered deriving (Eq, Ord, Show, Typeable)
data CloseDocument = CloseDocument deriving (Eq, Ord, Show, Typeable)
data DeleteDocumentRecordIfRequired = DeleteDocumentRecordIfRequired deriving (Eq, Ord, Show, Typeable)
data DeleteSigAttachment = DeleteSigAttachment deriving (Eq, Ord, Show, Typeable)
data DocumentFromSignatoryData = DocumentFromSignatoryData deriving (Eq, Ord, Show, Typeable)
data ErrorDocument = ErrorDocument deriving (Eq, Ord, Show, Typeable)
data FileModTime = FileModTime deriving (Eq, Ord, Show, Typeable)
data FileMovedToAWS = FileMovedToAWS deriving (Eq, Ord, Show, Typeable)
data FileMovedToDisk = FileMovedToDisk deriving (Eq, Ord, Show, Typeable)
data GetDeletedDocumentsByCompany = GetDeletedDocumentsByCompany deriving (Eq, Ord, Show, Typeable)
data GetDeletedDocumentsByUser = GetDeletedDocumentsByUser deriving (Eq, Ord, Show, Typeable)
data GetDocumentByDocumentID = GetDocumentByDocumentID deriving (Eq, Ord, Show, Typeable)
data GetDocumentByFileID = GetDocumentByFileID deriving (Eq, Ord, Show, Typeable)
data GetDocumentStats = GetDocumentStats deriving (Eq, Ord, Show, Typeable)
data GetDocumentStatsByUser = GetDocumentStatsByUser deriving (Eq, Ord, Show, Typeable)
data GetDocuments = GetDocuments deriving (Eq, Ord, Show, Typeable)
data GetDocumentsByAuthor = GetDocumentsByAuthor deriving (Eq, Ord, Show, Typeable)
data GetDocumentsByCompany = GetDocumentsByCompany deriving (Eq, Ord, Show, Typeable)
data GetDocumentsByCompanyAndTags = GetDocumentsByCompanyAndTags deriving (Eq, Ord, Show, Typeable)
data GetDocumentsByDocumentID = GetDocumentsByDocumentID deriving (Eq, Ord, Show, Typeable)
data GetDocumentsBySignatory = GetDocumentsBySignatory deriving (Eq, Ord, Show, Typeable)
data GetDocumentsByUser = GetDocumentsByUser deriving (Eq, Ord, Show, Typeable)
data GetDocumentsSharedInCompany = GetDocumentsSharedInCompany deriving (Eq, Ord, Show, Typeable)
data GetFilesThatShouldBeMovedToAmazon = GetFilesThatShouldBeMovedToAmazon deriving (Eq, Ord, Show, Typeable)
data GetMagicHash = GetMagicHash deriving (Eq, Ord, Show, Typeable)
data GetNumberOfDocumentsOfUser = GetNumberOfDocumentsOfUser deriving (Eq, Ord, Show, Typeable)
data GetSignatoryLinkIDs = GetSignatoryLinkIDs deriving (Eq, Ord, Show, Typeable)
data GetTimeoutedButPendingDocuments = GetTimeoutedButPendingDocuments deriving (Eq, Ord, Show, Typeable)
data GetUniqueSignatoryLinkID = GetUniqueSignatoryLinkID deriving (Eq, Ord, Show, Typeable)
data MarkDocumentSeen = MarkDocumentSeen deriving (Eq, Ord, Show, Typeable)
data MarkInvitationRead = MarkInvitationRead deriving (Eq, Ord, Show, Typeable)
data MigrateDocumentSigLinkCompanies = MigrateDocumentSigLinkCompanies deriving (Eq, Ord, Show, Typeable)
data NewDocument = NewDocument deriving (Eq, Ord, Show, Typeable)
data ReallyDeleteDocuments = ReallyDeleteDocuments deriving (Eq, Ord, Show, Typeable)
data RejectDocument = RejectDocument deriving (Eq, Ord, Show, Typeable)
data RestartDocument = RestartDocument deriving (Eq, Ord, Show, Typeable)
data RestoreArchivedDocuments = RestoreArchivedDocuments deriving (Eq, Ord, Show, Typeable)
data SaveDocumentForUser = SaveDocumentForUser deriving (Eq, Ord, Show, Typeable)
data SaveSigAttachment = SaveSigAttachment deriving (Eq, Ord, Show, Typeable)
data SetDocumentTags = SetDocumentTags deriving (Eq, Ord, Show, Typeable)
data SetDocumentTimeoutTime = SetDocumentTimeoutTime deriving (Eq, Ord, Show, Typeable)
data SetDocumentTitle = SetDocumentTitle deriving (Eq, Ord, Show, Typeable)
data SetDocumentTrustWeaverReference = SetDocumentTrustWeaverReference deriving (Eq, Ord, Show, Typeable)
data SetDocumentUI = SetDocumentUI deriving (Eq, Ord, Show, Typeable)
data SetInvitationDeliveryStatus = SetInvitationDeliveryStatus deriving (Eq, Ord, Show, Typeable)
data ShareDocument = ShareDocument deriving (Eq, Ord, Show, Typeable)
data SignDocument = SignDocument deriving (Eq, Ord, Show, Typeable)
data SignLinkFromDetailsForTest = SignLinkFromDetailsForTest deriving (Eq, Ord, Show, Typeable)
data SignableFromDocument = SignableFromDocument deriving (Eq, Ord, Show, Typeable)
data SignableFromDocumentIDWithUpdatedAuthor = SignableFromDocumentIDWithUpdatedAuthor deriving (Eq, Ord, Show, Typeable)
data StoreDocumentForTesting = StoreDocumentForTesting deriving (Eq, Ord, Show, Typeable)
data TemplateFromDocument = TemplateFromDocument deriving (Eq, Ord, Show, Typeable)
data TimeoutDocument = TimeoutDocument deriving (Eq, Ord, Show, Typeable)
data UpdateDocument = UpdateDocument deriving (Eq, Ord, Show, Typeable)
data UpdateDocumentAttachments = UpdateDocumentAttachments deriving (Eq, Ord, Show, Typeable)
data UpdateDocumentSimple = UpdateDocumentSimple deriving (Eq, Ord, Show, Typeable)
data UpdateSigAttachments = UpdateSigAttachments deriving (Eq, Ord, Show, Typeable)
