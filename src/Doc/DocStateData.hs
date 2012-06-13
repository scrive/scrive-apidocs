{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocStateData (
    module Doc.DocumentID
  , module Doc.SignatoryLinkID
  , CSVUpload(..)
  , CancelationReason(..)
  , DocStats(..)
  , Document(..)
  , DocumentHistoryEntry(..)
  , DocumentLogEntry(..)
  , DocumentSharing(..)
  , DocumentStatus(..)
  , DocumentTag(..)
  , DocumentUI(..)
  , DocumentType(..)
  , DocumentProcess(..)
  , FieldPlacement(..)
  , File(..)
  , FileStorage(..)
  , IdentificationType(..)
  , JpegPages(..)
  , MailsDeliveryStatus(..)
  , SignatureProvider(..)
  , SignInfo(..)
  , SignOrder(..)
  , SignatoryField(..)
  , FieldType(..)
  , TipSide(..)
  , SignatoryDetails(..)
  , SignatoryLink(..)
  , SignatoryRole(..)
  , SignatureInfo(..)
  , TimeoutTime(..)
  , AuthorAttachment(..)
  , SignatoryAttachment(..)
  , StatusClass(..)
  , getFieldOfType
  , getValueOfType
  , documentHistoryToDocumentLog
  , emptyDocumentUI
  , documentType
  , toDocumentProcess
  , doctypeFromString
  ) where

import API.Service.Model
import Company.Model
import Data.Data (Data)
import Data.Either
import Data.Maybe
import DB.Derive
import Happstack.Data
import IPAddress
import MagicHash (MagicHash)
import MinutesTime
import User.UserID
import User.Region
import User.Locale
--import qualified Data.ByteString as BS
--import qualified Data.ByteString.UTF8 as BS
import File.FileID
import File.File
import Doc.DocumentID
import Doc.JpegPages
import Doc.SignatoryLinkID
import Database.HDBC
import Data.List
import ELegitimation.SignatureProvider

newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''TimeoutTime)

instance Show TimeoutTime where
  show (TimeoutTime t) = show t

newtype SignOrder = SignOrder { unSignOrder :: Integer }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''SignOrder)


{- |
    We want the documents to be ordered like the icons in the bottom
    of the document list.  So this means:
    0 Draft - 1 Cancel - 2 Fall due - 3 Sent - 4 Opened - 5 Signed
-}

data StatusClass = SCDraft
                  | SCCancelled
                  | SCSent
                  | SCDelivered
                  | SCRead
                  | SCOpened
                  | SCSigned
                  deriving (Eq, Ord, Enum)

instance Show StatusClass where
  show SCDraft = "draft"
  show SCCancelled = "cancelled"
  show SCSent = "sent"
  show SCDelivered = "delivered"
  show SCRead = "read"
  show SCOpened = "opened"
  show SCSigned = "signed"

data IdentificationType = EmailIdentification
                        | ELegitimationIdentification
                        | PadIdentification
  deriving (Eq, Ord, Bounded, Enum, Show)

data SignatureInfo = SignatureInfo {
    signatureinfotext        :: String
  , signatureinfosignature   :: String
  , signatureinfocertificate :: String
  , signatureinfoprovider    :: SignatureProvider
  , signaturefstnameverified :: Bool
  , signaturelstnameverified :: Bool
  , signaturepersnumverified :: Bool
  , signatureinfoocspresponse :: Maybe String -- verified legal evidence issued by BankID
  } deriving (Eq, Ord, Show)

data FieldType = FirstNameFT
               | LastNameFT
               | CompanyFT
               | PersonalNumberFT
               | CompanyNumberFT
               | EmailFT
               | CustomFT String Bool -- label filledbyauthor
               | SignatureFT
               | CheckboxOptionalFT String 
               | CheckboxObligatoryFT String 
  deriving (Eq, Ord, Show, Data, Typeable)

data SignatoryField = SignatoryField {
    sfType       :: FieldType
  , sfValue      :: String
  , sfPlacements :: [FieldPlacement]
  } deriving (Eq, Ord, Show, Data, Typeable)

data FieldPlacement = FieldPlacement {
    placementx :: Int
  , placementy :: Int
  , placementpage :: Int
  , placementpagewidth :: Int
  , placementpageheight :: Int
  , placementtipside :: Maybe TipSide
  } deriving (Eq, Ord, Show, Data, Typeable)

data TipSide = LeftTip| RightTip deriving (Eq, Ord, Show, Data, Typeable)
    
data SignatoryDetails = SignatoryDetails {
    signatorysignorder :: SignOrder
  , signatoryfields    :: [SignatoryField]
  } deriving (Ord, Show)

instance Eq SignatoryDetails where
  SignatoryDetails {signatorysignorder=so1, signatoryfields=sf1} ==
    SignatoryDetails {signatorysignorder=so2, signatoryfields=sf2} =
      so1 == so2 && (sort sf2) == (sort sf1)

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
  , signatorylinkcsvupload     :: Maybe CSVUpload
  , signatoryattachments       :: [SignatoryAttachment]
  , signatorylinkstatusclass   :: StatusClass
  } deriving (Eq, Ord, Show)

data SignatoryRole = SignatoryPartner | SignatoryAuthor
    deriving (Eq, Ord, Bounded, Enum, Show)

data CSVUpload = CSVUpload {
    csvtitle :: String
  , csvcontents  :: [[String]]
  , csvsignatoryindex :: Int
  } deriving (Eq, Ord, Show)

data SignInfo = SignInfo {
    signtime :: MinutesTime
  , signipnumber :: IPAddress
  } deriving (Eq, Ord, Show)

{-
   Document start in Preparation state.

   Meaning:
   * Preparation: Only author can see it. He's still editing.
   * Pending: People can sign document. Could be timed out.
   * AwaitingAuthor: Everyone has signed but the author.
   * Closed: Everybody signed. This is final state.
   * Canceled: Author has canceled the document.
   * Timedout: This works as autocancel and has exactly same
     properties.

   Transitions:
   * Preparation to Pending: When invitations are sent.
   * Preparation to Cancel: mail about cancel to
     all who have signed it already is sent.
     TODO: Should other parties get an email?
   * Preparation to Timedout: mail about timeout to
     all who have signed it already is sent.
   * Pending to Closed: When everyone has signed.
     Info about closed deal is sent to everybody involved.
   * Pending to AwaitingAuthor: When all signatories have signed and there were fields.
     Info is sent to author.
   * AwaitingAuthor to Closed: Author signs it.
   * Pending to Cancel: Send no emails.
   * Pending to Timeout: TODO: No action?

   Allowed actions:
   * Preparation: change document, change title, add/rem signatories
   * Pending: change email of a signatory, signatory can sign
   * AwaitingAuthor: autho can sign.
   * Closed: nothing
   * Canceled: edit back to Preparation
   * Timedout: edit back to Preparation

   Archived bit:
   * This bit just moves document out of main view.
 -}

data DocumentStatus = Preparation
                    | Pending
                    | Closed
                    | Canceled
                    | Timedout
                    | Rejected
                    | DocumentError String
  deriving (Eq, Ord, Show)

data DocumentProcess = Contract | Offer | Order
  deriving (Eq, Ord, Show)

data DocumentType = Signable DocumentProcess
                  | Template DocumentProcess
                  | Attachment
  deriving (Eq, Ord, Show)

instance Convertible DocumentType SqlValue where
  safeConvert v = Right . SqlInteger $ case v of
    Signable _ -> 1
    Template _ -> 2
    Attachment -> 3

documentType :: (Int, Maybe DocumentProcess) -> DocumentType
documentType (1, Just p) = Signable p
documentType (2, Just p) = Template p
documentType (3, Nothing) = Attachment
documentType v = error $ "documentType: wrong values: " ++ show v

toDocumentProcess :: DocumentType -> Maybe DocumentProcess
toDocumentProcess (Signable p) = Just p
toDocumentProcess (Template p) = Just p
toDocumentProcess (Attachment) = Nothing

-- | Terrible, I know. Better idea?
-- | TODO: to be KILLED.
doctypeFromString :: String -> DocumentType
doctypeFromString "Signable Contract"  = Signable Contract
doctypeFromString "Signable Offer"     = Signable Offer
doctypeFromString "Signable Order"     = Signable Order
doctypeFromString "Template Contract"  = Template Contract
doctypeFromString "Template Offer"     = Template Offer
doctypeFromString "Template Order"     = Template Order
doctypeFromString "Attachment"         = Attachment
doctypeFromString _                    = error "Bad document type"

data DocumentSharing = Private
                     | Shared -- means that the document is shared with subaccounts, and those with same parent accounts
  deriving (Eq, Ord, Show)

data DocumentTag = DocumentTag {
    tagname :: String
  , tagvalue :: String
  } deriving (Eq, Ord, Show, Data, Typeable)

data DocumentUI = DocumentUI {
    documentmailfooter :: Maybe String
  } deriving (Eq, Ord, Show)

emptyDocumentUI :: DocumentUI
emptyDocumentUI = DocumentUI { documentmailfooter = Nothing }

data DocumentHistoryEntry =
  DocumentHistoryCreated {
    dochisttime :: MinutesTime
  }
  | DocumentHistoryInvitationSent {
      dochisttime :: MinutesTime
    , ipnumber :: IPAddress
    , dochistsignatories :: [SignatoryDetails]
  }    -- changed state from Preparatio to Pending
  | DocumentHistoryTimedOut {
    dochisttime :: MinutesTime
  }
  | DocumentHistorySigned {
      dochisttime :: MinutesTime
    , ipnumber :: IPAddress
    , dochistsignatorydetails :: SignatoryDetails
  }
  | DocumentHistoryRejected {
      dochisttime :: MinutesTime
    , ipnumber :: IPAddress
    , dochistsignatorydetails :: SignatoryDetails
  }
  | DocumentHistoryClosed {
      dochisttime :: MinutesTime
    , ipnumber :: IPAddress
  }
  | DocumentHistoryCanceled {
      dochisttime :: MinutesTime
    , ipnumber :: IPAddress
  }
  | DocumentHistoryRestarted {
      dochisttime :: MinutesTime
    , ipnumber :: IPAddress
  } deriving (Eq, Ord, Show)

data DocumentLogEntry = DocumentLogEntry MinutesTime String
  deriving (Eq, Ord)

instance Show DocumentLogEntry where
  show (DocumentLogEntry time rest) =
    formatMinutesTimeUTC time ++ " " ++ rest

instance Read DocumentLogEntry where
  readsPrec _ text =
    -- 2011-01-02 13:45:22 = 19 chars
    case parseMinutesTimeUTC timepart of
      Just time -> [(DocumentLogEntry time $ drop 1 restpart, "")]
      Nothing -> []
    where
     (timepart, restpart) = splitAt 19 text

instance Convertible [DocumentLogEntry] SqlValue where
  safeConvert = return . toSql . unlines . map show

instance Convertible SqlValue [DocumentLogEntry] where
  safeConvert = check . partitionEithers . map parse . lines . fromSql
    where
      check ((x:_), _) = Left x
      check ([], x) = Right x
      parse s = maybe (Left ConvertError {
          convSourceValue = s
        , convSourceType = "String"
        , convDestType = "DocumentLogEntry"
        , convErrorMessage = "Convertion error: reads returned " ++ show parsedS
        }) Right $ do
          [(v, "")] <- return parsedS
          return v
        where
          parsedS = reads s

getFieldOfType :: FieldType -> [SignatoryField] -> Maybe SignatoryField
getFieldOfType _ [] = Nothing
getFieldOfType t (sf:rest) =
  if sfType sf == t then Just sf else getFieldOfType t rest

getValueOfType :: FieldType -> SignatoryDetails -> String
getValueOfType t = fromMaybe "" . fmap sfValue . getFieldOfType t . signatoryfields

documentHistoryToDocumentLog :: DocumentHistoryEntry -> DocumentLogEntry
documentHistoryToDocumentLog DocumentHistoryCreated{..} =
  DocumentLogEntry dochisttime $ "Document created"
documentHistoryToDocumentLog DocumentHistoryInvitationSent{..} =
  DocumentLogEntry dochisttime $ "Invitations sent to signatories" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryTimedOut{..} =
  DocumentLogEntry dochisttime $ "Document timed out"
documentHistoryToDocumentLog DocumentHistorySigned{..} =
  DocumentLogEntry dochisttime $ "Document signed by a signatory" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryRejected{..} =
  DocumentLogEntry dochisttime $ "Document rejected by a signatory" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryClosed{..} =
  DocumentLogEntry dochisttime $ "Document closed" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryCanceled{..} =
  DocumentLogEntry dochisttime $ "Document canceled" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryRestarted{..} =
  DocumentLogEntry dochisttime $ "Document restarted" ++ formatIP ipnumber

data DocStats = DocStats {
    doccount          :: !Int
  , signaturecount    :: !Int
  , signaturecount1m  :: !Int
  , signaturecount2m  :: !Int
  , signaturecount3m  :: !Int
  , signaturecount6m  :: !Int
  , signaturecount12m :: !Int
  } deriving (Eq, Ord, Show, Data, Typeable) -- Data instance used for View modules (quite incorrectly there, please remove ASAP)

data Document = Document {
    documentid                     :: DocumentID
  , documenttitle                  :: String
  , documentsignatorylinks         :: [SignatoryLink]
  , documentfiles                  :: [FileID]
  , documentsealedfiles            :: [FileID]
  , documentstatus                 :: DocumentStatus
  , documenttype                   :: DocumentType
  , documentctime                  :: MinutesTime
  , documentmtime                  :: MinutesTime
  , documentdaystosign             :: Maybe Int
  , documenttimeouttime            :: Maybe TimeoutTime
  , documentinvitetime             :: Maybe SignInfo
  , documentlog                    :: [DocumentLogEntry]      -- to be made into plain text
  , documentinvitetext             :: String
  , documentallowedidtypes         :: [IdentificationType]
  , documentcancelationreason      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
  , documentsharing                :: DocumentSharing
  , documentrejectioninfo          :: Maybe (MinutesTime, SignatoryLinkID, String)
  , documenttags                   :: [DocumentTag]
  , documentservice                :: Maybe ServiceID
  , documentdeleted                :: Bool -- set to true when doc is deleted - the other fields will be cleared too, so it is really truely deleting, it's just we want to avoid re-using the docid.
  , documentauthorattachments      :: [AuthorAttachment]
  , documentui                     :: DocumentUI
  , documentregion                 :: Region
  , documentstatusclass            :: StatusClass
  } deriving (Eq, Ord, Show)

instance HasLocale Document where
  getLocale = mkLocaleFromRegion . documentregion

data CancelationReason = ManualCancel
                        -- The data returned by ELeg server
                        --                 msg                    fn            ln            num
                        | ELegDataMismatch String SignatoryLinkID String String String
  deriving (Eq, Ord, Show, Data, Typeable)


newtype AuthorAttachment = AuthorAttachment { authorattachmentfile :: FileID }
  deriving (Eq, Ord, Show)

data SignatoryAttachment = SignatoryAttachment {
    signatoryattachmentfile            :: Maybe FileID
  , signatoryattachmentname            :: String
  , signatoryattachmentdescription     :: String
  } deriving (Eq, Ord, Show)

data MailsDeliveryStatus = Delivered
                         | Undelivered
                         | Unknown
                         | Deferred
                           deriving (Eq, Ord, Show)

-- stuff for converting to pgsql

$(bitfieldDeriveConvertible ''SignatoryRole)
$(enumDeriveConvertible ''MailsDeliveryStatus)
$(newtypeDeriveConvertible ''SignOrder)
$(enumDeriveConvertible ''DocumentProcess)
$(enumDeriveConvertibleIgnoreFields ''DocumentStatus)
$(enumDeriveConvertibleIgnoreFields ''FieldType)
$(bitfieldDeriveConvertible ''IdentificationType)
$(newtypeDeriveConvertible ''DocumentID)
$(enumDeriveConvertible ''DocumentSharing)
$(jsonableDeriveConvertible [t| [DocumentTag] |])
$(jsonableDeriveConvertible [t| CancelationReason |])
$(jsonableDeriveConvertible [t| [[String]] |])
$(jsonableDeriveConvertible [t| [FieldPlacement] |])
