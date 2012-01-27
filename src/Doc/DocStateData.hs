{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Doc.DocStateData
    ( CSVUpload(..)
    , CancelationReason(..)
    , DocStats(..)
    , Document(..)
    , DocumentFunctionality(..)
    , DocumentHistoryEntry(..)
    , DocumentID(..)
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
    , SignatoryDetails(..)
    , SignatoryLink(..)
    , SignatoryLinkID(..)
    , SignatoryRole(..)
    , SignatureInfo(..)
    , TimeoutTime(..)
    , AuthorAttachment(..)
    , SignatoryAttachment(..)
    , getFieldOfType
    , getValueOfType
    , documentHistoryToDocumentLog
    , emptyDocumentUI
    , doctypeFromString
    ) where

import API.Service.Model
import Company.Model
import Data.Data (Data)
import Data.Either
import Data.Int
import Data.Maybe
import DB.Derive
import DB.Types
import Happstack.Data
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import MinutesTime
import Misc
import Numeric
import User.Model
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import File.FileID
import File.File
import Doc.JpegPages
import Database.HDBC
import Data.List
import ELegitimation.SignatureProvider
import Doc.SignatoryLinkID

newtype DocumentID = DocumentID { unDocumentID :: Int64 }
    deriving (Eq, Ord, Typeable, Data) -- Data needed by PayEx modules

instance Show DocumentID where
    showsPrec prec (DocumentID val) =
         showsPrec prec val

instance Read DocumentID where
    readsPrec _prec = let makeDocumentID (i,v) = (DocumentID i,v)
                      in map makeDocumentID . readSigned readDec

instance FromReqURI DocumentID where
    fromReqURI = readM

$(deriveSerialize ''DocumentID)
instance Version DocumentID

newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }
    deriving (Eq, Ord, Typeable)

instance Show TimeoutTime where
    showsPrec prec = showsPrec prec . unTimeoutTime

instance Convertible TimeoutTime SqlValue where
    safeConvert tm = safeConvert (unTimeoutTime tm)

instance Convertible SqlValue TimeoutTime where
    safeConvert sv = TimeoutTime `fmap` safeConvert sv

newtype SignOrder = SignOrder { unSignOrder :: Integer }
    deriving (Eq, Ord, Typeable)

instance Show SignOrder where
    show (SignOrder n) = show n

data IdentificationType = EmailIdentification
                        | ELegitimationIdentification
    deriving (Eq, Ord, Bounded, Enum, Typeable)

data SignatureInfo = SignatureInfo { signatureinfotext        :: String
                                   , signatureinfosignature   :: String
                                   , signatureinfocertificate :: String
                                   , signatureinfoprovider    :: SignatureProvider
                                   , signaturefstnameverified :: Bool
                                   , signaturelstnameverified :: Bool
                                   , signaturepersnumverified :: Bool
                                   }
    deriving (Eq, Ord, Typeable)

data FieldType = FirstNameFT
               | LastNameFT
               | CompanyFT
               | PersonalNumberFT
               | CompanyNumberFT
               | EmailFT
               | CustomFT BS.ByteString Bool -- label filledbyauthor
    deriving (Eq, Ord, Data, Typeable)

data SignatoryField = SignatoryField
  { sfType       :: FieldType
  , sfValue      :: BS.ByteString
  , sfPlacements :: [FieldPlacement]
  } deriving (Eq, Ord, Data, Typeable)

-- defines where a field is placed
data FieldPlacement = FieldPlacement
    { placementx :: Int
    , placementy :: Int
    , placementpage :: Int
    , placementpagewidth :: Int
    , placementpageheight :: Int
    }
    deriving (Eq, Ord, Data, Typeable)
-- end of updates for template system

data SignatoryDetails = SignatoryDetails
    { signatorysignorder :: SignOrder
    -- for templates
    , signatoryfields    :: [SignatoryField]
    }
    deriving (Ord, Typeable)

instance Eq SignatoryDetails where
  SignatoryDetails {signatorysignorder=so1, signatoryfields=sf1} ==
    SignatoryDetails {signatorysignorder=so2, signatoryfields=sf2} =
      so1 == so2 && (sort sf2) == (sort sf1)

data SignatoryLink = SignatoryLink
    { signatorylinkid            :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
    , signatorydetails           :: SignatoryDetails    -- ^ details of this person as filled in invitation
    , signatorymagichash         :: MagicHash           -- ^ authentication code
    , maybesignatory             :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
    , maybesupervisor            :: Maybe UserID        -- ^ THIS IS NOW DEPRECATED - use maybecompany instead
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
    }
    deriving (Eq, Ord, Typeable)

data SignatoryRole = SignatoryPartner | SignatoryAuthor
    deriving (Eq, Ord, Bounded, Enum, Typeable, Show)

data CSVUpload = CSVUpload
    { csvtitle :: BS.ByteString
    , csvcontents  :: [[BS.ByteString]]
    , csvsignatoryindex :: Int
    }
    deriving (Eq, Ord, Typeable)


data SignInfo = SignInfo
    { signtime :: MinutesTime
    , signipnumber :: IPAddress
    }
    deriving (Eq, Ord, Typeable)

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
                    | AwaitingAuthor
                    | DocumentError String
    deriving (Eq, Ord, Typeable, Data)

data DocumentProcess = Contract | Offer | Order
    deriving (Eq, Ord, Typeable)

data DocumentType = Signable DocumentProcess | Template DocumentProcess | Attachment | AttachmentTemplate
    deriving (Eq, Ord, Typeable)

-- | Terrible, I know. Better idea?
doctypeFromString :: String -> DocumentType
doctypeFromString "Signable Contract"  = Signable Contract
doctypeFromString "Signable Offer"     = Signable Offer
doctypeFromString "Signable Order"     = Signable Order
doctypeFromString "Template Contract"  = Template Contract
doctypeFromString "Template Offer"     = Template Offer
doctypeFromString "Template Order"     = Template Order
doctypeFromString "Attachment"         = Attachment
doctypeFromString "AttachmentTemplate" = AttachmentTemplate
doctypeFromString _                    = error "Bad document type"

data DocumentFunctionality = BasicFunctionality | AdvancedFunctionality
    deriving (Eq, Ord, Typeable, Read)

data DocumentSharing = Private
                       | Shared -- means that the document is shared with subaccounts, and those with same parent accounts
    deriving (Eq, Ord, Typeable)

data DocumentTag = DocumentTag {
        tagname :: BS.ByteString
     ,  tagvalue :: BS.ByteString
     } deriving (Eq, Ord, Typeable, Data)

data DocumentUI = DocumentUI {
        documentmailfooter :: Maybe BS.ByteString
    } deriving (Eq, Ord, Typeable)

emptyDocumentUI :: DocumentUI
emptyDocumentUI = DocumentUI {
                    documentmailfooter = Nothing
                   }

data DocumentHistoryEntry
    = DocumentHistoryCreated
      { dochisttime :: MinutesTime
      }
    | DocumentHistoryInvitationSent
      { dochisttime :: MinutesTime
      , ipnumber :: IPAddress
      , dochistsignatories :: [SignatoryDetails]
      }    -- changed state from Preparatio to Pending
    | DocumentHistoryTimedOut
      { dochisttime :: MinutesTime
      }
    | DocumentHistorySigned
      { dochisttime :: MinutesTime
      , ipnumber :: IPAddress
      , dochistsignatorydetails :: SignatoryDetails
      }
    | DocumentHistoryRejected
      { dochisttime :: MinutesTime
      , ipnumber :: IPAddress
      , dochistsignatorydetails :: SignatoryDetails
      }
    | DocumentHistoryClosed
      { dochisttime :: MinutesTime
      , ipnumber :: IPAddress
      }
    | DocumentHistoryCanceled
      { dochisttime :: MinutesTime
      , ipnumber :: IPAddress
      -- , dochistsignatorydetails :: SignatoryDetails
      }
    | DocumentHistoryRestarted
      { dochisttime :: MinutesTime
      , ipnumber :: IPAddress
      }
    deriving (Eq, Ord, Typeable)

data DocumentLogEntry = DocumentLogEntry MinutesTime BS.ByteString
    deriving (Typeable, Data, Eq, Ord)

instance Show DocumentLogEntry where
    showsPrec _ (DocumentLogEntry time rest) = (++) (formatMinutesTimeUTC time ++ " " ++ BS.toString rest)

instance Read DocumentLogEntry where
    readsPrec _ text =
      -- 2011-01-02 13:45:22 = 19 chars
      case parseMinutesTimeUTC timepart of
        Just time -> [(DocumentLogEntry time (BS.fromString (drop 1 restpart)),"")]
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

getValueOfType :: FieldType -> SignatoryDetails -> BS.ByteString
getValueOfType t = fromMaybe BS.empty . fmap sfValue . getFieldOfType t . signatoryfields


documentHistoryToDocumentLog :: DocumentHistoryEntry -> DocumentLogEntry
documentHistoryToDocumentLog DocumentHistoryCreated
      { dochisttime
      } = DocumentLogEntry dochisttime $ BS.fromString "Document created"
documentHistoryToDocumentLog DocumentHistoryInvitationSent
      { dochisttime
      , ipnumber
      } = DocumentLogEntry dochisttime $ BS.fromString $ "Invitations sent to signatories" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryTimedOut
      { dochisttime
      } = DocumentLogEntry dochisttime $ BS.fromString "Document timed out"
documentHistoryToDocumentLog DocumentHistorySigned
      { dochisttime
      , ipnumber
      } = DocumentLogEntry dochisttime $ BS.fromString $ "Document signed by a signatory" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryRejected
      { dochisttime
      , ipnumber
      } = DocumentLogEntry dochisttime $ BS.fromString $ "Document rejected by a signatory" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryClosed
      { dochisttime
      , ipnumber
      } = DocumentLogEntry dochisttime $ BS.fromString $ "Document closed" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryCanceled
      { dochisttime
      , ipnumber
      } = DocumentLogEntry dochisttime $ BS.fromString $ "Document canceled" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryRestarted
      { dochisttime
      , ipnumber
      } = DocumentLogEntry dochisttime $ BS.fromString $ "Document restarted" ++ formatIP ipnumber

data DocStats = DocStats
                { doccount          :: !Int
                , signaturecount    :: !Int
                , signaturecount1m  :: !Int
                , signaturecount2m  :: !Int
                , signaturecount3m  :: !Int
                , signaturecount6m  :: !Int
                , signaturecount12m :: !Int
                }
    deriving (Eq, Ord, Typeable, Data) -- Data instance used for View modules (quite incorrectly there, please remove ASAP)

data Document = Document
    { documentid                     :: DocumentID
    , documenttitle                  :: BS.ByteString
    , documentsignatorylinks         :: [SignatoryLink]
    , documentfiles                  :: [FileID]
    , documentsealedfiles            :: [FileID]
    , documentstatus                 :: DocumentStatus
    , documenttype                   :: DocumentType
    , documentfunctionality          :: DocumentFunctionality
    , documentctime                  :: MinutesTime
    , documentmtime                  :: MinutesTime
    , documentdaystosign             :: Maybe Int
    , documenttimeouttime            :: Maybe TimeoutTime
    , documentinvitetime             :: Maybe SignInfo
    , documentlog                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext             :: BS.ByteString
    , documentallowedidtypes         :: [IdentificationType]
    , documentcancelationreason      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing                :: DocumentSharing
    , documentrejectioninfo          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    , documenttags                   :: [DocumentTag]
    , documentservice                :: Maybe ServiceID
    , documentdeleted                :: Bool -- set to true when doc is deleted - the other fields will be cleared too, so it is really truely deleting, it's just we want to avoid re-using the docid.
    , documentauthorattachments      :: [AuthorAttachment]
    , documentsignatoryattachments   :: [SignatoryAttachment]
    , documentui                     :: DocumentUI
    , documentregion                 :: Region
    }

instance HasLocale Document where
  getLocale = mkLocaleFromRegion . documentregion

data CancelationReason =  ManualCancel
                        -- The data returned by ELeg server
                        --                 msg                    fn            ln            num
                        | ELegDataMismatch String SignatoryLinkID BS.ByteString BS.ByteString BS.ByteString
    deriving (Eq, Ord, Typeable, Data)


data AuthorAttachment = AuthorAttachment { authorattachmentfile :: FileID }
                      deriving (Eq, Ord, Typeable)

data SignatoryAttachment = SignatoryAttachment { signatoryattachmentfile            :: Maybe FileID
                                               , signatoryattachmentemail           :: BS.ByteString
                                               , signatoryattachmentname            :: BS.ByteString
                                               , signatoryattachmentdescription     :: BS.ByteString
                                               }
                         deriving (Eq, Ord, Typeable)

deriving instance Eq Document
deriving instance Ord Document

deriving instance Show Document
deriving instance Show DocumentStatus
deriving instance Show DocumentType
deriving instance Show DocumentProcess
deriving instance Show DocumentFunctionality
deriving instance Show CSVUpload
deriving instance Show DocumentSharing
deriving instance Show DocumentTag
deriving instance Show DocumentUI

deriving instance Show DocStats

deriving instance Show FieldPlacement
deriving instance Show SignatoryField
deriving instance Show FieldType

deriving instance Show AuthorAttachment
deriving instance Show SignatoryAttachment

deriving instance Show SignatoryLink
deriving instance Show SignInfo
deriving instance Show SignatoryDetails
deriving instance Show DocumentHistoryEntry
deriving instance Show IdentificationType
deriving instance Show CancelationReason
deriving instance Show SignatureInfo

data MailsDeliveryStatus = Delivered | Undelivered | Unknown | Deferred
                           deriving (Eq, Ord, Typeable, Show, Read, Data)

-- stuff for converting to pgsql

$(bitfieldDeriveConvertible ''SignatoryRole)
$(enumDeriveConvertible ''MailsDeliveryStatus)
$(newtypeDeriveConvertible ''SignOrder)
$(jsonableDeriveConvertible [t| [SignatoryField] |])
$(enumDeriveConvertible ''DocumentFunctionality)
$(enumDeriveConvertible ''DocumentProcess)
-- $(jsonableDeriveConvertible [t| DocumentStatus |])
$(enumDeriveConvertibleIgnoreFields ''DocumentStatus)
$(bitfieldDeriveConvertible ''IdentificationType)
$(newtypeDeriveConvertible ''DocumentID)
$(enumDeriveConvertible ''DocumentSharing)
$(jsonableDeriveConvertible [t| [DocumentTag] |])
$(jsonableDeriveConvertible [t| CancelationReason |])
$(jsonableDeriveConvertible [t| [[BS.ByteString ]] |])