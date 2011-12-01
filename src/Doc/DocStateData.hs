{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Doc.DocStateData
    ( Author(..)
    , CSVUpload(..)
    , CancelationReason(..)
    , ChargeMode(..)
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
    , Documents
    , FieldDefinition(..)
    , FieldPlacement(..)
    , File(..)
    , FileStorage(..)
    , IdentificationType(..)
    , JpegPages(..)
    , SignatureProvider(..)
    , SignInfo(..)
    , SignOrder(..)
    , Signatory(..)
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
    , Supervisor(..)
    , getFieldOfType
    , getValueOfType
    , documentHistoryToDocumentLog
    , emptyDocumentUI
    , doctypeFromString
    ) where

import API.Service.Model
import Company.Model
import Data.Data (Data)
import Data.Int
import Data.Maybe
import Data.Word
import DB.Derive
import DB.Types
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.State
import Happstack.Util.Common
import Mails.MailsUtil
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
newtype Author = Author { unAuthor :: UserID }
    deriving (Eq, Ord, Typeable)

newtype DocumentID = DocumentID { unDocumentID :: Int64 }
    deriving (Eq, Ord, Typeable, Data) -- Data needed by PayEx modules
newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int64 }
    deriving (Eq, Ord, Typeable, Data)
newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }
    deriving (Eq, Ord, Typeable)

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

data SignatureInfo0 = SignatureInfo0 { signatureinfotext0        :: String
                                     , signatureinfosignature0   :: String
                                     , signatureinfocertificate0 :: String
                                     , signatureinfoprovider0    :: SignatureProvider
                                     }
    deriving (Eq, Ord, Typeable)

data SignatureInfo = SignatureInfo { signatureinfotext        :: String
                                   , signatureinfosignature   :: String
                                   , signatureinfocertificate :: String
                                   , signatureinfoprovider    :: SignatureProvider
                                   , signaturefstnameverified :: Bool
                                   , signaturelstnameverified :: Bool
                                   , signaturepersnumverified :: Bool
                                   }
    deriving (Eq, Ord, Typeable)

-- added by Eric Normand for template system
-- Defines a new field to be placed in a contract
data FieldDefinition0 = FieldDefinition0
    { fieldlabel0 :: BS.ByteString
    , fieldvalue0 :: BS.ByteString
    , fieldplacements0 :: [FieldPlacement]
    }
    deriving (Eq, Ord, Typeable)

data FieldDefinition = FieldDefinition
    { fieldlabel :: BS.ByteString
    , fieldvalue :: BS.ByteString
    , fieldplacements :: [FieldPlacement]
    , fieldfilledbyauthor :: Bool
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

data SignatoryDetails0 = SignatoryDetails0
    { signatoryname00      :: BS.ByteString  -- "Gracjan Polak"
    , signatorycompany00   :: BS.ByteString  -- SkrivaPå
    , signatorynumber00    :: BS.ByteString  -- 123456789
    , signatoryemail00     :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
    }
    deriving (Eq, Ord, Typeable)

data SignatoryDetails1 = SignatoryDetails1
    { signatoryname1      :: BS.ByteString
    , signatorycompany1   :: BS.ByteString
    , signatorynumber1    :: BS.ByteString
    , signatoryemail1     :: BS.ByteString
    , signatorynameplacements1 :: [FieldPlacement]
    , signatorycompanyplacements1 :: [FieldPlacement]
    , signatoryemailplacements1 :: [FieldPlacement]
    , signatorynumberplacements1 :: [FieldPlacement]
    , signatoryotherfields1 :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)

data SignatoryDetails2 = SignatoryDetails2
    { signatoryfstname2   :: BS.ByteString
    , signatorysndname2   :: BS.ByteString
    , signatorycompany2   :: BS.ByteString
    , signatorynumber2    :: BS.ByteString
    , signatoryemail2     :: BS.ByteString
    , signatorynameplacements2 :: [FieldPlacement]
    , signatorycompanyplacements2 :: [FieldPlacement]
    , signatoryemailplacements2 :: [FieldPlacement]
    , signatorynumberplacements2 :: [FieldPlacement]
    , signatoryotherfields2 :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)


data SignatoryDetails3 = SignatoryDetails3
    { signatoryfstname3   :: BS.ByteString  -- "Gracjan Polak"
    , signatorysndname3   :: BS.ByteString  -- "Gracjan Polak"
    , signatorycompany3   :: BS.ByteString  -- SkrivaPå
    , signatorynumber3    :: BS.ByteString  -- 123456789
    , signatoryemail3     :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
    -- for templates
    , signatoryfstnameplacements3 :: [FieldPlacement]
    , signatorysndnameplacements3 :: [FieldPlacement]
    , signatorycompanyplacements3 :: [FieldPlacement]
    , signatoryemailplacements3 :: [FieldPlacement]
    , signatorynumberplacements3 :: [FieldPlacement]
    , signatoryotherfields3 :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)

data SignatoryDetails4 = SignatoryDetails4
    { signatoryfstname4        :: BS.ByteString  -- "Gracjan"
    , signatorysndname4        :: BS.ByteString  -- "Polak"
    , signatorycompany4        :: BS.ByteString  -- SkrivaPå
    , signatorypersonalnumber4 :: BS.ByteString  -- 123456789
    , signatorycompanynumber4  :: BS.ByteString  -- 123456789
    , signatoryemail4          :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
    -- for templates
    , signatoryfstnameplacements4        :: [FieldPlacement]
    , signatorysndnameplacements4        :: [FieldPlacement]
    , signatorycompanyplacements4        :: [FieldPlacement]
    , signatoryemailplacements4          :: [FieldPlacement]
    , signatorypersonalnumberplacements4 :: [FieldPlacement]
    , signatorycompanynumberplacements4  :: [FieldPlacement]
    , signatoryotherfields4              :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)

data SignatoryDetails5 = SignatoryDetails5
    { signatoryfstname5        :: BS.ByteString  -- "Gracjan"
    , signatorysndname5        :: BS.ByteString  -- "Polak"
    , signatorycompany5        :: BS.ByteString  -- SkrivaPå
    , signatorypersonalnumber5 :: BS.ByteString  -- 123456789
    , signatorycompanynumber5  :: BS.ByteString  -- 123456789
    , signatoryemail5          :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
    -- for ordered signing
    , signatorysignorder5      :: SignOrder
    -- for templates
    , signatoryfstnameplacements5        :: [FieldPlacement]
    , signatorysndnameplacements5        :: [FieldPlacement]
    , signatorycompanyplacements5        :: [FieldPlacement]
    , signatoryemailplacements5          :: [FieldPlacement]
    , signatorypersonalnumberplacements5 :: [FieldPlacement]
    , signatorycompanynumberplacements5  :: [FieldPlacement]
    , signatoryotherfields5              :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)

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

data SignatoryLink1 = SignatoryLink1
    { signatorylinkid1    :: SignatoryLinkID
    , signatorydetails1   :: SignatoryDetails
    , maybesignatory1     :: Maybe Signatory
    , maybesigninfo1      :: Maybe SignInfo
    , maybeseentime1      :: Maybe MinutesTime
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink2 = SignatoryLink2
    { signatorylinkid2    :: SignatoryLinkID
    , signatorydetails2   :: SignatoryDetails
    , signatorymagichash2 :: MagicHash
    , maybesignatory2     :: Maybe Signatory
    , maybesigninfo2      :: Maybe SignInfo
    , maybeseentime2      :: Maybe MinutesTime
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink3 = SignatoryLink3
    { signatorylinkid3    :: SignatoryLinkID
    , signatorydetails3   :: SignatoryDetails
    , signatorymagichash3 :: MagicHash
    , maybesignatory3     :: Maybe Signatory
    , maybesigninfo3      :: Maybe SignInfo
    , maybeseeninfo3      :: Maybe SignInfo
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink4 = SignatoryLink4
    { signatorylinkid4    :: SignatoryLinkID
    , signatorydetails4   :: SignatoryDetails
    , signatorymagichash4 :: MagicHash
    , maybesignatory4     :: Maybe Signatory
    , maybesigninfo4      :: Maybe SignInfo
    , maybeseeninfo4      :: Maybe SignInfo
    , invitationdeliverystatus4 :: MailsDeliveryStatus
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink5 = SignatoryLink5
    { signatorylinkid5          :: SignatoryLinkID
    , signatorydetails5         :: SignatoryDetails
    , signatorymagichash5       :: MagicHash
    , maybesignatory5           :: Maybe Signatory
    , maybesigninfo5            :: Maybe SignInfo
    , maybeseeninfo5            :: Maybe SignInfo
    , invitationdeliverystatus5 :: MailsDeliveryStatus
    , signatorysignatureinfo5   :: Maybe SignatureInfo
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink6 = SignatoryLink6
    { signatorylinkid6          :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
    , signatorydetails6         :: SignatoryDetails    -- ^ details of this person as filled in invitation
    , signatorymagichash6       :: MagicHash           -- ^ authentication code
    , maybesignatory6           :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
    , maybesigninfo6            :: Maybe SignInfo      -- ^ when a person has signed this document
    , maybeseeninfo6            :: Maybe SignInfo      -- ^ when a person has first seen this document
    , invitationdeliverystatus6 :: MailsDeliveryStatus -- ^ status of email delivery
    , signatorysignatureinfo6   :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink7 = SignatoryLink7
    { signatorylinkid7          :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
    , signatorydetails7         :: SignatoryDetails    -- ^ details of this person as filled in invitation
    , signatorymagichash7       :: MagicHash           -- ^ authentication code
    , maybesignatory7           :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
    , maybesigninfo7            :: Maybe SignInfo      -- ^ when a person has signed this document
    , maybeseeninfo7            :: Maybe SignInfo      -- ^ when a person has first seen this document
    , invitationdeliverystatus7 :: MailsDeliveryStatus -- ^ status of email delivery
    , signatorysignatureinfo7   :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
    , signatoryroles7           :: [SignatoryRole]
    , signatorylinkdeleted7     :: Bool
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink8 = SignatoryLink8
    { signatorylinkid8          :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
    , signatorydetails8         :: SignatoryDetails    -- ^ details of this person as filled in invitation
    , signatorymagichash8       :: MagicHash           -- ^ authentication code
    , maybesignatory8           :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
    , maybesigninfo8            :: Maybe SignInfo      -- ^ when a person has signed this document
    , maybeseeninfo8            :: Maybe SignInfo      -- ^ when a person has first seen this document
    , maybereadinvite8          :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read
    , invitationdeliverystatus8 :: MailsDeliveryStatus -- ^ status of email delivery
    , signatorysignatureinfo8   :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
    , signatoryroles8           :: [SignatoryRole]
    , signatorylinkdeleted8     :: Bool
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink9 = SignatoryLink9
    { signatorylinkid9          :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
    , signatorydetails9         :: SignatoryDetails    -- ^ details of this person as filled in invitation
    , signatorymagichash9       :: MagicHash           -- ^ authentication code
    , maybesignatory9           :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
    , maybesupervisor9          :: Maybe UserID        -- ^ if this document has been saved to an account with a supervisor, this is the userid
    , maybesigninfo9            :: Maybe SignInfo      -- ^ when a person has signed this document
    , maybeseeninfo9            :: Maybe SignInfo      -- ^ when a person has first seen this document
    , maybereadinvite9          :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read
    , invitationdeliverystatus9 :: MailsDeliveryStatus -- ^ status of email delivery
    , signatorysignatureinfo9   :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
    , signatoryroles9           :: [SignatoryRole]
    , signatorylinkdeleted9     :: Bool
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink10 = SignatoryLink10
    { signatorylinkid10            :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
    , signatorydetails10           :: SignatoryDetails    -- ^ details of this person as filled in invitation
    , signatorymagichash10         :: MagicHash           -- ^ authentication code
    , maybesignatory10             :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
    , maybesupervisor10            :: Maybe UserID        -- ^ if this document has been saved to an account with a supervisor, this is the userid
    , maybesigninfo10              :: Maybe SignInfo      -- ^ when a person has signed this document
    , maybeseeninfo10              :: Maybe SignInfo      -- ^ when a person has first seen this document
    , maybereadinvite10            :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read
    , invitationdeliverystatus10   :: MailsDeliveryStatus -- ^ status of email delivery
    , signatorysignatureinfo10     :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
    , signatoryroles10             :: [SignatoryRole]
    , signatorylinkdeleted10       :: Bool -- ^ when true sends the doc to the recycle bin for that sig
    , signatorylinkreallydeleted10 :: Bool -- ^ when true it means that the doc has been removed from the recycle bin
    }
    deriving (Eq, Ord, Typeable)

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
    }
    deriving (Eq, Ord, Typeable)

data SignatoryRole = SignatoryPartner | SignatoryAuthor
    deriving (Eq, Ord, Bounded, Enum, Typeable, Show)

instance Version SignatoryRole



data SignInfo = SignInfo
    { signtime :: MinutesTime
    , signipnumber :: Word32
    }
    deriving (Eq, Ord, Typeable)

data SignInfo0 = SignInfo0
    { signtime0 :: MinutesTime
    }
    deriving (Eq, Ord, Typeable)

newtype Signatory = Signatory { unSignatory :: UserID }
    deriving (Eq, Ord, Typeable)

newtype Supervisor = Supervisor { unSupervisor :: UserID }
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

data DocumentType0 = Contract0 | ContractTemplate0 | Offer0 | OfferTemplate0 | Attachment0 | AttachmentTemplate0
    deriving (Eq, Ord, Typeable)

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

{- |
    This is no longer used because there's no quarantine anymore, it's been replaced
    with a flag called documentdeleted
-}
data DocumentRecordStatus = LiveDocument | QuarantinedDocument | DeletedDocument
    deriving (Eq, Ord, Typeable, Show)

data DocumentFunctionality = BasicFunctionality | AdvancedFunctionality
    deriving (Eq, Ord, Typeable, Read)

data ChargeMode = ChargeInitialFree   -- initial 5 documents are free
                | ChargeNormal        -- value times number of people involved

    deriving (Eq, Ord, Typeable)

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

data DocumentHistoryEntry0 = DocumentHistoryCreated0 { dochisttime0 :: MinutesTime }
                          | DocumentHistoryInvitationSent0 { dochisttime0 :: MinutesTime
                                                          , ipnumber0 :: Word32
                                                          }    -- changed state from Preparatio to Pending
    deriving (Eq, Ord, Typeable)

data DocumentHistoryEntry
    = DocumentHistoryCreated
      { dochisttime :: MinutesTime
      }
    | DocumentHistoryInvitationSent
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      , dochistsignatories :: [SignatoryDetails]
      }    -- changed state from Preparatio to Pending
    | DocumentHistoryTimedOut
      { dochisttime :: MinutesTime
      }
    | DocumentHistorySigned
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      , dochistsignatorydetails :: SignatoryDetails
      }
    | DocumentHistoryRejected
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      , dochistsignatorydetails :: SignatoryDetails
      }
    | DocumentHistoryClosed
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      }
    | DocumentHistoryCanceled
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      -- , dochistsignatorydetails :: SignatoryDetails
      }
    | DocumentHistoryRestarted
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      }
    deriving (Eq, Ord, Typeable)

data DocumentLogEntry = DocumentLogEntry MinutesTime BS.ByteString
    deriving (Typeable, Show, Data, Eq, Ord)

$(deriveSerialize ''DocumentLogEntry)
instance Version DocumentLogEntry

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
    , documentcsvupload              :: Maybe CSVUpload
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


{- | Watch out. This instance is a bit special. It has to be
   "Document" - as this is what database uses as table name.  Simple
   deriving clause will create a "MyApp.MyModule.Document"!  -}

instance Typeable Document where typeOf _ = mkTypeOf "Document"

data CSVUpload = CSVUpload
    { csvtitle :: BS.ByteString
    , csvcontents  :: [[BS.ByteString]]
    , csvsignatoryindex :: Int
    }
    deriving (Eq, Ord, Typeable)

data AuthorAttachment = AuthorAttachment { authorattachmentfile :: FileID }
                      deriving (Eq, Ord, Typeable)

instance Version AuthorAttachment where
    mode = extension 1 (Proxy :: Proxy ())

instance Migrate () AuthorAttachment where
    migrate _ = error "Cannot migrate AuthorAttachment anymore"



data SignatoryAttachment = SignatoryAttachment { signatoryattachmentfile            :: Maybe FileID
                                               , signatoryattachmentemail           :: BS.ByteString
                                               , signatoryattachmentname            :: BS.ByteString
                                               , signatoryattachmentdescription     :: BS.ByteString
                                               }
                         deriving (Eq, Ord, Typeable)

instance Version SignatoryAttachment where
    mode = extension 1 (Proxy :: Proxy ())

instance Migrate () SignatoryAttachment where
    migrate _ = error "Cannot migrate SignatoryAttachment anymore"

$(deriveSerialize ''SignatoryAttachment)
$(deriveSerialize ''AuthorAttachment)


#ifndef DOCUMENTS_IN_POSTGRES
instance Eq Document where
    a == b = documentid a == documentid b

instance Ord Document where
    compare a b | documentid a == documentid b = EQ
                | otherwise = compare (documentmtime b,documenttitle a,documentid a)
                                      (documentmtime a,documenttitle b,documentid b)
                              -- see above: we use reverse time here!
#else

deriving instance Eq Document
deriving instance Ord Document

#endif

instance Show SignatoryLinkID where
    showsPrec prec (SignatoryLinkID x) = showsPrec prec x


deriving instance Show Document
deriving instance Show DocumentStatus
deriving instance Show DocumentType
deriving instance Show DocumentProcess
deriving instance Show DocumentFunctionality
deriving instance Show CSVUpload
deriving instance Show ChargeMode
deriving instance Show DocumentSharing
deriving instance Show DocumentTag
deriving instance Show DocumentUI
deriving instance Show Author

deriving instance Show DocStats

deriving instance Show FieldDefinition
deriving instance Show FieldPlacement
deriving instance Show SignatoryField
deriving instance Show FieldType

deriving instance Show AuthorAttachment
deriving instance Show SignatoryAttachment

instance Show TimeoutTime where
    showsPrec prec = showsPrec prec . unTimeoutTime

deriving instance Show SignatoryLink
deriving instance Show SignatoryLink1
deriving instance Show SignatoryLink2
deriving instance Show SignInfo
deriving instance Show SignInfo0
deriving instance Show SignatoryDetails
deriving instance Show SignatoryDetails0
deriving instance Show DocumentHistoryEntry
deriving instance Show IdentificationType
deriving instance Show CancelationReason
deriving instance Show SignatureInfo0
deriving instance Show SignatureInfo

instance Show Signatory where
    showsPrec prec (Signatory userid) = showsPrec prec userid

instance Show Supervisor where
  showsPrec prec (Supervisor userid) = showsPrec prec userid

instance Show DocumentID where
    showsPrec prec (DocumentID val) =
         showsPrec prec val

instance Read DocumentID where
    readsPrec _prec = let makeDocumentID (i,v) = (DocumentID i,v)
                      in map makeDocumentID . readDec

instance Read SignatoryLinkID where
    readsPrec _prec = let make (i,v) = (SignatoryLinkID i,v)
                      in map make . readDec

instance FromReqURI DocumentID where
    fromReqURI = readM

instance FromReqURI SignatoryLinkID where
    fromReqURI = readM

$(deriveSerialize ''FieldDefinition0)
instance Version FieldDefinition0

$(deriveSerialize ''FieldDefinition)
instance Version FieldDefinition where
    mode = extension 1 (Proxy :: Proxy FieldDefinition0)

instance Migrate FieldDefinition0 FieldDefinition where
    migrate (FieldDefinition0
             { fieldlabel0
             , fieldvalue0
             , fieldplacements0
             }) = FieldDefinition
                { fieldlabel = fieldlabel0
                , fieldvalue = fieldvalue0
                , fieldplacements = fieldplacements0
                , fieldfilledbyauthor = False
                }

$(deriveSerialize ''FieldPlacement)
instance Version FieldPlacement

$(deriveSerialize ''SignInfo0)
instance Version SignInfo0

$(deriveSerialize ''SignInfo)
instance Version SignInfo where
    mode = extension 1 (Proxy :: Proxy SignInfo0)

$(deriveSerialize ''SignOrder)
instance Version SignOrder

$(deriveSerialize ''IdentificationType)
instance Version IdentificationType

$(deriveSerialize ''CancelationReason)
instance Version CancelationReason

$(deriveSerialize ''SignatureInfo0)
instance Version SignatureInfo0

$(deriveSerialize ''SignatureInfo)
instance Version SignatureInfo where
    mode = extension 1 (Proxy :: Proxy SignatureInfo0)

instance Migrate SignatureInfo0 SignatureInfo where
    migrate (SignatureInfo0
            { signatureinfotext0
            , signatureinfosignature0
            , signatureinfocertificate0
            , signatureinfoprovider0
            }) = SignatureInfo
            { signatureinfotext = signatureinfotext0
            , signatureinfosignature = signatureinfosignature0
            , signatureinfocertificate = signatureinfocertificate0
            , signatureinfoprovider = signatureinfoprovider0
            , signaturefstnameverified = False
            , signaturelstnameverified = False
            , signaturepersnumverified = False
            }

instance Migrate SignInfo0 SignInfo where
    migrate (SignInfo0
             { signtime0
             }) = SignInfo
                { signtime = signtime0
                , signipnumber = 0 -- mean unknown
                }

$(deriveSerialize ''SignatoryDetails0)
instance Version SignatoryDetails0

$(deriveSerialize ''SignatoryDetails1)
instance Version SignatoryDetails1 where
    mode = extension 1 (Proxy :: Proxy SignatoryDetails0)

$(deriveSerialize ''SignatoryDetails2)
instance Version SignatoryDetails2 where
    mode = extension 2 (Proxy :: Proxy SignatoryDetails1)

$(deriveSerialize ''SignatoryDetails3)
instance Version SignatoryDetails3 where
    mode = extension 3 (Proxy :: Proxy SignatoryDetails2)

$(deriveSerialize ''SignatoryDetails4)
instance Version SignatoryDetails4 where
    mode = extension 4 (Proxy :: Proxy SignatoryDetails3)

$(deriveSerialize ''SignatoryDetails5)
instance Version SignatoryDetails5 where
    mode = extension 5 (Proxy :: Proxy SignatoryDetails4)

$(deriveSerialize ''SignatoryDetails)
instance Version SignatoryDetails where
    mode = extension 6 (Proxy :: Proxy SignatoryDetails5)

$(deriveSerialize ''SignatoryField)
instance Version SignatoryField

$(deriveSerialize ''FieldType)
instance Version FieldType

$(deriveSerialize ''SignatoryLink1)
instance Version SignatoryLink1 where
    mode = extension 1 (Proxy :: Proxy ())

$(deriveSerialize ''SignatoryLink2)
instance Version SignatoryLink2 where
    mode = extension 2 (Proxy :: Proxy SignatoryLink1)

$(deriveSerialize ''SignatoryLink3)
instance Version SignatoryLink3 where
    mode = extension 3 (Proxy :: Proxy SignatoryLink2)

$(deriveSerialize ''SignatoryLink4)
instance Version SignatoryLink4 where
    mode = extension 4 (Proxy :: Proxy SignatoryLink3)

$(deriveSerialize ''SignatoryLink5)
instance Version SignatoryLink5 where
    mode = extension 5 (Proxy :: Proxy SignatoryLink4)

$(deriveSerialize ''SignatoryLink6)
instance Version SignatoryLink6 where
    mode = extension 6 (Proxy :: Proxy SignatoryLink5)

$(deriveSerialize ''SignatoryLink7)
instance Version SignatoryLink7 where
    mode = extension 7 (Proxy :: Proxy SignatoryLink6)

$(deriveSerialize ''SignatoryLink8)
instance Version SignatoryLink8 where
    mode = extension 8 (Proxy :: Proxy SignatoryLink7)

$(deriveSerialize ''SignatoryLink9)
instance Version SignatoryLink9 where
    mode = extension 9 (Proxy :: Proxy SignatoryLink8)

$(deriveSerialize ''SignatoryLink10)
instance Version SignatoryLink10 where
    mode = extension 10 (Proxy :: Proxy SignatoryLink9)

$(deriveSerialize ''SignatoryLink)
instance Version SignatoryLink where
    mode = extension 11 (Proxy :: Proxy SignatoryLink10)

instance Migrate SignatoryDetails0 SignatoryDetails1 where
    migrate (SignatoryDetails0
             { signatoryname00
             , signatorycompany00
             , signatorynumber00
             , signatoryemail00
             }) = SignatoryDetails1
                { signatoryname1 = signatoryname00
                , signatorycompany1 = signatorycompany00
                , signatorynumber1 = signatorynumber00
                , signatoryemail1 = signatoryemail00
                , signatorynameplacements1 = []
                , signatorycompanyplacements1 = []
                , signatoryemailplacements1 = []
                , signatorynumberplacements1 = []
                , signatoryotherfields1 = []
                }


instance Migrate SignatoryDetails1 SignatoryDetails2 where
    migrate (SignatoryDetails1
             {  signatoryname1
                , signatorycompany1
                , signatorynumber1
                , signatoryemail1
                , signatorynameplacements1
                , signatorycompanyplacements1
                , signatoryemailplacements1
                , signatorynumberplacements1
                , signatoryotherfields1
              }) = SignatoryDetails2
                { signatoryfstname2 =  signatoryname1
                , signatorysndname2 = BS.empty
                , signatorycompany2 = signatorycompany1
                , signatorynumber2 = signatorynumber1
                , signatoryemail2 = signatoryemail1
                , signatorynameplacements2 = signatorynameplacements1
                , signatorycompanyplacements2 = signatorycompanyplacements1
                , signatoryemailplacements2 = signatoryemailplacements1
                , signatorynumberplacements2 = signatorynumberplacements1
                , signatoryotherfields2 = signatoryotherfields1
                }


instance Migrate SignatoryDetails2 SignatoryDetails3 where
    migrate (SignatoryDetails2
             {  signatoryfstname2
                , signatorysndname2
                , signatorycompany2
                , signatorynumber2
                , signatoryemail2
                , signatorynameplacements2
                , signatorycompanyplacements2
                , signatoryemailplacements2
                , signatorynumberplacements2
                , signatoryotherfields2
                }) = SignatoryDetails3
                { signatoryfstname3 =  signatoryfstname2
                , signatorysndname3 = signatorysndname2
                , signatorycompany3 = signatorycompany2
                , signatorynumber3 = signatorynumber2
                , signatoryemail3 = signatoryemail2
                , signatoryfstnameplacements3 = signatorynameplacements2
                , signatorysndnameplacements3 = []
                , signatorycompanyplacements3 = signatorycompanyplacements2
                , signatoryemailplacements3 = signatoryemailplacements2
                , signatorynumberplacements3 = signatorynumberplacements2
                , signatoryotherfields3 = signatoryotherfields2
                }


instance Migrate SignatoryDetails3 SignatoryDetails4 where
    migrate (SignatoryDetails3
             {  signatoryfstname3
                , signatorysndname3
                , signatorycompany3
                , signatorynumber3
                , signatoryemail3
                , signatoryfstnameplacements3
                , signatorysndnameplacements3
                , signatorycompanyplacements3
                , signatoryemailplacements3
                , signatorynumberplacements3
                , signatoryotherfields3
                }) = SignatoryDetails4
                { signatoryfstname4 =  signatoryfstname3
                , signatorysndname4 = signatorysndname3
                , signatorycompany4 = signatorycompany3
                , signatorypersonalnumber4 = signatorynumber3
                , signatorycompanynumber4 = BS.empty
                , signatoryemail4 = signatoryemail3
                , signatoryfstnameplacements4 = signatoryfstnameplacements3
                , signatorysndnameplacements4 = signatorysndnameplacements3
                , signatorycompanyplacements4 = signatorycompanyplacements3
                , signatoryemailplacements4 = signatoryemailplacements3
                , signatorypersonalnumberplacements4 = signatorynumberplacements3
                , signatorycompanynumberplacements4 = []
                , signatoryotherfields4 = signatoryotherfields3
                }

instance Migrate SignatoryDetails4 SignatoryDetails5 where
    migrate (SignatoryDetails4
             {  signatoryfstname4
                , signatorysndname4
                , signatorycompany4
                , signatorypersonalnumber4
                , signatorycompanynumber4
                , signatoryemail4
                , signatoryfstnameplacements4
                , signatorysndnameplacements4
                , signatorycompanyplacements4
                , signatoryemailplacements4
                , signatorypersonalnumberplacements4
                , signatorycompanynumberplacements4
                , signatoryotherfields4
                }) = SignatoryDetails5
                { signatoryfstname5 = signatoryfstname4
                , signatorysndname5 = signatorysndname4
                , signatorycompany5 = signatorycompany4
                , signatorypersonalnumber5 = signatorypersonalnumber4
                , signatorycompanynumber5 = signatorycompanynumber4
                , signatoryemail5 = signatoryemail4
                , signatorysignorder5 = SignOrder 1
                , signatoryfstnameplacements5 = signatoryfstnameplacements4
                , signatorysndnameplacements5 = signatorysndnameplacements4
                , signatorycompanyplacements5 = signatorycompanyplacements4
                , signatoryemailplacements5 = signatoryemailplacements4
                , signatorypersonalnumberplacements5 = signatorypersonalnumberplacements4
                , signatorycompanynumberplacements5 = signatorycompanynumberplacements4
                , signatoryotherfields5 = signatoryotherfields4
                }

instance Migrate SignatoryDetails5 SignatoryDetails where
    migrate (SignatoryDetails5
             {  signatoryfstname5
                , signatorysndname5
                , signatorycompany5
                , signatorypersonalnumber5
                , signatorycompanynumber5
                , signatoryemail5
                , signatorysignorder5
                , signatoryfstnameplacements5
                , signatorysndnameplacements5
                , signatorycompanyplacements5
                , signatoryemailplacements5
                , signatorypersonalnumberplacements5
                , signatorycompanynumberplacements5
                , signatoryotherfields5
                }) = SignatoryDetails
                { signatorysignorder = signatorysignorder5
                , signatoryfields = fields
                }
      where
        fields = [
            SignatoryField {
                sfType = FirstNameFT
              , sfValue = signatoryfstname5
              , sfPlacements = signatoryfstnameplacements5
              }
          , SignatoryField {
                sfType = LastNameFT
              , sfValue = signatorysndname5
              , sfPlacements = signatorysndnameplacements5
              }
          , SignatoryField {
                sfType = CompanyFT
              , sfValue = signatorycompany5
              , sfPlacements = signatorycompanyplacements5
              }
          , SignatoryField {
                sfType = PersonalNumberFT
              , sfValue = signatorypersonalnumber5
              , sfPlacements = signatorypersonalnumberplacements5
              }
          , SignatoryField {
                sfType = CompanyNumberFT
              , sfValue = signatorycompanynumber5
              , sfPlacements = signatorycompanynumberplacements5
              }
          , SignatoryField {
                sfType = EmailFT
              , sfValue = signatoryemail5
              , sfPlacements = signatoryemailplacements5
              }
            ] ++ map toSF signatoryotherfields5
        toSF FieldDefinition{fieldlabel, fieldvalue, fieldplacements, fieldfilledbyauthor} = SignatoryField {
            sfType = CustomFT fieldlabel fieldfilledbyauthor
          , sfValue = fieldvalue
          , sfPlacements = fieldplacements
          }

instance Migrate () SignatoryLink1 where
  migrate _ = error "no migration to SignatoryLink1"

instance Migrate SignatoryLink1 SignatoryLink2 where
    migrate (SignatoryLink1
             { signatorylinkid1
             , signatorydetails1
             , maybesignatory1
             , maybesigninfo1
             , maybeseentime1
             }) = SignatoryLink2
                { signatorylinkid2 = signatorylinkid1
                , signatorydetails2 = signatorydetails1
                , maybesignatory2 = maybesignatory1
                , maybesigninfo2 = maybesigninfo1
                , maybeseentime2 = maybeseentime1
                , signatorymagichash2 = MagicHash $
                                       fromIntegral (unSignatoryLinkID signatorylinkid1) +
                                                        0xcde156781937458e37
                }


instance Migrate SignatoryLink2 SignatoryLink3 where
      migrate (SignatoryLink2
          { signatorylinkid2
          , signatorydetails2
          , signatorymagichash2
          , maybesignatory2
          , maybesigninfo2
          , maybeseentime2
          }) = SignatoryLink3
          { signatorylinkid3    = signatorylinkid2
          , signatorydetails3   = signatorydetails2
          , signatorymagichash3 = signatorymagichash2
          , maybesignatory3     = maybesignatory2
          , maybesigninfo3      = maybesigninfo2
          , maybeseeninfo3      = maybe Nothing (\t -> Just (SignInfo t 0)) maybeseentime2
          }


instance Migrate SignatoryLink3 SignatoryLink4 where
      migrate (SignatoryLink3
          { signatorylinkid3
          , signatorydetails3
          , signatorymagichash3
          , maybesignatory3
          , maybesigninfo3
          , maybeseeninfo3
          }) = SignatoryLink4
          { signatorylinkid4    = signatorylinkid3
          , signatorydetails4   = signatorydetails3
          , signatorymagichash4 = signatorymagichash3
          , maybesignatory4     = maybesignatory3
          , maybesigninfo4      = maybesigninfo3
          , maybeseeninfo4      = maybeseeninfo3
          , invitationdeliverystatus4 = Delivered
          }

instance Migrate SignatoryLink4 SignatoryLink5 where
    migrate (SignatoryLink4
             { signatorylinkid4
             , signatorydetails4
             , signatorymagichash4
             , maybesignatory4
             , maybesigninfo4
             , maybeseeninfo4
             , invitationdeliverystatus4
             }) = SignatoryLink5
             { signatorylinkid5 = signatorylinkid4
             , signatorydetails5 = signatorydetails4
             , signatorymagichash5 = signatorymagichash4
             , maybesignatory5 = maybesignatory4
             , maybesigninfo5 = maybesigninfo4
             , maybeseeninfo5 = maybeseeninfo4
             , invitationdeliverystatus5 = invitationdeliverystatus4
             , signatorysignatureinfo5 = Nothing
             }

instance Migrate SignatoryLink5 SignatoryLink6 where
    migrate (SignatoryLink5
             { signatorylinkid5
             , signatorydetails5
             , signatorymagichash5
             , maybesignatory5
             , maybesigninfo5
             , maybeseeninfo5
             , invitationdeliverystatus5
             , signatorysignatureinfo5
             }) = SignatoryLink6
             { signatorylinkid6 = signatorylinkid5
             , signatorydetails6 = signatorydetails5
             , signatorymagichash6 = signatorymagichash5
             , maybesignatory6 = fmap unSignatory maybesignatory5
             , maybesigninfo6 = maybesigninfo5
             , maybeseeninfo6 = maybeseeninfo5
             , invitationdeliverystatus6 = invitationdeliverystatus5
             , signatorysignatureinfo6 = signatorysignatureinfo5
             }

instance Migrate SignatoryLink6 SignatoryLink7 where
    migrate (SignatoryLink6
             { signatorylinkid6
             , signatorydetails6
             , signatorymagichash6
             , maybesignatory6
             , maybesigninfo6
             , maybeseeninfo6
             , invitationdeliverystatus6
             , signatorysignatureinfo6
             }) = SignatoryLink7
                { signatorylinkid7           = signatorylinkid6
                , signatorydetails7          = signatorydetails6
                , signatorymagichash7        = signatorymagichash6
                , maybesignatory7            = maybesignatory6
                , maybesigninfo7             = maybesigninfo6
                , maybeseeninfo7             = maybeseeninfo6
                , invitationdeliverystatus7  = invitationdeliverystatus6
                , signatorysignatureinfo7    = signatorysignatureinfo6
                , signatorylinkdeleted7      = False
                , signatoryroles7            = [SignatoryPartner]
                }

instance Migrate SignatoryLink7 SignatoryLink8 where
    migrate (SignatoryLink7
             { signatorylinkid7
             , signatorydetails7
             , signatorymagichash7
             , maybesignatory7
             , maybesigninfo7
             , maybeseeninfo7
             , invitationdeliverystatus7
             , signatorysignatureinfo7
             , signatorylinkdeleted7
             , signatoryroles7
             }) = SignatoryLink8
                { signatorylinkid8           = signatorylinkid7
                , signatorydetails8          = signatorydetails7
                , signatorymagichash8        = signatorymagichash7
                , maybesignatory8            = maybesignatory7
                , maybesigninfo8             = maybesigninfo7
                , maybeseeninfo8             = maybeseeninfo7
                , maybereadinvite8           = Nothing
                , invitationdeliverystatus8  = invitationdeliverystatus7
                , signatorysignatureinfo8    = signatorysignatureinfo7
                , signatorylinkdeleted8      = signatorylinkdeleted7
                , signatoryroles8            = signatoryroles7
                }

instance Migrate SignatoryLink8 SignatoryLink9 where
    migrate (SignatoryLink8
             { signatorylinkid8
             , signatorydetails8
             , signatorymagichash8
             , maybesignatory8
             , maybesigninfo8
             , maybeseeninfo8
             , maybereadinvite8
             , invitationdeliverystatus8
             , signatorysignatureinfo8
             , signatorylinkdeleted8
             , signatoryroles8
             }) = SignatoryLink9
                { signatorylinkid9           = signatorylinkid8
                , signatorydetails9          = signatorydetails8
                , signatorymagichash9        = signatorymagichash8
                , maybesignatory9            = maybesignatory8
                , maybesupervisor9           = Nothing
                , maybesigninfo9             = maybesigninfo8
                , maybeseeninfo9             = maybeseeninfo8
                , maybereadinvite9           = maybereadinvite8
                , invitationdeliverystatus9  = invitationdeliverystatus8
                , signatorysignatureinfo9    = signatorysignatureinfo8
                , signatorylinkdeleted9      = signatorylinkdeleted8
                , signatoryroles9            = signatoryroles8
                }

instance Migrate SignatoryLink9 SignatoryLink10 where
    migrate (SignatoryLink9
             { signatorylinkid9
             , signatorydetails9
             , signatorymagichash9
             , maybesignatory9
             , maybesupervisor9
             , maybesigninfo9
             , maybeseeninfo9
             , maybereadinvite9
             , invitationdeliverystatus9
             , signatorysignatureinfo9
             , signatorylinkdeleted9
             , signatoryroles9
             }) = SignatoryLink10
                { signatorylinkid10            = signatorylinkid9
                , signatorydetails10           = signatorydetails9
                , signatorymagichash10         = signatorymagichash9
                , maybesignatory10             = maybesignatory9
                , maybesupervisor10            = maybesupervisor9
                , maybesigninfo10              = maybesigninfo9
                , maybeseeninfo10              = maybeseeninfo9
                , maybereadinvite10            = maybereadinvite9
                , invitationdeliverystatus10   = invitationdeliverystatus9
                , signatorysignatureinfo10     = signatorysignatureinfo9
                , signatorylinkdeleted10       = signatorylinkdeleted9
                , signatorylinkreallydeleted10 = False
                , signatoryroles10             = signatoryroles9
                }

instance Migrate SignatoryLink10 SignatoryLink where
    migrate (SignatoryLink10
             { signatorylinkid10
             , signatorydetails10
             , signatorymagichash10
             , maybesignatory10
             , maybesupervisor10
             , maybesigninfo10
             , maybeseeninfo10
             , maybereadinvite10
             , invitationdeliverystatus10
             , signatorysignatureinfo10
             , signatorylinkdeleted10
             , signatorylinkreallydeleted10
             , signatoryroles10
             }) = SignatoryLink
                { signatorylinkid            = signatorylinkid10
                , signatorydetails           = signatorydetails10
                , signatorymagichash         = signatorymagichash10
                , maybesignatory             = maybesignatory10
                , maybesupervisor            = maybesupervisor10
                , maybecompany               = Nothing
                , maybesigninfo              = maybesigninfo10
                , maybeseeninfo              = maybeseeninfo10
                , maybereadinvite            = maybereadinvite10
                , invitationdeliverystatus   = invitationdeliverystatus10
                , signatorysignatureinfo     = signatorysignatureinfo10
                , signatorylinkdeleted       = signatorylinkdeleted10
                , signatorylinkreallydeleted = signatorylinkreallydeleted10
                , signatoryroles             = signatoryroles10
                }

$(deriveSerialize ''SignatoryLinkID)
instance Version SignatoryLinkID

$(deriveSerialize ''DocumentID)
instance Version DocumentID

$(deriveSerialize ''TimeoutTime)
instance Version TimeoutTime

$(deriveSerialize ''Author)
instance Version Author

$(deriveSerialize ''Signatory)
instance Version Signatory where

$(deriveSerialize ''Supervisor)
instance Version Supervisor where

$(deriveSerialize ''DocumentHistoryEntry0)
instance Version DocumentHistoryEntry0

$(deriveSerialize ''DocumentHistoryEntry)
instance Version DocumentHistoryEntry where
    mode = extension 1 (Proxy :: Proxy DocumentHistoryEntry0)

$(deriveSerialize ''CSVUpload)
instance Version CSVUpload


$(deriveSerialize ''Document)
instance Version Document where
    mode = extension 31 (Proxy :: Proxy ())


instance Migrate DocumentHistoryEntry0 DocumentHistoryEntry where
        migrate (DocumentHistoryCreated0 { dochisttime0 }) =
            DocumentHistoryCreated dochisttime0
        migrate (DocumentHistoryInvitationSent0 { dochisttime0
                                                , ipnumber0
                                                })
            = DocumentHistoryInvitationSent dochisttime0 ipnumber0 []

instance Migrate () Document where
    migrate _ = error "Cannot migrate Document anymore"

$(deriveSerialize ''DocumentStatus)
instance Version DocumentStatus where

$(deriveSerialize ''DocumentProcess)
instance Version DocumentProcess where

$(deriveSerialize ''DocumentType0)
instance Version DocumentType0 where

instance Migrate DocumentType0 DocumentType where
    migrate Contract0 = Signable Contract
    migrate ContractTemplate0 = Template Contract
    migrate Offer0 = Signable Offer
    migrate OfferTemplate0 = Template Offer
    migrate Attachment0 = Attachment
    migrate AttachmentTemplate0 = AttachmentTemplate

$(deriveSerialize ''DocumentType)
instance Version DocumentType where
    mode = extension 1 (Proxy :: Proxy DocumentType0)

$(deriveSerialize ''DocumentRecordStatus)
instance Version DocumentRecordStatus where

$(deriveSerialize ''DocumentFunctionality)
instance Version DocumentFunctionality where

$(deriveSerialize ''ChargeMode)
instance Version ChargeMode where

$(deriveSerialize ''DocumentSharing)
instance Version DocumentSharing where

$(deriveSerialize ''DocumentTag)
instance Version DocumentTag where

$(deriveSerialize ''DocumentUI)
instance Version DocumentUI where


$(deriveSerialize ''DocStats)
instance Version DocStats where



type Documents = IxSet Document


instance Indexable Document where
  empty =
    ixSet [ ixFun (\x -> [documentid x] :: [DocumentID])
            -- wait, wait, wait: the following is wrong, signatory link ids are valid only in
            -- the scope of a single document! FIXME
          , ixFun (\x -> map signatorylinkid (documentsignatorylinks x) :: [SignatoryLinkID])
          , ixFun $ ifDocumentNotDeleted (maybeToList . documenttimeouttime)
          , ixFun $ ifDocumentNotDeleted (\x -> [documenttype x] :: [DocumentType])
          , ixFun $ ifDocumentNotDeleted (\x -> documenttags x :: [DocumentTag])
          , ixFun $ ifDocumentNotDeleted (\x -> [documentservice x] :: [Maybe ServiceID])
          , ixFun $ ifDocumentNotDeleted (\x -> [documentstatus x] :: [DocumentStatus])

          , ixFun $ ifDocumentNotDeleted (\x ->
                      (map Signatory . catMaybes . map maybesignatory $ undeletedSigLinks x) :: [Signatory])
          , ixFun $ ifDocumentNotDeleted (\x ->
                      (catMaybes . map maybecompany $ undeletedSigLinks x) :: [CompanyID])
          , ixFun $ ifDocumentNotDeleted (\x ->
                      (catMaybes . map maybesignatory $ undeletedSigLinks x) :: [UserID])
          , ixFun $ ifDocumentNotDeleted (\x ->
                      (map Author . catMaybes . map maybesignatory .
                         filter (\sl -> (SignatoryAuthor `elem` signatoryroles sl)) $ undeletedSigLinks x) :: [Author])
          ]
          where
            ifDocumentNotDeleted :: (Document -> [a]) -> Document -> [a]
            ifDocumentNotDeleted f doc
              | documentdeleted doc = []
              | otherwise = f doc
            undeletedSigLinks doc =
              filter (not . signatorylinkreallydeleted) $ documentsignatorylinks doc

instance Component Documents where
  type Dependencies Documents = End
  initialValue = empty

$(deriveSerialize ''SignatoryRole)

-- stuff for converting to pgsql

$(bitfieldDeriveConvertible ''SignatoryRole)
$(enumDeriveConvertible ''MailsDeliveryStatus)
$(newtypeDeriveConvertible ''SignOrder)
$(jsonableDeriveConvertible [t| [SignatoryField] |])
$(jsonableDeriveConvertible [t| [DocumentLogEntry] |])
$(enumDeriveConvertible ''DocumentFunctionality)
$(enumDeriveConvertible ''DocumentProcess)
-- $(jsonableDeriveConvertible [t| DocumentStatus |])
$(enumDeriveConvertibleIgnoreFields ''DocumentStatus)
$(bitfieldDeriveConvertible ''IdentificationType)
$(newtypeDeriveConvertible ''DocumentID)
$(enumDeriveConvertible ''DocumentSharing)
$(newtypeDeriveConvertible ''SignatoryLinkID)
$(jsonableDeriveConvertible [t| [DocumentTag] |])
$(jsonableDeriveConvertible [t| CancelationReason |])
$(jsonableDeriveConvertible [t| [[BS.ByteString ]] |])
