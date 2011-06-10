{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
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
    , DocumentType(..)
    , DocumentRecordStatus(..)
    , Documents
    , FieldDefinition(..)
    , FieldPlacement(..)
    , File(..)
    , FileID(..)
    , FileStorage(..)
    , IdentificationType(..)
    , JpegPages(..)
    , SignInfo(..)
    , SignOrder(..)
    , Signatory(..)
    , SignatoryDetails(..)
    , SignatoryLink(..)
    , SignatoryLinkID(..)
    , SignatoryRole(..)
    , SignatureInfo(..)
    , SignatureProvider(..)
    , TimeoutTime(..)
    , AuthorAttachment(..)
    , SignatoryAttachment(..)
    , documentHistoryToDocumentLog
    , getAuthorSigLink
    ) where

import API.Service.ServiceState
import Company.CompanyState
import Control.Monad
import Data.Bits
import Data.Data (Data)
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import Happstack.Server.SimpleHTTP
import Happstack.State
import Happstack.Util.Common
import Mails.MailsUtil
import MinutesTime
import Misc
import User.UserState
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

newtype Author = Author { unAuthor :: UserID }
    deriving (Eq, Ord, Typeable)

newtype DocumentID = DocumentID { unDocumentID :: Int64 }
    deriving (Eq, Ord, Typeable, Data) -- Data needed by PayEx modules
newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int }
    deriving (Eq, Ord, Typeable)
newtype FileID = FileID { unFileID :: Int }
    deriving (Eq, Ord, Typeable)
newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }
    deriving (Eq, Ord, Typeable)
newtype SignOrder = SignOrder { unSignOrder :: Integer }
    deriving (Eq, Ord, Typeable)

instance Show SignOrder where
    show (SignOrder n) = show n

data IdentificationType = EmailIdentification
                        | ELegitimationIdentification
    deriving (Eq, Ord, Typeable)

data SignatureProvider = BankIDProvider
                       | TeliaProvider
                       | NordeaProvider
    deriving (Eq, Ord, Typeable)

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

-- defines where a field is placed
data FieldPlacement = FieldPlacement
    { placementx :: Int
    , placementy :: Int
    , placementpage :: Int
    , placementpagewidth :: Int
    , placementpageheight :: Int
    }
    deriving (Eq, Ord, Typeable)
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

data SignatoryDetails = SignatoryDetails
    { signatoryfstname        :: BS.ByteString  -- "Gracjan" 
    , signatorysndname        :: BS.ByteString  -- "Polak" 
    , signatorycompany        :: BS.ByteString  -- SkrivaPå
    , signatorypersonalnumber :: BS.ByteString  -- 123456789
    , signatorycompanynumber  :: BS.ByteString  -- 123456789
    , signatoryemail          :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
    -- for ordered signing
    , signatorysignorder      :: SignOrder
    -- for templates
    , signatoryfstnameplacements        :: [FieldPlacement]
    , signatorysndnameplacements        :: [FieldPlacement]
    , signatorycompanyplacements        :: [FieldPlacement]
    , signatoryemailplacements          :: [FieldPlacement]
    , signatorypersonalnumberplacements :: [FieldPlacement]
    , signatorycompanynumberplacements  :: [FieldPlacement]
    , signatoryotherfields              :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)

data SignatoryLink0 = SignatoryLink0 
    { signatorylinkid0    :: SignatoryLinkID
    , signatoryname0      :: BS.ByteString 
    , signatorycompany0   :: BS.ByteString 
    , signatoryemail0     :: BS.ByteString
    , maybesignatory0     :: Maybe Signatory
    , maybesigninfo0      :: Maybe SignInfo
    , maybeseentime0      :: Maybe MinutesTime
    }
    deriving (Eq, Ord, Typeable)

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

data SignatoryLink = SignatoryLink 
    { signatorylinkid          :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
    , signatorydetails         :: SignatoryDetails    -- ^ details of this person as filled in invitation
    , signatorymagichash       :: MagicHash           -- ^ authentication code
    , maybesignatory           :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
    , maybesupervisor          :: Maybe UserID        -- ^ if this document has been saved to an account with a supervisor, this is the userid
    , maybesigninfo            :: Maybe SignInfo      -- ^ when a person has signed this document
    , maybeseeninfo            :: Maybe SignInfo      -- ^ when a person has first seen this document
    , maybereadinvite          :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read 
    , invitationdeliverystatus :: MailsDeliveryStatus -- ^ status of email delivery
    , signatorysignatureinfo   :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
    , signatoryroles           :: [SignatoryRole]
    , signatorylinkdeleted     :: Bool
    }    
    deriving (Eq, Ord, Typeable)

data SignatoryRole = SignatoryPartner | SignatoryAuthor
    deriving (Eq, Ord, Typeable, Show)

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
    deriving (Eq, Ord, Typeable)
                    
data DocumentType = Contract | ContractTemplate | Offer | OfferTemplate | Attachment | AttachmentTemplate
    deriving (Eq, Ord, Typeable)

data DocumentRecordStatus = LiveDocument | QuarantinedDocument | DeletedDocument
    deriving (Eq, Ord, Typeable, Show)

data DocumentFunctionality = BasicFunctionality | AdvancedFunctionality
    deriving (Eq, Ord, Typeable)

data ChargeMode = ChargeInitialFree   -- initial 5 documents are free
                | ChargeNormal        -- value times number of people involved

    deriving (Eq, Ord, Typeable)

data DocumentSharing = Private
                       | Shared -- means that the document is shared with subaccounts, and those with same parent accounts
    deriving (Eq, Ord, Typeable)

data DocumentTag = DocumentTag {
        tagname :: BS.ByteString
     ,  tagvalue :: BS.ByteString
     } deriving (Eq, Ord, Typeable)

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
    deriving (Typeable, Show)

$(deriveSerialize ''DocumentLogEntry)
instance Version DocumentLogEntry

-- oh boy, this is really network byte order!
formatIP :: Word32 -> String
formatIP 0 = ""
-- formatIP 0x7f000001 = ""
formatIP x = " (IP: " ++ show ((x `shiftR` 0) .&. 255) ++
                   "." ++ show ((x `shiftR` 8) .&. 255) ++
                   "." ++ show ((x `shiftR` 16) .&. 255) ++
                   "." ++ show ((x `shiftR` 24) .&. 255) ++ ")"

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

    
data Document11 = Document11
    { documentid11               :: DocumentID
    , documenttitle11            :: BS.ByteString
    , documentauthor11           :: Author
    , documentsignatorylinks11   :: [SignatoryLink]  
    , documentfiles11            :: [File]
    , documentsealedfiles11      :: [File]
    , documentstatus11           :: DocumentStatus
    , documentctime11            :: MinutesTime
    , documentmtime11            :: MinutesTime
    , documentchargemode11       :: ChargeMode
    , documentdaystosign11       :: Maybe Int
    , documenttimeouttime11      :: Maybe TimeoutTime 
    , documentdeleted11          :: Bool
    , documenthistory11          :: [DocumentHistoryEntry]
    , documentinvitetext11       :: BS.ByteString
    , documenttrustweaverreference11 :: Maybe BS.ByteString
    , documentallowedidtypes11   :: [IdentificationType]
    , authorfstnameplacements11 :: [FieldPlacement]
    , authorsndnameplacements11 :: [FieldPlacement]
    , authorcompanyplacements11 :: [FieldPlacement]
    , authoremailplacements11 :: [FieldPlacement]
    , authornumberplacements11 :: [FieldPlacement]
    , authorotherfields11 :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)

    
data Document12 = Document12
    { documentid12               :: DocumentID
    , documenttitle12            :: BS.ByteString
    , documentauthor12           :: Author
    , documentsignatorylinks12   :: [SignatoryLink]  
    , documentfiles12            :: [File]
    , documentsealedfiles12      :: [File]
    , documentstatus12           :: DocumentStatus
    , documenttype12             :: DocumentType
    , documentctime12            :: MinutesTime
    , documentmtime12            :: MinutesTime
    , documentchargemode12       :: ChargeMode
    , documentdaystosign12       :: Maybe Int
    , documenttimeouttime12      :: Maybe TimeoutTime 
    -- | If true, this Document will not appear in the document list
    , documentdeleted12          :: Bool
    , documenthistory12          :: [DocumentHistoryEntry]
    , documentinvitetext12       :: BS.ByteString
    , documenttrustweaverreference12 :: Maybe BS.ByteString
    , documentallowedidtypes12   :: [IdentificationType]
    , authorfstnameplacements12 :: [FieldPlacement]
    , authorsndnameplacements12 :: [FieldPlacement]
    , authorcompanyplacements12 :: [FieldPlacement]
    , authoremailplacements12 :: [FieldPlacement]
    , authornumberplacements12 :: [FieldPlacement]
    , authorotherfields12 :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)
    
data Document13 = Document13
    { documentid13               :: DocumentID
    , documenttitle13            :: BS.ByteString
    , documentauthor13           :: Author
    , documentsignatorylinks13   :: [SignatoryLink]  
    , documentfiles13            :: [File]
    , documentsealedfiles13      :: [File]
    , documentstatus13           :: DocumentStatus
    , documenttype13             :: DocumentType
    , documentctime13            :: MinutesTime
    , documentmtime13            :: MinutesTime
    , documentchargemode13       :: ChargeMode
    , documentdaystosign13       :: Maybe Int
    , documenttimeouttime13      :: Maybe TimeoutTime 
    -- | If true, this Document will not appear in the document list
    , documentdeleted13          :: Bool
    , documenthistory13          :: [DocumentHistoryEntry]
    , documentinvitetext13       :: BS.ByteString
    , documenttrustweaverreference13 :: Maybe BS.ByteString
    , documentallowedidtypes13   :: [IdentificationType]
    , authorfstnameplacements13 :: [FieldPlacement]
    , authorsndnameplacements13 :: [FieldPlacement]
    , authorcompanyplacements13 :: [FieldPlacement]
    , authoremailplacements13 :: [FieldPlacement]
    , authornumberplacements13 :: [FieldPlacement]
    , authorotherfields13 :: [FieldDefinition]
    , documentcancelationreason13 :: Maybe CancelationReason
    }
    deriving (Eq, Ord, Typeable)

data Document14 = Document14
    { documentid14                   :: DocumentID
    , documenttitle14                :: BS.ByteString
    , documentauthor14               :: Author                  -- to be moved to siglinks
    , documentsignatorylinks14       :: [SignatoryLink]  
    , documentfiles14                :: [File]
    , documentsealedfiles14          :: [File]
    , documentstatus14               :: DocumentStatus
    , documenttype14                 :: DocumentType
    , documentctime14                :: MinutesTime
    , documentmtime14                :: MinutesTime
    , documentchargemode14           :: ChargeMode              -- to be removed
    , documentdaystosign14           :: Maybe Int    
    , documenttimeouttime14          :: Maybe TimeoutTime 
    , documentdeleted14              :: Bool                    -- to be moved to links
    , documenthistory14              :: [DocumentHistoryEntry]  -- to be made into plain text 
    , documentinvitetext14           :: BS.ByteString             
    , documenttrustweaverreference14 :: Maybe BS.ByteString
    , documentallowedidtypes14       :: [IdentificationType]
    , documentcsvupload14            :: Maybe CSVUpload
    , authorfstnameplacements14      :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements14      :: [FieldPlacement]
    , authorcompanyplacements14      :: [FieldPlacement]
    , authoremailplacements14        :: [FieldPlacement]
    , authornumberplacements14       :: [FieldPlacement]
    , authorotherfields14            :: [FieldDefinition]
    , documentcancelationreason14    :: Maybe CancelationReason -- ??
    }
    deriving (Eq, Ord, Typeable)

data Document15 = Document15
    { documentid15                   :: DocumentID
    , documenttitle15                :: BS.ByteString
    , documentauthor15               :: Author                  -- to be moved to siglinks
    , documentsignatorylinks15       :: [SignatoryLink]  
    , documentfiles15                :: [File]
    , documentsealedfiles15          :: [File]
    , documentstatus15               :: DocumentStatus
    , documenttype15                 :: DocumentType
    , documentctime15                :: MinutesTime
    , documentmtime15                :: MinutesTime
    , documentdaystosign15           :: Maybe Int    
    , documenttimeouttime15          :: Maybe TimeoutTime
    , documentinvitetime15           :: Maybe SignInfo
    , documentdeleted15              :: Bool                    -- to be moved to links
    , documentlog15                  :: [DocumentLogEntry]      -- to be made into plain text 
    , documentinvitetext15           :: BS.ByteString             
    , documenttrustweaverreference15 :: Maybe BS.ByteString
    , documentallowedidtypes15       :: [IdentificationType]
    , documentcsvupload15            :: Maybe CSVUpload
    , authorfstnameplacements15      :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements15      :: [FieldPlacement]
    , authorcompanyplacements15      :: [FieldPlacement]
    , authoremailplacements15        :: [FieldPlacement]
    , authornumberplacements15       :: [FieldPlacement]
    , authorotherfields15            :: [FieldDefinition]
    , documentcancelationreason15    :: Maybe CancelationReason -- ??
    }
    deriving Typeable

data Document16 = Document16
    { documentid16                     :: DocumentID
    , documenttitle16                  :: BS.ByteString
    , documentauthor16                 :: Author                  -- to be moved to siglinks
    , documentsignatorylinks16         :: [SignatoryLink]  
    , documentfiles16                  :: [File]
    , documentsealedfiles16            :: [File]
    , documentstatus16                 :: DocumentStatus
    , documenttype16                   :: DocumentType
    , documentctime16                  :: MinutesTime
    , documentmtime16                  :: MinutesTime
    , documentdaystosign16             :: Maybe Int    
    , documenttimeouttime16            :: Maybe TimeoutTime
    , documentinvitetime16             :: Maybe SignInfo
    , documentdeleted16                :: Bool                    -- to be moved to links
    , documentlog16                    :: [DocumentLogEntry]      -- to be made into plain text 
    , documentinvitetext16             :: BS.ByteString             
    , documenttrustweaverreference16   :: Maybe BS.ByteString
    , documentallowedidtypes16         :: [IdentificationType]
    , documentcsvupload16              :: Maybe CSVUpload
    , authorfstnameplacements16        :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements16        :: [FieldPlacement]
    , authorcompanyplacements16        :: [FieldPlacement]
    , authoremailplacements16          :: [FieldPlacement]
    , authorpersonalnumberplacements16 :: [FieldPlacement]
    , authorcompanynumberplacements16  :: [FieldPlacement]
    , authorotherfields16              :: [FieldDefinition]
    , documentcancelationreason16      :: Maybe CancelationReason -- ??
    }
    deriving Typeable

data Document17 = Document17
    { documentid17                     :: DocumentID
    , documenttitle17                  :: BS.ByteString
    , documentauthor17                 :: Author                  -- to be moved to siglinks
    , documentsignatorylinks17         :: [SignatoryLink]  
    , documentfiles17                  :: [File]
    , documentsealedfiles17            :: [File]
    , documentstatus17                 :: DocumentStatus
    , documenttype17                   :: DocumentType
    , documentfunctionality17          :: DocumentFunctionality
    , documentctime17                  :: MinutesTime
    , documentmtime17                  :: MinutesTime
    , documentdaystosign17             :: Maybe Int    
    , documenttimeouttime17            :: Maybe TimeoutTime
    , documentinvitetime17             :: Maybe SignInfo
    , documentdeleted17                :: Bool                    -- to be moved to links
    , documentlog17                    :: [DocumentLogEntry]      -- to be made into plain text 
    , documentinvitetext17             :: BS.ByteString             
    , documenttrustweaverreference17   :: Maybe BS.ByteString
    , documentallowedidtypes17         :: [IdentificationType]
    , documentcsvupload17              :: Maybe CSVUpload
    , authorfstnameplacements17        :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements17        :: [FieldPlacement]
    , authorcompanyplacements17        :: [FieldPlacement]
    , authoremailplacements17          :: [FieldPlacement]
    , authorpersonalnumberplacements17 :: [FieldPlacement]
    , authorcompanynumberplacements17  :: [FieldPlacement]
    , authorotherfields17              :: [FieldDefinition]
    , documentcancelationreason17      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    }
    deriving Typeable

data Document18 = Document18
    { documentid18                     :: DocumentID
    , documenttitle18                  :: BS.ByteString
    , documentauthor18                 :: Author                  -- to be moved to siglinks
    , documentsignatorylinks18         :: [SignatoryLink]
    , documentfiles18                  :: [File]
    , documentsealedfiles18            :: [File]
    , documentstatus18                 :: DocumentStatus
    , documenttype18                   :: DocumentType
    , documentfunctionality18          :: DocumentFunctionality
    , documentctime18                  :: MinutesTime
    , documentmtime18                  :: MinutesTime
    , documentdaystosign18             :: Maybe Int    
    , documenttimeouttime18            :: Maybe TimeoutTime
    , documentinvitetime18             :: Maybe SignInfo
    , documentdeleted18                :: Bool                    -- to be moved to links
    , documentlog18                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext18             :: BS.ByteString
    , documenttrustweaverreference18   :: Maybe BS.ByteString
    , documentallowedidtypes18         :: [IdentificationType]
    , documentcsvupload18              :: Maybe CSVUpload
    , authorfstnameplacements18        :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements18        :: [FieldPlacement]
    , authorcompanyplacements18        :: [FieldPlacement]
    , authoremailplacements18          :: [FieldPlacement]
    , authorpersonalnumberplacements18 :: [FieldPlacement]
    , authorcompanynumberplacements18  :: [FieldPlacement]
    , authorotherfields18              :: [FieldDefinition]
    , documentcancelationreason18      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing18                :: DocumentSharing
    } deriving Typeable
                

data Document19 = Document19
    { documentid19                     :: DocumentID
    , documenttitle19                  :: BS.ByteString
    , documentauthor19                 :: Author                  -- to be moved to siglinks
    , documentsignatorylinks19         :: [SignatoryLink]
    , documentfiles19                  :: [File]
    , documentsealedfiles19            :: [File]
    , documentstatus19                 :: DocumentStatus
    , documenttype19                   :: DocumentType
    , documentfunctionality19          :: DocumentFunctionality
    , documentctime19                  :: MinutesTime
    , documentmtime19                  :: MinutesTime
    , documentdaystosign19             :: Maybe Int    
    , documenttimeouttime19            :: Maybe TimeoutTime
    , documentinvitetime19             :: Maybe SignInfo
    , documentdeleted19                :: Bool                    -- to be moved to links
    , documentlog19                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext19             :: BS.ByteString
    , documenttrustweaverreference19   :: Maybe BS.ByteString
    , documentallowedidtypes19         :: [IdentificationType]
    , documentcsvupload19              :: Maybe CSVUpload
    , authorfstnameplacements19        :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements19        :: [FieldPlacement]
    , authorcompanyplacements19        :: [FieldPlacement]
    , authoremailplacements19          :: [FieldPlacement]
    , authorpersonalnumberplacements19 :: [FieldPlacement]
    , authorcompanynumberplacements19  :: [FieldPlacement]
    , authorotherfields19              :: [FieldDefinition]
    , documentcancelationreason19      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing19                :: DocumentSharing
    , documentrejectioninfo19          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    } deriving Typeable

data Document20 = Document20
    { documentid20                     :: DocumentID
    , documenttitle20                  :: BS.ByteString
    , documentauthor20                 :: Author                  -- to be moved to siglinks
    , documentsignatorylinks20         :: [SignatoryLink]
    , documentfiles20                  :: [File]
    , documentsealedfiles20            :: [File]
    , documentstatus20                 :: DocumentStatus
    , documenttype20                   :: DocumentType
    , documentfunctionality20          :: DocumentFunctionality
    , documentctime20                  :: MinutesTime
    , documentmtime20                  :: MinutesTime
    , documentdaystosign20             :: Maybe Int    
    , documenttimeouttime20            :: Maybe TimeoutTime
    , documentinvitetime20             :: Maybe SignInfo
    , documentdeleted20                :: Bool                    -- to be moved to links
    , documentlog20                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext20             :: BS.ByteString
    , documenttrustweaverreference20   :: Maybe BS.ByteString
    , documentallowedidtypes20         :: [IdentificationType]
    , documentcsvupload20              :: Maybe CSVUpload
    , authorfstnameplacements20        :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements20        :: [FieldPlacement]
    , authorcompanyplacements20        :: [FieldPlacement]
    , authoremailplacements20          :: [FieldPlacement]
    , authorpersonalnumberplacements20 :: [FieldPlacement]
    , authorcompanynumberplacements20  :: [FieldPlacement]
    , authorotherfields20              :: [FieldDefinition]
    , documentcancelationreason20      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing20                :: DocumentSharing
    , documentrejectioninfo20          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    , documenttags20                   :: [DocumentTag]
    } deriving Typeable


data Document21 = Document21
    { documentid21                     :: DocumentID
    , documenttitle21                  :: BS.ByteString
    , documentauthor21                 :: Author                  -- to be moved to siglinks
    , documentsignatorylinks21         :: [SignatoryLink]
    , documentfiles21                  :: [File]
    , documentsealedfiles21            :: [File]
    , documentstatus21                 :: DocumentStatus
    , documenttype21                   :: DocumentType
    , documentfunctionality21          :: DocumentFunctionality
    , documentctime21                  :: MinutesTime
    , documentmtime21                  :: MinutesTime
    , documentdaystosign21             :: Maybe Int    
    , documenttimeouttime21            :: Maybe TimeoutTime
    , documentinvitetime21             :: Maybe SignInfo
    , documentdeleted21                :: Bool                    -- to be moved to links
    , documentlog21                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext21             :: BS.ByteString
    , documenttrustweaverreference21   :: Maybe BS.ByteString
    , documentallowedidtypes21         :: [IdentificationType]
    , documentcsvupload21              :: Maybe CSVUpload
    , authorfstnameplacements21        :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements21        :: [FieldPlacement]
    , authorcompanyplacements21        :: [FieldPlacement]
    , authoremailplacements21          :: [FieldPlacement]
    , authorpersonalnumberplacements21 :: [FieldPlacement]
    , authorcompanynumberplacements21  :: [FieldPlacement]
    , authorotherfields21              :: [FieldDefinition]
    , documentcancelationreason21      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing21                :: DocumentSharing
    , documentrejectioninfo21          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    , documenttags21                   :: [DocumentTag]
    , documentservice21                :: Maybe ServiceID
    } deriving Typeable

data Document22 = Document22
    { documentid22                     :: DocumentID
    , documenttitle22                  :: BS.ByteString
    , documentauthor22                 :: Author                  -- to be moved to siglinks
    , documentsignatorylinks22         :: [SignatoryLink]
    , documentfiles22                  :: [File]
    , documentsealedfiles22            :: [File]
    , documentstatus22                 :: DocumentStatus
    , documenttype22                   :: DocumentType
    , documentfunctionality22          :: DocumentFunctionality
    , documentctime22                  :: MinutesTime
    , documentmtime22                  :: MinutesTime
    , documentdaystosign22             :: Maybe Int    
    , documenttimeouttime22            :: Maybe TimeoutTime
    , documentinvitetime22             :: Maybe SignInfo
    , documentdeleted22                :: Bool                    -- to be moved to links
    , documentlog22                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext22             :: BS.ByteString
    , documenttrustweaverreference22   :: Maybe BS.ByteString
    , documentallowedidtypes22         :: [IdentificationType]
    , documentcsvupload22              :: Maybe CSVUpload
    , authorfstnameplacements22        :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements22        :: [FieldPlacement]
    , authorcompanyplacements22        :: [FieldPlacement]
    , authoremailplacements22          :: [FieldPlacement]
    , authorpersonalnumberplacements22 :: [FieldPlacement]
    , authorcompanynumberplacements22  :: [FieldPlacement]
    , authorotherfields22              :: [FieldDefinition]
    , documentcancelationreason22      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing22                :: DocumentSharing
    , documentrejectioninfo22          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    , documenttags22                   :: [DocumentTag]
    , documentservice22                :: Maybe ServiceID
    , documentattachments22            :: [DocumentID]
    } deriving Typeable

data Document23 = Document23
    { documentid23                     :: DocumentID
    , documenttitle23                  :: BS.ByteString
    , documentsignatorylinks23         :: [SignatoryLink]
    , documentfiles23                  :: [File]
    , documentsealedfiles23            :: [File]
    , documentstatus23                 :: DocumentStatus
    , documenttype23                   :: DocumentType
    , documentfunctionality23          :: DocumentFunctionality
    , documentctime23                  :: MinutesTime
    , documentmtime23                  :: MinutesTime
    , documentdaystosign23             :: Maybe Int    
    , documenttimeouttime23            :: Maybe TimeoutTime
    , documentinvitetime23             :: Maybe SignInfo
    , documentlog23                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext23             :: BS.ByteString
    , documenttrustweaverreference23   :: Maybe BS.ByteString
    , documentallowedidtypes23         :: [IdentificationType]
    , documentcsvupload23              :: Maybe CSVUpload
    , documentcancelationreason23      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing23                :: DocumentSharing
    , documentrejectioninfo23          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    , documenttags23                   :: [DocumentTag]
    , documentservice23                :: Maybe ServiceID
    , documentattachments23            :: [DocumentID]
    } deriving Typeable

data Document24 = Document24
    { documentid24                     :: DocumentID
    , documenttitle24                  :: BS.ByteString
    , documentsignatorylinks24         :: [SignatoryLink]
    , documentfiles24                  :: [File]
    , documentsealedfiles24            :: [File]
    , documentstatus24                 :: DocumentStatus
    , documenttype24                   :: DocumentType
    , documentfunctionality24          :: DocumentFunctionality
    , documentctime24                  :: MinutesTime
    , documentmtime24                  :: MinutesTime
    , documentdaystosign24             :: Maybe Int    
    , documenttimeouttime24            :: Maybe TimeoutTime
    , documentinvitetime24             :: Maybe SignInfo
    , documentlog24                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext24             :: BS.ByteString
    , documenttrustweaverreference24   :: Maybe BS.ByteString
    , documentallowedidtypes24         :: [IdentificationType]
    , documentcsvupload24              :: Maybe CSVUpload
    , documentcancelationreason24      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing24                :: DocumentSharing
    , documentrejectioninfo24          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    , documenttags24                   :: [DocumentTag]
    , documentservice24                :: Maybe ServiceID
    , documentattachments24            :: [DocumentID]
    , documentoriginalcompany24        :: Maybe CompanyID
    } deriving Typeable

-- migration for author attachments
data Document25 = Document25
    { documentid25                     :: DocumentID
    , documenttitle25                  :: BS.ByteString
    , documentsignatorylinks25         :: [SignatoryLink]
    , documentfiles25                  :: [File]
    , documentsealedfiles25            :: [File]
    , documentstatus25                 :: DocumentStatus
    , documenttype25                   :: DocumentType
    , documentfunctionality25          :: DocumentFunctionality
    , documentctime25                  :: MinutesTime
    , documentmtime25                  :: MinutesTime
    , documentdaystosign25             :: Maybe Int    
    , documenttimeouttime25            :: Maybe TimeoutTime
    , documentinvitetime25             :: Maybe SignInfo
    , documentlog25                    :: [DocumentLogEntry]      -- to be made into plain text
    , documentinvitetext25             :: BS.ByteString
    , documenttrustweaverreference25   :: Maybe BS.ByteString
    , documentallowedidtypes25         :: [IdentificationType]
    , documentcsvupload25              :: Maybe CSVUpload
    , documentcancelationreason25      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing25                :: DocumentSharing
    , documentrejectioninfo25          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    , documenttags25                   :: [DocumentTag]
    , documentservice25                :: Maybe ServiceID
    , documentattachments25            :: [DocumentID]
    , documentoriginalcompany25        :: Maybe CompanyID
    , documentrecordstatus25           :: DocumentRecordStatus
    , documentquarantineexpiry25       :: Maybe MinutesTime  -- the time when any quarantine will end (included as a separate field to record status for easy indexing)
    } deriving Typeable

-- migration for author attachments
data Document = Document
    { documentid                     :: DocumentID
    , documenttitle                  :: BS.ByteString
    , documentsignatorylinks         :: [SignatoryLink]
    , documentfiles                  :: [File]
    , documentsealedfiles            :: [File]
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
    , documenttrustweaverreference   :: Maybe BS.ByteString
    , documentallowedidtypes         :: [IdentificationType]
    , documentcsvupload              :: Maybe CSVUpload
    , documentcancelationreason      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
    , documentsharing                :: DocumentSharing
    , documentrejectioninfo          :: Maybe (MinutesTime, SignatoryLinkID, BS.ByteString)
    , documenttags                   :: [DocumentTag]
    , documentservice                :: Maybe ServiceID
    , documentattachments            :: [DocumentID]
    , documentoriginalcompany        :: Maybe CompanyID
    , documentrecordstatus           :: DocumentRecordStatus
    , documentquarantineexpiry       :: Maybe MinutesTime  -- the time when any quarantine will end (included as a separate field to record status for easy indexing)
    , documentauthorattachments      :: [AuthorAttachment]
    , documentsignatoryattachments   :: [SignatoryAttachment]    
    }


data CancelationReason =  ManualCancel
                        -- The data returned by ELeg server
                        --                 msg                    fn            ln            num
                        | ELegDataMismatch String SignatoryLinkID BS.ByteString BS.ByteString BS.ByteString
    deriving (Eq, Ord, Typeable)


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
    
data File0 = File0 
    { fileid0       :: FileID
    , filename0     :: BS.ByteString
    , filepdf0      :: BS.ByteString 
    , filejpgpages0 :: [BS.ByteString]
    }
    deriving (Eq, Ord, Typeable)

data File1 = File1 
    { fileid1       :: FileID
    , filename1     :: BS.ByteString
    , filepdf1      :: BS.ByteString 
    , filejpgpages1 :: JpegPages
    }
    deriving (Eq, Ord, Typeable)

data File2 = File2 
    { fileid2       :: FileID
    , filename2     :: BS.ByteString
    , filepdf2      :: BS.ByteString 
    }
    deriving (Eq, Ord, Typeable)

data File = File 
    { fileid          :: FileID
    , filename        :: BS.ByteString
    , filestorage     :: FileStorage
    }
    deriving (Typeable)
             
-- for Author Attachment and Signatory Attachments, obviously -EN
data AuthorAttachment = AuthorAttachment { authorattachmentfile :: File }
                      deriving (Eq, Ord, Typeable)
                               
data SignatoryAttachment = SignatoryAttachment { signatoryattachmentfile            :: Maybe File
                                               , signatoryattachmentemail           :: BS.ByteString
                                               , signatoryattachmentname            :: BS.ByteString
                                               , signatoryattachmentdescription     :: BS.ByteString
                                               }
                         deriving (Eq, Ord, Typeable)

             

data JpegPages0 = JpegPagesPending0
               | JpegPages0 [BS.ByteString]   
               | JpegPagesError0 BS.ByteString 
    deriving (Eq, Ord, Typeable)

data JpegPages = JpegPagesPending
               | JpegPages [(BS.ByteString,Int,Int)]  -- Data + width + height (scaled with some resolution)
               | JpegPagesError BS.ByteString 
    deriving (Eq, Ord, Typeable)
               
data FileStorage = FileStorageMemory BS.ByteString
                 | FileStorageAWS BS.ByteString BS.ByteString -- ^ bucket, url inside bucket
                 | FileStorageDisk FilePath -- ^ filepath
    deriving (Eq, Ord, Typeable)



instance Eq Document where
    a == b = documentid a == documentid b

instance Ord Document where
    compare a b | documentid a == documentid b = EQ
                | otherwise = compare (documentmtime b,documenttitle a,documentid a) 
                                      (documentmtime a,documenttitle b,documentid b)
                              -- see above: we use reverse time here!

instance Eq File where
    a == b = fileid a == fileid b

instance Ord File where
    compare a b | fileid a == fileid b = EQ
                | otherwise = compare (fileid a,filename a) 
                                      (fileid b,filename b)

instance Show SignatoryLinkID where
    showsPrec prec (SignatoryLinkID x) = showsPrec prec x

instance Show JpegPages where 
    show JpegPagesPending = "penging"
    show (JpegPages l) = show l   
    show (JpegPagesError c) = "error " ++ (show c)

deriving instance Show Document
deriving instance Show DocumentStatus
deriving instance Show DocumentType
deriving instance Read DocumentType
deriving instance Show DocumentFunctionality
deriving instance Show CSVUpload
deriving instance Show ChargeMode
deriving instance Show DocumentSharing
deriving instance Show DocumentTag
deriving instance Show Author

deriving instance Show DocStats

deriving instance Show FieldDefinition
deriving instance Show FieldPlacement

deriving instance Show AuthorAttachment
deriving instance Show SignatoryAttachment

instance Show TimeoutTime where
    showsPrec prec = showsPrec prec . unTimeoutTime

deriving instance Show SignatoryLink
deriving instance Show SignatoryLink0
deriving instance Show SignatoryLink1
deriving instance Show SignatoryLink2
deriving instance Show SignInfo
deriving instance Show SignInfo0
deriving instance Show SignatoryDetails
deriving instance Show SignatoryDetails0
deriving instance Show DocumentHistoryEntry
deriving instance Show IdentificationType
deriving instance Show CancelationReason
deriving instance Show SignatureProvider
deriving instance Show SignatureInfo0
deriving instance Show SignatureInfo

instance Show Signatory where
    showsPrec prec (Signatory userid) = showsPrec prec userid

instance Show DocumentID where
    showsPrec prec (DocumentID val) = 
         showsPrec prec val

instance Read DocumentID where
    readsPrec prec = let makeDocumentID (i,v) = (DocumentID i,v) 
                     in map makeDocumentID . readsPrec prec 

instance Read SignatoryLinkID where
    readsPrec prec = let make (i,v) = (SignatoryLinkID i,v) 
                     in map make . readsPrec prec 

instance Show File where
    showsPrec _prec file = (++) (BS.toString (filename file))

instance Show FileID where
    showsPrec prec (FileID val) = showsPrec prec val

instance Read FileID where
    readsPrec prec = let make (i,v) = (FileID i,v) 
                     in map make . readsPrec prec 

instance FromReqURI DocumentID where
    fromReqURI = readM

instance FromReqURI SignatoryLinkID where
    fromReqURI = readM

instance FromReqURI FileID where
    fromReqURI = readM
    
$(deriveSerialize ''AuthorAttachment)
instance Version AuthorAttachment

$(deriveSerialize ''SignatoryAttachment)
instance Version SignatoryAttachment

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

$(deriveSerialize ''SignatureProvider)
instance Version SignatureProvider

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

$(deriveSerialize ''SignatoryDetails)
instance Version SignatoryDetails where
    mode = extension 5 (Proxy :: Proxy SignatoryDetails4)

$(deriveSerialize ''SignatoryLink0)
instance Version SignatoryLink0

$(deriveSerialize ''SignatoryLink1)
instance Version SignatoryLink1 where
    mode = extension 1 (Proxy :: Proxy SignatoryLink0)

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

$(deriveSerialize ''SignatoryLink)
instance Version SignatoryLink where
    mode = extension 9 (Proxy :: Proxy SignatoryLink8)

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

instance Migrate SignatoryDetails4 SignatoryDetails where
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
                }) = SignatoryDetails
                { signatoryfstname = signatoryfstname4
                , signatorysndname = signatorysndname4
                , signatorycompany = signatorycompany4
                , signatorypersonalnumber = signatorypersonalnumber4
                , signatorycompanynumber = signatorycompanynumber4
                , signatoryemail = signatoryemail4
                , signatorysignorder = SignOrder 1
                , signatoryfstnameplacements = signatoryfstnameplacements4
                , signatorysndnameplacements = signatorysndnameplacements4
                , signatorycompanyplacements = signatorycompanyplacements4
                , signatoryemailplacements = signatoryemailplacements4
                , signatorypersonalnumberplacements = signatorypersonalnumberplacements4
                , signatorycompanynumberplacements = signatorycompanynumberplacements4
                , signatoryotherfields = signatoryotherfields4
                }

instance Migrate SignatoryLink0 SignatoryLink1 where
    migrate (SignatoryLink0 
          { signatorylinkid0
          , signatoryname0
          , signatorycompany0
          , signatoryemail0
          , maybesignatory0
          , maybesigninfo0
          , maybeseentime0
          }) = SignatoryLink1 
          { signatorylinkid1 = signatorylinkid0
          , signatorydetails1 = SignatoryDetails 
                               { signatoryfstname = signatoryname0
                               , signatorysndname = BS.empty
                               , signatorycompany = signatorycompany0
                               , signatorypersonalnumber = BS.empty
                               , signatorycompanynumber = BS.empty
                               , signatoryemail = signatoryemail0
                               , signatoryfstnameplacements = []
                               , signatorysndnameplacements = []
                               , signatorycompanyplacements = []
                               , signatoryemailplacements = []
                               , signatorypersonalnumberplacements = []
                               , signatorycompanynumberplacements = []
                               , signatoryotherfields = []
                               , signatorysignorder = SignOrder 1
                               }
          , maybesignatory1 = maybesignatory0
          , maybesigninfo1 = maybesigninfo0
          , maybeseentime1 = maybeseentime0
          }

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

instance Migrate SignatoryLink8 SignatoryLink where
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
             }) = SignatoryLink
                { signatorylinkid           = signatorylinkid8
                , signatorydetails          = signatorydetails8
                , signatorymagichash        = signatorymagichash8
                , maybesignatory            = maybesignatory8
                , maybesupervisor           = Nothing
                , maybesigninfo             = maybesigninfo8
                , maybeseeninfo             = maybeseeninfo8
                , maybereadinvite           = maybereadinvite8
                , invitationdeliverystatus  = invitationdeliverystatus8
                , signatorysignatureinfo    = signatorysignatureinfo8
                , signatorylinkdeleted      = signatorylinkdeleted8
                , signatoryroles            = signatoryroles8
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

$(deriveSerialize ''DocumentHistoryEntry0)
instance Version DocumentHistoryEntry0

$(deriveSerialize ''DocumentHistoryEntry)
instance Version DocumentHistoryEntry where
    mode = extension 1 (Proxy :: Proxy DocumentHistoryEntry0)

$(deriveSerialize ''CSVUpload)
instance Version CSVUpload



$(deriveSerialize ''Document11)
instance Version Document11 where
    mode = extension 11 (Proxy :: Proxy ())

$(deriveSerialize ''Document12)
instance Version Document12 where
    mode = extension 12 (Proxy :: Proxy Document11)

$(deriveSerialize ''Document13)
instance Version Document13 where
    mode = extension 13 (Proxy :: Proxy Document12)
    
$(deriveSerialize ''Document14)
instance Version Document14 where
    mode = extension 14 (Proxy :: Proxy Document13)

$(deriveSerialize ''Document15)
instance Version Document15 where
    mode = extension 15 (Proxy :: Proxy Document14)

$(deriveSerialize ''Document16)
instance Version Document16 where
    mode = extension 16 (Proxy :: Proxy Document15)

$(deriveSerialize ''Document17)
instance Version Document17 where
    mode = extension 17 (Proxy :: Proxy Document16)

$(deriveSerialize ''Document18)
instance Version Document18 where
    mode = extension 18 (Proxy :: Proxy Document17)

$(deriveSerialize ''Document19)
instance Version Document19 where
    mode = extension 19 (Proxy :: Proxy Document18)

$(deriveSerialize ''Document20)
instance Version Document20 where
    mode = extension 20 (Proxy :: Proxy Document19)

$(deriveSerialize ''Document21)
instance Version Document21 where
    mode = extension 21 (Proxy :: Proxy Document20)

$(deriveSerialize ''Document22)
instance Version Document22 where
    mode = extension 22 (Proxy :: Proxy Document21)

$(deriveSerialize ''Document23)
instance Version Document23 where
    mode = extension 23 (Proxy :: Proxy Document22)

$(deriveSerialize ''Document24)
instance Version Document24 where
    mode = extension 24 (Proxy :: Proxy Document23)

$(deriveSerialize ''Document25)
instance Version Document25 where
    mode = extension 25 (Proxy :: Proxy Document24)

$(deriveSerialize ''Document)
instance Version Document where
    mode = extension 26 (Proxy :: Proxy Document25)

instance Migrate DocumentHistoryEntry0 DocumentHistoryEntry where
        migrate (DocumentHistoryCreated0 { dochisttime0 }) = 
            DocumentHistoryCreated dochisttime0
        migrate (DocumentHistoryInvitationSent0 { dochisttime0 
                                                , ipnumber0
                                                }) 
            = DocumentHistoryInvitationSent dochisttime0 ipnumber0 []


instance Migrate () Document11 where
    migrate () = error "No way to migrate to Document11"

instance Migrate Document11 Document12 where
    migrate (Document11
                { documentid11
                , documenttitle11
                , documentauthor11
                , documentsignatorylinks11
                , documentfiles11
                , documentsealedfiles11
                , documentstatus11
                , documentctime11
                , documentmtime11
                , documentchargemode11
                , documentdaystosign11
                , documenttimeouttime11
                , documentdeleted11
                , documenthistory11
                , documentinvitetext11
                , documenttrustweaverreference11
                , documentallowedidtypes11
                , authorfstnameplacements11
                , authorsndnameplacements11
                , authorcompanyplacements11
                , authoremailplacements11
                , authornumberplacements11
                , authorotherfields11
                }) = Document12
                { documentid12 = documentid11
                , documenttitle12 = documenttitle11
                , documentauthor12 = documentauthor11
                , documentsignatorylinks12 = documentsignatorylinks11
                , documentfiles12 = documentfiles11
                , documentsealedfiles12 = documentsealedfiles11
                , documentstatus12 = documentstatus11
                , documenttype12 = Contract
                , documentctime12 = documentctime11
                , documentmtime12 = documentmtime11
                , documentchargemode12 = documentchargemode11
                , documentdaystosign12 = documentdaystosign11
                , documenttimeouttime12 = documenttimeouttime11
                , documentdeleted12 = documentdeleted11
                , documenthistory12 = documenthistory11
                , documentinvitetext12 = documentinvitetext11
                , documenttrustweaverreference12 = documenttrustweaverreference11
                , documentallowedidtypes12 = documentallowedidtypes11
                , authorfstnameplacements12 = authorfstnameplacements11
                , authorsndnameplacements12 = authorsndnameplacements11
                , authorcompanyplacements12 = authorcompanyplacements11
                , authoremailplacements12 =  authoremailplacements11
                , authornumberplacements12 =  authornumberplacements11
                , authorotherfields12 = authorotherfields11
                }
                
instance Migrate Document12 Document13 where
    migrate (Document12
                { documentid12
                , documenttitle12
                , documentauthor12
                , documentsignatorylinks12
                , documentfiles12
                , documentsealedfiles12
                , documentstatus12
                , documenttype12
                , documentctime12
                , documentmtime12
                , documentchargemode12
                , documentdaystosign12
                , documenttimeouttime12
                , documentdeleted12
                , documenthistory12
                , documentinvitetext12
                , documenttrustweaverreference12
                , documentallowedidtypes12
                , authorfstnameplacements12
                , authorsndnameplacements12
                , authorcompanyplacements12
                , authoremailplacements12
                , authornumberplacements12
                , authorotherfields12
                }) = Document13
                { documentid13 = documentid12
                , documenttitle13 = documenttitle12
                , documentauthor13 = documentauthor12
                , documentsignatorylinks13 = documentsignatorylinks12
                , documentfiles13 = documentfiles12
                , documentsealedfiles13 = documentsealedfiles12
                , documentstatus13 = documentstatus12
                , documenttype13 = documenttype12
                , documentctime13 = documentctime12
                , documentmtime13 = documentmtime12
                , documentchargemode13 = documentchargemode12
                , documentdaystosign13 = documentdaystosign12
                , documenttimeouttime13 = documenttimeouttime12
                , documentdeleted13 = documentdeleted12
                , documenthistory13 = documenthistory12
                , documentinvitetext13 = documentinvitetext12
                , documenttrustweaverreference13 = documenttrustweaverreference12
                , documentallowedidtypes13 = documentallowedidtypes12
                , authorfstnameplacements13 = authorfstnameplacements12
                , authorsndnameplacements13 = authorsndnameplacements12
                , authorcompanyplacements13 = authorcompanyplacements12
                , authoremailplacements13 = authoremailplacements12
                , authornumberplacements13 = authornumberplacements12
                , authorotherfields13 = authorotherfields12
                , documentcancelationreason13 = if documentstatus12 == Canceled
                                                then Just ManualCancel
                                                else Nothing
                }

instance Migrate Document13 Document14 where
    migrate (Document13
                { documentid13
                , documenttitle13
                , documentauthor13
                , documentsignatorylinks13
                , documentfiles13
                , documentsealedfiles13
                , documentstatus13
                , documenttype13
                , documentctime13
                , documentmtime13
                , documentchargemode13
                , documentdaystosign13
                , documenttimeouttime13
                , documentdeleted13
                , documenthistory13
                , documentinvitetext13
                , documenttrustweaverreference13
                , documentallowedidtypes13
                , authorfstnameplacements13
                , authorsndnameplacements13
                , authorcompanyplacements13
                , authoremailplacements13
                , authornumberplacements13
                , authorotherfields13
                , documentcancelationreason13
                }) = Document14
                { documentid14                   = documentid13
                , documenttitle14                = documenttitle13
                , documentauthor14               = documentauthor13
                , documentsignatorylinks14       = documentsignatorylinks13
                , documentfiles14                = documentfiles13
                , documentsealedfiles14          = documentsealedfiles13
                , documentstatus14               = documentstatus13
                , documenttype14                 = documenttype13
                , documentctime14                = documentctime13
                , documentmtime14                = documentmtime13
                , documentchargemode14           = documentchargemode13
                , documentdaystosign14           = documentdaystosign13
                , documenttimeouttime14          = documenttimeouttime13
                , documentdeleted14              = documentdeleted13
                , documenthistory14              = documenthistory13
                , documentinvitetext14           = documentinvitetext13
                , documenttrustweaverreference14 = documenttrustweaverreference13
                , documentallowedidtypes14       = documentallowedidtypes13
                , documentcsvupload14            = Nothing
                , authorfstnameplacements14      = authorfstnameplacements13
                , authorsndnameplacements14      = authorsndnameplacements13
                , authorcompanyplacements14      = authorcompanyplacements13
                , authoremailplacements14        = authoremailplacements13
                , authornumberplacements14       = authornumberplacements13
                , authorotherfields14            = authorotherfields13
                , documentcancelationreason14    = documentcancelationreason13
                }

instance Migrate Document14 Document15 where
    migrate (Document14
                { documentid14
                , documenttitle14
                , documentauthor14
                , documentsignatorylinks14
                , documentfiles14
                , documentsealedfiles14
                , documentstatus14
                , documenttype14
                , documentctime14
                , documentmtime14
                , documentchargemode14 = _
                , documentdaystosign14
                , documenttimeouttime14
                , documentdeleted14
                , documenthistory14
                , documentinvitetext14
                , documenttrustweaverreference14
                , documentallowedidtypes14
                , documentcsvupload14
                , authorfstnameplacements14
                , authorsndnameplacements14
                , authorcompanyplacements14
                , authoremailplacements14
                , authornumberplacements14
                , authorotherfields14
                , documentcancelationreason14
                }) = Document15
                { documentid15                   = documentid14
                , documenttitle15                = documenttitle14
                , documentauthor15               = documentauthor14
                , documentsignatorylinks15       = documentsignatorylinks14
                , documentfiles15                = documentfiles14
                , documentsealedfiles15          = documentsealedfiles14
                , documentstatus15               = documentstatus14
                , documenttype15                 = documenttype14
                , documentctime15                = documentctime14
                , documentmtime15                = documentmtime14
                , documentdaystosign15           = documentdaystosign14
                , documenttimeouttime15          = documenttimeouttime14
                -- here we see if there is a time of sending invitations in the history of a document
                , documentinvitetime15           = msum $ map (\x -> case x of
                                                                   DocumentHistoryInvitationSent time ipnumber _ -> Just (SignInfo time ipnumber)
                                                                   _ -> Nothing) documenthistory14
                , documentdeleted15              = documentdeleted14
                , documentlog15                  = map documentHistoryToDocumentLog documenthistory14
                , documentinvitetext15           = documentinvitetext14
                , documenttrustweaverreference15 = documenttrustweaverreference14
                , documentallowedidtypes15       = documentallowedidtypes14
                , documentcsvupload15            = documentcsvupload14
                , authorfstnameplacements15      = authorfstnameplacements14
                , authorsndnameplacements15      = authorsndnameplacements14
                , authorcompanyplacements15      = authorcompanyplacements14
                , authoremailplacements15        = authoremailplacements14
                , authornumberplacements15       = authornumberplacements14
                , authorotherfields15            = authorotherfields14
                , documentcancelationreason15    = documentcancelationreason14
                }

instance Migrate Document15 Document16 where
    migrate (Document15
                { documentid15
                , documenttitle15
                , documentauthor15
                , documentsignatorylinks15
                , documentfiles15
                , documentsealedfiles15
                , documentstatus15
                , documenttype15
                , documentctime15
                , documentmtime15
                , documentdaystosign15
                , documenttimeouttime15
                , documentinvitetime15
                , documentdeleted15
                , documentlog15
                , documentinvitetext15
                , documenttrustweaverreference15
                , documentallowedidtypes15
                , documentcsvupload15
                , authorfstnameplacements15
                , authorsndnameplacements15
                , authorcompanyplacements15
                , authoremailplacements15
                , authornumberplacements15
                , authorotherfields15
                , documentcancelationreason15
                }) = Document16
                { documentid16                     = documentid15
                , documenttitle16                  = documenttitle15
                , documentauthor16                 = documentauthor15
                , documentsignatorylinks16         = documentsignatorylinks15
                , documentfiles16                  = documentfiles15
                , documentsealedfiles16            = documentsealedfiles15
                , documentstatus16                 = documentstatus15
                , documenttype16                   = documenttype15
                , documentctime16                  = documentctime15
                , documentmtime16                  = documentmtime15
                , documentdaystosign16             = documentdaystosign15
                , documenttimeouttime16            = documenttimeouttime15
                , documentinvitetime16             = documentinvitetime15
                , documentdeleted16                = documentdeleted15
                , documentlog16                    = documentlog15
                , documentinvitetext16             = documentinvitetext15
                , documenttrustweaverreference16   = documenttrustweaverreference15
                , documentallowedidtypes16         = documentallowedidtypes15
                , documentcsvupload16              = documentcsvupload15
                , authorfstnameplacements16        = authorfstnameplacements15
                , authorsndnameplacements16        = authorsndnameplacements15
                , authorcompanyplacements16        = authorcompanyplacements15
                , authoremailplacements16          = authoremailplacements15
                , authorpersonalnumberplacements16 = authornumberplacements15
                , authorcompanynumberplacements16  = []
                , authorotherfields16              = authorotherfields15
                , documentcancelationreason16      = documentcancelationreason15
                }

instance Migrate Document16 Document17 where
    migrate (Document16
                { documentid16
                , documenttitle16
                , documentauthor16
                , documentsignatorylinks16
                , documentfiles16
                , documentsealedfiles16
                , documentstatus16
                , documenttype16
                , documentctime16
                , documentmtime16
                , documentdaystosign16
                , documenttimeouttime16
                , documentinvitetime16
                , documentdeleted16
                , documentlog16
                , documentinvitetext16
                , documenttrustweaverreference16
                , documentallowedidtypes16
                , documentcsvupload16
                , authorfstnameplacements16
                , authorsndnameplacements16
                , authorcompanyplacements16
                , authoremailplacements16
                , authorpersonalnumberplacements16
                , authorcompanynumberplacements16
                , authorotherfields16
                , documentcancelationreason16
                }) = Document17
                { documentid17                     = documentid16
                , documenttitle17                  = documenttitle16
                , documentauthor17                 = documentauthor16
                , documentsignatorylinks17         = documentsignatorylinks16
                , documentfiles17                  = documentfiles16
                , documentsealedfiles17            = documentsealedfiles16
                , documentstatus17                 = documentstatus16
                , documenttype17                   = documenttype16
                , documentfunctionality17          = AdvancedFunctionality
                , documentctime17                  = documentctime16
                , documentmtime17                  = documentmtime16
                , documentdaystosign17             = documentdaystosign16
                , documenttimeouttime17            = documenttimeouttime16
                , documentinvitetime17             = documentinvitetime16
                , documentdeleted17                = documentdeleted16
                , documentlog17                    = documentlog16
                , documentinvitetext17             = documentinvitetext16
                , documenttrustweaverreference17   = documenttrustweaverreference16
                , documentallowedidtypes17         = documentallowedidtypes16
                , documentcsvupload17              = documentcsvupload16
                , authorfstnameplacements17        = authorfstnameplacements16
                , authorsndnameplacements17        = authorsndnameplacements16
                , authorcompanyplacements17        = authorcompanyplacements16
                , authoremailplacements17          = authoremailplacements16
                , authorpersonalnumberplacements17 = authorpersonalnumberplacements16
                , authorcompanynumberplacements17  = authorcompanynumberplacements16
                , authorotherfields17              = authorotherfields16
                , documentcancelationreason17      = documentcancelationreason16
                }

instance Migrate Document17 Document18 where
    migrate (Document17
                { documentid17
                , documenttitle17
                , documentauthor17
                , documentsignatorylinks17
                , documentfiles17
                , documentsealedfiles17
                , documentstatus17
                , documenttype17
                , documentfunctionality17
                , documentctime17
                , documentmtime17
                , documentdaystosign17
                , documenttimeouttime17
                , documentinvitetime17
                , documentdeleted17
                , documentlog17
                , documentinvitetext17
                , documenttrustweaverreference17
                , documentallowedidtypes17
                , documentcsvupload17
                , authorfstnameplacements17
                , authorsndnameplacements17
                , authorcompanyplacements17
                , authoremailplacements17
                , authorpersonalnumberplacements17
                , authorcompanynumberplacements17
                , authorotherfields17
                , documentcancelationreason17
                }) = Document18
                { documentid18                     = documentid17
                , documenttitle18                  = documenttitle17
                , documentauthor18                 = documentauthor17
                , documentsignatorylinks18         = documentsignatorylinks17
                , documentfiles18                  = documentfiles17
                , documentsealedfiles18            = documentsealedfiles17
                , documentstatus18                 = documentstatus17
                , documenttype18                   = documenttype17
                , documentfunctionality18          = documentfunctionality17
                , documentctime18                  = documentctime17
                , documentmtime18                  = documentmtime17
                , documentdaystosign18             = documentdaystosign17
                , documenttimeouttime18            = documenttimeouttime17
                , documentinvitetime18             = documentinvitetime17
                , documentdeleted18                = documentdeleted17
                , documentlog18                    = documentlog17
                , documentinvitetext18             = documentinvitetext17
                , documenttrustweaverreference18   = documenttrustweaverreference17
                , documentallowedidtypes18         = documentallowedidtypes17
                , documentcsvupload18              = documentcsvupload17
                , authorfstnameplacements18        = authorfstnameplacements17
                , authorsndnameplacements18        = authorsndnameplacements17
                , authorcompanyplacements18        = authorcompanyplacements17
                , authoremailplacements18          = authoremailplacements17
                , authorpersonalnumberplacements18 = authorpersonalnumberplacements17
                , authorcompanynumberplacements18  = authorcompanynumberplacements17
                , authorotherfields18              = authorotherfields17
                , documentcancelationreason18      = documentcancelationreason17
                , documentsharing18                = Private
                }

instance Migrate Document18 Document19 where
    migrate (Document18
             { documentid18
             , documenttitle18
             , documentauthor18
             , documentsignatorylinks18
             , documentfiles18
             , documentsealedfiles18
             , documentstatus18
             , documenttype18
             , documentfunctionality18
             , documentctime18
             , documentmtime18
             , documentdaystosign18
             , documenttimeouttime18
             , documentinvitetime18
             , documentdeleted18
             , documentlog18
             , documentinvitetext18
             , documenttrustweaverreference18
             , documentallowedidtypes18
             , documentcsvupload18
             , authorfstnameplacements18
             , authorsndnameplacements18
             , authorcompanyplacements18
             , authoremailplacements18
             , authorpersonalnumberplacements18
             , authorcompanynumberplacements18
             , authorotherfields18
             , documentcancelationreason18
             , documentsharing18
             }) = Document19
                { documentid19                     = documentid18
                , documenttitle19                  = documenttitle18
                , documentauthor19                 = documentauthor18
                , documentsignatorylinks19         = documentsignatorylinks18
                , documentfiles19                  = documentfiles18
                , documentsealedfiles19            = documentsealedfiles18
                , documentstatus19                 = documentstatus18
                , documenttype19                   = documenttype18
                , documentfunctionality19          = documentfunctionality18
                , documentctime19                  = documentctime18
                , documentmtime19                  = documentmtime18
                , documentdaystosign19             = documentdaystosign18
                , documenttimeouttime19            = documenttimeouttime18
                , documentinvitetime19             = documentinvitetime18
                , documentdeleted19                = documentdeleted18
                , documentlog19                    = documentlog18
                , documentinvitetext19             = documentinvitetext18
                , documenttrustweaverreference19   = documenttrustweaverreference18
                , documentallowedidtypes19         = documentallowedidtypes18
                , documentcsvupload19              = documentcsvupload18
                , authorfstnameplacements19        = authorfstnameplacements18
                , authorsndnameplacements19        = authorsndnameplacements18
                , authorcompanyplacements19        = authorcompanyplacements18
                , authoremailplacements19          = authoremailplacements18
                , authorpersonalnumberplacements19 = authorpersonalnumberplacements18
                , authorcompanynumberplacements19  = authorcompanynumberplacements18
                , authorotherfields19              = authorotherfields18
                , documentcancelationreason19      = documentcancelationreason18
                , documentsharing19                = documentsharing18
                , documentrejectioninfo19          = Nothing
                }             

instance Migrate Document19 Document20 where
    migrate (Document19
             { documentid19
             , documenttitle19
             , documentauthor19
             , documentsignatorylinks19
             , documentfiles19
             , documentsealedfiles19
             , documentstatus19
             , documenttype19
             , documentfunctionality19
             , documentctime19
             , documentmtime19
             , documentdaystosign19
             , documenttimeouttime19
             , documentinvitetime19
             , documentdeleted19
             , documentlog19
             , documentinvitetext19
             , documenttrustweaverreference19
             , documentallowedidtypes19
             , documentcsvupload19
             , authorfstnameplacements19
             , authorsndnameplacements19
             , authorcompanyplacements19
             , authoremailplacements19
             , authorpersonalnumberplacements19
             , authorcompanynumberplacements19
             , authorotherfields19
             , documentcancelationreason19
             , documentsharing19
             , documentrejectioninfo19
             }) = Document20
                { documentid20                     = documentid19
                , documenttitle20                  = documenttitle19
                , documentauthor20                 = documentauthor19
                , documentsignatorylinks20         = documentsignatorylinks19
                , documentfiles20                  = documentfiles19
                , documentsealedfiles20            = documentsealedfiles19
                , documentstatus20                 = documentstatus19
                , documenttype20                   = documenttype19
                , documentfunctionality20          = documentfunctionality19
                , documentctime20                  = documentctime19
                , documentmtime20                  = documentmtime19
                , documentdaystosign20             = documentdaystosign19
                , documenttimeouttime20            = documenttimeouttime19
                , documentinvitetime20             = documentinvitetime19
                , documentdeleted20                = documentdeleted19
                , documentlog20                    = documentlog19
                , documentinvitetext20             = documentinvitetext19
                , documenttrustweaverreference20   = documenttrustweaverreference19
                , documentallowedidtypes20         = documentallowedidtypes19
                , documentcsvupload20              = documentcsvupload19
                , authorfstnameplacements20        = authorfstnameplacements19
                , authorsndnameplacements20        = authorsndnameplacements19
                , authorcompanyplacements20        = authorcompanyplacements19
                , authoremailplacements20          = authoremailplacements19
                , authorpersonalnumberplacements20 = authorpersonalnumberplacements19
                , authorcompanynumberplacements20  = authorcompanynumberplacements19
                , authorotherfields20              = authorotherfields19
                , documentcancelationreason20      = documentcancelationreason19
                , documentsharing20                = documentsharing19
                , documentrejectioninfo20          = documentrejectioninfo19
                , documenttags20                   = []
                }             


instance Migrate Document20 Document21 where
    migrate (Document20
             { documentid20
             , documenttitle20
             , documentauthor20
             , documentsignatorylinks20
             , documentfiles20
             , documentsealedfiles20
             , documentstatus20
             , documenttype20
             , documentfunctionality20
             , documentctime20
             , documentmtime20
             , documentdaystosign20
             , documenttimeouttime20
             , documentinvitetime20
             , documentdeleted20
             , documentlog20
             , documentinvitetext20
             , documenttrustweaverreference20
             , documentallowedidtypes20
             , documentcsvupload20
             , authorfstnameplacements20
             , authorsndnameplacements20
             , authorcompanyplacements20
             , authoremailplacements20
             , authorpersonalnumberplacements20
             , authorcompanynumberplacements20
             , authorotherfields20
             , documentcancelationreason20
             , documentsharing20
             , documentrejectioninfo20
             , documenttags20   
             }) = Document21
                { documentid21                     = documentid20
                , documenttitle21                  = documenttitle20
                , documentauthor21                 = documentauthor20
                , documentsignatorylinks21         = documentsignatorylinks20
                , documentfiles21                  = documentfiles20
                , documentsealedfiles21            = documentsealedfiles20
                , documentstatus21                 = documentstatus20
                , documenttype21                   = documenttype20
                , documentfunctionality21          = documentfunctionality20
                , documentctime21                  = documentctime20
                , documentmtime21                  = documentmtime20
                , documentdaystosign21             = documentdaystosign20
                , documenttimeouttime21            = documenttimeouttime20
                , documentinvitetime21             = documentinvitetime20
                , documentdeleted21                = documentdeleted20
                , documentlog21                    = documentlog20
                , documentinvitetext21             = documentinvitetext20
                , documenttrustweaverreference21   = documenttrustweaverreference20
                , documentallowedidtypes21         = documentallowedidtypes20
                , documentcsvupload21              = documentcsvupload20
                , authorfstnameplacements21        = authorfstnameplacements20
                , authorsndnameplacements21        = authorsndnameplacements20
                , authorcompanyplacements21        = authorcompanyplacements20
                , authoremailplacements21          = authoremailplacements20
                , authorpersonalnumberplacements21 = authorpersonalnumberplacements20
                , authorcompanynumberplacements21  = authorcompanynumberplacements20
                , authorotherfields21              = authorotherfields20
                , documentcancelationreason21      = documentcancelationreason20
                , documentsharing21                = documentsharing20
                , documentrejectioninfo21          = documentrejectioninfo20
                , documenttags21                   = documenttags20   
                , documentservice21                = Nothing
                }        


instance Migrate Document21 Document22 where
    migrate (Document21
             { documentid21
             , documenttitle21
             , documentauthor21
             , documentsignatorylinks21
             , documentfiles21
             , documentsealedfiles21
             , documentstatus21
             , documenttype21
             , documentfunctionality21
             , documentctime21
             , documentmtime21
             , documentdaystosign21
             , documenttimeouttime21
             , documentinvitetime21
             , documentdeleted21
             , documentlog21
             , documentinvitetext21
             , documenttrustweaverreference21
             , documentallowedidtypes21
             , documentcsvupload21
             , authorfstnameplacements21
             , authorsndnameplacements21
             , authorcompanyplacements21
             , authoremailplacements21
             , authorpersonalnumberplacements21
             , authorcompanynumberplacements21
             , authorotherfields21
             , documentcancelationreason21
             , documentsharing21
             , documentrejectioninfo21
             , documenttags21
             , documentservice21
             }) = Document22
                { documentid22                     = documentid21
                , documenttitle22                  = documenttitle21
                , documentauthor22                 = documentauthor21
                , documentsignatorylinks22         = documentsignatorylinks21
                , documentfiles22                  = documentfiles21
                , documentsealedfiles22            = documentsealedfiles21
                , documentstatus22                 = documentstatus21
                , documenttype22                   = documenttype21
                , documentfunctionality22          = documentfunctionality21
                , documentctime22                  = documentctime21
                , documentmtime22                  = documentmtime21
                , documentdaystosign22             = documentdaystosign21
                , documenttimeouttime22            = documenttimeouttime21
                , documentinvitetime22             = documentinvitetime21
                , documentdeleted22                = documentdeleted21
                , documentlog22                    = documentlog21
                , documentinvitetext22             = documentinvitetext21
                , documenttrustweaverreference22   = documenttrustweaverreference21
                , documentallowedidtypes22         = documentallowedidtypes21
                , documentcsvupload22              = documentcsvupload21
                , authorfstnameplacements22        = authorfstnameplacements21
                , authorsndnameplacements22        = authorsndnameplacements21
                , authorcompanyplacements22        = authorcompanyplacements21
                , authoremailplacements22          = authoremailplacements21
                , authorpersonalnumberplacements22 = authorpersonalnumberplacements21
                , authorcompanynumberplacements22  = authorcompanynumberplacements21
                , authorotherfields22              = authorotherfields21
                , documentcancelationreason22      = documentcancelationreason21
                , documentsharing22                = documentsharing21
                , documentrejectioninfo22          = documentrejectioninfo21
                , documenttags22                   = documenttags21   
                , documentservice22                = documentservice21
                , documentattachments22            = []
                }

instance Migrate Document22 Document23 where
    migrate (Document22
             { documentid22
             , documenttitle22
             , documentsignatorylinks22
             , documentfiles22
             , documentsealedfiles22
             , documentstatus22
             , documenttype22
             , documentfunctionality22
             , documentctime22
             , documentmtime22
             , documentdaystosign22
             , documenttimeouttime22
             , documentinvitetime22
             , documentlog22
             , documentinvitetext22
             , documenttrustweaverreference22
             , documentallowedidtypes22
             , documentcsvupload22
             , documentcancelationreason22
             , documentsharing22
             , documentrejectioninfo22
             , documenttags22
             , documentservice22
             , documentattachments22
             }) = Document23
                { documentid23                     = documentid22
                , documenttitle23                  = documenttitle22
                , documentsignatorylinks23         = documentsignatorylinks22
                , documentfiles23                  = documentfiles22
                , documentsealedfiles23            = documentsealedfiles22
                , documentstatus23                 = documentstatus22
                , documenttype23                   = documenttype22
                , documentfunctionality23          = documentfunctionality22
                , documentctime23                  = documentctime22
                , documentmtime23                  = documentmtime22
                , documentdaystosign23             = documentdaystosign22
                , documenttimeouttime23            = documenttimeouttime22
                , documentinvitetime23             = documentinvitetime22
                , documentlog23                    = documentlog22
                , documentinvitetext23             = documentinvitetext22
                , documenttrustweaverreference23   = documenttrustweaverreference22
                , documentallowedidtypes23         = documentallowedidtypes22
                , documentcsvupload23              = documentcsvupload22
                , documentcancelationreason23      = documentcancelationreason22
                , documentsharing23                = documentsharing22
                , documentrejectioninfo23          = documentrejectioninfo22
                , documenttags23                   = documenttags22   
                , documentservice23                = documentservice22
                , documentattachments23            = documentattachments22
                }


instance Migrate Document23 Document24 where
    migrate (Document23
             { documentid23
             , documenttitle23
             , documentsignatorylinks23
             , documentfiles23
             , documentsealedfiles23
             , documentstatus23
             , documenttype23
             , documentfunctionality23
             , documentctime23
             , documentmtime23
             , documentdaystosign23
             , documenttimeouttime23
             , documentinvitetime23
             , documentlog23
             , documentinvitetext23
             , documenttrustweaverreference23
             , documentallowedidtypes23
             , documentcsvupload23
             , documentcancelationreason23
             , documentsharing23
             , documentrejectioninfo23
             , documenttags23
             , documentservice23
             , documentattachments23
             }) = Document24
                { documentid24                     = documentid23
                , documenttitle24                  = documenttitle23
                , documentsignatorylinks24         = documentsignatorylinks23
                , documentfiles24                  = documentfiles23
                , documentsealedfiles24            = documentsealedfiles23
                , documentstatus24                 = documentstatus23
                , documenttype24                   = documenttype23
                , documentfunctionality24          = documentfunctionality23
                , documentctime24                  = documentctime23
                , documentmtime24                  = documentmtime23
                , documentdaystosign24             = documentdaystosign23
                , documenttimeouttime24            = documenttimeouttime23
                , documentinvitetime24             = documentinvitetime23
                , documentlog24                    = documentlog23
                , documentinvitetext24             = documentinvitetext23
                , documenttrustweaverreference24   = documenttrustweaverreference23
                , documentallowedidtypes24         = documentallowedidtypes23
                , documentcsvupload24              = documentcsvupload23
                , documentcancelationreason24      = documentcancelationreason23
                , documentsharing24                = documentsharing23
                , documentrejectioninfo24          = documentrejectioninfo23
                , documenttags24                   = documenttags23  
                , documentservice24                = documentservice23
                , documentattachments24            = documentattachments23
                , documentoriginalcompany24        = Nothing
                }

instance Migrate Document24 Document25 where
    migrate (Document24
             { documentid24
             , documenttitle24
             , documentsignatorylinks24
             , documentfiles24
             , documentsealedfiles24
             , documentstatus24
             , documenttype24
             , documentfunctionality24
             , documentctime24
             , documentmtime24
             , documentdaystosign24
             , documenttimeouttime24
             , documentinvitetime24
             , documentlog24
             , documentinvitetext24
             , documenttrustweaverreference24
             , documentallowedidtypes24
             , documentcsvupload24
             , documentcancelationreason24
             , documentsharing24
             , documentrejectioninfo24
             , documenttags24
             , documentservice24
             , documentoriginalcompany24
             , documentattachments24
             }) = Document25
                { documentid25                     = documentid24
                , documenttitle25                  = documenttitle24
                , documentsignatorylinks25         = documentsignatorylinks24
                , documentfiles25                  = documentfiles24
                , documentsealedfiles25            = documentsealedfiles24
                , documentstatus25                 = documentstatus24
                , documenttype25                   = documenttype24
                , documentfunctionality25          = documentfunctionality24
                , documentctime25                  = documentctime24
                , documentmtime25                  = documentmtime24
                , documentdaystosign25             = documentdaystosign24
                , documenttimeouttime25            = documenttimeouttime24
                , documentinvitetime25             = documentinvitetime24
                , documentlog25                    = documentlog24
                , documentinvitetext25             = documentinvitetext24
                , documenttrustweaverreference25   = documenttrustweaverreference24
                , documentallowedidtypes25         = documentallowedidtypes24
                , documentcsvupload25              = documentcsvupload24
                , documentcancelationreason25      = documentcancelationreason24
                , documentsharing25                = documentsharing24
                , documentrejectioninfo25          = documentrejectioninfo24
                , documenttags25                   = documenttags24
                , documentservice25                = documentservice24
                , documentoriginalcompany25        = documentoriginalcompany24
                , documentrecordstatus25           = LiveDocument
                , documentquarantineexpiry25       = Nothing
                , documentattachments25            = documentattachments24
                }
                

instance Migrate Document25 Document where
    migrate (Document25
             { documentid25
             , documenttitle25
             , documentsignatorylinks25
             , documentfiles25
             , documentsealedfiles25
             , documentstatus25
             , documenttype25
             , documentfunctionality25
             , documentctime25
             , documentmtime25
             , documentdaystosign25
             , documenttimeouttime25
             , documentinvitetime25
             , documentlog25
             , documentinvitetext25
             , documenttrustweaverreference25
             , documentallowedidtypes25
             , documentcsvupload25
             , documentcancelationreason25
             , documentsharing25
             , documentrejectioninfo25
             , documenttags25
             , documentservice25
             , documentoriginalcompany25
             , documentattachments25
             , documentrecordstatus25
             , documentquarantineexpiry25
             }) = Document
                { documentid                     = documentid25
                , documenttitle                  = documenttitle25
                , documentsignatorylinks         = documentsignatorylinks25
                , documentfiles                  = documentfiles25
                , documentsealedfiles            = documentsealedfiles25
                , documentstatus                 = documentstatus25
                , documenttype                   = documenttype25
                , documentfunctionality          = documentfunctionality25
                , documentctime                  = documentctime25
                , documentmtime                  = documentmtime25
                , documentdaystosign             = documentdaystosign25
                , documenttimeouttime            = documenttimeouttime25
                , documentinvitetime             = documentinvitetime25
                , documentlog                    = documentlog25
                , documentinvitetext             = documentinvitetext25
                , documenttrustweaverreference   = documenttrustweaverreference25
                , documentallowedidtypes         = documentallowedidtypes25
                , documentcsvupload              = documentcsvupload25
                , documentcancelationreason      = documentcancelationreason25
                , documentsharing                = documentsharing25
                , documentrejectioninfo          = documentrejectioninfo25
                , documenttags                   = documenttags25
                , documentservice                = documentservice25
                , documentoriginalcompany        = documentoriginalcompany25
                , documentattachments            = documentattachments25
                , documentrecordstatus           = documentrecordstatus25
                , documentquarantineexpiry       = documentquarantineexpiry25
                , documentauthorattachments      = []
                , documentsignatoryattachments   = []
                }


$(deriveSerialize ''DocumentStatus)
instance Version DocumentStatus where

$(deriveSerialize ''DocumentType)
instance Version DocumentType where

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

$(deriveSerialize ''DocStats)
instance Version DocStats where

$(deriveSerialize ''File0)
instance Version File0 where

instance Migrate File0 File1 where
    migrate (File0 
             { fileid0
             , filename0
             , filepdf0
             , filejpgpages0
             }) = File1 
                { fileid1 = fileid0
                , filename1 = filename0
                , filepdf1 = filepdf0
                , filejpgpages1 = JpegPages $ map (\x -> (x,1000,1000)) filejpgpages0
                }

$(deriveSerialize ''File1)
instance Version File1 where
    mode = extension 1 (Proxy :: Proxy File0)

instance Migrate File1 File2 where
    migrate (File1 
                { fileid1
                , filename1
                , filepdf1
                }) = File2 
                { fileid2 = fileid1
                , filename2 = filename1
                , filepdf2 = filepdf1
                }

$(deriveSerialize ''File2)
instance Version File2 where
    mode = extension 2 (Proxy :: Proxy File1)

instance Migrate File2 File where
    migrate (File2
                { fileid2
                , filename2
                , filepdf2
                }) = File 
                { fileid = fileid2
                , filename = filename2
                , filestorage = FileStorageMemory filepdf2
                }

$(deriveSerialize ''File)
instance Version File where
    mode = extension 3 (Proxy :: Proxy File2)

$(deriveSerialize ''FileStorage)
instance Version FileStorage where

$(deriveSerialize ''JpegPages0)
instance Version JpegPages0 where

$(deriveSerialize ''JpegPages)
instance Version JpegPages where
    mode = extension 1 (Proxy :: Proxy JpegPages0)

instance Migrate JpegPages0 JpegPages where
    migrate JpegPagesPending0 = JpegPagesPending
    migrate (JpegPagesError0 x) = JpegPagesError x
    migrate (JpegPages0 l) = JpegPages $ map (\x -> (x,943,1335)) l

$(deriveSerialize ''FileID)
instance Version FileID where

type Documents = IxSet Document

{- |
   Get the author's signatory link.
 -}
getAuthorSigLink :: Document -> Maybe SignatoryLink
getAuthorSigLink = find (elem SignatoryAuthor . signatoryroles) . documentsignatorylinks

instance Indexable Document where
        empty = ixSet [ ixFun (\x -> [documentid x] :: [DocumentID])
                      , ixFun (\x -> (map Signatory (catMaybes (map maybesignatory (documentsignatorylinks x)))) :: [Signatory])
                              
                      -- wait, wait, wait: the following is wrong, signatory link ids are valid only in 
                      -- the scope of a single document! FIXME
                      , ixFun (\x -> map signatorylinkid (documentsignatorylinks x) :: [SignatoryLinkID])
                      , ixFun (\x -> map fileid (documentfiles x 
                                                 ++ documentsealedfiles x
                                                 ++ map authorattachmentfile (documentauthorattachments x)
                                                 ++ [f | SignatoryAttachment{signatoryattachmentfile = Just f} <- (documentsignatoryattachments x)]) :: [FileID])
                      , ixFun (\x -> (case documenttimeouttime x of
                                         Just time -> [time]
                                         Nothing -> []) :: [TimeoutTime])
                      , ixFun (\x -> [documenttype x] :: [DocumentType])
                      , ixFun (\x -> [documentrecordstatus x] :: [DocumentRecordStatus])
                      , ixFun (\x -> documenttags x :: [DocumentTag])
                      , ixFun (\x -> [documentservice x] :: [Maybe ServiceID])
                      , ixFun (\x -> [documentoriginalcompany x] :: [Maybe CompanyID])
                      , ixFun (\x -> [userid | siglink <- documentsignatorylinks x
                                             , not (signatorylinkdeleted siglink)
                                             , Just userid <- [maybesignatory siglink]
                                             ] :: [UserID])
                      , ixFun (\x ->
                          case getAuthorSigLink x of
                               Just asl ->
                                   case maybesignatory asl of
                                        Just uid -> if not (signatorylinkdeleted asl) 
                                                    then [Author uid]
                                                    else []
                                        Nothing -> []
                               Nothing -> [])
                      ]
                                                    
instance Component Documents where
  type Dependencies Documents = End
  initialValue = empty

$(deriveSerialize ''SignatoryRole)


