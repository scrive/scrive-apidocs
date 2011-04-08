{-# OPTIONS_GHC -Wall #-}
module Doc.DocStateData(
      Author(..)
    , CancelationReason(..)
    , ChargeMode(..)
    , Document(..)
    , DocumentHistoryEntry(..)
    , DocumentLogEntry(..)
    , DocumentID(..)
    , DocumentStatus(..)
    , DocumentType(..)
    , Documents
    , CSVUpload(..)
    , FieldDefinition(..)
    , FieldPlacement(..)
    , File(..)
    , FileID(..)
    , FileStorage(..)
    , JpegPages(..)
    , SignInfo(..)
    , Signatory(..)
    , SignatoryDetails(..)
    , SignatoryLink(..)
    , SignatoryLinkID(..)
    , TimeoutTime(..)
    , DocStats(..)
    , SignatureInfo(..)
    , IdentificationType(..)
    , SignatureProvider(..)
    , SignatoryRole(..)

    , documentHistoryToDocumentLog
    ) where
import Happstack.Data
import Happstack.State
import User.UserState
import Happstack.Data.IxSet as IxSet
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Misc
import MinutesTime
import Data.Word
import Data.Int
import Data.Maybe
import Mails.MailsUtil
import Data.Data (Data)
import Control.Monad
import Data.Bits

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

data SignatoryDetails = SignatoryDetails
    { signatoryfstname        :: BS.ByteString  -- "Gracjan" 
    , signatorysndname        :: BS.ByteString  -- "Polak" 
    , signatorycompany        :: BS.ByteString  -- SkrivaPå
    , signatorypersonalnumber :: BS.ByteString  -- 123456789
    , signatorycompanynumber  :: BS.ByteString  -- 123456789
    , signatoryemail          :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
    -- for templates
    , signatoryfstnameplacements :: [FieldPlacement]
    , signatorysndnameplacements :: [FieldPlacement]
    , signatorycompanyplacements :: [FieldPlacement]
    , signatoryemailplacements :: [FieldPlacement]
    , signatorypersonalnumberplacements :: [FieldPlacement]
    , signatorycompanynumberplacements :: [FieldPlacement]
    , signatoryotherfields :: [FieldDefinition]
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

data SignatoryLink = SignatoryLink 
    { signatorylinkid          :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
    , signatorydetails         :: SignatoryDetails    -- ^ details of this person as filled in invitation
    , signatorymagichash       :: MagicHash           -- ^ authentication code
    , maybesignatory           :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
    , maybesigninfo            :: Maybe SignInfo      -- ^ when a person has signed this document
    , maybeseeninfo            :: Maybe SignInfo      -- ^ when a person has first seen this document
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
                    
data DocumentType = Contract | Template
    deriving (Eq, Ord, Typeable)

data ChargeMode = ChargeInitialFree   -- initial 5 documents are free
                | ChargeNormal        -- value times number of people involved

    deriving (Eq, Ord, Typeable)

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

documentHistoryToDocumentLog DocumentHistoryCreated 
      { dochisttime
      } = DocumentLogEntry dochisttime $ BS.fromString "Document created"
documentHistoryToDocumentLog DocumentHistoryInvitationSent 
      { dochisttime
      , ipnumber
      , dochistsignatories
      } = DocumentLogEntry dochisttime $ BS.fromString $ "Invitations sent to signatories" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryTimedOut
      { dochisttime
      } = DocumentLogEntry dochisttime $ BS.fromString "Document timed out"
documentHistoryToDocumentLog DocumentHistorySigned
      { dochisttime
      , ipnumber
      , dochistsignatorydetails
      } = DocumentLogEntry dochisttime $ BS.fromString $ "Document signed by a signatory" ++ formatIP ipnumber
documentHistoryToDocumentLog DocumentHistoryRejected
      { dochisttime
      , ipnumber
      , dochistsignatorydetails
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

data DocStats = DocStats {
                 doccount :: Int
               , signaturecount :: Int
             }
    deriving (Eq, Ord, Typeable, Data) -- Data instance used for View modules (quite incorrectly there, please remove ASAP)

 
data Document0 = Document0
    { documentid0               :: DocumentID
    , documenttitle0            :: BS.ByteString
    , documentauthor0           :: Author
    , documentsignatorylinks0   :: [SignatoryLink]  
    , documentfiles0            :: [File]
    , documentstatus0           :: DocumentStatus
    , documentctime0            :: MinutesTime
    , documentmtime0            :: MinutesTime
    }
    deriving (Eq, Ord, Typeable)

data Document1 = Document1
    { documentid1               :: DocumentID
    , documenttitle1            :: BS.ByteString
    , documentauthor1           :: Author
    , documentsignatorylinks1   :: [SignatoryLink]  
    , documentfiles1            :: [File]
    , documentstatus1           :: DocumentStatus
    , documentctime1            :: MinutesTime
    , documentmtime1            :: MinutesTime
    , documentchargemode1       :: ChargeMode
    }
    deriving (Eq, Ord, Typeable)

data Document2 = Document2
    { documentid2               :: DocumentID
    , documenttitle2            :: BS.ByteString
    , documentauthor2           :: Author
    , documentsignatorylinks2   :: [SignatoryLink]  
    , documentfiles2            :: [File]
    , documentstatus2           :: DocumentStatus
    , documentctime2            :: MinutesTime
    , documentmtime2            :: MinutesTime
    , documentchargemode2       :: ChargeMode
    , documentdaystosign2       :: Int
    , documenttimeouttime2      :: Maybe TimeoutTime

    -- we really should keep history here so we know what happened
    }
    deriving (Eq, Ord, Typeable)

data Document3 = Document3
    { documentid3               :: DocumentID
    , documenttitle3            :: BS.ByteString
    , documentauthor3           :: Author
    , documentsignatorylinks3   :: [SignatoryLink]  
    , documentfiles3            :: [File]
    , documentstatus3           :: DocumentStatus
    , documentctime3            :: MinutesTime
    , documentmtime3            :: MinutesTime
    , documentchargemode3       :: ChargeMode
    , documentdaystosign3       :: Int
    , documenttimeouttime3      :: Maybe TimeoutTime
    , documentdeleted3          :: Bool -- should not appear in list
    , documentauthordetails3    :: SignatoryDetails
    , documentmaybesigninfo3    :: Maybe SignInfo      -- about the author signed the document
    , documenthistory3          :: [DocumentHistoryEntry]

    -- we really should keep history here so we know what happened
    }
    deriving (Eq, Ord, Typeable)

data Document4 = Document4
    { documentid4               :: DocumentID
    , documenttitle4            :: BS.ByteString
    , documentauthor4           :: Author
    , documentsignatorylinks4   :: [SignatoryLink]  
    , documentfiles4            :: [File]
    , documentstatus4           :: DocumentStatus
    , documentctime4            :: MinutesTime
    , documentmtime4            :: MinutesTime
    , documentchargemode4       :: ChargeMode
    , documentdaystosign4       :: Int
    , documenttimeouttime4      :: Maybe TimeoutTime 
    , documentdeleted4          :: Bool -- should not appear in list
    , documentauthordetails4    :: SignatoryDetails
    , documentmaybesigninfo4    :: Maybe SignInfo      -- about the author signed the document |should be droped and check at runtime|
    , documenthistory4          :: [DocumentHistoryEntry]
    , documentinvitetext4       :: BS.ByteString

    -- we really should keep history here so we know what happened
    }
    deriving (Eq, Ord, Typeable)

data Document5 = Document5
    { documentid5               :: DocumentID
    , documenttitle5            :: BS.ByteString
    , documentauthor5           :: Author
    , documentsignatorylinks5   :: [SignatoryLink]  
    , documentfiles5            :: [File]
    , documentsealedfiles5      :: [File]
    , documentstatus5           :: DocumentStatus
    , documentctime5            :: MinutesTime
    , documentmtime5            :: MinutesTime
    , documentchargemode5       :: ChargeMode
    , documentdaystosign5       :: Int
    , documenttimeouttime5      :: Maybe TimeoutTime 
    , documentdeleted5          :: Bool -- should not appear in list
    , documentauthordetails5    :: SignatoryDetails
    , documentmaybesigninfo5    :: Maybe SignInfo      -- about the author signed the document |should be droped and check at runtime|
    , documenthistory5          :: [DocumentHistoryEntry]
    , documentinvitetext5       :: BS.ByteString
    }
    deriving (Eq, Ord, Typeable)

data Document6 = Document6
    { documentid6               :: DocumentID
    , documenttitle6            :: BS.ByteString
    , documentauthor6           :: Author
    , documentsignatorylinks6   :: [SignatoryLink]  
    , documentfiles6            :: [File]
    , documentsealedfiles6      :: [File]
    , documentstatus6           :: DocumentStatus
    , documentctime6            :: MinutesTime
    , documentmtime6            :: MinutesTime
    , documentchargemode6       :: ChargeMode
    , documentdaystosign6       :: Maybe Int
    , documenttimeouttime6      :: Maybe TimeoutTime 
    , documentdeleted6          :: Bool -- should not appear in list
    , documentauthordetails6    :: SignatoryDetails
    , documentmaybesigninfo6    :: Maybe SignInfo      -- about the author signed the document 
    , documenthistory6          :: [DocumentHistoryEntry]
    , documentinvitetext6       :: BS.ByteString

    -- we really should keep history here so we know what happened
    }
    deriving (Eq, Ord, Typeable)

data Document7 = Document7
    { documentid7               :: DocumentID
    , documenttitle7            :: BS.ByteString
    , documentauthor7           :: Author
    , documentsignatorylinks7   :: [SignatoryLink]  
    , documentfiles7            :: [File]
    , documentsealedfiles7      :: [File]
    , documentstatus7           :: DocumentStatus
    , documentctime7            :: MinutesTime
    , documentmtime7            :: MinutesTime
    , documentchargemode7       :: ChargeMode
    , documentdaystosign7       :: Maybe Int
    , documenttimeouttime7      :: Maybe TimeoutTime 
    -- | If true, this Document will not appear in the document list
    , documentdeleted7          :: Bool
    , documenthistory7          :: [DocumentHistoryEntry]
    , documentinvitetext7       :: BS.ByteString
    }
    deriving (Eq, Ord, Typeable)

data Document8 = Document8
    { documentid8               :: DocumentID
    , documenttitle8            :: BS.ByteString
    , documentauthor8           :: Author
    , documentsignatorylinks8   :: [SignatoryLink]  
    , documentfiles8            :: [File]
    , documentsealedfiles8      :: [File]
    , documentstatus8           :: DocumentStatus
    , documentctime8            :: MinutesTime
    , documentmtime8            :: MinutesTime
    , documentchargemode8       :: ChargeMode
    , documentdaystosign8       :: Maybe Int
    , documenttimeouttime8      :: Maybe TimeoutTime 
    -- | If true, this Document will not appear in the document list
    , documentdeleted8          :: Bool
    , documenthistory8          :: [DocumentHistoryEntry]
    , documentinvitetext8       :: BS.ByteString
    , documenttrustweaverreference8 :: Maybe BS.ByteString
    }
    deriving (Eq, Ord, Typeable)

data Document9 = Document9
    { documentid9               :: DocumentID
    , documenttitle9            :: BS.ByteString
    , documentauthor9           :: Author
    , documentsignatorylinks9   :: [SignatoryLink]  
    , documentfiles9            :: [File]
    , documentsealedfiles9      :: [File]
    , documentstatus9           :: DocumentStatus
    , documentctime9            :: MinutesTime
    , documentmtime9            :: MinutesTime
    , documentchargemode9       :: ChargeMode
    , documentdaystosign9       :: Maybe Int
    , documenttimeouttime9      :: Maybe TimeoutTime 
    -- | If true, this Document will not appear in the document list
    , documentdeleted9          :: Bool
    , documenthistory9          :: [DocumentHistoryEntry]
    , documentinvitetext9       :: BS.ByteString
    , documenttrustweaverreference9 :: Maybe BS.ByteString
    , documentallowedidtypes9   :: [IdentificationType]
    }
    deriving (Eq, Ord, Typeable)

data Document10 = Document10
    { documentid10               :: DocumentID
    , documenttitle10            :: BS.ByteString
    , documentauthor10           :: Author
    , documentsignatorylinks10   :: [SignatoryLink]  
    , documentfiles10            :: [File]
    , documentsealedfiles10      :: [File]
    , documentstatus10           :: DocumentStatus
    , documentctime10            :: MinutesTime
    , documentmtime10            :: MinutesTime
    , documentchargemode10       :: ChargeMode
    , documentdaystosign10       :: Maybe Int
    , documenttimeouttime10      :: Maybe TimeoutTime 
    -- | If true, this Document will not appear in the document list
    , documentdeleted10          :: Bool
    , documenthistory10          :: [DocumentHistoryEntry]
    , documentinvitetext10       :: BS.ByteString
    , documenttrustweaverreference10 :: Maybe BS.ByteString
    , documentallowedidtypes10   :: [IdentificationType]
    , authornameplacements10 :: [FieldPlacement]
    , authorcompanyplacements10 :: [FieldPlacement]
    , authoremailplacements10 :: [FieldPlacement]
    , authornumberplacements10 :: [FieldPlacement]
    , authorotherfields10 :: [FieldDefinition]
    }
    deriving (Eq, Ord, Typeable)
    
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

data Document = Document
    { documentid                     :: DocumentID
    , documenttitle                  :: BS.ByteString
    , documentauthor                 :: Author                  -- to be moved to siglinks
    , documentsignatorylinks         :: [SignatoryLink]  
    , documentfiles                  :: [File]
    , documentsealedfiles            :: [File]
    , documentstatus                 :: DocumentStatus
    , documenttype                   :: DocumentType
    , documentctime                  :: MinutesTime
    , documentmtime                  :: MinutesTime
    , documentdaystosign             :: Maybe Int    
    , documenttimeouttime            :: Maybe TimeoutTime
    , documentinvitetime             :: Maybe SignInfo
    , documentdeleted                :: Bool                    -- to be moved to links
    , documentlog                    :: [DocumentLogEntry]      -- to be made into plain text 
    , documentinvitetext             :: BS.ByteString             
    , documenttrustweaverreference   :: Maybe BS.ByteString
    , documentallowedidtypes         :: [IdentificationType]
    , documentcsvupload              :: Maybe CSVUpload
    , authorfstnameplacements        :: [FieldPlacement]        -- the below to be moved to siglinks
    , authorsndnameplacements        :: [FieldPlacement]
    , authorcompanyplacements        :: [FieldPlacement]
    , authoremailplacements          :: [FieldPlacement]
    , authorpersonalnumberplacements :: [FieldPlacement]
    , authorcompanynumberplacements  :: [FieldPlacement]
    , authorotherfields              :: [FieldDefinition]
    , documentcancelationreason      :: Maybe CancelationReason -- ??
    }

data CancelationReason =  ManualCancel
                        -- The data returned by ELeg server
                        --                 msg                    fn            ln            num
                        | ELegDataMismatch String SignatoryLinkID BS.ByteString BS.ByteString BS.ByteString
    deriving (Eq, Ord, Typeable)


{-| Watch out. This instance is a bit special. It has to be
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
deriving instance Show CSVUpload
deriving instance Show ChargeMode
deriving instance Show Author

deriving instance Show DocStats

deriving instance Show FieldDefinition
deriving instance Show FieldPlacement


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

$(deriveSerialize ''SignatoryDetails)
instance Version SignatoryDetails where
    mode = extension 4 (Proxy :: Proxy SignatoryDetails3)
    
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

$(deriveSerialize ''SignatoryLink)
instance Version SignatoryLink where
    mode = extension 7 (Proxy :: Proxy SignatoryLink6)
    
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


instance Migrate SignatoryDetails3 SignatoryDetails where
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
                }) = SignatoryDetails
                { signatoryfstname =  signatoryfstname3
                , signatorysndname = signatorysndname3
                , signatorycompany = signatorycompany3
                , signatorypersonalnumber = signatorynumber3
                , signatorycompanynumber = BS.empty
                , signatoryemail = signatoryemail3
                , signatoryfstnameplacements = signatoryfstnameplacements3
                , signatorysndnameplacements = signatorysndnameplacements3
                , signatorycompanyplacements = signatorycompanyplacements3
                , signatoryemailplacements = signatoryemailplacements3
                , signatorypersonalnumberplacements = signatorynumberplacements3
                , signatorycompanynumberplacements = []
                , signatoryotherfields = signatoryotherfields3
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

instance Migrate SignatoryLink6 SignatoryLink where
    migrate (SignatoryLink6 
             { signatorylinkid6
             , signatorydetails6
             , signatorymagichash6
             , maybesignatory6
             , maybesigninfo6
             , maybeseeninfo6
             , invitationdeliverystatus6
             , signatorysignatureinfo6
             }) = SignatoryLink
                { signatorylinkid           = signatorylinkid6
                , signatorydetails          = signatorydetails6
                , signatorymagichash        = signatorymagichash6
                , maybesignatory            = maybesignatory6
                , maybesigninfo             = maybesigninfo6
                , maybeseeninfo             = maybeseeninfo6
                , invitationdeliverystatus  = invitationdeliverystatus6
                , signatorysignatureinfo    = signatorysignatureinfo6
                , signatorylinkdeleted      = False
                , signatoryroles            = [SignatoryPartner]
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

$(deriveSerialize ''Document0)
instance Version Document0 where

$(deriveSerialize ''Document1)
instance Version Document1 where
    mode = extension 1 (Proxy :: Proxy Document0)

$(deriveSerialize ''Document2)
instance Version Document2 where
    mode = extension 2 (Proxy :: Proxy Document1)

$(deriveSerialize ''Document3)
instance Version Document3 where
    mode = extension 3 (Proxy :: Proxy Document2)

$(deriveSerialize ''Document4)
instance Version Document4 where
    mode = extension 4 (Proxy :: Proxy Document3)

$(deriveSerialize ''Document5)
instance Version Document5 where
    mode = extension 5 (Proxy :: Proxy Document4)
    
$(deriveSerialize ''Document6)
instance Version Document6 where
    mode = extension 6 (Proxy :: Proxy Document5)

$(deriveSerialize ''Document7)
instance Version Document7 where
    mode = extension 7 (Proxy :: Proxy Document6)

$(deriveSerialize ''Document8)
instance Version Document8 where
    mode = extension 8 (Proxy :: Proxy Document7)

$(deriveSerialize ''Document9)
instance Version Document9 where
    mode = extension 9 (Proxy :: Proxy Document8)

$(deriveSerialize ''Document10)
instance Version Document10 where
    mode = extension 10 (Proxy :: Proxy Document9)

$(deriveSerialize ''Document11)
instance Version Document11 where
    mode = extension 11 (Proxy :: Proxy Document10)

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

$(deriveSerialize ''Document)
instance Version Document where
    mode = extension 16 (Proxy :: Proxy Document15)

instance Migrate DocumentHistoryEntry0 DocumentHistoryEntry where
        migrate (DocumentHistoryCreated0 { dochisttime0 }) = 
            DocumentHistoryCreated dochisttime0
        migrate (DocumentHistoryInvitationSent0 { dochisttime0 
                                                , ipnumber0
                                                }) 
            = DocumentHistoryInvitationSent dochisttime0 ipnumber0 []

instance Migrate Document0 Document1 where
      migrate (Document0
          { documentid0
          , documenttitle0
          , documentauthor0
          , documentsignatorylinks0
          , documentfiles0
          , documentstatus0
          , documentctime0
          , documentmtime0
          }) = Document1
          { documentid1 = documentid0
          , documenttitle1 = documenttitle0
          , documentauthor1 = documentauthor0
          , documentsignatorylinks1 = documentsignatorylinks0
          , documentfiles1 = documentfiles0
          , documentstatus1 = documentstatus0
          , documentctime1 = documentctime0
          , documentmtime1 = documentmtime0
          , documentchargemode1 = ChargeInitialFree
          }

instance Migrate Document1 Document2 where
      migrate (Document1
          { documentid1
          , documenttitle1
          , documentauthor1
          , documentsignatorylinks1
          , documentfiles1
          , documentstatus1
          , documentctime1
          , documentmtime1
          , documentchargemode1
          }) = Document2
          { documentid2 = documentid1
          , documenttitle2 = documenttitle1
          , documentauthor2 = documentauthor1
          , documentsignatorylinks2 = documentsignatorylinks1
          , documentfiles2 = documentfiles1
          , documentstatus2 = documentstatus1
          , documentctime2 = documentctime1
          , documentmtime2 = documentmtime1
          , documentchargemode2 = documentchargemode1
          , documentdaystosign2 = 30
          , documenttimeouttime2 = Nothing
          }

instance Migrate Document2 Document3 where
      migrate (Document2 
          { documentid2
          , documenttitle2
          , documentauthor2
          , documentsignatorylinks2
          , documentfiles2
          , documentstatus2
          , documentctime2
          , documentmtime2
          , documentchargemode2
          , documentdaystosign2
          , documenttimeouttime2
          }) = Document3
          { documentid3 = documentid2
          , documenttitle3 = documenttitle2
          , documentauthor3 = documentauthor2
          , documentsignatorylinks3 = documentsignatorylinks2
          , documentfiles3 = documentfiles2
          , documentstatus3 = documentstatus2
          , documentctime3 = documentctime2
          , documentmtime3 = documentmtime2
          , documentchargemode3 = documentchargemode2
          , documentdaystosign3 = documentdaystosign2
          , documenttimeouttime3 = documenttimeouttime2
          , documentdeleted3 = False
          , documentauthordetails3 = SignatoryDetails BS.empty BS.empty BS.empty BS.empty BS.empty BS.empty [] [] [] [] [] [] []
          , documentmaybesigninfo3 = Nothing
          , documenthistory3 = []
          }

instance Migrate Document3 Document4 where
      migrate (Document3
          { documentid3
          , documenttitle3
          , documentauthor3
          , documentsignatorylinks3
          , documentfiles3
          , documentstatus3
          , documentctime3
          , documentmtime3
          , documentchargemode3
          , documentdaystosign3
          , documenttimeouttime3
          , documentdeleted3
          , documentauthordetails3
          , documentmaybesigninfo3
          , documenthistory3
          }) = Document4
          { documentid4 = documentid3
          , documenttitle4 = documenttitle3
          , documentauthor4 = documentauthor3
          , documentsignatorylinks4 = documentsignatorylinks3
          , documentfiles4 = documentfiles3
          , documentstatus4 = documentstatus3
          , documentctime4 = documentctime3
          , documentmtime4 = documentmtime3
          , documentchargemode4 = documentchargemode3
          , documentdaystosign4 = documentdaystosign3
          , documenttimeouttime4 = documenttimeouttime3
          , documentdeleted4 = documentdeleted3
          , documentauthordetails4 = documentauthordetails3
          , documentmaybesigninfo4 = documentmaybesigninfo3
          , documenthistory4 = documenthistory3
          , documentinvitetext4 = BS.empty
          }

instance Migrate Document4 Document5 where
      migrate (Document4
          { documentid4
          , documenttitle4
          , documentauthor4
          , documentsignatorylinks4
          , documentfiles4
          , documentstatus4
          , documentctime4
          , documentmtime4
          , documentchargemode4
          , documentdaystosign4
          , documenttimeouttime4
          , documentdeleted4
          , documentauthordetails4
          , documentmaybesigninfo4
          , documenthistory4
          , documentinvitetext4
          }) = Document5
          { documentid5 = documentid4
          , documenttitle5 = documenttitle4
          , documentauthor5 = documentauthor4
          , documentsignatorylinks5 = documentsignatorylinks4
          , documentfiles5 = if documentstatus4 == Closed
                            then []
                            else documentfiles4
          , documentstatus5 = documentstatus4
          , documentctime5 = documentctime4
          , documentmtime5 = documentmtime4
          , documentchargemode5 = documentchargemode4
          , documentdaystosign5 = documentdaystosign4
          , documenttimeouttime5 = documenttimeouttime4
          , documentdeleted5 = documentdeleted4
          , documentauthordetails5 = documentauthordetails4
          , documentmaybesigninfo5 = documentmaybesigninfo4
          , documenthistory5 = documenthistory4
          , documentinvitetext5 = documentinvitetext4
          , documentsealedfiles5 = if documentstatus4 == Closed
                                  then documentfiles4
                                  else []
          }
          
          
instance Migrate Document5 Document6 where
      migrate (Document5
          { documentid5
          , documenttitle5
          , documentauthor5
          , documentsignatorylinks5
          , documentfiles5
          , documentstatus5
          , documentctime5
          , documentmtime5
          , documentchargemode5
          , documentdaystosign5
          , documenttimeouttime5
          , documentdeleted5
          , documentauthordetails5
          , documentmaybesigninfo5
          , documenthistory5
          , documentinvitetext5
          , documentsealedfiles5
          }) = Document6
          { documentid6 = documentid5
          , documenttitle6 = documenttitle5
          , documentauthor6 = documentauthor5
          , documentsignatorylinks6 = documentsignatorylinks5
          , documentfiles6 = documentfiles5 
          , documentstatus6 = documentstatus5
          , documentctime6 = documentctime5
          , documentmtime6 = documentmtime5
          , documentchargemode6 = documentchargemode5
          , documentdaystosign6 = Just documentdaystosign5
          , documenttimeouttime6 = documenttimeouttime5
          , documentdeleted6 = documentdeleted5
          , documentauthordetails6 = documentauthordetails5
          , documentmaybesigninfo6 = documentmaybesigninfo5
          , documenthistory6 = documenthistory5
          , documentinvitetext6 = documentinvitetext5
          , documentsealedfiles6 =  documentsealedfiles5
          }

instance Migrate Document6 Document7 where
    migrate (Document6
             { documentid6
             , documenttitle6
             , documentauthor6
             , documentsignatorylinks6
             , documentfiles6
             , documentsealedfiles6
             , documentstatus6
             , documentctime6
             , documentmtime6
             , documentchargemode6
             , documentdaystosign6
             , documenttimeouttime6
             , documentdeleted6
             , documentauthordetails6
             , documentmaybesigninfo6
             , documenthistory6
             , documentinvitetext6
             }) = Document7
                { documentid7               = documentid6
                , documenttitle7            = documenttitle6
                , documentauthor7           = documentauthor6
                , documentsignatorylinks7   = (authorlink : documentsignatorylinks6)
                , documentfiles7            = documentfiles6
                , documentsealedfiles7      = documentsealedfiles6
                , documentstatus7           = documentstatus6
                , documentctime7            = documentctime6
                , documentmtime7            = documentmtime6
                , documentchargemode7       = documentchargemode6
                , documentdaystosign7       = documentdaystosign6
                , documenttimeouttime7      = documenttimeouttime6
                , documentdeleted7          = documentdeleted6
                , documenthistory7          = documenthistory6
                , documentinvitetext7       = documentinvitetext6
                }
                  where authorlink = SignatoryLink { maybesigninfo = documentmaybesigninfo6
                                                   , maybeseeninfo = documentmaybesigninfo6
                                                   , maybesignatory = Just $ unAuthor $ documentauthor6
                                                   , signatorydetails = documentauthordetails6
                                                   , invitationdeliverystatus = Delivered
                                                   , signatorymagichash = MagicHash 0
                                                   , signatorylinkid = SignatoryLinkID 0
                                                   , signatorysignatureinfo = Nothing
                                                   }

instance Migrate Document7 Document8 where
    migrate (Document7
                { documentid7            
                , documenttitle7         
                , documentauthor7        
                , documentsignatorylinks7
                , documentfiles7       
                , documentsealedfiles7 
                , documentstatus7      
                , documentctime7       
                , documentmtime7       
                , documentchargemode7  
                , documentdaystosign7  
                , documenttimeouttime7 
                , documentdeleted7   
                , documenthistory7   
                , documentinvitetext7
                }) = Document8
                { documentid8 = documentid7           
                , documenttitle8 = documenttitle7        
                , documentauthor8 = documentauthor7       
                , documentsignatorylinks8 = documentsignatorylinks7
                , documentfiles8 = documentfiles7      
                , documentsealedfiles8 = documentsealedfiles7
                , documentstatus8 = documentstatus7     
                , documentctime8 = documentctime7      
                , documentmtime8 = documentmtime7      
                , documentchargemode8 = documentchargemode7 
                , documentdaystosign8 = documentdaystosign7 
                , documenttimeouttime8 = documenttimeouttime7 
                , documentdeleted8 = documentdeleted7  
                , documenthistory8 = documenthistory7  
                , documentinvitetext8 = documentinvitetext7
                , documenttrustweaverreference8 = Nothing
                }

instance Migrate Document8 Document9 where
    migrate (Document8
                { documentid8
                , documenttitle8
                , documentauthor8
                , documentsignatorylinks8
                , documentfiles8
                , documentsealedfiles8
                , documentstatus8 
                , documentctime8
                , documentmtime8
                , documentchargemode8
                , documentdaystosign8
                , documenttimeouttime8
                , documentdeleted8
                , documenthistory8
                , documentinvitetext8
                , documenttrustweaverreference8
                }) = Document9
                { documentid9 = documentid8
                , documenttitle9 = documenttitle8
                , documentauthor9 = documentauthor8
                , documentsignatorylinks9 = documentsignatorylinks8
                , documentfiles9 = documentfiles8
                , documentsealedfiles9 = documentsealedfiles8
                , documentstatus9 = documentstatus8
                , documentctime9 = documentctime8
                , documentmtime9 = documentmtime8
                , documentchargemode9 = documentchargemode8
                , documentdaystosign9 = documentdaystosign8
                , documenttimeouttime9 = documenttimeouttime8
                , documentdeleted9 = documentdeleted8
                , documenthistory9 = documenthistory8
                , documentinvitetext9 = documentinvitetext8
                , documenttrustweaverreference9 = documenttrustweaverreference8
                , documentallowedidtypes9 = [EmailIdentification]
                }

instance Migrate Document9 Document10 where
    migrate (Document9
                { documentid9
                , documenttitle9
                , documentauthor9
                , documentsignatorylinks9
                , documentfiles9
                , documentsealedfiles9
                , documentstatus9
                , documentctime9
                , documentmtime9
                , documentchargemode9
                , documentdaystosign9
                , documenttimeouttime9
                , documentdeleted9
                , documenthistory9
                , documentinvitetext9
                , documenttrustweaverreference9
                , documentallowedidtypes9
                }) = Document10
                { documentid10 = documentid9
                , documenttitle10 = documenttitle9
                , documentauthor10 = documentauthor9
                , documentsignatorylinks10 = documentsignatorylinks9
                , documentfiles10 = documentfiles9
                , documentsealedfiles10 = documentsealedfiles9
                , documentstatus10 = documentstatus9
                , documentctime10 = documentctime9
                , documentmtime10 = documentmtime9
                , documentchargemode10 = documentchargemode9
                , documentdaystosign10 = documentdaystosign9
                , documenttimeouttime10 = documenttimeouttime9
                , documentdeleted10 = documentdeleted9
                , documenthistory10 = documenthistory9
                , documentinvitetext10 = documentinvitetext9
                , documenttrustweaverreference10 = documenttrustweaverreference9
                , documentallowedidtypes10 = documentallowedidtypes9
                , authornameplacements10 = []
                , authorcompanyplacements10 = []
                , authoremailplacements10 = []
                , authornumberplacements10 = []
                , authorotherfields10 = []
                }

instance Migrate Document10 Document11 where
    migrate (Document10
                { documentid10 
                , documenttitle10 
                , documentauthor10
                , documentsignatorylinks10 
                , documentfiles10 
                , documentsealedfiles10
                , documentstatus10
                , documentctime10
                , documentmtime10
                , documentchargemode10
                , documentdaystosign10
                , documenttimeouttime10
                , documentdeleted10
                , documenthistory10
                , documentinvitetext10
                , documenttrustweaverreference10
                , documentallowedidtypes10
                , authornameplacements10
                , authorcompanyplacements10
                , authoremailplacements10
                , authornumberplacements10
                , authorotherfields10
                }) = Document11
                { documentid11 = documentid10
                , documenttitle11 = documenttitle10
                , documentauthor11 = documentauthor10
                , documentsignatorylinks11 = documentsignatorylinks10
                , documentfiles11 = documentfiles10
                , documentsealedfiles11 = documentsealedfiles10
                , documentstatus11 = documentstatus10
                , documentctime11 = documentctime10
                , documentmtime11 = documentmtime10
                , documentchargemode11 = documentchargemode10
                , documentdaystosign11 = documentdaystosign10
                , documenttimeouttime11 = documenttimeouttime10
                , documentdeleted11 = documentdeleted10
                , documenthistory11 = documenthistory10
                , documentinvitetext11 = documentinvitetext10
                , documenttrustweaverreference11 = documenttrustweaverreference10
                , documentallowedidtypes11 = documentallowedidtypes10
                , authorfstnameplacements11 = authornameplacements10
                , authorsndnameplacements11 = []
                , authorcompanyplacements11 = authorcompanyplacements10
                , authoremailplacements11 =  authoremailplacements10
                , authornumberplacements11 =  authornumberplacements10
                , authorotherfields11 = authorotherfields10
                }

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

instance Migrate Document15 Document where
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
                }) = Document
                { documentid                     = documentid15
                , documenttitle                  = documenttitle15
                , documentauthor                 = documentauthor15
                , documentsignatorylinks         = documentsignatorylinks15
                , documentfiles                  = documentfiles15
                , documentsealedfiles            = documentsealedfiles15
                , documentstatus                 = documentstatus15
                , documenttype                   = documenttype15
                , documentctime                  = documentctime15
                , documentmtime                  = documentmtime15
                , documentdaystosign             = documentdaystosign15
                , documenttimeouttime            = documenttimeouttime15
                , documentinvitetime             = documentinvitetime15
                , documentdeleted                = documentdeleted15
                , documentlog                    = documentlog15
                , documentinvitetext             = documentinvitetext15
                , documenttrustweaverreference   = documenttrustweaverreference15
                , documentallowedidtypes         = documentallowedidtypes15
                , documentcsvupload              = documentcsvupload15
                , authorfstnameplacements        = authorfstnameplacements15
                , authorsndnameplacements        = authorsndnameplacements15
                , authorcompanyplacements        = authorcompanyplacements15
                , authoremailplacements          = authoremailplacements15
                , authorpersonalnumberplacements = authornumberplacements15
                , authorcompanynumberplacements  = []
                , authorotherfields              = authorotherfields15
                , documentcancelationreason      = documentcancelationreason15
                }

$(deriveSerialize ''DocumentStatus)
instance Version DocumentStatus where

$(deriveSerialize ''DocumentType)
instance Version DocumentType where
    
$(deriveSerialize ''ChargeMode)
instance Version ChargeMode where

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

instance Indexable Document where
        empty = ixSet [ ixFun (\x -> [documentid x] :: [DocumentID])
                      , ixFun (\x -> [documentauthor x] :: [Author])
                      , ixFun (\x -> (map Signatory (catMaybes (map maybesignatory (documentsignatorylinks x)))) :: [Signatory])
                              
                      -- wait, wait, wait: the following is wrong, signatory link ids are valid only in 
                      -- the scope of a single document! FIXME
                      , ixFun (\x -> map signatorylinkid (documentsignatorylinks x) :: [SignatoryLinkID])
                      , ixFun (\x -> map fileid (documentfiles x ++ documentsealedfiles x) :: [FileID])
                      , ixFun (\x -> (case documenttimeouttime x of
                                         Just time -> [time]
                                         Nothing -> []) :: [TimeoutTime])
                      , ixFun (\x -> [documenttype x] :: [DocumentType])
                      ]
                                                    
instance Component Documents where
  type Dependencies Documents = End
  initialValue = empty

$(deriveSerialize ''SignatoryRole)
