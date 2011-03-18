module Doc.DocStateData(
      Author(..)
    , ChargeMode(..)
    , Document(..)
    , DocumentHistoryEntry(..)
    , DocumentID(..)
    , DocumentStatus(..)
    , DocumentType(..)
    , Documents(..)
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
    ) where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Trans
import User.UserState
import Happstack.Data.IxSet as IxSet
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Control.Applicative ((<$>))
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Debug.Trace
import Misc
import Control.Monad
import Data.List (find)
import MinutesTime
import Data.List (zipWith4,partition)
import System.Random
import Data.Word
import Data.Int
import System.Log.Logger (errorM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Mails.MailsUtil
import Data.Data (Data)
import qualified Data.Generics.SYB.WithClass.Derive as SYB



newtype Author = Author { unAuthor :: UserID }
    deriving (Eq, Ord, Typeable, Data)

newtype DocumentID = DocumentID { unDocumentID :: Int64 }
    deriving (Eq, Ord, Typeable, Data)
newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int }
    deriving (Eq, Ord, Typeable, Data)
newtype FileID = FileID { unFileID :: Int }
    deriving (Eq, Ord, Typeable, Data)
newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }
    deriving (Eq, Ord, Typeable, Data)

data IdentificationType = EmailIdentification
                        | ELegitimationIdentification
    deriving (Eq, Ord, Typeable, Data)

data SignatureProvider = BankIDProvider
                       | TeliaProvider
                       | NordeaProvider
    deriving (Eq, Ord, Typeable, Data)

data SignatureInfo = SignatureInfo { signatureinfotext        :: String
                                   , signatureinfosignature   :: String
                                   , signatureinfocertificate :: String
                                   , signatureinfoprovider    :: SignatureProvider
                                   }
    deriving (Eq, Ord, Typeable, Data)

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
    deriving (Eq, Ord, Typeable, Data)

-- defines where a field is placed
data FieldPlacement = FieldPlacement
    { placementx :: Int
    , placementy :: Int
    , placementpage :: Int
    , placementpagewidth :: Int
    , placementpageheight :: Int
    }
    deriving (Eq, Ord, Typeable, Data)
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


data SignatoryDetails = SignatoryDetails
    { signatoryfstname   :: BS.ByteString  -- "Gracjan Polak" 
    , signatorysndname   :: BS.ByteString  -- "Gracjan Polak" 
    , signatorycompany   :: BS.ByteString  -- SkrivaPå
    , signatorynumber    :: BS.ByteString  -- 123456789
    , signatoryemail     :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
    -- for templates
    , signatoryfstnameplacements :: [FieldPlacement]
    , signatorysndnameplacements :: [FieldPlacement]
    , signatorycompanyplacements :: [FieldPlacement]
    , signatoryemailplacements :: [FieldPlacement]
    , signatorynumberplacements :: [FieldPlacement]
    , signatoryotherfields :: [FieldDefinition]
    }     
    deriving (Eq, Ord, Typeable, Data)

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

data SignatoryLink = SignatoryLink 
    { signatorylinkid    :: SignatoryLinkID
    , signatorydetails   :: SignatoryDetails
    , signatorymagichash :: MagicHash
    , maybesignatory     :: Maybe Signatory
    , maybesigninfo      :: Maybe SignInfo
    , maybeseeninfo      :: Maybe SignInfo
    , invitationdeliverystatus :: MailsDeliveryStatus
    , signatorysignatureinfo :: Maybe SignatureInfo
    }    
    deriving (Eq, Ord, Typeable, Data)

data SignInfo = SignInfo
    { signtime :: MinutesTime
    , signipnumber :: Word32
    }
    deriving (Eq, Ord, Typeable, Data)

data SignInfo0 = SignInfo0
    { signtime0 :: MinutesTime
    }
    deriving (Eq, Ord, Typeable)

newtype Signatory = Signatory { unSignatory :: UserID }
    deriving (Eq, Ord, Typeable, Data)

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
                    
data DocumentType = Contract | Template
    deriving (Eq, Ord, Typeable, Data)

data ChargeMode = ChargeInitialFree   -- initial 5 documents are free
                | ChargeNormal        -- value times number of people involved

    deriving (Eq, Ord, Typeable, Data)

data DocumentHistoryEntry 
    = DocumentHistoryCreated 
      { dochisttime :: MinutesTime }
    | DocumentHistoryInvitationSent 
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      }    -- changed state from Preparatio to Pending
    | DocumentHistoryTimedOut
      { dochisttime :: MinutesTime }
    | DocumentHistorySigned
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      }
    | DocumentHistoryRejected
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      }
    | DocumentHistoryClosed
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      }
    | DocumentHistoryCanceled
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      }
    | DocumentHistoryRestarted
      { dochisttime :: MinutesTime
      , ipnumber :: Word32
      }
    deriving (Eq, Ord, Typeable, Data)

data DocStats = DocStats {
                 doccount :: Int
               , signaturecount :: Int
             }
    deriving (Eq, Ord, Typeable, Data)


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
    
data Document = Document
    { documentid               :: DocumentID
    , documenttitle            :: BS.ByteString
    , documentauthor           :: Author
    , documentsignatorylinks   :: [SignatoryLink]  
    , documentfiles            :: [File]
    , documentsealedfiles      :: [File]
    , documentstatus           :: DocumentStatus
    , documenttype             :: DocumentType
    , documentctime            :: MinutesTime
    , documentmtime            :: MinutesTime
    , documentchargemode       :: ChargeMode
    , documentdaystosign       :: Maybe Int
    , documenttimeouttime      :: Maybe TimeoutTime 
    -- | If true, this Document will not appear in the document list
    , documentdeleted          :: Bool
    , documenthistory          :: [DocumentHistoryEntry]
    , documentinvitetext       :: BS.ByteString
    , documenttrustweaverreference :: Maybe BS.ByteString
    , documentallowedidtypes   :: [IdentificationType]
    , authorfstnameplacements :: [FieldPlacement]
    , authorsndnameplacements :: [FieldPlacement]
    , authorcompanyplacements :: [FieldPlacement]
    , authoremailplacements :: [FieldPlacement]
    , authornumberplacements :: [FieldPlacement]
    , authorotherfields :: [FieldDefinition]
    }

{-| Watch out. This instance is a bit special. It has to be
   "Document" - as this is what database uses as table name.  Simple
   deriving clause will create a "MyApp.MyModule.Document"!  -}

instance Typeable Document where typeOf _ = mkTypeOf "Document"

deriving instance Data Document
    
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
    deriving (Typeable, Data)

data JpegPages0 = JpegPagesPending0
               | JpegPages0 [BS.ByteString]   
               | JpegPagesError0 BS.ByteString 
    deriving (Eq, Ord, Typeable)

data JpegPages = JpegPagesPending
               | JpegPages [(BS.ByteString,Int,Int)]  -- Data + width + height (scaled with some resolution)
               | JpegPagesError BS.ByteString 
    deriving (Eq, Ord, Typeable, Data)
               
data FileStorage = FileStorageMemory BS.ByteString
                 | FileStorageAWS BS.ByteString BS.ByteString -- ^ bucket, url inside bucket
    deriving (Eq, Ord, Typeable, Data)



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
deriving instance Show SignatureProvider
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
    showsPrec prec file = (++) (BS.toString (filename file))

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

$(deriveSerialize ''SignatureProvider)
instance Version SignatureProvider

$(deriveSerialize ''SignatureInfo)
instance Version SignatureInfo

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

$(deriveSerialize ''SignatoryDetails)
instance Version SignatoryDetails where
    mode = extension 3 (Proxy :: Proxy SignatoryDetails2)
    
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

$(deriveSerialize ''SignatoryLink)
instance Version SignatoryLink where
    mode = extension 5 (Proxy :: Proxy SignatoryLink4)
    
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


instance Migrate SignatoryDetails2 SignatoryDetails where
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
                }) = SignatoryDetails
                { signatoryfstname =  signatoryfstname2
                , signatorysndname = signatorysndname2
                , signatorycompany = signatorycompany2
                , signatorynumber = signatorynumber2
                , signatoryemail = signatoryemail2
                , signatoryfstnameplacements = signatorynameplacements2
                , signatorysndnameplacements = []
                , signatorycompanyplacements = signatorycompanyplacements2
                , signatoryemailplacements = signatoryemailplacements2
                , signatorynumberplacements = signatorynumberplacements2
                , signatoryotherfields = signatoryotherfields2
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
                               , signatorynumber = BS.empty
                               , signatoryemail = signatoryemail0
                               , signatoryfstnameplacements = []
                               , signatorysndnameplacements = []
                               , signatorycompanyplacements = []
                               , signatoryemailplacements = []
                               , signatorynumberplacements = []
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

instance Migrate SignatoryLink4 SignatoryLink where
    migrate (SignatoryLink4
             { signatorylinkid4
             , signatorydetails4
             , signatorymagichash4
             , maybesignatory4
             , maybesigninfo4
             , maybeseeninfo4
             , invitationdeliverystatus4
             }) = SignatoryLink
             { signatorylinkid = signatorylinkid4
             , signatorydetails = signatorydetails4
             , signatorymagichash = signatorymagichash4
             , maybesignatory = maybesignatory4
             , maybesigninfo = maybesigninfo4
             , maybeseeninfo = maybeseeninfo4
             , invitationdeliverystatus = invitationdeliverystatus4
             , signatorysignatureinfo = Nothing
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

$(deriveSerialize ''DocumentHistoryEntry)
instance Version DocumentHistoryEntry

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

$(deriveSerialize ''Document)
instance Version Document where
    mode = extension 12 (Proxy :: Proxy Document11)
    
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
          , documentauthordetails3 = SignatoryDetails BS.empty BS.empty BS.empty BS.empty BS.empty [] [] [] [] [] []
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
                                                   , maybesignatory = Just $ Signatory $ unAuthor $ documentauthor6
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

instance Migrate Document11 Document where
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
                }) = Document
                { documentid = documentid11
                , documenttitle = documenttitle11
                , documentauthor = documentauthor11
                , documentsignatorylinks = documentsignatorylinks11
                , documentfiles = documentfiles11
                , documentsealedfiles = documentsealedfiles11
                , documentstatus = documentstatus11
                , documenttype = Contract
                , documentctime = documentctime11
                , documentmtime = documentmtime11
                , documentchargemode = documentchargemode11
                , documentdaystosign = documentdaystosign11
                , documenttimeouttime = documenttimeouttime11
                , documentdeleted = documentdeleted11
                , documenthistory = documenthistory11
                , documentinvitetext = documentinvitetext11
                , documenttrustweaverreference = documenttrustweaverreference11
                , documentallowedidtypes = documentallowedidtypes11
                , authorfstnameplacements = authorfstnameplacements11
                , authorsndnameplacements = authorsndnameplacements11
                , authorcompanyplacements = authorcompanyplacements11
                , authoremailplacements =  authoremailplacements11
                , authornumberplacements =  authornumberplacements11
                , authorotherfields = authorotherfields11
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

$(inferIxSet "Documents" ''Document 'noCalcs 
                 [ ''DocumentID
                 , ''Author
                 , ''Signatory
                 , ''SignatoryLinkID
                 , ''FileID
                 , ''TimeoutTime
                 , ''DocumentType
                 ])
instance Component Documents where
  type Dependencies Documents = End
  initialValue = empty
