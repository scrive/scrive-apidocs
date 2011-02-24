module Doc.DocState 
    ( Author(..)
    , ChargeMode(..)
    , Document(..)
    , DocumentHistoryEntry(..)
    , DocumentID(..)
    , DocumentStatus(..)
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
    , isAuthor
    , isMatchingSignatoryLink
    , anyInvitationUndelivered
    , undeliveredSignatoryLinks
    , ArchiveDocuments(..)
    , AttachFile(..)
    , AttachSealedFile(..)
    , AuthorSignDocument(..)
    , AuthorSendDocument(..)
    , RejectDocument(..)
    , FileModTime(..)
    , FileMovedToAWS(..)
    , FragileTakeOverDocuments(..)
    , GetDocumentByDocumentID(..)
    , GetDocumentStats(..)
    , GetDocumentStatsByUser(..)
    , GetDocuments(..)
    , GetDocumentsByAuthor(..)
    , GetDocumentsBySignatory(..)
    , GetDocumentsByUser(..)
    , GetFilesThatShouldBeMovedToAmazon(..)
    , GetNumberOfDocumentsOfUser(..)
    , GetTimeoutedButPendingDocuments(..)
    , MarkDocumentSeen(..)
    , SetInvitationDeliveryStatus(..)
    , NewDocument(..)
    , SaveDocumentForSignedUser(..)
    , SetDocumentTimeoutTime(..)
    , SetDocumentTrustWeaverReference(..)
    , SignDocument(..)
    , TimeoutDocument(..)
    , UpdateDocument(..)
    , CloseDocument(..)
    , CancelDocument(..)
    -- , WithdrawnDocument(..)
    , RestartDocument(..)
    , ChangeSignatoryEmailWhenUndelivered(..)
    , signatoryDetailsFromUser
    , SetSignatoryLinks(..)
    , GetUniqueSignatoryLinkID(..)
    , GetMagicHash(..)
    , GetDocumentByFileID(..)
    , ErrorDocument(..)
    , IdentificationType(..)
    , SignatureInfo(..)
    , SignatureProvider(..)
    )
where
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

$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
      newtype Author = Author { unAuthor :: UserID }
      newtype DocumentID = DocumentID { unDocumentID :: Int64 }
      newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int }
      newtype FileID = FileID { unFileID :: Int }
      newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }

      data IdentificationType = EmailIdentification
                              | ELegitimationIdentification

      data SignatureProvider = BankIDProvider
                             | TeliaProvider
                             | NordeaProvider

      data SignatureInfo = SignatureInfo { signatureinfotext        :: String
                                         , signatureinfosignature   :: String
                                         , signatureinfocertificate :: String
                                         , signatureinfoprovider    :: SignatureProvider
                                         }

      -- added by Eric Normand for template system
      -- Defines a new field to be placed in a contract
      data FieldDefinition0 = FieldDefinition0
          { fieldlabel0 :: BS.ByteString 
          , fieldvalue0 :: BS.ByteString
          , fieldplacements0 :: [FieldPlacement]
          }

      data FieldDefinition = FieldDefinition
          { fieldlabel :: BS.ByteString 
          , fieldvalue :: BS.ByteString
          , fieldplacements :: [FieldPlacement]
          , fieldfilledbyauthor :: Bool
          }

      -- defines where a field is placed
      data FieldPlacement = FieldPlacement
          { placementx :: Int
          , placementy :: Int
          , placementpage :: Int
          , placementpagewidth :: Int
          , placementpageheight :: Int
          }
      -- end of updates for template system

      data SignatoryDetails0 = SignatoryDetails0
          { signatoryname00      :: BS.ByteString  -- "Gracjan Polak" 
          , signatorycompany00   :: BS.ByteString  -- SkrivaPå
          , signatorynumber00    :: BS.ByteString  -- 123456789
          , signatoryemail00     :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
          }

      data SignatoryDetails = SignatoryDetails
          { signatoryname      :: BS.ByteString  -- "Gracjan Polak" 
          , signatorycompany   :: BS.ByteString  -- SkrivaPå
          , signatorynumber    :: BS.ByteString  -- 123456789
          , signatoryemail     :: BS.ByteString  -- "gracjanpolak@skrivapa.se"
          -- for templates
          , signatorynameplacements :: [FieldPlacement]
          , signatorycompanyplacements :: [FieldPlacement]
          , signatoryemailplacements :: [FieldPlacement]
          , signatorynumberplacements :: [FieldPlacement]
          , signatoryotherfields :: [FieldDefinition]
          } 

      data SignatoryLink0 = SignatoryLink0 
          { signatorylinkid0    :: SignatoryLinkID
          , signatoryname0      :: BS.ByteString 
          , signatorycompany0   :: BS.ByteString 
          , signatoryemail0     :: BS.ByteString
          , maybesignatory0     :: Maybe Signatory
          , maybesigninfo0      :: Maybe SignInfo
          , maybeseentime0      :: Maybe MinutesTime
          }
      data SignatoryLink1 = SignatoryLink1 
          { signatorylinkid1    :: SignatoryLinkID
          , signatorydetails1   :: SignatoryDetails
          , maybesignatory1     :: Maybe Signatory
          , maybesigninfo1      :: Maybe SignInfo
          , maybeseentime1      :: Maybe MinutesTime
          }
      data SignatoryLink2 = SignatoryLink2 
          { signatorylinkid2    :: SignatoryLinkID
          , signatorydetails2   :: SignatoryDetails
          , signatorymagichash2 :: MagicHash
          , maybesignatory2     :: Maybe Signatory
          , maybesigninfo2      :: Maybe SignInfo
          , maybeseentime2      :: Maybe MinutesTime
          }
      data SignatoryLink3 = SignatoryLink3 
          { signatorylinkid3    :: SignatoryLinkID
          , signatorydetails3   :: SignatoryDetails
          , signatorymagichash3 :: MagicHash
          , maybesignatory3     :: Maybe Signatory
          , maybesigninfo3      :: Maybe SignInfo
          , maybeseeninfo3      :: Maybe SignInfo
          }        
      data SignatoryLink4 = SignatoryLink4
          { signatorylinkid4    :: SignatoryLinkID
          , signatorydetails4   :: SignatoryDetails
          , signatorymagichash4 :: MagicHash
          , maybesignatory4     :: Maybe Signatory
          , maybesigninfo4      :: Maybe SignInfo
          , maybeseeninfo4      :: Maybe SignInfo
          , invitationdeliverystatus4 :: MailsDeliveryStatus
          }
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
      data SignInfo = SignInfo
          { signtime :: MinutesTime
          , signipnumber :: Word32
          }
      data SignInfo0 = SignInfo0
          { signtime0 :: MinutesTime
          }
      newtype Signatory = Signatory { unSignatory :: UserID }
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

      data ChargeMode = ChargeInitialFree   -- initial 5 documents are free
                      | ChargeNormal        -- value times number of people involved

      data DocumentHistoryEntry = DocumentHistoryCreated { dochisttime :: MinutesTime }
                                | DocumentHistoryInvitationSent { dochisttime :: MinutesTime
                                                                , ipnumber :: Word32
                                                                }    -- changed state from Preparatio to Pending

      data DocStats = DocStats {
                       doccount :: Int
                     , signaturecount :: Int
                   }
   |])

$(deriveAll [''Default]
  [d|
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

      data Document = Document
          { documentid               :: DocumentID
          , documenttitle            :: BS.ByteString
          , documentauthor           :: Author
          , documentsignatorylinks   :: [SignatoryLink]  
          , documentfiles            :: [File]
          , documentsealedfiles      :: [File]
          , documentstatus           :: DocumentStatus
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
          , authornameplacements :: [FieldPlacement]
          , authorcompanyplacements :: [FieldPlacement]
          , authoremailplacements :: [FieldPlacement]
          , authornumberplacements :: [FieldPlacement]
          , authorotherfields :: [FieldDefinition]
          }

      data File0 = File0 
          { fileid0       :: FileID
          , filename0     :: BS.ByteString
          , filepdf0      :: BS.ByteString 
          , filejpgpages0 :: [BS.ByteString]
          }
      data File1 = File1 
          { fileid1       :: FileID
          , filename1     :: BS.ByteString
          , filepdf1      :: BS.ByteString 
          , filejpgpages1 :: JpegPages
          }
      data File2 = File2 
          { fileid2       :: FileID
          , filename2     :: BS.ByteString
          , filepdf2      :: BS.ByteString 
          }
      data File = File 
          { fileid          :: FileID
          , filename        :: BS.ByteString
          , filestorage     :: FileStorage
          }

      data JpegPages0 = JpegPagesPending0
                     | JpegPages0 [BS.ByteString]   
                     | JpegPagesError0 BS.ByteString 
      
      data JpegPages = JpegPagesPending
                     | JpegPages [(BS.ByteString,Int,Int)]  -- Data + width + height (scaled with some resolution)
                     | JpegPagesError BS.ByteString 
                     
      data FileStorage = FileStorageMemory BS.ByteString
                       | FileStorageAWS BS.ByteString BS.ByteString -- ^ bucket, url inside bucket
   |])

instance Eq Document where
    a == b = documentid a == documentid b

instance Ord Document where
    compare a b | documentid a == documentid b = EQ
                | otherwise = compare (documentmtime b,documenttitle a,documentid a) 
                                      (documentmtime a,documenttitle b,documentid b)
                              -- see above: we use reverse time here!

instance Eq Document0 where
    a == b = documentid0 a == documentid0 b

instance Ord Document0 where
    compare a b | documentid0 a == documentid0 b = EQ
                | otherwise = compare (documentmtime0 b,documenttitle0 a,documentid0 a) 
                                      (documentmtime0 a,documenttitle0 b,documentid0 b)
                              -- see above: we use reverse time here!

instance Eq Document1 where
    a == b = documentid1 a == documentid1 b

instance Ord Document1 where
    compare a b | documentid1 a == documentid1 b = EQ
                | otherwise = compare (documentmtime1 b,documenttitle1 a,documentid1 a) 
                                      (documentmtime1 a,documenttitle1 b,documentid1 b)
                              -- see above: we use reverse time here!

instance Eq Document2 where
    a == b = documentid2 a == documentid2 b

instance Ord Document2 where
    compare a b | documentid2 a == documentid2 b = EQ
                | otherwise = compare (documentmtime2 b,documenttitle2 a,documentid2 a) 
                                      (documentmtime2 a,documenttitle2 b,documentid2 b)
                              -- see above: we use reverse time here!

instance Eq Document3 where
    a == b = documentid3 a == documentid3 b

instance Ord Document3 where
    compare a b | documentid3 a == documentid3 b = EQ
                | otherwise = compare (documentmtime3 b,documenttitle3 a,documentid3 a) 
                                      (documentmtime3 a,documenttitle3 b,documentid3 b)
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

$(deriveSerialize ''SignatoryDetails)
instance Version SignatoryDetails where
    mode = extension 1 (Proxy :: Proxy SignatoryDetails0)

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
    
instance Migrate SignatoryDetails0 SignatoryDetails where
    migrate (SignatoryDetails0
             { signatoryname00 
             , signatorycompany00
             , signatorynumber00 
             , signatoryemail00
             }) = SignatoryDetails
                { signatoryname = signatoryname00
                , signatorycompany = signatorycompany00
                , signatorynumber = signatorynumber00
                , signatoryemail = signatoryemail00
                , signatorynameplacements = []
                , signatorycompanyplacements = []
                , signatoryemailplacements = []
                , signatorynumberplacements = []
                , signatoryotherfields = []
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
                               { signatoryname = signatoryname0
                               , signatorycompany = signatorycompany0
                               , signatorynumber = BS.empty
                               , signatoryemail = signatoryemail0
                               , signatorynameplacements = []
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

$(deriveSerialize ''Document)
instance Version Document where
    mode = extension 10 (Proxy :: Proxy Document9)

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
          , documentauthordetails3 = SignatoryDetails BS.empty BS.empty BS.empty BS.empty [] [] [] [] []
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

instance Migrate Document9 Document where
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
                }) = Document
                { documentid = documentid9
                , documenttitle = documenttitle9
                , documentauthor = documentauthor9
                , documentsignatorylinks = documentsignatorylinks9
                , documentfiles = documentfiles9
                , documentsealedfiles = documentsealedfiles9
                , documentstatus = documentstatus9
                , documentctime = documentctime9
                , documentmtime = documentmtime9
                , documentchargemode = documentchargemode9
                , documentdaystosign = documentdaystosign9
                , documenttimeouttime = documenttimeouttime9
                , documentdeleted = documentdeleted9
                , documenthistory = documenthistory9
                , documentinvitetext = documentinvitetext9
                , documenttrustweaverreference = documenttrustweaverreference9
                , documentallowedidtypes = documentallowedidtypes9
                , authornameplacements = []
                , authorcompanyplacements = []
                , authoremailplacements = []
                , authornumberplacements = []
                , authorotherfields = []
                }


$(deriveSerialize ''DocumentStatus)
instance Version DocumentStatus where

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
                 ])
instance Component Documents where
  type Dependencies Documents = End
  initialValue = empty

getDocuments:: Query Documents [Document]
getDocuments = do
    documents <- ask
    return $ toList documents  

getDocumentByDocumentID :: DocumentID -> Query Documents (Maybe Document)
getDocumentByDocumentID documentid = do
  documents <- ask
  return $ getOne (documents @= documentid)

getDocumentsByAuthor :: UserID -> Query Documents [Document]
getDocumentsByAuthor userid = do
    documents <- ask
    return $ toList (documents @= Author userid)

getDocumentsByUser :: User -> Query Documents [Document]
getDocumentsByUser user = do
    authoredDocs <- getDocumentsByAuthor $ userid user
    signatoryDocs <- getDocumentsBySignatory $ user
    return $ authoredDocs ++ signatoryDocs

getDocumentsBySignatory :: User -> Query Documents [Document]
getDocumentsBySignatory user = do
    documents <- ask
    let involvedAsSignatory doc = (any (isMatchingSignatoryLink user) $ documentsignatorylinks doc)
                                   && (Preparation /= documentstatus doc)
    return $ filter involvedAsSignatory (toList documents)
       
isMatchingSignatoryLink :: User -> SignatoryLink -> Bool
isMatchingSignatoryLink user sigLink = signatoryMatches || emailMatches
  where signatoryMatches = case (maybesignatory sigLink) of
                                             Just (Signatory sigid) | sigid == (userid user) -> True
                                             _ -> False
        emailMatches = (signatoryemail . signatorydetails $ sigLink) == (unEmail . useremail $ userinfo user)


getTimeoutedButPendingDocuments :: MinutesTime -> Query Documents [Document]
getTimeoutedButPendingDocuments now = do
  docs <-  ask
  return $ (flip filter) (toList docs) $ \doc -> case (documenttimeouttime doc) of
                                                  Just timeout -> (documentstatus doc) == Pending &&(unTimeoutTime timeout) < now
                                                  _ -> False           
    

newDocument :: User
            -> BS.ByteString
            -> MinutesTime 
            -> Bool -- is free?
            -> Update Documents Document
newDocument user title ctime isfree = do
  documents <- ask
  docid <- getUnique64 documents DocumentID
  authorlink <- signLinkFromDetails [(unEmail $ useremail $ userinfo user, user)] $ signatoryDetailsFromUser user
  let doc = Document
          { documentid = docid
          , documenttitle = title
          , documentauthor = Author $ userid user
          , documentsignatorylinks = [authorlink]
          , documentfiles = []
          , documentstatus = Preparation
          , documentctime = ctime
          , documentmtime = ctime
          , documentchargemode = if isfree then ChargeInitialFree else ChargeNormal
          , documentdaystosign = Nothing
          , documenttimeouttime = Nothing
          , documentdeleted = False
          , documenthistory = []
          , documentinvitetext = BS.empty
          , documentsealedfiles = []
          , documenttrustweaverreference = Nothing
          , documentallowedidtypes = [EmailIdentification]
          , authornameplacements = []
          , authoremailplacements = []
          , authorcompanyplacements = []
          , authornumberplacements = []
          , authorotherfields = []
          }
  modify $ insert doc
  return doc

fileMovedToAWS :: FileID 
               -> BS.ByteString
               -> BS.ByteString
               -> Update Documents (Either String Document)
fileMovedToAWS fileid' bucket url = do
  documents <- ask
  case getOne (documents @= fileid') of
    Nothing -> return $ Left "no such file id"
    Just document ->
        modifyDocument (documentid document) $ moved
  where
    moved doc@Document{documentfiles,documentsealedfiles} =
        Right $ doc { documentfiles = map moved1 documentfiles
                    , documentsealedfiles = map moved1 documentsealedfiles
                    }
    moved1 file@File{ fileid
                    , filestorage = FileStorageMemory _
                    } | fileid == fileid' =
                                 file { filestorage = FileStorageAWS bucket url }
                      | otherwise = file
    moved1 file = file

getDocumentByFileID :: FileID 
                       -> Query Documents (Either String Document)
getDocumentByFileID fileid' = do
  documents <- ask
  case getOne (documents @= fileid') of
    Nothing -> return $ Left "no such file id"
    Just document -> return $ Right document

attachFile :: DocumentID 
           -> BS.ByteString 
           -> BS.ByteString 
           -> Update Documents (Either String Document)
attachFile documentid filename1 content = do
  documents <- ask
  fileid2 <- getUnique documents FileID
  modifyDocument documentid $ \document ->
      let nfile = File { fileid = fileid2
                       , filename = filename1
                       , filestorage = FileStorageMemory content
                       }
      in Right $ document { documentfiles = documentfiles document ++ [nfile] }

attachSealedFile :: DocumentID 
                 -> BS.ByteString 
                 -> BS.ByteString 
                 -> Update Documents (Either String Document)
attachSealedFile documentid filename1 content = do
  documents <- ask
  fileid2 <- getUnique documents FileID
  modifyDocument documentid $ \document ->
      let nfile = File { fileid = fileid2
                       , filename = filename1
                       , filestorage = FileStorageMemory content
                       }
      in Right $ document { documentsealedfiles = documentsealedfiles document ++ [nfile] }

updateDocument :: MinutesTime
               -> DocumentID
               -> [SignatoryDetails]
               -> Maybe Int
               -> BS.ByteString
               -> User
               -> SignatoryDetails
               -> [IdentificationType]
               -> Update Documents Document
updateDocument time documentid signatories daystosign invitetext author authordetails idtypes = do
  documents <- ask
  let Just document = getOne (documents @= documentid)
      authoremail = unEmail $ useremail $ userinfo author
  signatorylinks <- sequence $ map (signLinkFromDetails [(authoremail, author)]) signatories
  let doc2 = document { documentsignatorylinks = signatorylinks
                      , documentdaystosign = daystosign 
                      , documentmtime = time
                      , documentinvitetext = invitetext
                      , documentallowedidtypes = idtypes
                      , authornameplacements = signatorynameplacements authordetails
                      , authoremailplacements = signatoryemailplacements authordetails
                      , authorcompanyplacements = signatorycompanyplacements authordetails
                      , authornumberplacements = signatorynumberplacements authordetails
                      , authorotherfields = signatoryotherfields authordetails
                      }
  if documentstatus document == Preparation
     then do
       modify (updateIx documentid doc2)
       return doc2
     else
         return document
                     
timeoutDocument :: DocumentID
                -> MinutesTime
                -> Update Documents (Either String Document)
timeoutDocument documentid time = do
  modifyDocument documentid $ \document ->
      let
          newdocument = document { documentstatus = Timedout }
      in case documentstatus document of
           Pending -> Right newdocument
           _ -> Left "Illegal document status change"

signDocument :: DocumentID
             -> SignatoryLinkID 
             -> MinutesTime 
             -> Word32
             -> Maybe SignatureInfo
             -> [(BS.ByteString, BS.ByteString)]
             -> Update Documents (Either String Document)
signDocument documentid signatorylinkid1 time ipnumber msiginfo fields = do
  modifyDocument documentid $ \document ->
      let
          signeddocument = document { documentsignatorylinks = newsignatorylinks }
          newsignatorylinks = map maybesign (documentsignatorylinks document)
          maybesign link@(SignatoryLink {signatorylinkid, signatorydetails} ) 
              | signatorylinkid == signatorylinkid1 = 
                  link { maybesigninfo = Just (SignInfo time ipnumber)
                       , signatorydetails = updateWithFields fields signatorydetails
                       , signatorysignatureinfo = msiginfo
                    }
          maybesign link = link
          authorid = unAuthor $ documentauthor signeddocument
          allbutauthor = filter ((maybe True ((/= authorid) . unSignatory)) . maybesignatory) newsignatorylinks
          allsignedbutauthor = all (isJust . maybesigninfo) allbutauthor
          isallsigned = all (isJust . maybesigninfo) newsignatorylinks
          
          -- Check if there are custom fields in any signatory (that is, not author)
          hasfields = any ((any (not . fieldfilledbyauthor)) . (signatoryotherfields . signatorydetails)) (documentsignatorylinks document)

          updateWithFields [] sd = sd
          updateWithFields ((name, value):fs) sd 
              | name == BS.fromString "sigco" = updateWithFields fs sd { signatorycompany = value }
              | name == BS.fromString "signr" = updateWithFields fs sd { signatorynumber = value }
              | otherwise = updateWithFields fs sd { signatoryotherfields = updateOtherFields name value (signatoryotherfields sd) }

          updateOtherFields _    _     []      = []
          updateOtherFields name value (f@FieldDefinition { fieldlabel }:fs)  
              | name == fieldlabel = f { fieldvalue = value} : fs
              | otherwise          = f : updateOtherFields name value fs

          signeddocument2 = 
              if isallsigned
              then signeddocument { documentstatus = Closed }
              else if allsignedbutauthor 
                   then signeddocument { documentstatus = AwaitingAuthor }
                   else signeddocument

      in case documentstatus document of
           Pending ->  Right signeddocument2
           Timedout -> Left "Förfallodatum har passerat"
           _ ->        Left ("Bad document status: " ++ show (documentstatus document))

signWithUserID [] _ _ _ = []
signWithUserID (s:ss) id sinfo msiginfo
    | maybe False (((==) id) . unSignatory) (maybesignatory s) = s {maybesigninfo = sinfo, maybeseeninfo = sinfo, signatorysignatureinfo = msiginfo} : ss
    | otherwise = s : signWithUserID ss id sinfo msiginfo

authorSendDocument :: DocumentID
                   -> MinutesTime
                   -> Word32
                   -> User
                   -> Maybe SignatureInfo
                   -> Update Documents (Either String Document)
authorSendDocument documentid time ipnumber author msiginfo =
    modifyDocument documentid $ \document ->
        case documentstatus document of
          Preparation -> 
              let timeout = do
                             days <- documentdaystosign document 
                             return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                  authorid = userid author
                  sinfo = Just (SignInfo time ipnumber)
              in Right $ document { documenttimeouttime = timeout
                                  , documentmtime = time
                                  , documentstatus = Pending
                                  , documenthistory = documenthistory document ++ [DocumentHistoryInvitationSent time ipnumber]
                                  }
              
          Timedout -> Left "Förfallodatum har passerat" -- possibly quite strange here...
          _ ->        Left ("Bad document status: " ++ show (documentstatus document))


-- maybe this goes away
authorSignDocument :: DocumentID
                   -> MinutesTime
                   -> Word32
                   -> User
                   -> Maybe SignatureInfo
                   -> Update Documents (Either String Document)
authorSignDocument documentid time ipnumber author msiginfo =
    modifyDocument documentid $ \document ->
        case documentstatus document of
          Preparation -> 
              let timeout = do
                             days <- documentdaystosign document 
                             return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                  authorid = userid author
                  sinfo = Just (SignInfo time ipnumber)
              in Right $ document { documenttimeouttime = timeout
                                  , documentmtime = time
                                  , documentsignatorylinks = signWithUserID (documentsignatorylinks document) authorid sinfo msiginfo
                                  , documentstatus = Pending
                                  , documenthistory = documenthistory document ++ [DocumentHistoryInvitationSent time ipnumber]
                                  }
              
          Timedout -> Left "Förfallodatum har passerat" -- possibly quite strange here...
          _ ->        Left ("Bad document status: " ++ show (documentstatus document))
  
getMagicHash :: Update Documents MagicHash
getMagicHash = getRandom

setSignatoryLinks :: DocumentID -> [SignatoryLink] -> Update Documents (Either String Document)
setSignatoryLinks docid links =
    modifyDocument docid (\doc -> Right doc { documentsignatorylinks = links })

modifyDocument :: DocumentID 
               -> (Document -> Either String Document) 
               -> Update Documents (Either String Document)
modifyDocument docid action = modifyDocument' docid (return . action)
                
modifyDocument' :: DocumentID 
               -> (Document ->  Update Documents (Either String Document)) 
               -> Update Documents (Either String Document)
modifyDocument' docid action = do
  documents <- ask
  case getOne (documents @= docid) of
    Nothing -> return $ Left "no such document"
    Just document -> 
        do
        actionresult <- action document
        case actionresult of
          Left message -> return $ Left message
          Right newdocument -> 
              do
                when (documentid newdocument /= docid) $ error "new document must have same id as old one"
                modify (updateIx docid newdocument)
                return $ Right newdocument

rejectDocument :: DocumentID
               -> SignatoryLinkID 
               -> MinutesTime 
               -> Word32 
               -> Update Documents (Either String Document)
rejectDocument documentid signatorylinkid1 time ipnumber = do
  modifyDocument documentid $ \document ->
      let
          newdocument = document { documentstatus = Rejected }
          -- FIXME: need to say who has cancelled the document
          -- what his IP was, and time of happening
      in case documentstatus document of
           Pending ->  Right newdocument
           Timedout -> Left "Förfallodatum har passerat"
           _ ->        Left "Bad document status"
  

-- | 'markDocumentSeen' should set the time when the document was seen
-- first time by the user. It should change the first seen time later
-- on.
markDocumentSeen :: DocumentID 
                 -> SignatoryLinkID 
                 -> MinutesTime 
                 -> Word32
                 -> Update Documents (Maybe Document)
markDocumentSeen documentid signatorylinkid1 time ipnumber = do
  documents <- ask
  case getOne (documents @= documentid) of
    Nothing -> return Nothing
    Just document -> do
      let document' = document { documentsignatorylinks = s }
          s = map c (documentsignatorylinks document)
          c l@(SignatoryLink {signatorylinkid, maybeseeninfo})
            | signatorylinkid == signatorylinkid1 && maybeseeninfo==Nothing = 
              l { maybeseeninfo = Just (SignInfo time ipnumber) }
            | otherwise = l
      modify (updateIx documentid document')
      return (Just document')


-- | We set info about delivering invitation. On undeliver we autocancel document
setInvitationDeliveryStatus::SignatoryLinkID -> MailsDeliveryStatus -> Update Documents (Maybe Document)
setInvitationDeliveryStatus siglnkid status = do
                                               documents <- ask 
                                               case getOne (documents @= siglnkid) of   
                                                    Nothing -> return Nothing
                                                    Just doc -> do
                                                                let oldsls = documentsignatorylinks doc
                                                                let newsls = for oldsls $ \sl -> if (signatorylinkid sl == siglnkid)
                                                                                                 then sl {invitationdeliverystatus = status}
                                                                                                 else sl
                                                                let newdoc = doc {documentsignatorylinks = newsls}           
                                                                modify (updateIx (documentid doc) newdoc)
                                                                return $ Just newdoc


getDocumentStats :: Query Documents DocStats
getDocumentStats = do
  documents <- ask
  let signatureCountForDoc :: Document -> Int
      signatureCountForDoc doc = length $ filter (isJust . maybesigninfo) (documentsignatorylinks doc)
  return DocStats {
                      doccount = (size documents)
                    , signaturecount = sum $ map signatureCountForDoc (toList documents)
                   }

fileModTime :: FileID -> Query Documents MinutesTime
fileModTime fileid = do
  documents <- ask
  let Just doc = getOne (documents @= fileid)
  return (documentmtime doc)

saveDocumentForSignedUser :: DocumentID -> UserID -> SignatoryLinkID 
                          -> Update Documents (Maybe Document)
saveDocumentForSignedUser documentid userid signatorylinkid1 = do
  documents <- ask
  case getOne (documents @= documentid) of
    Nothing -> return Nothing
    Just document -> do
      let signeddocument = document { documentsignatorylinks = newsignatorylinks }
          newsignatorylinks = map maybesign (documentsignatorylinks document)
          maybesign x@(SignatoryLink {signatorylinkid} ) 
            | signatorylinkid == signatorylinkid1 = 
              x { maybesignatory = Just (Signatory userid)
                }
          maybesign x = x
      modify (updateIx documentid signeddocument)
      return (Just signeddocument)

getNumberOfDocumentsOfUser :: User -> Query Documents Int
getNumberOfDocumentsOfUser user = do
  documents <- ask
  let numdoc = size (documents @= Author (userid user))
  return numdoc

getDocumentStatsByUser :: User -> Query Documents DocStats
getDocumentStatsByUser user = do
  doccount' <- getNumberOfDocumentsOfUser user
  sigdocs <- getDocumentsBySignatory user
  let signaturecount' = length $ filter (isSigned . relevantSigLink) sigdocs
      relevantSigLink :: Document -> SignatoryLink
      relevantSigLink doc = head $ filter (isMatchingSignatoryLink user) (documentsignatorylinks doc)
      isSigned :: SignatoryLink -> Bool
      isSigned = isJust . maybesigninfo
  return DocStats { doccount = doccount', 
                    signaturecount = signaturecount' }

setDocumentTimeoutTime :: DocumentID -> TimeoutTime -> Update Documents Document
setDocumentTimeoutTime documentid timeouttime = do
  -- check if document status change is a legal transition
  documents <- ask
  let Just document = getOne (documents @= documentid)
  let newdoc = document { documenttimeouttime = Just timeouttime }
  modify (updateIx documentid newdoc)
  return newdoc

archiveDocuments :: User -> [DocumentID] -> Update Documents ()
archiveDocuments user docidlist = do
  -- FIXME: can use a fold here
  forM_ docidlist $ \docid -> do
      modify $ \documents -> case getOne (documents @= docid) of
                               Just doc -> if (isAuthor doc user)
                                            then updateIx docid (doc { documentdeleted = True }) documents
                                            else documents
                               Nothing -> documents
      

fragileTakeOverDocuments :: User -> User -> Update Documents ()
fragileTakeOverDocuments destuser srcuser = do
  documents <- ask
  let hisdocuments = documents @= Author (userid srcuser)
      sigdocuments = filter ((any (isMatchingSignatoryLink srcuser)) . documentsignatorylinks) (toList documents)
  mapM_ (updateDoc takeoverAsAuthor) (IxSet.toList hisdocuments)
  mapM_ (updateDoc takeoverAsSignatory) sigdocuments
  return ()
  where updateDoc takeover document = modify $ updateIx (documentid document) (takeover document)
        takeoverAsAuthor document = document { documentauthor = Author (userid destuser) }
        takeoverAsSignatory document = document { documentsignatorylinks = takeoverSigLinks (documentsignatorylinks document) }
        takeoverSigLinks siglinks = (map takeoverSigLink matching) ++ others
                                    where (matching, others) = partition (isMatchingSignatoryLink srcuser) siglinks 
        takeoverSigLink siglink = siglink {maybesignatory = Just (Signatory (userid destuser)),
                                           signatorydetails = takeoverSigDetails (signatorydetails siglink) }
        takeoverSigDetails sigdetails = sigdetails {signatoryname = BS.intercalate (BS.fromString " ") [userfstname info, usersndname info],
                                                    signatorycompany = usercompanyname info,
                                                    signatoryemail = unEmail $ useremail info }
                                        where info = userinfo destuser

--This is only for Eric functionality with awayting author
--We should add current state checkers here (not co cancel closed documents etc.)
closeDocument :: DocumentID 
              -> MinutesTime
              -> Word32
              -> User
              -> Maybe SignatureInfo
              -> Update Documents (Maybe Document)
closeDocument docid time ipnumber author msiginfo = do
  doc <- modifyDocument docid 
         (\document -> let timeout = do
                                      days <- documentdaystosign document 
                                      return $ TimeoutTime $ (days * 24 *60) `minutesAfter` time
                           authorid = userid author
                           sinfo = Just (SignInfo time ipnumber)
                  
                       in Right $ document { documenttimeouttime = timeout
                                           , documentmtime = time
                                           , documentsignatorylinks = signWithUserID (documentsignatorylinks document) authorid sinfo msiginfo
                                           , documentstatus = Closed
                                           })
  case doc of
    Left _ -> return Nothing
    Right d -> return $ Just d

--We should add current state checkers here (not co cancel closed documents etc.)
cancelDocument :: DocumentID -> Update Documents (Maybe Document)
cancelDocument docid = do
  doc <- modifyDocument docid (\d -> Right $ d { documentstatus = Canceled }) 
  case doc of
    Left _ -> return Nothing
    Right d -> return $ Just d

{-
withdrawnDocument:: DocumentID -> Update Documents (Maybe Document)
withdrawnDocument docid = do
  doc <- modifyDocument docid (\d -> Right $ d { documentstatus = Withdrawn }) 
  case doc of
    Left _ -> return Nothing
    Right d -> return $ Just d
-}

getFilesThatShouldBeMovedToAmazon :: Query Documents [File]
getFilesThatShouldBeMovedToAmazon = do
  documents <- ask
  let doclist = IxSet.toList documents
  let getFiles Document{documentfiles,documentsealedfiles} = documentfiles ++ documentsealedfiles
  let allFiles = concatMap getFiles doclist
  let getID file@File{ filestorage = FileStorageMemory _ } = [file]
      getID _ = []
  return (concatMap getID allFiles)


{- Restarts document,    
    Checks the autor and status
    Clears sign links and stuff
    Sets status to Pending
    
    It is passed a document 
-} 
restartDocument :: DocumentID -> User-> Update Documents (Either String Document)
restartDocument docid user =
   modifyDocument' docid (\d -> tryToGetRestarted d user)    


{- | 
    Returns restarted version of document
    Checks the autor and status
    Clears sign links and stuff
-}
tryToGetRestarted::Document->User-> Update Documents (Either String Document)
tryToGetRestarted doc user= 
                            if (documentstatus doc `notElem` [Canceled,Timedout, Rejected])
                             then return $ Left $ "Can't restart document with " ++ (show $ documentstatus doc) ++ " status"
                             else if (not $ isAuthor doc user)
                                   then return $ Left $ "Can't restart document is You are not it's author"
                                   else do 
                                         doc' <-clearSignInfofromDoc doc user
                                         return $ Right doc'


clearSignInfofromDoc doc author = do
  let signatoriesDetails = map signatorydetails $ documentsignatorylinks doc
      authoremail = unEmail $ useremail $ userinfo author
  newSignLinks <- sequence $ map (signLinkFromDetails [(authoremail, author)]) signatoriesDetails
  return doc {documentstatus=Preparation,
              documenttimeouttime = Nothing,
              documentsignatorylinks = newSignLinks
             }

findMaybeUserByEmail [] _ = Nothing
findMaybeUserByEmail ((email, user):eus) em 
    | email == em = Just user
    | otherwise   = findMaybeUserByEmail eus em

changeSignatoryEmailWhenUndelivered::DocumentID -> SignatoryLinkID -> BS.ByteString ->  Update Documents (Either String Document)
changeSignatoryEmailWhenUndelivered did slid email = modifyDocument did $ changeEmail
  where changeEmail doc = let signlinks = documentsignatorylinks doc
                              mnsignlink = do
                                           sl <- find ((== slid) . signatorylinkid) signlinks
                                           when (invitationdeliverystatus sl /= Undelivered ) Nothing
                                           return $ sl {invitationdeliverystatus = Unknown, signatorydetails = (signatorydetails sl) {signatoryemail = email}}
                                           
                          in case mnsignlink  of
                           Just nsl -> let sll = for signlinks $ \sl -> if ( slid == signatorylinkid sl) then nsl else sl      
                                       in  if (documentstatus doc == Pending || documentstatus doc == AwaitingAuthor)
                                            then Right $ doc {documentsignatorylinks = sll}
                                            else Left "We cant change status of not pending documents"
                                            
                           Nothing -> Left "We could not find signatory"            
                     
--UTILS - have to be put before creating action constructors
signLinkFromDetails emails details = do
          sg <- ask
          linkid <- getUnique sg SignatoryLinkID
          magichash <- getRandom
          let muser = findMaybeUserByEmail emails (signatoryemail details)
              msig = maybe Nothing (Just . Signatory . userid) muser
          return $ SignatoryLink 
                     { signatorylinkid = linkid
                     , signatorydetails = details
                     , signatorymagichash = magichash
                     , maybesignatory = msig
                     , maybesigninfo  = Nothing
                     , maybeseeninfo  = Nothing
                     , invitationdeliverystatus = Unknown
                     , signatorysignatureinfo = Nothing
                     }

getUniqueSignatoryLinkID :: Update Documents SignatoryLinkID
getUniqueSignatoryLinkID = do
  sg <- ask
  linkid <- getUnique sg SignatoryLinkID
  return linkid

setDocumentTrustWeaverReference :: DocumentID -> String -> Update Documents (Either String Document)
setDocumentTrustWeaverReference documentid reference = do
  modifyDocument documentid $ \document ->
      let
          newdocument = document { documenttrustweaverreference = Just (BS.fromString reference) }
      in Right newdocument
  
errorDocument :: DocumentID -> String -> Update Documents (Either String Document)
errorDocument documentid errormsg = 
  modifyDocument documentid $ \document ->
      let
          newdocument = document { documentstatus = DocumentError errormsg }
      in Right newdocument

{- |
   The user is the author of the document
 -}
class MayBeAuthor a where
  isAuthor::Document->a->Bool

instance MayBeAuthor User where
  isAuthor d u = isAuthor d $ userid u
  
instance MayBeAuthor UserID where
  isAuthor d uid = uid == ( unAuthor . documentauthor $ d)   
  
instance MayBeAuthor SignatoryLink where
  isAuthor d sl = case maybesignatory sl of
                   Just s -> unSignatory s == ( unAuthor . documentauthor $ d)
                   Nothing -> False

instance (MayBeAuthor a) => MayBeAuthor (Maybe a) where
  isAuthor d (Just s) = isAuthor d s
  isAuthor _ _ = False

anyInvitationUndelivered =  not . Prelude.null . undeliveredSignatoryLinks
undeliveredSignatoryLinks doc =  filter ((== Undelivered) . invitationdeliverystatus) $ documentsignatorylinks doc

{- |
   Build a SignatoryDetails from a User with no fields
 -}
signatoryDetailsFromUser user = 
    SignatoryDetails { signatoryname = userfullname user
                     , signatoryemail = unEmail $ useremail $ userinfo user
                     , signatorycompany = usercompanyname $ userinfo user
                     , signatorynumber = usercompanynumber $ userinfo user
                     , signatorynameplacements = []
                     , signatorycompanyplacements = []
                     , signatoryemailplacements = []
                     , signatorynumberplacements = []
                     , signatoryotherfields = []
                     }

-- create types for event serialization
$(mkMethods ''Documents [ 'getDocuments
                        , 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'newDocument
                        , 'getDocumentByDocumentID
                        , 'getTimeoutedButPendingDocuments
                        , 'updateDocument
                        , 'signDocument
                        , 'authorSignDocument
                        , 'authorSendDocument
                        , 'rejectDocument
                        , 'attachFile
                        , 'attachSealedFile
                        , 'markDocumentSeen
                        , 'setInvitationDeliveryStatus
                        , 'getDocumentStats
                        , 'getDocumentStatsByUser
                        , 'fileModTime
                        , 'saveDocumentForSignedUser
                        , 'getDocumentsByUser
                        , 'getNumberOfDocumentsOfUser
                        , 'setDocumentTimeoutTime
                        , 'setDocumentTrustWeaverReference
                        , 'archiveDocuments
                        , 'timeoutDocument
                        , 'closeDocument
                        , 'cancelDocument
                        -- , 'withdrawnDocument
                        , 'fileMovedToAWS

                          -- admin only area follows
                        , 'fragileTakeOverDocuments
                        , 'getFilesThatShouldBeMovedToAmazon
                        , 'restartDocument
                        , 'changeSignatoryEmailWhenUndelivered
                        , 'setSignatoryLinks
                        , 'getUniqueSignatoryLinkID
                        , 'getMagicHash
                          
                        , 'getDocumentByFileID
                        , 'errorDocument
                        ])
