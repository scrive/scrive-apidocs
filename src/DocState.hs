{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, TypeSynonymInstances,
    GeneralizedNewtypeDeriving, StandaloneDeriving, NamedFieldPuns #-}

module DocState where
import Happstack.Data
import Happstack.State
import "mtl" Control.Monad.Reader (ask)
import "mtl" Control.Monad.State (modify)
import "mtl" Control.Monad.Trans
import UserState
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
import Data.List (zipWith4)
import System.Random
import Data.Word
import Data.Int
import System.Log.Logger (errorM)
import qualified Data.Map as Map
import qualified Data.Set as Set

$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
      newtype Author = Author { unAuthor :: UserID }
      newtype DocumentID = DocumentID { unDocumentID :: Int64 }
      newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int }
      newtype FileID = FileID { unFileID :: Int }
      newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }

      -- added by Eric Normand for template system
      -- Defines a new field to be placed in a contract
      data FieldDefinition = FieldDefinition
          { fieldlabel :: BS.ByteString 
          , fieldvalue :: BS.ByteString
          , fieldplacements :: [FieldPlacement]
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
      data SignatoryLink = SignatoryLink 
          { signatorylinkid    :: SignatoryLinkID
          , signatorydetails   :: SignatoryDetails
          , signatorymagichash :: MagicHash
          , maybesignatory     :: Maybe Signatory
          , maybesigninfo      :: Maybe SignInfo
          , maybeseeninfo      :: Maybe SignInfo
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

         * Preparation: can add/remove signatories, edit document,
           edit dates and times. Nobody else can see the document.
           Goes to ReadyToSign or Canceled state.
         * Pending: people can sign document. When last person
           has signed, goes to Closed state. When owner cancels document
           goes to Canceled state. When time is up, goes to Timedout
           state.
         * Closed: everybody signed. This is final state.
         * Canceled: this can be final state or we can go back to 
           Preparation.
         * Timedout: this works as autocancel and has exactly same 
           properties. Can go to Preparation.

         Transitions:
         * Preparation to ReadyToSign: invitations are sent.
         * ReadyToSign to Closed: info about closed deal is sent to
           everybody involved.
         * ReadyToSign/Preparation to Cancel: mail about cancel to 
           all who have signed it already is sent. 
         * ReadyToSign/Preparation to Timedout: mail about timeout to 
           all who have signed it already is sent.

         Allowed actions:
         * Preparation: change document, change title, add/rem signatories
         * ReadyToSign: change email of a signatory
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

      data ChargeMode = ChargeInitialFree   -- initial 5 documents are free
                      | ChargeNormal        -- value times number of people involved

      data DocumentHistoryEntry = DocumentHistoryCreated
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
      data Document = Document
          { documentid               :: DocumentID
          , documenttitle            :: BS.ByteString
          , documentauthor           :: Author
          , documentsignatorylinks   :: [SignatoryLink]  
          , documentfiles            :: [File]
          , documentstatus           :: DocumentStatus
          , documentctime            :: MinutesTime
          , documentmtime            :: MinutesTime
          , documentchargemode       :: ChargeMode
          , documentdaystosign       :: Int
          , documenttimeouttime      :: Maybe TimeoutTime 
          , documentdeleted          :: Bool -- should not appear in list
          , documentauthordetails    :: SignatoryDetails
          , documentmaybesigninfo    :: Maybe SignInfo      -- about the author signed the document |should be droped and check at runtime|
          , documenthistory          :: [DocumentHistoryEntry]
          , documentinvitetext       :: BS.ByteString

          -- we really should keep history here so we know what happened
          }
      data File = File 
          { fileid       :: FileID
          , filename     :: BS.ByteString
          , filepdf      :: BS.ByteString 
          }
      data File1 = File1 
          { fileid1       :: FileID
          , filename1     :: BS.ByteString
          , filepdf1      :: BS.ByteString 
          , filejpgpages1 :: JpegPages
          }
      data File0 = File0 
          { fileid0       :: FileID
          , filename0     :: BS.ByteString
          , filepdf0      :: BS.ByteString 
          , filejpgpages0 :: [BS.ByteString]
          }

      data JpegPages = JpegPagesPending
                     | JpegPages [BS.ByteString]    -- FIXME: add width and height to each one
                     | JpegPagesError BS.ByteString -- normalization error with whole log from normalizer

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

deriving instance Show Document
deriving instance Show DocumentStatus
deriving instance Show ChargeMode
deriving instance Show Author

deriving instance Show FieldDefinition
deriving instance Show FieldPlacement


instance Show TimeoutTime where
    showsPrec prec = showsPrec prec . unTimeoutTime

deriving instance Show SignatoryLink
deriving instance Show SignatoryLink0
deriving instance Show SignatoryLink1
deriving instance Show SignInfo
deriving instance Show SignInfo0
deriving instance Show SignatoryDetails
deriving instance Show SignatoryDetails0
deriving instance Show DocumentHistoryEntry

instance Show Signatory where
    showsPrec prec (Signatory userid) = showsPrec prec userid

instance Show DocumentID where
    showsPrec prec (DocumentID val) = 
        -- let s = show val in (++) (take (10-length s) "000000000" ++ s)
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

$(deriveSerialize ''FieldDefinition)
instance Version FieldDefinition

$(deriveSerialize ''FieldPlacement)
instance Version FieldPlacement

$(deriveSerialize ''SignInfo0)
instance Version SignInfo0

$(deriveSerialize ''SignInfo)
instance Version SignInfo where
    mode = extension 1 (Proxy :: Proxy SignInfo0)


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

$(deriveSerialize ''SignatoryLink)
instance Version SignatoryLink where
    mode = extension 3 (Proxy :: Proxy SignatoryLink2)

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


instance Migrate SignatoryLink2 SignatoryLink where
      migrate (SignatoryLink2 
          { signatorylinkid2
          , signatorydetails2
          , signatorymagichash2
          , maybesignatory2
          , maybesigninfo2
          , maybeseentime2
          }) = SignatoryLink 
          { signatorylinkid    = signatorylinkid2
          , signatorydetails   = signatorydetails2
          , signatorymagichash = signatorymagichash2
          , maybesignatory     = maybesignatory2
          , maybesigninfo      = maybesigninfo2
          , maybeseeninfo      = maybe Nothing (\t -> Just (SignInfo t 0)) maybeseentime2
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

$(deriveSerialize ''Document)
instance Version Document where
    mode = extension 4 (Proxy :: Proxy Document3)

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

instance Migrate Document3 Document where
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
          }) = Document
          { documentid = documentid3
          , documenttitle = documenttitle3
          , documentauthor = documentauthor3
          , documentsignatorylinks = documentsignatorylinks3
          , documentfiles = documentfiles3
          , documentstatus = documentstatus3
          , documentctime = documentctime3
          , documentmtime = documentmtime3
          , documentchargemode = documentchargemode3
          , documentdaystosign = documentdaystosign3
          , documenttimeouttime = documenttimeouttime3
          , documentdeleted = documentdeleted3
          , documentauthordetails = documentauthordetails3
          , documentmaybesigninfo = documentmaybesigninfo3
          , documenthistory = documenthistory3
          , documentinvitetext = BS.empty
          }


$(deriveSerialize ''DocumentStatus)
instance Version DocumentStatus where


$(deriveSerialize ''ChargeMode)
instance Version ChargeMode where

$(deriveSerialize ''File)
instance Version File where
    mode = extension 2 (Proxy :: Proxy File1)

$(deriveSerialize ''File1)
instance Version File1 where
    mode = extension 1 (Proxy :: Proxy File0)

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
                , filejpgpages1 = JpegPages filejpgpages0
                }

instance Migrate File1 File where
    migrate (File1 
                { fileid1
                , filename1
                , filepdf1
                }) = File 
                { fileid = fileid1
                , filename = filename1
                , filepdf = filepdf1
                }


$(deriveSerialize ''JpegPages)
instance Version JpegPages where

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

getDocumentsByUser :: UserID -> Query Documents [Document]
getDocumentsByUser userid = do
    documents <- ask
    return $ toList (documents @= Author userid ||| documents @= Signatory userid)

getDocumentsBySignatory :: UserID -> Query Documents [Document]
getDocumentsBySignatory userid = do
    documents <- ask
    return $ toList (documents @= Signatory userid)
    
getTimeoutedButPendingDocuments  :: MinutesTime -> Query Documents [Document]
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
newDocument user@User{userid,userfullname,usercompanyname,usercompanynumber,useremail} title ctime isfree = do
  documents <- ask
  docid <- getUnique64 documents DocumentID
  let doc = Document
          { documentid = docid
          , documenttitle = title
          , documentauthor = Author userid
          , documentsignatorylinks = []
          , documentfiles = []
          , documentstatus = Preparation
          , documentctime = ctime
          , documentmtime = ctime
          , documentchargemode = if isfree then ChargeInitialFree else ChargeNormal
          , documentdaystosign = 7
          , documenttimeouttime = Nothing
          , documentdeleted = False
          , documentauthordetails = details
          , documentmaybesigninfo = Nothing
          , documenthistory = []
          , documentinvitetext = BS.empty
          }
      details = SignatoryDetails  
                { signatoryname = userfullname
                , signatorycompany = usercompanyname
                , signatorynumber = usercompanynumber
                , signatoryemail = unEmail $ useremail
                , signatorynameplacements = []
                , signatorycompanyplacements = []
                , signatoryemailplacements = []
                , signatorynumberplacements = []
                , signatoryotherfields = []
                }

  modify $ insert doc
  return doc

attachFile :: DocumentID 
           -> BS.ByteString 
           -> BS.ByteString 
           -> Update Documents ()
attachFile documentid filename1 content = do
  documents <- ask
  let Just document = getOne (documents @= documentid)
  fileid2 <- getUnique documents FileID
  let nfile = File { fileid = fileid2
                   , filename = filename1
                   , filepdf = content
                   }
  let document2 = document { documentfiles = documentfiles document ++ [nfile] }
  modify $ updateIx documentid document2

updateDocument :: MinutesTime
               -> DocumentID
               -> [SignatoryDetails]
               -> SignatoryDetails
               -> Int
               -> BS.ByteString
               -> Update Documents Document
updateDocument time documentid signatories author daystosign invitetext = do
  documents <- ask
  let Just document = getOne (documents @= documentid)
  signatorylinks <- sequence $ map mm signatories
  let doc2 = document { documentsignatorylinks = signatorylinks
                      , documentdaystosign = daystosign 
                      , documentmtime = time
                      , documentinvitetext = invitetext
                      , documentauthordetails = author
                      }
  if documentstatus document == Preparation
     then do
       modify (updateIx documentid doc2)
       return doc2
     else
         return document
  where mm details = do
          sg <- ask
          linkid <- getUnique sg SignatoryLinkID
          magichash <- getRandom
          return $ SignatoryLink 
                     { signatorylinkid = linkid
                     , signatorydetails = details
                     , signatorymagichash = magichash
                     , maybesignatory = Nothing
                     , maybesigninfo  = Nothing
                     , maybeseeninfo  = Nothing
                     }

updateDocumentStatus :: DocumentID
                     -> DocumentStatus 
                     -> MinutesTime
                     -> Word32
                     -> Update Documents (Either String Document)
updateDocumentStatus documentid newstatus time ipnumber = do
  -- check if document status change is a legal transition
  documents <- ask
  let Just document = getOne (documents @= documentid )
  let legal = (documentstatus document,newstatus) `elem`
              [ (Preparation,Pending)
              , (Pending,Canceled)
              , (Pending,Timedout)
              , (Pending,Closed)
              , (Canceled,Preparation)
              , (Timedout,Preparation)
              ]

  let newdoc = document { documentstatus = newstatus
                        , documentmtime = time
                        , documentmaybesigninfo = case newstatus of
                                                    Pending -> Just (SignInfo time ipnumber)
                                                    _ -> documentmaybesigninfo document
                        }
  if legal 
     then do
       modify (updateIx documentid newdoc)
       return $ Right newdoc
     else do
       return $ Left $ "Illegal document state change from:"++(show $ documentstatus document)++" to: " ++ (show newstatus)
         

signDocument :: DocumentID
             -> SignatoryLinkID 
             -> MinutesTime 
             -> Word32 
             -> Update Documents (Either String Document)
signDocument documentid signatorylinkid1 time ipnumber = do
  documents <- ask
  let Just document = getOne (documents @= documentid)
      signeddocument = document { documentsignatorylinks = newsignatorylinks }
      newsignatorylinks = map maybesign (documentsignatorylinks document)
      maybesign x@(SignatoryLink {signatorylinkid} ) 
          | signatorylinkid == signatorylinkid1 = 
              x { maybesigninfo = Just (SignInfo time ipnumber)
                }
      maybesign x = x
  if (documentstatus document == Pending)
   then do
        modify (updateIx documentid signeddocument)
        return (Right signeddocument)
   else return $ Left (case (documentstatus document) of  
                            Timedout -> "Förfallodatum har passerat"
                            _ ->        "Bad document status" )
  

-- ^ 'markDocumentSeen' should set the time when the document was seen
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
  

getDocumentStats :: Query Documents Int
getDocumentStats = do
  documents <- ask 
  return (size documents)


replaceFile :: DocumentID -> File -> Update Documents Document
replaceFile documentid file = do
  documents <- ask
  let Just doc = getOne (documents @= documentid)
  let newdoc = doc {documentfiles = [file]} -- FIXME: care about many files here
  modify (updateIx documentid newdoc)
  return newdoc

removeFileFromDoc :: DocumentID -> Update Documents Document
removeFileFromDoc documentid = do
  documents <- ask
  let Just doc = getOne (documents @= documentid)
  let newdoc = doc {documentfiles = []} -- FIXME: care about many files here
  modify (updateIx documentid newdoc)
  return newdoc

fileModTime :: FileID -> Query Documents MinutesTime
fileModTime fileid = do
  documents <- ask
  let Just doc = getOne (documents @= fileid)
  return (documentmtime doc)

fileByFileID :: FileID -> Query Documents (Maybe File)
fileByFileID fileid = do
  documents <- ask
  case getOne (documents @= fileid) of
    Just doc -> return (Just $ safehead "fileByFileID" $ documentfiles doc)
    Nothing -> return Nothing


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

setDocumentTimeoutTime :: DocumentID -> TimeoutTime -> Update Documents Document
setDocumentTimeoutTime documentid timeouttime = do
  -- check if document status change is a legal transition
  documents <- ask
  let Just document = getOne (documents @= documentid)
  let newdoc = document { documenttimeouttime = Just timeouttime }
  modify (updateIx documentid newdoc)
  return newdoc

archiveDocuments :: [DocumentID] -> Update Documents ()
archiveDocuments docidlist = do
  -- FIXME: can use a fold here
  forM_ docidlist $ \docid -> do
      modify $ \documents -> case getOne (documents @= docid) of
                               Just doc -> updateIx docid (doc { documentdeleted = True }) documents
                               Nothing -> documents
      

fragileTakeOverDocuments :: UserID -> UserID -> Update Documents ()
fragileTakeOverDocuments destuserid srcuserid = do
  documents <- ask
  let hisdocuments = documents @= Author srcuserid
      takeover document = modify $ updateIx (documentid document) (document { documentauthor = Author destuserid })
  mapM_ takeover (IxSet.toList hisdocuments)
  return ()


-- create types for event serialization
$(mkMethods ''Documents [ 'getDocuments
                        , 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'newDocument
                        , 'getDocumentByDocumentID
                        , 'getTimeoutedButPendingDocuments
                        , 'updateDocument
                        , 'updateDocumentStatus
                        , 'signDocument
                        , 'attachFile
                        , 'markDocumentSeen
                        , 'getDocumentStats
                        , 'replaceFile
                        , 'fileModTime
                        , 'fileByFileID
                        , 'removeFileFromDoc
                        , 'saveDocumentForSignedUser
                        , 'getDocumentsByUser
                        , 'getNumberOfDocumentsOfUser
                        , 'setDocumentTimeoutTime
                        , 'archiveDocuments

                          -- admin only area follows
                        , 'fragileTakeOverDocuments
                        ])



isAuthor::User->Document->Bool
isAuthor u d = (userid u) == ( unAuthor . documentauthor $ d)   