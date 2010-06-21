{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, TypeSynonymInstances,
    GeneralizedNewtypeDeriving, StandaloneDeriving, NamedFieldPuns #-}

module DocState where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import UserState
import Happstack.Data.IxSet
import qualified Data.ByteString.UTF8 as BS
import Control.Applicative ((<$>))
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Debug.Trace
import Misc
import Control.Monad
import Data.List (find)
import MinutesTime
import Control.Monad.Trans


$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
      newtype Author = Author { unAuthor :: UserID }
      newtype DocumentID = DocumentID { unDocumentID :: Int }
      newtype SignatoryLinkID = SignatoryLinkID { unSignatoryLinkID :: Int }
      newtype FileID = FileID { unFileID :: Int }

      data SignatoryLink = SignatoryLink 
          { signatorylinkid    :: SignatoryLinkID
          , signatoryname      :: BS.ByteString 
          , signatorycompany   :: BS.ByteString 
          , signatoryemail     :: BS.ByteString
          , maybesignatory     :: Maybe Signatory
          , maybesigninfo      :: Maybe SignInfo
          , maybeseentime      :: Maybe MinutesTime
          }
      data SignInfo = SignInfo
          { signtime :: MinutesTime
            -- some authorization info probably
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
          }
      data File = File 
          { fileid       :: FileID
          , filename     :: BS.ByteString
          , filepdf      :: BS.ByteString 
          , filejpgpages :: [BS.ByteString]
          }
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

instance Eq File where
    a == b = fileid a == fileid b

instance Ord File where
    compare a b | fileid a == fileid b = EQ
                | otherwise = compare (fileid a,filename a) 
                                      (fileid b,filename b)

instance Show SignatoryLinkID where
    showsPrec prec (SignatoryLinkID x) = showsPrec prec x

instance Show SignatoryLink where
    showsPrec prec (SignatoryLink _ name company email Nothing Nothing Nothing) = 
        (++) (BS.toString name ++ " <" ++ BS.toString email ++ "> never seen")
    showsPrec prec (SignatoryLink _ name company email Nothing Nothing (Just time)) = 
        (++) (BS.toString name ++ " <" ++ BS.toString email ++ "> seen " ++ show time)
    showsPrec prec (SignatoryLink _ name company email _ (Just signinfo) _) = 
        (++) $ "Signed by " ++ (BS.toString name ++ " <" ++ BS.toString email ++ "> on " ++ show (signtime signinfo))

deriving instance Show Document
deriving instance Show DocumentStatus
deriving instance Show ChargeMode
deriving instance Show Author
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

$(deriveSerialize ''SignInfo)
instance Version SignInfo

$(deriveSerialize ''SignatoryLink)
instance Version SignatoryLink

$(deriveSerialize ''SignatoryLinkID)
instance Version SignatoryLinkID

$(deriveSerialize ''DocumentID)
instance Version DocumentID

$(deriveSerialize ''Author)
instance Version Author

$(deriveSerialize ''Signatory)
instance Version Signatory where

$(deriveSerialize ''Document0)
instance Version Document0 where

$(deriveSerialize ''Document)
instance Version Document where
    mode = extension 1 (Proxy :: Proxy Document0)

instance Migrate Document0 Document where
      migrate (Document0
          { documentid0
          , documenttitle0
          , documentauthor0
          , documentsignatorylinks0
          , documentfiles0
          , documentstatus0
          , documentctime0
          , documentmtime0
          }) = Document
          { documentid = documentid0
          , documenttitle = documenttitle0
          , documentauthor = documentauthor0
          , documentsignatorylinks = documentsignatorylinks0
          , documentfiles = documentfiles0
          , documentstatus = documentstatus0
          , documentctime = documentctime0
          , documentmtime = documentmtime0
          , documentchargemode = ChargeInitialFree
          }


$(deriveSerialize ''DocumentStatus)
instance Version DocumentStatus where


$(deriveSerialize ''ChargeMode)
instance Version ChargeMode where

$(deriveSerialize ''File)
instance Version File where

$(deriveSerialize ''FileID)
instance Version FileID where


$(inferIxSet "Documents" ''Document 'noCalcs 
                 [ ''DocumentID
                 , ''Author
                 , ''Signatory
                 , ''SignatoryLinkID
                 , ''FileID
                 ])

instance Component Documents where
  type Dependencies Documents = End
  initialValue = empty

  
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

newDocument :: UserID 
            -> BS.ByteString
            -> MinutesTime 
            -> Bool -- is free?
            -> Update Documents Document
newDocument userid title ctime isfree = do
  documents <- ask
  docid <- getUnique documents DocumentID
  let doc = Document docid title (Author userid) [] []
            Preparation ctime ctime (if isfree then ChargeInitialFree else ChargeNormal)
  modify $ insert doc
  return doc

attachFile :: DocumentID -> BS.ByteString -> BS.ByteString 
           -> [BS.ByteString] -> Update Documents ()
attachFile documentid filename1 content jpgpages = do
  documents <- ask
  let Just document = getOne (documents @= documentid)
  fileid2 <- getUnique documents FileID
  let nfile = File { fileid = fileid2
                   , filejpgpages = jpgpages
                   , filename = filename1
                   , filepdf = content
                   }
  let document2 = document { documentfiles = documentfiles document ++ [nfile] }
  modify $ updateIx documentid document2

updateDocumentSignatories :: Document -> [BS.ByteString] -> [BS.ByteString] 
                          -> [BS.ByteString] -> Update Documents Document
updateDocumentSignatories document signatorynames signatorycompanies signatoryemails = do
  signatorylinks <- sequence $ zipWith3 mm signatorynames signatorycompanies signatoryemails
  let doc2 = document { documentsignatorylinks = signatorylinks }
  if documentstatus document == Preparation
     then do
       modify (updateIx (documentid doc2) doc2)
       return doc2
     else
         return document
  where mm name company email = do
          sg <- ask
          x <- getUnique sg SignatoryLinkID
          return $ SignatoryLink x name company email Nothing Nothing Nothing

updateDocumentStatus :: Document 
                     -> DocumentStatus 
                     -> Update Documents Document
updateDocumentStatus document newstatus = do
  -- check if document status change is a legal transition
  let legal = (documentstatus document,newstatus) `elem`
              [ (Preparation,Pending)
              , (Pending,Canceled)
              , (Pending,Timedout)
              , (Pending,Closed)
              , (Canceled,Preparation)
              , (Timedout,Preparation)
              ]

  let newdoc = document { documentstatus = newstatus }
  if legal 
     then do
       modify (updateIx (documentid newdoc) newdoc)
       return newdoc
     else do
       -- FIXME: throw some error? log it somewhere?
       return document
  

signDocument :: DocumentID -> SignatoryLinkID 
             -> MinutesTime -> Update Documents (Maybe Document)
signDocument documentid signatorylinkid1 time = do
  documents <- ask
  let Just document = getOne (documents @= documentid)
      signeddocument = document { documentsignatorylinks = newsignatorylinks }
      newsignatorylinks = map maybesign (documentsignatorylinks document)
      maybesign x@(SignatoryLink {signatorylinkid} ) 
          | signatorylinkid == signatorylinkid1 = 
              x { maybesigninfo = Just (SignInfo time)
                }
      maybesign x = x
  modify (updateIx documentid signeddocument)
  return (Just signeddocument)
  

getFilePageJpg :: FileID -> Int -> Query Documents (Maybe BS.ByteString)
getFilePageJpg xfileid pageno = do
  documents <- ask
  return $ do -- maybe monad!
    document <- getOne (documents @= xfileid)
    nfile <- find (\f -> fileid f == xfileid) (documentfiles document)
    let jpgs = filejpgpages nfile
    jpg <- return (jpgs!!(pageno-1))
    return jpg

markDocumentSeen :: DocumentID -> SignatoryLinkID 
                 -> MinutesTime -> Update Documents (Maybe Document)
markDocumentSeen documentid signatorylinkid1 time = do
  documents <- ask
  case getOne (documents @= documentid) of
    Nothing -> return Nothing
    Just document -> do
      let document' = document { documentsignatorylinks = s }
          s = map c (documentsignatorylinks document)
          c l@(SignatoryLink {signatorylinkid})
            | signatorylinkid == signatorylinkid1 = 
              l { maybeseentime = Just time }
            | otherwise = l
      modify (updateIx documentid document')
      return (Just document)
  

getDocumentStats :: Query Documents Int
getDocumentStats = do
  documents <- ask 
  return (size documents)


replaceFile :: Document -> File -> Update Documents Document
replaceFile (Document{documentid}) file = do
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

-- create types for event serialization
$(mkMethods ''Documents [ 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'newDocument
                        , 'getDocumentByDocumentID
                        , 'updateDocumentSignatories
                        , 'updateDocumentStatus
                        , 'signDocument
                        , 'getFilePageJpg
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
                        ])



