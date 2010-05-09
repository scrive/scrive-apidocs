{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, TypeSynonymInstances,
    GeneralizedNewtypeDeriving, StandaloneDeriving, NamedFieldPuns
    #-}

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
      data Document = Document
          { documentid     :: DocumentID
          , title          :: BS.ByteString
          , author         :: Author
          , signatorylinks :: [SignatoryLink]  
          , files          :: [File]
          , status         :: DocumentStatus
          , documentctime  :: MinutesTime
          , documentmtime  :: MinutesTime
          }
      newtype Author = Author UserID
      newtype DocumentID = DocumentID Int
      newtype SignatoryLinkID = SignatoryLinkID Int
      newtype FileID = FileID Int

      data SignatoryLink = SignatoryLink 
          { signatorylinkid :: SignatoryLinkID
          , signatoryname   :: BS.ByteString 
          , signatoryemail  :: BS.ByteString
          , maybesignatory  :: Maybe Signatory
          , maybesigninfo   :: Maybe SignInfo
          }
      data SignInfo = SignInfo
          { signtime :: MinutesTime
            -- some authorization info probably
          }
      data Signatory = Signatory UserID
      data File = File 
          { fileid       :: FileID
          , filename     :: BS.ByteString
          , filepdf      :: BS.ByteString 
          , filejpgpages :: [BS.ByteString]
          }

      data DocumentStatus = Preparation | ReadyToSign | Signed | Postponed
   |])

instance Show SignatoryLinkID where
    showsPrec prec (SignatoryLinkID x) = showsPrec prec x

instance Show SignatoryLink where
    showsPrec prec (SignatoryLink _ name email Nothing Nothing) = 
        (++) (BS.toString name ++ " <" ++ BS.toString email ++ ">")
    showsPrec prec (SignatoryLink _ name email _ (Just signinfo)) = 
        (++) $ "Signed by " ++ (BS.toString name ++ " <" ++ BS.toString email ++ "> on " ++ show (signtime signinfo))

deriving instance Show Document
deriving instance Show DocumentStatus
deriving instance Show Author
instance Show Signatory where
    showsPrec prec (Signatory userid) = showsPrec prec userid

instance Show DocumentID where
    showsPrec prec (DocumentID val) = showsPrec prec val

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

$(deriveSerialize ''Document)
instance Version Document where

$(deriveSerialize ''DocumentStatus)
instance Version DocumentStatus where

$(deriveSerialize ''File)
instance Version File where

$(deriveSerialize ''FileID)
instance Version FileID where


$(inferIxSet "Documents" ''Document 'noCalcs [''DocumentID, ''Author, ''Signatory, ''SignatoryLinkID, ''FileID])

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

getDocumentsBySignatory :: UserID -> Query Documents [Document]
getDocumentsBySignatory userid = do
    documents <- ask
    return $ toList (documents @= Signatory userid)

newDocument :: UserID -> BS.ByteString
            -> MinutesTime -> Update Documents Document
newDocument userid title ctime = do
  documents <- ask
  docid <- getUnique documents DocumentID
  let doc = Document docid title (Author userid) [] []
            Preparation ctime ctime
  modify $ insert doc
  return doc

attachFile :: DocumentID -> BS.ByteString -> BS.ByteString 
           -> [BS.ByteString] -> Update Documents ()
attachFile documentid filename1 content jpgpages = do
  -- trace "attachFile begin" $ return ()
  documents <- ask
  let Just document = getOne (documents @= documentid)
  fileid2 <- getUnique documents FileID
  let nfile = File { fileid = fileid2
                   , filejpgpages = jpgpages
                   , filename = filename1
                   , filepdf = content
                   }
  -- trace ("attachFile fileid = " ++ show (fileid nfile)) $ return ()
  -- FIXME: update change time here
  let document2 = document { files = files document ++ [nfile] }
  modify $ updateIx documentid document2
  -- trace "attachFile end" $ return ()

updateDocumentSignatories :: Document -> [BS.ByteString] -> [BS.ByteString] -> Update Documents Document
updateDocumentSignatories document signatorynames signatoryemails = do
  signatorylinks <- zipWithM mm signatorynames signatoryemails
  let doc2 = document { signatorylinks = signatorylinks }
  modify (updateIx (documentid doc2) doc2)
  return doc2
  where mm name email = do
          sg <- ask
          x <- getUnique sg SignatoryLinkID
          return $ SignatoryLink x name email Nothing Nothing

markDocumentAsFinal :: Document -> Update Documents Document
markDocumentAsFinal document = do
  let doc2 = document { status = ReadyToSign }
  modify (updateIx (documentid doc2) doc2)
  return doc2

signDocument :: DocumentID -> UserID -> SignatoryLinkID 
             -> MinutesTime -> Update Documents (Maybe Document)
signDocument documentid userid signatorylinkid1 time = do
  documents <- ask
  let Just document = getOne (documents @= documentid)
      signeddocument = document { signatorylinks = newsignatorylinks }
      newsignatorylinks = map maybesign (signatorylinks document)
      maybesign x@(SignatoryLink {signatorylinkid} ) 
          | signatorylinkid == signatorylinkid1 = 
              x { maybesigninfo = Just (SignInfo time)
                , maybesignatory = Just (Signatory userid) 
                }
      maybesign x = x
  modify (updateIx documentid signeddocument)
  return (Just signeddocument)
  

getFilePageJpg :: FileID -> Int -> Query Documents (Maybe BS.ByteString)
getFilePageJpg xfileid pageno = do
  documents <- ask
  return $ do -- maybe monad!
    document <- getOne (documents @= xfileid)
    nfile <- find (\f -> fileid f == xfileid) (files document)
    let jpgs = filejpgpages nfile
    jpg <- return (jpgs!!(pageno-1))
    return jpg
  


-- create types for event serialization
$(mkMethods ''Documents [ 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'newDocument
                        , 'getDocumentByDocumentID
                        , 'updateDocumentSignatories
                        , 'markDocumentAsFinal
                        , 'signDocument
                        , 'getFilePageJpg
                        , 'attachFile
                        ])



