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
import qualified Data.ByteString.UTF8 as BSC
import Control.Applicative ((<$>))
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common

$(deriveAll [''Eq, ''Ord, ''Default]
  [d|
      data Document = Document
          { documentid     :: DocumentID
          , title          :: String
          , author         :: Author
          , signatories    :: [Signatory]  
          , files          :: [BSC.ByteString]
          , status         :: DocumentStatus
          }
      newtype Author = Author UserID
      newtype DocumentID = DocumentID Int
      newtype EmailCookie = EmailCookie Int
      
      data Signatory = Signatory UserID
                     | EmailOnly String (Either EmailCookie UserID) 

      data DocumentStatus = Preparation | ReadyToSign | Signed | Postponed
   |])

deriving instance Show Document
deriving instance Show DocumentStatus
deriving instance Show Author
instance Show Signatory where
    showsPrec prec (Signatory userid) = showsPrec prec userid
    showsPrec prec (EmailOnly email _) = (++) email

instance Show DocumentID where
    showsPrec prec (DocumentID val) = showsPrec prec val

instance Read DocumentID where
    readsPrec prec = let makeDocumentID (i,v) = (DocumentID i,v) 
                     in map makeDocumentID . readsPrec prec 

instance FromReqURI DocumentID where
    fromReqURI = readM

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

$(deriveSerialize ''EmailCookie)
instance Version EmailCookie where

$(inferIxSet "Documents" ''Document 'noCalcs [''DocumentID, ''Author, ''Signatory])

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

newDocument :: UserID -> BSC.ByteString -> Update Documents ()
newDocument userid title = do
  docid <- DocumentID <$> getRandomR (0,maxBound)
  modify $ insert (Document docid (BSC.toString title) (Author userid) [] [title] Preparation)


updateDocumentSignatories :: Document -> [Signatory] -> Update Documents Document
updateDocumentSignatories document signatories = do
  let doc2 = document { signatories = signatories }
  modify (updateIx (documentid doc2) doc2)
  return doc2

markDocumentAsFinal :: Document -> Update Documents Document
markDocumentAsFinal document = do
  let doc2 = document { status = ReadyToSign }
  modify (updateIx (documentid doc2) doc2)
  return doc2

signDocument :: DocumentID -> UserID -> String -> EmailCookie -> Update Documents (Maybe Document)
signDocument documentid userid emailaddress emailcookie = do
  documents <- ask
  let Just document = getOne (documents @= documentid)
      signeddocument = document { signatories = newsignatories }
      newsignatories = map maybesign (signatories document)
      maybesign (EmailOnly x (Left cookie)) | x == emailaddress && cookie==emailcookie = Signatory userid
      maybesign x = x
  modify (updateIx documentid signeddocument)
  return (Just signeddocument)
  
  


-- create types for event serialization
$(mkMethods ''Documents [ 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'newDocument
                        , 'getDocumentByDocumentID
                        , 'updateDocumentSignatories
                        , 'markDocumentAsFinal
                        , 'signDocument
                        ])



