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
          { documentid     :: DocumentID -- this will be time soon
          , author         :: Author
          , signatories    :: [Signatory]  
          , files          :: [BSC.ByteString]
          , final          :: Bool
          }
      data Document1 = Document1
          { documentid1     :: DocumentID -- this will be time soon
          , author1         :: Author
          , signatories1    :: [Signatory]  
          , files1          :: [BSC.ByteString]
          }
      newtype Author = Author UserID
      newtype DocumentID = DocumentID Integer
      
      data Signatory = Signatory UserID
                     | EmailOnly String
      newtype Signatory1 = Signatory1 UserID
   |])

deriving instance Show Document
deriving instance Show Author
instance Show Signatory where
    showsPrec prec (Signatory userid) = showsPrec prec userid
    showsPrec prec (EmailOnly email) = (++) email

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
    mode = extension 1 (Proxy :: Proxy Signatory1)

instance Migrate Signatory1 Signatory where
    migrate (Signatory1 sig) = Signatory sig

$(deriveSerialize ''Signatory1)
instance Version Signatory1

$(deriveSerialize ''Document)
instance Version Document where
    mode = extension 1 (Proxy :: Proxy Document1)

instance Migrate Document1 Document where
    migrate (Document1 a b c d) = Document a b c d False

$(deriveSerialize ''Document1)
instance Version Document1

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
  docid <- DocumentID <$> getRandom
  modify $ insert (Document docid (Author userid) [] [title] False)


updateDocumentSignatories :: Document -> [Signatory] -> Update Documents Document
updateDocumentSignatories document signatories = do
  let doc2 = document { signatories = signatories }
  modify (updateIx (documentid doc2) doc2)
  return doc2

markDocumentAsFinal :: Document -> Update Documents Document
markDocumentAsFinal document = do
  let doc2 = document { final = True }
  modify (updateIx (documentid doc2) doc2)
  return doc2


-- create types for event serialization
$(mkMethods ''Documents [ 'getDocumentsByAuthor
                        , 'getDocumentsBySignatory
                        , 'newDocument
                        , 'getDocumentByDocumentID
                        , 'updateDocumentSignatories
                        , 'markDocumentAsFinal])



