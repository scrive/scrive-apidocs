-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocStateUtils
-- Maintainer  :
-- Stability   :  development
-- Portability :  portable
--
-- This module provides  low level interface for document modifications and some utils
-- Reasons for that: 1) growing number of operations in DocState
--                   2) Stopping devs from iusing modify function since there is a split to templates and docs
--                      and some updates make no sense on templates.
-----------------------------------------------------------------------------

module Doc.DocStateUtils (
    -- DB updates
      insertNewDocument
    , newFromDocument
    , queryDocs
    , modifySignable
    , modifySignableWithAction
    , modifySignableOrTemplate
    , modifySignableOrTemplateWithAction
    , modifyDocumentWithActionTime

    )

where
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Doc.DocStateData
import Doc.DocUtils
import Happstack.Data.IxSet as IxSet
import Happstack.State
import MinutesTime
import Misc


queryDocs :: (Documents -> a) -> Query Documents a
queryDocs queryFunc = do
  docs <- ask
  return $ queryFunc docs

-- DB UPDATE UTILS
insertNewDocument :: Document ->  Update Documents Document
insertNewDocument doc = do
  documents <- ask
  docid <- getUnique64 documents DocumentID
  now <- getMinuteTimeDB
  let docWithId = doc {documentid = docid, documentmtime  = now, documentctime = now}
  modify $ insert docWithId
  return docWithId


-- Create new document based on existing one
newFromDocument :: (Document -> Document) -> DocumentID -> Update Documents (Either String Document)
newFromDocument f docid = do
    documents <- ask
    case (getOne (documents @= docid)) of
      Just doc -> fmap Right $ insertNewDocument $ f doc
      Nothing -> return $ Left $ "Document " ++ show docid ++ " does not exist"

-- | There are six methods for update. We want force an exact info if
-- any operation that changes a document makes sense on templates.
modifySignable :: DocumentID
               -> (Document -> Either String Document)
               -> Update Documents (Either String Document)
modifySignable docid action = modifySignableWithAction docid (return . action)


modifySignableOrTemplate :: DocumentID
               -> (Document -> Either String Document)
               -> Update Documents (Either String Document)
modifySignableOrTemplate docid action = modifySignableOrTemplateWithAction docid (return . action)


modifySignableWithAction :: DocumentID
               -> (Document ->  Update Documents (Either String Document))
               -> Update Documents (Either String Document)
modifySignableWithAction  = modifyDocumentWithAction isSignable


modifySignableOrTemplateWithAction:: DocumentID
               -> (Document ->  Update Documents (Either String Document))
               -> Update Documents (Either String Document)
modifySignableOrTemplateWithAction = modifyDocumentWithAction (const True)

modifyDocumentWithAction :: (Document -> Bool) -> DocumentID 
               -> (Document ->  Update Documents (Either String Document))
               -> Update Documents (Either String Document)                            
modifyDocumentWithAction = modifyDocumentWithActionTime True

modifyDocumentWithActionTime :: Bool 
               -> (Document -> Bool) -> DocumentID 
               -> (Document -> Update Documents (Either String Document))
               -> Update Documents (Either String Document)                            
modifyDocumentWithActionTime touchtime condition docid action = do
  documents <- ask
  case getOne (documents @= docid) of
    Nothing -> return $ Left "no such document"
    Just document | documentdeleted document -> return $ Left "document has been deleted"
    Just document ->
      if (condition document)
       then do
             actionresult <- action document
             case actionresult of
                Left message -> return $ Left message
                Right newdocument -> do
                        when (documentid newdocument /= docid) $ error "new document must have same id as old one"
                        now <- getMinuteTimeDB
                        let maybetoucheddoc = 
                              if touchtime
                              then newdocument { documentmtime = now }
                              else newdocument
                        modify (updateIx docid $ maybetoucheddoc )
                        return $ Right maybetoucheddoc
       else return $ Left "Document didn't match condition required for this action"
