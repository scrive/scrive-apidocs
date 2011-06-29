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
    , queryQuarantinedDocs
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
import Data.List hiding (insert)
import Doc.DocStateData
import Doc.DocUtils
import Happstack.Data.IxSet as IxSet
import Happstack.State
import MinutesTime
import Misc
import Util.SignatoryLinkUtils

{- |
    Cleans out all the deleted and quarantined documents
-}
queryDocs :: (Documents -> a) -> Query Documents a
queryDocs queryFunc = do
  docs <- ask
  let livedocs = docs @= LiveDocument
  return $ queryFunc livedocs

queryQuarantinedDocs :: (Documents -> a) -> Query Documents a
queryQuarantinedDocs queryFunc = do
  docs <- ask
  let quarantineddocs = docs @= QuarantinedDocument
  return $ queryFunc quarantineddocs

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
  case getOne (documents @= docid @+ [LiveDocument, QuarantinedDocument] ) of
    Nothing -> return $ Left "no such document"
    Just document ->
      if (condition document)
       then do
             actionresult <- action document
             case actionresult of
                Left message -> return $ Left message
                Right newdocument -> do
                        when (documentid newdocument /= docid) $ error "new document must have same id as old one"
                        now <- getMinuteTimeDB
                        let newdocumentNoUnsupportedFutures = 
                              if touchtime
                              then (dropUnsupportedFeatures newdocument) { documentmtime = now }
                              else (dropUnsupportedFeatures newdocument)
                        modify (updateIx docid $ newdocumentNoUnsupportedFutures )
                        return $ Right newdocument
       else return $ Left "Document didn't match condition required for this action"


-- Feature checking

{- |
    These features are either available for AdvancedFunctionality
    or not available for BasicFunctionality.  I split them into individual
    features because I thought it made the code for checking each of them
    easier to write, not because I thought we'd be needing that level of
    granuality particularly.
-}
data Feature = CSVUse
               | DaysToSignUse
               | MultiplePartiesUse
               | SecretaryUse
               | SpecialRoleUse
               | AuthorCustomFieldUse
               | AuthorPlacementUse
               | SigCustomFieldUse
               | SigPlacementUse
               | SignOrderUse
               | AttachmentUse
               deriving (Show, Eq, Bounded, Enum)

{- |
    Drops unsupported fetures, so if for example attachents are not supported they are dropped
-}
dropUnsupportedFeatures :: Document -> Document
dropUnsupportedFeatures doc =
  let unsupported =  filter (not . isSupported (documentfunctionality doc)) allValues
      dropper = foldl (.) id $ map dropFeature unsupported
  in if (documentstatus doc == Preparation)
        then dropper doc
        else doc

dropFeature:: Feature -> Document -> Document
dropFeature CSVUse doc = doc {documentcsvupload = Nothing}
dropFeature DaysToSignUse doc = doc {documentdaystosign = Nothing}
dropFeature MultiplePartiesUse doc = doc {documentsignatorylinks = take 2 $ documentsignatorylinks doc}
dropFeature SecretaryUse doc = doc {documentsignatorylinks = map makeAuthorSignatory $ documentsignatorylinks doc}
    where makeAuthorSignatory sl =
            if (isAuthor sl && not (isSignatory sl))
              then sl {signatoryroles = SignatoryPartner : (signatoryroles sl) }
              else sl
dropFeature SpecialRoleUse doc = doc {documentsignatorylinks = map standarizeRoles $ documentsignatorylinks doc}
   where
        standarizeRoles :: SignatoryLink -> SignatoryLink
        standarizeRoles sl =
          let standardRoles = [SignatoryPartner, SignatoryAuthor]
              limitedRoles =  filter  (`elem` standardRoles) $ signatoryroles sl
          in if (Data.List.null limitedRoles)
              then sl {signatoryroles  = [SignatoryPartner]}
              else sl {signatoryroles = limitedRoles}

dropFeature AuthorCustomFieldUse doc = doc {documentsignatorylinks = map dropAuthorCustomFields $ documentsignatorylinks doc}
   where
        dropAuthorCustomFields sl =  if isAuthor sl
                                        then sl {signatorydetails  = (signatorydetails sl) {signatoryotherfields = []} }
                                        else sl

dropFeature AuthorPlacementUse doc =  doc {documentsignatorylinks = map dropAuthorPlacementFields $ documentsignatorylinks doc}
    where
       dropAuthorPlacementFields sl =  if isAuthor sl
                                        then sl {signatorydetails  = (signatorydetails sl) {
                                              signatoryfstnameplacements = []
                                            , signatorysndnameplacements = []
                                            , signatorycompanyplacements = []
                                            , signatoryemailplacements = []
                                            , signatorypersonalnumberplacements = []
                                            , signatorycompanynumberplacements = []} }
                                        else sl
dropFeature SigCustomFieldUse doc = doc {documentsignatorylinks = map dropCustomFields $ documentsignatorylinks doc}
   where
      dropCustomFields sl =   sl {signatorydetails  = (signatorydetails sl) {signatoryotherfields = []} }

dropFeature SigPlacementUse doc =  doc {documentsignatorylinks = map dropPlacementFields $ documentsignatorylinks doc}
    where
       dropPlacementFields sl =  sl {signatorydetails  = (signatorydetails sl) {
                                              signatoryfstnameplacements = []
                                            , signatorysndnameplacements = []
                                            , signatorycompanyplacements = []
                                            , signatoryemailplacements = []
                                            , signatorypersonalnumberplacements = []
                                            , signatorycompanynumberplacements = []} }

dropFeature SignOrderUse doc = doc {documentsignatorylinks = map dropOrder $ documentsignatorylinks doc}
    where dropOrder sl = sl {signatorydetails = (signatorydetails sl)
                {signatorysignorder = SignOrder $ if isAuthor sl then 0 else 1}}

dropFeature AttachmentUse doc = doc { documentauthorattachments    = []
                                    , documentsignatoryattachments = []
                                    }


{-|
       Defines which Feature is supported by each type of DocumentFunctionality.
-}
isSupported ::  DocumentFunctionality -> Feature -> Bool
isSupported AdvancedFunctionality _ = True
isSupported BasicFunctionality _  = False
