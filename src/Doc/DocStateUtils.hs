{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
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
    , modifySignable
    , modifySignableWithAction 
    , modifySignableOrTemplate
    , modifySignableOrTemplateWithAction 
    )

where
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.List hiding (insert)
import Data.Maybe
import Doc.DocStateData
import Doc.DocUtils
import Happstack.Data.IxSet as IxSet
import Happstack.State
import MinutesTime
import Misc

-- DB UPDATE UTILS
insertNewDocument :: Document ->  Update Documents Document
insertNewDocument doc = do 
  documents <- ask
  docid <- getUnique64 documents DocumentID
  let docWithId = doc {documentid = docid}
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
               
modifyDocumentWithAction condition docid action = do
  documents <- ask
  case getOne (documents @= docid) of
    Nothing -> return $ Left "no such document"
    Just document -> 
      if (condition document)
       then do
             actionresult <- action document
             case actionresult of
                Left message -> return $ Left message
                Right newdocument -> do
                    case (checkFeatureSupport newdocument) of
                      (Left msg) -> return $ Left msg
                      (Right _) -> do
                        now <- getMinuteTimeDB
                        when (documentid newdocument /= docid) $ error "new document must have same id as old one"
                        modify (updateIx docid $ newdocument {documentmtime=now})
                        return $ Right newdocument
       else return $ Left "Document didn't matche condition required for this action"

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
               deriving (Show, Eq)

wmap :: [(a -> b)] -> a -> [b]
wmap fs v = map (\f -> f v) fs

{- |
    This bit ensures that all the features used by a document
    are valid for it's documentfunctionality. It's hoped 
    that without bugs or hacks this should never fail.
-}
checkFeatureSupport :: Document -> Either String ()
checkFeatureSupport doc =
  case (filter (checkSupport doc) features, filter (not . checkSupport doc) features) of
    (_, []) -> Right ()
    (_, notSupported) -> Left ("features required but not supported: " ++ show notSupported)
  where
    features = [CSVUse, DaysToSignUse, MultiplePartiesUse, SecretaryUse, SpecialRoleUse,
                AuthorCustomFieldUse, AuthorPlacementUse, SigCustomFieldUse, AttachmentUse]
    {-|
       Checks that any features in use are supported.
    -}
    checkSupport :: Document -> Feature -> Bool
    checkSupport ddoc@Document{documentfunctionality} feature =
        case (isRequired feature ddoc, isSupported feature documentfunctionality) of
          (True, False) -> False
          _ -> True
    {-|
       Determines whether a feature is in use for a document.
    -}
    isRequired :: Feature -> Document -> Bool
    isRequired CSVUse Document{documentcsvupload} = 
      isJust documentcsvupload
    isRequired DaysToSignUse Document{documentdaystosign} = 
      isJust documentdaystosign
    isRequired MultiplePartiesUse Document{documentsignatorylinks} = 
      (length documentsignatorylinks) > 2
    isRequired SecretaryUse Document{documentsignatorylinks} = 
      and . map (\sl -> not $ siglinkIsAuthor sl) $ documentsignatorylinks
    isRequired SpecialRoleUse Document{documentsignatorylinks} =
      and . map isSpecialRole $ documentsignatorylinks
      where 
        isSpecialRole :: SignatoryLink -> Bool
        isSpecialRole SignatoryLink{signatoryroles} =
          case signatoryroles of
            [SignatoryPartner]                  -> False
            [SignatoryPartner, SignatoryAuthor] -> False
            [SignatoryAuthor, SignatoryPartner] -> False
            _                                   -> True
    isRequired AuthorCustomFieldUse ddoc = maybe False (not . Data.List.null . signatoryotherfields . signatorydetails) $ getAuthorSigLink ddoc
    isRequired AuthorPlacementUse ddoc = maybe False 
      (any (not . Data.List.null) . wmap
       [ signatoryfstnameplacements
       , signatorysndnameplacements
       , signatorycompanyplacements
       , signatoryemailplacements
       , signatorypersonalnumberplacements
       , signatorycompanynumberplacements] . signatorydetails) $ getAuthorSigLink ddoc
    isRequired SigCustomFieldUse Document{documentsignatorylinks} =
      any (not . Data.List.null . signatoryotherfields . signatorydetails) documentsignatorylinks
    isRequired SigPlacementUse Document{documentsignatorylinks} =
      any (isPlacement . signatorydetails) documentsignatorylinks
      where isPlacement :: SignatoryDetails -> Bool
            isPlacement SignatoryDetails{ 
                             signatoryfstnameplacements
                           , signatorysndnameplacements
                           , signatorycompanyplacements
                           , signatoryemailplacements
                           , signatorypersonalnumberplacements
                           , signatorycompanynumberplacements } =
              any (not . Data.List.null) 
                               [ signatoryfstnameplacements
                               , signatorysndnameplacements
                               , signatorycompanyplacements
                               , signatoryemailplacements
                               , signatorypersonalnumberplacements
                               , signatorycompanynumberplacements ]
    isRequired SignOrderUse Document{documentsignatorylinks} =
      any isSpecialSignOrder documentsignatorylinks
        where isSpecialSignOrder :: SignatoryLink -> Bool
              isSpecialSignOrder sl@SignatoryLink{signatorydetails}
                | siglinkIsAuthor sl = (signatorysignorder signatorydetails) /= (SignOrder 0)
                | otherwise = (signatorysignorder signatorydetails) /= (SignOrder 1)
    isRequired AttachmentUse Document{documentattachments} = not $ Data.List.null documentattachments

    {-|
       Defines which Feature is supported by each type of DocumentFunctionality.
    -}
    isSupported :: Feature -> DocumentFunctionality -> Bool
    isSupported _ AdvancedFunctionality = True
    isSupported _ BasicFunctionality = False

