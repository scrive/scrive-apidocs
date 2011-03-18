{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocStateUtils
-- Maintainer  :  
-- Stability   :  development
-- Portability :  portable
--
-- This module provides  low operations modify document interface and some utils for working with documents
-- Reasons for that: 1) growing number of operations in DocState 
--                   2) Stopping devs from iusing modify function since there is a split to templates and docs 
--                      and some updates make no sense on templates.
-----------------------------------------------------------------------------

module Doc.DocStateUtils (
    -- DB updates
      insertNewDocument
    , newFromDocument  
    , modifyContract
    , modifyContractWithAction 
    , modifyContractOrTemplate
    , modifyContractOrTemplateWithAction 
    
    -- Checkers - checking properties of document
    , MayBeAuthor(isAuthor)
    , anyInvitationUndelivered
    
    -- Getters - digging some info from about document
    , signatoryname
    , undeliveredSignatoryLinks
    
    -- Other utils
    , signatoryDetailsFromUser
    , isMatchingSignatoryLink

    -- History management
    , appendHistory
    )

where
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import User.UserState
import Happstack.Data.IxSet as IxSet
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Misc
import Control.Monad
import Mails.MailsUtil
import Doc.DocStateData


-- DB UPDATE UTILS
insertNewDocument doc = 
   do 
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
      Nothing -> return $ Left "No such document"
      
-- | There are six methods for update. We want force an exact info if any operation that chandes document makes sense on templates.

modifyContract :: DocumentID 
               -> (Document -> Either String Document) 
               -> Update Documents (Either String Document)
modifyContract docid action = modifyContractWithAction docid (return . action)


modifyContractOrTemplate :: DocumentID 
               -> (Document -> Either String Document) 
               -> Update Documents (Either String Document)
modifyContractOrTemplate docid action = modifyContractOrTemplateWithAction docid (return . action)


modifyContractWithAction :: DocumentID 
               -> (Document ->  Update Documents (Either String Document)) 
               -> Update Documents (Either String Document)
modifyContractWithAction  = modifyDocumentWithAction isContract


modifyContractOrTemplateWithAction:: DocumentID 
               -> (Document ->  Update Documents (Either String Document)) 
               -> Update Documents (Either String Document)
modifyContractOrTemplateWithAction = modifyDocumentWithAction (const True)


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
                    when (documentid newdocument /= docid) $ error "new document must have same id as old one"
                    modify (updateIx docid newdocument)
                    return $ Right newdocument
       else return $ Left "Document didn't matche condition required for this action"

-- CHECKERS

{- |
   Is the user an author of the document. We may want to ask the same if we just have userid or signatorylink
 -}
class MayBeAuthor a where
  isAuthor :: Document -> a -> Bool

instance MayBeAuthor User where
  isAuthor d u = isAuthor d $ userid u
  
instance MayBeAuthor UserID where
  isAuthor d uid = uid == (unAuthor . documentauthor $ d)   
  
instance MayBeAuthor SignatoryLink where
  isAuthor d sl = case maybesignatory sl of
                   Just s -> unSignatory s == ( unAuthor . documentauthor $ d)
                   Nothing -> False

instance (MayBeAuthor a) => MayBeAuthor (Maybe a) where
  isAuthor d (Just s) = isAuthor d s
  isAuthor _ _        = False


anyInvitationUndelivered =  not . Prelude.null . undeliveredSignatoryLinks

isContract = ((==) Contract) . documenttype
isTemplate = ((==) Template) . documenttype

-- GETTERS

undeliveredSignatoryLinks doc =  filter ((== Undelivered) . invitationdeliverystatus) $ documentsignatorylinks doc

signatoryname :: SignatoryDetails -> BS.ByteString
signatoryname s = 
    if (BS.null $ signatorysndname s) 
        then (signatoryfstname s) 
        else (signatoryfstname s) `BS.append` (BS.fromString " ") `BS.append` (signatorysndname s)


-- OTHER UTILS

{- |
   Build a SignatoryDetails from a User with no fields
 -}
signatoryDetailsFromUser user = 
    SignatoryDetails { signatoryfstname = userfstname $ userinfo user 
                     , signatorysndname = usersndname $ userinfo user 
                     , signatoryemail = unEmail $ useremail $ userinfo user
                     , signatorycompany = usercompanyname $ userinfo user
                     , signatorynumber = usercompanynumber $ userinfo user
                     , signatoryfstnameplacements = []
                     , signatorysndnameplacements = []
                     , signatorycompanyplacements = []
                     , signatoryemailplacements = []
                     , signatorynumberplacements = []
                     , signatoryotherfields = []
                     }
                     
isMatchingSignatoryLink :: User -> SignatoryLink -> Bool
isMatchingSignatoryLink user sigLink = signatoryMatches || emailMatches
  where 
  signatoryMatches = maybe False (\s -> unSignatory s == userid user)  (maybesignatory sigLink)
  emailMatches = (signatoryemail . signatorydetails $ sigLink) == (unEmail . useremail $ userinfo user)
  

appendHistory :: Document -> [DocumentHistoryEntry] -> Document
appendHistory document history =
    document { documenthistory = documenthistory document ++ history }
