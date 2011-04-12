{-# OPTIONS_GHC -Wall #-}
{-# IncoherentInstances #-}

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
    
    -- Checkers - checking properties of document
    , sameUser
    , isAuthor
    , anyInvitationUndelivered
    , checkCSVSigIndex
    , isTemplate
    , isContract
    , isOffer
    -- Getters - digging some info from about document
    , signatoryname
    , undeliveredSignatoryLinks
    
    -- Other utils
    , signatoryDetailsFromUser
    , isMatchingSignatoryLink

    -- History management
    , appendHistory

    , isELegDataMismatch
    , allowsIdentification
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
import Data.List hiding (insert)
import Data.Maybe
import MinutesTime

-- DB UPDATE UTILS
insertNewDocument :: Document ->  Update Documents Document
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
                    now <- getMinuteTimeDB
                    when (documentid newdocument /= docid) $ error "new document must have same id as old one"
                    modify (updateIx docid $ newdocument {documentmtime=now})
                    return $ Right newdocument
       else return $ Left "Document didn't matche condition required for this action"

-- CHECKERS

{- | 
  We introduce some types that basicly describes the user. No we want a unified way of comparring them.   
-}
class MaybeUser u where
  getUserID:: u -> Maybe UserID
  
instance MaybeUser SignatoryLink where
  getUserID  = maybesignatory
  
instance MaybeUser  Author where
  getUserID = Just . unAuthor 

instance MaybeUser User where
  getUserID = Just . userid 

instance MaybeUser UserID where
  getUserID = Just

instance (MaybeUser u) => MaybeUser (Maybe u) where
  getUserID  = join . (fmap getUserID)


{- |  And this is a function for comparison -}
sameUser::(MaybeUser u1, MaybeUser u2) =>  u1 ->  u2 -> Bool  
sameUser u1 u2 = getUserID u1 == getUserID u2

{- |   And checking for being an author of a document -}
isAuthor::(MaybeUser u) => Document -> u -> Bool
isAuthor d u = getUserID u ==  getUserID (documentauthor d) 


{- |
  Some info utils about state of document
-}

anyInvitationUndelivered :: Document -> Bool
anyInvitationUndelivered =  not . Prelude.null . undeliveredSignatoryLinks

isContract :: Document -> Bool
isContract d =  (documenttype d == ContractTemplate) || (documenttype d == Contract)

isOffer :: Document -> Bool 
isOffer = not . isContract

isTemplate :: Document -> Bool
isTemplate d = (documenttype d == ContractTemplate) || (documenttype d == OfferTemplate)

isSignable :: Document -> Bool 
isSignable = not . isTemplate


checkCSVSigIndex :: UserID -> [SignatoryLink] -> Int -> Either String Int
checkCSVSigIndex authorid sls n
  | n<0 || n>=slcount = Left $ "signatory with index " ++ (show n) ++ " doesn't exist."
  | n==0 && slcount>0 && sameUser authorid (head sls) = Left "author can't be set from csv"
  | otherwise = Right n
  where slcount = length $ sls

-- GETTERS

undeliveredSignatoryLinks :: Document -> [SignatoryLink]
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
signatoryDetailsFromUser :: User -> SignatoryDetails
signatoryDetailsFromUser user = 
    SignatoryDetails { signatoryfstname           = userfstname $ userinfo user 
                     , signatorysndname           = usersndname $ userinfo user 
                     , signatoryemail             = unEmail $ useremail $ userinfo user
                     , signatorycompany           = usercompanyname $ userinfo user
                     -- I'm changing this because, well, it breaks Eleg signing
                     -- It should be fixed properly when we split personnnummer and felaktignnummer
                     -- (change from usercompanynumber -> userpersonalnumber)
                     --    --EN
                     , signatorypersonalnumber    = userpersonalnumber $ userinfo user
                     , signatorycompanynumber     = usercompanynumber $ userinfo user
                     , signatoryfstnameplacements        = []
                     , signatorysndnameplacements        = []
                     , signatorycompanyplacements        = []
                     , signatoryemailplacements          = []
                     , signatorypersonalnumberplacements = []
                     , signatorycompanynumberplacements  = []
                     , signatoryotherfields              = []
                     }
                     
isMatchingSignatoryLink :: User -> SignatoryLink -> Bool
isMatchingSignatoryLink user sigLink =  sameUser user sigLink || 
    (signatoryemail . signatorydetails $ sigLink) == (unEmail . useremail $ userinfo user)
  

appendHistory :: Document -> [DocumentHistoryEntry] -> Document
appendHistory document history = 
    document { documentlog = documentlog document ++ map documentHistoryToDocumentLog history }

isELegDataMismatch (ELegDataMismatch _ _ _ _ _) = True
isELegDataMismatch _                            = False

allowsIdentification :: Document -> IdentificationType -> Bool
allowsIdentification document idtype = 
    isJust $ find (== idtype) $ documentallowedidtypes document
