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
    , matchingType
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
                    case (checkFeatureSupport newdocument) of
                      (Left msg) -> return $ Left msg
                      (Right _) -> do
                        now <- getMinuteTimeDB
                        when (documentid newdocument /= docid) $ error "new document must have same id as old one"
                        modify (updateIx docid $ newdocument {documentmtime=now})
                        return $ Right newdocument
       else return $ Left "Document didn't matche condition required for this action"

-- Feature checking

data Feature = CSVUse
               | ElegUse
               | MultiplePartiesUse
               | SecretaryUse
               | SpecialRoleUse

{- |
    This bit ensures that all the features used by a document
    are valid for it's documentfunctionality. It's hoped 
    that without bugs or hacks this should never fail.
-}
checkFeatureSupport :: Document -> Either String ()
checkFeatureSupport doc =
  case (all (checkSupport doc) features) of
    False -> Left "features are not supported"
    True -> Right ()
  where
    features = [CSVUse, ElegUse, MultiplePartiesUse, SecretaryUse, SpecialRoleUse]
    {-|
       Checks that any features in use are supported.
    -}
    checkSupport :: Document -> Feature -> Bool
    checkSupport doc@Document{documentfunctionality} feature =
        case (isRequired feature doc, isSupported feature documentfunctionality) of
          (True, False) -> False
          _ -> True
    {-|
       Determines whether a feature is in use for a document.
    -}
    isRequired :: Feature -> Document -> Bool
    isRequired CSVUse Document{documentcsvupload} = 
      isJust documentcsvupload
    isRequired ElegUse Document{documentallowedidtypes} = 
      isJust $ find (== ELegitimationIdentification) documentallowedidtypes
    isRequired MultiplePartiesUse Document{documentsignatorylinks} = 
      (length documentsignatorylinks) > 2
    isRequired SecretaryUse doc@Document{documentsignatorylinks} = 
      and . map (\sl -> not $ isAuthor doc sl) $ documentsignatorylinks
    isRequired SpecialRoleUse doc@Document{documentsignatorylinks} =
      and . map isSpecialRole $ documentsignatorylinks
      where 
        isSpecialRole :: SignatoryLink -> Bool
        isSpecialRole SignatoryLink{signatoryroles} =
          case signatoryroles of
            [SignatoryPartner] -> False
            _ -> True
    {-|
       Defines which Feature is supported by each type of DocumentFunctionality.
    -}
    isSupported :: Feature -> DocumentFunctionality -> Bool
    isSupported _ AdvancedFunctionality = True
    isSupported _ BasicFunctionality = False

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

class MaybeTemplate a where
   isTemplate :: a -> Bool
   isSignable :: a -> Bool 
   isSignable = not . isTemplate

class MaybeContractOrOffer a where
   isContract :: a -> Bool 
   isOffer :: a -> Bool 
   isOffer = not . isContract
   
instance  MaybeTemplate DocumentType where
   isTemplate t = (t == ContractTemplate) || (t == OfferTemplate) 
   
instance  MaybeContractOrOffer DocumentType where
   isContract t =  (t == ContractTemplate) || (t == Contract)

instance  MaybeTemplate Document where
   isTemplate =  isTemplate . documenttype
   
instance  MaybeContractOrOffer Document where
   isContract =  isContract . documenttype
 
matchingType::(MaybeContractOrOffer a, MaybeContractOrOffer b) => a -> b -> Bool
matchingType a b = (isContract a && isContract b) || (isOffer a && isOffer b)


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
