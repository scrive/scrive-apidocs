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
    , isViewer
    , anyInvitationUndelivered
    , checkCSVSigIndex
    , isTemplate
    , isContract
    , isOffer
    , matchingType
    , isEligibleForReminder
    -- Getters - digging some info from about document
    , signatoryname
    , undeliveredSignatoryLinks
    
    -- Other utils
    , documentcurrentsignorder
    , signatoryDetailsFromUser
    , isMatchingSignatoryLink
    , removeFieldsAndPlacements
    , replaceSignOrder
    

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
import Doc.DocUtils

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
    features = [CSVUse, DaysToSignUse, MultiplePartiesUse, SecretaryUse, SpecialRoleUse,
                AuthorCustomFieldUse, AuthorPlacementUse, SigCustomFieldUse, AttachmentUse]
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
    isRequired DaysToSignUse Document{documentdaystosign} = 
      isJust documentdaystosign
    isRequired MultiplePartiesUse Document{documentsignatorylinks} = 
      (length documentsignatorylinks) > 2
    isRequired SecretaryUse doc@Document{documentsignatorylinks} = 
      and . map (\sl -> not $ siglinkIsAuthor sl) $ documentsignatorylinks
    isRequired SpecialRoleUse doc@Document{documentsignatorylinks} =
      and . map isSpecialRole $ documentsignatorylinks
      where 
        isSpecialRole :: SignatoryLink -> Bool
        isSpecialRole SignatoryLink{signatoryroles} =
          case signatoryroles of
            [SignatoryPartner] -> False
            _ -> True
    isRequired AuthorCustomFieldUse doc@Document{authorotherfields} = not $ Data.List.null authorotherfields
    isRequired AuthorPlacementUse doc@Document{
                                      authorfstnameplacements
                                    , authorsndnameplacements
                                    , authorcompanyplacements
                                    , authoremailplacements
                                    , authorpersonalnumberplacements
                                    , authorcompanynumberplacements} =
      any (not . Data.List.null) 
                       [ authorfstnameplacements
                       , authorsndnameplacements
                       , authorcompanyplacements
                       , authoremailplacements
                       , authorpersonalnumberplacements
                       , authorcompanynumberplacements]
    isRequired SigCustomFieldUse doc@Document{documentsignatorylinks} =
      any (not . Data.List.null . signatoryotherfields . signatorydetails) documentsignatorylinks
    isRequired SigPlacementUse doc@Document{documentsignatorylinks} =
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
    isRequired SignOrderUse doc@Document{documentsignatorylinks} =
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
sameUser:: (MaybeUser u1, MaybeUser u2) =>  u1 ->  u2 -> Bool  
sameUser u1 u2 = getUserID u1 == getUserID u2


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
  where slcount = length sls

-- GETTERS

undeliveredSignatoryLinks :: Document -> [SignatoryLink]
undeliveredSignatoryLinks doc =  filter ((== Undelivered) . invitationdeliverystatus) $ documentsignatorylinks doc

signatoryname :: SignatoryDetails -> BS.ByteString
signatoryname s = 
    if (BS.null $ signatorysndname s) 
        then (signatoryfstname s) 
        else (signatoryfstname s) `BS.append` (BS.fromString " ") `BS.append` (signatorysndname s)


-- OTHER UTILS

-- | Indicates which signatories were activated (received
-- invitation email). All signatories with sign order
-- not greater than current sign order of the document
-- are considered to be activated.
documentcurrentsignorder :: Document -> SignOrder
documentcurrentsignorder doc =
    case filter notSigned sigs of
         [] -> maximum $ map signorder sigs
         xs -> minimum $ map signorder xs
    where
        signorder = signatorysignorder . signatorydetails
        sigs = documentsignatorylinks doc
        notSigned siglnk = isNothing (maybesigninfo siglnk)
            && SignatoryPartner `elem` signatoryroles siglnk -- we exclude non-signatories
            && signorder siglnk > SignOrder 0 -- we omit author

{- |
   Build a SignatoryDetails from a User with no fields
 -}
signatoryDetailsFromUser :: User -> SignatoryDetails
signatoryDetailsFromUser user = 
    SignatoryDetails { signatoryfstname           = userfstname $ userinfo user 
                     , signatorysndname           = usersndname $ userinfo user 
                     , signatoryemail             = unEmail $ useremail $ userinfo user
                     , signatorycompany           = usercompanyname $ userinfo user
                     , signatorysignorder         = SignOrder 1
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

isViewer doc user = any f $ documentsignatorylinks doc
    where f link = (Just (userid user)  == maybesignatory link 
                    || (unEmail $ useremail $ userinfo user) == (signatoryemail $ signatorydetails link))
                   && signatoryroles link == []

{- |
    Checks whether a signatory link is eligible for sending a reminder.
    The user must be the author, and the signatory musn't be the author.
    Also the signatory must be next in the signorder, and also not be a viewer.
    In addition the document must be in the correct state.  There's quite a lot to check!
-}
isEligibleForReminder :: Maybe User -> Document -> SignatoryLink -> Bool
isEligibleForReminder muser document@Document{documentstatus} siglink = 
  signatoryActivated 
    && isUserAuthor 
    && (not isUserSignator) 
    && (not dontShowAnyReminder) 
    && ((invitationdeliverystatus siglink) /= Undelivered) 
    && (isClosed || not wasSigned)
    && isSignatoryPartner
  where
    isUserAuthor = maybe False (isAuthor document) muser
    isUserSignator = maybe False (\user -> (unEmail . useremail . userinfo $ user) == (signatoryemail $ signatorydetails siglink)) muser 
    wasSigned =  (isJust $ maybesigninfo siglink) && (not $ isUserSignator && (documentstatus == AwaitingAuthor))
    isClosed = documentstatus == Closed
    signatoryActivated = documentcurrentsignorder document >= (signatorysignorder $ signatorydetails siglink)
    dontShowAnyReminder = documentstatus `elem` [Timedout, Canceled, Rejected]
    isSignatoryPartner = SignatoryPartner `elem` (signatoryroles siglink)

{- |
    Removes the field placements and the custom fields.
-}
removeFieldsAndPlacements :: SignatoryDetails -> SignatoryDetails
removeFieldsAndPlacements sd = sd {
      signatoryfstnameplacements = []
    , signatorysndnameplacements = []
    , signatorycompanyplacements = []
    , signatoryemailplacements = []
    , signatorypersonalnumberplacements = []
    , signatorycompanynumberplacements = []
    , signatoryotherfields = []
  }

{- |
    Sets the sign order on some signatory details.
-}
replaceSignOrder :: SignOrder -> SignatoryDetails -> SignatoryDetails
replaceSignOrder signorder sd = sd {
    signatorysignorder = signorder
}

{- |
   Get the Just SignatoryLink from doc that has sid. Nothing when not found.
 -}
signlinkFromDocById :: Document -> SignatoryLinkID -> Maybe SignatoryLink
signlinkFromDocById doc sid = find ((== sid) . signatorylinkid) (documentsignatorylinks  doc)

{- |
   Get the author's signatory link.
 -}
getAuthorSigLink :: Document -> Maybe SignatoryLink
getAuthorSigLink = find (elem SignatoryAuthor . signatoryroles) . documentsignatorylinks

{- |
   Does the siglink belong to a user with userid and email?
 -}
isSigLinkForUserInfo :: UserID -> BS.ByteString -> SignatoryLink -> Bool
isSigLinkForUserInfo userid email siglink = 
    Just userid == maybesignatory siglink
       || email == signatoryemail $ signatorydetails siglink

isSigLinkForUser user = isSigLinkForUserInfo (userid user) (useremail $ userinfo user)

{- |
   Can the user view thid document directly? (not counting friends)
 -}
canUserInfoViewDirectly userid email doc = 
    isJust $ find (isSigLinkForUser userid email) $ documentsignatorylinks doc

canUserViewDirectly user = canUserInfoViewDirectly (userid user) (useremail $ userinfo user)

isSigLinkIDForSigLink siglinkid siglink = siglinkid == signatorylinkid siglink

getSigLinkBySigLinkID siglinkid = 
    find (isSigLinkIDForSigLink siglinkid) . documentsignatorylinks

{- |
   Has the signatory's sign order come up?
 -}
isActivatedSignatory :: SignOrder -> SignatoryLink -> Bool
isActivatedSignatory signorder siglink = 
    signorder >= signatorysignorder $ signatorydetails siglink

isCurrentSignatory :: SignOrder -> SignatoryLink -> Bool
isCurrentSignatory signorder siglink =
  signorder == signatorysignorder $ signatorydetails siglink

siglinkIsAuthor siglink = SignatoryAuthor `elem` signatoryrole siglink
