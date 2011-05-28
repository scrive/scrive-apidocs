{-# OPTIONS_GHC -Wall -Werror #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocUtils
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility functions for accessing the innards of Documents and other
-- datatypes in Doc.DocStateData
-----------------------------------------------------------------------------

module Doc.DocUtils where

import Doc.DocStateData
import API.Service.ServiceState
import Mails.MailsUtil
import Misc
import Templates.Templates 
import User.UserState

import Control.Monad
import Data.List hiding (insert)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS


type SignatoryAccount = (UserID, Maybe UserID)

class HasSignatoryAccount a where
  getSignatoryAccount :: a -> SignatoryAccount

instance HasSignatoryAccount User where
  getSignatoryAccount User{userid, usersupervisor} = 
    (userid, fmap (UserID . unSupervisorID) usersupervisor)

instance HasSignatoryAccount SignatoryAccount where
  getSignatoryAccount = id

instance HasSignatoryAccount SignatoryLink where
  getSignatoryAccount sl =
    let Just userid = maybesignatory sl in
    (userid, maybesupervisor sl)

copySignatoryAccount :: (HasSignatoryAccount a) => a -> SignatoryLink -> SignatoryLink
copySignatoryAccount acc siglink =
  let (userid, msuperid) = getSignatoryAccount acc in
  siglink { maybesignatory = Just userid, maybesupervisor = msuperid }

{- |
    Checks whether the document is deletable, this is not the case for live documents.
-}
isDeletableDocument :: Document -> Bool
isDeletableDocument doc = not $ (documentstatus doc) `elem` [Pending, AwaitingAuthor]

{- |
   Is the given SignatoryLink marked as a signatory (someone who can must sign)?
 -}
isSignatory :: SignatoryLink -> Bool
isSignatory sl = SignatoryPartner `elem` signatoryroles sl

{- |
   Does the given SignatoryLink have SignInfo (meaning the signatory has signed)?
 -}
hasSigned :: SignatoryLink -> Bool
hasSigned sl = isJust $ maybesigninfo sl

{- |
   Given a Document, return all of the signatory details for all signatories (exclude viewers but include author if he must sign).
   See also: partyListButAuthor to exclude the author.
 -}
partyList :: Document -> [SignatoryDetails]
partyList document = [signatorydetails sl | sl <- documentsignatorylinks document
                                          , isSignatory sl]
  
{- |
   Given a Document, return all of the signatory details for all signatories who have not yet signed.
 -}
partyUnsignedList :: Document -> [SignatoryDetails]
partyUnsignedList document = [signatorydetails sl | sl <- documentsignatorylinks document
                                                  , isSignatory sl
                                                  , not $ hasSigned sl]

{- |
   Given a Document, return all of the signatory details for all signatories who have signed.
 -}
partySignedList :: Document -> [SignatoryDetails]
partySignedList document = [signatorydetails sl | sl <- documentsignatorylinks document
                                                , isSignatory sl
                                                ,  hasSigned sl]

-- ?? What is this? -EN
partyUnsignedMeAndList :: MagicHash -> Document -> [SignatoryDetails]
partyUnsignedMeAndList magichash document =
    let signalinks = filter isSignatory $ documentsignatorylinks document
        cond signlink = signatorymagichash signlink /= magichash &&
                        maybesigninfo signlink == Nothing
        unsignalinks = filter cond signalinks
        me = SignatoryDetails { signatoryfstname = BS.fromString "du"
                              , signatorysndname = BS.empty
                              , signatorycompany = BS.empty
                              , signatorypersonalnumber = BS.empty
                              , signatorycompanynumber = BS.empty
                              , signatoryemail = BS.empty
                              , signatorysignorder = SignOrder 1
                              , signatoryfstnameplacements = []
                              , signatorysndnameplacements = []
                              , signatorycompanyplacements = []
                              , signatorypersonalnumberplacements = []
                              , signatorycompanynumberplacements = []
                              , signatoryemailplacements = []
                              , signatoryotherfields = []
                              }
        signas = map signatorydetails unsignalinks
    in me : signas

{- |
   Given a Document, return all signatories except the author.
   See also: partyList to include the author.
 -}
partyListButAuthor :: Document -> [SignatoryDetails]
partyListButAuthor document = [signatorydetails sl | sl <- documentsignatorylinks document
                                                   , isSignatory sl
                                                   , not $ siglinkIsAuthor sl]

{- |
   Insert the first list between each list of the second list.
 -}
joinWith :: [a] -> [[a]] -> [a]
joinWith _ [] = []
joinWith _ [x] = x
joinWith s (x:xs) = x ++ s ++ joinWith s xs

{- Either a signatory name or email address. We dont want to show empty strings -}
{- |
   Given a SignatoryLink, return a "smart" name: either the name or the email.
 -}
personname :: SignatoryLink -> BS.ByteString 
personname = personname' . signatorydetails 

{- Same but unwrapped. We need this cause author details are in this format  -}
{- |
   Given a SignatoryDetails, return a "smart" name: either the name or the email.
 -}
personname' :: SignatoryDetails -> BS.ByteString 
personname' signdetails = if BS.null $ signatoryname signdetails
                          then  signatoryemail signdetails
                          else  signatoryname  signdetails

{- |
   Given a SignatoryLink, returns a tuple containing the name and the email address.
   
   Useful for sending emails.
 -}
emailFromSignLink :: SignatoryLink -> (BS.ByteString, BS.ByteString)
emailFromSignLink sl = (signatoryname $ signatorydetails sl, signatoryemail $ signatorydetails sl) 

-- where does this go? -EN
renderListTemplate:: KontrakcjaTemplates -> [String] -> IO String
renderListTemplate templates list = 
  if length list > 1
  then  renderTemplate templates "morethenonelist" [("list", init list), ("last", [last list])]   
  else  renderTemplate templates "nomorethanonelist" [("list", list)]   

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
  getUserID  = join . fmap getUserID

instance MaybeUser ServiceAdmin where
  getUserID = Just . UserID . unServiceAdmin 

{- |  And this is a function for comparison -}
sameUser:: (MaybeUser u1, MaybeUser u2) =>  u1 ->  u2 -> Bool  
sameUser u1 u2 = getUserID u1 == getUserID u2

class MaybeTemplate a where
   isTemplate :: a -> Bool
   isSignable :: a -> Bool 
   isSignable = not . isTemplate

class MaybeContract a where
   isContract :: a -> Bool 
   
class MaybeOffer a where
   isOffer :: a -> Bool 
   
class MaybeAttachment a where   
   isAttachment :: a -> Bool 
    
  
   
instance  MaybeTemplate DocumentType where
   isTemplate t = (t == ContractTemplate) || (t == OfferTemplate) || (t == AttachmentTemplate) 
   
instance  MaybeContract DocumentType where
   isContract t =  (t == ContractTemplate) || (t == Contract)

instance  MaybeOffer DocumentType where
   isOffer t =  (t == OfferTemplate) || (t == Offer)
   
instance  MaybeAttachment DocumentType where
   isAttachment t =  (t == AttachmentTemplate) || (t == Attachment)

instance  MaybeTemplate Document where
   isTemplate =  isTemplate . documenttype

instance  MaybeContract Document where
   isContract =  isContract . documenttype
   
instance  MaybeOffer Document where
   isOffer =  isOffer . documenttype
  
instance  MaybeAttachment Document where
   isAttachment =  isAttachment . documenttype
  
   
matchingType::(MaybeContract a, MaybeContract b,MaybeOffer a, MaybeOffer b,MaybeAttachment a, MaybeAttachment b) => a -> b -> Bool
matchingType a b = (isContract a && isContract b) || (isOffer a && isOffer b) || (isAttachment a && isAttachment b)

-- does this need to change now? -EN
checkCSVSigIndex :: [SignatoryLink] -> Int -> Either String Int
checkCSVSigIndex sls n
  | n<0 || n>=slcount = Left $ "signatory with index " ++ show n ++ " doesn't exist."
  | n==0 && slcount>0 && siglinkIsAuthor (head sls) = Left "author can't be set from csv"
  | otherwise = Right n
  where slcount = length sls

{- |
   Is this SignatoryLink undelivered?
 -}
isUndelivered :: SignatoryLink -> Bool
isUndelivered sl = invitationdeliverystatus sl == Undelivered

{- |
   Is this SignatoryLink Deferred?
 -}
isDeferred :: SignatoryLink -> Bool
isDeferred sl = invitationdeliverystatus sl == Deferred

{- |
   Given a Document, return all of the undelivered signatorylinks.
 -}
undeliveredSignatoryLinks :: Document -> [SignatoryLink]
undeliveredSignatoryLinks doc = filter isUndelivered $ documentsignatorylinks doc

{- |
   Are there any undelivered signatory links?
 -}
anyInvitationUndelivered :: Document -> Bool
anyInvitationUndelivered doc =  any isUndelivered $ documentsignatorylinks doc

{- |
   Get the full name of a SignatoryDetails.
 -}
signatoryname :: SignatoryDetails -> BS.ByteString
signatoryname s = 
  if BS.null $ signatorysndname s
  then signatoryfstname s 
  else signatoryfstname s `BS.append` BS.fromString " " `BS.append` signatorysndname s

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
                     
{- |
   
 -}
isMatchingSignatoryLink :: User -> SignatoryLink -> Bool
isMatchingSignatoryLink user sigLink = isSigLinkForUser user sigLink

{- |
   Add some history to a document.
 -}
appendHistory :: Document -> [DocumentHistoryEntry] -> Document
appendHistory document history = 
    document { documentlog = documentlog document ++ map documentHistoryToDocumentLog history }

{- |
   Is a CancelationReason due to ELegDataMismatch?
 -}
isELegDataMismatch :: CancelationReason -> Bool
isELegDataMismatch (ELegDataMismatch _ _ _ _ _) = True
isELegDataMismatch _                            = False

{- |
   Does document allow idtype?
 -}
allowsIdentification :: Document -> IdentificationType -> Bool
allowsIdentification document idtype = idtype `elem` documentallowedidtypes document

{- |
   Is the user able to view the doc?
 -}
isViewer :: Document -> User -> Bool
isViewer doc user = any (isSigLinkForUser user) $ documentsignatorylinks doc

-- Not ready to refactor this quite yet.
isEligibleForReminder :: Maybe User -> Document -> SignatoryLink -> Bool
isEligibleForReminder muser doc siglink = isEligibleForReminder'' muser doc siglink

{- |
   Is this Document closed?
 -}
isClosed :: Document -> Bool
isClosed doc = documentstatus doc == Closed

-- this should actually not take Maybe User but User instead
{- |
    Checks whether a signatory link is eligible for sending a reminder.
    The user must be the author, and the signatory musn't be the author.
    Also the signatory must be next in the signorder, and also not be a viewer.
    In addition the document must be in the correct state.  There's quite a lot to check!
-}
isEligibleForReminder' :: User -> Document -> SignatoryLink -> Bool
isEligibleForReminder' user document siglink = 
  isActivatedSignatory (documentcurrentsignorder document) siglink
  && isUserAuthor document user
  && not (isSigLinkForUser user siglink)
  && isDocumentEligibleForReminder document
  && not (isUndelivered siglink)
  && not (isDeferred siglink)
  && isClosed document
  && isSignatory siglink
  
isEligibleForReminder'' :: Maybe User -> Document -> SignatoryLink -> Bool  
isEligibleForReminder'' muser document@Document{documentstatus} siglink =
  signatoryActivated 
    && userIsAuthor 
    && not isUserSignator
    && not dontShowAnyReminder
    && invitationdeliverystatus siglink /= Undelivered
    && invitationdeliverystatus siglink /= Deferred
    && (isClosed' || not wasSigned)
    && isSignatoryPartner
  where
    userIsAuthor = maybe False (isUserAuthor document) muser
    isUserSignator = maybe False (flip isSigLinkForUser siglink) muser
    wasSigned = isJust (maybesigninfo siglink) && not (isUserSignator && (documentstatus == AwaitingAuthor))
    isClosed' = documentstatus == Closed
    signatoryActivated = documentcurrentsignorder document >= signatorysignorder (signatorydetails siglink)
    dontShowAnyReminder = documentstatus `elem` [Timedout, Canceled, Rejected]
    isSignatoryPartner = SignatoryPartner `elem` signatoryroles siglink
    
-- Please define this better. Maybe in the positive?    
{- |
   Is this document eligible for a reminder (depends on documentstatus)?
 -}
isDocumentEligibleForReminder :: Document -> Bool
isDocumentEligibleForReminder doc = not $ documentstatus doc `elem` [Timedout, Canceled, Rejected]
    
{- |
    Removes the field placements and the custom fields.
-}
removeFieldsAndPlacements :: SignatoryDetails -> SignatoryDetails
removeFieldsAndPlacements sd = sd { signatoryfstnameplacements = []
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
replaceSignOrder signorder sd = sd { signatorysignorder = signorder
                                   }

{- |
   Get the Just SignatoryLink from doc that has sid. Nothing when not found.
 -}
signlinkFromDocById :: Document -> SignatoryLinkID -> Maybe SignatoryLink
signlinkFromDocById doc sid = find ((== sid) . signatorylinkid) (documentsignatorylinks  doc)

{- |
   Does this SignatoryLink belong to the User with userid?
 -}
isSigLinkForUserID :: UserID -> SignatoryLink -> Bool
isSigLinkForUserID userid siglink = Just userid == maybesignatory siglink

isSigLinkForSupervisor :: UserID -> SignatoryLink -> Bool
isSigLinkForSupervisor userid siglink = Just userid == maybesupervisor siglink


{- |
   Get the SignatoryLink for a given UserID from a Document.
 -}
getSigLinkForUserID :: Document -> UserID -> Maybe SignatoryLink
getSigLinkForUserID doc uid = find (isSigLinkForUserID uid) $ documentsignatorylinks doc

{- |
   Does the siglink belong to a user with userid and email?
 -}
isSigLinkForUserInfo :: UserID -> BS.ByteString -> SignatoryLink -> Bool
isSigLinkForUserInfo userid email siglink = 
    Just userid == maybesignatory siglink
       || isSigLinkForEmail email siglink
       
       
isSigLinkForEmail :: BS.ByteString -> SignatoryLink -> Bool
isSigLinkForEmail email siglink = email == signatoryemail (signatorydetails siglink)

sigLinkForEmail :: Document -> BS.ByteString -> Maybe SignatoryLink
sigLinkForEmail doc email = find (isSigLinkForEmail email) (documentsignatorylinks doc)

{- |
   Given a document, a userid, and an email, return the SignatoryLink that belongs to the User with the email or userid.
 -}
getSigLinkForUserInfo :: UserID -> BS.ByteString -> Document -> Maybe SignatoryLink
getSigLinkForUserInfo userid email document = find (isSigLinkForUserInfo userid email) $ documentsignatorylinks document

{- |
   Does the SignatoryLink belong to User?
 -}
isSigLinkForUser :: User -> SignatoryLink -> Bool
isSigLinkForUser user = isSigLinkForUserInfo (userid user) (unEmail $ useremail $ userinfo user)

{- |
   Get the siglink for a specific user.
-}
getSigLinkForUser :: Document -> User -> Maybe SignatoryLink
getSigLinkForUser doc user = find (isSigLinkForUser user) $ documentsignatorylinks doc

{- |
   Can the user view this document directly? (not counting friends)
 -}
canUserInfoViewDirectly :: UserID -> BS.ByteString -> Document -> Bool
canUserInfoViewDirectly userid email doc = 
  case getSigLinkForUserInfo userid email doc of
    Nothing                                                                    -> False
    Just siglink | signatorylinkdeleted siglink                                -> False
    Just siglink | siglinkIsAuthor siglink                                     -> True
    Just _       | Preparation == documentstatus doc                           -> False
    Just siglink | isActivatedSignatory (documentcurrentsignorder doc) siglink -> True
    _                                                                          -> False

{- |
   Can a user view this document?
 -}
canUserViewDirectly :: User -> Document -> Bool
canUserViewDirectly user = canUserInfoViewDirectly (userid user) (unEmail $ useremail $ userinfo user)

{- |
   Is this SignatoryLinkID for this SignatoryLink?
 -}
isSigLinkIDForSigLink :: SignatoryLinkID -> SignatoryLink -> Bool
isSigLinkIDForSigLink siglinkid siglink = siglinkid == signatorylinkid siglink

{- |
   Get the SignatoryLink from Document with SignatoryLinkID.
 -}
getSigLinkBySigLinkID :: SignatoryLinkID -> Document -> Maybe SignatoryLink
getSigLinkBySigLinkID siglinkid = 
  find (isSigLinkIDForSigLink siglinkid) . documentsignatorylinks

{- |
   Has the signatory's sign order come up?
 -}
isActivatedSignatory :: SignOrder -> SignatoryLink -> Bool
isActivatedSignatory signorder siglink = 
  signorder >= signatorysignorder (signatorydetails siglink)

{- |
   Given a SignOrder and a SignatoryLink, determine whether the
   SignatoryLink is the next one up.
 -}
isCurrentSignatory :: SignOrder -> SignatoryLink -> Bool
isCurrentSignatory signorder siglink =
  signorder == signatorysignorder (signatorydetails siglink)

{- |
   Is this SignatoryLink an author?
 -}
siglinkIsAuthor :: SignatoryLink -> Bool
siglinkIsAuthor siglink = SignatoryAuthor `elem` signatoryroles siglink

{- |
   Is the Author of this Document a signatory (not a Secretary)?
 -}
isAuthorSignatory :: Document -> Bool
isAuthorSignatory document =
  case getAuthorSigLink document of
    Just siglink -> isSignatory siglink
    _ -> False

{- |
   Is this user the author of doc?
 -}
isUserAuthor :: Document -> User -> Bool
isUserAuthor doc user = maybe False siglinkIsAuthor $ getSigLinkForUser doc user

{- |
   Does the author of this doc have this userid?
 -}
isUserIDAuthor :: Document -> UserID -> Bool
isUserIDAuthor doc userid = maybe False (isSigLinkForUserID userid) $ getAuthorSigLink doc

{- |
   Is the document deleted for this userid?
 -}
isDeletedForUserID :: Document -> UserID -> Bool
isDeletedForUserID doc userid = maybe False signatorylinkdeleted $ getSigLinkForUserID doc userid


{- | Add a tag to tag list -}
addTag:: [DocumentTag] -> (BS.ByteString,BS.ByteString) -> [DocumentTag]
addTag ((DocumentTag n v):ts) (n',v') = if n == n'
                           then (DocumentTag n v') : ts 
                           else (DocumentTag n v)  : (addTag ts (n',v'))
addTag _ (n,v) = [DocumentTag n v]        

{- |
   The user with id uid is a friend of user.
   Should be moved to User and imported
 -}
isFriendOf :: UserID -> User -> Bool
isFriendOf uid user = (unUserID uid `elem` map unFriend (userfriends user) || Just (SupervisorID $ unUserID uid) == usersupervisor user)

isFriendOf' :: UserID -> Maybe User -> Bool
isFriendOf' uid muser = fromMaybe False $ fmap (isFriendOf uid) muser

{- |
   Given a Document, return the best guess at the author's name:
     * First Name + Last Name
     * email address if no name info
-}
getAuthorName :: Document -> BS.ByteString
getAuthorName doc = 
  let Just authorsiglink = getAuthorSigLink doc
  in personname authorsiglink

