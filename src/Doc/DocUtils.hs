{-# OPTIONS_GHC -Wall -Werror #-}

module Doc.DocUtils where

import Doc.DocStateData
import Mails.MailsUtil
import Misc
import Templates.Templates 
import User.UserState

import Control.Monad
import Data.List hiding (insert)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

isSignatory :: SignatoryLink -> Bool
isSignatory sl = SignatoryPartner `elem` signatoryroles sl

hasSigned :: SignatoryLink -> Bool
hasSigned sl = isJust $ maybesigninfo sl

partyList :: Document -> [SignatoryDetails]
partyList document = [signatorydetails sl | sl <- documentsignatorylinks document
                                          , isSignatory sl]
  
partyUnsignedList :: Document -> [SignatoryDetails]
partyUnsignedList document = [signatorydetails sl | sl <- documentsignatorylinks document
                                                  , isSignatory sl
                                                  , not $ hasSigned sl]

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

partyListButAuthor :: Document -> [SignatoryDetails]
partyListButAuthor document = [signatorydetails sl | sl <- documentsignatorylinks document
                                                   , isSignatory sl
                                                   , not $ siglinkIsAuthor sl]
  
joinWith :: [a] -> [[a]] -> [a]
joinWith _ [] = []
joinWith _ [x] = x
joinWith s (x:xs) = x ++ s ++ joinWith s xs

{- Either a signatory name or email address. We dont want to show empty strings -}
personname :: SignatoryLink -> BS.ByteString 
personname = personname' . signatorydetails 

{- Same but unwrapped. We need this cause author details are in this format  -}
personname' :: SignatoryDetails -> BS.ByteString 
personname' signdetails = if BS.null $ signatoryname signdetails
                          then  signatoryemail signdetails
                          else  signatoryname  signdetails

{- Function for changing SignatoryLink into our inner email address so u dont have to unwrap every time-}
emailFromSignLink::SignatoryLink->(BS.ByteString,BS.ByteString)
emailFromSignLink sl = (signatoryname $ signatorydetails sl,signatoryemail $ signatorydetails sl) 

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

checkCSVSigIndex :: [SignatoryLink] -> Int -> Either String Int
checkCSVSigIndex sls n
  | n<0 || n>=slcount = Left $ "signatory with index " ++ show n ++ " doesn't exist."
  | n==0 && slcount>0 && siglinkIsAuthor (head sls) = Left "author can't be set from csv"
  | otherwise = Right n
  where slcount = length sls

-- GETTERS

undeliveredSignatoryLinks :: Document -> [SignatoryLink]
undeliveredSignatoryLinks doc =  filter ((== Undelivered) . invitationdeliverystatus) $ documentsignatorylinks doc

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
                     
isMatchingSignatoryLink :: User -> SignatoryLink -> Bool
isMatchingSignatoryLink user sigLink = isSigLinkForUser user sigLink
  

appendHistory :: Document -> [DocumentHistoryEntry] -> Document
appendHistory document history = 
    document { documentlog = documentlog document ++ map documentHistoryToDocumentLog history }

isELegDataMismatch :: CancelationReason -> Bool
isELegDataMismatch (ELegDataMismatch _ _ _ _ _) = True
isELegDataMismatch _                            = False

allowsIdentification :: Document -> IdentificationType -> Bool
allowsIdentification document idtype = 
  idtype `elem` documentallowedidtypes document

isViewer :: Document -> User -> Bool
isViewer doc user = any (isSigLinkForUser user) $ documentsignatorylinks doc

{- |
    Checks whether a signatory link is eligible for sending a reminder.
    The user must be the author, and the signatory musn't be the author.
    Also the signatory must be next in the signorder, and also not be a viewer.
    In addition the document must be in the correct state.  There's quite a lot to check!
-}
isEligibleForReminder :: Maybe User -> Document -> SignatoryLink -> Bool
isEligibleForReminder muser document@Document{documentstatus} siglink = 
  signatoryActivated 
    && userIsAuthor 
    && not isUserSignator
    && not dontShowAnyReminder
    && invitationdeliverystatus siglink /= Undelivered
    && invitationdeliverystatus siglink /= Deferred
    && (isClosed || not wasSigned)
    && isSignatoryPartner
  where
    userIsAuthor = maybe False (isUserAuthor document) muser
    isUserSignator = maybe False (flip isSigLinkForUser siglink) muser
    wasSigned = isJust (maybesigninfo siglink) && not (isUserSignator && (documentstatus == AwaitingAuthor))
    isClosed = documentstatus == Closed
    signatoryActivated = documentcurrentsignorder document >= signatorysignorder (signatorydetails siglink)
    dontShowAnyReminder = documentstatus `elem` [Timedout, Canceled, Rejected]
    isSignatoryPartner = SignatoryPartner `elem` signatoryroles siglink

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
   Get the author's signatory link.
 -}
getAuthorSigLink :: Document -> Maybe SignatoryLink
getAuthorSigLink = find (elem SignatoryAuthor . signatoryroles) . documentsignatorylinks

isSigLinkForUserID :: UserID -> SignatoryLink -> Bool
isSigLinkForUserID userid siglink = Just userid == maybesignatory siglink

getSigLinkForUserID :: Document -> UserID -> Maybe SignatoryLink
getSigLinkForUserID doc uid = find (isSigLinkForUserID uid) $ documentsignatorylinks doc

{- |
   Does the siglink belong to a user with userid and email?
 -}
isSigLinkForUserInfo :: UserID -> BS.ByteString -> SignatoryLink -> Bool
isSigLinkForUserInfo userid email siglink = 
    Just userid == maybesignatory siglink
       || email == signatoryemail (signatorydetails siglink)
       
getSigLinkForUserInfo :: UserID -> BS.ByteString -> Document -> Maybe SignatoryLink
getSigLinkForUserInfo userid email document = find (isSigLinkForUserInfo userid email) $ documentsignatorylinks document

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

canUserViewDirectly :: User -> Document -> Bool
canUserViewDirectly user = canUserInfoViewDirectly (userid user) (unEmail $ useremail $ userinfo user)

isSigLinkIDForSigLink :: SignatoryLinkID -> SignatoryLink -> Bool
isSigLinkIDForSigLink siglinkid siglink = siglinkid == signatorylinkid siglink

getSigLinkBySigLinkID :: SignatoryLinkID -> Document -> Maybe SignatoryLink
getSigLinkBySigLinkID siglinkid = 
  find (isSigLinkIDForSigLink siglinkid) . documentsignatorylinks

{- |
   Has the signatory's sign order come up?
 -}
isActivatedSignatory :: SignOrder -> SignatoryLink -> Bool
isActivatedSignatory signorder siglink = 
  signorder >= signatorysignorder (signatorydetails siglink)

isCurrentSignatory :: SignOrder -> SignatoryLink -> Bool
isCurrentSignatory signorder siglink =
  signorder == signatorysignorder (signatorydetails siglink)

siglinkIsAuthor :: SignatoryLink -> Bool
siglinkIsAuthor siglink = SignatoryAuthor `elem` signatoryroles siglink

isAuthorSignatory :: Document -> Bool
isAuthorSignatory document =
  case getAuthorSigLink document of
    Just siglink -> isSignatory siglink
    _ -> False

isUserAuthor :: Document -> User -> Bool
isUserAuthor doc user = maybe False siglinkIsAuthor $ getSigLinkForUser doc user

isUserIDAuthor :: Document -> UserID -> Bool
isUserIDAuthor doc userid = maybe False (isSigLinkForUserID userid) $ getAuthorSigLink doc

isDeletedForUserID :: Document -> UserID -> Bool
isDeletedForUserID doc userid = maybe False signatorylinkdeleted $ getSigLinkForUserID doc userid

isAttachment :: Document -> Bool
isAttachment doc = Attachment == documenttype doc


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

getAuthorName :: Document -> BS.ByteString
getAuthorName doc = 
  let Just authorsiglink = getAuthorSigLink doc
      authorfstname = signatoryfstname $ signatorydetails authorsiglink
      authorsndname = signatorysndname $ signatorydetails authorsiglink
      authoremail   = signatoryemail   $ signatorydetails authorsiglink
      authorname    = BS.concat [authorfstname, BS.fromString " ", authorsndname]
  in if BS.null authorname then authoremail else authorname

