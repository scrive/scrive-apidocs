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

import Util.HasSomeUserInfo
import Doc.DocStateData
import API.Service.ServiceState
import Mails.MailsUtil
import Misc
import Templates.Templates
import User.UserState
import Util.SignatoryLinkUtils

import Control.Monad
import Data.List hiding (insert)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

-- some docs please? -EN
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
isDeletableDocument doc =
    (not  $ (documentstatus doc) `elem` [Pending, AwaitingAuthor]) -- We dont allow to delete pending documents
    || (isAttachment doc || isTemplate doc)  -- But attachments and templates never can be pending (it they are this is a bug somewere else)
    
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
                                                   , not $ isAuthor sl]

{- |
   Insert the first list between each list of the second list.
 -}
joinWith :: [a] -> [[a]] -> [a]
joinWith _ [] = []
joinWith _ [x] = x
joinWith s (x:xs) = x ++ s ++ joinWith s xs

-- where does this go? -EN
renderListTemplate :: TemplatesMonad m => [String] -> m String
renderListTemplate list =
  if length list > 1
     then renderTemplateFM "morethenonelist" $ do
         field "list" $ init list
         field "last" $ last list
     else renderTemplateFM "nomorethanonelist" $ field "list" list

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

instance  MaybeTemplate DocumentType where
   isTemplate (Template _) = True
   isTemplate AttachmentTemplate = True
   isTemplate _ = False

instance  MaybeTemplate Document where
   isTemplate =  isTemplate . documenttype

class MaybeAttachment a where
   isAttachment :: a -> Bool

instance  MaybeAttachment DocumentType where
   isAttachment t =  (t == AttachmentTemplate) || (t == Attachment)

instance  MaybeAttachment Document where
   isAttachment =  isAttachment . documenttype


-- does this need to change now? -EN
checkCSVSigIndex :: [SignatoryLink] -> Int -> Either String Int
checkCSVSigIndex sls n
  | n<0 || n>=slcount = Left $ "signatory with index " ++ show n ++ " doesn't exist."
  | n==0 && slcount>0 && isAuthor (head sls) = Left "author can't be set from csv"
  | otherwise = Right n
  where slcount = length sls

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
            && not (isAuthor siglnk) -- we omit author

{- |
   Build a SignatoryDetails from a User with no fields
 -}
signatoryDetailsFromUser :: User -> SignatoryDetails
signatoryDetailsFromUser user =
    SignatoryDetails { signatoryfstname                  = getFirstName      user
                     , signatorysndname                  = getLastName       user
                     , signatoryemail                    = getEmail          user
                     , signatorycompany                  = getCompanyName    user
                     , signatorypersonalnumber           = getPersonalNumber user
                     , signatorycompanynumber            = getCompanyNumber  user
                     , signatorysignorder                = SignOrder 1
                     , signatoryfstnameplacements        = []
                     , signatorysndnameplacements        = []
                     , signatorycompanyplacements        = []
                     , signatoryemailplacements          = []
                     , signatorypersonalnumberplacements = []
                     , signatorycompanynumberplacements  = []
                     , signatoryotherfields              = []
                     }

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
  && isAuthor (document, user)
  && not (isSigLinkFor user siglink)
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
    userIsAuthor = isAuthor (document, muser)
    isUserSignator = isSigLinkFor muser siglink
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
replaceSignOrder signorder sd = sd { signatorysignorder = signorder }

{- |
   Can the user view this document directly? (not counting friends)
 -}
canUserInfoViewDirectly :: UserID -> BS.ByteString -> Document -> Bool
canUserInfoViewDirectly userid email doc =
  case getSigLinkFor doc (userid, email) of
    Nothing                                                                    -> False
    Just siglink | signatorylinkdeleted siglink                                -> False
    Just siglink | isAuthor siglink                                            -> True
    Just _       | Preparation == documentstatus doc                           -> False
    Just siglink | isActivatedSignatory (documentcurrentsignorder doc) siglink -> True
    _                                                                          -> False

{- |
   Can a user view this document?
 -}
canUserViewDirectly :: User -> Document -> Bool
canUserViewDirectly user = canUserInfoViewDirectly (userid user) (getEmail user)

{- |
   Has the signatory's sign order come up?
 -}
isActivatedSignatory :: SignOrder -> SignatoryLink -> Bool
isActivatedSignatory signorder siglink =
  (not $ isAuthor siglink) &&
  signorder >= signatorysignorder (signatorydetails siglink)

{- |
   Given a SignOrder and a SignatoryLink, determine whether the
   SignatoryLink is the next one up.
 -}
isCurrentSignatory :: SignOrder -> SignatoryLink -> Bool
isCurrentSignatory signorder siglink =
  (not $ isAuthor siglink) &&
  signorder == signatorysignorder (signatorydetails siglink)


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


samenameanddescription :: BS.ByteString -> BS.ByteString -> (BS.ByteString, BS.ByteString, [(BS.ByteString, BS.ByteString)]) -> Bool
samenameanddescription n d (nn, dd, _) = n == nn && d == dd

buildattach :: String -> Document -> [SignatoryAttachment] 
               -> [(BS.ByteString, BS.ByteString, [(BS.ByteString, BS.ByteString)])]
               -> [(BS.ByteString, BS.ByteString, [(BS.ByteString, BS.ByteString)])]
buildattach _ _ [] a = a
buildattach csvstring d (f:fs) a =
  case getSigLinkFor d (signatoryattachmentemail f) of
    Nothing -> if signatoryattachmentemail f == BS.fromString "csv"
               then case find (samenameanddescription (signatoryattachmentname f) (signatoryattachmentdescription f)) a of
                 Nothing -> buildattach csvstring d fs (((signatoryattachmentname f), (signatoryattachmentdescription f), [(BS.fromString csvstring, BS.fromString "csv")]):a)
                 Just (nx, dx, sigs) -> buildattach csvstring d fs ((nx, dx, (BS.fromString csvstring, BS.fromString "csv"):sigs):(delete (nx, dx, sigs) a))
               else buildattach csvstring d fs a
    Just sl -> case find (samenameanddescription (signatoryattachmentname f) (signatoryattachmentdescription f)) a of
      Nothing -> buildattach csvstring d fs (((signatoryattachmentname f), (signatoryattachmentdescription f), [(getFullName sl, getEmail sl)]):a)
      Just (nx, dx, sigs) -> buildattach csvstring d fs ((nx, dx, (getFullName sl, getEmail sl):sigs):(delete (nx, dx, sigs) a))

sameDocID :: Document -> Document -> Bool
sameDocID doc1 doc2 = (documentid doc1) == (documentid doc2)
