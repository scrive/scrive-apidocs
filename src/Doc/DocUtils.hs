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

import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Doc.DocStateData
import Templates.Templates
import User.Model
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Company.Model
import DB.Classes
import Misc

import Control.Monad
import Control.Monad.IO.Class
import Data.List hiding (insert)
import Data.Maybe
--import qualified Data.ByteString as BS
import File.Model
import Control.Applicative
import MinutesTime

--import Happstack.State


{- |
    Checks whether the document is deletable, this is not the case for live documents.
-}
isDeletableDocument :: Document -> Bool
isDeletableDocument doc =
    (not  $ (documentstatus doc) `elem` [Pending, AwaitingAuthor]) -- We dont allow to delete pending documents

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
renderListTemplate = renderListTemplate' renderTemplateFM

renderLocalListTemplate :: (HasLocale a, TemplatesMonad m) => a -> [String] -> m String
renderLocalListTemplate haslocale = renderListTemplate' (renderLocalTemplateFM haslocale)

renderListTemplate' :: TemplatesMonad m
                       => (String -> Fields m -> m String)
                       -> [String]
                       -> m String
renderListTemplate' renderFunc list =
  if length list > 1
     then renderFunc "morethenonelist" $ do
         field "list" $ init list
         field "last" $ last list
     else renderFunc "nomorethanonelist" $ field "list" list

-- CHECKERS

{- |
  We introduce some types that basicly describes the user. No we want a unified way of comparring them.
-}
class MaybeUser u where
  getUserID:: u -> Maybe UserID

instance MaybeUser SignatoryLink where
  getUserID  = maybesignatory

instance MaybeUser User where
  getUserID = Just . userid

instance MaybeUser UserID where
  getUserID = Just

instance (MaybeUser u) => MaybeUser (Maybe u) where
  getUserID  = join . fmap getUserID

{- |  And this is a function for comparison -}
sameUser:: (MaybeUser u1, MaybeUser u2) =>  u1 ->  u2 -> Bool
sameUser u1 u2 = getUserID u1 == getUserID u2

class MaybeTemplate a where
   isTemplate :: a -> Bool
   isSignable :: a -> Bool

instance MaybeTemplate DocumentType where
   isTemplate (Template _) = True
   isTemplate _ = False
   isSignable (Signable _) = True
   isSignable _ = False

instance MaybeTemplate Document where
   isTemplate = isTemplate . documenttype
   isSignable = isSignable . documenttype

class MaybeAttachment a where
   isAttachment :: a -> Bool

instance  MaybeAttachment DocumentType where
   isAttachment t = (t == Attachment)

instance  MaybeAttachment Document where
   isAttachment =  isAttachment . documenttype

class MaybeShared a where
    isShared :: a -> Bool

instance  MaybeShared Document where
    isShared doc = documentsharing doc == Shared

class HasFieldType a where
    fieldType :: a -> FieldType

instance HasFieldType FieldType where
    fieldType = id
    
instance HasFieldType SignatoryField where
    fieldType = sfType
        
matchingFieldType:: (HasFieldType a, HasFieldType b) => a -> b -> Bool
matchingFieldType a b = case (fieldType a, fieldType b) of
                        (CustomFT a' _, CustomFT b' _) -> a' == b'
                        (a',b') -> a' == b'

class HasFields a where
    replaceField :: SignatoryField -> a -> a
    getAllFields:: a ->  [SignatoryField]
    
instance HasFields  [SignatoryField] where
    getAllFields = id
    replaceField f fs = if (any (matchingFieldType f) fs) 
                                then map (\f' ->  f <| (matchingFieldType f f') |> f' )  fs
                                else fs  ++ [f]

instance HasFields SignatoryDetails where
    getAllFields = getAllFields . signatoryfields
    replaceField f s = s {signatoryfields = replaceField f (signatoryfields s) } 

instance HasFields SignatoryLink where
    getAllFields =  getAllFields . signatorydetails
    replaceField f s = s {signatorydetails = replaceField f (signatorydetails s) }     

replaceFieldValue :: HasFields a =>  FieldType -> String -> a -> a
replaceFieldValue ft v a = case (find (matchingFieldType ft) $ getAllFields a) of
                            Just f  -> replaceField (f { sfType = ft, sfValue = v}) a
                            Nothing -> replaceField (SignatoryField { sfType = ft, sfValue = v, sfPlacements =[]}) a

-- does this need to change now? -EN
checkCSVSigIndex :: [SignatoryLink] -> Int -> Either String Int
checkCSVSigIndex sls n
  | n < 0 || n >= length sls = Left $ "checkCSVSigIndex: signatory with index " ++ show n ++ " doesn't exist."
  | isAuthor (sls !! n) = Left $ "checkCSVSigIndex: signatory at index " ++ show n ++ " is an author and can't be set from csv"
  | otherwise = Right n

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
signatoryDetailsFromUser :: User -> Maybe Company -> SignatoryDetails
signatoryDetailsFromUser user mcompany = SignatoryDetails {
    signatorysignorder = SignOrder 1
  , signatoryfields = [
      toSF FirstNameFT $ getFirstName user
    , toSF LastNameFT $ getLastName user
    , toSF EmailFT $ getEmail user
    , toSF CompanyFT $ getCompanyName (user, mcompany)
    , toSF PersonalNumberFT $ getPersonalNumber user
    , toSF CompanyNumberFT $ getCompanyNumber (user, mcompany)
    ]
  }
  where
    toSF t v = SignatoryField {
        sfType = t
      , sfValue = v
      , sfPlacements = []
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
removeFieldsAndPlacements sd = sd { signatoryfields = filter (not . isFieldCustom)
  $ map (\sf -> sf { sfPlacements = [] }) $ signatoryfields sd }

hasFieldsAndPlacements :: SignatoryDetails -> Bool
hasFieldsAndPlacements sd = any (isFieldCustom ||^ (not . Data.List.null . sfPlacements)) (signatoryfields sd)

{- |
    Sets the sign order on some signatory details.
-}
replaceSignOrder :: SignOrder -> SignatoryDetails -> SignatoryDetails
replaceSignOrder signorder sd = sd { signatorysignorder = signorder }

{- |
   Can the user view this document directly?
 -}
canUserInfoViewDirectly :: UserID -> String -> Document -> Bool
canUserInfoViewDirectly userid email doc =
    (checkSigLink' $ getSigLinkFor doc userid) || (checkSigLink' $ getSigLinkFor doc email)
  where 
    checkSigLink' a = case a of
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

type CustomSignatoryField = (SignatoryField, String, Bool)

filterCustomField :: [SignatoryField] -> [CustomSignatoryField]
filterCustomField l = [(sf, cs, cb) | sf@SignatoryField{sfType = CustomFT cs cb} <- l]

isFieldCustom :: SignatoryField -> Bool
isFieldCustom SignatoryField{sfType = CustomFT{}} = True
isFieldCustom _ = False

isStandardField :: SignatoryField -> Bool
isStandardField SignatoryField{sfType = CustomFT{}} = False
isStandardField SignatoryField{sfType = SignatureFT{}} = False
isStandardField _ = True

findCustomField :: HasFields a => String -> a -> Maybe SignatoryField
findCustomField name = find (matchingFieldType (CustomFT name False)) . getAllFields

{- | Add a tag to tag list -}
addTag:: [DocumentTag] -> (String, String) -> [DocumentTag]
addTag ((DocumentTag n v):ts) (n',v') = if n == n'
                           then (DocumentTag n v') : ts
                           else (DocumentTag n v)  : (addTag ts (n',v'))
addTag _ (n,v) = [DocumentTag n v]

samenameanddescription :: String -> String -> (String, String, [(String, String)]) -> Bool
samenameanddescription n d (nn, dd, _) = n == nn && d == dd

getSignatoryAttachment :: Document -> SignatoryLinkID -> String -> Maybe SignatoryAttachment
getSignatoryAttachment doc slid name = join $ find (\a -> name == signatoryattachmentname a) 
                                       <$> signatoryattachments 
                                       <$> (find (\sl -> slid == signatorylinkid sl) $ documentsignatorylinks doc)

sameDocID :: Document -> Document -> Bool
sameDocID doc1 doc2 = (documentid doc1) == (documentid doc2)


isAuthoredByCompany :: CompanyID -> Document -> Bool
isAuthoredByCompany companyid doc = (getAuthorSigLink doc >>= maybecompany) == Just companyid

isAuthorAdmin :: User -> Document -> Bool
isAuthorAdmin user doc =
  useriscompanyadmin user
  && maybe False (flip isAuthoredByCompany doc) (usercompany user)

getFileIDsByStatus :: Document -> [FileID]
getFileIDsByStatus doc
  | isClosed doc =  documentsealedfiles doc
  | otherwise    =  documentfiles doc

getFilesByStatus :: (MonadIO m, DBMonad m) => Document -> m [File]
getFilesByStatus doc = liftM catMaybes $ mapM runDBQuery (GetFileByFileID <$> (getFileIDsByStatus doc))


documentfilesM :: Document -> DB [File]
documentfilesM Document{documentfiles} = do
    liftM catMaybes $ mapM (dbQuery . GetFileByFileID) documentfiles

documentsealedfilesM :: Document -> DB [File]
documentsealedfilesM Document{documentsealedfiles} = do
    liftM catMaybes $ mapM (dbQuery . GetFileByFileID) documentsealedfiles

fileInDocument :: Document -> FileID -> Bool
fileInDocument doc fid =
    elem fid $      (documentfiles doc)
                 ++ (documentsealedfiles doc)
                 ++ (fmap authorattachmentfile $ documentauthorattachments doc)
                 ++ (catMaybes $ fmap signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc)

filterPlacementsByID :: [(String, String, FieldPlacement)]
                        -> String
                        -> String
                        -> [FieldPlacement]
filterPlacementsByID placements sigid fieldid =
    [x | (s, f, x) <- placements, s == sigid, f == fieldid]

filterFieldDefsByID :: [(String, SignatoryField)]
                    -> String
                    -> [SignatoryField]
filterFieldDefsByID fielddefs sigid =
    [x | (s, x) <- fielddefs, s == sigid]

makeSignatory ::[(String, String, FieldPlacement)]
                -> [(String, SignatoryField)]
                -> String
                -> String
                -> String
                -> String
                -> SignOrder
                -> String
                -> String
                -> String
                -> SignatoryDetails
makeSignatory pls fds sid sfn  ssn  se  sso  sc  spn  scn = SignatoryDetails {
    signatorysignorder = sso
  , signatoryfields = [
      sf FirstNameFT sfn "fstname"
    , sf LastNameFT ssn "sndname"
    , sf EmailFT se "email"
    , sf CompanyFT sc "company"
    , sf PersonalNumberFT spn "personalnumber"
    , sf CompanyNumberFT scn "companynumber"
    ] ++ filterFieldDefsByID fds sid
  }
  where
    sf ftype value texttype = SignatoryField {
        sfType = ftype
      , sfValue = value
      , sfPlacements = filterPlacementsByID pls sid texttype
    }

recentDate :: Document -> MinutesTime
recentDate doc = 
  maximum $ [documentctime doc, documentmtime doc] ++
  (maybeToList $ signtime <$> documentinvitetime doc) ++
  (maybeToList $ (\(a,_,_) -> a) <$> documentrejectioninfo doc) ++
  concat (for (documentsignatorylinks doc) (\sl ->
                                             (maybeToList $ signtime <$> maybeseeninfo sl) ++
                                             (maybeToList $ signtime <$> maybesigninfo sl) ++
                                             (maybeToList $ id       <$> maybereadinvite sl)))

