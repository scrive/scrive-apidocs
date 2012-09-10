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

import Control.Logic
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Doc.DocStateData
import Templates.Templates
import User.Model
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Company.Model
import DB
import Utils.Prelude
import qualified Templates.Fields as F

import Control.Monad
import Data.List hiding (insert)
import Data.Maybe
import File.Model
import Control.Applicative
import MinutesTime
import Kontra
import qualified Data.Map as Map
import MagicHash

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

renderListTemplateNormal :: TemplatesMonad m => [String] -> m String
renderListTemplateNormal = renderListTemplateNormalHelper renderTemplate

renderListTemplateNormalHelper :: TemplatesMonad m
                         => (String -> Fields m () -> m String)
                         -> [String]
                         -> m String
renderListTemplateNormalHelper renderFunc list =
  if length list > 1
     then renderFunc "morethenonelistnormal" $ do
         F.value "list" $ init list
         F.value "last" $ last list
     else renderFunc "nomorethanonelistnormal" $ F.value "list" list


-- where does this go? -EN
renderListTemplate :: TemplatesMonad m => [String] -> m String
renderListTemplate = renderListTemplateHelper renderTemplate

renderLocalListTemplate :: (HasLocale a, TemplatesMonad m) => a -> [String] -> m String
renderLocalListTemplate = renderListTemplateHelper .renderLocalTemplate

renderListTemplateHelper :: TemplatesMonad m
                         => (String -> Fields m () -> m String)
                         -> [String]
                         -> m String
renderListTemplateHelper renderFunc list =
  if length list > 1
     then renderFunc "morethenonelist" $ do
         F.value "list" $ init list
         F.value "last" $ last list
     else renderFunc "nomorethanonelist" $ F.value "list" list

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
   Is a CancelationReason due to ELegDataMismatch?
 -}
isELegDataMismatch :: CancelationReason -> Bool
isELegDataMismatch (ELegDataMismatch _ _ _ _ _) = True
isELegDataMismatch _                            = False

allowsAuthMethod :: Document -> AuthenticationMethod -> Bool
allowsAuthMethod doc auth = documentauthenticationmethod doc == auth

{- | Determine is document is designed to be signed using pad - this determines if invitation emais are send and if author can get access to siglink -}
sendMailsDurringSigning :: Document -> Bool
sendMailsDurringSigning doc = (not $ documentdeliverymethod doc == PadDelivery) && (not $ documentdeliverymethod doc == APIDelivery)

hasOtherSignatoriesThenAuthor :: Document -> Bool
hasOtherSignatoriesThenAuthor doc = not . null $ filter (isSignatory &&^ not . isAuthor) $ documentsignatorylinks doc

{- |
    Checks whether a signatory link is eligible for sending a reminder.
    The user must be the author, and the signatory musn't be the author.
    Also the signatory must be next in the signorder, and also not be a viewer.
    In addition the document must be in the correct state.  There's quite a lot to check!
-}
isEligibleForReminder :: User -> Document -> SignatoryLink -> Bool
isEligibleForReminder user document@Document{documentstatus} siglink = 
  signatoryActivated
    && userIsAuthor
    && not isUserSignator
    && not dontShowAnyReminder
    && invitationdeliverystatus siglink /= Undelivered
    && invitationdeliverystatus siglink /= Deferred
    && wasNotSigned
    && isSignatoryPartner
  where
    userIsAuthor = isAuthor (document, user)
    isUserSignator = isSigLinkFor user siglink
    wasNotSigned = isNothing (maybesigninfo siglink)
    signatoryActivated = documentcurrentsignorder document >= signatorysignorder (signatorydetails siglink)
    dontShowAnyReminder = documentstatus `elem` [Timedout, Canceled, Rejected]
    isSignatoryPartner = SignatoryPartner `elem` signatoryroles siglink

-- | Can author sign now according to sign order?
canAuthorSignNow :: Document -> Bool
canAuthorSignNow doc =
     isPending doc
  && documentcurrentsignorder doc >= signatorysignorder (signatorydetails author)
  && (not . hasSigned $ author)
  && isSignatory author
  where author = case getAuthorSigLink doc of
                   Just a -> a
                   _ -> error $ "Document " ++ show (documentid doc) ++ " does not have author"

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
  signorder >= signatorysignorder (signatorydetails siglink)

{- |
   Given a SignOrder and a SignatoryLink, determine whether the
   SignatoryLink is the next one up.
 -}
isCurrentSignatory :: SignOrder -> SignatoryLink -> Bool
isCurrentSignatory signorder siglink =
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

getFilesByStatus :: MonadDB m => Document -> m [File]
getFilesByStatus doc = liftM catMaybes $ mapM dbQuery (GetFileByFileID <$> (getFileIDsByStatus doc))


documentfilesM :: MonadDB m => Document -> m [File]
documentfilesM Document{documentfiles} = do
  catMaybes `liftM` mapM (dbQuery . GetFileByFileID) documentfiles

documentsealedfilesM :: MonadDB m => Document -> m [File]
documentsealedfilesM Document{documentsealedfiles} = do
  catMaybes `liftM` mapM (dbQuery . GetFileByFileID) documentsealedfiles

fileInDocument :: Document -> FileID -> Bool
fileInDocument doc fid =
    elem fid $      (documentfiles doc)
                 ++ (documentsealedfiles doc)
                 ++ (fmap authorattachmentfile $ documentauthorattachments doc)
                 ++ (catMaybes $ fmap signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc)
                 
mainFileOfDocument :: Document -> FileID -> Bool
mainFileOfDocument doc fid = fid `elem` (documentfiles doc)
                 
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
       (maybeToList $ signtime <$> maybeseeninfo sl)
    ++ (maybeToList $ signtime <$> maybesigninfo sl)
    ++ (maybeToList $ id       <$> maybereadinvite sl)))


getMagicHashFromContext :: (KontraMonad m) => SignatoryLinkID -> m (Maybe MagicHash)
getMagicHashFromContext slid = do
  Context{ctxmagichashes} <- getContext
  return $! Map.lookup slid ctxmagichashes


addMagicHashToContext :: (KontraMonad m) => SignatoryLinkID -> MagicHash -> m ()
addMagicHashToContext slid mh = do
  modifyContext (\ctx -> ctx { ctxmagichashes = Map.insert slid mh (ctxmagichashes ctx) })
