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

module Doc.DocUtils(
    renderListTemplateNormal
  , renderListTemplate
  , renderLocalListTemplate
  , replaceFieldValue
  , documentcurrentsignorder
  , signatoryFieldsFromUser
  , isEligibleForReminder
  , canAuthorSignNow
  , canSignatorySignNow
  , isActivatedSignatory
  , isCurrentSignatory
  , isFieldCustom
  , isAuthorAdmin
  , findCustomField
  , getSignatoryAttachment
  , documentfileM
  , documentsealedfileM
  , fileInDocument
  , documentDeletedForUser
  , documentReallyDeletedForUser
  , userCanPerformSigningAction
  , MaybeTemplate(..)
  , HasFields(..)
  , MaybeUser(..)
) where

import Control.Logic
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Doc.DocStateData
import Text.StringTemplates.Templates
import Templates
import User.Model
import Doc.SignatoryLinkID
import Util.SignatoryLinkUtils
import Doc.DocInfo
import Company.Model
import DB
import qualified Text.StringTemplates.Fields as F

import Control.Monad
import Data.List hiding (insert)
import Data.Maybe
import File.Model
import File.File
import Control.Applicative

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

renderLocalListTemplate :: (HasLang a, TemplatesMonad m) => a -> [String] -> m String
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

class MaybeTemplate a where
   isTemplate :: a -> Bool
   isSignable :: a -> Bool

instance MaybeTemplate DocumentType where
   isTemplate t = t == Template
   isSignable t = t == Signable

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

instance HasFields SignatoryLink where
    getAllFields =  signatoryfields
    replaceField f s = s {signatoryfields = replaceField f (signatoryfields s) }

replaceFieldValue :: HasFields a =>  FieldType -> String -> a -> a
replaceFieldValue ft v a = case (find (matchingFieldType ft) $ getAllFields a) of
                            Just f  -> replaceField (f { sfType = ft, sfValue = v}) a
                            Nothing -> replaceField (SignatoryField { sfType = ft, sfValue = v, sfPlacements =[], sfObligatory = True, sfShouldBeFilledBySender = False}) a

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
        signorder = signatorysignorder
        sigs = documentsignatorylinks doc
        notSigned siglnk = isNothing (maybesigninfo siglnk)
          && signatoryispartner siglnk -- we exclude non-signatories

{- |
   Build signatory fields from user
 -}
signatoryFieldsFromUser :: (MonadDB m) => User -> m [SignatoryField]
signatoryFieldsFromUser user = do
  company <- dbQuery $ GetCompanyByUserID (userid user)
  return $
        [ SignatoryField FirstNameFT (getFirstName user) True True []
        , SignatoryField LastNameFT (getLastName user) True True []
        , SignatoryField EmailFT (getEmail user) True True []
        , SignatoryField CompanyFT (getCompanyName company) False False []
        ] ++
        (if (not $ null $ getPersonalNumber user) then [SignatoryField PersonalNumberFT (getPersonalNumber user) False False []] else [])
          ++
        (if (not $ null $ getMobile user) then [SignatoryField MobileFT (getMobile user) False False []] else [])
          ++
        (if (not $ null $ getCompanyNumber company) then [SignatoryField CompanyNumberFT (getCompanyNumber company) False False []] else [])

{- |
    Checks whether a signatory link is eligible for sending a reminder.
    The user must be the author, and the signatory musn't be the author.
    Also the signatory must be next in the signorder, and also not be a viewer.
    In addition the document must be in the correct state.  There's quite a lot to check!
-}
isEligibleForReminder :: User -> Document -> SignatoryLink -> Bool
isEligibleForReminder user document@Document{documentstatus} siglink =
       documentDeliverableTosignatory
    && signatoryActivated
    && userIsAuthor
    && not isUserSignator
    && not dontShowAnyReminder
    && (signatorylinkdeliverymethod siglink /= EmailDelivery || (mailinvitationdeliverystatus siglink /= Undelivered && mailinvitationdeliverystatus siglink /= Deferred))
    && (signatorylinkdeliverymethod siglink /= MobileDelivery || (smsinvitationdeliverystatus siglink /= Undelivered && smsinvitationdeliverystatus siglink /= Deferred))
    && (signatorylinkdeliverymethod siglink /= EmailAndMobileDelivery
                                      || (mailinvitationdeliverystatus siglink /= Undelivered && mailinvitationdeliverystatus siglink /= Deferred)
                                      || (smsinvitationdeliverystatus siglink /= Undelivered && smsinvitationdeliverystatus siglink /= Deferred)
                                      )
    && wasNotSigned
    && signatoryispartner siglink
  where
    userIsAuthor = isAuthor (document, user)
    isUserSignator = isSigLinkFor user siglink
    wasNotSigned = isNothing (maybesigninfo siglink)
    signatoryActivated = documentcurrentsignorder document >= signatorysignorder siglink
    dontShowAnyReminder = documentstatus `elem` [Timedout, Canceled, Rejected]
    documentDeliverableTosignatory =  signatorylinkdeliverymethod siglink `elem` [EmailDelivery, MobileDelivery, EmailAndMobileDelivery]

-- | Can author sign now according to sign order?
canAuthorSignNow :: Document -> Bool
canAuthorSignNow doc =
     isPending doc
  && documentcurrentsignorder doc >= signatorysignorder author
  && (not . hasSigned $ author)
  && isSignatory author
  where author = case getAuthorSigLink doc of
                   Just a -> a
                   _ -> error $ "Document " ++ show (documentid doc) ++ " does not have author"


-- Checks if signatory can sign now
canSignatorySignNow :: Document -> SignatoryLink -> Bool
canSignatorySignNow doc sl =
  isPending doc
  && documentcurrentsignorder doc >= signatorysignorder sl
  && (not . hasSigned $ sl)
  && isSignatory sl



{- |
   Has the signatory's sign order come up?
 -}
isActivatedSignatory :: SignOrder -> SignatoryLink -> Bool
isActivatedSignatory signorder siglink =
  signorder >= signatorysignorder siglink

{- |
   Given a SignOrder and a SignatoryLink, determine whether the
   SignatoryLink is the next one up.
 -}
isCurrentSignatory :: SignOrder -> SignatoryLink -> Bool
isCurrentSignatory signorder siglink =
  signorder == signatorysignorder siglink

isFieldCustom :: SignatoryField -> Bool
isFieldCustom SignatoryField{sfType = CustomFT{}} = True
isFieldCustom _ = False

findCustomField :: HasFields a => String -> a -> Maybe SignatoryField
findCustomField name = find (matchingFieldType (CustomFT name False)) . getAllFields

getSignatoryAttachment :: Document -> SignatoryLinkID -> String -> Maybe SignatoryAttachment
getSignatoryAttachment doc slid name = join $ find (\a -> name == signatoryattachmentname a)
                                       <$> signatoryattachments
                                       <$> (find (\sl -> slid == signatorylinkid sl) $ documentsignatorylinks doc)

isAuthorAdmin :: User -> Document -> Bool
isAuthorAdmin user doc =
  useriscompanyadmin user && (getAuthorSigLink doc >>= maybesignatory) == Just (userid user)

documentfileM :: MonadDB m => Document -> m (Maybe File)
documentfileM = maybe (return Nothing) (fmap Just . dbQuery . GetFileByFileID) . documentfile

documentsealedfileM :: MonadDB m => Document -> m (Maybe File)
documentsealedfileM = maybe (return Nothing) (fmap Just . dbQuery . GetFileByFileID) . documentsealedfile

fileInDocument :: Document -> FileID -> Bool
fileInDocument doc fid =
    elem fid $      maybeToList (documentfile doc)
                 ++ maybeToList (documentsealedfile doc)
                 ++ (fmap authorattachmentfile $ documentauthorattachments doc)
                 ++ (catMaybes $ fmap signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc)


documentDeletedForUser :: Document -> UserID -> Bool
documentDeletedForUser doc uid = fromMaybe False (fmap (isJust . signatorylinkdeleted) $ (getSigLinkFor doc uid `mplus` getAuthorSigLink doc))

documentReallyDeletedForUser :: Document -> UserID -> Bool
documentReallyDeletedForUser doc uid = fromMaybe False (fmap (isJust . signatorylinkreallydeleted) $ (getSigLinkFor doc uid `mplus` getAuthorSigLink doc))

userCanPerformSigningAction :: UserID -> Document  -> Bool
userCanPerformSigningAction uid doc =
      (isJust msl && (canSignatorySignNow doc sl))
   || (isJust msl && isAuthor sl && any (canSignatorySignNow doc &&^ ((== PadDelivery) . signatorylinkdeliverymethod)) (documentsignatorylinks doc))
  where
    msl = getSigLinkFor doc uid
    sl  = fromJust msl
