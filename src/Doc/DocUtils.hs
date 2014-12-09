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
  , documentprevioussignorder
  , isLastViewer
  , signatoryFieldsFromUser
  , isEligibleForReminder
  , canAuthorSignNow
  , canSignatorySignNow
  , isActivatedSignatory
  , isFieldCustom
  , findCustomField
  , getSignatoryAttachment
  , documentfileM
  , documentsealedfileM
  , documentDeletedForUser
  , documentReallyDeletedForUser
  , userCanPerformSigningAction
  , MaybeTemplate(..)
  , HasFields(..)
  , MaybeUser(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Data.List hiding (insert)
import Data.Maybe
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import Company.Model
import Control.Logic
import DB
import Doc.DocInfo
import Doc.DocStateData
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import File.File
import File.Model
import Templates
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

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

replaceFieldValue :: HasFields a =>  FieldType -> SignatoryFieldValue -> a -> a
replaceFieldValue ft v a = case (find (matchingFieldType ft) $ getAllFields a) of
                            Just f  -> replaceField (f { sfType = ft, sfValue = v}) a
                            Nothing -> replaceField (SignatoryField { sfID = unsafeSignatoryFieldID 0, sfType = ft, sfValue = v, sfPlacements =[], sfObligatory = True, sfShouldBeFilledBySender = False}) a

-- OTHER UTILS

-- | The document's current sign order is either the sign order of the
-- next partner(s) that should sign; or in case all partners have
-- signed already: the largest sign order among all signatories.
documentcurrentsignorder :: Document -> SignOrder
documentcurrentsignorder doc =
    case filter notSignedPartner sigs of
         [] -> maximum $ map signatorysignorder sigs
         xs -> minimum $ map signatorysignorder xs
    where
        sigs = documentsignatorylinks doc
        notSignedPartner siglnk = isNothing (maybesigninfo siglnk)
          && signatoryispartner siglnk -- we exclude non-signatories

-- | The document's previous sign order is either the sign order of
-- the last partner(s) that signed, or zero in case nobody has signed
-- yet.
documentprevioussignorder :: Document -> SignOrder
documentprevioussignorder doc =
    case filter (isJust . maybesigninfo) (documentsignatorylinks doc) of
         [] -> SignOrder (-1) -- Allow for signorder 0 (default value of an integer when JSON serializing and not setting a value)
         xs -> maximum $ map signatorysignorder xs

-- | True if signatory is a viewer and all other document partners' signorder are smaller
isLastViewer :: Document -> SignatoryLink -> Bool
isLastViewer doc sl =
      not (signatoryispartner sl)
  && all (\s -> not (signatoryispartner s) || signatorysignorder s < signatorysignorder sl)
         (documentsignatorylinks doc)

{- |
   Build signatory fields from user
 -}
signatoryFieldsFromUser :: (MonadDB m, MonadThrow m) => User -> m [SignatoryField]
signatoryFieldsFromUser user = do
  company <- dbQuery $ GetCompanyByUserID (userid user)
  return $
        [ SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT (TextField $ getFirstName user) True True []
        , SignatoryField (unsafeSignatoryFieldID 0) LastNameFT (TextField $ getLastName user) True True []
        , SignatoryField (unsafeSignatoryFieldID 0) EmailFT (TextField $ getEmail user) True True []
        , SignatoryField (unsafeSignatoryFieldID 0) CompanyFT (TextField $ getCompanyName company) False False []
        ] ++
        (if (not $ null $ getPersonalNumber user) then [SignatoryField (unsafeSignatoryFieldID 0) PersonalNumberFT (TextField $ getPersonalNumber user) False False []] else [])
          ++
        (if (not $ null $ getMobile user) then [SignatoryField (unsafeSignatoryFieldID 0) MobileFT (TextField $ getMobile user) False False []] else [])
          ++
        (if (not $ null $ getCompanyNumber company) then [SignatoryField (unsafeSignatoryFieldID 0) CompanyNumberFT (TextField $ getCompanyNumber company) False False []] else [])

{- |
    Checks whether a signatory link is eligible for sending a reminder.
    Signatory must be next in the signorder, and also not be a viewer.
    In addition the document must be in the correct state.  There's quite a lot to check!
-}
isEligibleForReminder :: Document -> SignatoryLink -> Bool
isEligibleForReminder document@Document{documentstatus} siglink =
       documentDeliverableTosignatory
    && signatoryActivated
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
    wasNotSigned = isNothing (maybesigninfo siglink)
    signatoryActivated = isActivatedSignatory (documentcurrentsignorder document) siglink
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

isFieldCustom :: SignatoryField -> Bool
isFieldCustom SignatoryField{sfType = CustomFT{}} = True
isFieldCustom _ = False

findCustomField :: HasFields a => String -> a -> Maybe SignatoryField
findCustomField name = find (matchingFieldType (CustomFT name False)) . getAllFields

getSignatoryAttachment :: SignatoryLinkID -> String -> Document -> Maybe SignatoryAttachment
getSignatoryAttachment slid name doc = join $ find (\a -> name == signatoryattachmentname a)
                                       <$> signatoryattachments
                                       <$> (find (\sl -> slid == signatorylinkid sl) $ documentsignatorylinks doc)

documentfileM :: (MonadDB m, MonadThrow m) => Document -> m (Maybe File)
documentfileM = maybe (return Nothing) (fmap Just . dbQuery . GetFileByFileID) . documentfile

documentsealedfileM :: (MonadDB m, MonadThrow m) => Document -> m (Maybe File)
documentsealedfileM = maybe (return Nothing) (fmap Just . dbQuery . GetFileByFileID) . documentsealedfile


documentDeletedForUser :: Document -> UserID -> Bool
documentDeletedForUser doc uid = fromMaybe False (fmap (isJust . signatorylinkdeleted) $ (getSigLinkFor uid doc `mplus` getAuthorSigLink doc))

documentReallyDeletedForUser :: Document -> UserID -> Bool
documentReallyDeletedForUser doc uid = fromMaybe False (fmap (isJust . signatorylinkreallydeleted) $ (getSigLinkFor uid doc `mplus` getAuthorSigLink doc))

userCanPerformSigningAction :: UserID -> Document  -> Bool
userCanPerformSigningAction uid doc =
      (isJust msl && (canSignatorySignNow doc sl))
   || (isJust msl && isAuthor sl && any (canSignatorySignNow doc &&^ ((== PadDelivery) . signatorylinkdeliverymethod)) (documentsignatorylinks doc))
  where
    msl = getSigLinkFor uid doc
    sl  = fromJust msl
