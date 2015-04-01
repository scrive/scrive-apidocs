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
  , documentcurrentsignorder
  , documentprevioussignorder
  , isLastViewer
  , signatoryFieldsFromUser
  , isEligibleForReminder
  , canAuthorSignNow
  , canSignatorySignNow
  , isActivatedSignatory
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

import Control.Monad.Catch
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
import KontraPrelude
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
         F.value "last" $ $last list
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
         F.value "last" $ $last list
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


class HasFields a where
    getAllFields:: a ->  [SignatoryField]

instance HasFields  [SignatoryField] where
    getAllFields = id

instance HasFields SignatoryLink where
    getAllFields =  signatoryfields

-- OTHER UTILS

-- | The document's current sign order is either the sign order of the
-- next partner(s) that should sign; or in case all partners have
-- signed already: the largest sign order among all signatories.
documentcurrentsignorder :: Document -> SignOrder
documentcurrentsignorder doc =
    case filter notSignedPartner sigs of
         [] -> $maximum $ map signatorysignorder sigs
         xs -> $minimum $ map signatorysignorder xs
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
         xs -> $maximum $ map signatorysignorder xs

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
        [ SignatoryNameField $ NameField {
              snfID                     = (unsafeSignatoryFieldID 0)
            , snfNameOrder              = NameOrder 1
            , snfValue                  = getFirstName user
            , snfObligatory             = True
            , snfShouldBeFilledBySender = True
            , snfPlacements             = []
          }
        , SignatoryNameField $ NameField {
              snfID                     = (unsafeSignatoryFieldID 0)
            , snfNameOrder              = NameOrder 2
            , snfValue                  = getLastName user
            , snfObligatory             = True
            , snfShouldBeFilledBySender = True
            , snfPlacements             = []
          }
        , SignatoryEmailField $ EmailField {
              sefID                     = (unsafeSignatoryFieldID 0)
            , sefValue                  = getEmail user
            , sefObligatory             = True
            , sefShouldBeFilledBySender = True
            , sefPlacements             = []
          }
         , SignatoryCompanyField $ CompanyField {
              scfID                     = (unsafeSignatoryFieldID 0)
            , scfValue                  = getCompanyName company
            , scfObligatory             = False
            , scfShouldBeFilledBySender = False
            , scfPlacements             = []
          }
        ] ++
        (if (not $ null $ getPersonalNumber user)
            then [
              SignatoryPersonalNumberField $ PersonalNumberField {
                  spnfID                     = (unsafeSignatoryFieldID 0)
                , spnfValue                  = getPersonalNumber user
                , spnfObligatory             = False
                , spnfShouldBeFilledBySender = False
                , spnfPlacements             = []
              }
             ]
            else [])
          ++
        (if (not $ null $ getMobile user)
            then [
              SignatoryMobileField $ MobileField {
                  smfID                     = (unsafeSignatoryFieldID 0)
                , smfValue                  = getMobile user
                , smfObligatory             = False
                , smfShouldBeFilledBySender = False
                , smfPlacements             = []
              }
             ]
            else []
        )
          ++
        (if (not $ null $ getCompanyNumber company)
           then [
              SignatoryCompanyNumberField $ CompanyNumberField {
                  scnfID                     = (unsafeSignatoryFieldID 0)
                , scnfValue                  = getCompanyNumber company
                , scnfObligatory             = False
                , scnfShouldBeFilledBySender = False
                , scnfPlacements             = []
              }
             ]
           else []
        )

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
    sl  = $fromJust msl
