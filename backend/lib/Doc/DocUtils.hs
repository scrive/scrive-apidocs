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

module Doc.DocUtils (
    renderLocalListTemplate
  , mkAuthKind
  , documentcurrentsignorder
  , documentprevioussignorder
  , isLastViewer
  , signatoryFieldsFromUser
  , isEligibleForReminder
  , canAuthorSignNow
  , canSignatorySignNow
  , isActivatedSignatory
  , getSignatoryAttachment
  , documentDeletedForUser
  , documentReallyDeletedForUser
  , userCanPerformSigningAction
  , fileFromMainFile
  , allRequiredAttachmentsAreOnList
  , detailsOfGroupedPortalSignatoriesThatCanSignNow
) where

import Control.Monad.Catch
import Text.StringTemplates.Templates
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import DB
import Doc.DocInfo
import Doc.DocStateData
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import File.File
import File.Model
import Templates
import User.Email
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

renderLocalListTemplate :: (HasLang a, TemplatesMonad m) => a -> [Text] -> m Text
renderLocalListTemplate = renderListTemplateHelper . renderLocalTemplate

renderListTemplateHelper
  :: TemplatesMonad m => (Text -> Fields m () -> m Text) -> [Text] -> m Text
renderListTemplateHelper renderFunc list = if length list > 1
  then renderFunc "morethenonelist" $ do
    F.value "list" $ init list
    F.value "last" $ last list
  else renderFunc "nomorethanonelist" $ F.value "list" list

-- OTHER UTILS

-- | Construct AuthenticationKind from a Document. Document must be either in
-- Pending or Closed state.
mkAuthKind :: Document -> AuthenticationKind
mkAuthKind doc = case documentstatus doc of
  Pending -> AuthenticationToView
  Closed  -> AuthenticationToViewArchived
  status  -> unexpectedError $ "invalid status: " <> (showt status)

-- | The document's current sign order is either the sign order of the
-- next partner(s) that should sign; or in case all partners have
-- signed already: the largest sign order among all signatories.
documentcurrentsignorder :: Document -> SignOrder
documentcurrentsignorder doc = case filter notSignedOrApproved sigs of
  [] -> maximum $ map signatorysignorder sigs
  xs -> minimum $ map signatorysignorder xs
  where
    sigs = documentsignatorylinks doc
    notSignedOrApproved siglnk =
      -- we exclude viewers
      (isSignatoryAndHasNotSigned || isApproverAndHasNotApproved) siglnk

-- | The document's previous sign order is either the sign order of
-- the last partner(s) that signed, or zero in case nobody has signed
-- yet.
documentprevioussignorder :: Document -> SignOrder
documentprevioussignorder doc =
  case
      filter (isSignatoryAndHasSigned || isApproverAndHasApproved)
             (documentsignatorylinks doc)
    of
      [] -> SignOrder (-1) -- Allow for signorder 0 (default value of
                           -- an integer when JSON serializing and not
                           -- setting a value)
      xs -> maximum $ map signatorysignorder xs

-- | True if the given signatory is a viewer and all other parties
-- (regardless of type) have smaller signorders.
isLastViewer :: Document -> SignatoryLink -> Bool
isLastViewer doc sl = isViewer sl && all
  (\s -> isViewer s || signatorysignorder s < signatorysignorder sl)
  (documentsignatorylinks doc)

{- |
   Build signatory fields from user
 -}
signatoryFieldsFromUser :: (MonadDB m, MonadThrow m) => User -> m [SignatoryField]
signatoryFieldsFromUser user = do
  ugwp <- dbQuery . UserGroupGetWithParentsByUserID . userid $ user
  return
    $  [ SignatoryNameField $ NameField { snfID = (unsafeSignatoryFieldID 0)
                                        , snfNameOrder              = NameOrder 1
                                        , snfValue                  = getFirstName user
                                        , snfObligatory             = True
                                        , snfShouldBeFilledBySender = True
                                        , snfPlacements             = []
                                        }
       , SignatoryNameField $ NameField { snfID = (unsafeSignatoryFieldID 0)
                                        , snfNameOrder              = NameOrder 2
                                        , snfValue                  = getLastName user
                                        , snfObligatory             = True
                                        , snfShouldBeFilledBySender = True
                                        , snfPlacements             = []
                                        }
       , SignatoryEmailField $ EmailField { sefID = (unsafeSignatoryFieldID 0)
                                          , sefValue                  = getEmail user
                                          , sefObligatory             = True
                                          , sefShouldBeFilledBySender = True
                                          , sefEditableBySignatory    = False
                                          , sefPlacements             = []
                                          }
       , SignatoryCompanyField $ CompanyField { scfID = (unsafeSignatoryFieldID 0)
                                              , scfValue = getUGEntityName ugwp
                                              , scfObligatory = False
                                              , scfShouldBeFilledBySender = False
                                              , scfPlacements = []
                                              }
       ]
    ++ (if (not $ T.null $ getPersonalNumber user)
         then
           [ SignatoryPersonalNumberField $ PersonalNumberField
               { spnfID                     = (unsafeSignatoryFieldID 0)
               , spnfValue                  = getPersonalNumber user
               , spnfObligatory             = False
               , spnfShouldBeFilledBySender = False
               , spnfPlacements             = []
               }
           ]
         else []
       )
    ++ (if (not $ T.null $ getMobile user)
         then
           [ SignatoryMobileField $ MobileField { smfID = (unsafeSignatoryFieldID 0)
                                                , smfValue = getMobile user
                                                , smfObligatory = False
                                                , smfShouldBeFilledBySender = False
                                                , smfEditableBySignatory = False
                                                , smfPlacements = []
                                                }
           ]
         else []
       )
    ++ (if (not . T.null . getUGCompanyNumber $ ugwp)
         then
           [ SignatoryCompanyNumberField $ CompanyNumberField
               { scnfID                     = (unsafeSignatoryFieldID 0)
               , scnfValue                  = getUGCompanyNumber ugwp
               , scnfObligatory             = False
               , scnfShouldBeFilledBySender = False
               , scnfPlacements             = []
               }
           ]
         else []
       )
  where
    getUGEntityName    = ugaEntityName . ugwpAddress
    getUGCompanyNumber = ugaCompanyNumber . ugwpAddress

{- |
    Checks whether a signatory link is eligible for sending a reminder.
    Signatory must be next in the signorder, and also not be a viewer.
    In addition the document must be in the correct state.  There's quite a lot to check!
-}
isEligibleForReminder :: Document -> SignatoryLink -> Bool
isEligibleForReminder document@Document { documentstatus } siglink =
  documentDeliverableTosignatory
    && signatoryActivated
    && not dontShowAnyReminder
    && (  signatorylinkdeliverymethod siglink
       /= EmailDelivery
       || (  mailinvitationdeliverystatus siglink
          /= Undelivered
          && mailinvitationdeliverystatus siglink
          /= Deferred
          )
       )
    && (  signatorylinkdeliverymethod siglink
       /= MobileDelivery
       || (  smsinvitationdeliverystatus siglink
          /= Undelivered
          && smsinvitationdeliverystatus siglink
          /= Deferred
          )
       )
    && (  signatorylinkdeliverymethod siglink
       /= EmailAndMobileDelivery
       || (  mailinvitationdeliverystatus siglink
          /= Undelivered
          && mailinvitationdeliverystatus siglink
          /= Deferred
          )
       || (  smsinvitationdeliverystatus siglink
          /= Undelivered
          && smsinvitationdeliverystatus siglink
          /= Deferred
          )
       )
    && ((isSignatoryAndHasNotSigned || isApproverAndHasNotApproved) siglink)
  where
    signatoryActivated  = isActivatedSignatory (documentcurrentsignorder document) siglink
    dontShowAnyReminder = documentstatus `elem` [Timedout, Canceled, Rejected]
    documentDeliverableTosignatory =
      signatorylinkdeliverymethod siglink
        `elem` [EmailDelivery, MobileDelivery, EmailAndMobileDelivery]

-- | Can author sign now according to sign order?
canAuthorSignNow :: Document -> Bool
canAuthorSignNow doc =
  isPending doc
    && documentcurrentsignorder doc
    >= signatorysignorder author
    && isSignatoryAndHasNotSigned author
  where
    author = case getAuthorSigLink doc of
      Just a -> a
      _ ->
        unexpectedError $ "document " <> showt (documentid doc) <> " does not have author"


-- Checks if signatory can sign now
canSignatorySignNow :: Document -> SignatoryLink -> Bool
canSignatorySignNow doc sl =
  isPending doc
    && documentcurrentsignorder doc
    >= signatorysignorder sl
    && isSignatoryAndHasNotSigned sl

{- |
   Has the signatory's sign order come up?
 -}
isActivatedSignatory :: SignOrder -> SignatoryLink -> Bool
isActivatedSignatory signorder siglink = signorder >= signatorysignorder siglink

getSignatoryAttachment :: SignatoryLinkID -> Text -> Document -> Maybe SignatoryAttachment
getSignatoryAttachment slid name doc =
  join
    $   find (\a -> name == signatoryattachmentname a)
    <$> signatoryattachments
    <$> getSigLinkFor slid doc

-- Changes MainFile into file. Works on maybe since this is main case in our system
fileFromMainFile :: (MonadDB m, MonadThrow m) => Maybe MainFile -> m (Maybe File)
fileFromMainFile mmf = case mmf of
  Nothing -> return Nothing
  Just mf -> Just <$> (dbQuery $ GetFileByFileID $ mainfileid mf)

documentDeletedForUser :: Document -> UserID -> Bool
documentDeletedForUser doc uid = fromMaybe
  False
  ( fmap (isJust . signatorylinkdeleted)
  $ (getSigLinkFor uid doc `mplus` getAuthorSigLink doc)
  )

documentReallyDeletedForUser :: Document -> UserID -> Bool
documentReallyDeletedForUser doc uid = fromMaybe
  False
  ( fmap (isJust . signatorylinkreallydeleted)
  $ (getSigLinkFor uid doc `mplus` getAuthorSigLink doc)
  )

userCanPerformSigningAction :: UserID -> Document -> Bool
userCanPerformSigningAction uid doc =
  (isJust msl && (canSignatorySignNow doc sl))
    || (isJust msl && isAuthor sl && any
         (canSignatorySignNow doc && ((== PadDelivery) . signatorylinkdeliverymethod))
         (documentsignatorylinks doc)
       )
  where
    msl = getSigLinkFor uid doc
    sl  = fromJust msl

allRequiredAttachmentsAreOnList :: [FileID] -> Document -> Bool
allRequiredAttachmentsAreOnList acceptedAttachments doc =
  all (\i -> i `elem` acceptedAttachments)
    $ map authorattachmentfileid
    $ requiredAuthorAttachments doc

requiredAuthorAttachments :: Document -> [AuthorAttachment]
requiredAuthorAttachments doc =
  filter authorattachmentrequired $ documentauthorattachments doc

detailsOfGroupedPortalSignatoriesThatCanSignNow :: [Document] -> [(Email, Text)]
detailsOfGroupedPortalSignatoriesThatCanSignNow docs =
  nubBy (\(e1, _) (e2, _) -> e1 == e2)
    $ (concatMap detailsOfPortalSignatoriesThatCanSignNow docs)

detailsOfPortalSignatoriesThatCanSignNow :: Document -> [(Email, Text)]
detailsOfPortalSignatoriesThatCanSignNow doc = map slToData $ portalSigs
  where
    portalSig sl =
      signatorylinkdeliverymethod sl == PortalDelivery && canSignatorySignNow doc sl
    portalSigs = filter portalSig $ documentsignatorylinks doc
    slToData sl = (Email $ getEmail sl, getFullName sl)
