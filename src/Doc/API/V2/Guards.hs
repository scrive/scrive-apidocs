module Doc.API.V2.Guards (
  guardThatDocument
, guardDocumentStatus
, guardThatUserIsAuthor
, guardThatUserIsAuthorOrCompanyAdmin
, guardThatUserIsAuthorOrDocumentIsShared
, guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared
, guardThatDocumentCanBeStarted
, guardThatObjectVersionMatchesIfProvided
) where

import Control.Conditional (unlessM, whenM)
import Data.Text (Text, pack, append)

import API.V2
import DB
import Doc.API.V2.Parameters
import Doc.Conditions
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model.Query
import InputValidation
import Kontra
import KontraPrelude
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

guardThatDocument :: (DocumentMonad m, Kontrakcja m) => (Document -> Bool) -> Text -> m ()
guardThatDocument f text = unlessM (f <$> theDocument) $ apiError $ documentStateError text

guardDocumentStatus :: (Kontrakcja m, DocumentMonad m) => DocumentStatus -> m ()
guardDocumentStatus s = unlessM ((\d -> documentstatus d == s) <$> theDocument) $ apiError $ documentStateError errorMsg
  where errorMsg = "The document status should be " `append` (pack $ show s)

-- | Internal function used in all guards on User
-- Helps code reuse and keep error messages consistent
guardDocumentAuthorIs :: (Kontrakcja m, DocumentMonad m) => (User -> Bool) -> m ()
guardDocumentAuthorIs condition = do
  let msgNoAuthor = "Document doesn't have author signatory link connected with user account"
  authorUserId <- apiGuardJustM (serverError msgNoAuthor) $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
  let msgNoUser = "Document doesn't have author user account for the author signatory link"
  author <- apiGuardJustM (serverError msgNoUser) $ dbQuery $ GetUserByIDIncludeDeleted authorUserId
  when (not $ condition author) $ do
    apiError documentActionForbidden

guardThatUserIsAuthor :: (DocumentMonad m, Kontrakcja m) => User -> m ()
guardThatUserIsAuthor user = guardDocumentAuthorIs (\a -> userid user == userid a)

guardThatUserIsAuthorOrCompanyAdmin :: (Kontrakcja m, DocumentMonad m) => User -> m ()
guardThatUserIsAuthorOrCompanyAdmin user = guardDocumentAuthorIs
  (\a -> userid user == userid a
      || (usercompany user == usercompany a && useriscompanyadmin user)
  )

guardThatUserIsAuthorOrDocumentIsShared :: (Kontrakcja m, DocumentMonad m) => User -> m ()
guardThatUserIsAuthorOrDocumentIsShared user = do
  doc <- theDocument
  guardDocumentAuthorIs (\a -> userid user == userid a
    || (usercompany user == usercompany a && isDocumentShared doc)
    )

guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared :: (Kontrakcja m, DocumentMonad m) => User -> m ()
guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user = do
  doc <- theDocument
  guardDocumentAuthorIs (\a -> userid user == userid a
    || (usercompany user == usercompany a && (useriscompanyadmin user || isDocumentShared doc))
    )

guardThatObjectVersionMatchesIfProvided :: Kontrakcja m => DocumentID -> m ()
guardThatObjectVersionMatchesIfProvided did = do
  reqObjectVersion <- apiV2ParameterOptional (ApiV2ParameterInt "object_version")
  case reqObjectVersion of
    Nothing -> return ()
    Just ov -> (dbQuery $ CheckDocumentObjectVersionIs did (fromIntegral ov))
      `catchKontra` (\e@DocumentObjectVersionDoesNotMatch {} -> apiError $ documentObjectVersionMismatch e)

-- Checks if document can be strated. Throws matching API exception if it does not
guardThatDocumentCanBeStarted :: (DocumentMonad m, Kontrakcja m) => m ()
guardThatDocumentCanBeStarted = do
    whenM (isTemplate <$> theDocument) $ do
       apiError $ (documentStateError "Document is a template, templates can not be started")
    unlessM (((all signatoryHasValidDeliverySettings) . documentsignatorylinks) <$> theDocument) $ do
       apiError $ documentStateError "Some signatories have invalid email address or phone number, and it is required for invitation delivery."
    whenM (isNothing . documentfile <$> theDocument) $ do
       apiError $ documentStateError "Document must have a file before it can be started"
    return ()
 where
    signatoryHasValidDeliverySettings sl = (isAuthor sl) || case (signatorylinkdeliverymethod sl) of
      EmailDelivery  ->  isGood $ asValidEmail $ getEmail sl
      MobileDelivery ->  isGood $ asValidPhoneForSMS $ getMobile sl
      EmailAndMobileDelivery -> (isGood $ asValidPhoneForSMS $ getMobile sl) && (isGood $ asValidEmail $ getEmail sl)
      _ -> True
