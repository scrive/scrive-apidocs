module Doc.API.V2.Guards (
      guardThatDocument
    , guardDocumentStatus
    , guardThatUserIsAuthor
    , guardThatUserIsAuthorOrCompanyAdmin
    , guardThatUserIsAuthorOrDocumentIsShared
    , guardThatDocumentCanBeStarted
    , guardThatObjectVersionMatchesIfProvided
  ) where

import KontraPrelude
import Control.Conditional (unlessM, whenM)
import Data.Text (Text, pack, append)
import Doc.DocumentID

import Doc.DocStateData
import API.Monad.V2
import Kontra
import Doc.DocumentMonad
import DB
import User.Model
import Util.SignatoryLinkUtils
import Control.Exception.Lifted
import Doc.DocUtils
import Doc.DocInfo
import InputValidation
import Util.HasSomeUserInfo
import Doc.Model.Query
import Doc.API.V2.Parameters

guardThatDocument :: (DocumentMonad m, Kontrakcja m) => (Document -> Bool) -> Text -> m ()
guardThatDocument f text = unlessM (f <$> theDocument) $ throwIO . SomeKontraException $ documentStateError text

guardDocumentStatus :: (Kontrakcja m, DocumentMonad m) => DocumentStatus -> m ()
guardDocumentStatus s = unlessM ((\d -> documentstatus d == s) <$> theDocument) $ throwIO . SomeKontraException $ documentStateError errorMsg
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
    throwIO . SomeKontraException $ documentActionForbidden

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
    || (isDocumentShared doc && usercompany user == usercompany a)
    )

guardThatObjectVersionMatchesIfProvided :: Kontrakcja m => DocumentID -> m ()
guardThatObjectVersionMatchesIfProvided did = do
  reqObjectVersion <- apiV2Parameter (ApiV2ParameterInt "object_version" Optional)
  case reqObjectVersion of
    Nothing -> return ()
    Just ov -> dbQuery $ CheckDocumentObjectVersionIs did (fromIntegral ov)

-- Checks if document can be strated. Throws matching API exception if it does not
guardThatDocumentCanBeStarted :: (DocumentMonad m, Kontrakcja m) => m ()
guardThatDocumentCanBeStarted = do
    whenM (isTemplate <$> theDocument) $ do
       throwIO . SomeKontraException $ (documentStateError "Document is a template, templates can not be started")
    unlessM (((all signatoryHasValidDeliverySettings) . documentsignatorylinks) <$> theDocument) $ do
       throwIO . SomeKontraException $ documentStateError "Some signatories have invalid email address or phone number, and it is required for invitation delivery."
    whenM (isNothing . documentfile <$> theDocument) $ do
       throwIO . SomeKontraException $ documentStateError "Document must have a file before it can be started"
    return ()
 where
    signatoryHasValidDeliverySettings sl = (isAuthor sl) || case (signatorylinkdeliverymethod sl) of
      EmailDelivery  ->  isGood $ asValidEmail $ getEmail sl
      MobileDelivery ->  isGood $ asValidPhoneForSMS $ getMobile sl
      EmailAndMobileDelivery -> (isGood $ asValidPhoneForSMS $ getMobile sl) && (isGood $ asValidEmail $ getEmail sl)
      _ -> True
