-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocStateQuery
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- // Description is not right and this will be refactored as rest of the code.
-- Each exported function is a middle-man between the Controller and the Model.
-- It handles HTTP-related security, including checking whether a user is logged
-- in.
--
-- They provide these extra features over the Model:
--  * Access control
--  * Aggregation
--  * Filtering
--
-- This module only provides queries.
--
-- This module should control access centrally (instead of inside each Controller)
-- This module should aggregate results from multiple queries (such as a join)
-- This module should filter results from queries (as necessary)
--
-- Please put failures first.
-----------------------------------------------------------------------------

module Doc.DocStateQuery
    ( getDocumentByCurrentUser
    , getDocByDocIDAndAccessTokenV1
    , getDocByDocIDAndAccessTokenV2
    , getDocByDocIDForAuthor
    , getDocByDocIDForAuthorOrAuthorsCompanyAdmin
    , checkIfUserCanAccessDocumentAsSignatory
    , signatoryNeedsToIdentifyToView
    , getDocumentAndSignatoryForEIDAuth
    , getDocumentAndSignatoryForEIDSign
    ) where

import Control.Monad.Catch (throwM)
import Log

import AccessControl.Check
import AccessControl.Model
import AccessControl.Types
import API.V2.Errors
import API.V2.Utils
import DB
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EID.Authentication.Model
import Kontra
import MagicHash
import User.Model
import Util.MonadUtils
import Util.SignatoryLinkUtils

-- TODO: clean this up, try combining it with DocumentMonad.

getDocumentByCurrentUser :: Kontrakcja m => DocumentID -> m Document
getDocumentByCurrentUser docId = do
  user  <- guardJust . contextUser =<< getContext
  roles <- dbQuery $ GetRoles user
  let availablePerm = concatMap (hasPermissions . accessRoleTarget) roles
  withDocumentID docId $ do
    mSL          <- getSigLinkFor user <$> theDocument
    resources    <- docResources <$> theDocument
    requiredPerm <- apiRequireAnyPermission [ canDo ReadA res | res <- resources ]
    let hasReadPermission = accessControlCheck availablePerm requiredPerm
    if isJust mSL || hasReadPermission
      then theDocument
      else throwM . SomeDBExtraException $ insufficientPrivileges

getDocByDocIDAndAccessTokenV1
  :: Kontrakcja m => DocumentID -> Maybe MagicHash -> m Document
getDocByDocIDAndAccessTokenV1 docId mToken = do
  mDoc <- case mToken of
    Just accessToken -> getMaybeDocumentByAccessToken docId accessToken
    Nothing          -> return Nothing
  case mDoc of
    Just doc -> return doc
    Nothing  -> getDocumentByCurrentUser docId

getDocByDocIDAndAccessTokenV2 :: Kontrakcja m => DocumentID -> MagicHash -> m Document
getDocByDocIDAndAccessTokenV2 = getDocumentByAccessToken

getMaybeDocumentByAccessToken
  :: Kontrakcja m => DocumentID -> MagicHash -> m (Maybe Document)
getMaybeDocumentByAccessToken docId accessToken = do
  docs <- dbQuery $ GetDocuments (DocumentsVisibleViaAccessToken accessToken)
                                 [DocumentFilterByDocumentID docId]
                                 []
                                 1
  return $ listToMaybe docs

getDocumentByAccessToken :: Kontrakcja m => DocumentID -> MagicHash -> m Document
getDocumentByAccessToken docId accessToken = do
  mDoc <- getMaybeDocumentByAccessToken docId accessToken
  case mDoc of
    Just doc -> return doc
    Nothing  -> do
      throwM . SomeDBExtraException $ invalidAuthorizationWithMsg
        "Access token was invalid."

-- | Same as getDocByDocID, but works only for author
getDocByDocIDForAuthor :: Kontrakcja m => DocumentID -> m Document
getDocByDocIDForAuthor did = do
  ctx <- getContext
  case view #id <$> contextUser ctx of
    Nothing -> unexpectedError
      "Impossible happened (user being logged in is guarded up in the call stack)"
    Just uid -> dbQuery $ GetDocument
      (DocumentsVisibleToUser uid)
      [DocumentFilterByDocumentID did, DocumentFilterByAuthor uid]

-- | Same as getDocByDocID, but works only for author or authors company admin
getDocByDocIDForAuthorOrAuthorsCompanyAdmin :: Kontrakcja m => DocumentID -> m Document
getDocByDocIDForAuthorOrAuthorsCompanyAdmin did = do
  ctx <- getContext
  case view #id <$> contextUser ctx of
    Nothing -> unexpectedError
      "Impossible happened (user being logged in is guarded up in the call stack)"
    Just uid -> dbQuery $ GetDocument
      (DocumentsVisibleToUser uid)
      [DocumentFilterByDocumentID did, DocumentFilterLinkIsAuthor True]

-- | Check that the user has access to the document.
checkIfUserCanAccessDocumentAsSignatory
  :: Kontrakcja m => User -> DocumentID -> SignatoryLinkID -> m Bool
checkIfUserCanAccessDocumentAsSignatory user did sigid = do
  mdoc <- dbQuery $ GetDocuments (DocumentsVisibleToUser $ user ^. #id)
                                 [DocumentFilterByDocumentID did]
                                 []
                                 1
  return $ case mdoc of
    [doc] -> case getMaybeSignatoryLink (doc, sigid) of
      Just sig ->
        (isAuthor (doc, user) && signatorylinkdeliverymethod sig == PadDelivery)
          || isSigLinkFor user sig
      Nothing -> False
    _ -> False

signatoryNeedsToIdentifyToView :: (Kontrakcja m) => SignatoryLink -> Document -> m Bool
signatoryNeedsToIdentifyToView sl doc = if isClosed doc
  then check AuthenticationToViewArchived signatorylinkauthenticationtoviewarchivedmethod
  else check AuthenticationToView signatorylinkauthenticationtoviewmethod
  where
    check authKind signatoryAuthMethod = case signatoryAuthMethod sl of
      StandardAuthenticationToView -> return False
      authtoview                   -> do
        sid       <- view #sessionID <$> getContext
        mauthindb <- dbQuery (GetEAuthentication authKind sid $ signatorylinkid sl)
        return $ maybe True (not . authViewMatchesAuth authtoview) mauthindb

-- | Fetch the document and signatory for e-auth. If there is no session token
-- for the document, checks documents of a current user before giving up as
-- authentication can be requested from author's view.
getDocumentAndSignatoryForEIDAuth
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m (Document, SignatoryLink)
getDocumentAndSignatoryForEIDAuth did slid = do
  sid   <- view #sessionID <$> getContext
  check <- dbQuery $ CheckDocumentSession sid slid
  if check
    then do
      logInfo_ "Document session found"
      doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
      let sl = fromJust $ getSigLinkFor slid doc
      if isForwarded sl
        then do
          logInfo_ "Signatory link has forwarded role"
          respond404
        else return (doc, sl)
    else do
      -- Authentication to view archived in author's view case. There is no
      -- magic hash, just find the document in the archive.
      logInfo_ "No document session, checking logged user's documents"
      (contextUser <$> getContext) >>= \case
        Just user -> do
          doc <- dbQuery $ GetDocument (DocumentsVisibleToUser $ user ^. #id)
                                       [DocumentFilterByDocumentID did]
          let slink = fromJust $ getSigLinkFor slid doc
          if maybesignatory slink == Just (user ^. #id)
            then return (doc, slink)
            else do
              logInfo_ "Current user is not author of the document"
              respond404
        Nothing -> do
          logInfo_ "No user found"
          respond404

-- | Fetch the document and signatory for e-signing. Checks that the document is
-- in the correct state and the signatory hasn't signed yet. Requires session
-- token to be set.
getDocumentAndSignatoryForEIDSign
  :: Kontrakcja m => SignatoryLinkID -> m (Document, SignatoryLink)
getDocumentAndSignatoryForEIDSign slid = do
  sid   <- view #sessionID <$> getContext
  check <- dbQuery $ CheckDocumentSession sid slid
  if check
    then do
      logInfo_ "Document session found"
      doc <- dbQuery $ GetDocumentBySignatoryLinkID slid
      -- This should always succeed as we already got the document.
      let slink = fromJust $ getSigLinkFor slid doc
      when (isSignatoryAndHasSigned slink) $ do
        logInfo_ "Signatory already signed the document"
        respond404
      -- Check that signatory uses EID for signing.
      let am = signatorylinkauthenticationtosignmethod slink
      when (am == StandardAuthenticationToSign || am == SMSPinAuthenticationToSign) $ do
        logInfo "Signatory has incorrect authentication to sign method"
          $ object ["method" .= show am]
      return (doc, slink)
    else do
      logInfo_ "No document token found"
      respond404
