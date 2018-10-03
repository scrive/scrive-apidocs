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
    ( getDocByDocID
    , getDocByDocIDAndAccessTokenV1
    , getDocByDocIDAndAccessTokenV2
    , getDocByDocIDForAuthor
    , getDocByDocIDForAuthorOrAuthorsCompanyAdmin
    , getMagicHashForDocumentSignatoryWithUser
    , signatoryNeedsToIdentifyToView
    , getDocumentAndSignatoryForEIDAuth
    , getDocumentAndSignatoryForEIDSign
    ) where

import Control.Monad.Catch (throwM)
import Log

import API.V2.Errors
import DB
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentID
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EID.Authentication.Model
import Kontra
import Log.Identifier
import MagicHash
import User.Model
import Util.MonadUtils
import Util.SignatoryLinkUtils

-- TODO: clean this up, try combining it with DocumentMonad.

getDocByDocID :: Kontrakcja m => DocumentID -> m Document
getDocByDocID docid = getDocByDocIDAndAccessTokenV1 docid Nothing

getDocByDocIDAndAccessTokenV1 :: Kontrakcja m
                              => DocumentID -> Maybe MagicHash -> m Document
getDocByDocIDAndAccessTokenV1 did mhash =
  getDocByDocIDAndAccessToken did mhash TryUserDocuments

getDocByDocIDAndAccessTokenV2 :: Kontrakcja m
                              => DocumentID -> MagicHash -> m Document
getDocByDocIDAndAccessTokenV2 did hash =
  getDocByDocIDAndAccessToken did (Just hash) DontTryUserDocuments


data TryUserDocuments = TryUserDocuments | DontTryUserDocuments
  deriving Eq

-- | Common implementation of
-- getDocByDocIDAndAccessToken{V1,V2}. TryUserDocuments case only
-- happens in V1.
getDocByDocIDAndAccessToken :: Kontrakcja m =>
  DocumentID -> Maybe MagicHash -> TryUserDocuments -> m Document
getDocByDocIDAndAccessToken did mhash tryuser = case mhash of
  Just accessToken -> do
    logInfo "Trying to get a document via access token" $ object [
        identifier did
      , "access_token" .= show accessToken
      ]
    mdoc <- dbQuery $ GetDocuments
      (DocumentsVisibleViaAccessToken accessToken)
      [DocumentFilterByDocumentID did]
      []
      1
    case mdoc of
      [doc] -> return doc
      _ | tryuser == TryUserDocuments -> do
            logInfo "Access token was invalid, trying user documents" $ object [
              identifier did
              ]
            getDocumentOfUser

        | otherwise -> do
            logInfo "Access token was invalid" $ object [ identifier did ]
            throwM . SomeDBExtraException $ invalidAuthorizationWithMsg
              "Access token was invalid."

  Nothing | tryuser == TryUserDocuments -> getDocumentOfUser
          | otherwise                   -> do
              logInfo "No access token provided" $ object [ identifier did ]
              throwM . SomeDBExtraException $ invalidAuthorizationWithMsg
                "No access token provided."
  where
    getDocumentOfUser = do
      user <- guardJust . getContextUser =<< getContext
      dbQuery $ GetDocument
        (DocumentsVisibleToUser $ userid user)
        [DocumentFilterByDocumentID did]

-- | Same as getDocByDocID, but works only for author
getDocByDocIDForAuthor :: Kontrakcja m => DocumentID -> m Document
getDocByDocIDForAuthor did = do
  ctx <- getContext
  case userid <$> getContextUser ctx of
    Nothing -> unexpectedError "Impossible happened (user being logged in is guarded up in the call stack)"
    Just uid -> dbQuery $ GetDocument
      (DocumentsVisibleToUser uid)
      [DocumentFilterByDocumentID did, DocumentFilterByAuthor uid]

-- | Same as getDocByDocID, but works only for author or authors company admin
getDocByDocIDForAuthorOrAuthorsCompanyAdmin
  :: Kontrakcja m
  => DocumentID
  -> m Document
getDocByDocIDForAuthorOrAuthorsCompanyAdmin did = do
  ctx <- getContext
  case userid <$> getContextUser ctx of
    Nothing -> unexpectedError "Impossible happened (user being logged in is guarded up in the call stack)"
    Just uid -> dbQuery $ GetDocument
      (DocumentsVisibleToUser uid)
      [DocumentFilterByDocumentID did, DocumentFilterLinkIsAuthor True]

-- | Get a magichash for given signatory. Only possible if given user is author.
getMagicHashForDocumentSignatoryWithUser
  :: Kontrakcja m
  => DocumentID
  -> SignatoryLinkID
  -> User
  -> m (Maybe MagicHash)
getMagicHashForDocumentSignatoryWithUser did sigid user = do
   mdoc <- dbQuery $ GetDocuments
           (DocumentsVisibleToUser $ userid user)
           [DocumentFilterByDocumentID did]
           []
           1
   case mdoc of
     [doc] ->  case getMaybeSignatoryLink (doc,sigid) of
       Just sig -> if ((isAuthor (doc, user) && signatorylinkdeliverymethod sig == PadDelivery) || (isSigLinkFor user sig))
                   then return $ Just $ signatorymagichash sig
                   else return Nothing
       Nothing -> return Nothing
     _ -> return Nothing

signatoryNeedsToIdentifyToView
  :: (Kontrakcja m)
  => SignatoryLink
  -> Document
  -> m Bool
signatoryNeedsToIdentifyToView sl doc =
  if isClosed doc
  then check AuthenticationToViewArchived signatorylinkauthenticationtoviewarchivedmethod
  else check AuthenticationToView          signatorylinkauthenticationtoviewmethod
  where
    check authKind signatoryAuthMethod = case signatoryAuthMethod sl of
      StandardAuthenticationToView -> return False
      authtoview -> do
        sid <- get ctxsessionid <$> getContext
        mauthindb <- dbQuery (GetEAuthentication authKind sid $ signatorylinkid sl)
        return $ case mauthindb of
          Nothing -> True
          Just authindb -> not $ authViewMatchesAuth authtoview authindb

-- | Fetch the document and signatory for e-auth. If there is no session token
-- for the document, checks documents of a current user before giving up as
-- authentication can be requested from author's view.
getDocumentAndSignatoryForEIDAuth
  :: Kontrakcja m
  => DocumentID
  -> SignatoryLinkID
  -> m (Document,SignatoryLink)
getDocumentAndSignatoryForEIDAuth did slid =
  dbQuery (GetDocumentSessionToken slid) >>= \case
    Just mh -> do
      logInfo_ "Document token found"
      doc <- dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh)
      return (doc, fromJust $ getSigLinkFor slid doc)
    Nothing -> do
      -- Authentication to view archived in author's view case. There is no
      -- magic hash, just find the document in the archive.
      logInfo_ "No document token, checking logged user's documents"
      (getContextUser <$> getContext) >>= \case
        Just user -> do
          doc <- dbQuery $ GetDocument (DocumentsVisibleToUser $ userid user)
                                       [DocumentFilterByDocumentID did]
          let slink = fromJust $ getSigLinkFor slid doc
          if maybesignatory slink == Just (userid user)
            then return (doc, slink)
            else do
              logInfo_ "Current user is not author of the document"
              respond404
        Nothing   -> do
          logInfo_ "No user found"
          respond404

-- | Fetch the document and signatory for e-signing. Checks that the document is
-- in the correct state and the signatory hasn't signed yet. Requires session
-- token to be set.
getDocumentAndSignatoryForEIDSign
  :: Kontrakcja m
  => DocumentID
  -> SignatoryLinkID
  -> m (Document,SignatoryLink)
getDocumentAndSignatoryForEIDSign did slid = dbQuery (GetDocumentSessionToken slid) >>= \case
  Just mh -> do
    logInfo_ "Document token found"
    doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
    -- This should always succeed as we already got the document.
    let slink = fromJust $ getSigLinkFor slid doc
    when (hasSigned slink) $ do
      logInfo_ "Signatory already signed the document"
      respond404
    -- Check that signatory uses EID for signing.
    let am = signatorylinkauthenticationtosignmethod slink
    when (am == StandardAuthenticationToSign || am == SMSPinAuthenticationToSign) $ do
      logInfo "Signatory has incorrect authentication to sign method" $
        object [ "method" .= show am ]
    return (doc, slink)
  Nothing -> do
    logInfo_ "No document token found"
    respond404
