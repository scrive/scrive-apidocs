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
    , getDocByDocIDEx
    , getDocByDocIDForAuthor
    , getDocByDocIDForAuthorOrAuthorsCompanyAdmin
    , getMagicHashForDocumentSignatoryWithUser
    , signatoryNeedsToIdentifyToView
    , getDocumentAndSignatoryForEID
    ) where

import Control.Monad.Base
import Control.Monad.Catch
import Log

import DB
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad (DocumentMonad, theDocument)
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EID.Authentication.Model
import Kontra
import KontraPrelude
import MagicHash
import User.Model
import Util.SignatoryLinkUtils

getDocByDocID :: Kontrakcja m => DocumentID -> m Document
getDocByDocID docid = getDocByDocIDEx docid Nothing

getDocByDocIDEx :: Kontrakcja m => DocumentID -> Maybe MagicHash -> m Document
getDocByDocIDEx docid maccesstoken = do
  ctx <- getContext
  -- document will be returned if ANY of the below is true (logical OR)
  let visibility = catMaybes [
          DocumentsVisibleToUser . userid <$> getContextUser ctx
        , DocumentsVisibleViaAccessToken  <$> maccesstoken
        ]
  dbQuery $ GetDocument
    visibility
    [DocumentFilterByDocumentID docid]

-- | Same as getDocByDocID, but works only for author
getDocByDocIDForAuthor :: Kontrakcja m => DocumentID -> m Document
getDocByDocIDForAuthor docid = do
  ctx <- getContext
  case getContextUser ctx of
    Nothing -> do
      -- we should never come to this place as user being loggen in is
      -- guarded up in the call stack
      internalError
    Just User{userid} -> dbQuery $ GetDocument
      [DocumentsVisibleToUser userid]
      [DocumentFilterByDocumentID docid, DocumentFilterByAuthor userid]

-- | Same as getDocByDocID, but works only for author or authors company admin
getDocByDocIDForAuthorOrAuthorsCompanyAdmin :: Kontrakcja m => DocumentID -> m Document
getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid = do
  ctx <- getContext
  case getContextUser ctx of
    Nothing -> do
      -- we should never come to this place as user being loggen in is
      -- guarded up in the call stack
      internalError
    Just User{userid} -> dbQuery $ GetDocument
      [DocumentsVisibleToUser userid]
      [DocumentFilterByDocumentID docid, DocumentFilterLinkIsAuthor True]

-- | Get a magichash for given signatory. Only possible if given user is author.
getMagicHashForDocumentSignatoryWithUser :: Kontrakcja m
                                   => DocumentID
                                   -> SignatoryLinkID
                                   -> User
                                   -> m (Maybe MagicHash)
getMagicHashForDocumentSignatoryWithUser did sigid user = do
   mdoc <- dbQuery (GetDocuments [ DocumentsVisibleToUser (userid user)
                                    ] [ DocumentFilterByDocumentID did
                                      ]
                                    [] (0,1))
   case mdoc of
     [doc] ->  case getMaybeSignatoryLink (doc,sigid) of
                 Just sig -> if ((isAuthor (doc,user) && signatorylinkdeliverymethod  sig == PadDelivery) || (isSigLinkFor user sig))
                                then return $ Just $ signatorymagichash sig
                                else return Nothing
                 Nothing -> return Nothing
     _ -> return Nothing


signatoryNeedsToIdentifyToView :: (Kontrakcja m , DocumentMonad m) => SignatoryLink -> m Bool
signatoryNeedsToIdentifyToView sl = do
  doc <- theDocument
  if (hasSigned sl || (not $ isPending doc))
  then return False
  else case (signatorylinkauthenticationtoviewmethod sl) of
   StandardAuthenticationToView -> return False
   NOBankIDAuthenticationToView -> do
    sid <- ctxsessionid <$> getContext
    auth <- dbQuery (GetEAuthentication sid $ signatorylinkid sl)
    case auth of
      Just (NetsNOBankIDAuthentication_ _) -> return False
      _ -> return True
   SEBankIDAuthenticationToView -> do
    sid <- ctxsessionid <$> getContext
    auth <- dbQuery (GetEAuthentication sid $ signatorylinkid sl)
    case auth of
      Just (CGISEBankIDAuthentication_ _) -> return False
      _ -> return True

-- | Fetch the document and signatory for e-signing or e-auth. Checks that the document
-- is in the correct state and the signatory hasn't signed yet. Requires session token to be set
getDocumentAndSignatoryForEID :: (MonadDB m, MonadLog m, KontraMonad m, MonadThrow m,MonadBase IO m)
            => DocumentID -> SignatoryLinkID -> m (Document,SignatoryLink)
getDocumentAndSignatoryForEID did slid = dbQuery (GetDocumentSessionToken slid) >>= \case
  Just mh -> do
    logInfo_ "Document token found"
    doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
    when (documentstatus doc /= Pending) $ do
      logInfo "Unexpected status of the document" $ object [
          "expected" .= show Pending
        , "given" .= show (documentstatus doc)
        ]
      respond404
    -- this should always succeed as we already got the document
    let slink = $fromJust $ getSigLinkFor slid doc
    when (hasSigned slink) $ do
      logInfo_ "Signatory already signed the document"
      respond404
    return (doc,slink)
  Nothing -> do
    logInfo_ "No document token found"
    respond404
