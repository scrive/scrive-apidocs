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
    , getDocByDocIDForAuthor
    , getDocByDocIDForAuthorOrAuthorsCompanyAdmin
    , getDocByDocIDSigLinkIDAndMagicHash
    ) where

import Control.Monad
import DB
import DBError
import Doc.Model
import Doc.DocStateData
import Kontra
import MagicHash
import Util.SignatoryLinkUtils
import qualified Log
import User.Model
import Data.Maybe

{- |
   Securely find a document by documentid for the author or within their company.
   User must be logged in (otherwise Left DBNotLoggedIn).
   Document must exist (otherwise Left DBNotAvailable).
   Logged in user is author OR logged in user is in the company of the author (otherwise LeftDBNotAvailable).
 -}
getDocByDocID :: Kontrakcja m => DocumentID -> m (Either DBError Document)
getDocByDocID docid = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> return $ Left DBNotLoggedIn
    Just user -> do
      mdoc <- dbQuery (GetDocuments [ DocumentsForSignatoryDeleteValue (userid user) False
                                    , DocumentsOfAuthorDeleteValue (userid user) False
                                    ] [ DocumentFilterByDocumentID docid ]
                                    [] (DocumentPagination 0 1))
      case mdoc of
        [doc] -> do
          return $ Right doc
        _ -> do
          Log.debug $ "Document " ++ show docid ++ " does not exist (in getDocByDocID)"
          return $ Left DBResourceNotAvailable

{- | Same as getDocByDocID, but works only for author -}
getDocByDocIDForAuthor :: Kontrakcja m => DocumentID -> m (Either DBError Document)
getDocByDocIDForAuthor docid = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> return $ Left DBNotLoggedIn
    Just user -> do
      mdoc <- dbQuery (GetDocuments [ DocumentsOfAuthorDeleteValue (userid user) False
                                    ] [ DocumentFilterByDocumentID docid ]
                                    [] (DocumentPagination 0 1))
      case mdoc of
        [doc] -> do
          return $ Right doc
        _ -> do
          Log.debug $ "Document " ++ show docid ++ " does not exist (in getDocByDocIDForAuthor)"
          return $ Left DBResourceNotAvailable

{- | Same as getDocByDocID, but works only for author or authors company admin-}
getDocByDocIDForAuthorOrAuthorsCompanyAdmin :: Kontrakcja m => DocumentID -> m (Either DBError Document)
getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> return $ Left DBNotLoggedIn
    Just user -> do
      mdoc <- dbQuery (GetDocuments [ DocumentsOfAuthorDeleteValue (userid user) False
                                    , TemplatesSharedInUsersCompany (userid user)
                                    ] [ DocumentFilterByDocumentID docid ]
                                    [] (DocumentPagination 0 1))
      case mdoc of
        [doc] -> do
          return $ Right doc
        _ -> do
          Log.debug $ "Document " ++ show docid ++ " does not exist (in getDocByDocIDForAuthorOrAuthorsCompanyAdmin)"
          return $ Left DBResourceNotAvailable

{- |
   Get a document using docid, siglink, and magichash.
   ALWAYS FAILS THE SAME WAY FOR SECURITY PURPOSES (Left DBResourceNotAvailable).
   Document must exist.
   SignatoryLinkID must correspond to a siglink in document.
   MagicHash must match.
 -}
getDocByDocIDSigLinkIDAndMagicHash :: Kontrakcja m
                                   => DocumentID
                                   -> SignatoryLinkID
                                   -> MagicHash
                                   -> m (Either DBError Document)
getDocByDocIDSigLinkIDAndMagicHash docid sigid mh = do
  mdoc <- dbQuery $ GetDocumentByDocumentID docid
  case mdoc of
    Just doc | isJust $ getSigLinkFor doc (sigid, mh) -> return $ Right doc
    _                                                 -> return $ Left DBResourceNotAvailable
