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
    , getDocsByDocIDs
    , getDocByDocIDForAuthor
    , getDocByDocIDForAuthorOrAuthorsCompanyAdmin
    , getMagicHashForDocumentSignatoryWithUser
    ) where

import Control.Monad
import DB
import DBError
import Doc.Model
import Doc.DocStateData
import Doc.SignatoryLinkID
import Doc.DocumentID
import Kontra
import MagicHash
import qualified Log
import User.Model
import Data.List
import Util.SignatoryLinkUtils
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
      mdoc <- dbQuery (GetDocuments [ DocumentsVisibleToUser (userid user)
                                    ] [ DocumentFilterByDocumentID docid
                                      ]
                                    [] (0,1))
      case mdoc of
        [doc] -> do
          return $ Right doc
        _ -> do
          Log.debug $ "Document " ++ show docid ++ " does not exist (in getDocByDocID)"
          return $ Left DBResourceNotAvailable

getDocsByDocIDs :: Kontrakcja m => [DocumentID] -> m (Either DBError [Document])
getDocsByDocIDs docids = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> return $ Left DBNotLoggedIn
    Just user -> do
      docs <- dbQuery (GetDocuments [ DocumentsVisibleToUser (userid user)
                                    ] [ DocumentFilterByDocumentIDs docids
                                      ]
                                    [] (0,-1))
      -- lets see if all requested documents were found
      let documentsThatWereNotFound = docids \\ (map documentid docs)
      if null documentsThatWereNotFound
        then do
          return $ Right docs
        else do
          Log.debug $ "Documents " ++ show documentsThatWereNotFound ++ " do not exist (in getDocsByDocIDs)"
          return $ Left DBResourceNotAvailable

{- | Same as getDocByDocID, but works only for author -}
getDocByDocIDForAuthor :: Kontrakcja m => DocumentID -> m (Either DBError Document)
getDocByDocIDForAuthor docid = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> return $ Left DBNotLoggedIn
    Just user -> do
      mdoc <- dbQuery (GetDocuments [ DocumentsVisibleToUser (userid user)
                                    ] [ DocumentFilterByDocumentID docid
                                      , DocumentFilterByAuthor (userid user)
                                      ]
                                    [] (0,1))
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
      mdoc <- dbQuery (GetDocuments [ DocumentsVisibleToUser (userid user)
                                    ] [ DocumentFilterByDocumentID docid
                                      , DocumentFilterLinkIsAuthor True
                                      ]
                                    [] (0,1))
      case mdoc of
        [doc] -> do
          return $ Right doc
        _ -> do
          Log.debug $ "Document " ++ show docid ++ " does not exist (in getDocByDocIDForAuthorOrAuthorsCompanyAdmin)"
          return $ Left DBResourceNotAvailable


{- |
   Get a magichash for given signatory. Only possible if give user is author
 -}
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
