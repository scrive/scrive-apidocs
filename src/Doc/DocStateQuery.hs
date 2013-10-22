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
import Control.Monad.IO.Class
import Control.Exception
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
getDocByDocID :: Kontrakcja m => DocumentID -> m Document
getDocByDocID docid = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> liftIO $ throwIO DBNotLoggedIn
    Just user -> do
      (_,mdoc) <- dbQuery (GetDocuments2 False
                                    [ DocumentsVisibleToUser (userid user) ]
                                    [ DocumentFilterByDocumentID docid ]
                                    [] (0,1, Nothing))
      case mdoc of
        [doc] -> do
          return doc
        _ -> error "This will never happen due to allowzeroresults=False in statement above"

getDocsByDocIDs :: Kontrakcja m => [DocumentID] -> m [Document]
getDocsByDocIDs docids = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> liftIO $ throwIO DBNotLoggedIn
    Just user -> do
      (_,docs) <- dbQuery (GetDocuments2 True [ DocumentsVisibleToUser (userid user)
                                    ] [ DocumentFilterByDocumentIDs docids
                                      ]
                                    [] (0,-1, Nothing))
      -- lets see if all requested documents were found
      let documentsThatWereNotFound = docids \\ (map documentid docs)
      if null documentsThatWereNotFound
        then do
          return $ docs
        else do
          Log.debug $ "Documents " ++ show documentsThatWereNotFound ++ " do not exist (in getDocsByDocIDs)"
          liftIO $ throwIO DBResourceNotAvailable

{- | Same as getDocByDocID, but works only for author -}
getDocByDocIDForAuthor :: Kontrakcja m => DocumentID -> m Document
getDocByDocIDForAuthor docid = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> liftIO $ throwIO DBNotLoggedIn
    Just user -> do
      (_,mdoc) <- dbQuery (GetDocuments2 False [ DocumentsVisibleToUser (userid user)
                                    ] [ DocumentFilterByDocumentID docid
                                      , DocumentFilterByAuthor (userid user)
                                      ]
                                    [] (0,1, Nothing))
      case mdoc of
        [doc] -> do
          return $ doc
        _ -> error "This will never happen due to allowzeroresults=False in statement above"

{- | Same as getDocByDocID, but works only for author or authors company admin-}
getDocByDocIDForAuthorOrAuthorsCompanyAdmin :: Kontrakcja m => DocumentID -> m Document
getDocByDocIDForAuthorOrAuthorsCompanyAdmin docid = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
    Nothing -> liftIO $ throwIO DBNotLoggedIn
    Just user -> do
      (_,mdoc) <- dbQuery (GetDocuments2 False [ DocumentsVisibleToUser (userid user)
                                    ] [ DocumentFilterByDocumentID docid
                                      , DocumentFilterLinkIsAuthor True
                                      ]
                                    [] (0,1, Nothing))
      case mdoc of
        [doc] -> do
          return $ doc
        _ -> error "This will never happen due to allowzeroresults=False in statement above"


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
