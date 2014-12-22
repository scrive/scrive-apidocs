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
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe

import DB
import Doc.DocStateData
import Doc.DocumentID
import Doc.Model
import Doc.SignatoryLinkID
import Kontra
import MagicHash
import User.Model
import Util.SignatoryLinkUtils

getDocByDocID :: Kontrakcja m => DocumentID -> m Document
getDocByDocID docid = getDocByDocIDEx docid Nothing

getDocByDocIDEx :: Kontrakcja m => DocumentID -> Maybe MagicHash -> m Document
getDocByDocIDEx docid maccesstoken = do
  Context{ctxmaybeuser, ctxmaybepaduser} <- getContext
  -- document will be returned if ANY of the below is true (logical OR)
  let visibility = catMaybes [
          DocumentsVisibleToUser . userid <$> ctxmaybeuser
        , DocumentsVisibleToUser . userid <$> ctxmaybepaduser
        , DocumentsVisibleViaAccessToken  <$> maccesstoken
        ]
  dbQuery $ GetDocument
    visibility
    [DocumentFilterByDocumentID docid]

-- | Same as getDocByDocID, but works only for author
getDocByDocIDForAuthor :: Kontrakcja m => DocumentID -> m Document
getDocByDocIDForAuthor docid = do
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
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
  Context { ctxmaybeuser, ctxmaybepaduser} <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser) of
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
