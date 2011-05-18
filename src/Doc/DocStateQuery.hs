{-# OPTIONS_GHC -Wall -Werror #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocStateQuery
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Each exported function is a middle-man between the Controller and the Model.
-- They provide these extra features over the Model:
--  * Access control
--  * Aggregation
--  * Filtering
--
-- This module only provides queries.
--
-- This module should control access centrally (instead of inside each Controller)
-- This module should aggregate results from multiple queries (such as a join)
-- This module should filter results from queries (such as removing the "deleted" documents)
-----------------------------------------------------------------------------

module Doc.DocStateQuery
    ( getDocByDocID
    , getDocsByLoggedInUser
    , getDocByDocIDSigLinkIDAndMagicHash
    ) where
    
import DBError
import Doc.DocState
import Doc.DocUtils
import Kontra
import Misc

import Control.Monad.State (get)
import Happstack.State     (query)

{- |
   Securely find a document by documentid for the author or his friends.
   User must be logged in (otherwise Left DBNotLoggedIn).
   Document must exist (otherwise Left DBNotAvailable).
   Logged in user is author OR logged in user is friend of author (otherwise LeftDBNotAvailable).
 -}
getDocByDocID :: DocumentID -> Kontra (Either DBError Document)
getDocByDocID docid = do
  Context { ctxmaybeuser } <- get
  case ctxmaybeuser of
    Nothing   -> return $ Left DBNotLoggedIn
    Just user -> do
      mdoc <- query $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> return $ Left DBResourceNotAvailable
        Just doc ->
          case isUserAuthor doc user of
            True  -> return $ Right doc
            False -> do
              usersImFriendsWith <- query $ GetUsersByFriendUserID (userid user)
              case any (isUserAuthor doc) usersImFriendsWith of
                True  -> return $ Right doc
                False -> return $ Left DBResourceNotAvailable

{- |
   Get all of the documents a user can view.
   User must be logged in.
   Logged in user is in the documentsignatorylinks or a friend of someone with the documentsignatorylinks
 -}
getDocsByLoggedInUser :: Kontra (Either DBError [Document])
getDocsByLoggedInUser = do
  ctx <- get
  case ctxmaybeuser ctx of
    Nothing   -> return $ Left DBNotLoggedIn
    Just user -> do
      docs <- query $ GetDocuments $ currentServiceID ctx
      usersImFriendsWith <- query $ GetUsersByFriendUserID (userid user)
      return $ Right [ doc | doc <- docs
                           , any (\u -> canUserViewDirectly u doc) (user : usersImFriendsWith) ]

{- |
   Get a document using docid, siglink, and magichash.
   ALWAYS FAILS THE SAME WAY FOR SECURITY PURPOSES (Left DBResourceNotAvailable).
   Document must exist.
   SignatoryLinkID must correspond to a siglink in document.
   MagicHash must match.
 -}
getDocByDocIDSigLinkIDAndMagicHash :: DocumentID 
                                   -> SignatoryLinkID 
                                   -> MagicHash 
                                   -> Kontra (Either DBError Document)
getDocByDocIDSigLinkIDAndMagicHash docid sigid mh = do
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing  -> return $ Left DBResourceNotAvailable
    Just doc ->
      case getSigLinkBySigLinkID sigid doc of
        Just siglink | signatorymagichash siglink == mh -> return $ Right doc
        _ -> return $ Left DBResourceNotAvailable

