-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocStateQuery
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
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
    , getDocsByLoggedInUser
    , getDocByDocIDSigLinkIDAndMagicHash
    , canUserViewDoc
    ) where

import DB.Types
import DB.Classes
import DBError
import Doc.DocState
import Doc.DocUtils
import Company.Model
import Kontra
import Happstack.State (query)
import Util.SignatoryLinkUtils
import qualified AppLogger as Log
import Doc.DocInfo
import User.Model

{- |
   Assuming user is the logged in user, can he view the Document?

   They can if:
     the document is saved for them or their company if they are a company admin
     the document is authored within the user's company and shared
     the document is saved for one of the user's friends, or saved for their friend's company if their friend is a company admin
 -}
canUserViewDoc :: Kontrakcja m => User -> Document -> m Bool
canUserViewDoc user doc 
  | docIsSavedFor user = return True --the doc is saved for the user (or the user's company if they are an admin)
  | isSharedWithinCompany = return True --the doc is shared within the user's company
  | otherwise = do --the doc is saved for one of the user's friends (or the user's friend's company if their friend is an admin)
      friends <- runDBQuery $ GetUsersByFriendUserID (userid user)
      return $ any docIsSavedFor friends
  where
    docIsSavedFor u =
      any (isSigLinkSavedFor u) $ documentsignatorylinks doc
    isSharedWithinCompany = isDocumentShared doc && isAuthoredWithinCompany
    isAuthoredWithinCompany = isSigLinkFor (usercompany user) (getAuthorSigLink doc)

{- |
   Securely find a document by documentid for the author or his friends.
   User must be logged in (otherwise Left DBNotLoggedIn).
   Document must exist (otherwise Left DBNotAvailable).
   Logged in user is author OR logged in user is friend of author (otherwise LeftDBNotAvailable).
 -}
getDocByDocID :: Kontrakcja m => DocumentID -> m (Either DBError Document)
getDocByDocID docid = do
  Context { ctxmaybeuser, ctxcompany } <- getContext
  case (ctxmaybeuser, ctxcompany) of
    (Nothing, Nothing) -> return $ Left DBNotLoggedIn
    (Just user, _) -> do
      mdoc <- query $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> do
          Log.debug "Does not exist"
          return $ Left DBResourceNotAvailable
        Just doc -> do
          canAccess <- canUserViewDoc user doc
          case canAccess of
            False -> return $ Left DBResourceNotAvailable
            True  -> return $ Right doc
    (_, Just company) -> do
      Log.debug "Logged in as company"
      mdoc <- query $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> return $ Left DBResourceNotAvailable
        Just doc -> if any ((== (Just . companyid $ company)) . maybecompany) (documentsignatorylinks doc)
                     then return $ Right doc
                     else return $ Left DBResourceNotAvailable

{- |
   Get all of the documents a user can view.
   User must be logged in.
   Logged in user is in the documentsignatorylinks or a friend of someone with the documentsignatorylinks
   What about companies?
 -}
getDocsByLoggedInUser :: Kontrakcja m => m (Either DBError [Document])
getDocsByLoggedInUser = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Nothing   -> return $ Left DBNotLoggedIn
    Just user -> do
      docs <- query $ GetDocuments (currentServiceID ctx)
      usersImFriendsWith <- runDBQuery $ GetUsersByFriendUserID (userid user)
      return $ Right [ doc | doc <- docs
                           , any (\u -> canUserViewDirectly u doc) (user : usersImFriendsWith) ]

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
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Just doc | isSigLinkFor mh (getSigLinkFor doc sigid) -> return $ Right doc
    _ -> return $ Left DBResourceNotAvailable

