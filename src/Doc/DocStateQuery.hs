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

import DBError
import Doc.DocState
import Doc.DocUtils
import Company.CompanyState
import Kontra
import Misc
import Happstack.State     (query)
import Util.SignatoryLinkUtils
import qualified AppLogger as Log
import Control.Monad.State

{- |
   Assuming user is the logged in user, can he view the Document?

   Checks author's friends.
   Checks author.
   Checks author's supervisors.
   Checks author's supervisors friends.
   Checks if sharing is enabled and author is related to user.
 -}
canUserViewDoc :: User -> Document -> IO Bool
canUserViewDoc user doc 
  | isAuthor (doc, user) = return True
  | otherwise = do
    usersImFriendsWith <- query $ GetUsersByFriendUserID (userid user)
    usersFriendsAreSupervising <- fmap concat $ sequence $ map (query . GetUserSubaccounts . userid) usersImFriendsWith
    usersImSupervising <- query $ GetUserSubaccounts     (userid user)
    related            <- query $ GetUserRelatedAccounts (userid user)
    return $ (any isAuthor (zip (repeat doc) (usersImSupervising ++ usersImFriendsWith ++ usersFriendsAreSupervising)))
      || (any isAuthor (zip (repeat doc) related) && Shared == documentsharing doc)
  

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
          canAccess <- liftIO $ canUserViewDoc user doc              
          case canAccess of
            True  -> return $ Right doc
            False -> return $ Left DBResourceNotAvailable
    (_, Just company) -> do
      Log.debug "Logged in as company"
      mdoc <- query $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> return $ Left DBResourceNotAvailable
        Just doc -> if (documentoriginalcompany doc == Just (companyid company))
                     then return $ Right doc
                     else return $ Left DBResourceNotAvailable

{- |
   Get all of the documents a user can view.
   User must be logged in.
   Logged in user is in the documentsignatorylinks or a friend of someone with the documentsignatorylinks
 -}
getDocsByLoggedInUser :: Kontrakcja m => m (Either DBError [Document])
getDocsByLoggedInUser = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Nothing   -> return $ Left DBNotLoggedIn
    Just user -> do
      docs <- query $ GetDocuments (currentServiceID ctx)
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
getDocByDocIDSigLinkIDAndMagicHash :: Kontrakcja m
                                   => DocumentID
                                   -> SignatoryLinkID
                                   -> MagicHash
                                   -> m (Either DBError Document)
getDocByDocIDSigLinkIDAndMagicHash docid sigid mh = do
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing  -> return $ Left DBResourceNotAvailable
    Just doc ->
      case getSigLinkFor doc sigid of
        Just siglink | signatorymagichash siglink == mh -> return $ Right doc
        _ -> return $ Left DBResourceNotAvailable

