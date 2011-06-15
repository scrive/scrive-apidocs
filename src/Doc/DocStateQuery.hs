{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
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
import Company.CompanyState
import Kontra
import Misc
import Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Happstack.State     (query)

{- |
   Securely find a document by documentid for the author or his friends.
   User must be logged in (otherwise Left DBNotLoggedIn).
   Document must exist (otherwise Left DBNotAvailable).
   Logged in user is author OR logged in user is friend of author (otherwise LeftDBNotAvailable).
 -}
getDocByDocID :: DocumentID -> Kontra (Either DBError Document)
getDocByDocID docid = do
  Context { ctxmaybeuser, ctxcompany } <- get
  case (ctxmaybeuser,ctxcompany) of
    (Just user,_) -> do
      mdoc <- query $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> do
          liftIO $ print "does not exist"
          return $ Left DBResourceNotAvailable
        Just doc ->
          case isUserAuthor doc user of
            True  -> do
              liftIO $ print "Is the author"
              return $ Right doc
            False -> do
              liftIO $ print "is not author"
              usersImFriendsWith <- query $ GetUsersByFriendUserID (userid user)
              usersImSupervising <- query $ GetUserSubaccounts  (userid user)
              related <- query $ GetUserRelatedAccounts  (userid user)
              let canAcces =    (any (isUserAuthor doc) $ usersImSupervising ++ usersImFriendsWith)
                             || ((any (isUserAuthor doc) related) && (Shared == documentsharing doc))
              case (canAcces) of
                True  -> return $ Right doc
                False -> return $ Left DBResourceNotAvailable
    (_,Just company) -> do
      liftIO $ putStrLn $ "logged as company"  
      mdoc <- query $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> return $ Left DBResourceNotAvailable
        Just doc -> if (documentoriginalcompany doc == (Just $ companyid company)) 
                     then return $ Right doc 
                     else return $ Left DBResourceNotAvailable
    (Nothing,Nothing) -> return $ Left DBNotLoggedIn            

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

