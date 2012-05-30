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
    , getDocByDocIDSigLinkIDAndMagicHash
    ) where

import Control.Applicative
import Control.Monad
import DB
import DBError
import Doc.Model
import Doc.DocStateData
import Company.Model
import Kontra
import MagicHash
import Util.SignatoryLinkUtils
import qualified Log
import Doc.DocInfo
import User.Model
import Data.Maybe

{- |
   Assuming user is the logged in user, can he view the Document?

   They can if:
     the document is saved for them or their company if they are a company admin
     the document is authored within the user's company and shared
 -}
canUserViewDoc :: User -> Document -> Bool
canUserViewDoc user doc =
  docIsSavedForUser --the doc is saved for the user (or the user's company if they are an admin)
  || isSharedWithinCompany --the doc is shared within the user's company
  where
    docIsSavedForUser =
      any (isSigLinkSavedFor user) $ documentsignatorylinks doc
    isSharedWithinCompany = isDocumentShared doc && isAuthoredWithinCompany
    isAuthoredWithinCompany = (isJust $ getSigLinkFor doc SignatoryAuthor) 
                              && (isJust $ getSigLinkFor doc $ usercompany user)

{- |
   Securely find a document by documentid for the author or within their company.
   User must be logged in (otherwise Left DBNotLoggedIn).
   Document must exist (otherwise Left DBNotAvailable).
   Logged in user is author OR logged in user is in the company of the author (otherwise LeftDBNotAvailable).
 -}
getDocByDocID :: Kontrakcja m => DocumentID -> m (Either DBError Document)
getDocByDocID docid = do
  Context { ctxmaybeuser, ctxmaybepaduser , ctxcompany } <- getContext
  case (ctxmaybeuser `mplus` ctxmaybepaduser, ctxcompany) of
    (Nothing, Nothing) -> return $ Left DBNotLoggedIn
    (Just user, _) -> do
      mdoc <- dbQuery $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> do
          Log.debug "Does not exist"
          return $ Left DBResourceNotAvailable
        Just doc -> do
          case canUserViewDoc user doc of
            False -> return $ Left DBResourceNotAvailable
            True  -> return $ Right doc
    (_, Just company) -> do
      Log.debug "Logged in as company"
      mdoc <- dbQuery $ GetDocumentByDocumentID docid
      case mdoc of
        Nothing  -> return $ Left DBResourceNotAvailable
        Just doc -> if isJust $ getSigLinkFor doc (companyid company)
                    then return $ Right doc
                    else return $ Left DBResourceNotAvailable

{- | Same as getDocByDocID, but works only for author -}
getDocByDocIDForAuthor :: Kontrakcja m => DocumentID -> m (Either DBError Document)
getDocByDocIDForAuthor docid = do
      muser <- liftM2 mplus (ctxmaybeuser <$> getContext) (ctxmaybepaduser <$> getContext)
      edoc <- getDocByDocID docid
      case edoc of
           Right doc -> if (isAuthor (doc, muser))
                           then return $ Right doc
                           else return $ Left DBResourceNotAvailable
           e -> return e

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
    Just doc | (isJust $ getSigLinkFor doc mh) 
               && (isJust $ getSigLinkFor doc sigid) -> return $ Right doc
    _ -> return $ Left DBResourceNotAvailable

