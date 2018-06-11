-----------------------------------------------------------------------------
-- |
-- Module      :  Doc.DocInfo
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility functions for accessing the innards of Documents.
-----------------------------------------------------------------------------
module Doc.DocInfo(
    isPending
  , isPreparation
  , isClosed
  , isCanceled
  , isTimedout
  , isRejected
  , isDocumentError
  , isDocumentShared
  , isSignable
  , isTemplate
  , getLastSignedTime
  , isAccessibleBySignatories
  , documentStatusesAccessibleBySignatories
) where

import Doc.DocStateData
import MinutesTime

-- Predicates on documentstatus

{- |
   Is the document pending?
 -}
isPending :: Document -> Bool
isPending doc = Pending == documentstatus doc

{- |
   Is the document in preparation?
 -}
isPreparation :: Document -> Bool
isPreparation doc = Preparation == documentstatus doc

{- |
   Is the document Closed?
 -}
isClosed :: Document -> Bool
isClosed doc = Closed == documentstatus doc

{- |
   Is the document canceled?
 -}
isCanceled :: Document -> Bool
isCanceled doc = Canceled == documentstatus doc

{- |
   Is the document timedout?
 -}
isTimedout :: Document -> Bool
isTimedout doc = Timedout == documentstatus doc

{- |
   Is the document rejected?
 -}
isRejected :: Document -> Bool
isRejected doc = Rejected == documentstatus doc

{- |
   Is document error?
 -}
isDocumentError :: Document -> Bool
isDocumentError doc =  DocumentError == documentstatus doc

{- |
   Is the document signable?
 -}
isSignable :: Document -> Bool
isSignable doc = documenttype doc == Signable

{- |
   Is the document a template?
 -}
isTemplate :: Document -> Bool
isTemplate d = documenttype d == Template

{- |
   Is document shared?
 -}
isDocumentShared :: Document -> Bool
isDocumentShared doc = Shared == documentsharing doc && isTemplate doc

{- |
  Get the time of the last signature as Int. Returns unixEpoch when there are no signatures.
-}
getLastSignedTime :: Document -> UTCTime
getLastSignedTime doc =
  maximum $ unixEpoch : [signtime si | SignatoryLink {maybesigninfo = Just si} <- documentsignatorylinks doc]

-- | Can signatories see the document?
isAccessibleBySignatories :: Document -> Bool
isAccessibleBySignatories doc =
  documentstatus doc `elem` documentStatusesAccessibleBySignatories

-- | Statuses in which a document is accessible by signatories.
documentStatusesAccessibleBySignatories :: [DocumentStatus]
documentStatusesAccessibleBySignatories = [Pending, Closed]
