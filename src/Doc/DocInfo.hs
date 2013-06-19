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
  , getLastSignedTime
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
isDocumentError doc = case documentstatus doc of
  DocumentError _ -> True
  _               -> False

{- |
   Is document shared?
 -}
isDocumentShared :: Document -> Bool
isDocumentShared doc = Shared == documentsharing doc

{- |
  Get the time of the last signature as Int. Returns MinutesTime 0 when there are no signatures.
-}
getLastSignedTime :: Document -> MinutesTime
getLastSignedTime doc =
  maximum $ fromSeconds 0 : [signtime si | SignatoryLink {maybesigninfo = Just si} <- documentsignatorylinks doc]



