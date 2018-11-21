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
  , getLastSignedOrApprovedTime
  , isAccessibleBySignatories
  , documentStatusesAccessibleBySignatories
) where

import Data.Time

import Doc.DocStateData
import MinutesTime
import Util.SignatoryLinkUtils

-- Predicates on document status

-- | Is the document pending?
isPending :: Document -> Bool
isPending        =   (==) Pending       . documentstatus

-- | Is the document in preparation?
isPreparation :: Document -> Bool
isPreparation    =   (==) Preparation   . documentstatus

-- | Is the document Closed?
isClosed :: Document -> Bool
isClosed         =   (==) Closed        . documentstatus

-- | Is the document canceled?
isCanceled :: Document -> Bool
isCanceled       =   (==) Canceled      . documentstatus

-- | Is the document timedout?
isTimedout :: Document -> Bool
isTimedout       =   (==) Timedout      . documentstatus

-- | Is the document rejected?
isRejected :: Document -> Bool
isRejected       =   (==) Rejected      . documentstatus

-- | Is document error?
isDocumentError :: Document -> Bool
isDocumentError  =   (==) DocumentError . documentstatus

-- | Is the document signable?
isSignable :: Document -> Bool
isSignable       =   (==) Signable      . documenttype

-- | Is the document a template?
isTemplate :: Document -> Bool
isTemplate       =   (==) Template      . documenttype

-- | Is document shared?
isDocumentShared :: Document -> Bool
isDocumentShared =  ((==) Shared . documentsharing)
                 && isTemplate

-- | Get the time of the last signature/approval as Int. Returns unixEpoch when
-- there are no signatures/approvals.
getLastSignedOrApprovedTime :: Document -> UTCTime
getLastSignedOrApprovedTime doc =
  maximum $ unixEpoch :
  [ signtime si
  | SignatoryLink { maybesigninfo = Just si } <-
      filter (isSignatory || isApprover) . documentsignatorylinks $ doc ]

-- | Can signatories see the document?
isAccessibleBySignatories :: UTCTime -> Document -> Bool
isAccessibleBySignatories _now doc =
  -- It adds 31 days and stops at midnight so that, counting days, it is
  -- accessible for 30 days.
  let _limit = (addUTCTime (31*24*3600) (documentmtime doc)) { utctDayTime = 0 }
  in documentstatus doc `elem` documentStatusesAccessibleBySignatories
  -- TEMPORARILY DISABLED
  -- && (isPending doc || now < limit)

-- | Statuses in which a document is accessible by signatories.
documentStatusesAccessibleBySignatories :: [DocumentStatus]
documentStatusesAccessibleBySignatories = [Pending, Closed]
