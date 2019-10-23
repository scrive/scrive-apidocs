{-# LANGUAGE TemplateHaskell #-}

module DataRetentionPolicy
  ( DataRetentionPolicy(..)
  , defaultDataRetentionPolicy
  , drpIdleDocTimeout
  , drpIdleDocTimeoutPreparation
  , drpIdleDocTimeoutClosed
  , drpIdleDocTimeoutCanceled
  , drpIdleDocTimeoutTimedout
  , drpIdleDocTimeoutRejected
  , drpIdleDocTimeoutError
  , drpImmediateTrash
  , makeStricterDataRetentionPolicy
  , unjsonDataRetentionPolicy
  ) where

import Data.Int
import Data.Label
import Data.Unjson

import Doc.Types.DocumentStatus

data DataRetentionPolicy = DataRetentionPolicy
  { _drpIdleDocTimeoutPreparation :: Maybe Int16
  , _drpIdleDocTimeoutClosed      :: Maybe Int16
  , _drpIdleDocTimeoutCanceled    :: Maybe Int16
  , _drpIdleDocTimeoutTimedout    :: Maybe Int16
  , _drpIdleDocTimeoutRejected    :: Maybe Int16
  , _drpIdleDocTimeoutError       :: Maybe Int16
  , _drpImmediateTrash            :: Bool
  } deriving (Eq, Ord, Show)

mkLabel ''DataRetentionPolicy

drpIdleDocTimeout :: DocumentStatus -> DataRetentionPolicy :-> Maybe Int16
drpIdleDocTimeout = \case
  Preparation   -> drpIdleDocTimeoutPreparation
  Closed        -> drpIdleDocTimeoutClosed
  Canceled      -> drpIdleDocTimeoutCanceled
  Timedout      -> drpIdleDocTimeoutTimedout
  Rejected      -> drpIdleDocTimeoutRejected
  DocumentError -> drpIdleDocTimeoutError
  Pending       -> lens (const Nothing) (const id)

defaultDataRetentionPolicy :: DataRetentionPolicy
defaultDataRetentionPolicy = DataRetentionPolicy
  { _drpIdleDocTimeoutPreparation = Nothing
  , _drpIdleDocTimeoutClosed      = Nothing
  , _drpIdleDocTimeoutCanceled    = Nothing
  , _drpIdleDocTimeoutTimedout    = Nothing
  , _drpIdleDocTimeoutRejected    = Nothing
  , _drpIdleDocTimeoutError       = Nothing
  , _drpImmediateTrash            = False
  }

makeStricterDataRetentionPolicy
  :: DataRetentionPolicy -> DataRetentionPolicy -> DataRetentionPolicy
makeStricterDataRetentionPolicy drp1 drp2 = DataRetentionPolicy
  { _drpIdleDocTimeoutPreparation = choose drpIdleDocTimeoutPreparation
  , _drpIdleDocTimeoutClosed      = choose drpIdleDocTimeoutClosed
  , _drpIdleDocTimeoutCanceled    = choose drpIdleDocTimeoutCanceled
  , _drpIdleDocTimeoutTimedout    = choose drpIdleDocTimeoutTimedout
  , _drpIdleDocTimeoutRejected    = choose drpIdleDocTimeoutRejected
  , _drpIdleDocTimeoutError       = choose drpIdleDocTimeoutError
  , _drpImmediateTrash = get drpImmediateTrash drp1 || get drpImmediateTrash drp2
  }

  where
    choose :: Ord a => DataRetentionPolicy :-> Maybe a -> Maybe a
    choose l = case (get l drp1, get l drp2) of
      (Just x1     , Just x2) -> Just $ min x1 x2
      (mx1@(Just _), _      ) -> mx1
      (_           , mx2    ) -> mx2

unjsonDataRetentionPolicy :: UnjsonDef DataRetentionPolicy
unjsonDataRetentionPolicy =
  objectOf
    $   DataRetentionPolicy
    <$> fieldOpt "idle_doc_timeout_preparation"
                 (get drpIdleDocTimeoutPreparation)
                 "Number of days before moving documents in preparation to trash"
    <*> fieldOpt "idle_doc_timeout_closed"
                 (get drpIdleDocTimeoutClosed)
                 "Number of days before moving closed documents to trash"
    <*> fieldOpt "idle_doc_timeout_canceled"
                 (get drpIdleDocTimeoutCanceled)
                 "Number of days before moving cancelled documents to trash"
    <*> fieldOpt "idle_doc_timeout_timedout"
                 (get drpIdleDocTimeoutTimedout)
                 "Number of days before moving timed out documents to trash"
    <*> fieldOpt "idle_doc_timeout_rejected"
                 (get drpIdleDocTimeoutRejected)
                 "Number of days before moving rejected documents to trash"
    <*> fieldOpt "idle_doc_timeout_error"
                 (get drpIdleDocTimeoutError)
                 "Number of days before moving documents with errors to trash"
    <*> fieldDef "immediate_trash"
                 (get drpImmediateTrash defaultDataRetentionPolicy)
                 (get drpImmediateTrash)
                 "Option to delete documents in trash immediately"
