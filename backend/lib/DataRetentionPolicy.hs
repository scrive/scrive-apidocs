{-# LANGUAGE TemplateHaskell #-}

module DataRetentionPolicy
  ( DataRetentionPolicy(..)
  , defaultDataRetentionPolicy
  , drpIdleDocTimeout
  , makeStricterDataRetentionPolicy
  , unjsonDataRetentionPolicy
  ) where

import Data.Int
import Data.Unjson
import Optics

import Doc.Types.DocumentStatus

data DataRetentionPolicy = DataRetentionPolicy
  { drpIdleDocTimeoutPreparation :: Maybe Int16
  , drpIdleDocTimeoutClosed      :: Maybe Int16
  , drpIdleDocTimeoutCanceled    :: Maybe Int16
  , drpIdleDocTimeoutTimedout    :: Maybe Int16
  , drpIdleDocTimeoutRejected    :: Maybe Int16
  , drpIdleDocTimeoutError       :: Maybe Int16
  , drpImmediateTrash            :: Bool
  } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''DataRetentionPolicy

drpIdleDocTimeout :: DocumentStatus -> Lens' DataRetentionPolicy (Maybe Int16)
drpIdleDocTimeout = \case
  Preparation   -> #drpIdleDocTimeoutPreparation
  Closed        -> #drpIdleDocTimeoutClosed
  Canceled      -> #drpIdleDocTimeoutCanceled
  Timedout      -> #drpIdleDocTimeoutTimedout
  Rejected      -> #drpIdleDocTimeoutRejected
  DocumentError -> #drpIdleDocTimeoutError
  Pending       -> lens (const Nothing) const

defaultDataRetentionPolicy :: DataRetentionPolicy
defaultDataRetentionPolicy = DataRetentionPolicy
  { drpIdleDocTimeoutPreparation = Nothing
  , drpIdleDocTimeoutClosed      = Nothing
  , drpIdleDocTimeoutCanceled    = Nothing
  , drpIdleDocTimeoutTimedout    = Nothing
  , drpIdleDocTimeoutRejected    = Nothing
  , drpIdleDocTimeoutError       = Nothing
  , drpImmediateTrash            = False
  }

makeStricterDataRetentionPolicy
  :: DataRetentionPolicy -> DataRetentionPolicy -> DataRetentionPolicy
makeStricterDataRetentionPolicy drp1 drp2 = DataRetentionPolicy
  { drpIdleDocTimeoutPreparation = choose drpIdleDocTimeoutPreparation
  , drpIdleDocTimeoutClosed      = choose drpIdleDocTimeoutClosed
  , drpIdleDocTimeoutCanceled    = choose drpIdleDocTimeoutCanceled
  , drpIdleDocTimeoutTimedout    = choose drpIdleDocTimeoutTimedout
  , drpIdleDocTimeoutRejected    = choose drpIdleDocTimeoutRejected
  , drpIdleDocTimeoutError       = choose drpIdleDocTimeoutError
  , drpImmediateTrash            = drpImmediateTrash drp1 || drpImmediateTrash drp2
  }

  where
    choose :: Ord a => (DataRetentionPolicy -> Maybe a) -> Maybe a
    choose l = case (l drp1, l drp2) of
      (Just x1     , Just x2) -> Just $ min x1 x2
      (mx1@(Just _), _      ) -> mx1
      (_           , mx2    ) -> mx2

unjsonDataRetentionPolicy :: UnjsonDef DataRetentionPolicy
unjsonDataRetentionPolicy =
  objectOf
    $   DataRetentionPolicy
    <$> fieldOpt "idle_doc_timeout_preparation"
                 drpIdleDocTimeoutPreparation
                 "Number of days before moving documents in preparation to trash"
    <*> fieldOpt "idle_doc_timeout_closed"
                 drpIdleDocTimeoutClosed
                 "Number of days before moving closed documents to trash"
    <*> fieldOpt "idle_doc_timeout_canceled"
                 drpIdleDocTimeoutCanceled
                 "Number of days before moving cancelled documents to trash"
    <*> fieldOpt "idle_doc_timeout_timedout"
                 drpIdleDocTimeoutTimedout
                 "Number of days before moving timed out documents to trash"
    <*> fieldOpt "idle_doc_timeout_rejected"
                 drpIdleDocTimeoutRejected
                 "Number of days before moving rejected documents to trash"
    <*> fieldOpt "idle_doc_timeout_error"
                 drpIdleDocTimeoutError
                 "Number of days before moving documents with errors to trash"
    <*> fieldDef "immediate_trash"
                 (drpImmediateTrash defaultDataRetentionPolicy)
                 drpImmediateTrash
                 "Option to delete documents in trash immediately"
