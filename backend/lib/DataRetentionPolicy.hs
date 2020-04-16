module DataRetentionPolicy
  ( DataRetentionPolicy
  , defaultDataRetentionPolicy
  , drpIdleDocTimeout
  , makeStricterDataRetentionPolicy
  , unjsonDataRetentionPolicy
  ) where

import Data.Int
import Data.Unjson
import Optics (lens)

import DataRetentionPolicy.Internal
import Doc.Types.DocumentStatus

drpIdleDocTimeout :: DocumentStatus -> Lens' DataRetentionPolicy (Maybe Int16)
drpIdleDocTimeout = \case
  Preparation   -> #idleDocTimeoutPreparation
  Closed        -> #idleDocTimeoutClosed
  Canceled      -> #idleDocTimeoutCanceled
  Timedout      -> #idleDocTimeoutTimedout
  Rejected      -> #idleDocTimeoutRejected
  DocumentError -> #idleDocTimeoutError
  Pending       -> lens (const Nothing) const

defaultDataRetentionPolicy :: DataRetentionPolicy
defaultDataRetentionPolicy = DataRetentionPolicy { idleDocTimeoutPreparation = Nothing
                                                 , idleDocTimeoutClosed      = Nothing
                                                 , idleDocTimeoutCanceled    = Nothing
                                                 , idleDocTimeoutTimedout    = Nothing
                                                 , idleDocTimeoutRejected    = Nothing
                                                 , idleDocTimeoutError       = Nothing
                                                 , immediateTrash            = False
                                                 }

makeStricterDataRetentionPolicy
  :: DataRetentionPolicy -> DataRetentionPolicy -> DataRetentionPolicy
makeStricterDataRetentionPolicy drp1 drp2 = DataRetentionPolicy
  { idleDocTimeoutPreparation = choose idleDocTimeoutPreparation
  , idleDocTimeoutClosed      = choose idleDocTimeoutClosed
  , idleDocTimeoutCanceled    = choose idleDocTimeoutCanceled
  , idleDocTimeoutTimedout    = choose idleDocTimeoutTimedout
  , idleDocTimeoutRejected    = choose idleDocTimeoutRejected
  , idleDocTimeoutError       = choose idleDocTimeoutError
  , immediateTrash            = immediateTrash drp1 || immediateTrash drp2
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
                 idleDocTimeoutPreparation
                 "Number of days before moving documents in preparation to trash"
    <*> fieldOpt "idle_doc_timeout_closed"
                 idleDocTimeoutClosed
                 "Number of days before moving closed documents to trash"
    <*> fieldOpt "idle_doc_timeout_canceled"
                 idleDocTimeoutCanceled
                 "Number of days before moving cancelled documents to trash"
    <*> fieldOpt "idle_doc_timeout_timedout"
                 idleDocTimeoutTimedout
                 "Number of days before moving timed out documents to trash"
    <*> fieldOpt "idle_doc_timeout_rejected"
                 idleDocTimeoutRejected
                 "Number of days before moving rejected documents to trash"
    <*> fieldOpt "idle_doc_timeout_error"
                 idleDocTimeoutError
                 "Number of days before moving documents with errors to trash"
    <*> fieldDef "immediate_trash"
                 (immediateTrash defaultDataRetentionPolicy)
                 immediateTrash
                 "Option to delete documents in trash immediately"
