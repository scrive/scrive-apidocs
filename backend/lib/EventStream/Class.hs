module EventStream.Class
  ( MonadEventStream(..)
  , PartitionKey
  , StreamId
  ) where

import Control.Monad.Reader
import Data.Aeson

type StreamId = Text
type PartitionKey = Text

-- | Event stream monad with one function to push message into the event stream.
--   When pushing into the stream, you can determine the name of the stream as
--   well as the partitioning key. Partition key allows you to scale stuff in
--   more specific ways if needed. See documentation regarding
--   `kinesis partition key` for more details.
class Monad m => MonadEventStream m where
  pushEvent :: ToJSON a => StreamId -> PartitionKey -> a -> m ()

instance {-# OVERLAPPABLE #-} (MonadEventStream m, MonadTrans t, Monad (t m))
    => MonadEventStream (t m) where
  pushEvent streamId parititionKey message =
    lift $ pushEvent streamId parititionKey message
