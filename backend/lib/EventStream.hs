module EventStream
    ( module EventStream.Kinesis
    , module EventStream.Class
    , chargeableItemsStreamId
    ) where

import EventStream.Class
import EventStream.Kinesis

chargeableItemsStreamId :: Text
chargeableItemsStreamId = "chargeable_items"
