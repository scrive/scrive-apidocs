module Mails.SendGrid where

import Data.Int

data SendGridEventType =
    Processed
  | Opened
  | Dropped String              -- ^ drop reason
  | Deferred String Int         -- ^ response, delivery attempt
  | Delivered String            -- ^ response from mta
  | Bounce String String String -- ^ status, reason, type
    deriving (Eq, Ord, Show)

data SendGridEvent = SendgridEvent {
    seAddress :: String
  , seType    :: SendGridEventType
  , seData    :: String
  } deriving Show
