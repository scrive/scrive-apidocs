module ScriveByMail.Model 
    (
     MailAPIInfo(..)
    )  
    where

import MagicHash

import Data.Int

data MailAPIInfo = MailAPIInfo {
    umapiKey          :: MagicHash
  , umapiDailyLimit   :: Int32
  , umapiSentToday    :: Int32
  } deriving (Eq, Ord, Show)

