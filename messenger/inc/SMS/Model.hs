module SMS.Model where

import SMS.Data
import SMS.Tables

import Data.Monoid

import DB
import MagicHash
import MinutesTime
import OurPrelude

data CreateSMS = CreateSMS MagicHash String String String MinutesTime
instance MonadDB m => DBUpdate m CreateSMS ShortMessageID where
  update (CreateSMS token originator msisdn body to_be_sent) =
    $fromJust `fmap` insertSMS token originator msisdn body to_be_sent

insertSMS :: MonadDB m => MagicHash -> String -> String -> String -> MinutesTime -> DBEnv m (Maybe ShortMessageID)
insertSMS token originator msisdn body to_be_sent =
  getOne $ mkSQL INSERT tableMails [
      sql "token" token
    , sql "originator" originator
    , sql "msisdn" msisdn
    , sql "body" body
    , sql "to_be_sent" to_be_sent
    ] <> SQL "RETURNING id" []
