module SMS.Model where

import SMS.Data
import SMS.Tables

import Data.Monoid

import DB
import DB.SQL2
import MagicHash
import MinutesTime
import OurPrelude
import Control.Monad.State.Lazy

sqlSelectSMSes :: State SqlSelect () -> SqlSelect
sqlSelectSMSes refine = sqlSelect "smses" $ do
                    sqlResult "smses.id"
                    sqlResult "smses.token"
                    sqlResult "smses.originator"
                    sqlResult "smses.msisdn"
                    sqlResult "smses.body"
                    --sqlResult "smses.to_be_sent"
                    --sqlResult "smses.sent"
                    sqlOrderBy "id DESC"
                    refine

selectSMSes :: MonadDB m => SqlSelect -> m [ShortMessage]
selectSMSes query' = do
  kRun_ $ query'
  smses <- fetchSMSes
  return smses

fetchSMSes :: MonadDB m => m [ShortMessage]
fetchSMSes = kFold decoder []
  where
    -- Note: this function gets mails in reversed order, but all queries
    -- use ORDER BY DESC, so in the end everything is properly ordered.
    decoder acc smsid token originator msisdn body
     = ShortMessage {
         smID         = smsid
       , smToken      = token
       , smOriginator = originator
       , smMSISDN     = msisdn
       , smBody       = body
       } : acc

data CreateSMS = CreateSMS MagicHash String String String MinutesTime
instance MonadDB m => DBUpdate m CreateSMS ShortMessageID where
  update (CreateSMS token originator msisdn body to_be_sent) =
    $fromJust `fmap` insertSMS token originator msisdn body to_be_sent

insertSMS :: MonadDB m => MagicHash -> String -> String -> String -> MinutesTime -> m (Maybe ShortMessageID)
insertSMS token originator msisdn body to_be_sent =
  getOne $ mkSQL INSERT tableSMSes [
      sql "token" token
    , sql "originator" originator
    , sql "msisdn" msisdn
    , sql "body" body
    , sql "to_be_sent" to_be_sent
    ] <> SQL "RETURNING id" []

data GetIncomingSMSes = GetIncomingSMSes
instance MonadDB m => DBQuery m GetIncomingSMSes [ShortMessage] where
  query GetIncomingSMSes = do
    selectSMSes $ sqlSelectSMSes $ do
      sqlWhere "body IS NOT NULL AND to_be_sent <= now() AND sent IS NULL"

data MarkSMSAsSent = MarkSMSAsSent ShortMessageID MinutesTime
instance MonadDB m => DBUpdate m MarkSMSAsSent Bool where
  update (MarkSMSAsSent mid time) = do
    kRun01 $ SQL "UPDATE smses SET sent = ? WHERE id = ?"
             [toSql time, toSql mid]

data DeferSMS = DeferSMS ShortMessageID MinutesTime
instance MonadDB m => DBUpdate m DeferSMS Bool where
  update (DeferSMS mid time) =
    kRun01 $ sqlUpdate "smses" $ do
        sqlSet "to_be_sent" time
        sqlWhereEq "id" mid


data DeleteSMSesOlderThenDays = DeleteSMSesOlderThenDays Integer
instance MonadDB m => DBUpdate m DeleteSMSesOlderThenDays Integer where
  update (DeleteSMSesOlderThenDays days) = do
    kRun $ sqlDelete "smses" $ do
      sqlWhere $ "(now() > to_be_sent + interval" <+> raw (unsafeFromString ("'"++show days++" days'")) <+> ")" -- Sorry but it did not work as param.
