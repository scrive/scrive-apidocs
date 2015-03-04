module SMS.Model where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.State
import Data.Int

import DB
import MinutesTime
import OurPrelude
import SMS.Data

sqlSelectSMSes :: State SqlSelect () -> SqlSelect
sqlSelectSMSes refine = sqlSelect "smses" $ do
  sqlResult "smses.id"
  sqlResult "smses.originator"
  sqlResult "smses.msisdn"
  sqlResult "smses.body"
  sqlResult "smses.data"
  sqlResult "smses.attempt"
  --sqlResult "smses.to_be_sent"
  --sqlResult "smses.sent"
  sqlOrderBy "id"
  refine

selectSMSes :: MonadDB m => SqlSelect -> m [ShortMessage]
selectSMSes query' = do
  runQuery_ query'
  fetchMany fetchSMS

fetchSMS :: (ShortMessageID, String, String, String, String, Int32) -> ShortMessage
fetchSMS (smsid, originator, msisdn, body, sdata, attempt) = ShortMessage {
  smID         = smsid
, smOriginator = originator
, smMSISDN     = msisdn
, smBody       = body
, smData       = sdata
, smAttempt    = attempt
}

data CreateSMS = CreateSMS String String String String
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateSMS ShortMessageID where
  update (CreateSMS originator msisdn body sdata) =
    $fromJust `fmap` insertSMS originator msisdn body sdata 0

insertSMS :: (MonadDB m, MonadThrow m) => String -> String -> String -> String -> Int32 -> m (Maybe ShortMessageID)
insertSMS originator msisdn body sdata attempt = do
  runQuery_ . sqlInsert "smses" $ do
    sqlSet "originator" originator
    sqlSet "msisdn" msisdn
    sqlSet "body" body
    sqlSet "to_be_sent" unixEpoch
    sqlSet "data" sdata
    sqlSet "attempt" attempt
    sqlResult "id"
  fetchOne runIdentity

data GetIncomingSMSes = GetIncomingSMSes
instance (MonadDB m, MonadTime m) => DBQuery m GetIncomingSMSes [ShortMessage] where
  query GetIncomingSMSes = do
    now <- currentTime
    selectSMSes . sqlSelectSMSes $ do
      sqlWhere "body IS NOT NULL"
      sqlWhere "sent IS NULL"
      sqlWhere $ "to_be_sent <=" <?> now

data MarkSMSAsSent = MarkSMSAsSent ShortMessageID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m MarkSMSAsSent Bool where
  update (MarkSMSAsSent mid time) = do
    runQuery01 . sqlUpdate "smses" $ do
      sqlSet "sent" time
      sqlWhereEq "id" mid

data DeferSMS = DeferSMS ShortMessageID UTCTime
instance (MonadDB m, MonadThrow m) => DBUpdate m DeferSMS Bool where
  update (DeferSMS mid time) =
    runQuery01 $ sqlUpdate "smses" $ do
      sqlSet "to_be_sent" time
      sqlSetCmd "attempt" "attempt + 1"
      sqlWhereEq "id" mid

data DeleteSMSesOlderThenDays = DeleteSMSesOlderThenDays Int
instance (MonadDB m, MonadTime m) => DBUpdate m DeleteSMSesOlderThenDays Int where
  update (DeleteSMSesOlderThenDays days) = do
    past <- (days `daysBefore`) <$> currentTime
    runQuery . sqlDelete "smses" $ do
      sqlWhere $ "to_be_sent <=" <?> past

data UpdateWithSMSEvent = UpdateWithSMSEvent ShortMessageID SMSEvent
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateWithSMSEvent Bool where
  update (UpdateWithSMSEvent mid ev) = do
    runQuery01 . sqlInsert "sms_events" $ do
      sqlSet "sms_id" mid
      sqlSet "event" ev

data GetUnreadSMSEvents = GetUnreadSMSEvents
instance MonadDB m => DBQuery m GetUnreadSMSEvents [(SMSEventID, ShortMessageID, SMSEvent, String)] where
  query GetUnreadSMSEvents = do
    runQuery_ . sqlSelect "sms_events" $ do
      sqlJoinOn "smses" "smses.id = sms_events.sms_id"

      sqlResult "sms_events.id"
      sqlResult "sms_events.sms_id"
      sqlResult "sms_events.event"
      sqlResult "smses.data"

      sqlWhere "sms_events.event_read IS NULL"
      sqlOrderBy "sms_events.id"
    fetchMany id

data DeleteSMS = DeleteSMS ShortMessageID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteSMS Bool where
  update (DeleteSMS smsid) = do
    runQuery01 . sqlDelete "smses" $ do
      sqlWhereEq "id" smsid

data MarkSMSEventAsRead = MarkSMSEventAsRead SMSEventID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m MarkSMSEventAsRead Bool where
  update (MarkSMSEventAsRead eid) = do
    now <- currentTime
    runQuery01 . sqlUpdate "sms_events" $ do
      sqlSet "event_read" now
      sqlWhereEq "id" eid
