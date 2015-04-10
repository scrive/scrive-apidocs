module SMS.Model (
    messengerJobSelectors
  , messengerJobFetcher
  --, smsNotificationChannel
  , smsSelectors
  , smsFetcher
  , CreateSMS(..)
  , CleanSMSesOlderThanDays(..)
  , UpdateWithSMSEvent(..)
  , GetUnreadSMSEvents(..)
  , MarkSMSEventAsRead(..)
  ) where

import Control.Applicative
import Control.Monad.Catch
import Data.Int

import DB
import MinutesTime
import SMS.Data

messengerJobSelectors :: [SQL]
messengerJobSelectors = ["id", "attempts"]

messengerJobFetcher :: (JobType, Int32) -> MessengerJob
messengerJobFetcher (jtype, attempts) = MessengerJob {
  mjType = jtype
, mjAttempts = attempts
}

----------------------------------------

--smsNotificationChannel :: Channel
--smsNotificationChannel = "sms"

smsSelectors :: [SQL]
smsSelectors = [
    "id"
  , "originator"
  , "msisdn"
  , "body"
  , "data"
  , "attempts"
  ]

smsFetcher :: (ShortMessageID, String, String, String, String, Int32) -> ShortMessage
smsFetcher (smsid, originator, msisdn, body, sdata, attempts) = ShortMessage {
  smID         = smsid
, smOriginator = originator
, smMSISDN     = msisdn
, smBody       = body
, smData       = sdata
, smAttempts   = attempts
}

----------------------------------------

data CreateSMS = CreateSMS String String String String
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateSMS ShortMessageID where
  update (CreateSMS originator msisdn body sdata) = do
    runQuery_ . sqlInsert "smses" $ do
      sqlSet "originator" originator
      sqlSet "msisdn" msisdn
      sqlSet "body" body
      sqlSet "run_at" unixEpoch
      sqlSet "data" sdata
      sqlResult "id"
    mid <- fetchOne runIdentity
    --notify smsNotificationChannel ""
    return mid

data CleanSMSesOlderThanDays = CleanSMSesOlderThanDays Int
instance (MonadDB m, MonadTime m) => DBUpdate m CleanSMSesOlderThanDays Int where
  update (CleanSMSesOlderThanDays days) = do
    past <- (days `daysBefore`) <$> currentTime
    runQuery . sqlDelete "smses" $ do
      sqlWhere $ "finished_at <=" <?> past

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

data MarkSMSEventAsRead = MarkSMSEventAsRead SMSEventID
instance (MonadDB m, MonadThrow m, MonadTime m) => DBUpdate m MarkSMSEventAsRead Bool where
  update (MarkSMSEventAsRead eid) = do
    now <- currentTime
    runQuery01 . sqlUpdate "sms_events" $ do
      sqlSet "event_read" now
      sqlWhereEq "id" eid
