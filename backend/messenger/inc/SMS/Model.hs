module SMS.Model (
    messengerJobSelectors
  , messengerJobFetcher
  , smsNotificationChannel
  , smsSelectors
  , smsFetcher
  , CreateSMS(..)
  , CleanSMSesOlderThanDays(..)
  , UpdateSMSWithTeliaID(..)
  , UpdateSMSWithMbloxID(..)
  , UpdateWithSMSEvent(..)
  , UpdateWithSMSEventForTeliaID(..)
  , UpdateWithSMSEventForMbloxID(..)
  , GetUnreadSMSEvents(..)
  , MarkSMSEventAsRead(..)
  ) where

import Control.Monad.Catch
import Data.Int

import DB
import KontraPrelude
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

smsNotificationChannel :: Channel
smsNotificationChannel = "sms"

smsSelectors :: [SQL]
smsSelectors = [
    "id"
  , "provider"
  , "originator"
  , "msisdn"
  , "body"
  , "data"
  , "attempts"
  ]

smsFetcher :: (ShortMessageID, SMSProvider, String, String, String, String, Int32) -> ShortMessage
smsFetcher (smsid, provider, originator, msisdn, body, sdata, attempts) = ShortMessage {
  smID         = smsid
, smProvider   = provider
, smOriginator = originator
, smMSISDN     = msisdn
, smBody       = body
, smData       = sdata
, smAttempts   = attempts
}

----------------------------------------

data CreateSMS = CreateSMS SMSProvider String String String String
instance (MonadDB m, MonadThrow m) => DBUpdate m CreateSMS ShortMessageID where
  update (CreateSMS provider originator msisdn body sdata) = do
    runQuery_ . sqlInsert "smses" $ do
      sqlSet "provider" provider
      sqlSet "originator" originator
      sqlSet "msisdn" msisdn
      sqlSet "body" body
      sqlSet "run_at" unixEpoch
      sqlSet "data" sdata
      sqlResult "id"
    mid <- fetchOne runIdentity
    notify smsNotificationChannel ""
    return mid

data CleanSMSesOlderThanDays = CleanSMSesOlderThanDays Int
instance (MonadDB m, MonadTime m) => DBUpdate m CleanSMSesOlderThanDays Int where
  update (CleanSMSesOlderThanDays days) = do
    past <- (days `daysBefore`) <$> currentTime
    runQuery . sqlDelete "smses" $ do
      sqlWhere $ "finished_at <=" <?> past

data UpdateSMSWithTeliaID = UpdateSMSWithTeliaID ShortMessageID String
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateSMSWithTeliaID Bool where
  update (UpdateSMSWithTeliaID mid tid) = do
    runQuery01 . sqlUpdate "smses" $ do
      sqlSet "telia_id" tid
      sqlWhereEq "id" mid

data UpdateSMSWithMbloxID = UpdateSMSWithMbloxID ShortMessageID String
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateSMSWithMbloxID Bool where
  update (UpdateSMSWithMbloxID mid mlxid) = do
    runQuery01 . sqlUpdate "smses" $ do
      sqlSet "mblox_id" mlxid
      sqlWhereEq "id" mid

data UpdateWithSMSEvent = UpdateWithSMSEvent ShortMessageID SMSEvent
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateWithSMSEvent Bool where
  update (UpdateWithSMSEvent mid ev) = do
    runQuery01 . sqlInsert "sms_events" $ do
      sqlSet "sms_id" mid
      sqlSet "event" ev

data UpdateWithSMSEventForTeliaID = UpdateWithSMSEventForTeliaID String SMSEvent
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateWithSMSEventForTeliaID Bool where
  update (UpdateWithSMSEventForTeliaID tid ev) = do
    runQuery_ . sqlSelect "smses" $ do
      sqlWhereEq "telia_id" tid
      sqlResult "id"
    (mid :: ShortMessageID) <- fetchOne runIdentity
    runQuery01 . sqlInsert "sms_events" $ do
      sqlSet "sms_id" mid
      sqlSet "event" ev

data UpdateWithSMSEventForMbloxID = UpdateWithSMSEventForMbloxID String SMSEvent
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateWithSMSEventForMbloxID Bool where
  update (UpdateWithSMSEventForMbloxID mlxid ev) = do
    runQuery_ . sqlSelect "smses" $ do
      sqlWhereEq "mblox_id" mlxid
      sqlResult "id"
    (mid :: ShortMessageID) <- fetchOne runIdentity
    runQuery01 . sqlInsert "sms_events" $ do
      sqlSet "sms_id" mid
      sqlSet "event" ev

data GetUnreadSMSEvents = GetUnreadSMSEvents
instance MonadDB m => DBQuery m GetUnreadSMSEvents [(SMSEventID, ShortMessageID, SMSEvent, String, String)] where
  query GetUnreadSMSEvents = do
    runQuery_ . sqlSelect "sms_events" $ do
      sqlJoinOn "smses" "smses.id = sms_events.sms_id"

      sqlResult "sms_events.id"
      sqlResult "sms_events.sms_id"
      sqlResult "sms_events.event"
      sqlResult "smses.data"
      sqlResult "smses.msisdn"

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
