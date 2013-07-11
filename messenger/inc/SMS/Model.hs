module SMS.Model where

import SMS.Data

import DB
import DB.SQL2
import MinutesTime
import OurPrelude
import Control.Monad.State.Lazy

sqlSelectSMSes :: State SqlSelect () -> SqlSelect
sqlSelectSMSes refine = sqlSelect "smses" $ do
                    sqlResult "smses.id"
                    sqlResult "smses.originator"
                    sqlResult "smses.msisdn"
                    sqlResult "smses.body"
                    sqlResult "smses.data"
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
    decoder acc smsid originator msisdn body sdata
     = ShortMessage {
         smID         = smsid
       , smOriginator = originator
       , smMSISDN     = msisdn
       , smBody       = body
       , smData       = sdata
       } : acc

data CreateSMS = CreateSMS String String String String MinutesTime
instance MonadDB m => DBUpdate m CreateSMS ShortMessageID where
  update (CreateSMS originator msisdn body sdata to_be_sent) =
    $fromJust `fmap` insertSMS originator msisdn body sdata to_be_sent

insertSMS :: MonadDB m => String -> String -> String -> String -> MinutesTime -> m (Maybe ShortMessageID)
insertSMS originator msisdn body sdata to_be_sent =
  getOne $ sqlInsert "smses" $ do
    sqlSet "originator" originator
    sqlSet "msisdn" msisdn
    sqlSet "body" body
    sqlSet "to_be_sent" to_be_sent
    sqlSet "data" sdata
    sqlResult "id"

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

data UpdateWithSMSEvent = UpdateWithSMSEvent ShortMessageID SMSEvent
instance MonadDB m => DBUpdate m UpdateWithSMSEvent Bool where
  update (UpdateWithSMSEvent mid ev) = do
    kRun01 $ SQL "INSERT INTO sms_events (sms_id, event)  (SELECT id, ? FROM smses WHERE id = ?);"
             [toSql ev, toSql mid]


data GetUnreadSMSEvents = GetUnreadSMSEvents
instance MonadDB m => DBQuery m GetUnreadSMSEvents [(SMSEventID, ShortMessageID, SMSEvent, String)] where
  query GetUnreadSMSEvents = do
    kRun_ $ sqlSelect "sms_events" $ do
      sqlJoinOn "smses" "smses.id = sms_events.sms_id"

      sqlResult "sms_events.id"
      sqlResult "sms_events.sms_id"
      sqlResult "sms_events.event"
      sqlResult "smses.data"

      sqlWhere "sms_events.event_read IS NULL"
      sqlOrderBy "sms_events.id DESC"
    kFold fetchEvents []
    where
      fetchEvents acc eid smsid event sdata = (eid, smsid, event, sdata) : acc

data DeleteSMS = DeleteSMS ShortMessageID
instance MonadDB m => DBUpdate m DeleteSMS Bool where
  update (DeleteSMS smsid) = do
    kRun01 $ sqlDelete "smses" $ do
      sqlWhereEq "id" smsid

data MarkSMSEventAsRead = MarkSMSEventAsRead SMSEventID
instance MonadDB m => DBUpdate m MarkSMSEventAsRead Bool where
  update (MarkSMSEventAsRead eid) =
    kRun01 $ sqlUpdate "sms_events" $ do
      sqlSetCmd "event_read" "now()"
      sqlWhereEq "id" eid
