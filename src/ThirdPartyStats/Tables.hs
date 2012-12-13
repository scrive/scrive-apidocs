-- | Queue table for stowing stats to be sent to a third party until someone
--   has the time to deal with them.
module ThirdPartyStats.Tables where
import DB

-- | The table used to queue up async events.
--   Expected usage is basically write record, batch read records, batch
--   delete records in fairly rapid sequence. This, coupled with the
--   expectation that the table will never grow to any significant size
--   makes the omission of any keys seem the more performant choice.
tableAsyncEventQueue :: Table
tableAsyncEventQueue = tblTable {
  tblName = "async_event_queue"
  , tblVersion = 0
  , tblCreateOrValidate = \desc -> case desc of
      [("sequence_number", SqlColDesc { colType       = SqlBigIntT
                                      , colNullable   = Just False}),
       ("json",            SqlColDesc { colType       = SqlVarCharT
                                      , colNullable   = Just False})] ->
        return TVRvalid
      [] -> do
        kRunRaw $ "CREATE TABLE async_event_queue ("
          <> "  sequence_number SERIAL      NOT NULL"
          <> ", json            TEXT        NOT NULL"
          <> ")"
        return TVRcreated
      _ -> return TVRinvalid
  , tblForeignKeys = []
  }
