-- | Queue table for stowing stats to be sent to a third party until someone
--   has the time to deal with them.
module ThirdPartyStats.Tables where

import DB

-- | The table used to queue up async events.
--   Expected usage is basically write record, batch read records, batch
--   delete records in fairly rapid sequence. This, coupled with the
--   expectation that the table will never grow to any significant size
--   makes the omission of any keys seem the more performant choice - however,
--   we need a primary key to enable no downtime updates.
tableAsyncEventQueue :: Table
tableAsyncEventQueue = tblTable
  { tblName       = "async_event_queue"
  , tblVersion    = 3
  , tblColumns = [ tblColumn { colName     = "sequence_number"
                             , colType     = BigSerialT
                             , colNullable = False
                             }
                 , tblColumn { colName = "event", colType = TextT, colNullable = False }
                 ]
  , tblPrimaryKey = pkOnColumn "sequence_number"
  }
