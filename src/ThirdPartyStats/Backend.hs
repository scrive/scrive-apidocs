-- | Backend API for logging events to be sent off to a third party.
module ThirdPartyStats.Backend where
import DB
import DB.SQL2
import ThirdPartyStats.Tables
import Text.JSON
import Kontra
import qualified Log

-- | Denotes how many events should be processed.
data NumEvents = All | NoMoreThan Integer


-- | Indicates how the processing of an event fared.
data ProcRes a
  = OK            -- ^ Processing succeeded, we're done with this event.
  | PutBack a     -- ^ Processing failed, but might succeed if we retry later.
  | Failed String -- ^ Processing failed permanently, discard event and log
                  --   the failure.


-- | Remove a number of events from the queue and process them.
asyncProcessEvents :: (Kontrakcja m, JSON a)
                   => (a -> m (ProcRes a)) -- ^ Event processing function.
                   -> NumEvents            -- ^ Max events to process.
                   -> m ()
asyncProcessEvents process numEvts = do
    (evts, lastEvt) <- fetchEvents
    mapM_ processEvent evts
    deleteEvents lastEvt
  where
    processEvent (Ok evt) = do
        result <- process evt
        case result of
          PutBack e  -> asyncLogEvent e
          Failed msg -> Log.error $ "Event processing failed because: " ++ msg
          _          -> return ()
    processEvent (Error why) = do
        Log.error $ "Unable to parse JSON from async event queue: " ++ why

    decoder (json_acc, max_seq) seqnum json =
        (decode json : json_acc, max seqnum max_seq)

    deleteEvents lastEvt = do
        runDBEnv $ do
          _ <- kRun $ sqlDelete (tblName tableAsyncEventQueue) $ do
                      sqlWhere $ SQL "sequence_number <= ?" [toSql lastEvt]
          return ()

    fetchEvents = do
        runDBEnv $ do
            _ <- kRun_ $ sqlSelect (tblName tableAsyncEventQueue) $ do
                sqlResult "sequence_number"
                sqlResult "json"
                sqlOrderBy "sequence_number ASC"
                case numEvts of
                  NoMoreThan n -> sqlLimit n
                  _            -> return ()
            foldDB decoder ([], 0 :: Integer)

-- | Stuff a JSON message into the async event queue for later processing.
asyncLogEvent :: (Kontrakcja m, JSON a) => a -> m ()
asyncLogEvent evt = do
    _ <- runDBEnv $ kRun $ mkSQL INSERT tableAsyncEventQueue jsonRecord
    return ()
  where
    jsonRecord = [sql "json" $ encode (showJSON evt)]
