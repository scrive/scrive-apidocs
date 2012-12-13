-- | Backend API for logging events to be sent off to a third party.
module ThirdPartyStats.Backend where
import DB
import ThirdPartyStats.Tables
import Text.JSON
import Kontra

-- | Stuff a JSON message into the async event queue for later processing.
asyncLogEvent :: (Kontrakcja m, JSON a) => a -> m ()
asyncLogEvent evt = do
    _ <- runDBEnv $ kRun $ mkSQL INSERT tableAsyncEventQueue jsonRecord
    return ()
  where
    jsonRecord = [sql "json" $ encode (showJSON evt)]
