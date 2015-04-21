module Log.Backend.PostgreSQL (pgLogger) where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.List.Split
import Database.PostgreSQL.PQTypes

import KontraPrelude
import Log.Data
import Log.Logger

pgLogger :: ConnectionSource -> IO Logger
pgLogger cs = mkBulkLogger "PostgreSQL" $ mapM_ serialize . chunksOf 1000
  where
    serialize :: [LogMessage] -> IO ()
    serialize msgs = runDBT cs ts (runSQL_ $ "INSERT INTO logs (insertion_time, insertion_order, time, level, component, message, data) VALUES" <+> mintercalate ", " (map sqlifyMessage $ zip [1..] msgs)) `catches` [
        Handler $ \(e::AsyncException) -> throwIO e
      , Handler $ \(e::SomeException) -> do
        putStrLn $ "PostgreSQL: couldn't serialize logs: " ++ show e ++ ", retrying in 10 seconds"
        threadDelay $ 10 * 1000000
        serialize msgs
      ]

    sqlifyMessage :: (Int, LogMessage) -> SQL
    sqlifyMessage (n, LogMessage{..}) = "("
             <+> "now()"
      <> "," <?> n
      <> "," <?> lmTime
      <> "," <?> sqlifyLevel lmLevel
      <> "," <?> lmComponent
      <> "," <?> lmMessage
      <> "," <?> toStrict (encode lmData) <> "::jsonb"
      <> ")"

    sqlifyLevel :: LogLevel -> ByteString
    sqlifyLevel LogError = "error"
    sqlifyLevel LogInfo  = "info"
    sqlifyLevel LogTrace = "trace"

    ts :: TransactionSettings
    ts = def {
      tsAutoTransaction = False
    }
