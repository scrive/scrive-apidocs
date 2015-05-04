module Log.Configuration (
    module Log.Data
  , LogConfig(..)
  , LoggerDef(..)
  , LogRunner(..)
  , mkLogRunner
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Default
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Unjson
import Database.PostgreSQL.PQTypes

import KontraPrelude
import Log.Backend.PostgreSQL
import Log.Backend.StandardOutput
import Log.Data
import Log.Monad
import Utils.TH

import Log.Tables
import Log.Logger
import Log.Migrations
import DB.PostgreSQL
import DB.Checks

data LogConfig = LogConfig {
  lcSuffix  :: !Text
, lcLoggers :: ![LoggerDef]
} deriving (Eq, Ord, Show)

instance Default LogConfig where
  def = LogConfig {
    lcSuffix  = "dev"
  , lcLoggers = [StandardOutput]
  }

instance Unjson LogConfig where
  unjsonDef = objectOf $ LogConfig
    <$> fieldBy "suffix"
        lcSuffix
        "Suffix of a component"
        unjsonAeson
    <*> field "loggers"
        lcLoggers
        "List of loggers"

data LoggerDef
  = StandardOutput
  | PostgreSQL ByteString
  deriving (Eq, Ord, Show)

instance Unjson LoggerDef where
  unjsonDef = disjointUnionOf "logger" [
      ("stdout", (== StandardOutput), pure StandardOutput)
    , ("postgresql", $(isConstr 'PostgreSQL), PostgreSQL
        <$> fieldBy "database"
            (\(PostgreSQL ci) -> ci)
            "Database connection string"
            unjsonAeson
      )
    ]

----------------------------------------

data LogRunner = LogRunner {
  withLogger     :: forall m r. LogT m r -> m r
, withLoggerWait :: forall m r. (MonadBase IO m, MonadMask m) => LogT m r -> m r
}

mkLogRunner :: Text -> LogConfig -> IO LogRunner
mkLogRunner component LogConfig{..} = do
  logger <- fold <$> mapM defLogger lcLoggers
  let run :: LogT m r -> m r
      run = runLogT (component <> "-" <> lcSuffix) logger
  return LogRunner {
    withLogger = run
  , withLoggerWait = \m -> run m `finally` liftBase (waitForLogger logger)
  }
  where
    defLogger StandardOutput = stdoutLogger
    defLogger (PostgreSQL ci) = do
      pool <- poolSource def { csConnInfo = ci } 1 10 1
      withPostgreSQL pool $ do
        migrateDatabase (liftBase . putStrLn) [] [] logsTables logsMigrations
      pgLogger pool
