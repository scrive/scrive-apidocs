module Log.Configuration (
    module Log.Data
  , LogConfig(..)
  , LoggerDef(..)
  , LogRunner(..)
  , mkLogRunner
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Default
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Log.Backend.ElasticSearch
import Log.Backend.PostgreSQL
import Log.Backend.StandardOutput
import Log.Data
import Log.Internal.Logger
import Log.Monad

import Crypto.RNG
import DB.Checks
import DB.PostgreSQL
import KontraPrelude
import Log.Migrations
import Log.Tables
import Utils.TH

data LogConfig = LogConfig {
  lcSuffix  :: !Text
, lcLoggers :: ![LoggerDef]
} deriving (Eq, Show)

instance Default LogConfig where
  def = LogConfig {
    lcSuffix  = "dev"
  , lcLoggers = [StandardOutput]
  }

instance Unjson LogConfig where
  unjsonDef = objectOf $ LogConfig
    <$> field "suffix"
        lcSuffix
        "Suffix of a component"
    <*> field "loggers"
        lcLoggers
        "List of loggers"

data LoggerDef
  = StandardOutput
  | ElasticSearch ElasticSearchConfig
  | PostgreSQL Text
  deriving (Eq, Show)

instance Unjson LoggerDef where
  unjsonDef = disjointUnionOf "logger" [
      ("stdout", (== StandardOutput), pure StandardOutput)
    , ("elasticsearch", $(isConstr 'ElasticSearch), ElasticSearch
        <$> fieldBy "configuration"
            (\(ElasticSearch es) -> es)
            "ElasticSearch configuration"
            esUnjsonConfig
      )
    , ("postgresql", $(isConstr 'PostgreSQL), PostgreSQL
        <$> field "database"
            (\(PostgreSQL ci) -> ci)
            "Database connection string"
      )
    ]
    where
      esUnjsonConfig = objectOf $ ElasticSearchConfig
        <$> field "server"
            esServer
            "Server (host:port)"
        <*> field "index"
            esIndex
            "Index"
        <*> field "mapping"
            esMapping
            "Mapping"

----------------------------------------

data LogRunner = LogRunner {
  withLogger     :: forall m r. LogT m r -> m r
, withLoggerWait :: forall m r. (MonadBase IO m, MonadMask m) => LogT m r -> m r
}

mkLogRunner :: Text -> LogConfig -> CryptoRNGState -> IO LogRunner
mkLogRunner component LogConfig{..} rng = do
  logger <- fold <$> mapM defLogger lcLoggers
  return LogRunner {
    withLogger = run logger
  , withLoggerWait = \m -> run logger m `finally` liftBase (waitForLogger logger)
  }
  where
    run :: Logger -> LogT m r -> m r
    run = runLogT (component <> "-" <> lcSuffix)

    defLogger StandardOutput = stdoutLogger
    defLogger (ElasticSearch ec) = elasticSearchLogger ec $ runCryptoRNGT rng boundedIntegralRandom
    defLogger (PostgreSQL ci) = do
      ConnectionSource pool <- poolSource def { csConnInfo = ci } 1 10 1
      withPostgreSQL pool . run simpleStdoutLogger $ do
        migrateDatabase [] [] logsTables logsMigrations
      pgLogger pool
