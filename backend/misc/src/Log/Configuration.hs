module Log.Configuration (
    module Log.Data
  , LogConfig(..)
  , LoggerDef(..)
  , LogRunner(..)
  , mkLogRunner
  , runWithLogRunner
  ) where

import Data.Default
import Data.List.NonEmpty (fromList)
import Data.Semigroup
import Data.Text (Text)
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Checks
import Log.Backend.ElasticSearch
import Log.Backend.PostgreSQL
import Log.Backend.StandardOutput
import Log.Data
import Log.Internal.Logger
import Log.Monad

import Crypto.RNG
import DB.PostgreSQL
import KontraPrelude hiding ((<>))
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
        <$> fieldDefBy "configuration" defaultElasticSearchConfig
            (\(ElasticSearch es) -> es)
            "ElasticSearch configuration, defaults to localhost:9200/logs/log"
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
  -- | Run an IO action with a newly-allocated logger, ensuring that
  -- the logger shuts down properly on exit, even in the presence of
  -- exceptions. Normally should only be used in the 'main' function
  -- of the application.
  withLogger :: forall r . (Logger -> IO r) -> IO r,

  -- | Run a 'LogT' action with a logger previously allocated by
  -- 'withLogger'. Doesn't imply any costly synchronisation, making it
  -- appropriate for frequent use (e.g. on every HTTP request).
  runLogger  :: forall m r . Logger -> LogT m r -> m r
}

-- | 'withLogger' and 'runLogger' rolled into one. Useful when you
-- only have a single top-level 'runLogger' call.
runWithLogRunner :: LogRunner -> LogT IO r -> IO r
runWithLogRunner LogRunner{..} act =
  withLogger $ \logger -> runLogger logger act

newtype WithLoggerFun = WithLoggerFun {
  withLoggerFun :: forall r . (Logger -> IO r) -> IO r
}

instance Semigroup WithLoggerFun where
  (WithLoggerFun with0) <> (WithLoggerFun with1) = WithLoggerFun $
    \f -> with0 (\logger0 -> with1 (\logger1 -> f $ logger0 <> logger1))

mkLogRunner :: Text -> LogConfig -> CryptoRNGState -> IO LogRunner
mkLogRunner component LogConfig{..} rng = do
  withLoggerFuns <- mapM toWithLoggerFun lcLoggers
  let loggerFun = sconcat . fromList $ withLoggerFuns
  return LogRunner {
    withLogger = \act -> withLoggerFun loggerFun $ act,
    runLogger  = \logger act -> run logger act
    }
  where
    toWithLoggerFun :: LoggerDef -> IO WithLoggerFun
    toWithLoggerFun StandardOutput     =
      return WithLoggerFun {
        withLoggerFun = \act -> withSimpleStdOutLogger act
        }
    toWithLoggerFun (ElasticSearch ec) =
      return WithLoggerFun {
        withLoggerFun = \act -> withElasticSearchLogger ec
                                (runCryptoRNGT rng boundedIntegralRandom) act
        }
    toWithLoggerFun (PostgreSQL ci)    = do
      ConnectionSource pool <- poolSource def { csConnInfo = ci} 1 10 1
      withSimpleStdOutLogger $ \logger ->
        withPostgreSQL pool $ run logger $ do
          migrateDatabase [] [] [] logsTables logsMigrations
      return WithLoggerFun {
        withLoggerFun = \act -> withPgLogger pool act
        }

    run :: Logger -> LogT m a -> m a
    run = runLogT (component <> "-" <> lcSuffix)
