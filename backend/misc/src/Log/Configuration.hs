{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Log.Configuration (
    module Log.Data
  , LogConfig(..)
  , LoggerDef(..)
  , LogRunner(..)
  , mkLogRunner
  , runWithLogRunner
  ) where

import Crypto.RNG
import Data.Either (lefts, rights)
import Data.Functor.Invariant (invmap)
import Data.List.NonEmpty (fromList)
import Data.Semigroup
import Data.Text (Text)
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Database.PostgreSQL.PQTypes.Checks
import Log.Backend.ElasticSearch.V5
import Log.Backend.ElasticSearch.V5.Internal
import Log.Backend.PostgreSQL
import Log.Backend.StandardOutput
import Log.Data
import Log.Internal.Logger hiding (withLogger)
import Log.Monad
import Prelude hiding ((<>))

import DB.PostgreSQL
import Log.Migrations
import Log.Tables
import Utils.TH

data LogConfig = LogConfig
  { lcSuffix  :: !Text
  , lcLoggers :: ![LoggerDef]
  } deriving (Eq, Show)

instance Unjson LogConfig where
  unjsonDef =
    objectOf $ LogConfig <$> field "suffix" lcSuffix "Suffix of a component" <*> field
      "loggers"
      lcLoggers
      "List of loggers"

data LoggerDef
  = StandardOutput
  | ElasticSearch ElasticSearchConfig
  | PostgreSQL Text
  deriving (Eq, Show)

instance Unjson LoggerDef where
  unjsonDef = disjointUnionOf
    "logger"
    [ ("stdout", (== StandardOutput), pure StandardOutput)
    , ( "elasticsearch"
      , $(isConstr 'ElasticSearch)
      , ElasticSearch
        <$> fieldDefBy
              "configuration"
              defaultElasticSearchConfig
              (\(ElasticSearch es) -> es)
              (  "ElasticSearch configuration, "
              <> "defaults to http://localhost:9200/logs/log"
              )
              esUnjsonConfig
      )
    , ( "postgresql"
      , $(isConstr 'PostgreSQL)
      , PostgreSQL
        <$> field "database" (\(PostgreSQL ci) -> ci) "Database connection string"
      )
    ]
    where

      esDefCfg :: ElasticSearchConfig
      esDefCfg = defaultElasticSearchConfig

      -- allows keys in any order; all have defaults
      esUnjsonConfig =
        objectOf
          $    pure esDefCfg
          <**> (    fieldDefBy "server"
                               (esServer esDefCfg)
                               esServer
                               "Server (host:port)"
                               unjsonDef
               <**> pure (\s esCfg -> esCfg { esServer = s })
               )
          <**> (    fieldDefBy "index" (esIndex esDefCfg) esIndex "Index" unjsonDef
               <**> pure (\i esCfg -> esCfg { esIndex = i })
               )
          <**> (    fieldDefBy "shards"
                               (esShardCount esDefCfg)
                               esShardCount
                               "Shard count"
                               unjsonDef
               <**> pure (\s esCfg -> esCfg { esShardCount = s })
               )
          <**> (    fieldDefBy "replicas"
                               (esReplicaCount esDefCfg)
                               esReplicaCount
                               "Replica count"
                               unjsonDef
               <**> pure (\r esCfg -> esCfg { esReplicaCount = r })
               )
          <**> (fieldDefBy "mapping" (esMapping esDefCfg) esMapping "Mapping" unjsonDef
               <**> pure (\m esCfg -> esCfg { esMapping = m })
               )
          <**> (    fieldOptBy "login"
                               esLogin
                               "Login info, optional, default: empty"
                               unjsonESLogin
               <**> pure (\ml esCfg -> esCfg { esLogin = ml })
               )
          <**> (    fieldDefBy "loginInsecure"
                               (esLoginInsecure esDefCfg)
                               esLoginInsecure
                               "Allow basic authentication over non-TLS connections."
                               unjsonDef
               <**> pure (\l esCfg -> esCfg { esLoginInsecure = l })
               )

      unjsonESLogin :: UnjsonDef (EsUsername, EsPassword)
      unjsonESLogin = objectOf $ (,) <$> field "username" fst "User name" <*> field
        "password"
        snd
        "Password"

instance Unjson EsUsername where
  unjsonDef = invmap EsUsername esUsername unjsonAeson

instance Unjson EsPassword where
  unjsonDef = invmap EsPassword esPassword unjsonAeson

----------------------------------------

newtype LogRunner = LogRunner {
  -- | Run an IO action with a newly-allocated logger, ensuring that
  -- the logger shuts down properly on exit, even in the presence of
  -- exceptions.
  --
  -- The IO action gets passed a 'runLogger' function, which should be
  -- used to run any nested 'LogT' actions. Unlike 'withLogger', it
  -- doesn't imply any costly synchronisation, and therefore is
  -- appropriate for frequent use (e.g. on every HTTP request).
  --
  -- Normally 'withLogger' should only be used in the 'main' function
  -- of the application.
  withLogger :: forall a . ((forall m r . LogT m r -> m r) -> IO a) -> IO a
}

-- | 'withLogger' and 'runLogger' rolled into one. Useful when you
-- only have a single top-level 'runLogger' call.
runWithLogRunner :: LogRunner -> LogT IO r -> IO r
runWithLogRunner LogRunner {..} act = withLogger $ \runLogger -> runLogger act

newtype WithLoggerFun = WithLoggerFun {
  withLoggerFun :: forall r . (Logger -> IO r) -> IO r
}

instance Semigroup WithLoggerFun where
  (WithLoggerFun with0) <> (WithLoggerFun with1) =
    WithLoggerFun $ \f -> with0 (\logger0 -> with1 (\logger1 -> f $ logger0 <> logger1))

{-# ANN mkLogRunner ("HLint: ignore Avoid lambda" :: String) #-}
mkLogRunner :: Text -> LogConfig -> CryptoRNGState -> IO ([Text], LogRunner)
mkLogRunner component LogConfig {..} rng = do
  let run :: Logger -> LogT m a -> m a
      run = runLogT (component <> "-" <> lcSuffix)

  let toWithLoggerFun :: LoggerDef -> IO (Either Text WithLoggerFun)
      toWithLoggerFun StandardOutput =
        return . Right $ WithLoggerFun { withLoggerFun = withSimpleStdOutLogger }
      toWithLoggerFun (ElasticSearch ec) = checkElasticSearchConnection ec >>= \case
        Left _ ->
          return
            .  Left
            $  "ElasticSearch: unexpected error; "
            <> "is ElasticSearch server running?\n"
            -- @review-note:include the below? A bit noisy
            -- (pack . show) ex
        Right () -> return . Right $ WithLoggerFun
          { withLoggerFun = \act -> do
                              let randGen = runCryptoRNGT rng boundedIntegralRandom
                              withElasticSearchLogger ec randGen act
          }
      toWithLoggerFun (PostgreSQL ci) = do
        ConnectionSource pool <- poolSource
          defaultConnectionSettings { csConnInfo = ci }
          1
          10
          1
        withSimpleStdOutLogger $ \logger -> withPostgreSQL pool . run logger $ do
          let extrasOptions = defaultExtrasOptions
          migrateDatabase extrasOptions [] [] [] logsTables logsMigrations
        return . Right $ WithLoggerFun { withLoggerFun = withPgLogger pool }

  eWithLoggerFuns <- mapM toWithLoggerFun lcLoggers

  let withLoggerFuns = rights eWithLoggerFuns
      errorReports   = lefts eWithLoggerFuns
  when (null withLoggerFuns) $ do
    unexpectedError "List of loggers is empty; aborting."

  let loggerFun = sconcat . fromList $ withLoggerFuns

  let withLogger :: ((forall m r . LogT m r -> m r) -> IO a) -> IO a
      withLogger act = withLoggerFun loggerFun (\logger -> act $ run logger)
  return (errorReports, LogRunner { withLogger = withLogger })

-- @review-note here's what the `show`'n exception looks like
-- Produces ~
-- ElasticSearch: unexpected error; is ElasticSearch server running?
-- HttpExceptionRequest Request {
--   host                 = "es-figaro.bugthunk.net"
--   port                 = 9200
--   secure               = False
--   requestHeaders       = [("Authorization","<REDACTED>"),("Content-Type","application/json")]
--   path                 = "/_cat/indices"
--   queryString          = "?v"
--   method               = "GET"
--   proxy                = Nothing
--   rawBody              = False
--   redirectCount        = 10
--   responseTimeout      = ResponseTimeoutDefault
--   requestVersion       = HTTP/1.1
-- }
--  (ConnectionFailure Network.Socket.getAddrInfo (called with preferred socket type/protocol: AddrInfo {addrFlags = [AI_ADDRCONFIG], addrFamily = AF_UNSPEC, addrSocketType = Stream, addrProtocol = 0, addrAddress = <assumed to be undefine
-- d>, addrCanonName = <assumed to be undefined>}, host name: Just "es-figaro.bugthunk.net", service name: Just "9200"): does not exist (Name or service not known))
