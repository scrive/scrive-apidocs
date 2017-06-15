module Main (main) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Crypto.RNG
import Database.PostgreSQL.PQTypes.Checks
import Happstack.Server hiding (waitForTermination)
import Happstack.StaticRouting
import Log
import Network.Curl
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.IO
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Traversable as F

import AppConf
import AppControl
import AppDBTables
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Company.Model
import Configuration
import Database.Redis.Configuration
import DB
import DB.PostgreSQL
import Happstack.Server.ReqHandler
import KontraPrelude
import Log.Configuration
import RoutingTable
import Templates
import User.Email
import User.Model
import Utils.IO
import Utils.Network
import qualified HostClock.Model as HC
import qualified MemCache
import qualified VersionTH

data CmdConf = CmdConf {
  config :: String
} deriving (Data, Typeable)

cmdConf :: String -> CmdConf
cmdConf progName = CmdConf {
  config = configFile
        &= help ("Configuration file (default: " ++ configFile ++ ")")
        &= typ "FILE"
} &= program progName
  where
    configFile = "kontrakcja.conf"

----------------------------------------

type MainM = LogT IO

main :: IO ()
main = withCurlDo $ do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  appConf <- readConfig putStrLn config
  let connSettings = pgConnSettings $ dbConfig appConf
  pool <- liftBase $ createPoolSource (connSettings kontraComposites) (maxDBConnections appConf)
  rng <- newCryptoRNGState
  lr <- mkLogRunner "kontrakcja" (logConfig appConf) rng
  withLogger lr $ \runLogger -> runLogger $ do
    logInfo "Starting kontrakcja-server" $ object [
        "version" .= VersionTH.versionID
      ]
    checkExecutables

    withPostgreSQL (unConnectionSource . simpleSource $ connSettings []) $ do
      checkDatabase kontraDomains kontraTables
      dbUpdate $ SetMainDomainURL $ mainDomainUrl appConf

    appGlobals <- do
      templates <- liftBase (newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates)
      mrediscache <- F.forM (redisCacheConfig appConf) mkRedisConnection
      filecache <- MemCache.new BS.length (localFileCacheSize appConf)
      return AppGlobals {
          templates          = templates
        , mrediscache        = mrediscache
        , filecache          = filecache
        , cryptorng          = rng
        , connsource         = pool
        , runlogger          = runLogger
        }

    startSystem appGlobals appConf

startSystem :: AppGlobals -> AppConf -> MainM ()
startSystem appGlobals appConf = E.bracket startServer stopServer waitForTerm
  where
    startServer :: MainM ThreadId
    startServer = do
      let (iface,port) = httpBindAddress appConf
      listensocket <- liftBase $ listenOn (htonl iface) (fromIntegral port)
      routes <- case compile $ staticRoutes (production appConf) of
        Left e -> do
          logInfo "Error while compiling routes" $ object [
              "error" .= e
            ]
          $unexpectedErrorM "static routing"
        Right r -> return r
      let conf = nullConf {
            port = fromIntegral port
          , timeout = 120
          , logAccess = Nothing
          }

      fork . liftBase . runReqHandlerT listensocket conf $ do
        (runlogger appGlobals) $ appHandler routes appConf appGlobals
    stopServer = killThread
    waitForTerm _ = do
      withPostgreSQL (unConnectionSource $ connsource appGlobals (maxConnectionTracker $ maxDBConnections appConf)) . runCryptoRNGT (cryptorng appGlobals) $ do
        initDatabaseEntries appConf
      liftBase $ waitForTermination
      logInfo_ "Termination request received"

initDatabaseEntries :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m) => AppConf -> m ()
initDatabaseEntries appConf = do
  when (not $ production appConf) $ do
    -- Add some host_clock entries in "dev" mode if there are no valid samples
    clockErrors <- dbQuery $ HC.GetNClockErrorEstimates 10
    when (not $ HC.enoughClockErrorOffsetSamples clockErrors) $ do
      _ <- dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.001) 0.5
      _ <- dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.0015) 0.5
      return ()
  flip mapM_ (initialUsers appConf) $ \(email, passwordstring) -> do
    -- create initial database entries
    passwd <- createPassword passwordstring
    maybeuser <- dbQuery $ GetUserByEmail email
    case maybeuser of
      Nothing -> do
        bd <- dbQuery $ GetMainBrandedDomain
        company <- dbUpdate $ CreateCompany
        _ <- dbUpdate $ AddUser ("", "") (unEmail email) (Just passwd) (companyid company,True) def (bdid bd) ByAdmin
        return ()
      Just _ -> return () -- user exist, do not add it
