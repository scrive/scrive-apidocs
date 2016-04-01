module Main (main) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Happstack.Server hiding (waitForTermination)
import Happstack.StaticRouting
import Log
import Network.Curl
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.IO
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import qualified Data.Text as T

import AppConf
import AppControl
import AppDBTables
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Company.Model
import Configuration
import Crypto.RNG
import DB
import DB.Checks
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
import qualified Doc.RenderedPages as RenderedPages
import qualified HostClock.Model as HC
import qualified MemCache
import qualified Version

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
  (pool, usedConns) <- liftBase . createPoolSource $ connSettings kontraComposites
  lr@LogRunner{..} <- mkLogRunner (Just usedConns) "kontrakcja" $ logConfig appConf
  withLoggerWait $ do
    logInfo "Starting kontrakcja-server" $ object [
        "version" .= Version.versionID
      ]
    checkExecutables

    withPostgreSQL (simpleSource $ connSettings []) $ do
      checkDatabase (logInfo_ . T.pack) kontraDomains kontraTables
      dbUpdate $ SetMainDomainURL $ mainDomainUrl appConf

    appGlobals <- do
      templates <- liftBase (newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates)
      filecache <- MemCache.new BS.length 200000000
      lesscache <- MemCache.new BSL8.length 50000000
      brandedimagescache <- MemCache.new BSL8.length 50000000
      docs <- MemCache.new RenderedPages.pagesCount 10000
      rng <- newCryptoRNGState
      return AppGlobals {
          templates = templates
        , filecache = filecache
        , lesscache = lesscache
        , brandedimagescache = brandedimagescache
        , docscache = docs
        , cryptorng = rng
        , connsource = pool
        , logrunner = lr
        }

    startSystem lr appGlobals appConf

startSystem :: LogRunner -> AppGlobals -> AppConf -> MainM ()
startSystem LogRunner{..} appGlobals appConf = E.bracket startServer stopServer waitForTerm
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
        mapReqHandlerT withLogger $ appHandler routes appConf appGlobals
    stopServer = killThread
    waitForTerm _ = do
      withPostgreSQL (connsource appGlobals $ maxConnectionTracker withLogger) . runCryptoRNGT (cryptorng appGlobals) $ do
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
