module Main where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Happstack.Server hiding (waitForTermination)
import Happstack.StaticRouting
import Network.Curl
import System.IO
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL8

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
import Log
import Log.Configuration
import RoutingTable
import Templates
import User.Email
import User.Model
import Utils.Default
import Utils.IO
import Utils.Network
import qualified Doc.RenderedPages as RenderedPages
import qualified MemCache
import qualified Version

type MainM = LogT IO

main :: IO ()
main = withCurlDo $ do
  appConf <- readConfig putStrLn "kontrakcja.conf"
  lr@LogRunner{..} <- mkLogRunner "kontrakcja" $ logConfig appConf
  withLoggerWait $ do
    logInfo_ $ "Starting kontrakcja-server build " ++ Version.versionID
    checkExecutables

    let connSettings = pgConnSettings $ dbConfig appConf
    withPostgreSQL (simpleSource $ connSettings []) $
      checkDatabase logInfo_ kontraDomains kontraTables

    appGlobals <- do
      templates <- liftBase (newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates)
      filecache <- MemCache.new BS.length 200000000
      lesscache <- MemCache.new BSL8.length 50000000
      brandedimagescache <- MemCache.new BSL8.length 50000000
      docs <- MemCache.new RenderedPages.pagesCount 3000
      rng <- newCryptoRNGState
      connpool <- liftBase . createPoolSource (liftBase . withLogger . logTrace_) $ connSettings kontraComposites
      return AppGlobals {
          templates = templates
        , filecache = filecache
        , lesscache = lesscache
        , brandedimagescache = brandedimagescache
        , docscache = docs
        , cryptorng = rng
        , connsource = connpool
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
                    logInfo_ e
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
      withPostgreSQL (connsource appGlobals) . runCryptoRNGT (cryptorng appGlobals) $ do
        initDatabaseEntries $ initialUsers appConf
      liftBase $ waitForTermination
      logInfo_ $ "Termination request received"

initDatabaseEntries :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m)
                    => [(Email, String)] -> m ()
initDatabaseEntries = mapM_ $ \(email, passwordstring) -> do
  -- create initial database entries
  passwd <- createPassword passwordstring
  maybeuser <- dbQuery $ GetUserByEmail email
  case maybeuser of
    Nothing -> do
      bd <- dbQuery $ GetMainBrandedDomain
      company <- dbUpdate $ CreateCompany
      _ <- dbUpdate $ AddUser ("", "") (unEmail email) (Just passwd) (companyid company,True) defaultValue (bdid bd)
      return ()
    Just _ -> return () -- user exist, do not add it
