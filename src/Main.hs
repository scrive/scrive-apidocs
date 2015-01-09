module Main where

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Happstack.Server hiding (waitForTermination)
import Happstack.StaticRouting
import Network.Curl
import System.IO
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS

import AppConf
import AppControl
import AppDBTables
import Company.Model
import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.PostgreSQL
import RoutingTable
import Templates
import User.Email
import User.Model
import Utils.Default
import Utils.IO
import Utils.Network
import qualified Doc.RenderedPages as RenderedPages
import qualified Log
import qualified MemCache
import qualified Version

main :: IO ()
main = withCurlDo . Log.withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  liftBase $ hSetEncoding stdout utf8
  liftBase $ hSetEncoding stderr utf8

  Log.mixlog_ $ "Starting kontrakcja-server build " ++ Version.versionID

  appConf <- do
    readConfig Log.mixlog_ "kontrakcja.conf"

  checkExecutables

  let connSettings = pgConnSettings $ dbConfig appConf
  withPostgreSQL (defaultSource $ connSettings []) $
    checkDatabase Log.mixlog_ kontraTables

  appGlobals <- do
    templates <- liftBase (newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates)
    filecache <- MemCache.new BS.length 50000000
    docs <- MemCache.new RenderedPages.pagesCount 1000
    rng <- newCryptoRNGState
    connpool <- liftBase . createPoolSource $ connSettings kontraComposites
    return AppGlobals {
        templates = templates
      , filecache = filecache
      , docscache = docs
      , cryptorng = rng
      , connsource = connpool
      }

  startSystem appGlobals appConf

startSystem :: AppGlobals -> AppConf -> Log.LogT IO ()
startSystem appGlobals appConf = E.bracket startServer stopServer waitForTerm
  where
    startServer :: Log.LogT IO ThreadId
    startServer = do
      let (iface,port) = httpBindAddress appConf
      listensocket <- liftBase $ listenOn (htonl iface) (fromIntegral port)
      routes <- case compile $ staticRoutes (production appConf) of
                  Left e -> do
                    Log.mixlog_ e
                    error "static routing"
                  Right r -> return r
      let conf = nullConf {
            port = fromIntegral port
          , timeout = 120
          , logAccess = Nothing
          }

      fork $ liftBase $ simpleHTTPWithSocket listensocket conf (mapServerPartT Log.withLogger (appHandler routes appConf appGlobals))
    stopServer = killThread
    waitForTerm _ = do
      withPostgreSQL (connsource appGlobals) . runCryptoRNGT (cryptorng appGlobals) $ do
        initDatabaseEntries $ initialUsers appConf
      liftBase $ waitForTermination
      Log.mixlog_ $ "Termination request received"

initDatabaseEntries :: (CryptoRNG m, MonadDB m, MonadThrow m) => [(Email, String)] -> m ()
initDatabaseEntries = mapM_ $ \(email, passwordstring) -> do
  -- create initial database entries
  passwd <- createPassword passwordstring
  maybeuser <- dbQuery $ GetUserByEmail email
  case maybeuser of
    Nothing -> do
      company <- dbUpdate $ CreateCompany
      _ <- dbUpdate $ AddUser ("", "") (unEmail email) (Just passwd) (companyid company,True) defaultValue Nothing
      return ()
    Just _ -> return () -- user exist, do not add it
