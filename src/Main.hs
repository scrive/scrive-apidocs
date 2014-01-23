module Main where

import Control.Concurrent
import Control.Monad
import Happstack.Server hiding (waitForTermination)
import Happstack.StaticRouting
import System.Environment
import System.IO
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS

import AppConf
import AppControl
import Configuration
import Crypto.RNG
import DB
import DB.PostgreSQL
import Utils.Default
import Utils.IO
import Utils.Network
import RoutingTable
import Templates
import User.Model
import Company.Model
import AppDBTables (kontraTables)
import DB.Checks
import qualified Log
import qualified MemCache
import qualified Version
import qualified Doc.JpegPages as JpegPages



main :: IO ()
main = Log.withLogger $ do
  -- progname effects where state is stored and what the logfile is named
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  Log.mixlog_ $ "Starting kontrakcja-server build " ++ Version.versionID

  appConf <- do
    appname <- getProgName
    args <- getArgs
    readConfig Log.mixlog_ appname args "kontrakcja.conf"

  checkExecutables

  withPostgreSQL (dbConfig appConf) $
    checkDatabase Log.mixlog_ kontraTables

  appGlobals <- do
    templates <- newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates
    filecache <- MemCache.new BS.length 50000000
    docs <- MemCache.new JpegPages.pagesCount 1000
    rng <- newCryptoRNGState
    return AppGlobals {
        templates = templates
      , filecache = filecache
      , docscache = docs
      , cryptorng = rng
      }

  startSystem appGlobals appConf

startSystem :: AppGlobals -> AppConf -> IO ()
startSystem appGlobals appConf = E.bracket startServer stopServer waitForTerm
  where
    startServer = do
      let (iface,port) = httpBindAddress appConf
      listensocket <- listenOn (htonl iface) (fromIntegral port)
      let (routes,overlaps) = compile staticRoutes
      maybe (return ()) Log.mixlog_ overlaps
      let conf = nullConf {
            port = fromIntegral port
          , timeout = 120
          }
      forkIO . simpleHTTPWithSocket listensocket conf $ appHandler routes appConf appGlobals
    stopServer = killThread
    waitForTerm _ = do
      withPostgreSQL (dbConfig appConf) . runCryptoRNGT (cryptorng appGlobals) $
        initDatabaseEntries $ initialUsers appConf
      waitForTermination
      Log.mixlog_ $ "Termination request received"

initDatabaseEntries :: (CryptoRNG m, MonadDB m) => [(Email, String)] -> m ()
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
