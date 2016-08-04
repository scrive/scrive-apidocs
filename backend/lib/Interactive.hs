module Interactive where

-- Use this module for interactively trying out server code and for utility scripts

import Control.Concurrent
import Log
import Log.Configuration
import System.IO
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL8

import AppDBTables
import AppConf
import AppControl
import Configuration
import Crypto.RNG
import DB
import User.Lang
import DB.PostgreSQL
import KontraPrelude
import Templates
import qualified Amazon as AWS
import qualified Doc.RenderedPages as RenderedPages
import Text.StringTemplates.Templates (TemplatesT)
import qualified MemCache

run :: TemplatesT (AWS.AmazonMonadT (CryptoRNGT (DBT (LogT IO)))) a -> IO a
run m = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  appConf <- readConfig putStrLn "kontrakcja.conf"

  rng <- newCryptoRNGState
  lr@LogRunner{..} <- mkLogRunner "kontrakcja" (logConfig appConf) rng

  let connSettings = pgConnSettings $ dbConfig appConf
  globalTemplates <- readGlobalTemplates
  appGlobals <- do
    templates <- newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates
    filecache <- MemCache.new BS.length 50000000
    lesscache <- MemCache.new BSL8.length 50000000
    brandedimagescache <- MemCache.new BSL8.length 50000000
    docs <- MemCache.new RenderedPages.pagesCount 1000
    connpool <- createPoolSource $ connSettings kontraComposites
    return AppGlobals {
        templates = templates
      , filecache = filecache
      , docscache = docs
      , cryptorng = rng
      , connsource = connpool
      , mrediscache = Nothing
      , lesscache = lesscache
      , brandedimagescache = brandedimagescache
      , logrunner = lr
      }

  ConnectionSource pool <- ($ maxConnectionTracker) `liftM` (createPoolSource $ connSettings kontraComposites)

  withLoggerWait $ withPostgreSQL pool . runCryptoRNGT (cryptorng appGlobals) .
   AWS.runAmazonMonadT (AWS.AmazonConfig (amazonConfig appConf) (filecache appGlobals) Nothing) $ runTemplatesT (LANG_EN, globalTemplates) m
