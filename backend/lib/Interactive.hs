module Interactive where

-- Use this module for interactively trying out server code and for utility scripts

import Control.Concurrent
import Log
import System.IO
import Text.StringTemplates.Templates (TemplatesT)
import qualified Data.ByteString.Char8 as BS

import AppConf
import AppControl
import AppDBTables
import Configuration
import Crypto.RNG
import DB
import DB.PostgreSQL
import KontraPrelude
import Log.Configuration
import Templates
import User.Lang
import qualified Amazon as AWS
import qualified Doc.RenderedPages as RenderedPages
import qualified MemCache

run :: TemplatesT (AWS.AmazonMonadT (CryptoRNGT (DBT (LogT IO)))) a -> IO a
run m = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  appConf <- readConfig putStrLn "kontrakcja.conf"

  rng <- newCryptoRNGState
  logRunner <- mkLogRunner "kontrakcja" (logConfig appConf) rng

  let connSettings = pgConnSettings $ dbConfig appConf
  globalTemplates <- readGlobalTemplates
  withLogger logRunner $ \runLogger -> do
    appGlobals <- do
      templates <- newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates
      filecache <- MemCache.new BS.length 50000000
      docs <- MemCache.new RenderedPages.pagesCount 1000
      connpool <- createPoolSource $ connSettings kontraComposites
      return AppGlobals
        { templates = templates
        , filecache = filecache
        , docscache = docs
        , cryptorng = rng
        , connsource = connpool
        , mrediscache = Nothing
        , runlogger = runLogger
        }

    ConnectionSource pool <- ($ maxConnectionTracker)
      `liftM` (createPoolSource $ connSettings kontraComposites)

    runLogger $ withPostgreSQL pool . runCryptoRNGT (cryptorng appGlobals) .
      AWS.runAmazonMonadT (AWS.AmazonConfig (amazonConfig appConf)
                           (filecache appGlobals) Nothing)
      $ runTemplatesT (LANG_EN, globalTemplates) m
