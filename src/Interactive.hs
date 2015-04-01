module Interactive where

-- Use this module for interactively trying out server code and for utility scripts

import Control.Concurrent
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as BS

import AppConf
import AppControl
import Configuration
import Crypto.RNG
import DB
import DB.PostgreSQL
import KontraPrelude
import Templates
import qualified Amazon as AWS
import qualified Doc.RenderedPages as RenderedPages
import qualified Log
import qualified MemCache

run :: AWS.AmazonMonadT (CryptoRNGT (DBT IO)) a -> IO a
run m = Log.withLogger $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  appConf <- do
    appname <- getProgName
    args <- getArgs
    readConfig Log.mixlog_ appname args "kontrakcja.conf"

  let connSettings = pgConnSettings $ dbConfig appConf
  appGlobals <- do
    templates <- newMVar =<< liftM2 (,) getTemplatesModTime readGlobalTemplates
    filecache <- MemCache.new BS.length 50000000
    docs <- MemCache.new RenderedPages.pagesCount 1000
    rng <- newCryptoRNGState
    connpool <- createPoolSource connSettings
    return AppGlobals {
        templates = templates
      , filecache = filecache
      , docscache = docs
      , cryptorng = rng
      , connsource = connpool
      }

  withPostgreSQL (defaultSource connSettings) . runCryptoRNGT (cryptorng appGlobals) .
   AWS.runAmazonMonadT (AWS.AmazonConfig (amazonConfig appConf) (filecache appGlobals)) $ m
