module AppDBMain (
   main
  ) where

import Control.Monad.Base
import System.IO

import AppConf
import AppDBMigrations
import AppDBTables
import Configuration
import DB
import DB.Checks
import DB.PostgreSQL
import DB.SQLFunction
import qualified Log

main :: IO ()
main = Log.withLogger $ do
  liftBase $ hSetEncoding stdout utf8
  liftBase $ hSetEncoding stderr utf8

  appConf <- do
    readConfig (liftBase . putStrLn) "kontrakcja.conf"

  -- composite types are not available in migrations
  let connSource = defaultSource $ pgConnSettings (dbConfig appConf) []
  withPostgreSQL connSource $ do
    migrateDatabase (liftBase . putStrLn) kontraDomains kontraTables kontraMigrations
    defineComposites kontraComposites
    defineFunctions kontraFunctions
