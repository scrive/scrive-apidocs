module AppDBMain where

import Log
import System.IO

import AppConf
import AppDBMigrations
import AppDBTables
import Configuration
import DB
import DB.Checks
import DB.PostgreSQL
import DB.SQLFunction
import KontraPrelude
import Log.Configuration

main :: IO ()
main = do
  appConf <- readConfig putStrLn "kontrakcja.conf"
  LogRunner{..} <- mkLogRunner "kontrakcja-migrate" $ logConfig appConf
  withLoggerWait $ do
    -- composite types are not available in migrations
    let connSource = simpleSource $ pgConnSettings (dbConfig appConf) []
    withPostgreSQL connSource $ do
      migrateDatabase logInfo_ kontraExtensions kontraDomains kontraTables kontraMigrations
      defineComposites kontraComposites
      defineFunctions kontraFunctions
