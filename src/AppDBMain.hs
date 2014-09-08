module AppDBMain (
   main
  ) where

import Control.Monad.IO.Class
import System.IO
import AppConf
import Configuration
import DB
import DB.Checks
import DB.PostgreSQL
import DB.SQLFunction

import AppDBTables
import AppDBMigrations

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  appConf <- do
    readConfig putStrLn "kontrakcja.conf"

  let connSource = defaultSource . pgConnSettings $ dbConfig appConf
  withPostgreSQL connSource $ do
    migrateDatabase (liftIO . putStrLn) kontraTables kontraMigrations
    defineMany kontraFunctions
