module AppDBMain (
   main
  ) where

import DB.SQLFunction
import Control.Monad.IO.Class
import System.IO
import System.Environment
import AppConf
import Configuration
import DB.Checks
import DB.PostgreSQL

import AppDBTables
import AppDBMigrations


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  appConf <- do
    appname <- getProgName
    args <- getArgs
    readConfig (liftIO . putStrLn) appname args "kontrakcja.conf"

  withPostgreSQL (dbConfig appConf) $ do
    migrateDatabase (liftIO . putStrLn) kontraTables kontraMigrations
    defineMany kontraFunctions
