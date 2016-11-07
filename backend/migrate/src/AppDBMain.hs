module AppDBMain where

import Database.PostgreSQL.PQTypes.Checks
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.IO

import AppDBConfig
import AppDBMigrations
import AppDBTables
import Configuration
import Crypto.RNG
import DB
import DB.PostgreSQL
import DB.SQLFunction
import KontraPrelude
import Log.Configuration

data CmdConf = CmdConf {
  config :: String,
  force :: Bool
} deriving (Data, Typeable)

cmdConf :: String -> CmdConf
cmdConf progName = CmdConf {
  config = configFile
        &= help ("Configuration file (default: " ++ configFile ++ ")")
        &= typ "FILE",
  force =  False &= help ("Force commit after each migrationg - DB will be permanently changed even if migrations will fail")
} &= program progName
  where
    configFile = "kontrakcja.conf"

----------------------------------------

main :: IO ()
main = do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  AppDBConf{..} <- readConfig putStrLn config
  rng <- newCryptoRNGState
  logRunner <- mkLogRunner "kontrakcja-migrate" logConfig rng
  runWithLogRunner logRunner $ do
    -- composite types are not available in migrations
    let connSource = simpleSource $ pgConnSettings dbConfig []
    let migrationOptions = if (force) then [ForceCommitAfterEveryMigration] else []
    withPostgreSQL (unConnectionSource connSource) $ do
      migrateDatabase migrationOptions kontraExtensions kontraDomains kontraTables kontraMigrations
      defineComposites kontraComposites
      defineFunctions kontraFunctions
