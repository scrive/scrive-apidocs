module AppDBMain where

import Crypto.RNG
import Database.PostgreSQL.PQTypes.Checks
import System.Console.CmdArgs hiding (def)
import System.Environment

import AppDBConfig
import AppDBMigrations
import AppDBTables
import Configuration
import DB
import DB.PostgreSQL
import DB.SQLFunction
import DB.SQLTrigger
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
  force =  False &= help ("Force commit after each migration - DB will be permanently changed even if migrations will fail")
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
        extrasOptions =  ExtrasOptions
                         { eoForceCommit = force
                         , eoEnforcePKs = True }
    withPostgreSQL (unConnectionSource connSource) $ do
      migrateDatabase extrasOptions kontraExtensions kontraDomains kontraTables kontraMigrations
      defineComposites kontraComposites
      defineFunctions kontraFunctions
      defineTriggers kontraTriggers
