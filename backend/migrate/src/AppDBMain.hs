module AppDBMain where

import Log
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.IO
import qualified Data.Text as T

import AppDBConfig
import AppDBMigrations
import AppDBTables
import Configuration
import Crypto.RNG
import DB
import DB.Checks
import DB.PostgreSQL
import DB.SQLFunction
import KontraPrelude
import Log.Configuration

data CmdConf = CmdConf {
  config :: String
} deriving (Data, Typeable)

cmdConf :: String -> CmdConf
cmdConf progName = CmdConf {
  config = configFile
        &= help ("Configuration file (default: " ++ configFile ++ ")")
        &= typ "FILE"
} &= program progName
  where
    configFile = "kontrakcja.conf"

----------------------------------------

main :: IO ()
main = do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  AppDBConf{..} <- readConfig putStrLn config
  rng <- newCryptoRNGState
  LogRunner{..} <- mkLogRunner "kontrakcja-migrate" logConfig rng
  withLoggerWait $ do
    -- composite types are not available in migrations
    let connSource = simpleSource $ pgConnSettings dbConfig []
    withPostgreSQL (unConnectionSource connSource) $ do
      migrateDatabase (logInfo_ . T.pack) kontraExtensions kontraDomains kontraTables kontraMigrations
      defineComposites kontraComposites
      defineFunctions kontraFunctions
