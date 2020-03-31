module AppDBMain where

import Crypto.RNG
import Database.PostgreSQL.PQTypes.Checks
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.FilePath ((</>))
import qualified Data.Text.IO as T

import AppDBConfig
import AppDBMigrations
import AppDBTables
import AppDir (AppPaths(..), setupAppPaths)
import Configuration
import DB
import DB.PostgreSQL
import DB.SQLFunction
import DB.SQLTrigger
import Log.Configuration

data CmdConf = CmdConf
  { config :: String
  , force :: Bool
  } deriving (Data, Typeable)

cmdConf :: FilePath -> String -> CmdConf
cmdConf workspaceRoot progName =
  CmdConf
      { config =
        configFile &= help ("Configuration file (default: " ++ configFile ++ ")") &= typ
          "FILE"
      , force  =
        False
          &= help
               "Force commit after each migration - DB will be permanently changed even if migrations will fail"
      }
    &= program progName
  where configFile = workspaceRoot </> "kontrakcja.conf"

----------------------------------------

main :: IO ()
main = do
  (AppPaths _ workspaceRoot) <- setupAppPaths

  CmdConf {..}               <- cmdArgs . cmdConf workspaceRoot =<< getProgName
  AppDBConf {..}             <- readConfig putStrLn config
  rng                        <- newCryptoRNGState
  (errs, logRunner)          <- mkLogRunner "kontrakcja-migrate" logConfig rng
  mapM_ T.putStrLn errs

  runWithLogRunner logRunner $ do
    -- composite types are not available in migrations
    let connSource    = simpleSource $ pgConnSettings dbConfig []
        extrasOptions = ExtrasOptions { eoForceCommit = force, eoEnforcePKs = True }
    withPostgreSQL (unConnectionSource connSource) $ do
      migrateDatabase extrasOptions
                      kontraExtensions
                      kontraComposites
                      kontraDomains
                      kontraTables
                      kontraMigrations
      defineFunctions kontraFunctions
      defineTriggers kontraTriggers
