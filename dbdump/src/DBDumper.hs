module Main(main) where

import KontrakcjaServer
import AppConf
import Misc
import qualified Data.ByteString.Lazy as BSL
import Data.Functor
import Control.Monad
import System.Exit
import System.Directory
import Data.List
import Data.Binary
import System.Environment
import MinutesTime

data DBDump = DBDump {
                      pgdump :: BSL.ByteString,
                      hsdump  ::BSL.ByteString
                     } 
                     
instance Binary DBDump where
    put dbdump = put (pgdump dbdump,hsdump dbdump)
    get = uncurry DBDump <$> get 
    
main :: IO ()
main = do
    args <- getArgs
    case args of
         ["dump"] -> do
             dmp <- getDBDump
             time <- getMinutesTime
             encodeFile ("data-dump-" ++ showDateYMD time) dmp
         ["load",name] -> do
             dmp <- decodeFile name
             loadDBDump dmp
         _ -> do
             putStrLn "Usage: dbdump dump | dbdump load name_of_dump_file"    
             putStrLn "   - 'dump' will create dumfile with current happstack state and postgres database dump"    
             putStrLn "   - 'load' will replate current happstack state and postgres database with content of dump file"    
             
    return () 

happstackStoreDir = "_local/kontrakcja_state"     

getDBDump:: IO DBDump
getDBDump = do
    db <- dbConfig <$> readAppConfig
    (code,pgd,stderror) <- readProcessWithExitCode' "pg_dump" ["-Ft",db] BSL.empty
    when (code /= ExitSuccess) $
        error $ "Failed to pg_dump database \"" ++ db ++ "\" with " ++ show stderror 
    putStrLn $ "Postgres:" ++ db   
    putStrLn "Postgres dumping successfull"    
    lastCheckpoint <- find ("checkpoint" `isPrefixOf`) <$> reverse <$> sort <$> getDirectoryContents happstackStoreDir
    hsd <- case lastCheckpoint of
                   Nothing -> do
                        error "No last checkpoint found"
                   Just name -> do
                        putStrLn $ "Checkpoint file selected: " ++ name                       
                        BSL.readFile $ happstackStoreDir ++ "/" ++ name
    putStrLn "Happstack dumping successfull"    
    return $ DBDump {pgdump = pgd, hsdump =  hsd}


loadDBDump :: DBDump -> IO ()
loadDBDump dump = do
    db <- dbConfig <$> readAppConfig
    (code,pgd,stderror) <- readProcessWithExitCode' "pg_restore" ["-C","-d",db] (pgdump dump)
    when (code /= ExitSuccess) $
        error $ "Failed to pg_restore database \"" ++ db ++ "\" with " ++ show stderror 
    putStrLn "Postgress restored"            
    files <- getDirectoryContents happstackStoreDir  
    mapM_ removeFile files  
    BSL.writeFile  (happstackStoreDir ++ "/" ++  "checkpoint-0000000000") (hsdump dump)
    putStrLn "Happstack state checkpoint recreatedd"
    return ()

