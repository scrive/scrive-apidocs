{-# LANGUAGE CPP #-}
module DBDumper(main) where

import KontrakcjaServer
import AppConf
import Misc
import qualified Data.ByteString.Lazy.UTF8 as BSL
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
                      -- Checkpoint file name , content , current content
                      hsdump  :: ((String,BSL.ByteString),BSL.ByteString)
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
             encodeFile ("backup-" ++ showMinutesTimeForFileName time) dmp
             putStrLn $ "Dump saved in file " ++ "backup-" ++ showMinutesTimeForFileName time
         ["load",name] -> do
             dmp <- decodeFile name
             backup <- question "Dump accepted. This operation will destroy local data. Would you like to create backup?"
             when (backup) $ do
                    backupdmp <- getDBDump
                    time <- getMinutesTime
                    encodeFile ("backup-" ++ showMinutesTimeForFileName time) backupdmp
                    putStrLn $ "Backedup in file " ++ "backup-" ++ showMinutesTimeForFileName time
                    putStrLn $ ""
             loadDBDump dmp
         ["extract_pg",name] -> do
             dmp <- decodeFile name
             putStrLn "Postgres dumping successfull"    
             BSL.writeFile ("pg_dump extracted_" ++name) (pgdump dmp)
         _ -> do
             putStrLn ""
             putStrLn "Program : dbdump"
             putStrLn "Backuping and relocating kontrakcja database structure"    
             putStrLn "To load, owner of postgres database must be a superuser"
             putStrLn ""    
             putStrLn "Usage: dbdump dump | dbdump load filename | dbdump extract_pg filename"    
             putStrLn "   - 'dump' will create dumfile with current happstack state and postgres database dump"    
             putStrLn "   - 'load filename' will replate current happstack state and postgres database with content of dump file"    
             putStrLn "   - 'extract_pg filename' will get a pg_dump part from a dump and write it to some local file"    
             
    return () 

happstackStoreDir :: String
happstackStoreDir = "_local/kontrakcja_state"     

getDBDump:: IO DBDump
getDBDump = do
    db <- dbConfig <$> readAppConfig
    (code,pgd,stderror) <- readProcessWithExitCode' "pg_dump" ["-Ft", "--no-owner" ,db] BSL.empty
    when (code /= ExitSuccess) $
        error $ "Failed to pg_dump database \"" ++ db ++ "\" with: \n" ++ BSL.toString stderror 
    putStrLn $ "Postgres:" ++ db   
    putStrLn "Postgres dumping successfull"    
    lastCheckpoint <- find ("checkpoint" `isPrefixOf`) <$> reverse <$> sort <$> getDirectoryContents happstackStoreDir
    lastCurrent <- find ("current" `isPrefixOf`) <$> reverse <$> sort <$> getDirectoryContents happstackStoreDir
    hsd <- case (lastCheckpoint,lastCurrent) of
                   (Nothing,_) -> do
                        error "No last checkpoint found"
                   (_,Nothing) -> do
                        error "No last current found"
                   (Just chname,Just crname) -> do
                        putStrLn $ "Checkpoint file selected: " ++ chname                       
                        ch <- BSL.readFile $ happstackStoreDir ++ "/" ++ chname
                        putStrLn $ "Current file selected: " ++ crname                       
                        curr <- BSL.readFile $ happstackStoreDir ++ "/" ++ crname
                        return ((chname,ch),curr)
    putStrLn "Happstack dumping successfull"    
    return $ DBDump {pgdump = pgd, hsdump =  hsd}


loadDBDump :: DBDump -> IO ()
loadDBDump dump = do
    db <- dbConfig <$> readAppConfig
    (code,_,stderror) <- readProcessWithExitCode' "pg_restore" ["-c","-d",db] (pgdump dump)
    when (code /= ExitSuccess) $
        error $ "Failed to pg_restore database \"" ++ db ++ "\" with: \n" ++ BSL.toString stderror 
    putStrLn "Postgress restored"            
    files <- getDirectoryContents happstackStoreDir  
    forM_ files $ \n -> when (not $ "." `isPrefixOf` n) $ do
        putStrLn $ "Droping old happstack state file : " ++ n
        removeFile $ happstackStoreDir ++ "/" ++n
    BSL.writeFile  (happstackStoreDir ++ "/" ++  (fst $ fst $ hsdump dump)) (snd $ fst $ hsdump dump)
    putStrLn "Happstack state checkpoint recreated"
    BSL.writeFile  (happstackStoreDir ++ "/" ++  "current-0000000000") (snd $ hsdump dump)
    putStrLn "Happstack state current recreated"
    return ()

question :: String -> IO Bool
question s = do
    putStrLn $ s ++ " (y,n)"
    c <- getChar
    putStrLn ""
    case c of
         'y' -> return True
         'n' -> return False
         _ -> question s