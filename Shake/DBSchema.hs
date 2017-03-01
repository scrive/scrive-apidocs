{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Shake.DBSchema (buildDBDocs) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Attoparsec.Text
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H

data DBConfig = DBConfig
  { dbcHost     :: String
  , dbcPort     :: String
  , dbcDatabase :: String
  , dbcUser     :: String
  , dbcPassword :: String
  } deriving Show

buildDBDocs :: FilePath -> Action ()
buildDBDocs tgt = do
  let tgtDir = takeDirectory tgt
  liftIO $ createDirectoryIfMissing False tgtDir
  runSchemaCrawler ["-outputformat=png", "-outputfile=kontra.png"]
  schemaCrawlerDir <- getSchemaCrawlerDir
  copyFileChanged (schemaCrawlerDir </> "kontra.png") (tgtDir </> "kontra.png")

  runSchemaCrawler [ "-outputformat=html", "-outputfile=kontra.html"
                   , "-routinetypes=" ]
  unit $ cmd (Cwd schemaCrawlerDir) ("sed" :: String)
    [ "-i" :: String, "256i\\<img src=\"kontra.png\" width=\"1280px\"\\>"
    , "kontra.html" ]
  copyFileChanged (schemaCrawlerDir </> "kontra.html")
    (tgtDir </> "kontra.html")
  where
    getSchemaCrawlerPath :: Action FilePath
    getSchemaCrawlerPath = do
      userHomeDir <- liftIO $ getHomeDirectory
      return $ userHomeDir </>
        "bin/schemacrawler/_schemacrawler/schemacrawler.sh"

    getSchemaCrawlerDir :: Action FilePath
    getSchemaCrawlerDir = takeDirectory <$> getSchemaCrawlerPath

    runSchemaCrawler :: [String] -> Action ()
    runSchemaCrawler extraArgs = do
      schemaCrawlerPath <- getSchemaCrawlerPath
      DBConfig{..} <- getKontrakcjaDBConfig
      cmd (Cwd $ takeDirectory schemaCrawlerPath) schemaCrawlerPath
        ([ "-loglevel=CONFIG", "-infolevel=standard", "-server=postgresql"
         , "-host", dbcHost, "-port", dbcPort, "-database", dbcDatabase
         , "-u", dbcUser, "-password", dbcPassword, "-c=brief"] ++ extraArgs)

    getKontrakcjaDBConfig :: Action DBConfig
    getKontrakcjaDBConfig = do
      appconfstr <- liftIO $ BL.readFile "kontrakcja.conf"
      let dbconf = fromMaybe [] $ do
            Object o <- decode appconfstr :: Maybe Value
            String dbconfstr <- H.lookup "database" o
            maybeResult . (`feed` "") . parse parserPostgresConnectionStr $ dbconfstr
      return $ DBConfig
        { dbcHost     = fromMaybe "localhost" . lookup "host"     $ dbconf
        , dbcPort     = fromMaybe "5432"      . lookup "port"     $ dbconf
        , dbcDatabase = fromMaybe "kontra"    . lookup "dbname"   $ dbconf
        , dbcUser     = fromMaybe "kontra"    . lookup "user"     $ dbconf
        , dbcPassword = fromMaybe "kontra"    . lookup "password" $ dbconf
        }

    parserPostgresConnectionStr :: Parser [(String,String)]
    parserPostgresConnectionStr = (`sepBy` (char ' ')) $ do
      key   <- many1 (notChar '=')
      _     <- char '='
      value <-     (many1 $ notChar '\'')
               <|> (do
                      _ <- char '\''
                      v <- many1 (notChar '\'')
                      _ <- char '\''
                      return v)
      return (key,value)
