{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Shake.DBSchema (buildDBDocs) where

import Control.Applicative ((<|>), many)
import Data.Aeson
import Data.Attoparsec.Text
import Data.Maybe
import Development.Shake hiding ((*>))
import Development.Shake.FilePath
import System.Directory
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H

import Shake.Utils

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

  runSchemaCrawler ["-outputformat=html", "-outputfile=kontra.html", "-routinetypes="]
  unit $ cmd
    (Cwd schemaCrawlerDir)
    ("sed" :: String)
    ["-i" :: String, "256i\\<img src=\"kontra.png\" width=\"1280px\"\\>", "kontra.html"]
  copyFileChanged (schemaCrawlerDir </> "kontra.html") (tgtDir </> "kontra.html")
  where
    getSchemaCrawlerPath :: Action FilePath
    getSchemaCrawlerPath = do
      (Exit code, Stdout stdout) <- cmd ("which schemacrawler.sh" :: String)
      case code of
        ExitSuccess -> return stdout
        _           -> do
          userHomeDir <- liftIO $ getHomeDirectory
          return
            $   userHomeDir
            </> "bin"
            </> "schemacrawler"
            </> "_schemacrawler"
            </> "schemacrawler.sh"

    getSchemaCrawlerDir :: Action FilePath
    getSchemaCrawlerDir = takeDirectory <$> getSchemaCrawlerPath

    runSchemaCrawler :: [String] -> Action ()
    runSchemaCrawler extraArgs = do
      schemaCrawlerPath <- getSchemaCrawlerPath
      DBConfig {..}     <- getKontrakcjaDBConfig
      cmd
        (Cwd $ takeDirectory schemaCrawlerPath)
        schemaCrawlerPath
        (  [ "-loglevel=CONFIG"
           , "-infolevel=standard"
           , "-server=postgresql"
           , "-host"
           , dbcHost
           , "-port"
           , dbcPort
           , "-database"
           , dbcDatabase
           , "-u"
           , dbcUser
           , "-password"
           , dbcPassword
           , "-c=brief"
           ]
        ++ extraArgs
        )

    getKontrakcjaDBConfig :: Action DBConfig
    getKontrakcjaDBConfig = do
      appconfstr <- liftIO $ BL.readFile "kontrakcja.conf"
      let dbconf = fromMaybe [] $ do
            Object o         <- decode appconfstr :: Maybe Value
            String dbconfstr <- H.lookup "database" o
            return
              . either (const []) id
              . parseOnly (parserPostgresConnectionStr <* endOfInput)
              $ dbconfstr
      return $ DBConfig { dbcHost     = findWithDefault "localhost" "host" $ dbconf
                        , dbcPort     = findWithDefault "5432" "port" $ dbconf
                        , dbcDatabase = findWithDefault "kontra" "dbname" $ dbconf
                        , dbcUser     = findWithDefault "kontra" "user" $ dbconf
                        , dbcPassword = findWithDefault "kontra" "password" $ dbconf
                        }

    parserPostgresConnectionStr :: Parser [(String, String)]
    parserPostgresConnectionStr = keyValue `sepBy` skipSpace
      where
        key      = many1 letter
        value    = many $ letter <|> digit <|> satisfy (inClass "._-")
        keyValue = (,) <$> key <*> (char '=' *> maybeInQuotes value)
        maybeInQuotes p = (char '\'' *> p <* char '\'') <|> p
