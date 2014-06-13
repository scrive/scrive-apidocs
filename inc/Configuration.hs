module Configuration (
    Configuration(..)
  , readConfig
  ) where

import Control.Monad
import System.Console.GetOpt
import System.Exit
import Text.Show.Pretty
import qualified Control.Exception.Lifted as E

class (Read a, Show a) => Configuration a where
  confDefault :: a
  confOptions :: [OptDescr (a -> a)]

readConfig :: forall a. Configuration a => (String -> IO ()) -> String -> [String] -> FilePath -> IO a
readConfig logger appname args path = do
  logger "Reading configuration file..."
  res <- E.try $ readFile path >>= readIO
  case res of
    Right app_conf -> do
      logger "Configuration file read and parsed."
      if not $ null args
        then case parseArgs of
          Right f -> return (f app_conf)
          Left errs -> do
            logger $ "Errors while parsing command line options:"
            forM_ errs $ \err -> logger $ "* " ++ err
            logger $ "Usage: " ++ usageInfo appname (confOptions :: [OptDescr (a -> a)])
            exitFailure
        else return app_conf
    Left (e::E.SomeException) -> do
      logger $ "Error while trying to read config file: " ++ show e
      logger $ "Please provide proper config file: " ++ path
      logger $ "Example configuration:"
      logger $ ppShow (confDefault :: a)
      exitFailure
  where
    parseArgs = case getOpt Permute confOptions args of
      (flags, _, []) -> Right $ \c -> foldr ($) c flags
      (_, _, errs)   -> Left $ map (filter (/= '\n')) errs
