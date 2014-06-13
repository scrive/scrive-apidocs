module Configuration (
    Configuration(..)
  , readConfig
  ) where

import System.Exit
import Text.Show.Pretty
import qualified Control.Exception.Lifted as E

class (Read a, Show a) => Configuration a where
  confDefault :: a

readConfig :: forall a. Configuration a => (String -> IO ()) -> String -> [String] -> FilePath -> IO a
readConfig logger _appname _args path = do
  logger "Reading configuration file..."
  res <- E.try $ readFile path >>= readIO
  case res of
    Right app_conf -> do
      logger "Configuration file read and parsed."
      return app_conf
    Left (e::E.SomeException) -> do
      logger $ "Error while trying to read config file: " ++ show e
      logger $ "Please provide proper config file: " ++ path
      logger $ "Example configuration:"
      logger $ ppShow (confDefault :: a)
      exitFailure
