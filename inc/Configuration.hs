module Configuration (
    readConfig
  ) where

import System.Exit
import Text.Show.Pretty
import qualified Control.Exception.Lifted as E
import Control.Monad.Base
import Control.Monad.Trans.Control
import Utils.Default

readConfig :: forall a m . (Read a, Show a, HasDefaultValue a, Monad m, MonadBaseControl IO m) => (String -> m ()) -> String -> [String] -> FilePath -> m a
readConfig logger _appname _args path = do
  logger "Reading configuration file..."
  res <- E.try $ (liftBase (readFile path >>= readIO))
  case res of
    Right app_conf -> do
      logger "Configuration file read and parsed."
      return app_conf
    Left (e::E.SomeException) -> do
      logger $ "Error while trying to read config file: " ++ show e
      logger $ "Please provide proper config file: " ++ path
      logger $ "Example configuration:"
      logger $ ppShow (defaultValue :: a)
      liftBase exitFailure
