module Configuration (
    readConfig
    , readConfig2
  ) where

import System.Exit
import Text.Show.Pretty
import qualified Control.Exception.Lifted as E
import Control.Monad.Base
import Control.Monad.Trans.Control
import Utils.Default
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)
import Data.Unjson
import Data.Aeson.Encode.Pretty

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

readConfig2 :: forall a m . (Unjson a, Read a, HasDefaultValue a, Monad m, MonadBaseControl IO m) => (String -> m ()) -> FilePath -> m a
readConfig2 logger path = do
  logger "Reading configuration file..."
  bsl' <- liftBase (BSL.readFile path)
  let bsl = BSL.dropWhile (`elem` [10,13,32]) bsl'
  res <- E.try $ if "{" `BSL.isPrefixOf` bsl
    then do -- json mode!
      parseEither bsl
    else do
      liftBase (readIO (BSL.toString bsl))
  case res of
    Right app_conf -> do
      logger "Configuration file read and parsed."
      return app_conf
    Left (e::E.SomeException) -> do
      let ud :: UnjsonDef a = unjsonDef
      logger $ "Error while reading '" ++ path ++ "' config file: " ++ show e
      logger $ "Configuration documentation:\n" ++ render ud
      logger $ "Example configuration:\n" ++ BSL.toString (encodePretty' (defConfig { confSort = compare }) (serialize ud (defaultValue :: a)))
      logger ""
      liftBase exitFailure
