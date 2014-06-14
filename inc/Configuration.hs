module Configuration (
    readConfig
  ) where

import qualified Control.Exception.Lifted as E
import Control.Monad.Base
import Control.Monad.Trans.Control
import Utils.Default
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)
import Data.Unjson
import Data.Aeson.Encode.Pretty
import Data.Aeson
import qualified Data.Text as Text


--
-- Error handling here:
-- 1. When no file: please create file and full docs
-- 2. When looks like json but not parse as json: info where it did not parse, no other info
-- 3. When does not look like json and does not readIO: full docs
-- 4. When unjson has issue, then just info about specific problems

readConfig :: forall a m . (Unjson a, Read a, HasDefaultValue a, Monad m, MonadBaseControl IO m) => (String -> m ()) -> FilePath -> m a
readConfig logger path = do
  logger $ "Reading configuration " ++ path ++ "..."
  bsl' <- either logExceptionAndPrintFullDocs return =<<
          E.try (liftBase (BSL.readFile path))

  let bsl = BSL.dropWhile (`elem` [10,13,32]) bsl'


  res <- if "{" `BSL.isPrefixOf` bsl
    then do
      js <- either logStringAndBlameJsonParser return $
            eitherDecode bsl
      case parse ud (Anchored mempty js) of
        Result value [] -> return value
        Result _ problems -> logProblems problems
    else do
      case reads (BSL.toString bsl) of
        [(a, "")] -> return a
        ((_,g):_) -> logString ("Garbage at the end of " ++ path ++ " file: " ++ g)
        _ -> logStringAndPrintFullDocs $ "Unable to parse configuration file (it is better to use json) " ++ path

  logger $ "Configuration file " ++ path ++ " read and parsed."
  return res
  where
    ud :: UnjsonDef a = unjsonDef
    logString :: String -> m g
    logString ex = do
      logger $ ex
      fail ex
    logStringAndBlameJsonParser :: String -> m g
    logStringAndBlameJsonParser ex = do
      -- sadly parsing issues in aeson as reported as badly as anything else
      logger $ ex
      logString $ "Configuration file '" ++ path ++ "' has syntax errors and is not a valid json"
    logExceptionAndPrintFullDocs :: E.SomeException -> m g
    logExceptionAndPrintFullDocs ex = logStringAndPrintFullDocs (show ex)
    logStringAndPrintFullDocs :: String -> m g
    logStringAndPrintFullDocs ex = do
      logger $ ex ++ "\n" ++ render ud ++ "\n" ++
             BSL.toString (encodePretty' (defConfig { confCompare = compare }) (serialize ud (defaultValue :: a)))
      fail (show ex)
    logProblem (Anchored xpath msg) = do
        case renderForPath xpath ud of
          Just moreInfo -> do
            logger $ show xpath ++ ": " ++ Text.unpack msg ++ "\n" ++ moreInfo
          Nothing -> do
            logger $ show xpath ++ ": " ++ Text.unpack msg
    logProblems problems = do
      logger $ "There were issues with the content of configuration " ++ path
      mapM_ logProblem problems
      fail $ "There were issues with the content of configuration " ++ path
