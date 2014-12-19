module Configuration (
    readConfig
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Char
import Data.Monoid
import Data.Unjson
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString, fromString)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import Utils.Default

--
-- Error handling here:
-- 1. When no file: please create file and full docs
-- 2. When looks like json but not parse as json: info where it did not parse, no other info
-- 3. When does not look like json and does not readIO: full docs
-- 4. When unjson has issue, then just info about specific problems

readConfig :: forall a m . (Unjson a, Read a, HasDefaultValue a, Monad m, MonadBaseControl IO m) => (String -> m ()) -> FilePath -> m a
readConfig logger path = do
  let pathWithJsonExt = path ++ ".json"
  logger $ "Reading configuration " ++ path ++ "..."
  bsl' <- either logExceptionAndPrintFullDocs return =<<
          E.try (liftBase (BSL.readFile path))

  let bsl = BSL.dropWhile (`elem` [10,13,32]) bsl'


  res <- if "{" `BSL.isPrefixOf` bsl
    then do
      js <- either logYamlParseExceptionAndBlameJsonParser return $
            Yaml.decodeEither' (BS.concat (BSL.toChunks bsl))
      case parse ud (Anchored mempty js) of
        Result value [] -> return value
        Result _ problems -> logProblems problems
    else do
      case reads (BSL.toString bsl) of
        [(a, g)] | all isSpace g -> do
          logger $ "It is better to use json/yaml format for configuration."
          logger $ "For your convenience your configuration has been written to '" ++ pathWithJsonExt ++ "'"
          liftBase $ BSL.writeFile pathWithJsonExt (BSL.fromString (configAsJsonString a))
          return a
        ((_, g):_) -> do
          logStringAndFail ("Garbage at the end of " ++ path ++ " file: " ++ g)
        _ -> logStringAndPrintFullDocs $ "Unable to parse configuration file (it is better to use json) " ++ path

  logger $ "Configuration file " ++ path ++ " read and parsed."
  return res
  where
    ud :: UnjsonDef a
    ud = unjsonDef
    logStringAndFail :: String -> m g
    logStringAndFail ex = do
      logger $ ex
      fail ex
    logYamlParseExceptionAndBlameJsonParser :: Yaml.ParseException -> m g
    logYamlParseExceptionAndBlameJsonParser ex = do
      -- sadly parsing issues in aeson as reported as badly as anything else
      logStringAndBlameJsonParser $ showNiceYamlParseException path ex
    logStringAndBlameJsonParser :: String -> m g
    logStringAndBlameJsonParser ex = do
      -- sadly parsing issues in aeson as reported as badly as anything else
      logger $ ex
      logStringAndFail $ "Configuration file '" ++ path ++ "' has syntax errors and is not a valid json"
    logExceptionAndPrintFullDocs :: E.SomeException -> m g
    logExceptionAndPrintFullDocs ex = logStringAndPrintFullDocs (show ex)
    logStringAndPrintFullDocs :: String -> m g
    logStringAndPrintFullDocs ex = do
      logger $ ex ++ "\n" ++ render ud ++ "\n" ++ configAsJsonString defaultValue
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
    configAsJsonString :: a -> String
    configAsJsonString a = BSL.toString (unjsonToByteStringLazyPretty ud a)

showNiceYamlParseException :: FilePath -> Yaml.ParseException -> String
showNiceYamlParseException filepath parseException =
  case parseException of
    Yaml.NonScalarKey -> filepath ++ ": non scalar key"
    Yaml.UnknownAlias anchorName -> filepath ++ ": unknown alias " ++ anchorName
    Yaml.UnexpectedEvent received expected -> filepath ++ ": unknown event received " ++ show received ++ " when expected " ++ show expected
    Yaml.InvalidYaml Nothing -> filepath ++ ": invalid yaml (no further info available)"
    Yaml.InvalidYaml (Just (Yaml.YamlException ex)) -> filepath ++ ": invalid yaml: " ++ ex
    Yaml.InvalidYaml (Just (Yaml.YamlParseException problem context (Yaml.YamlMark _index line column))) ->
      filepath ++ ":" ++ show (line+1) ++ ":" ++ show (column+1) ++ ": " ++ problem ++ " " ++ context
    Yaml.AesonException ex -> filepath ++ ": " ++ ex
    Yaml.OtherParseException ex -> filepath ++ ": " ++ show ex
    Yaml.NonStringKeyAlias anchorName value -> filepath ++ ": unknown non-string key alias " ++ show anchorName ++ ", " ++ show value
