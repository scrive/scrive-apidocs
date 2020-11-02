module Configuration (
    ReadConfigOptions(..),
    readConfig,
    readConfigEx
  ) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Unjson
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Lazy as H
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

readConfig
  :: forall a m
   . (MonadFail m, Unjson a, MonadBaseControl IO m)
  => (String -> m ())
  -> FilePath
  -> m a
readConfig logger path = readConfigEx logger path standardReadConfigOptions


newtype ReadConfigOptions = ReadConfigOptions {
  optReadConfigUncommentKeys :: Bool
  -- ^ Remove initial underscores from "commented out" keys (useful
  -- for testing).
  }

standardReadConfigOptions :: ReadConfigOptions
standardReadConfigOptions = ReadConfigOptions { optReadConfigUncommentKeys = False }

--
-- Error handling here:
-- 1. When no file: please create file and full docs
-- 2. When looks like json but not parse as json: info where it did
-- not parse, no other info
-- 3. When does not look like json and does not readIO: full docs
-- 4. When unjson has issue, then just info about specific problems

readConfigEx
  :: forall a m
   . (MonadFail m, Unjson a, MonadBaseControl IO m)
  => (String -> m ())
  -> FilePath
  -> ReadConfigOptions
  -> m a
readConfigEx logger path ReadConfigOptions {..} = do
  logger $ "Reading configuration " ++ path ++ "..."
  bsl' <- either logExceptionAndPrintFullDocs return
    =<< E.try (liftBase (BSL.readFile path))

  let bsl = BSL.dropWhile (`elem` [10, 13, 32]) bsl'


  res <- do
    js' <- either logYamlParseExceptionAndBlameJsonParser return
      $ Yaml.decodeEither' (BS.concat (BSL.toChunks bsl))
    let js = if optReadConfigUncommentKeys then removeTopLevelUnderscores js' else js'
    case parse ud js of
      Result value []       -> return value
      Result _     problems -> logProblems problems

  logger $ "Configuration file " ++ path ++ " read and parsed."
  return res
  where
    ud :: UnjsonDef a
    ud = unjsonDef

    removeTopLevelUnderscores :: Yaml.Value -> Yaml.Value
    removeTopLevelUnderscores (Yaml.Object hm) = Yaml.Object hm'
      where
        hm' = H.foldlWithKey'
          (\m k v -> if "_" `Text.isPrefixOf` k
            then H.insert (Text.tail k) v m
            else H.insert k v m
          )
          H.empty
          hm
    removeTopLevelUnderscores v = v

    logStringAndFail :: String -> m g
    logStringAndFail ex = do
      logger ex
      fail ex
    logYamlParseExceptionAndBlameJsonParser :: Yaml.ParseException -> m g
    logYamlParseExceptionAndBlameJsonParser ex = do
      -- sadly parsing issues in aeson are reported as badly as anything else
      logStringAndBlameJsonParser $ showNiceYamlParseException path ex
    logStringAndBlameJsonParser :: String -> m g
    logStringAndBlameJsonParser ex = do
      -- sadly parsing issues in aeson are reported as badly as anything else
      logger ex
      logStringAndFail
        $  "Configuration file '"
        ++ path
        ++ "' has syntax errors and is not valid JSON"
    logExceptionAndPrintFullDocs :: E.SomeException -> m g
    logExceptionAndPrintFullDocs ex = logStringAndPrintFullDocs (show ex)
    logStringAndPrintFullDocs :: String -> m g
    logStringAndPrintFullDocs ex = do
      logger
        $  ex
        ++ "\n"
        ++ render ud
        ++ "\n You can find configuration templates in configuration-templates directory.\n"
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

showNiceYamlParseException :: FilePath -> Yaml.ParseException -> String
showNiceYamlParseException filepath parseException = case parseException of
  Yaml.NonScalarKey            -> filepath ++ ": non scalar key"
  Yaml.UnknownAlias anchorName -> filepath ++ ": unknown alias " ++ anchorName
  Yaml.UnexpectedEvent received expected ->
    filepath
      ++ ": unknown event received "
      ++ show received
      ++ " when expected "
      ++ show expected
  Yaml.InvalidYaml Nothing -> filepath ++ ": invalid yaml (no further info available)"
  Yaml.InvalidYaml (Just (Yaml.YamlException ex)) -> filepath ++ ": invalid yaml: " ++ ex
  Yaml.InvalidYaml (Just (Yaml.YamlParseException problem context (Yaml.YamlMark _index line column)))
    -> filepath
      ++ ":"
      ++ show (line + 1)
      ++ ":"
      ++ show (column + 1)
      ++ ": "
      ++ problem
      ++ " "
      ++ context
  Yaml.MultipleDocuments      -> filepath ++ ": multiple documents"
  Yaml.AesonException      ex -> filepath ++ ": " ++ ex
  Yaml.OtherParseException ex -> filepath ++ ": " ++ show ex
  Yaml.NonStringKey        _  -> "non-string keys are not supported"
  Yaml.NonStringKeyAlias anchorName value ->
    filepath ++ ": unknown non-string key alias " ++ show anchorName ++ ", " ++ show value
  Yaml.CyclicIncludes             -> filepath ++ ": cyclic includes"
  Yaml.LoadSettingsException _ ex -> showNiceYamlParseException filepath ex
