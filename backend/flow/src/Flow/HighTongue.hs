module Flow.HighTongue
    ( Expect(..)
    , SystemAction(..)
    , HighTongue(..)
    , Stage(..)
    , DocumentName
    , UserName
    , MessageName
    , FieldName
    , DSLVersion(..)
    , ValidationError(..)
    , decodeHighTongue
    )
  where

import Control.Arrow
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types
import Data.List.Extra (nubOrd)
import Data.Set (Set)
import Data.Text.Encoding
import Data.Word
import Data.Yaml
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T

import Flow.Names
import Flow.Process

data HighTongue = HighTongue
    { stages :: [Stage]
    , dslVersion :: DSLVersion
    }
  deriving (Show, Eq, Generic)

-- TODO rename this to something nicer, e.g. FlowDsl
instance FromJSON HighTongue where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = trainCase }

data Expect
    = ReceivedData
        { expectDocuments :: [DocumentName]
        , expectUsers :: [UserName]
        , expectFields :: [FieldName]
        }
    | ApprovedBy
        { expectDocuments :: [DocumentName]
        , expectUsers :: [UserName]
        }
    | SignedBy
        { expectDocuments :: [DocumentName]
        , expectUsers :: [UserName]
        }
    | ViewedBy
        { expectDocuments :: [DocumentName]
        , expectUsers :: [UserName]
        }
  deriving (Show, Eq, Ord)

parseApprovedBy :: Value -> Parser Expect
parseApprovedBy = withObject "approved-by" $ \o -> do
  ApprovedBy <$> o .: "documents" <*> o .: "users"

parseReceivedData :: Value -> Parser Expect
parseReceivedData = withObject "received-data" $ \o -> do
  ReceivedData <$> o .: "documents" <*> o .: "users" <*> o .: "fields"

parseSignedBy :: Value -> Parser Expect
parseSignedBy = withObject "signed-by" $ \o -> do
  SignedBy <$> o .: "documents" <*> o .: "users"

parseViewedBy :: Value -> Parser Expect
parseViewedBy = withObject "viewed-by" $ \o -> do
  ViewedBy <$> o .: "documents" <*> o .: "users"

parseSetExpect :: Value -> Parser (Set Expect)
parseSetExpect = withObject "expect"
  $ \o -> Set.fromList <$> traverse (uncurry parseExpect) (HM.toList o)
  where
    parseExpect k v = case k of
      "approved-by"   -> parseApprovedBy v
      "received-data" -> parseReceivedData v
      "signed-by"     -> parseSignedBy v
      "viewed-by"     -> parseViewedBy v
      _               -> typeMismatch
        "Expected one of the keys `approved-by`, `received-data`, `signed-by`, `viewed-by`"
        v

data SystemAction
    = Notify
        { actionUsers :: [UserName]
        , actionMessage :: MessageName
        }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON SystemAction where
  toJSON (Notify users message) =
    object ["notify" .= object ["users" .= toJSON users, "message" .= toJSON message]]

parseNotify :: Value -> Parser SystemAction
parseNotify = withObject "notify" $ \o -> do
  Notify <$> o .: "users" <*> o .: "message"

instance FromJSON SystemAction where
  parseJSON = withObject "action" $ \o -> do
    (k, v) <- case HM.toList o of
      [(k, v)] -> pure (k, v)
      _        -> typeMismatch "action object should have one key only" (Object o)
    case k of
      "notify" -> parseNotify v
      _        -> typeMismatch "Expected key `notify`" (Object o)

data Stage = Stage
    { stageName :: StageName
    , stageActions :: [SystemAction]
    , stageExpect :: Set Expect
    }
  deriving (Show, Eq)

instance FromJSON Stage where
  parseJSON = withObject "named stage" $ \o -> do
    (k, v) <- case HM.toList o of
      [(k, v)] -> pure (k, v)
      _        -> typeMismatch "stage object should have one key only" (Object o)
    parseStage k v
    where
      parseStage k = withObject "stage" $ \o ->
        Stage (unsafeName k)
          <$> (o .: "actions" <?> Key "actions")
          <*> (explicitParseField parseSetExpect o "expect" <?> Key "expect")

newtype DSLVersion = DSLVersion Text
  deriving (Show, Eq)

instance FromJSON DSLVersion where
  parseJSON = withText "dsl-version" $ \t -> do
    unless (DSLVersion t `elem` supportedVersions)
      $ typeMismatch "supported DSL version" (String t)
    pure (DSLVersion t)

supportedVersions :: [DSLVersion]
supportedVersions = [DSLVersion "0.1.0"]

data ValidationError = ValidationError
    { line_number :: Word32
    , column :: Word32
    , error_message :: Text
    }
  deriving (Eq, Generic, Show)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

instance FromJSON ValidationError where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON ValidationError where
  toEncoding = genericToEncoding aesonOptions


decodeHighTongue :: Process -> Either ValidationError HighTongue
decodeHighTongue process =
  (packError `left` decodeEither' (encodeUtf8 $ fromProcess process))
    >>= validateStageNameUniqueness
  where
    packError err = ValidationError
      { line_number   = 0
      , column        = 0
      , error_message = T.pack $ prettyPrintParseException err
      }
    validateStageNameUniqueness highTongue@HighTongue {..} = do
      let uniqNames = nubOrd $ stageName <$> stages
      if length uniqNames == length stages
        then pure highTongue
        else Left ValidationError { line_number   = 0
                                  , column        = 0
                                  , error_message = "The stage names are not all unique"
                                  }
