{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Flow.HighTongue where

import Data.Aeson
import Data.Aeson.Types
import Data.Set (Set)
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

type DocumentName = Text
type UserName = Text
type MessageName = Text
type FieldName = Text
type StateName = Text


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

data Action
    = Notify
        { actionUsers :: [UserName]
        , actionMessage :: MessageName
        }
    | Close
        { actionDocuments :: [DocumentName]
        }
  deriving (Show, Eq, Ord)

parseNotify :: Value -> Parser Action
parseNotify = withObject "notify" $ \o -> do
  Notify <$> o .: "users" <*> o .: "message"

parseClose :: Value -> Parser Action
parseClose = withObject "close" $ \o -> do
  Close <$> o .: "documents"

instance FromJSON Action where
  parseJSON = withObject "action" $ \o -> do
    (k, v) <- case HM.toList o of
      [(k, v)] -> pure (k, v)
      _        -> typeMismatch "action object should have one key only" (Object o)
    case k of
      "notify" -> parseNotify v
      "close"  -> parseClose v
      _        -> typeMismatch "Expected key `notify`" (Object o)

data Stage = Stage
    { stageName :: Text
    , stageActions :: [Action]
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
        Stage k
          <$> o
          .:  "actions"
          <?> Key "actions"
          <*> explicitParseField parseSetExpect o "expect"
          <?> Key "expect"


newtype HighTongue = HighTongue
    { stages :: [Stage]
    }
  deriving (Show, Eq, Generic)

instance FromJSON HighTongue
