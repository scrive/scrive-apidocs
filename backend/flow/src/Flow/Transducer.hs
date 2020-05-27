{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Flow.Transducer
    ( TransducerEdge(..)
    , StateId
    , TransducerState(..)
    , Transducer(..)
    , Error(..)
    , createTransducer
    , step
    , getState
    , isFinalState
    )
  where

import Control.Exception (Exception, throwIO)
import Data.Aeson
import Data.Aeson.Casing
import Data.Either.Extra (maybeToEither)
import Data.Foldable
import Data.Map.Strict
import Data.Text hiding (find)
import Database.PostgreSQL.PQTypes
import GHC.Generics
import qualified Data.Map.Strict as M

type StateId = Text


-- TODO: Cleanup these data structures... i.e. remove duplicate fields.
data TransducerEdge a b = TransducerEdge
    { edgeInputLabel :: a
    , edgeOutputLabel :: b
    , edgeNextState :: StateId
    }
  deriving (Show, Eq, Generic)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

instance (FromJSON a, FromJSON b) => FromJSON (TransducerEdge a b) where
  parseJSON = genericParseJSON aesonOptions

instance (ToJSON a, ToJSON b) => ToJSON (TransducerEdge a b) where
  toJSON = genericToJSON aesonOptions

data TransducerState a b = TransducerState
    { stateId :: StateId
    , stateEdges :: Map a (TransducerEdge a b)
    }
  deriving (Show, Eq, Generic)

instance (Ord a, FromJSON a, FromJSON b) => FromJSON (TransducerState a b) where
  parseJSON = withObject "TransducerState" $ \v -> do
    stateId    <- v .: "state-id"
    stateEdges <- M.fromList <$> v .: "state-edges"
    pure TransducerState { .. }

instance (Ord a, ToJSON a, ToJSON b) => ToJSON (TransducerState a b) where
  toJSON TransducerState {..} =
    object ["state-id" .= stateId, "state-edges" .= M.toList stateEdges]

data Transducer a b = Transducer
    { states :: Map StateId (TransducerState a b)
    , initialState :: StateId
    , endStates :: [StateId]
    }
  deriving (Show, Eq, Generic)

instance (Ord a, FromJSON a, FromJSON b) => FromJSON (Transducer a b) where
  parseJSON = genericParseJSON aesonOptions

instance (Ord a, ToJSON a, ToJSON b) => ToJSON (Transducer a b) where
  toJSON = genericToJSON aesonOptions

{- HLINT ignore TranducerMarshallingException -}
data TranducerMarshallingException = TransducerFromJsonDecoding String
  deriving (Show, Exception)

instance (Ord a, FromJSON a, FromJSON b, ToJSON a, ToJSON b)
  => PQFormat (Transducer a b) where
  pqFormat = pqFormat @(JSON Value)

instance (Ord a, FromJSON a, FromJSON b, ToJSON a, ToJSON b) => FromSQL (Transducer a b) where
  type PQBase (Transducer a b) = PQBase (JSON Value)
  fromSQL mbase = do
    (JSON jsonValue) <- fromSQL mbase
    case fromJSON jsonValue of
      Error   err -> throwIO $ TransducerFromJsonDecoding err
      Success v   -> pure v

instance (Ord a, FromJSON a, FromJSON b, ToJSON a, ToJSON b) => ToSQL (Transducer a b) where
  type PQDest (Transducer a b) = PQDest (JSON Value)
  toSQL v = toSQL (JSON $ toJSON v)


data Error
    = TransitionNotFound
    | StateIdNotFound
    | DuplicitEdge Text
  deriving (Eq, Show)

step
  :: forall a b
   . (Eq a, Ord a)
  => Transducer a b
  -> StateId
  -> a
  -> Either Error (TransducerEdge a b)
step transducer@Transducer {..} stateId' input =
  getState transducer stateId' >>= findEdge
  where
    findEdge :: TransducerState a b -> Either Error (TransducerEdge a b)
    findEdge TransducerState {..} =
      maybeToEither TransitionNotFound $ stateEdges !? input

createTransducer
  :: forall a b
   . Ord a
  => Show a
  => Show b
  => StateId
  -> [StateId]
  -> [(StateId, TransducerEdge a b)]
  -> Either Error (Transducer a b)
createTransducer initialState endStates edges = do
  states <-
    (fromList (fmap toEndState endStates) <>) <$> foldlM addStateAndEndge M.empty edges
  pure Transducer { .. }
  where
    addStateAndEndge
      :: Map StateId (TransducerState a b)
      -> (StateId, TransducerEdge a b)
      -> Either Error (Map StateId (TransducerState a b))
    addStateAndEndge stateMap (stateId, edge) =
      alterF (addState stateId edge) stateId stateMap

    addState
      :: StateId
      -> TransducerEdge a b
      -> Maybe (TransducerState a b)
      -> Either Error (Maybe (TransducerState a b))
    addState stateId edge Nothing = pure . Just $ TransducerState
      { stateId
      , stateEdges = M.singleton (edgeInputLabel edge) edge
      }
    addState _ edge (Just state@TransducerState {..}) = do
      stateEdges' <- alterF (addEdge edge) (edgeInputLabel edge) stateEdges
      pure . Just $ state { stateEdges = stateEdges' }

    addEdge
      :: TransducerEdge a b
      -> Maybe (TransducerEdge a b)
      -> Either Error (Maybe (TransducerEdge a b))
    addEdge edge  Nothing  = pure $ Just edge
    addEdge _edge (Just e) = Left . DuplicitEdge . pack $ show e

    toEndState :: StateId -> (StateId, TransducerState a b)
    toEndState stateId = (stateId, emptyState stateId)

    emptyState :: StateId -> TransducerState a b
    emptyState stateId = TransducerState { stateId, stateEdges = mempty }

getState :: Transducer a b -> StateId -> Either Error (TransducerState a b)
getState Transducer {..} stateId = maybeToEither StateIdNotFound $ states !? stateId

-- TODO: tests
isFinalState :: Transducer a b -> StateId -> Bool
isFinalState Transducer {..} stateId = stateId `elem` endStates
