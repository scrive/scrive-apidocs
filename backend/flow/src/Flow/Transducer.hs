{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Flow.Transducer where

import Data.Either.Extra (maybeToEither)
import Data.Map.Strict
import qualified Data.Map.Strict as M
import Data.Text hiding (find)
import Data.Foldable
import GHC.Generics


type StateId = Text

data TransducerEdge a b = TransducerEdge
    { edgeInputLabel :: a
    , edgeOutputLabel :: b
    , edgeNextState :: StateId
    }
  deriving (Show, Eq, Generic)

data TransducerState a b = TransducerState
    { _stateId :: StateId
    , _stateEdges :: Map a (TransducerEdge a b)
    }
  deriving (Show, Eq, Generic)

data Transducer a b = Transducer
    { _states :: (Map StateId (TransducerState a b))
    , _initialState :: StateId
    , _endStates :: [StateId]
    }
  deriving (Show, Eq, Generic)

data Error
    = TransitionNotFound
    | StateIdNotFound
    | DuplicitEdge Text
  deriving (Eq, Show)

step :: forall a b. (Eq a, Ord a) => Transducer a b -> StateId -> a -> Either Error (TransducerEdge a b)
step Transducer{..} stateId input
    = getState >>= findEdge
  where
    getState = maybeToEither StateIdNotFound $ _states !? stateId
    findEdge :: TransducerState a b -> Either Error (TransducerEdge a b)
    findEdge TransducerState{..} = maybeToEither TransitionNotFound
        $ _stateEdges !? input

createTransducer
    :: forall a b
    . Ord a
    => Show a
    => Show b
    => StateId
    -> [StateId]
    -> [(StateId, TransducerEdge a b)]
    -> Either Error (Transducer a b)
createTransducer _initialState _endStates edges = do
    _states <- foldlM addStateAndEndge M.empty edges
    pure Transducer{..}
  where
    addStateAndEndge :: Map StateId (TransducerState a b) -> (StateId, TransducerEdge a b) -> Either Error (Map StateId (TransducerState a b))
    addStateAndEndge stateMap (stateId, edge) =
        alterF (addState stateId edge) stateId stateMap

    addState :: StateId -> TransducerEdge a b -> Maybe (TransducerState a b) -> Either Error (Maybe (TransducerState a b))
    addState _stateId edge Nothing = pure . Just $ TransducerState
        { _stateId
        , _stateEdges = M.singleton (edgeInputLabel edge) edge
        }
    addState _ edge (Just state@TransducerState{..}) = do
        _stateEdges <- alterF (addEdge edge) (edgeInputLabel edge) _stateEdges
        pure . Just $ state { _stateEdges }
    addEdge :: TransducerEdge a b -> Maybe (TransducerEdge a b) -> Either Error (Maybe (TransducerEdge a b))
    addEdge edge Nothing = pure $ Just edge
    addEdge _edge (Just e) = Left . DuplicitEdge . pack $ show e
