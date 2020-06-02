{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Flow.Aggregator
    ( AggregatorState(..)
    , AggregatorError(..)
    , AggregatorStep(..)
    , aggregate
    , aggregateAndStep
    , getAllowedEvents
    , makeNewState
    , prepareAggregatorState
    , runAggregatorStep
    )
  where

import Control.Arrow (left)
import Control.Exception (Exception, throwIO)
import Data.Aeson
import Data.Aeson.Casing
import Data.Set (Set)
import Database.PostgreSQL.PQTypes
import GHC.Generics
import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as S
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Flow.Machinize
import Flow.Transducer

data AggregatorState = AggregatorState
    { receivedEvents :: Set EventInfo
    , currentState :: StateId
    }
  deriving (Show, Eq, Generic)

newtype AggregatorConstans = AggregatorConstans
    { machine :: Machine
    }
  deriving (Show, Eq, Generic)

{- HLINT ignore AggregatorMarshallingException -}
data AggregatorMarshallingException = AggregatorMarshallingException String
  deriving (Show, Exception)

instance PQFormat AggregatorState where
  pqFormat = pqFormat @(JSON Value)

instance FromSQL AggregatorState where
  type PQBase AggregatorState = PQBase (JSON Value)
  fromSQL mbase = do
    (JSON jsonValue) <- fromSQL mbase
    case fromJSON jsonValue of
      Error   err -> throwIO $ AggregatorMarshallingException err
      Success v   -> pure v

instance ToSQL AggregatorState where
  type PQDest AggregatorState = PQDest (JSON Value)
  toSQL v = toSQL (JSON $ toJSON v)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

instance FromJSON AggregatorState where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON AggregatorState where
  toJSON = genericToJSON aesonOptions

data AggregatorStep
    = NeedMoreEvents
    | StateChange [LowAction]
    | FinalState [LowAction]
  deriving (Eq, Show, Generic)

data AggregatorError
    = UnknownEventInfo
    | DuplicateEvent
    | TransducerError Flow.Transducer.Error
  deriving (Eq, Show, Generic)

type AggregatorT = E.ExceptT AggregatorError (R.ReaderT Machine (S.State AggregatorState))

put :: AggregatorState -> AggregatorT ()
put = S.put

aggregate :: EventInfo -> AggregatorT (Maybe (Set EventInfo))
aggregate event = do
  state@AggregatorState {..} <- S.get
  machine <- R.ask
  machineState <- E.liftEither . left TransducerError $ getState machine currentState
  when (Set.notMember event $ getAllowedEvents machineState)
    $ E.throwError UnknownEventInfo
  -- TODO: Think hard about various scenarios where events may be duplicate.
  -- For example typos can lead to multiple events of the same type.
  when (Set.member event receivedEvents) $ E.throwError DuplicateEvent
  let newReceivedEvents = Set.insert event receivedEvents
  put $ state { receivedEvents = newReceivedEvents }
  pure . List.find (`Set.isSubsetOf` newReceivedEvents) $ getEdgeInputs machineState

aggregateAndStep :: EventInfo -> AggregatorT AggregatorStep
aggregateAndStep event = do
  AggregatorState {..}    <- S.get
  machine@Transducer {..} <- R.ask
  maybeEdgeInput          <- aggregate event
  case maybeEdgeInput of
    Nothing        -> pure NeedMoreEvents
    Just edgeInput -> do
      TransducerEdge {..} <- either (E.throwError . TransducerError) pure
        $ step machine currentState edgeInput
      put $ makeNewState edgeNextState
      pure $ if isFinalState machine edgeNextState
        then FinalState edgeOutputLabel
        else StateChange edgeOutputLabel

makeNewState :: StateId -> AggregatorState
makeNewState newState =
  AggregatorState { currentState = newState, receivedEvents = mempty }

prepareAggregatorState :: Machine -> AggregatorState
prepareAggregatorState Transducer {..} = makeNewState initialState

getAllowedEvents :: State -> Set EventInfo
getAllowedEvents TransducerState {..} = foldl Set.union Set.empty $ Map.keys stateEdges

getEdgeInputs :: State -> [Set EventInfo]
getEdgeInputs TransducerState {..} = Map.keys stateEdges

runAggregatorStep
  :: EventInfo
  -> AggregatorState
  -> Machine
  -> (Either AggregatorError AggregatorStep, AggregatorState)
runAggregatorStep event aggregatorState machine = do
  flip S.runState aggregatorState
    . flip R.runReaderT machine
    . E.runExceptT
    $ aggregateAndStep event
