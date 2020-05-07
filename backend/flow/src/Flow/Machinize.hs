{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Flow.Machinize where

import Data.List.Extra (snoc)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Flow.HighTongue
import Flow.Transducer

data Deed = Field FieldName | Approval | Signature | View
  deriving (Eq, Ord, Show)

data EventInfo = EventInfo
    { eventInfoDeed     :: Deed
    , eventInfoUser     :: UserName
    , eventInfoDocument :: DocumentName
    }
  deriving (Eq, Ord, Show)

eventize :: Expect -> [EventInfo]
eventize = \case
  ReceivedData {..} ->
    EventInfo <$> (Field <$> expectFields) <*> expectUsers <*> expectDocuments
  ApprovedBy {..} -> EventInfo Approval <$> expectUsers <*> expectDocuments
  ViewedBy {..}   -> EventInfo View <$> expectUsers <*> expectDocuments
  SignedBy {..}   -> EventInfo Signature <$> expectUsers <*> expectDocuments


empty :: StateId -> Transducer a b
empty id = Transducer Map.empty id []

type Machine = Transducer (Set EventInfo) [Action]
type State = TransducerState (Set EventInfo) [Action]
type Edge = TransducerEdge (Set EventInfo) [Action]

mkEdge :: (Stage, StateId) -> (StateId, Edge)
mkEdge (stage, next) = (stateId, edge)
  where
    stateId = stageName stage
    events  = Set.fromList $ Set.toList (stageExpect stage) >>= eventize
    edge    = TransducerEdge events (stageActions stage) next

-- TODO add end state
linear :: HighTongue -> Either Error Machine
linear HighTongue {..} = createTransducer initialState ["final"] edges
  where
    edges        = map mkEdge pairs
    initialState = stageName $ head stages
    nextStates   = snoc (map stageName $ tail stages) "final"
    pairs        = zip stages nextStates
