{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Engine
  ( EngineEvent(..)
  , EngineError(..)
  , processMachinizeEvent
  , processEvent
  , decodeHighTongueM
  ) where

import Control.Monad.Catch
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG
import Data.Aeson.Types
import Database.PostgreSQL.PQTypes (MonadDB)
import GHC.Generics
import Log (MonadLog, localData, logAttention_, logTrace_)
import Text.StringTemplates.Templates (TemplatesMonad)

import Doc.DocumentID (DocumentID)
import Doc.DocumentMonad
import Doc.SignatoryLinkID (SignatoryLinkID, fromSignatoryLinkID)
import EventStream.Class
import File.Storage (MonadFileStorage)
import Flow.ActionConsumers (consumeFlowAction, toConsumableAction)
import Flow.Aggregator
  ( AggregatorError(DuplicateEvent, UnknownEventInfo, UnknownStage)
  , AggregatorStep(NeedMoreEvents, StateChange), runAggregatorStep
  )
import Flow.HighTongue
import Flow.Id (InstanceId)
import Flow.Machinize
import Flow.Model as Model
import Flow.Model.Types
import Flow.Process
import GuardTime (GuardTimeConfMonad)
import MailContext

data EngineEvent = EngineEvent
    { instanceId :: InstanceId
    , userAction :: UserAction
    , signatoryId :: SignatoryLinkID
    , documentId :: DocumentID
    }
  deriving (Show, Eq, Generic)

toPairs :: EngineEvent -> [Pair]
toPairs EngineEvent {..} =
  [ "instance_id" .= instanceId
  , "user_action" .= userAction
  , "signatory_id" .= show (fromSignatoryLinkID signatoryId)
  , "document_id" .= documentId
  ]

data EngineError
    = AggregatorSteppingFailed
    | DuplicateEventReceived
    | InvalidProcess [ValidationError]
    | NoAssociatedDocument
    | NoAssociatedUser
    | NoInstance
    | UnexpectedEventReceived
  deriving (Show, Exception)

processEvent
  :: ( CryptoRNG m
     , DocumentMonad m
     , GuardTimeConfMonad m
     , MailContextMonad m
     , MonadBaseControl IO m
     , MonadDB m
     , MonadEventStream m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadThrow m
     , TemplatesMonad m
     )
  => EngineEvent
  -> ExceptT EngineError m ()
processEvent event@EngineEvent {..} = do
  localData (toPairs event) $ do
    documentName <- fromMaybeM noDocumentNameFound
      $ selectDocumentNameFromKV instanceId documentId
    userName <- fromMaybeM noUserNameFound $ selectUserNameFromKV instanceId signatoryId
    processMachinizeEvent
      instanceId
      EventInfo { eventInfoAction   = userAction
                , eventInfoUser     = userName
                , eventInfoDocument = documentName
                }
  where
    noDocumentNameFound = do
      logAttention_ "Given document not associated with flow instance."
      throwError NoAssociatedDocument
    noUserNameFound = do
      logAttention_ "Signatory not associated with flow intance."
      throwError NoAssociatedUser

pushActions
  :: ( CryptoRNG m
     , DocumentMonad m
     , GuardTimeConfMonad m
     , MailContextMonad m
     , MonadBaseControl IO m
     , MonadDB m
     , MonadEventStream m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadThrow m
     , TemplatesMonad m
     )
  => InstanceId
  -> [LowAction]
  -> ExceptT EngineError m ()
pushActions instanceId actions = do
  consumableActions <- mapM (toConsumableAction instanceId) actions
  mapM_ consumeFlowAction consumableActions

decodeHighTongueM :: (MonadLog m, MonadThrow m) => Process -> m HighTongue
decodeHighTongueM process = either throwDSLValidationError' pure
  $ decodeHighTongue process
  where
    throwDSLValidationError' errs = do
      logAttention_ $ "Flow DSL compatibility broken: " <> showt errs
      throwM $ InvalidProcess errs

processMachinizeEvent
  :: ( CryptoRNG m
     , DocumentMonad m
     , GuardTimeConfMonad m
     , MailContextMonad m
     , MonadBaseControl IO m
     , MonadDB m
     , MonadEventStream m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadThrow m
     , TemplatesMonad m
     )
  => InstanceId
  -> EventInfo
  -> ExceptT EngineError m ()
processMachinizeEvent instanceId eventInfo = do
  -- TODO Should we store store duplicate/unknown events?
  -- Currently they are being thrown away since this code runs in a single transaction
  -- that is aborted on errors.
  eventId      <- insertEvent $ toInsertEvent instanceId eventInfo
  fullInstance <- fromMaybeM noInstance $ selectFullInstance instanceId
  let aggregator = instanceToAggregator fullInstance
  highTongue <- decodeHighTongueM $ fullInstance ^. #template % #process
  let (aggregatorResult, newAggregator) =
        runAggregatorStep eventInfo aggregator highTongue
  either failGracefully (processStep eventId newAggregator) aggregatorResult
  where
    -- TODO: Think of some error monad. MonadFail is ugly.
    -- TODO: This fail thing may leak some error to user.
    -- TODO: Improve error messages.
    failGracefully
      :: (MonadLog m, MonadDB m, MonadThrow m)
      => AggregatorError
      -> ExceptT EngineError m ()
    failGracefully UnknownEventInfo = do
      logAttention_ "Unexpected event received."
      throwError UnexpectedEventReceived
    failGracefully DuplicateEvent = do
      logAttention_ "Duplicate event received."
      throwM DuplicateEventReceived
    failGracefully UnknownStage = do
      logAttention_ "Aggregator is in inconsistent state"
      throwM AggregatorSteppingFailed

    processStep eventId newAggregator NeedMoreEvents = do
      updateAggregatorState instanceId newAggregator eventId False
      logTrace_ "Aggregator needs more events to step."
    processStep eventId newAggregator (StateChange actions) = do
      updateAggregatorState instanceId newAggregator eventId True
      pushActions instanceId actions

    noInstance = do
      logAttention_ "Flow instance with this ID does not exist."
      throwM NoInstance
