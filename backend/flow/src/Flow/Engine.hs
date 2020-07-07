{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
module Flow.Engine
  ( processMachinizeEvent
  , finalizeFlow
  , processEvent
  , compile
  , parseTongue
  , translate
  ) where

import Control.Monad.Catch
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
import Flow.HighTongue
import Flow.Id (InstanceId)
import Flow.Machinize
import Flow.Model as Model
import Flow.Model.Types
import Flow.Process
import GuardTime (GuardTimeConfMonad)
import MailContext
import qualified Flow.Transducer as Transducer

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
    = NoAssociatedDocument
    | NoAssociatedUser
    | NoInstance
    | UnexpectedEventReceived
    | TransducerSteppingFailed Transducer.Error
    | DuplicateEventReceived
    | NotImplemented Text
    | InvalidProcess [ValidationError]
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
  -> m ()
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
      throwM NoAssociatedDocument
    noUserNameFound = do
      logAttention_ "Signatory not associated with flow intance."
      throwM NoAssociatedUser

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
  -> m ()
pushActions instanceId actions = do
  consumableActions <- mapM (toConsumableAction instanceId) actions
  mapM_ consumeFlowAction consumableActions

finalizeFlow :: (MonadLog m, MonadDB m, MonadThrow m) => m ()
finalizeFlow = throwM $ NotImplemented "Finalizing flow"

-- TODO use ExceptT
parseTongue :: (MonadLog m, MonadThrow m) => Process -> m HighTongue
parseTongue = either throwDSLValidationError' pure . decodeHighTongue
  where
    throwDSLValidationError' errs = do
      logAttention_ $ "Flow DSL compatibility broken, parsing failed: " <> showt errs
      throwM $ InvalidProcess errs

translate :: (MonadLog m, MonadThrow m) => HighTongue -> m Machine
translate = either throwDSLValidationError' pure . machinize
  where
    throwDSLValidationError' errs = do
      logAttention_
        $  "Flow DSL compatibility broken, translation to state machine failed: "
        <> showt errs
      throwM $ InvalidProcess errs

compile :: (MonadLog m, MonadThrow m) => Process -> m Machine
compile = parseTongue >=> translate

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
  -> m ()
processMachinizeEvent instanceId eventInfo = do
  -- TODO Should we store store duplicate/unknown events?
  -- Currently they are being thrown away since this code runs in a single transaction
  -- that is aborted on errors.
  eventId      <- insertEvent $ toInsertEvent instanceId eventInfo
  fullInstance <- fromMaybeM noInstance $ selectFullInstance instanceId
  let aggregator = instanceToAggregator fullInstance
  machine <- compile $ fullInstance ^. #template % #process
  let (aggregatorResult, newAggregator) = runAggregatorStep eventInfo aggregator machine
  either failGracefully processStep aggregatorResult
  let stateChange = currentState aggregator /= currentState newAggregator
  updateAggregatorState instanceId newAggregator eventId stateChange
  where
    -- TODO: Think of some error monad. MonadFail is ugly.
    -- TODO: This fail thing may leak some error to user.
    -- TODO: Improve error messages.
    failGracefully :: (MonadLog m, MonadDB m, MonadThrow m) => AggregatorError -> m ()
    failGracefully UnknownEventInfo = do
      logAttention_ "Unexpected event received."
      throwM UnexpectedEventReceived
    failGracefully DuplicateEvent = do
      logAttention_ "Duplicate event received."
      throwM DuplicateEventReceived
    failGracefully (TransducerError err) = do
      logAttention_ $ "Transducer is in inconsistent state: " <> showt err
      throwM $ TransducerSteppingFailed err


    processStep NeedMoreEvents        = logTrace_ "Aggregator needs more events to step."
    processStep (StateChange actions) = pushActions instanceId actions
    processStep (FinalState  actions) = do
      pushActions instanceId actions
      finalizeFlow

    noInstance = do
      logAttention_ "Flow instance with this ID does not exist."
      throwM NoInstance
