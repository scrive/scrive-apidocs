module Flow.ActionConsumers where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG
import Data.Aeson
import Data.Aeson.Casing
import Database.PostgreSQL.PQTypes
import GHC.Generics
import Log
import Text.StringTemplates.Templates (TemplatesMonad)

import DB hiding (JSON(..))
import Doc.Action (commonDocumentClosingActions)
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model.Query (GetDocumentByDocumentID(..))
import Doc.Signing.Model ()
import EventStream.Class
import File.Storage
import Flow.Id
import Flow.Model
import GuardTime (GuardTimeConfMonad)
import MailContext
import User.Model (UserID)
import qualified Flow.HighTongue as HighTongue
import qualified Flow.Machinize as Machinize

data ConsumableAction
  = Notify
      { users :: [UserID] -- TODO: Add email and phone number
      , message :: Text
      }
  | CloseAll
      { documentIds :: [DocumentID]
      }
  | Fail
  deriving (Show, Generic)

-- For logging
instance ToJSON ConsumableAction where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase }

toConsumableAction
  :: (MonadThrow m, MonadDB m) => InstanceId -> Machinize.LowAction -> m ConsumableAction
toConsumableAction instanceId = \case
  Machinize.CloseAll -> CloseAll <$> selectDocumentsByInstanceId instanceId

  -- TODO: Convert Action Notify to ConsumableAction Notify
  Machinize.Action (HighTongue.Notify _ _) -> undefined

  Machinize.Fail     -> pure Fail

consumeFlowAction
  :: ( CryptoRNG m
     , DocumentMonad m
     , GuardTimeConfMonad m
     , MailContextMonad m
     , MonadBaseControl IO m
     , MonadEventStream m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , TemplatesMonad m
     )
  => ConsumableAction
  -> m ()
consumeFlowAction action = do
  logInfo "Consuming Flow action: " action
  case action of
    CloseAll documentIds -> forM_ documentIds $ \docId -> do
      doc <- dbQuery $ GetDocumentByDocumentID docId
      withDocument doc $ commonDocumentClosingActions doc

    -- TODO: Implement the Notify consumer
    Notify _ _ -> undefined

    Fail       -> logInfo "Flow process failed: " action
