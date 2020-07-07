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
import qualified Data.Map as Map

import DB hiding (JSON(..))
import Doc.Action (commonDocumentClosingActions)
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model.Query (GetDocumentByDocumentID(..))
import Doc.Signing.Model ()
import EventStream.Class
import File.Storage
import Flow.Id
import Flow.Model as Model
import Flow.Model.Types as Model
import Flow.Names
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
  | Close
      { documentIds :: [DocumentID]
      }
  | Fail -- Expected failure of the process, e.g. user rejected a document
      { instanceId :: InstanceId
      }
  deriving (Show, Generic)

-- For logging
instance ToJSON ConsumableAction where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase }

toConsumableAction
  :: (MonadThrow m, MonadDB m) => InstanceId -> Machinize.LowAction -> m ConsumableAction
toConsumableAction instanceId = \case
  Machinize.Action (HighTongue.Close docNames) -> do
    docMap <-
      Map.fromList
      .   fmap (fmap unsafeDocumentID)
      <$> Model.selectInstanceKeyValues instanceId Document

    -- TODO Use DocumentName and get rid of `fromName`
    pure $ Close (mapMaybe ((`Map.lookup` docMap) . fromName) docNames)

  -- TODO: Convert Action Notify to ConsumableAction Notify
  Machinize.Action (HighTongue.Notify _ _) -> undefined

  Machinize.Fail -> pure $ Fail instanceId

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
    Close { documentIds } -> do
      documents <- mapM (dbQuery . GetDocumentByDocumentID) documentIds
      mapM_ commonDocumentClosingActions documents

    -- TODO: Implement the Notify consumer
    Notify _ _ -> undefined

    Fail _     -> logInfo "Flow process failed: " action

