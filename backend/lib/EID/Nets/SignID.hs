module EID.Nets.SignID (
    SignOrderUUID
  , newSignOrderUUID
  , parseSignOrderUUID
  , TrustSignMessageUUID
  , newTrustSignMessageUUID
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Base
import Control.Monad.IO.Class
import Data.Text as T
import Data.Typeable
import Data.UUID (UUID)
import Data.UUID.V1
import Log
import qualified Data.UUID as U

import DB
import KontraError
import Log.Identifier

newtype SignOrderUUID = SignOrderUUID UUID
  deriving (Eq, Ord)

instance Show SignOrderUUID where
  show (SignOrderUUID uuid) = U.toString uuid

instance Identifier SignOrderUUID where
  idDefaultLabel            = "nets_sign_order_id"
  idValue (SignOrderUUID k) = stringIdentifier . U.toString $ k

instance PQFormat SignOrderUUID where
  pqFormat    = pqFormat    @Text
  pqFormat0   = pqFormat0   @Text
  pqVariables = pqVariables @Text

instance FromSQL SignOrderUUID where
  type PQBase SignOrderUUID = PQBase Text
  fromSQL mbase = do
    t <- fromSQL mbase
    case U.fromText t of
      Nothing -> throwIO UUIDParseError
      Just u -> return . SignOrderUUID $ u

data UUIDParseError = UUIDParseError
  deriving (Show, Typeable)
instance Exception UUIDParseError

instance ToSQL SignOrderUUID where
  type PQDest SignOrderUUID = PQDest Text
  toSQL (SignOrderUUID t) = toSQL . U.toText $ t

newtype TrustSignMessageUUID = TrustSignMessageUUID UUID
  deriving (Eq, Ord)

instance Show TrustSignMessageUUID where
  show (TrustSignMessageUUID uuid) = U.toString uuid

instance Identifier TrustSignMessageUUID where
  idDefaultLabel                   = "nets_sign_message_id"
  idValue (TrustSignMessageUUID t) = stringIdentifier . U.toString $ t

newSignOrderUUID :: (MonadBase IO m, MonadIO m, MonadLog m) => m SignOrderUUID
newSignOrderUUID = SignOrderUUID <$> nextUUIDWrapper

newTrustSignMessageUUID :: (MonadBase IO m, MonadIO m, MonadLog m) => m TrustSignMessageUUID
newTrustSignMessageUUID = TrustSignMessageUUID <$> nextUUIDWrapper

nextUUIDWrapper :: (MonadBase IO m, MonadIO m, MonadLog m) => m UUID
nextUUIDWrapper = do
  muuid <- liftIO nextUUID
  case muuid of
    Nothing -> do
      logAttention_ "Cannot get nextUUID from system"
      internalError
    Just uuid -> return uuid

parseSignOrderUUID :: Text -> SignOrderUUID
parseSignOrderUUID t = SignOrderUUID $ fromMaybe
  (unexpectedError $ "Cannot parse OrderID:" <+> t)
  (U.fromString $ T.unpack t)
