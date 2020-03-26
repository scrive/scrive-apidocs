module EventStream.Kinesis
  ( KinesisConf(..)
  , KinesisT
  , runKinesisT
  , module EventStream.Class
  )
  where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Aeson
import Data.ByteString hiding (drop)
import Data.Unjson
import Data.Word
import GHC.Generics
import Log
import Optics
import qualified Control.Monad.Fail as MF
import qualified Control.Monad.Trans.AWS as AWS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.AWS.Data.Text as AWS
import qualified Network.AWS.Kinesis.PutRecord as AWS

import Data.ByteString.Utils
import DB
import EventStream.Class
import FileStorage.Class
import Log.Amazon

data KinesisConf = KinesisConf
    { kinesisConfRegion :: !AWS.Region
    , kinesisConfAccessKey :: !ByteString
    , kinesisConfSecretKey :: !ByteString
    }
  deriving (Show, Eq)


instance Unjson KinesisConf where
  unjsonDef =
    objectOf
      $   KinesisConf
      <$> fieldBy
            "event_stream_region"
            kinesisConfRegion
            "In which data center is the event stream located"
            (unjsonInvmapR (either fail return . AWS.fromText) AWS.toText unjsonDef)
      <*> fieldBy "event_stream_access_key"
                  kinesisConfAccessKey
                  "Stream access key"
                  unjsonByteString
      <*> fieldBy "event_stream_secret_key"
                  kinesisConfSecretKey
                  "Stream secret key"
                  unjsonByteString

newtype KinesisT m a = KinesisT { unKinesis :: ReaderT (Maybe AWS.Env) m a }
  deriving ( Alternative, Applicative, Functor, Monad, MonadDB, MonadIO
           , MonadLog, CryptoRNG, MonadTrans, MonadPlus, MonadBase b
           , MonadBaseControl b, MonadThrow, MonadCatch, MonadMask, MF.MonadFail
           , MonadTransControl, MonadFileStorage )

data Envelope a = Envelope
    { envelopeVersion :: Word32
    , envelopePayload :: a
    }
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Envelope a) where
  toJSON Envelope {..} =
    object ["version" .= envelopeVersion, "payload" .= envelopePayload]

defaultVersion :: Word32
defaultVersion = 0

instance (MonadThrow m, MonadIO m, MonadCatch m, MonadLog m)
  => MonadEventStream (KinesisT m) where
  pushEvent streamId parititionKey message = KinesisT $ do
    maybeEnv <- ask
    maybe
        (pure ())
        (\env ->
          void . liftIO . AWS.runResourceT . AWS.runAWST env . AWS.send $ AWS.putRecord
            streamId
            payload
            parititionKey
        )
        maybeEnv
      -- Database is source of truth and we don't want to fail if `kinesis` is
      -- unavailable (related to Chargeable Items). The exception should be
      -- re-thrown as soon as we determine the kisesis is stable for our use.
      `catch` \(e :: AWS.Error) -> do
                logInfo "Failed to push event to kinesis stream"
                  $ object
                      [ "error" .= show e
                      , "parititionKey" .= parititionKey
                      , "streamId" .= streamId
                      ]
    where payload = BSL.toStrict . encode $ Envelope defaultVersion message

runKinesisT
  :: (MonadCatch m, MonadIO m, MonadLog m) => Maybe KinesisConf -> KinesisT m a -> m a
runKinesisT Nothing m = do
  logAttention_ "Kinesis stream is disabled."
  runReaderT (unKinesis m) Nothing
runKinesisT (Just KinesisConf {..}) m = do
  logger <- awsLogger
  env    <-
    (lensVL AWS.envRegion .~ kinesisConfRegion)
    .   (lensVL AWS.envLogger .~ logger)
    <$> getAWSEnv
  runReaderT (unKinesis m) $ Just env
  where
    getAWSEnv = AWS.newEnv $ AWS.FromKeys (AWS.AccessKey kinesisConfAccessKey)
                                          (AWS.SecretKey kinesisConfSecretKey)
