module GuardTime (
    module GuardTime.Class
  , digitallySign
  , digitallyExtend
  , verify
  , VerifyResult(..)
  , GuardtimeSignature(..)
  , GuardTimeConf(..)
  , GuardTimeConfMonad(..)
  , GuardTimeConfT(..)
  , runGuardTimeConfT
  , privateGateway
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import Control.Monad.Trans
import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith, defaultLiftWith, defaultRestoreM, defaultRestoreT)
import Data.Time
import Log
import System.Exit
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen hiding (object)
import Text.JSON.String
import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.ByteString.Lazy.UTF8 as BSL

import GuardTime.Class
import KontraPrelude
import Log.Utils

newtype GuardTimeConfT m a = GuardTimeConfT { unGuardTimeConfT :: ReaderT GuardTimeConf m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadIO, MonadTrans, MonadBase b, MonadThrow, MonadCatch, MonadMask)

instance MonadBaseControl b m => MonadBaseControl b (GuardTimeConfT m) where
  type StM (GuardTimeConfT m) a = ComposeSt GuardTimeConfT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl GuardTimeConfT where
  type StT GuardTimeConfT m = StT (ReaderT GuardTimeConf) m
  liftWith = defaultLiftWith GuardTimeConfT unGuardTimeConfT
  restoreT = defaultRestoreT GuardTimeConfT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance Monad m => GuardTimeConfMonad (GuardTimeConfT m) where
  getGuardTimeConf = GuardTimeConfT ask

runGuardTimeConfT :: GuardTimeConf -> GuardTimeConfT m a -> m a
runGuardTimeConfT ts m = runReaderT (unGuardTimeConfT m) ts

invokeGuardtimeTool :: MonadIO m => String -> [String] -> m (ExitCode, BSL.ByteString, BSL.ByteString)
invokeGuardtimeTool tool args = do
  let a = ([ "-jar", "GuardTime/" ++ tool ++ ".jar"
             ] ++ args)
  liftIO $ readProcessWithExitCode "java" a BSL.empty

digitallySign :: (MonadLog m, MonadIO m) => GuardTimeConf -> String -> m ExitCode
digitallySign conf inputFileName = do
  (code,stdout,stderr) <- invokeGuardtimeTool "PdfSigner"
             [ "-i"
             , "-n", "Scrive"
             , "-s", guardTimeURL conf
             , "-f"
             , inputFileName
             ]
  when (code /= ExitSuccess) $ do
    logAttention "GuardTime signing failed" $ object [
        "exit_code" .= show code
      , "stdout" `equalsExternalBSL` stdout
      , "stderr" `equalsExternalBSL` stderr
      ]
  return code

-- Verification

digitallyExtend :: (MonadLog m, MonadIO m) => GuardTimeConf -> String -> m ExitCode
digitallyExtend conf inputFileName = do
  startTime <- currentTime
  (code,stdout,stderr) <- invokeGuardtimeTool "PdfExtender"
             [ "-x", guardTimeExtendingServiceURL conf
             , "-f"
             , inputFileName
             ]
  finishTime <- currentTime
  let elapsedTime = realToFrac $ diffUTCTime finishTime startTime :: Double
  case code of
    ExitFailure _ -> logAttention "GuardTime extending failed" $ object [
        "exit_code" .= show code
      , "stdout" `equalsExternalBSL` stdout
      , "stderr" `equalsExternalBSL` stderr
      , "elapsed_time" .= elapsedTime
      ]
    ExitSuccess -> logInfo "GuardTime digitallyExtend completed successfully" $ object [
        "elapsed_time" .= elapsedTime
      ]

  return code

-- Verification


data VerifyResult = Valid GuardtimeSignature |
                    Invalid String           |
                    Problem BSL.ByteString BSL.ByteString String
  deriving Show

data GuardtimeSignature =
  GuardtimeSignature { time :: String
                     , gateway_id :: String
                     , gateway_name :: String
                     , extended :: Bool
                     , extensible :: Bool
                     }
  deriving Show

-- | Signature indicates that a gateway exclusive to Scrive was used when signing
privateGateway :: GuardtimeSignature -> Bool
privateGateway gsig  = "Scrive" `elem` words (gateway_name gsig)

instance FromJSValue VerifyResult where
    fromJSValue = do
        mvalid   <- fromJSValueFieldCustom "valid" $ do
                      time <- fromJSValueField "time"
                      gid <- fromJSValueField "gateway_id"
                      gname <- fromJSValueField "gateway_name"
                      extended <- fromJSValueField "extended"
                      extensible <- fromJSValueField "extensible"
                      return $ Valid <$> (GuardtimeSignature <$> time <*> gid <*> gname <*> extended <*> extensible)
        minvalid <- fromJSValueFieldCustom "invalid" $ do
                      liftM (fmap Invalid) $ fromJSValueField "reason"
        mproblem <- fromJSValueFieldCustom "error" $ do
                      liftM (fmap $ Problem BSL.empty BSL.empty) $ fromJSValueField "reason"
        return $ mvalid `mplus` minvalid `mplus` mproblem


instance ToJSValue VerifyResult where
    toJSValue (Valid gtsig) =  runJSONGen $ do
      value "success" True
      value "time" (time gtsig)
      value "gateway" (gateway_id gtsig)
      value "extended" (extended gtsig)
    toJSValue (Invalid msg) =  runJSONGen $ do
      value "success" False
      value "error"   False
      value "message" msg
    toJSValue (Problem stdout stderr msg) = runJSONGen $ do
      value "success" False
      value "error"   True
      value "message" $ msg
        ++ if BSL.null stdout && BSL.null stderr
          then []
          else " (stdout: " ++ BSL.toString stdout ++ ", stderr: " ++ BSL.toString stderr ++ ")"

verify :: MonadIO m => GuardTimeConf -> String -> m VerifyResult
verify conf inputFileName = do
  (code,stdout,stderr) <- invokeGuardtimeTool "PdfVerifier"
             [ "-j"
             , "-x", guardTimeExtendingServiceURL conf       -- http://127.0.0.1:8080/gt-extendingservice
             , "-p", guardTimeControlPublicationsURL conf    -- http://127.0.0.1:8080/gt-controlpublications.bin
             , "-f"
             , inputFileName
             ]
  case code of
    ExitSuccess -> do
      case (runGetJSON readJSObject $ BSL.toString stdout) of
        Left s -> return . Problem stdout stderr $ "GuardTime verification result bad format: " ++ s
        Right json -> case fromJSValue json of
          Nothing -> return $ Problem stdout stderr "GuardTime verification result parsing error"
          Just res -> return res
    _ -> return $ Problem stdout stderr "GuardTime verification failed"
