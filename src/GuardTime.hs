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
import Control.Monad.Reader (ReaderT(..), runReaderT, ask)
import Control.Monad.Trans
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), ComposeSt, defaultLiftWith, defaultRestoreT, defaultLiftBaseWith, defaultRestoreM)
import System.Exit
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen
import Text.JSON.String
import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.ByteString.Lazy.UTF8 as BSL

import GuardTime.Class
import KontraPrelude
import Log
import Utils.IO

newtype GuardTimeConfT m a = GuardTimeConfT { unGuardTimeConfT :: ReaderT GuardTimeConf m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadIO, MonadTrans, MonadBase b, MonadThrow, MonadCatch, MonadMask)

instance MonadBaseControl b m => MonadBaseControl b (GuardTimeConfT m) where
  newtype StM (GuardTimeConfT m) a = StM { unStM :: ComposeSt GuardTimeConfT m a }
  liftBaseWith = defaultLiftBaseWith StM
  restoreM     = defaultRestoreM unStM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl GuardTimeConfT where
  newtype StT GuardTimeConfT m = StT { unStT :: StT (ReaderT GuardTimeConf) m }
  liftWith = defaultLiftWith GuardTimeConfT unGuardTimeConfT StT
  restoreT = defaultRestoreT GuardTimeConfT unStT
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
  liftIO $ readProcessWithExitCode' "java" a BSL.empty

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
    logInfo_ $ "GuardTime exit code " ++ show code
    when (not (BSL.null stdout)) $ do
      logInfo_ $ "GuardTime stdout  : " ++ BSL.toString stdout
    when (not (BSL.null stderr)) $ do
      logInfo_ $ "GuardTime errout  : " ++ BSL.toString stderr

  return code

-- Verification


digitallyExtend :: (MonadLog m, MonadIO m) => GuardTimeConf -> String -> m ExitCode
digitallyExtend conf inputFileName = do
  (code,stdout,stderr) <- invokeGuardtimeTool "PdfExtender"
             [ "-x", guardTimeExtendingServiceURL conf
             , "-f"
             , inputFileName
             ]
  when (code /= ExitSuccess) $ do
    logInfo_ $ "GuardTime exit code " ++ show code
    when (not (BSL.null stdout)) $ do
      logInfo_ $ "GuardTime stdout  : " ++ BSL.toString stdout
    when (not (BSL.null stderr)) $ do
      logInfo_ $ "GuardTime errout  : " ++ BSL.toString stderr

  return code

-- Verification


data VerifyResult = Valid GuardtimeSignature |
                    Invalid String           |
                    Problem String
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
                      liftM (fmap Problem) $ fromJSValueField "reason"
        return $ mvalid `mplus` minvalid `mplus` mproblem


instance ToJSValue VerifyResult where
    toJSValue (Valid gtsig)     =  runJSONGen $ do
                                        value "success" True
                                        value "time" (time gtsig)
                                        value "gateway" (gateway_id gtsig)
                                        value "extended" (extended gtsig)
    toJSValue (Invalid msg)     =  runJSONGen $ do
                                        value "success" False
                                        value "error"   False
                                        value "message" msg
    toJSValue (Problem msg)     = runJSONGen $ do
                                        value "success" False
                                        value "error"   True
                                        value "message" msg

verify :: (MonadLog m, MonadIO m) => GuardTimeConf -> String -> m VerifyResult
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
                Left s -> return $ Problem $ "GuardTime verification result bad format: " ++ s ++", stdout: " ++ BSL.toString stdout ++ ", stderr " ++ BSL.toString stderr
                Right json -> case fromJSValue json of
                                  Nothing -> do
                                      logInfo_ $ "GT parsing error " ++ BSL.toString stdout
                                      return $ Problem $ "GuardTime verification result parsing error"
                                  Just res -> return res
       _ -> return $ Problem $ "GuardTime verification failed: " ++ BSL.toString stderr
