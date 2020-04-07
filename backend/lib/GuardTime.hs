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
import Control.Monad.Trans.Control
  ( ComposeSt, MonadBaseControl(..), MonadTransControl(..), defaultLiftBaseWith
  , defaultLiftWith, defaultRestoreM, defaultRestoreT
  )
import Data.Time
import Log
import System.Directory (removeFile)
import System.Exit
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen hiding (object)
import Text.JSON.String
import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.ByteString.Lazy.UTF8 as BSL

import GuardTime.Class
import Log.Identifier
import Log.Utils
import Text.JSON.Convert

newtype GuardTimeConfT m a = GuardTimeConfT { unGuardTimeConfT :: ReaderT GuardTimeConf m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadPlus, MonadIO, MonadTrans, MonadBase b, MonadThrow, MonadCatch, MonadMask)

data GuardTimeCredentialsType = GTExtendingCredentials | GTSigningCredentials

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

invokeGuardtimeTool
  :: (MonadIO m, MonadMask m)
  => GuardTimeConf
  -> GuardTimeCredentialsType
  -> String
  -> [String]
  -> m (ExitCode, BSL.ByteString, BSL.ByteString)
invokeGuardtimeTool conf credentials tool args = do
  withGuardtimeConf conf credentials $ \confPath -> do
    let a = ["-jar", "GuardTime/" ++ tool ++ ".jar", "-c", confPath] ++ args
    liftIO $ readProcessWithExitCode "java" a BSL.empty

withGuardtimeConf
  :: (MonadIO m, MonadMask m)
  => GuardTimeConf
  -> GuardTimeCredentialsType
  -> (FilePath -> m (ExitCode, BSL.ByteString, BSL.ByteString))
  -> m (ExitCode, BSL.ByteString, BSL.ByteString)
withGuardtimeConf GuardTimeConf {..} credentials action = do
  let (login, key) = case credentials of
        GTSigningCredentials -> (guardTimeSigningLoginUser, guardTimeSigningLoginKey)
        GTExtendingCredentials ->
          (guardTimeExtendingLoginUser, guardTimeExtendingLoginKey)
  withSystemTempFile "guartime.conf" $ \confpath confhandle -> do
    let confstr =
          "signer.url="
            ++ guardTimeSigningServiceURL
            ++ "\n"
            ++ "extender.url="
            ++ guardTimeExtendingServiceURL
            ++ "\n"
            ++ "publicationsfile.url="
            ++ guardTimeControlPublicationsURL
            ++ "\n"
            ++ "publicationsfile.constraint=E=publications@guardtime.com\n"
            ++ "login.user="
            ++ login
            ++ "\n"
            ++ "login.key="
            ++ key
    liftIO $ hPutStr confhandle confstr
    liftIO $ hClose confhandle
    result <- action confpath
    liftIO $ removeFile confpath
    return result

digitallySign
  :: (MonadLog m, MonadIO m, MonadMask m)
  => GuardTimeConf
  -> String
  -> m (ExitCode, BSL.ByteString, BSL.ByteString)
digitallySign conf inputFileName = do
  (code, stdout, stderr) <- invokeGuardtimeTool conf
                                                GTSigningCredentials
                                                "pdf-signer"
                                                ["-n", "Scrive", "-f", inputFileName]
  -- The ExitFailure is here "just in case". The new KSI GT Pdftoolkit binary is always returning ExitSuccess.
  when (code /= ExitSuccess) $ do
    logAttention "GuardTime signing failed" $ object
      [ "exit_code" .= show code
      , "stdout" `equalsExternalBSL` stdout
      , "stderr" `equalsExternalBSL` stderr
      ]
  return (code, stdout, stderr)

-- Extending

digitallyExtend
  :: (MonadLog m, MonadIO m, MonadMask m)
  => GuardTimeConf
  -> String
  -> m (ExitCode, BSL.ByteString, BSL.ByteString)
digitallyExtend conf inputFileName = do
  startTime              <- currentTime
  (code, stdout, stderr) <- invokeGuardtimeTool conf
                                                GTExtendingCredentials
                                                "pdf-extender"
                                                ["-f", inputFileName]
  finishTime <- currentTime
  let elapsedTime = realToFrac $ diffUTCTime finishTime startTime :: Double
  case code of
    -- The ExitFailure is here "just in case". The new KSI GT Pdftoolkit binary is always returning ExitSuccess.
    ExitFailure _ -> logAttention "GuardTime extending failed" $ object
      [ "exit_code" .= show code
      , "stdout" `equalsExternalBSL` stdout
      , "stderr" `equalsExternalBSL` stderr
      , "elapsed_time" .= elapsedTime
      ]
    ExitSuccess -> logInfo "GuardTime digitallyExtend completed successfully"
      $ object ["elapsed_time" .= elapsedTime]

  return (code, stdout, stderr)

-- Verification


data VerifyResult = Valid GuardtimeSignature |
                    Invalid String           |
                    Problem BSL.ByteString BSL.ByteString String
  deriving Show

instance Loggable VerifyResult where
  logValue = jsonToAeson . toJSValue
  logDefaultLabel _ = "guard_time_verify_result"

data GuardtimeSignature =
  GuardtimeSignature { time :: String
                     , extended :: Bool
                     , extensible :: Bool
                     }
  deriving Show

-- | Signature indicates that a gateway exclusive to Scrive was used when signing
privateGateway :: GuardtimeSignature -> Bool
privateGateway _ = True -- Scrive always uses an internal gateway

instance FromJSValue VerifyResult where
  fromJSValue = do
    mvalid <- fromJSValueFieldCustom "valid" $ do
      time       <- fromJSValueField "time"
      extended   <- fromJSValueField "extended"
      extensible <- fromJSValueField "extensible"
      lastRev    <- fromJSValueField "last_revision"
      return $ case lastRev of
        Just ("true" :: String) ->
          Valid <$> (GuardtimeSignature <$> time <*> extended <*> extensible)
        _ -> Just $ Invalid "not last revision"
    minvalid <- fromJSValueFieldCustom "invalid" $ do
      fmap (Invalid . (show :: JSValue -> String)) <$> fromJSValueField "reason"
    mproblem <- fromJSValueFieldCustom "error" $ do
      fmap (Problem BSL.empty BSL.empty) <$> fromJSValueField "reason"
    return $ mvalid `mplus` minvalid `mplus` mproblem


instance ToJSValue VerifyResult where
  toJSValue (Valid gtsig) = runJSONGen $ do
    value "success"  True
    value "time"     (time gtsig)
    value "extended" (extended gtsig)
  toJSValue (Invalid msg) = runJSONGen $ do
    value "success" False
    value "error"   False
    value "message" msg
  toJSValue (Problem stdout stderr msg) = runJSONGen $ do
    value "success" False
    value "error"   True
    value "message" $ msg ++ if BSL.null stdout && BSL.null stderr
      then []
      else
        " (stdout: " ++ BSL.toString stdout ++ ", stderr: " ++ BSL.toString stderr ++ ")"


verify :: (MonadIO m, MonadMask m) => GuardTimeConf -> FilePath -> m VerifyResult
verify conf inputFileName = do
  (code, stdout, stderr) <- withGuardtimeConf conf GTExtendingCredentials $ \confPath ->
    do
      let a =
            [ "-Djava.util.logging.config.file=/dev/null"
            , "-cp"
            , "GuardTime/slf4j-jdk14-1.7.28.jar:GuardTime/pdf-verifier.jar"
            , "com.guardtime.pdftools.PdfVerifier"
            , "-c"
            , confPath
            , "-j"
            , "-f"
            , inputFileName
            ]
      liftIO $ readProcessWithExitCode "java" a BSL.empty
  -- if stdout is empty, output json with the error is in stderr
  let output = if BSL.null stdout then stderr else stdout
  case code of
    ExitSuccess -> do
      case runGetJSON readJSObject $ BSL.toString output of
        Left s ->
          return
            .  Problem stdout stderr
            $  "GuardTime verification result bad format: "
            ++ s
        Right json -> case fromJSValue json of
          Nothing ->
            return $ Problem stdout stderr "GuardTime verification result parsing error"
          Just res -> return res
    _ -> return $ Problem stdout stderr "GuardTime verification failed"
