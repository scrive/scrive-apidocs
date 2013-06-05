

module GuardTime
       ( digitallySign
       , digitallyExtend
       , verify
       , VerifyResult(Valid)
       , GuardtimeSignature(..)
       , GuardTimeConf(..)
       , privateGateway
       ) where

import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Utils.IO
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class
import System.Exit
import qualified Log as Log
import Text.JSON
import Text.JSON.String
import Text.JSON.FromJSValue
import Text.JSON.ToJSValue
import Text.JSON.Gen
import Control.Monad

data GuardTimeConf = GuardTimeConf
    { guardTimeURL ::  String
    , guardTimeExtendingServiceURL :: String
    , guardTimeControlPublicationsURL :: String
    } deriving (Eq, Ord, Show, Read)


invokeGuardtimeTool :: MonadIO m => String -> [String] -> m (ExitCode, BSL.ByteString, BSL.ByteString)
invokeGuardtimeTool tool args = do
  let a = ([ "-jar", "GuardTime/" ++ tool ++ ".jar"
             ] ++ args)
  liftIO $ readProcessWithExitCode' "java" a BSL.empty

digitallySign :: GuardTimeConf -> String -> IO ExitCode
digitallySign conf inputFileName = do
  (code,stdout,stderr) <- invokeGuardtimeTool "PdfSigner"
             [ "-i"
             , "-n", "Scrive"
             , "-s", guardTimeURL conf
             , "-f"
             , inputFileName
             ]
  when (code /= ExitSuccess) $ do
    Log.debug $ "GuardTime exit code " ++ show code
    when (not (BSL.null stdout)) $ do
      Log.debug $ "GuardTime stdout  : " ++ BSL.toString stdout
    when (not (BSL.null stderr)) $ do
      Log.debug $ "GuardTime errout  : " ++ BSL.toString stderr

  return code

-- Verification


digitallyExtend :: GuardTimeConf -> String -> IO ExitCode
digitallyExtend conf inputFileName = do
  (code,stdout,stderr) <- invokeGuardtimeTool "PdfExtender"
             [ "-x", guardTimeExtendingServiceURL conf
             , "-f"
             , inputFileName
             ]
  when (code /= ExitSuccess) $ do
    Log.debug $ "GuardTime exit code " ++ show code
    when (not (BSL.null stdout)) $ do
      Log.debug $ "GuardTime stdout  : " ++ BSL.toString stdout
    when (not (BSL.null stderr)) $ do
      Log.debug $ "GuardTime errout  : " ++ BSL.toString stderr

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
    fromJSValueM = do
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

verify :: GuardTimeConf -> String -> IO VerifyResult
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
                                      Log.debug $ "GT parsing error " ++ BSL.toString stdout
                                      return $ Problem $ "GuardTime verification result parsing error"
                                  Just res -> return res
       _ -> return $ Problem $ "GuardTime verification failed: " ++ BSL.toString stderr
