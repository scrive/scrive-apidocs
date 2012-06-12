

module GuardTime
       ( digitallySign
       , verify
       , VerifyResult
       , GuardTimeConf(..)
       ) where

import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.ByteString.Lazy.UTF8 as BSL 
import Misc
import Data.List
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
    } deriving (Eq, Ord, Show, Read)


guardTimeJars :: [String]
guardTimeJars =
  [ "GTJavaPDF-0.3.1.jar"
  , "PdfBundler-0.3.1.jar"
  , "PdfExtender-0.3.1.jar"
  , "PdfSigner-0.3.1.jar"
  , "PdfVerifier-0.3.1.jar"
  , "bcprov-ext-jdk15on-146.jar"
  , "iText-2.1.7.jar"
  , "GTJavaSDK-0.4.4.jar"
  , "bcprov-jdk15on-146.jar"
  ]

digitallySign :: GuardTimeConf -> String -> IO ExitCode
digitallySign conf inputFileName = do
  let args = [ "-classpath"
             , intercalate ":" (map ("GuardTime/" ++) guardTimeJars)
             , "com.guardtime.pdftools.PdfStamper"
             , "-i"
             , "-n", " Scrive "
             , "-s", guardTimeURL conf
             , "-f"
             , inputFileName
             ]
  (code,stdout,stderr) <- liftIO $ readProcessWithExitCode' "java" args BSL.empty
  Log.debug $ "GT stdout  : " ++ BSL.toString stdout
  Log.debug $ "GT errout  : " ++ BSL.toString stderr
  
  return code

-- Verification


data VerifyResult = Valid String String      |
                    Invalid String           |
                    Problem String

instance FromJSValue VerifyResult where
    fromJSValueM = do
        mvalid   <- fromJSValueFieldCustom "valid" $ do
                      time <- fromJSValueField "time" 
                      gid <- fromJSValueField "gateway_id"
                      return $ liftM2 Valid time gid
        minvalid <- fromJSValueFieldCustom "invalid" $ do
                      liftM (fmap Invalid) $ fromJSValueField "reason"
        mproblem <- fromJSValueFieldCustom "error" $ do
                      liftM (fmap Problem) $ fromJSValueField "reason"
        return $ mvalid `mplus` minvalid `mplus` mproblem


instance ToJSValue VerifyResult where
    toJSValue (Valid t g)       =  runJSONGen $ do
                                        value "success" True
                                        value "time" t
                                        value "gateway" g
    toJSValue (Invalid msg)     =  runJSONGen $ do
                                        value "success" False
                                        value "error"   False
                                        value "message" msg
    toJSValue (Problem msg)     = runJSONGen $ do
                                        value "success" False
                                        value "error"   True
                                        value "message" msg

verify :: String -> IO VerifyResult
verify inputFileName = do
  let args = [ "-classpath"
             , intercalate ":" (map ("GuardTime/" ++) guardTimeJars)
             , "com.guardtime.pdftools.PdfVerifier"
             , "-j"
             , "-f"
             , inputFileName
             ]
  (code,stdout,stderr) <- liftIO $ readProcessWithExitCode' "java" args BSL.empty
  case code of
       ExitSuccess -> do
           case (runGetJSON readJSObject $ BSL.toString stdout) of
                Left s -> return $ Problem $ "GuardTime verification result bad format: " ++ s
                Right json -> case fromJSValue json of
                                  Nothing -> do
                                      Log.debug $ "GT parsing error " ++ BSL.toString stdout
                                      return $ Problem $ "GuardTime verification result parsing error"
                                  Just res -> return res   
       _ -> return $ Problem $ "GuardTime verification failed: " ++ BSL.toString stderr



  