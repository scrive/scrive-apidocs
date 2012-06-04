

module GuardTime
       ( digitallySign
       , GuardTimeConf(..)
       ) where

import qualified Data.ByteString.Lazy as BSL hiding (length)
import qualified Data.ByteString.Lazy.UTF8 as BSL 
import Misc
import Data.List
import Control.Monad.IO.Class
import System.Exit
import qualified Log as Log

data GuardTimeConf = GuardTimeConf
    { guardTimeURL ::  String
    } deriving (Eq, Ord, Show, Read)


guardTimeJars :: [String]
guardTimeJars =
  [ "GTJavaPDF-0.3.1.jar"
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
             , "-s", guardTimeURL conf
             , "-f"
             , inputFileName
             ]
  (code,_stdout,_stderr) <- liftIO $ readProcessWithExitCode' "java" args BSL.empty
  Log.debug $ "GT stdout  : " ++ BSL.toString _stdout
  Log.debug $ "GT errout  : " ++ BSL.toString _stdout
  
  return code
