{-# LANGUAGE TemplateHaskell #-}
module Version (versionID) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.Environment
import Control.Exception as E

{-# NOINLINE versionID #-}
versionID :: String
versionID = $( litE =<< (fmap stringL $ runIO $ do
                 let catchIO :: IO a -> (E.IOException -> IO a) -> IO a
                     catchIO = E.catch
                 buildNumber <- catchIO (getEnv "BUILD_NUMBER") (const (return "build_number"))
                 buildVcsNumber <- catchIO (getEnv "BUILD_VCS_NUMBER") (const (return "build_vcs_number"))
                 buildDate <- catchIO (getEnv "BUILD_DATE") (const (return "build_date"))

                 return $ buildDate ++ "." ++ buildNumber ++ "." ++ buildVcsNumber)
   )
