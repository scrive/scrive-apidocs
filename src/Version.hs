{-# LANGUAGE TemplateHaskell #-}
module Version (versionID, silenceUnusedImport) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.Environment
import Control.Exception as E
import Data.List

{-# NOINLINE versionID #-}
versionID :: String
versionID = $( do
                  ver <- fmap stringL $ runIO $ do
                           let catchIO :: IO a -> (E.IOException -> IO a) -> IO a
                               catchIO = E.catch
                           buildNumber <- catchIO (getEnv "BUILD_NUMBER") (const (return "build_number"))
                           buildVcsNumber <- catchIO (getEnv "BUILD_VCS_NUMBER") (const (return "build_vcs_number"))
                           buildDate <- catchIO (getEnv "BUILD_DATE") (const (return "build_date"))

                           return $ intercalate "." [buildDate, buildNumber, buildVcsNumber]
                  litE ver
   )

-- Due to how Template Haskell has its phases in the second phase we
-- get error about unused imports from Data.List. Lets silence that
silenceUnusedImport :: forall a. [a] -> [[a]] -> [a]
silenceUnusedImport = intercalate
