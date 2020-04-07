{-# LANGUAGE TemplateHaskell #-}
module VersionTH (versionID) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import Version

{-# NOINLINE versionID #-}
versionID :: String
versionID = $( litE =<< fmap stringL (runIO genVersionID) )
