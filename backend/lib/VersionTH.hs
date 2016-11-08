module VersionTH (versionID) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import KontraPrelude
import Version

{-# NOINLINE versionID #-}
versionID :: String
versionID = $( litE =<< (fmap stringL $ runIO $ genVersionID) )
