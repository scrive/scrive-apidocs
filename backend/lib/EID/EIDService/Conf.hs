{-# LANGUAGE TemplateHaskell #-}
module EID.EIDService.Conf ( EIDServiceConf(..), unjsonEIDServiceConf ) where

import Data.Unjson
import Optics.TH
import qualified Data.Text as T

-- | A datatype for EIDService configuration data.
data EIDServiceConf = EIDServiceConf
  { eidServiceUrl :: T.Text
  , eidServiceToken :: T.Text
  , eidUseForDK :: Bool
  , eidUseForNOView :: Bool
  } deriving (Show, Eq, Ord)

unjsonEIDServiceConf :: UnjsonDef EIDServiceConf
unjsonEIDServiceConf =
  objectOf
    $   EIDServiceConf
    <$> field "url"   eidServiceUrl   "EIDService url"
    <*> field "token" eidServiceToken "EIDService token"
    <*> fieldDef "useForDK"
                 False
                 eidUseForDK
                 "Use EIDService for Danish NemID for auth to view"
    <*> fieldDef "useForNOView"
                 False
                 eidUseForNOView
                 "Use EIDService for Norwegian BankID for auth to view"

instance Unjson EIDServiceConf where
  unjsonDef = unjsonEIDServiceConf

makeFieldLabelsWith noPrefixFieldLabels ''EIDServiceConf
