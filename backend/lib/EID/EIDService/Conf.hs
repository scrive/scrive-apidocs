{-# LANGUAGE TemplateHaskell #-}
module EID.EIDService.Conf ( EIDServiceConf(..), unjsonEIDServiceConf ) where

import Data.Unjson
import Optics.TH
import qualified Data.Text as T

-- | A datatype for EIDService configuration data.
data EIDServiceConf = EIDServiceConf
  { eidServiceUrl :: T.Text
  , eidServiceToken :: T.Text
  , eidUseForDK :: Maybe Bool
  } deriving (Show, Eq, Ord)

unjsonEIDServiceConf :: UnjsonDef EIDServiceConf
unjsonEIDServiceConf =
  objectOf
    $   pure EIDServiceConf
    <*> field "url"   eidServiceUrl   "EIDService url"
    <*> field "token" eidServiceToken "EIDService token"
    <*> fieldOpt "useForDK" eidUseForDK "Use EIDService for Denmark"

instance Unjson EIDServiceConf where
  unjsonDef = unjsonEIDServiceConf

makeFieldLabelsWith noPrefixFieldLabels ''EIDServiceConf
