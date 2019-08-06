module EID.EIDService.Conf ( EIDServiceConf(..), unjsonEIDServiceConf ) where

import Data.Unjson
import qualified Data.Text as T

-- | A datatype for EIDService configuration data.
data EIDServiceConf = EIDServiceConf {
      eidServiceUrl :: T.Text
    , eidServiceToken :: T.Text
} deriving (Show, Eq, Ord)

unjsonEIDServiceConf :: UnjsonDef EIDServiceConf
unjsonEIDServiceConf = objectOf $ pure EIDServiceConf
  <*> field "url"
      eidServiceUrl
      "EIDService url"
  <*> field "token"
      eidServiceToken
      "EIDService token"

instance Unjson EIDServiceConf where
  unjsonDef = unjsonEIDServiceConf

