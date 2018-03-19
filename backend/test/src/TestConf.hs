module TestConf (
    TestConf(..)
  , unjsonTestConf
  ) where

import Data.Unjson
import qualified Data.Text as T

-- | Main application configuration.  This includes amongst other
-- things the http port number, AWS, GuardTime, E-ID and email
-- configuraton, as well as a handy boolean indicating whether this is
-- a production or development instance.
data TestConf = TestConf {
    testDBConfig           :: T.Text               -- ^ test postgresql configuration
  } deriving (Eq, Show)

unjsonTestConf :: UnjsonDef TestConf
unjsonTestConf = objectOf $ pure TestConf
  <*> field "database"
      testDBConfig
      "Database connection string"

instance Unjson TestConf where
  unjsonDef = unjsonTestConf
