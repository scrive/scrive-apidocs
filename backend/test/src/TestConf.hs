module TestConf (
    TestConf(..)
  , unjsonTestConf
  ) where

import Data.Unjson
import qualified Data.Text as T

import PdfToolsLambda.Conf

-- | Main application configuration.  This includes amongst other
-- things the http port number, AWS, GuardTime, E-ID and email
-- configuraton, as well as a handy boolean indicating whether this is
-- a production or development instance.
data TestConf = TestConf {
    testDBConfig           :: T.Text               -- ^ test postgresql configuration
  , testPdfToolsLambdaConf :: PdfToolsLambdaConf   -- ^ pdf tools lambda configuration for tests
  } deriving (Eq, Show)

unjsonTestConf :: UnjsonDef TestConf
unjsonTestConf = objectOf $ pure TestConf
  <*> field "database"
      testDBConfig
      "Database connection string"
  <*> field "pdftools_lambda"
      testPdfToolsLambdaConf
      "Configuration of PdfTools Lambda"

instance Unjson TestConf where
  unjsonDef = unjsonTestConf
