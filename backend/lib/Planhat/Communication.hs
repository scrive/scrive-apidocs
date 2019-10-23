module Planhat.Communication where

import Data.Aeson ((.:), (.=))
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client
  ( Request, parseRequest_, responseTimeout, responseTimeoutMicro )

import Network.HTTP.Simple (setRequestBodyJSON, setRequestMethod)
import System.FilePath ((</>))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

import Planhat.Config (PlanhatConf(..))
import User.Model
import UserGroup.Types

phActionURL :: PlanhatConf -> String
phActionURL PlanhatConf {..} = planhatBaseURL </> "analytics" </> planhatTenantID

phMetricsURL :: PlanhatConf -> String
phMetricsURL PlanhatConf {..} = planhatBaseURL </> planhatTenantID </> "dimensiondata"

mkPlanhatRequest :: PlanhatConf -> (PlanhatConf -> String) -> JSON.Value -> Request
mkPlanhatRequest phConf@PlanhatConf {..} phURL reqJSON =
  let timeout =
          responseTimeoutMicro
            $ 10 {- secs -}*  1000000
      req = (setRequestBodyJSON reqJSON) . (setRequestMethod "POST") $ parseRequest_
        (phURL phConf)
      req' = req { responseTimeout = timeout }
  in  req'

-- | Planhat says '200' about most things, but often (but not always) supplies a
-- JSON object that we can check for a key "errors". If this has non-zero length
-- there was an error. Sadly, for some types of errors (with non-obvious common
-- properties) we do not get a JSON-object but a string or even just an empty
-- response. Covering for all of these possibilites will be done when and only
-- when needed. This seems to be enough for the important cases and is enough
-- for now.
maybeErrors :: BSL.ByteString -> Maybe JSON.Value
maybeErrors resBody = do
  resJSON <- JSON.decode' resBody
  (flip JSON.parseMaybe) resJSON $ \val -> do
    (JSON.withObject
        "Planhat query result"
        (\obj -> do
          k <- length <$> (obj .: "errors" :: JSON.Parser JSON.Array)
          if k > 0 then return resJSON else fail ""
        )
      )
      val

-- JSON build helpers

planhatActionJSON :: String -> String -> UserID -> UTCTime -> JSON.Value
planhatActionJSON actionTag email userId time = JSON.object
  [ "email" .= email
  , "action" .= actionTag
  , "externalId" .= (show . unUserID $ userId)
  , "date" .= time
  ]

planhatMetricJSON
  :: String      -- ^ The name of the dimension used.
  -> Int64       -- ^ Value (usually a counter).
  -> UserGroupID -- ^ @rootugid@: the @UserGroupID@ of the invoicing
                   -- parent user group.
  -> UserGroupID -- ^ @sugid@: the @UserGroupID@ of the subgroup to
                   -- which a dimension belongs.
  -> UTCTime     -- ^ Timestamp.
  -> JSON.Value
planhatMetricJSON dimensionId value invoiceUgid sugid time = JSON.object
  [ "companyExternalId" .= (Text.pack $ show invoiceUgid)
  , "dimensionId" .= dimensionId
  , "value" .= value
  , "date" .= time
  , "assetExtId" .= (Text.pack $ show sugid)
  ]
