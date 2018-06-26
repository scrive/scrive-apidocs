module Planhat.Reporting where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Int (Int64)
import Log
import Network.HTTP.Client (Manager, Response, httpLbs)
import Network.HTTP.Simple (getResponseBody)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL

import DB
import Planhat.Communication
import Planhat.Config
import User.Model (GetUserGroupAccountsCountActive(..), GetUserGroupAccountsCountMainDomainBranding(..), GetUserGroupAccountsCountTotal(..))
import UserGroup.Data

doDailyPlanhatStats :: ( MonadDB m
                       , MonadLog m
                       , MonadThrow m
                       , MonadIO m
                       , MonadBaseControl IO m)
                    => PlanhatConf
                    -> Manager
                    -> m ()
doDailyPlanhatStats phConf reqManager = do
  now <- currentTime
  logInfo_ "Generating Planhat user stats"
  usersTotal  <- dbQuery GetUserGroupAccountsCountTotal
  usersActive <- dbQuery GetUserGroupAccountsCountActive
  usersMainDomainBranding <- dbQuery GetUserGroupAccountsCountMainDomainBranding

  -- metrics; the Planhat metrics API endpoint accepts a list so we concatenate
  -- the updates to be efficient
  logPlanhatErrors =<< (liftIO $ do
    httpLbs (mkPlanhatRequest
              phConf
              phMetricsURL
              (JSON.toJSON $
                (planhatMetricJSONs "users_total" usersTotal now) <>
                (planhatMetricJSONs "users_active" usersActive now) <>
                (planhatMetricJSONs "users_main_branding" usersMainDomainBranding now)))
            reqManager)

  where

    planhatMetricJSONs :: String -> [(UserGroupID, Int64)] -> UTCTime -> [JSON.Value]
    planhatMetricJSONs dimensionId uts now = map (\(ugid, k) ->
      planhatMetricJSON dimensionId k ugid now) uts

    logPlanhatErrors :: (MonadLog m) => Response BSL.ByteString-> m ()
    logPlanhatErrors res = do
      case maybeErrors . getResponseBody $ res of
        Nothing -> return ()
        Just errObject -> logInfo "Planhat call error" $ errObject
