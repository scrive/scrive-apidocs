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
import User.Model
  ( GetUserGroupAccountsCountActive(..), GetUserGroupAccountsCountTotal(..)
  )
import UserGroup.Types

doDailyPlanhatStats
  :: (MonadDB m, MonadLog m, MonadThrow m, MonadIO m, MonadBaseControl IO m)
  => PlanhatConf
  -> Manager
  -> m ()
doDailyPlanhatStats phConf reqManager = do
  now <- currentTime
  logInfo_ "Generating Planhat user stats"
  usersTotal  <- dbQuery GetUserGroupAccountsCountTotal
  usersActive <- dbQuery GetUserGroupAccountsCountActive

  -- metrics; the Planhat metrics API endpoint accepts a list so we concatenate
  -- the updates to be efficient
  logPlanhatErrors =<< liftIO
    (do
      httpLbs
        (mkPlanhatRequest
          phConf
          phMetricsURL
          (  JSON.toJSON
          $  planhatMetricJSONs "users_total"  usersTotal  now
          <> planhatMetricJSONs "users_active" usersActive now
          )
        )
        reqManager
    )
  where

    planhatMetricJSONs
      :: String -> [(UserGroupID, UserGroupID, Int64)] -> UTCTime -> [JSON.Value]
    planhatMetricJSONs dimensionId uts now = map
      (\(invoiceUgid, ugid, k) -> planhatMetricJSON dimensionId k invoiceUgid ugid now)
      uts

    logPlanhatErrors :: (MonadLog m) => Response BSL.ByteString -> m ()
    logPlanhatErrors res = do
      forM_ (maybeErrors . getResponseBody $ res) (logInfo "Planhat call error")
