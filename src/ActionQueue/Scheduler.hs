module ActionQueue.Scheduler (
    Scheduler
  , SchedulerData(..)
  , getGlobalTemplates
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Data.Time (UTCTime)

import ActionQueue.Monad
import AppConf
import Salesforce.Conf
import Templates

data SchedulerData = SchedulerData {
    sdAppConf   :: AppConf
  , sdTemplates :: MVar (UTCTime, KontrakcjaGlobalTemplates)
  }

type Scheduler = ActionQueue SchedulerData

-- Note: Do not define TemplatesMonad instance for Scheduler, use
-- TemplatesT instead. Reason? We don't have access to currently used
-- language, so we should rely on user's language settings the action is
-- assigned to and since TemplatesMonad doesn't give us the way to get
-- appropriate language version of templates, we need to do that manually.

getGlobalTemplates :: (MonadReader SchedulerData m, MonadIO m) => m KontrakcjaGlobalTemplates
getGlobalTemplates = do
  sd <- ask
  (_, templates) <- liftIO $ readMVar (sdTemplates sd)
  return templates


instance HasSalesforceConf SchedulerData where
  getSalesforceConf =  getSalesforceConf . sdAppConf
