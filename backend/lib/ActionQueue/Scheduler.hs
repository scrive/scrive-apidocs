module ActionQueue.Scheduler (
    Scheduler
  , SchedulerData(..)
  , getGlobalTemplates
  ) where

import Control.Monad.Reader
import Data.Maybe

import ActionQueue.Monad
import GuardTime.Class
import Salesforce.Conf
import Templates

data SchedulerData = SchedulerData {
    sdGuardTimeConf  :: GuardTimeConf
  , sdSalesforceConf :: Maybe SalesforceConf
  , sdTemplates      :: KontrakcjaGlobalTemplates
  }

type Scheduler = ActionQueue SchedulerData

-- Note: Do not define TemplatesMonad instance for Scheduler, use
-- TemplatesT instead. Reason? We don't have access to currently used
-- language, so we should rely on user's language settings the action is
-- assigned to and since TemplatesMonad doesn't give us the way to get
-- appropriate language version of templates, we need to do that manually.

getGlobalTemplates :: MonadReader SchedulerData m => m KontrakcjaGlobalTemplates
getGlobalTemplates = asks sdTemplates
