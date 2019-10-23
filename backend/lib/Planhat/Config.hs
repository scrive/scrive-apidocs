module Planhat.Config where

import Data.Unjson

type PlanhatTenantID = String

data PlanhatConf = PlanhatConf
  { planhatBaseURL     :: String
  , planhatTenantID    :: PlanhatTenantID
  } deriving (Eq, Show, Ord)

unjsonPlanhatConf :: UnjsonDef PlanhatConf
unjsonPlanhatConf =
  objectOf
    $   pure PlanhatConf
    <*> field "base_url"  planhatBaseURL  "Planhat API endpoint"
    <*> field "tenant_id" planhatTenantID "Planhat tenant ID"

instance Unjson PlanhatConf where
  unjsonDef = unjsonPlanhatConf
