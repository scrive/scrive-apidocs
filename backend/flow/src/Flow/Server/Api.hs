module Flow.Server.Api where

import Servant

import Flow.OrphanInstances ()
import Flow.Routes.Api as Api
import Flow.Server.Types
import qualified Flow.Server.Api.Instances as Instances
import qualified Flow.Server.Api.Templates as Templates

api :: ServerT FlowApi AppM
api = accountEndpoints :<|> instanceUserEndpoints :<|> noAuthEndpoints
  where
    accountEndpoints account =
      Templates.accountEndpoints account :<|> Instances.accountEndpoints account
    instanceUserEndpoints = Instances.getInstanceView
    noAuthEndpoints       = Templates.validateTemplate
