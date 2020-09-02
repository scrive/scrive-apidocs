module Flow.Server.Api where

import Servant
import qualified Data.Text as T

import Flow.OrphanInstances ()
import Flow.Routes.Api as Api
import Flow.Routes.Types
import Flow.Server.Types
import Version
import VersionTH
import qualified Flow.Server.Api.Instances as Instances
import qualified Flow.Server.Api.Templates as Templates

version :: AppM Version
version = pure . Version . T.pack $ buildVcsNumber buildVersion

api :: ServerT FlowApi AppM
api = accountEndpoints :<|> instanceUserEndpoints :<|> noAuthEndpoints
  where
    accountEndpoints account =
      Templates.accountEndpoints account :<|> Instances.accountEndpoints account
    instanceUserEndpoints = Instances.getInstanceView
    noAuthEndpoints       = Templates.validateTemplate :<|> version
