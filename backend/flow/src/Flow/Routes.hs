module Flow.Routes
    ( Routes
    , routesProxy
    )
  where

import Data.Proxy
import Servant.API

import Flow.Routes.Api
import Flow.Routes.Pages
import Flow.Routes.Types

type Spec = AddFlowPrefix ("documentation" :> Raw)

type Routes = FlowApi :<|> FlowPages :<|> Spec

routesProxy :: Proxy Routes
routesProxy = Proxy
