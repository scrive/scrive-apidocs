{-# LANGUAGE StrictData #-}
module Flow.Routes
    ( Routes
    , routesProxy
    )
  where

import Data.Proxy
import Servant.API

import Flow.Routes.Api
import Flow.Routes.Pages

type Routes = FlowApi :<|> FlowPages

routesProxy :: Proxy Routes
routesProxy = Proxy
