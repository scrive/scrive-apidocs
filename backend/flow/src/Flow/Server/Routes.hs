{-# LANGUAGE StrictData #-}
module Flow.Server.Routes
    ( Routes
    , routesProxy
    , pagesProxy
    )
  where

import Data.Proxy
import Servant.API
import Web.Cookie

import Auth.MagicHash
import Flow.Api
import Flow.Id
import Flow.Names
import Flow.Server.Cookies
import Flow.Server.Types hiding (aesonOptions)

type Routes = FlowAPI :<|> Pages

routesProxy :: Proxy Routes
routesProxy = Proxy

-- brittany-disable-next-binding
type Pages
    = "flow" :>
       ( AuthProtect "instance-user" :>
          "overview" :> Capture "instance_id" InstanceId :> Capture "user_name" UserName
                     :> Get '[HTML] Text
        :<|>
          -- No authentication
          "overview" :> Capture "instance_id" InstanceId
                     :> Capture "user_name" UserName
                     :> Capture "hash" MagicHash
                     :> Header "Cookie" Cookies'
                     :> Header "Host" Host
                     :> IsSecure
                     :> Get302 '[JSON] (Headers '[ Header "Location" Text
                                                 , Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie
                                                 ] NoContent)
       )

pagesProxy :: Proxy Pages
pagesProxy = Proxy
