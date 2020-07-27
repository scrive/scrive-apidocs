{-# LANGUAGE StrictData #-}
module Flow.Routes.Pages
    ( FlowPages
    , pagesProxy
    )
  where

import Data.Proxy
import Servant.API
import Web.Cookie

import Auth.MagicHash
import Flow.Id
import Flow.Names
import Flow.Routes.Types
import Flow.Server.Cookies

-- brittany-disable-next-binding
type Pages
  = AuthProtect "instance-user-html" :>
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

type FlowPages = AddFlowPrefix Pages

pagesProxy :: Proxy FlowPages
pagesProxy = Proxy
