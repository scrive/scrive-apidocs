module Flow.Routes.Pages
    ( FlowPages
    , pagesProxy
    )
  where

import Data.Proxy
import Servant.API
import Servant.HTML.Blaze
import Text.Blaze.Html5
import Web.Cookie

import Auth.MagicHash
import Flow.Id
import Flow.Names
import Flow.Routes.Types
import Flow.Server.Cookies

-- brittany-disable-next-binding
type Pages
  = AuthProtect "instance-user-html" :>
      "overview" :> Capture "instance_id" InstanceId
                 :> Capture "user_name" UserName
                 :> Header "Cookie" Cookies' -- this is only to get sessionId even in authenticated request
                 :> Header "Host" Host
                 :> IsSecure
                 :> Get '[HTML] Html
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
