{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module UserView where

import HSP hiding (Request)
import System.Locale (defaultTimeLocale)
import Control.Monad.Trans (MonadIO,liftIO,lift)
import Happstack.Server (Response)
import Happstack.Server.HStringTemplate (webST)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.SimpleHTTP
import qualified HSX.XMLGenerator as HSX (XML)
import Control.Monad
import Data.Object.Json as Json
import Data.Object as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSCL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.UTF8 as BSC
import Network.HTTP (urlEncode)
import Data.Time
import UserState
import Misc
import AppView
import User

showUser ctx@(Context (Just user) _) = 
    webHSP $ pageFromBody ctx TopAccount kontrakcja $ 
    <div class="doctable">
     <h1>Welcome <% fullname user %></h1>
      <div class="inlinebox">
       <p>Your name: <% fullname user %></p>
       <p>Your email address: <% email user %></p>
       <p>Företagsnamn: <% usercompanyname user %></p>
       <p>Organisationsnummer: <% usercompanynumber user %></p>
       <p>Faktureringsadress: <% userinvoiceaddress user %></p>
      </div>
    </div>
  
