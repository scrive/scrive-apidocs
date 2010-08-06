
module UserControl where
import UserState
import UserView
import qualified Data.ByteString.UTF8 as BS
import KontraLink
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import Control.Monad
import User
import AppView
import Control.Monad.Trans
import Misc


handleUser ctx = 
    msum 
    [ methodM GET >> showUser ctx
    , methodM POST >> handleUserPost ctx
    ]

g :: String -> Kontra BS.ByteString 
g name = do
  k <- getDataFnM (look name)
  return (BS.fromString k)

handleUserPost ctx@Context{ctxmaybeuser = Just user} = do
  fullname <- g "fullname"
  companyname <- g "companyname"
  companynumber <- g "companynumber"
  invoiceaddress <- g "invoiceaddress"
  newuser <- update $ SetUserDetails user fullname companyname companynumber invoiceaddress
  -- FIME: add flash-message here
  let link = show LinkAccount
  response <- webHSP (seeOtherXML link)
  seeOther link response


  