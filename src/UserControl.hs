
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


handleUser ctx = 
    msum 
    [ methodM GET >> showUser ctx
    , methodM POST >> handleUserPost ctx
    ]

g name = do
  m <- getDataFn (look name)
  case m of
    Nothing -> mzero -- finishWith undefined -- ("expected " ++ name)
    Just k -> return (BS.fromString k)

handleUserPost ctx@Context{ctxmaybeuser = Just user} = do
  fullname <- g "fullname"
  email <- g "email"
  companyname <- g "companyname"
  companynumber <- g "companynumber"
  invoiceaddress <- g "invoiceaddress"
  update $ SetUserDetails user fullname companyname companynumber invoiceaddress
  -- FIME: add flash-message here
  let link = show LinkAccount
  response <- webHSP (seeOtherXML link)
  seeOther link response


  