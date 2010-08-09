
module UserControl where
import UserState
import UserView
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
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
import SendMail
import System.Random

handleUser :: Context -> Kontra Response
handleUser ctx = 
    msum 
    [ methodM GET >> showUser ctx
    , methodM POST >> handleUserPost ctx
    , dir "subaccount" $ msum [ methodM GET  >> handleGetSubaccount ctx 
                              , methodM POST >> handlePostSubaccount ctx
                              ]
    ]


handleUserPost :: Context -> Kontra Response
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


handleGetSubaccount :: Context -> Kontra Response
handleGetSubaccount ctx@Context { ctxmaybeuser = Just user@User { userid } }  = do
  subaccounts <- query $ GetUserSubaccounts userid
  viewSubaccounts ctx subaccounts

handlePostSubaccount :: Context -> Kontra Response
handlePostSubaccount ctx@Context { ctxmaybeuser = Just (User { userid })} = do
  create <- g "create"   -- check if we are in proper action
  fullname <- g "fullname"
  email <- g "email"
  user <- liftIO $ createUser fullname email (Just userid)
  let link = show LinkSubaccount
  response <- webHSP (seeOtherXML link)
  seeOther link response

createUser :: BS.ByteString -> BS.ByteString -> Maybe UserID -> IO User
createUser fullname email maybesupervisor = do
  let letters =['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
  indexes <- liftIO $ replicateM 8 (randomRIO (0,length letters-1))
  let passwd = BS.fromString $ map (letters!!) indexes
  user <- update $ AddUser fullname email passwd maybesupervisor
  content <- liftIO $ passwordChangeMail email fullname passwd
  liftIO $ sendMail [(fullname, email)]
               (BS.fromString "Välkommen!") content BS.empty
  return user

