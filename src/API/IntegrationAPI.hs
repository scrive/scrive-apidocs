{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  API.Integration
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  portable
--
-- Integration API is advanced way to integrate with our service using mix of 
-- request and iframes
-----------------------------------------------------------------------------
module API.IntegrationAPI
    ( integrationAPI) 
    where

--import ELegitimation.BankID as BankID
import ActionSchedulerState
import Control.Monad (msum, mzero)
import Control.Monad.State
import Control.Monad.Trans (liftIO,lift)
import Control.Concurrent
import Data.Functor
import AppView as V
import Control.Concurrent
import Crypto
import Data.List
import Data.Maybe
import Doc.DocState
import HSP.XML (cdata)
import Happstack.Server hiding (simpleHTTP,host,body)
import Happstack.State (query)
import InspectXML
import InspectXMLInstances
import KontraLink
import MinutesTime
import Misc
import Network.Socket
import Session
import System.IO.Unsafe
import Kontra
import qualified User.UserControl as UserControl
import User.UserView as UserView
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.UTF8 as BS
import qualified Doc.DocControl as DocControl
import qualified HSP as HSP
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import qualified Network.AWS.AWSConnection as AWS
import qualified TrustWeaver as TW
import qualified Payments.PaymentsControl as Payments
import qualified Contacts.ContactsControl as Contacts
import qualified ELegitimation.BankID as BankID
import Templates.Templates (readTemplates, renderTemplate, KontrakcjaTemplates, getTemplatesModTime)
import qualified Administration.AdministrationControl as Administration
import Control.Concurrent.MVar
import Mails.MailsConfig
import Mails.SendGridEvents
import Mails.SendMail
import System.Log.Logger (Priority(..), logM)
import qualified FlashMessage as F
import qualified AppLogger as Log (error, security, debug)
import qualified MemCache
import Happstack.State (update)
import Redirect
import PayEx.PayExInterface -- Import so at least we check if it compiles
import InputValidation
import System.Directory
import ListUtil
import Data.Word
import System.Time
import Text.JSON
import Text.JSON.String
import Control.Monad.Reader
import Control.Monad.Error
import API.API
import Routing
import API.Service.ServiceState
import Doc.DocView
{- | 
  Definition of integration API
-}

data IntegrationAPIContext = IntegrationAPIContext {ibody :: APIRequestBody , service :: Service}
type IntegrationAPIFunction a = APIFunction IntegrationAPIContext a

instance APIContext IntegrationAPIContext where
    body= ibody
    newBody b ctx = ctx {ibody = b}
    apiContext  = do
        mservice <- integrationService
        mbody <- apiBody 
        case (mservice, mbody)  of
             (Just service, Right body) -> return $ Right $ IntegrationAPIContext {ibody=body,service=service}
             (Nothing,_) -> return $ Left $ "Bad service/password"
             (_,Left s) -> return $ Left $ "Parsing error: " ++ s 
    


integrationService ::  Kontra (Maybe Service)
integrationService = do
    sid <- getFieldUTFWithDefault BS.empty "service"
    mservice <- query $ GetService (ServiceID sid) 
    case mservice of 
         Just service -> do
             passwd <- getFieldUTFWithDefault BS.empty "password"
             if (verifyPassword (servicepassword service) passwd)
                then return $ Just service
                else return Nothing
         Nothing -> return Nothing
         
         
integrationAPI :: Kontra Response
integrationAPI =  dir "integration" $ msum [
                      apiCall "login" loginUser
                    , apiCall "documents" userDocuments  
                    , apiUnknownCall
                    , dir "connect" $ hGet $ connectSession
                  ]
--- Real api requests
getRequestUser:: IntegrationAPIFunction User
getRequestUser = do
    memail <- apiAskBS "email"
    when (isNothing memail) $ throwApiError "No user email provided"
    muser <- query $ GetUserByEmail $ Email $ fromJust memail
    when (isNothing muser) $ throwApiError "No user"
    let user = fromJust muser
    srv <-  service <$> ask
    when (not $ elem (userid $ user) $ serviceusers srv) $ throwApiError "User has not accepted this service"
    return user


loginUser :: IntegrationAPIFunction APIResponse
loginUser = do
    user <- getRequestUser 
    srv <-  service <$> ask
    location <- fromMaybe "" <$> apiAskString "location"
    sid <- createServiceSession (serviceid srv) (userid $ user) location
    ctx <- lift $ get
    mdocumentID <- maybeReadM $ apiAskString "document_id"
    list <-  apiAskString "list"
    let rlink = case (mdocumentID,list) of
                   (Just did,_) -> LinkIssueDoc did
                   (_,Just _) -> LinkContracts emptyListParams
                   _ -> LinkMain
    return $ toJSObject [("link",JSString $ toJSString $ (ctxhostpart ctx) ++ show (LinkConnectUserSession (serviceid srv) (userid user) sid rlink))]
    

userDocuments :: IntegrationAPIFunction APIResponse
userDocuments = do
    user <- getRequestUser 
    documents <- query $ GetDocumentsByUser user
    let contracts  = filter (not . isTemplate) $  filter isContract documents
    return $ toJSObject [("documents",JSArray $ map (JSObject .documentAPIObject) contracts )]

documentAPIObject :: Document -> JSObject JSValue
documentAPIObject doc = 
    toJSObject [
        ("document_id", JSString $ toJSString $ show $ documentid doc),
        ("title", JSString $ toJSString $ BS.toString $ documenttitle doc)
        ]
    
connectSession :: ServiceID -> UserID -> SessionId -> Kontra  KontraLink
connectSession sid uid ssid = do
    loaded <- loadServiceSession sid uid ssid
    if (loaded) 
     then liftIO (putStrLn "logged") >> (return $ BackToReferer)
     else liftIO (putStrLn "NOT logged") >> return LinkAbout


                      