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

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Doc.DocState
import Happstack.Server hiding (simpleHTTP,host,body)
import Happstack.State (query)
import KontraLink
import MinutesTime
import Misc
import Session
import Kontra
import qualified User.UserControl as UserControl
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Doc.DocControl as DocControl
import qualified Data.Map as Map
import Templates.Templates (readTemplates, renderTemplate, KontrakcjaTemplates, getTemplatesModTime)
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
import API.IntegrationAPIUtils
import Doc.DocUtils
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
             (Nothing,_) -> return $ Left $ (API_ERROR_LOGIN ,"Bad service/password")
             (_,Left s) -> return $ Left $ (API_ERROR_PARSING,"Parsing error: " ++ s) 
    


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
                      apiCall "embed_document_frame" embeddDocumentFrame
                    , apiCall "new_document" createDocument
                    , apiCall "documents" $  getDocuments
                    , apiCall "document"  getDocument
                    , apiCall "set_document_tag"  setDocumentTag
                    , apiCall "remove_document" removeDocument
                    , apiUnknownCall
                    , dir "connect" $ hGet $ connectSession
                  ]
--- Real api requests
getRequestUser:: IntegrationAPIFunction User
getRequestUser = do
    sid <- serviceid <$> service <$> ask
    memail <- apiAskBS "email"
    when (isNothing memail) $ throwApiError API_ERROR_MISSING_VALUE "No user email provided"
    muser <- query $ GetUserByEmail (Just sid) $ Email $ fromJust memail
    when (isNothing muser) $ throwApiError API_ERROR_NO_USER "No user"
    let user = fromJust muser
    --srv <-  service <$> ask
    --when (not $ elem (userid $ user) $ serviceusers srv) $ throwApiError "User has not accepted this service"
    return user


embeddDocumentFrame :: IntegrationAPIFunction APIResponse
embeddDocumentFrame = do
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
    return $ toJSObject [
        ("link",JSString $ toJSString $ (ctxhostpart ctx) ++ show 
            (LinkConnectUserSession (serviceid srv) (userid user) sid rlink)
        )]
    

createDocument  :: IntegrationAPIFunction APIResponse
createDocument = do
   company_id <- apiAskBS "company_id"
   title <- apiAskBS "title"
   files <- fmap (fromMaybe []) $ apiLocal "files" $ apiMapLocal $ do
                n <- apiAskBS "name"
                c <- apiAskBase64 "content"
                when (isNothing n || isNothing c) $ throwApiError API_ERROR_MISSING_VALUE "Problems with files upload"
                return $ Just (fromJust n, fromJust c)
   mtype <- liftMM (return . toSafeEnum) (apiAskInteger "type")
   when (isNothing mtype) $ throwApiError API_ERROR_MISSING_VALUE "BAD DOCUMENT TYPE"
   let doctype::DOCUMENT_TYPE = fromJust mtype
   mtemplate <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "template_id"
   invloved <- fmap (fromMaybe []) $ apiLocal "involved" $ apiMapLocal $ getSignatoryTMP
   tags <- fmap (fromMaybe []) $ apiLocal "tags" $ apiMapLocal $ do
                    n <- apiAskBS "name"
                    v <- apiAskBS "value"
                    when (isNothing n || isNothing v) $ throwApiError API_ERROR_MISSING_VALUE "MIssing tag name or value"
                    return $ Just $ DocumentTag (fromJust n) (fromJust v)
   throwApiError API_ERROR_OTHER "Not implemented yet"


getDocuments :: IntegrationAPIFunction APIResponse
getDocuments = do
    --mcompany <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "document_id"
    sid <- serviceid <$> service <$> ask
    documents <- query $ GetDocuments (Just sid)
    api_docs <- sequence $  map (api_document True)  documents
    return $ toJSObject [("documents",JSArray $ api_docs)] 


getDocument  :: IntegrationAPIFunction APIResponse
getDocument = do
    mdocument <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "document_id"
    when (isNothing mdocument) $ throwApiError API_ERROR_NO_DOCUMENT "No document"
    api_doc <- api_document True (fromJust mdocument)
    return $ toJSObject [("document",api_doc)]

setDocumentTag  :: IntegrationAPIFunction APIResponse
setDocumentTag =  do
    mdocument <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "document_id"
    when (isNothing mdocument) $ throwApiError API_ERROR_NO_DOCUMENT "No document"
    let doc = fromJust mdocument
    mtag <- apiLocal "tag" $ do
             liftM2 pairMaybe (apiAskBS "name") (apiAskBS "value")
    when (isNothing mtag) $ throwApiError API_ERROR_MISSING_VALUE "Conld not read tag values"         
    let tags = addTag (documenttags doc) (fromJust mtag)
    res <- update $ SetDocumentTags (documentid doc) tags
    when (isLeft res) $ throwApiError API_ERROR_NO_USER $ "Changing tag problem:" ++ fromLeft res 
    return $ toJSObject []


--TODO this will not work since we don't have real document removal
removeDocument  :: IntegrationAPIFunction APIResponse
removeDocument = do
    mdocument <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "document_id"
    when (isNothing mdocument) $ throwApiError API_ERROR_NO_DOCUMENT "No document to remove can be found"
    let doc = fromJust mdocument
    res <- update $ ArchiveDocumentForAll $ documentid doc
    when (isLeft res) $ throwApiError API_ERROR_NO_DOCUMENT $ "Error while removing a document: " ++ fromLeft res
    return $ toJSObject []

{- | Call connect user to session (all passed as URL params)
     and redirect user to referer
-}     
connectSession :: ServiceID -> UserID -> SessionId -> Kontra KontraLink
connectSession sid uid ssid = do
    loaded <- loadServiceSession sid uid ssid
    if (loaded) 
     then return $ BackToReferer
     else mzero
    

                      