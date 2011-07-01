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
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Happstack.State (update)
import PayEx.PayExInterface () -- Import so at least we check if it compiles
import InputValidation
import Text.JSON
import Control.Monad.Reader
import API.API
import Routing
import API.Service.ServiceState
import API.APICommons
import Doc.DocUtils
import Company.CompanyState
import Data.Foldable (fold)
import System.Random (randomIO)
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo

import qualified AppLogger as Log (debug)

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
             (Just service, Right body2) -> do
                Log.debug $ "API call from service:" ++ show (serviceid service)
                Log.debug $ "API call body is:" ++ (take 300 $ encode body2)
                return $ Right $ IntegrationAPIContext {ibody=body2,service=service}
             (Nothing,_) -> return $ Left $ (API_ERROR_LOGIN ,"Bad service/password")
             (_,Left s) -> return $ Left $ (API_ERROR_PARSING,"Parsing error: " ++ s)



integrationService ::  Kontra (Maybe Service)
integrationService = do
    sid <- getFieldUTFWithDefault BS.empty "service"
    mservice <- query $ GetService (ServiceID sid)
    case mservice of
         Just service -> do
             passwd <- getFieldUTFWithDefault BS.empty "password"
             if (verifyPassword (servicepassword $ servicesettings service) passwd)
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
                    , dir "connectuser" $ hGet3 $ connectUserToSession
                    , dir "connectcompany" $ hGet3 $ connectCompanyToSession
                  ]

--- Real api requests
-- unused function, commenting for now
{-getRequestUser:: IntegrationAPIFunction User
getRequestUser = do
    sid <- serviceid <$> service <$> ask
    memail <- apiAskBS "email"
    when (isNothing memail) $ throwApiError API_ERROR_MISSING_VALUE "No user email provided"
    muser <- query $ GetUserByEmail (Just sid) $ Email $ fromJust memail
    when (isNothing muser) $ throwApiError API_ERROR_NO_USER "No user"
    let user = fromJust muser
    --srv <-  service <$> ask
    --when (not $ elem (userid $ user) $ serviceusers srv) $ throwApiError "User has not accepted this service"
    return user-}


embeddDocumentFrame :: IntegrationAPIFunction APIResponse
embeddDocumentFrame = do
    ctx <- lift $ get
    srvs <-  service <$> ask
    let sid = serviceid srvs
    let slocation = fromMaybe (ctxhostpart ctx) $ show <$> (servicelocation $ servicesettings srvs)
    let returnLink l =  return $ toJSObject [ ("link",JSString $ toJSString $ slocation ++ show l)]
    location <- fold <$> apiAskString "location"
    mdocument <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "document_id"
    when (isNothing mdocument) $ throwApiError API_ERROR_NO_DOCUMENT "No such document"
    let doc = fromJust mdocument
    mcompany <- lift_M (update . GetOrCreateCompanyWithExternalID  (Just sid) ) $ maybeReadM $ apiAskString "company_id"
    msiglink <- liftMM (return . getSigLinkFor doc) (apiAskBS "email")
    case (mcompany,msiglink) of
         (Just company,Nothing) -> do
             ssid <- createServiceSession (Left $ companyid $ company) location
             returnLink $ LinkConnectCompanySession sid (companyid company) ssid $ LinkIssueDoc (documentid doc)
         (Just company, Just siglink) -> do
             if (isAuthor siglink && (isJust $ maybesignatory siglink))
                then do
                     ssid <- createServiceSession (Right $ fromJust $ maybesignatory siglink) location
                     returnLink $ LinkConnectUserSession sid  (fromJust $ maybesignatory siglink) ssid $ LinkIssueDoc (documentid doc)
                else do
                     ssid <- createServiceSession (Left $ companyid $ company) location
                     returnLink $ LinkConnectCompanySession sid (companyid company) ssid $ LinkIssueDoc (documentid doc)
         _ -> throwApiError API_ERROR_MISSING_VALUE "At least company connected to document must be provided."


createDocument  :: IntegrationAPIFunction APIResponse
createDocument = do
   sid <- serviceid <$> service <$> ask
   mcompany_id <- maybeReadM $ apiAskString "company_id"
   when (isNothing mcompany_id) $ throwApiError API_ERROR_MISSING_VALUE "No company id provided"
   company <- update $ GetOrCreateCompanyWithExternalID  (Just sid) (fromJust mcompany_id)
   mtitle <- apiAskBS "title"
   when (isNothing mtitle) $ throwApiError API_ERROR_MISSING_VALUE "No title provided"
   let title = fromJust mtitle
   files <- getFiles
   mtype <- liftMM (return . toSafeEnum) (apiAskInteger "type")
   when (isNothing mtype) $ throwApiError API_ERROR_MISSING_VALUE "BAD DOCUMENT TYPE"
   let doctype = toDocumentType $ fromJust mtype
   mtemplate <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "template_id"
   involved  <- fmap (fromMaybe []) $ apiLocal "involved" $ apiMapLocal $ getSignatoryTMP
   tags <- fmap (fromMaybe []) $ apiLocal "tags" $ apiMapLocal $ do
                    n <- apiAskBS "name"
                    v <- apiAskBS "value"
                    when (isNothing n || isNothing v) $ throwApiError API_ERROR_MISSING_VALUE "MIssing tag name or value"
                    return $ Just $ DocumentTag (fromJust n) (fromJust v)
   doc <- case mtemplate of
            Just _template -> throwApiError API_ERROR_OTHER "Template support is not implemented yet"
            Nothing -> do
                        d <- createAPIDocument company doctype title files involved tags
                        updateDocumentWithDocumentUI d
   liftIO $ putStrLn $ show $ doc
   return $ toJSObject [ ("document_id",JSString $ toJSString $ show $ documentid doc)]


updateDocumentWithDocumentUI :: Document -> IntegrationAPIFunction Document
updateDocumentWithDocumentUI doc = do
    mailfooter <- apiAskBS "mailfooter"
    ndoc <- update $ SetDocumentUI (documentid doc) $ (documentui doc) {documentmailfooter = mailfooter}
    return $ either (const doc) id ndoc
    
createAPIDocument:: Company ->
                    DocumentType ->
                    BS.ByteString ->
                    [(BS.ByteString,BS.ByteString)] ->
                    [SignatoryTMP] ->
                    [DocumentTag] ->
                    IntegrationAPIFunction Document
createAPIDocument _ _ _ _ [] _  =
    throwApiError API_ERROR_OTHER "One involved person must be provided"
createAPIDocument company doctype title files (authorTMP:signTMPS) tags = do
    now <- liftIO $ getMinutesTime
    author <- userFromTMP authorTMP
    doc <- update $ NewDocumentWithMCompany (Just $ companyid company) author title doctype now
    sequence_  $ map (update . uncurry (AttachFile $ documentid doc)) files
    _ <- update $ SetDocumentTags (documentid doc) tags
    doc' <- update $ UpdateDocumentSimple (documentid doc) (toSignatoryDetails authorTMP, getSignatoryAccount author) (map toSignatoryDetails signTMPS)
    when (isLeft doc') $ throwApiError API_ERROR_OTHER "Problem creating a document (SIGUPDATE) | This should never happend"
    return $ fromRight doc'



userFromTMP::SignatoryTMP ->  IntegrationAPIFunction User
userFromTMP uTMP = do
    sid <- serviceid <$> service <$> ask
    let remail = fold $ asValidEmail . BS.toString <$> email uTMP
    when (not $ isGood $ remail) $ throwApiError API_ERROR_OTHER "NOT valid email for first involved person"
    muser <- query $ GetUserByEmail (Just sid) $ Email $ fromGood remail
    user <- case muser of
              Just u -> return u
              Nothing -> do
                password <- liftIO $ createPassword . BS.fromString =<< (sequence $ replicate 12 randomIO)
                u <- update $ AddUser (fold $ fstname uTMP,fold $ sndname uTMP) (fromGood remail) password Nothing (Just sid) Nothing
                when (isNothing u) $ throwApiError API_ERROR_OTHER "Problem creating a user (BASE) | This should never happend"
                u' <- update $ AcceptTermsOfService (userid $ fromJust u) (fromSeconds 0)
                when (isLeft u') $ throwApiError API_ERROR_OTHER "Problem creating a user (TOS) | This should never happend"
                return $ fromRight u'
    user' <- update $ SetUserInfo (userid user) (userinfo user)
            {
              userfstname = fromMaybe (getFirstName user) $ fstname uTMP
            , usersndname = fromMaybe (getFirstName user) $ sndname uTMP
            , userpersonalnumber = fromMaybe (getPersonalNumber user) $ personalnumber uTMP
            , usercompanyname  = fromMaybe (getCompanyName user) $ company  uTMP
            , usercompanynumber  = fromMaybe (getCompanyNumber user) $ companynumber uTMP
            }
    when (isLeft user') $ throwApiError API_ERROR_OTHER "Problem creating a user (INFO) | This should never happend"
    return $ fromRight user'


getDocuments :: IntegrationAPIFunction APIResponse
getDocuments = do
    sid <- serviceid <$> service <$> ask
    mcompany_id <- maybeReadM $ apiAskString "company_id"
    when (isNothing mcompany_id) $ throwApiError API_ERROR_MISSING_VALUE "No company id provided"
    company <- update $ GetOrCreateCompanyWithExternalID  (Just sid) (fromJust mcompany_id)
    tags <- fmap (fromMaybe []) $ apiLocal "tags" $ apiMapLocal $ do
                    n <- apiAskBS "name"
                    v <- apiAskBS "value"
                    when (isNothing n || isNothing v) $ throwApiError API_ERROR_MISSING_VALUE "Missing tag name or value"
                    return $ Just $ DocumentTag (fromJust n) (fromJust v)
    documents <- query $ GetDocumentsByCompanyAndTags (Just sid) (companyid company) tags
    let notDeleted doc =  any (not . signatorylinkdeleted) $ documentsignatorylinks doc
    -- We support only offers and contracts by API calls    
    let supportedType doc = documenttype doc `elem` [Template Contract, Template Offer, Signable Contract, Signable Offer]
    api_docs <- sequence $  map (api_document False) $ filter (\d -> notDeleted d && supportedType d) documents
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
    when (isNothing mtag) $ throwApiError API_ERROR_MISSING_VALUE "Could not read tag name or value"
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
connectUserToSession :: ServiceID -> UserID -> SessionId -> Kontra KontraLink
connectUserToSession _ uid ssid = do
    loaded <- loadServiceSession (Right uid) ssid
    if (loaded)
     then return $ BackToReferer
     else mzero

connectCompanyToSession :: ServiceID -> CompanyID -> SessionId -> Kontra KontraLink
connectCompanyToSession _ cid ssid = do
    loaded <- loadServiceSession (Left cid) ssid
    if (loaded)
     then return $ BackToReferer
     else mzero

