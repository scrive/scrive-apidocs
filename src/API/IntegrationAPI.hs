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
module API.IntegrationAPI (
    -- For main server we export only this
      integrationAPI
    -- For tests (and only for tests)
    , IntegrationAPIFunction
    , embeddDocumentFrame
    , createDocument     
    , getDocuments       
    , getDocument        
    , setDocumentTag     
    , removeDocument
    ) where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import DB.Classes
import Doc.DocState
import Happstack.Server hiding (simpleHTTP,host,body)
import Happstack.State (query, update)
import KontraLink
import MinutesTime
import Misc
import Session
import Kontra
import AppView
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import InputValidation
import Text.JSON
import Control.Monad.Reader
import API.API
import Routing
import API.Service.Model
import API.APICommons
import Doc.DocUtils
import Company.Model
import User.Model
import Data.Foldable (fold)
import System.Random (randomIO)
import Util.SignatoryLinkUtils
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.ServiceUtils
import Util.MonadUtils
import Templates.Templates
import Stats.Control
import File.File

import qualified Data.ByteString.Lazy.UTF8 as BSL (fromString)
import qualified AppLogger as Log (debug)


{- |
  Definition of integration API
-}

data IntegrationAPIContext = IntegrationAPIContext {ibody :: APIRequestBody , service :: Service}
type IntegrationAPIFunction m a = APIFunction m IntegrationAPIContext a

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



integrationService :: Kontrakcja m => m (Maybe Service)
integrationService = do
    sid <- getFieldUTFWithDefault BS.empty "service"
    mservice <- runDBQuery $ GetService $ ServiceID sid
    case mservice of
         Just service -> do
             passwd <- getFieldUTFWithDefault BS.empty "password"
             if (verifyPassword (servicepassword $ servicesettings service) passwd)
                then return $ Just service
                else return Nothing
         Nothing -> return Nothing

integrationAPI :: Kontra Response
integrationAPI = dir "integration" $ msum [
      dir "api" $ apiCall "embed_document_frame" embeddDocumentFrame :: Kontrakcja m => m Response
    , dir "api" $ apiCall "new_document" createDocument              :: Kontrakcja m => m Response
    , dir "api" $ apiCall "documents" getDocuments                   :: Kontrakcja m => m Response
    , dir "api" $ apiCall "document" getDocument                     :: Kontrakcja m => m Response
    , dir "api" $ apiCall "set_document_tag" setDocumentTag          :: Kontrakcja m => m Response
    , dir "api" $ apiCall "remove_document" removeDocument           :: Kontrakcja m => m Response
    , dir "api" $ apiUnknownCall
    , dir "connectuser" $ hGet3 $ toK3 $ connectUserToSessionGet
    , dir "connectuser" $ hPostNoXToken3 $ toK3 $ connectUserToSessionPost
    , dir "connectcompany" $ hGet3 $ toK3 $ connectCompanyToSession
    ]


documentFromParam:: Kontrakcja m => IntegrationAPIFunction m Document
documentFromParam = do
    srvs <- service <$> ask
    mdocument <- liftMM (query . GetDocumentByDocumentID) $ maybeReadM $ apiAskString "document_id"
    when (isNothing mdocument || (not $ sameService srvs mdocument)) $ throwApiError API_ERROR_NO_DOCUMENT "No document exists"
    return $ fromJust mdocument

embeddDocumentFrame :: Kontrakcja m => IntegrationAPIFunction m APIResponse
embeddDocumentFrame = do
    ctx <- getContext
    srvs <-  service <$> ask
    let sid = serviceid srvs
    let slocation = fromMaybe (ctxhostpart ctx) $ (BS.toString . unServiceLocation) <$> (servicelocation $ servicesettings srvs)
    let returnLink l =  return $ toJSObject [ ("link",JSString $ toJSString $ slocation ++ show l)]
    location <- fold <$> apiAskString "location"
    doc <- documentFromParam
    mcompany <- lift_M (runDBUpdate . GetOrCreateCompanyWithExternalID  (Just sid)) (fmap ExternalCompanyID <$> apiAskBS "company_id")
    when (isNothing mcompany) $ throwApiError API_ERROR_MISSING_VALUE "At least company connected to document must be provided."
    let company = fromJust mcompany
    when (not $ isAuthoredByCompany (companyid company) doc) $ throwApiError API_ERROR_NO_DOCUMENT "No document exists"
    msiglink <- liftMM (return . getSigLinkFor doc) (apiAskBS "email")
    case msiglink of
         Nothing -> do
             when (not $ sameService srvs company) $ throwApiError API_ERROR_MISSING_VALUE "Not matching company | This should never happend"
             ssid <- createServiceSession (Left $ companyid $ company) location
             returnLink $ LinkConnectCompanySession sid (companyid company) ssid $ LinkIssueDoc (documentid doc)
         Just siglink -> do
             if (isAuthor siglink && (isJust $ maybesignatory siglink))
                then do
                     muser <- runDBQuery $ GetUserByID $ fromJust $ maybesignatory siglink
                     when (not $ sameService sid muser && sameService srvs company) $ throwApiError API_ERROR_MISSING_VALUE "Not matching user or company| This should never happend"
                     ssid <- createServiceSession (Right $ fromJust $ maybesignatory siglink) location
                     returnLink $ LinkConnectUserSession sid  (fromJust $ maybesignatory siglink) ssid $ LinkIssueDoc (documentid doc)
                else do
                     when (not $ sameService srvs company) $ throwApiError API_ERROR_MISSING_VALUE "Not matching company | This should never happend"
                     ssid <- createServiceSession (Left $ companyid $ company) location
                     returnLink $ LinkConnectCompanySession sid (companyid company) ssid $ LinkIssueDoc (documentid doc)
      


createDocument :: Kontrakcja m => IntegrationAPIFunction m APIResponse
createDocument = do
   sid <- serviceid <$> service <$> ask
   mcompany_id <- fmap ExternalCompanyID <$> apiAskBS "company_id"
   when (isNothing mcompany_id) $ throwApiError API_ERROR_MISSING_VALUE "No company id provided"
   company <- runDBUpdate $ GetOrCreateCompanyWithExternalID  (Just sid) (fromJust mcompany_id)
   mtitle <- apiAskBS "title"
   when (isNothing mtitle) $ throwApiError API_ERROR_MISSING_VALUE "No title provided"
   let title = fromJust mtitle
   files <- getFiles
   mtype <- liftMM (return . toSafeEnum) (apiAskInteger "type")
   when (isNothing mtype) $ throwApiError API_ERROR_MISSING_VALUE "BAD DOCUMENT TYPE"
   let doctype = toDocumentType $ fromJust mtype
   -- Again, we don't check that the document is from the service
   -- -EN
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
   return $ toJSObject [ ("document_id",JSString $ toJSString $ show $ documentid doc)]


updateDocumentWithDocumentUI :: Kontrakcja m => Document -> IntegrationAPIFunction m Document
updateDocumentWithDocumentUI doc = do
    mailfooter <- apiAskBS "mailfooter"
    ndoc <- update $ SetDocumentUI (documentid doc) $ (documentui doc) {documentmailfooter = mailfooter}
    return $ either (const doc) id ndoc

createAPIDocument :: Kontrakcja m
                  => Company
                  -> DocumentType
                  -> BS.ByteString
                  -> [(BS.ByteString,BS.ByteString)]
                  -> [SignatoryTMP]
                  -> [DocumentTag]
                  -> IntegrationAPIFunction m Document
createAPIDocument _ _ _ _ [] _  =
    throwApiError API_ERROR_OTHER "One involved person must be provided"
createAPIDocument company' doctype title files (authorTMP:signTMPS) tags = do
    now <- liftIO $ getMinutesTime
    company <- setCompanyInfoFromTMP authorTMP company'
    author <- userFromTMP authorTMP company
    mdoc <- update $ NewDocument author (Just company) title doctype now
    when (isLeft mdoc) $ throwApiError API_ERROR_OTHER "Problem created a document | This may be because the company and author don't match"
    let doc = fromRight mdoc
    let addAndAttachFile name content = do
            file <- update $ NewFile name content
            update $ AttachFile (documentid doc) (fileid file)
    mapM_ (uncurry addAndAttachFile) files
    _ <- update $ SetDocumentTags (documentid doc) tags
    doc' <- update $ UpdateDocumentSimple (documentid doc) (toSignatoryDetails authorTMP, author) (map toSignatoryDetails signTMPS)
    when (isLeft doc') $ throwApiError API_ERROR_OTHER "Problem creating a document (SIGUPDATE) | This should never happend"
    return $ fromRight doc'

userFromTMP :: Kontrakcja m => SignatoryTMP -> Company -> IntegrationAPIFunction m User
userFromTMP uTMP company = do
    sid <- serviceid <$> service <$> ask
    let remail = fold $ asValidEmail . BS.toString <$> email uTMP
    when (not $ isGood $ remail) $ throwApiError API_ERROR_OTHER "NOT valid email for first involved person"
    muser <- runDBQuery $ GetUserByEmail (Just sid) $ Email $ fromGood remail
    user <- case muser of
              Just u -> return u
              Nothing -> do
                password <- liftIO $ createPassword . BS.fromString =<< (sequence $ replicate 12 randomIO)
                mu <- runDBUpdate $ AddUser (fold $ fstname uTMP,fold $ sndname uTMP) (fromGood remail) (Just password) False (Just sid) (Just $ companyid company) defaultValue (mkLocaleFromRegion defaultValue)
                when (isNothing mu) $ throwApiError API_ERROR_OTHER "Problem creating a user (BASE) | This should never happend"
                let u = fromJust mu
                tos_accepted <- runDBUpdate $ AcceptTermsOfService (userid u) (fromSeconds 0)
                when (not tos_accepted) $ throwApiError API_ERROR_OTHER "Problem creating a user (TOS) | This should never happend"
                mtosuser <- runDBQuery $ GetUserByID (userid u)                
                when (isNothing mtosuser) $ throwApiError API_ERROR_OTHER "Problem reading a user (BASE) | This should never happend"
                let tosuser = fromJust mtosuser

                Context{ctxtime} <- getContext
                _ <- addUserIDSignTOSStatEvent (userid u) ctxtime (usercompany u) (userservice u)

                return tosuser
    info_set <- runDBUpdate $ SetUserInfo (userid user) (userinfo user)
            {
              userfstname = fromMaybe (getFirstName user) $ fstname uTMP
            , usersndname = fromMaybe (getFirstName user) $ sndname uTMP
            , userpersonalnumber = fromMaybe (getPersonalNumber user) $ personalnumber uTMP
            }
    when (not info_set) $ throwApiError API_ERROR_OTHER "Problem creating a user (INFO) | This should never happend"
    company_set <- runDBUpdate $ SetUserCompany (userid user) (Just $ companyid company)
    when (not company_set) $ throwApiError API_ERROR_OTHER "Problem creating a user (COMPANY) | This should never happend"
    Just user' <- runDBQuery $ GetUserByID $ userid user
    return user'

setCompanyInfoFromTMP :: Kontrakcja m => SignatoryTMP -> Company -> IntegrationAPIFunction m Company
setCompanyInfoFromTMP uTMP company = do
    info_set <- runDBUpdate $ SetCompanyInfo (companyid company) (companyinfo company)
                {
                  companyname = fromMaybe (getCompanyName company) $ API.APICommons.company uTMP
                , Company.Model.companynumber = fromMaybe (getCompanyNumber company) $ API.APICommons.companynumber uTMP
                }
    when (not info_set) $ throwApiError API_ERROR_OTHER "Problem create a user (COMPANY INFO) | This should never happen"
    Just company' <- runDBQuery $ GetCompany $ companyid company
    return company'

getDocuments :: Kontrakcja m => IntegrationAPIFunction m APIResponse
getDocuments = do
    sid <- serviceid <$> service <$> ask
    mcompany_id <- fmap ExternalCompanyID <$> apiAskBS "company_id"
    when (isNothing mcompany_id) $ throwApiError API_ERROR_MISSING_VALUE "No company id provided"
    company <- runDBUpdate $ GetOrCreateCompanyWithExternalID  (Just sid) (fromJust mcompany_id)
    tags <- fmap (fromMaybe []) $ apiLocal "tags" $ apiMapLocal $ do
                    n <- apiAskBS "name"
                    v <- apiAskBS "value"
                    when (isNothing n || isNothing v) $ throwApiError API_ERROR_MISSING_VALUE "Missing tag name or value"
                    return $ Just $ DocumentTag (fromJust n) (fromJust v)
    linkeddocuments <- query $ GetDocumentsByCompanyAndTags (Just sid) (companyid company) tags
    let documents = filter (isAuthoredByCompany $ companyid company) linkeddocuments
    let notDeleted doc =  any (not . signatorylinkdeleted) $ documentsignatorylinks doc
    -- We support only offers and contracts by API calls    
    let supportedType doc = documenttype doc `elem` [Template Contract, Template Offer, Signable Contract, Signable Offer]
    api_docs <- sequence $  map (api_document False) $ filter (\d -> notDeleted d && supportedType d) documents
    return $ toJSObject [("documents",JSArray $ api_docs)]
    


getDocument :: Kontrakcja m => IntegrationAPIFunction m APIResponse
getDocument = do
    doc <- documentFromParam
    api_doc <- api_document True doc
    return $ toJSObject [("document",api_doc)]

setDocumentTag :: Kontrakcja m => IntegrationAPIFunction m APIResponse
setDocumentTag =  do
    doc <- documentFromParam
    mtag <- apiLocal "tag" $ do
              liftM2 pairMaybe (apiAskBS "name") (apiAskBS "value")
    when (isNothing mtag) $ throwApiError API_ERROR_MISSING_VALUE "Could not read tag name or value"
    let tags = addTag (documenttags doc) (fromJust mtag)
    res <- update $ SetDocumentTags (documentid doc) tags
    when (isLeft res) $ throwApiError API_ERROR_NO_USER $ "Changing tag problem:" ++ fromLeft res
    return $ toJSObject []


--TODO this will not work since we don't have real document removal
removeDocument  :: Kontrakcja m => IntegrationAPIFunction m APIResponse
removeDocument = do
    doc <- documentFromParam
    -- we only control the author through the integration api
    res <- update $ ArchiveDocumentForAuthor $ documentid doc
    when (isLeft res) $ throwApiError API_ERROR_NO_DOCUMENT $ "Error while removing a document: " ++ fromLeft res
    return $ toJSObject []

{- | Call connect user to session (all passed as URL params)
     and redirect user to referer
-}
connectUserToSessionPost :: Kontrakcja m => ServiceID -> UserID -> SessionId -> m KontraLink
connectUserToSessionPost sid uid ssid = do
    matchingService <-sameService sid <$> (runDBQuery $ GetUserByID uid)
    when (not matchingService) mzero
    loaded <- loadServiceSession (Right uid) ssid
    -- just send back empty string
    when loaded $ finishWith $ toResponseBS (BS.fromString "text/html;charset=utf-8") (BSL.fromString "")
    mzero

connectUserToSessionGet :: Kontrakcja m => ServiceID -> UserID -> SessionId -> m Response
connectUserToSessionGet _sid _uid _ssid = do
  rq <- askRq
  let uri = rqUri rq
  Log.debug $ "uri: " ++ uri
  referer <- look "referer"
  Log.debug $ "referer: " ++ referer
  bdy <- renderTemplateFM "connectredirect" $ do
    field "url" uri
    field "referer" referer
  simpleResponse bdy  

connectCompanyToSession :: Kontrakcja m => ServiceID -> CompanyID -> SessionId -> m KontraLink
connectCompanyToSession sid cid ssid = do
    matchingService <- sameService sid <$> (runDBQuery $ GetCompany cid)
    when (not matchingService) mzero
    loaded <- loadServiceSession (Left cid) ssid
    if (loaded)
     then return $ BackToReferer
     else mzero
