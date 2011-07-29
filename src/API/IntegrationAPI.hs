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
module API.IntegrationAPI (integrationAPI) where

import Control.Monad.State
import Data.Functor
import Data.Maybe
import Doc.DocState
import Happstack.Server hiding (simpleHTTP,host,body)
import Happstack.State (query, update)
import KontraLink
import MinutesTime
import Misc
import Session
import Kontra
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
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
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.ServiceUtils
import Util.MonadUtils

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
    mservice <- query $ GetService (ServiceID sid)
    case mservice of
         Just service -> do
             passwd <- getFieldUTFWithDefault BS.empty "password"
             if (verifyPassword (servicepassword $ servicesettings service) passwd)
                then return $ Just service
                else return Nothing
         Nothing -> return Nothing

integrationAPI :: Kontra Response
integrationAPI = dir "integration" $ msum [
      dir "api" $  apiCall "embed_document_frame" embeddDocumentFrame :: Kontrakcja m => m Response
    , dir "api" $ apiCall "new_document" createDocument              :: Kontrakcja m => m Response
    , dir "api" $ apiCall "documents" getDocuments                   :: Kontrakcja m => m Response
    , dir "api" $ apiCall "document" getDocument                     :: Kontrakcja m => m Response
    , dir "api" $ apiCall "set_document_tag" setDocumentTag          :: Kontrakcja m => m Response
    , dir "api" $ apiCall "remove_document" removeDocument           :: Kontrakcja m => m Response
    , dir "api" $ apiUnknownCall
    , dir "connectuser" $ hGet3 $ toK3 $ connectUserToSession
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
    let slocation = fromMaybe (ctxhostpart ctx) $ show <$> (servicelocation $ servicesettings srvs)
    let returnLink l =  return $ toJSObject [ ("link",JSString $ toJSString $ slocation ++ show l)]
    location <- fold <$> apiAskString "location"
    doc <- documentFromParam
    mcompany <- lift_M (update . GetOrCreateCompanyWithExternalID  (Just sid) ) $ maybeReadM $ apiAskString "company_id"
    msiglink <- liftMM (return . getSigLinkFor doc) (apiAskBS "email")
    case (mcompany,msiglink) of
         (Just company,Nothing) -> do
             when (not $ sameService srvs company) $ throwApiError API_ERROR_MISSING_VALUE "Not matching company | This should never happend"
             ssid <- createServiceSession (Left $ companyid $ company) location
             returnLink $ LinkConnectCompanySession sid (companyid company) ssid $ LinkIssueDoc (documentid doc)
         (Just company, Just siglink) -> do
             if (isAuthor siglink && (isJust $ maybesignatory siglink))
                then do
                     muser <- query $ GetUserByUserID $ fromJust $ maybesignatory siglink
                     when (not $ sameService sid muser && sameService srvs company) $ throwApiError API_ERROR_MISSING_VALUE "Not matching user or company| This should never happend"
                     ssid <- createServiceSession (Right $ fromJust $ maybesignatory siglink) location
                     returnLink $ LinkConnectUserSession sid  (fromJust $ maybesignatory siglink) ssid $ LinkIssueDoc (documentid doc)
                else do
                     when (not $ sameService srvs company) $ throwApiError API_ERROR_MISSING_VALUE "Not matching company | This should never happend"
                     ssid <- createServiceSession (Left $ companyid $ company) location
                     returnLink $ LinkConnectCompanySession sid (companyid company) ssid $ LinkIssueDoc (documentid doc)
         _ -> throwApiError API_ERROR_MISSING_VALUE "At least company connected to document must be provided."


createDocument :: Kontrakcja m => IntegrationAPIFunction m APIResponse
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
   liftIO $ putStrLn $ show $ doc
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
    company <- setCompanyInfoFromTMP authorTMP company' --TODO EM i added this line to update the company info
    author <- userFromTMP authorTMP company --TODO EM i added company as an arg here
    mdoc <- update $ NewDocument author (Just company) title doctype now
    when (isLeft mdoc) $ throwApiError API_ERROR_OTHER "Problem created a document | This may be because the company and author don't match" --TODO EM can the user and company not match?!
    let doc = fromRight mdoc
    sequence_  $ map (update . uncurry (AttachFile $ documentid doc)) files
    _ <- update $ SetDocumentTags (documentid doc) tags
    doc' <- update $ UpdateDocumentSimple (documentid doc) (toSignatoryDetails authorTMP, author) (map toSignatoryDetails signTMPS)
    when (isLeft doc') $ throwApiError API_ERROR_OTHER "Problem creating a document (SIGUPDATE) | This should never happend"
    return $ fromRight doc'

userFromTMP :: Kontrakcja m => SignatoryTMP -> Company -> IntegrationAPIFunction m User
userFromTMP uTMP company = do
    sid <- serviceid <$> service <$> ask
    let remail = fold $ asValidEmail . BS.toString <$> email uTMP
    when (not $ isGood $ remail) $ throwApiError API_ERROR_OTHER "NOT valid email for first involved person"
    muser <- query $ GetUserByEmail (Just sid) $ Email $ fromGood remail
    user <- case muser of
              Just u -> return u
              Nothing -> do
                password <- liftIO $ createPassword . BS.fromString =<< (sequence $ replicate 12 randomIO)
                u <- update $ AddUser (fold $ fstname uTMP,fold $ sndname uTMP) (fromGood remail) password False (Just sid) (Just $ companyid company) defaultValue --TODO EM is this a user for the company?  if so should they be a company admin?  will users from the past migrate okay?
                when (isNothing u) $ throwApiError API_ERROR_OTHER "Problem creating a user (BASE) | This should never happend"
                u' <- update $ AcceptTermsOfService (userid $ fromJust u) (fromSeconds 0)
                when (isLeft u') $ throwApiError API_ERROR_OTHER "Problem creating a user (TOS) | This should never happend"
                return $ fromRight u'
    user' <- update $ SetUserInfo (userid user) (userinfo user)
            {
              userfstname = fromMaybe (getFirstName user) $ fstname uTMP
            , usersndname = fromMaybe (getFirstName user) $ sndname uTMP
            , userpersonalnumber = fromMaybe (getPersonalNumber user) $ personalnumber uTMP
            }
    when (isLeft user') $ throwApiError API_ERROR_OTHER "Problem creating a user (INFO) | This should never happend"
    return $ fromRight user'

setCompanyInfoFromTMP :: Kontrakcja m => SignatoryTMP -> Company -> IntegrationAPIFunction m Company
setCompanyInfoFromTMP uTMP company = do
    --TODO EM is this okay?  also users from the past, will they migrate okay
    company' <- update $ SetCompanyInfo company (companyinfo company)
                {
                  companyname = fromMaybe (getCompanyName company) $ API.APICommons.company uTMP
                , Company.CompanyState.companynumber = fromMaybe (getCompanyNumber company) $ API.APICommons.companynumber uTMP
                }
    when (isLeft company') $ throwApiError API_ERROR_OTHER "Problem create a user (COMPANY INFO) | This should never happen"
    return $ fromRight company'

getDocuments :: Kontrakcja m => IntegrationAPIFunction m APIResponse
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
    linkeddocuments <- query $ GetDocumentsByCompanyAndTags (Just sid) (companyid company) tags
    let documents = filter (isAuthoredByCompany $ companyid company) linkeddocuments -- TODO EM is this okay?  it picks out the docs authored by company, rather than just linked to the company (for example those it's been invited to sign)
    let notDeleted doc =  any (not . signatorylinkdeleted) $ documentsignatorylinks doc
    -- We support only offers and contracts by API calls    
    let supportedType doc = documenttype doc `elem` [Template Contract, Template Offer, Signable Contract, Signable Offer]
    api_docs <- sequence $  map (api_document False) $ filter (\d -> notDeleted d && supportedType d) documents
    return $ toJSObject [("documents",JSArray $ api_docs)]
    where
      isAuthoredByCompany :: CompanyID -> Document -> Bool
      isAuthoredByCompany companyid doc = (getAuthorSigLink doc >>= maybecompany) == Just companyid


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
    res <- update $ ArchiveDocumentForAll $ documentid doc
    when (isLeft res) $ throwApiError API_ERROR_NO_DOCUMENT $ "Error while removing a document: " ++ fromLeft res
    return $ toJSObject []

{- | Call connect user to session (all passed as URL params)
     and redirect user to referer
-}
connectUserToSession :: Kontrakcja m => ServiceID -> UserID -> SessionId -> m KontraLink
connectUserToSession sid uid ssid = do
    matchingService <-sameService sid <$> (query $ GetUserByUserID uid)
    when (not matchingService) mzero
    loaded <- loadServiceSession (Right uid) ssid
    if (loaded)
     then return $ BackToReferer
     else mzero

connectCompanyToSession :: Kontrakcja m => ServiceID -> CompanyID -> SessionId -> m KontraLink
connectCompanyToSession sid cid ssid = do
    matchingService <- sameService sid <$> (query $ GetCompany cid)
    when (not matchingService) mzero
    loaded <- loadServiceSession (Left cid) ssid
    if (loaded)
     then return $ BackToReferer
     else mzero

