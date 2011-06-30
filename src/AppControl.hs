{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module AppControl
    ( appHandler
    , AppConf(..)
    , AppGlobals(..)
    , defaultAWSAction
    , parseEmailMessage
    , parseEmailMessageToParts
    ) where

import API.IntegrationAPI
import API.Service.ServiceState
import API.Service.ServiceControl
import API.UserAPI
import API.MailAPI

import ActionSchedulerState
import AppView as V
import Crypto
import Doc.DocState
import InputValidation
import Kontra
import KontraLink
import Mails.MailsConfig
import Mails.SendGridEvents
import Mails.SendMail
import MinutesTime
import Misc
import PayEx.PayExInterface ()-- Import so at least we check if it compiles
import Redirect
import Routing
import Session
import Templates.Templates (langVersion, readAllLangsTemplates, KontrakcjaMultilangTemplates, getTemplatesModTime)
import User.UserView as UserView
import qualified Administration.AdministrationControl as Administration
import qualified AppLogger as Log (error, security, debug)
import qualified Contacts.ContactsControl as Contacts
import qualified Doc.DocControl as DocControl
import qualified ELegitimation.BankID as BankID
import qualified FlashMessage as F
import qualified MemCache
import qualified Payments.PaymentsControl as Payments
import qualified TrustWeaver as TW
import qualified User.UserControl as UserControl

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.State
import Data.Functor
import Data.List
import Data.Maybe
import Data.Word
import GHC.Int (Int64(..))
import Happstack.Server hiding (simpleHTTP, host)
import Happstack.Server.Internal.Cookie
import Happstack.State (query)
import Happstack.State (update)
import ListUtil
import Network.Socket
import System.Directory
import System.Time

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import InspectXMLInstances ()
import InspectXML
import User.Lang

{- |
  Defines the application's configuration.  This includes amongst other things
  the http port number, amazon, trust weaver and email configuraton,
  as well as a handy boolean indicating whether this is a production or
  development instance.
-}
data AppConf
    = AppConf { httpBindAddress :: (Word32, Word16)  -- ^ tcp address to bind to and port to listen on
                                                  -- (0x0100007f, 8000) localhost:8000 (default)
                                                  -- (0, 80)   all interfaces port 80
              , hostpart        :: String                       -- ^ hostname as it should looklike in emails for example
              , store           :: FilePath                     -- ^ where to put database files
              , docstore        :: FilePath                     -- ^ where to put files (active if amazonConfig is Nothing)
              , static          :: FilePath                     -- ^ static files directory
              , amazonConfig    :: Maybe (String,String,String) -- ^ bucket, access key, secret key
              , gsCmd           :: String
              , production      :: Bool                         -- ^ production flag, enables some production stuff, disables some development
              , trustWeaverSign :: Maybe (String,String,String) -- ^ TrustWeaver sign service (URL,pem file path,pem private key password)
              , trustWeaverAdmin :: Maybe (String,String,String) -- ^ TrustWeaver admin service (URL,pem file path,pem private key password)
              , trustWeaverStorage :: Maybe (String,String,String) -- ^ TrustWeaver storage service (URL,pem file path,pem private key password)
              , mailsConfig     :: MailsConfig                  -- ^ mail sendout configuration
              , aesConfig       :: AESConf                     -- ^ aes key/iv for encryption
              , admins          :: [String]                    -- ^ email addresses of people regarded as admins
              }
      deriving (Show,Read,Eq,Ord)

{- |
  Global application data
-}
data AppGlobals
    = AppGlobals { templates       :: MVar (ClockTime, KontrakcjaMultilangTemplates)
                 , filecache       :: MemCache.MemCache FileID BS.ByteString
                 , mailer          :: Mailer
                 , docscache       :: MVar (Map.Map FileID JpegPages)
                 , esenforcer      :: MVar ()
                 }

{- |
   The routing table for the app.
   Routes in this table should be of the form
   dir "segment1" $ dir "segment2" $ .. $ dir "segmentn" $ hgetx $ handler
   OR
   dir "segment1" $ dir "segment2" $ .. $ dir "segmentn" $ hpostx $ handler

   param "name" is also allowed, which will guard based on the
   existence of a post/get param

   No other logic should be in here and no similar logic should be in the handler.
   That is, all routing logic should be in this table to ensure that we can find
   the function for any given path and method.
-}
handleRoutes :: Kontra Response
handleRoutes = msum [
       hGetAllowHttp0 $ handleHomepage
     , hPost0 $ handleMainReaload

     -- static pages
     , dir "webbkarta"       $ hGetAllowHttp0 $ handleSitemapPage
     , dir "priser"          $ hGetAllowHttp0 $ handlePriceplanPage
     , dir "sakerhet"        $ hGetAllowHttp0 $ handleSecurityPage
     , dir "juridik"         $ hGetAllowHttp0 $ handleLegalPage
     , dir "sekretesspolicy" $ hGetAllowHttp0 $ handlePrivacyPolicyPage
     , dir "allmana-villkor" $ hGetAllowHttp0 $ handleTermsPage
     , dir "om-skrivapa"     $ hGetAllowHttp0 $ handleAboutPage
     , dir "partners"        $ hGetAllowHttp0 $ handlePartnersPage
     , dir "kunder"          $ hGetAllowHttp0 $ handleClientsPage

     -- this is SMTP to HTTP gateway
     , dir "mailapi" $ mailAPI

     -- e-legitimation stuff
     -- I put this stuff up here because someone changed things out from under me
     -- I will rearrange this later
     , dir "s" $ hGet4                        $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPostNoXToken3 $ BankID.handleSignPostBankID
     , dir "d" $ hGet2                        $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost1        $ BankID.handleIssuePostBankID

     , dir "s" $ hGet0 $ DocControl.handleSTable
     , dir "s" $ hGet3 $ DocControl.handleSignShow
     , dir "s" $ hGet4 $ DocControl.handleAttachmentDownloadForViewer
     , dir "s" $ param "sign"           $ hPostNoXToken3 $ DocControl.signDocument
     , dir "s" $ param "cancel"         $ hPostNoXToken3 $ DocControl.rejectDocument
     , dir "s" $ param "acceptaccount"  $ hPostNoXToken5 $ DocControl.handleAcceptAccountFromSign
     , dir "s" $ param "declineaccount" $ hPostNoXToken5 $ DocControl.handleDeclineAccountFromSign
     , dir "s" $ param "sigattachment"  $ hPostNoXToken3 $ DocControl.handleSigAttach

     , dir "sv" $ hGet3 $ DocControl.handleAttachmentViewForViewer

     --Q: This all needs to be done by author. Why we dont check it
     --here? MR

     --A: Because this table only contains routing logic. The logic of
     --what it does/access control is left to the handler. EN
     , dir "a"                     $ hGet0  $ DocControl.showAttachmentList
     , dir "a" $ param "archive"   $ hPost0 $ DocControl.handleAttachmentArchive
     , dir "a" $ param "share"     $ hPost0 $ DocControl.handleAttachmentShare
     , dir "a" $ dir "rename"      $ hPost1 $ DocControl.handleAttachmentRename
     , dir "a"                     $ hPost0 $ DocControl.handleCreateNewAttachment

     , dir "t" $ hGet0  $ DocControl.showTemplatesList
     , dir "t" $ param "archive" $ hPost0 $ DocControl.handleTemplateArchive
     , dir "t" $ param "share" $ hPost0 $ DocControl.handleTemplateShare
     , dir "t" $ param "template" $ hPost0  $ DocControl.handleCreateFromTemplate
     , dir "t" $ hPost0  $ DocControl.handleCreateNewTemplate

     , dir "o" $ hGet0  $ DocControl.showOfferList
     , dir "o" $ param "archive" $ hPost0  $ DocControl.handleOffersArchive
     , dir "o" $ param "remind" $ hPost0 $ DocControl.handleBulkOfferRemind
     , dir "o" $ hPost0 $ DocControl.handleOffersReload

     , dir "or" $ hGet0  $ DocControl.showOrdersList
     , dir "or" $ param "archive" $ hPost0  $ DocControl.handleOrdersArchive
     , dir "or" $ param "remind" $ hPost0 $ DocControl.handleBulkOrderRemind
     , dir "or" $ hPost0 $ DocControl.handleOrdersReload

     , dir "d"                     $ hGet2  $ DocControl.handleAttachmentDownloadForAuthor
     , dir "d"                     $ hGet0  $ DocControl.showContractsList
     , dir "d"                     $ hGet1  $ DocControl.handleIssueShowGet
     , dir "d"                     $ hGet2  $ DocControl.handleIssueShowTitleGet
     , dir "d"                     $ hGet4  $ DocControl.handleIssueShowTitleGetForSignatory
     , dir "d" $ {- param "doc" $ -} hPost0 $ DocControl.handleIssueNewDocument
     , dir "d" $ param "archive"   $ hPost0 $ DocControl.handleContractArchive
     , dir "d" $ param "remind"    $ hPost0 $ DocControl.handleBulkContractRemind
     , dir "d"                     $ hPost0 $ DocControl.handleContractsReload
     , dir "d"                     $ hPost1 $ DocControl.handleIssueShowPost


     , dir "df"                    $ hGet2  $ DocControl.handleFileGet
     , dir "dv"                    $ hGet1  $ DocControl.handleAttachmentViewForAuthor

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hPost2 $ DocControl.handleResend
     , dir "changeemail" $ hPost2 $ DocControl.handleChangeSignatoryEmail
     -- , dir "withdrawn" $ hPost0 $ DocControl.handleWithdrawn
     , dir "restart" $ hPost1 $ DocControl.handleRestart
     , dir "cancel"  $ hPost1 $ DocControl.handleCancel

     , dir "pages"  $ hGetAjax3 $ DocControl.showPage
     , dir "pages"  $ hGetAjax5 $ DocControl.showPageForSignatory
     , dir "templates" $ hGetAjax0 $ DocControl.getTemplatesForAjax
     , dir "template"  $ hPost0 $ DocControl.handleCreateFromTemplate

     , dir "pagesofdoc" $ hGetAjax1 $ DocControl.handlePageOfDocument
     , dir "pagesofdoc" $ hGetAjax3 $ DocControl.handlePageOfDocumentForSignatory

     -- UserControl
     , dir "account"                    $ hGet0  $ UserControl.handleUserGet
     , dir "account"                    $ hPost0 $ UserControl.handleUserPost
     , dir "account" $ dir "subaccount" $ hGet0  $ UserControl.handleGetSubaccount
     , dir "account" $ dir "subaccount" $ hPost0 $ UserControl.handlePostSubaccount
     , dir "account" $ dir "sharing" $ hGet0 $ UserControl.handleGetSharing
     , dir "account" $ dir "sharing" $ hPost0 $ UserControl.handlePostSharing
     , dir "account" $ dir "security" $ hGet0 $ UserControl.handleGetUserSecurity
     , dir "account" $ dir "security" $ hPost0 $ UserControl.handlePostUserSecurity
     , dir "account" $ dir "mailapi" $ hGet0 $ UserControl.handleGetUserMailAPI
     , dir "account" $ dir "mailapi" $ hPost0 $ UserControl.handlePostUserMailAPI
     , dir "account" $ dir "bsa" $ hGet1 $ UserControl.handleGetBecomeSubaccountOf
     , dir "account" $ dir "bsa" $ hPost1 $ UserControl.handlePostBecomeSubaccountOf
     , dir "contacts"  $ hGet0  $ Contacts.showContacts
     , dir "contacts"  $ hPost0 $ Contacts.handleContactsChange
     , dir "accepttos" $ hGet0  $ UserControl.handleAcceptTOSGet
     , dir "accepttos" $ hPost0 $ UserControl.handleAcceptTOSPost

     -- super user only
     , dir "stats"      $ hGet0  $ Administration.showStats
     , dir "createuser" $ hPost0 $ Administration.handleCreateUser
     , dir "sendgrid" $ dir "events" $ hPostNoXToken0 handleSendgridEvent
     , dir "adminonly" $ hGet0 $ Administration.showAdminMainPage
     , dir "adminonly" $ dir "advuseradmin" $ hGet0 Administration.showAdminUserAdvanced
     , dir "adminonly" $ dir "useradminforsales" $ hGet0 $ Administration.showAdminUsersForSales
     , dir "adminonly" $ dir "useradminforpayments" $ hGet0 $ Administration.showAdminUsersForPayments
     , dir "adminonly" $ dir "useradmin" $ hGet1 $ Administration.showAdminUsers . Just
     , dir "adminonly" $ dir "useradmin" $ hGet0 $ Administration.showAdminUsers Nothing
     , dir "adminonly" $ dir "useradmin" $ dir "usagestats" $ hGet1 $ Administration.showAdminUserUsageStats
     , dir "adminonly" $ dir "useradmin" $ hPost1 Administration.handleUserChange
     , dir "adminonly" $ dir "useradmin" $ hPost1 Administration.handleUserEnableTrustWeaverStorage
     , dir "adminonly" $ dir "db" $ hGet0 $ Administration.indexDB
     , dir "adminonly" $ dir "db" $ onlySuperUser $ serveDirectory DisableBrowsing [] "_local/kontrakcja_state"
     , dir "adminonly" $ dir "quarantine" $ hGet0  $ Administration.handleShowQuarantine
     , dir "adminonly" $ dir "quarantine" $ hPost0 $ Administration.handleQuarantinePost

     , dir "adminonly" $ dir "cleanup"           $ hPost0 $ Administration.handleDatabaseCleanup
     , dir "adminonly" $ dir "statistics"        $ hGet0  $ Administration.handleStatistics
     , dir "adminonly" $ dir "skrivapausers.csv" $ hGet0  $ Administration.getUsersDetailsToCSV
     , dir "adminonly" $ dir "payments"          $ hGet0  $ Payments.handlePaymentsModelForViewView
     , dir "adminonly" $ dir "advpayments"       $ hGet0  $ Payments.handlePaymentsModelForEditView
     , dir "adminonly" $ dir "advpayments"       $ hPost0 $ Payments.handleAccountModelsChange

     , dir "adminonly" $ dir "services" $ hGet0 $ Administration.showServicesPage
     , dir "adminonly" $ dir "services" $ param "create" $ hPost0 $ Administration.handleCreateService
     , dir "adminonly" $ dir "translations" $ hGet0 $ Administration.showAdminTranslations

     -- a temporary service to help migration

     , dir "adminonly" $ dir "migrate0" $ hGet0 $ Administration.handleMigrate0
     , dir "adminonly" $ dir "deletemigrate" $ hGet0 $ Administration.handleMigrateForDeletion
     , dir "adminonly" $ dir "migrateattachments" $ hGet0 $ DocControl.handleMigrateDocumentAuthorAttachments
     , dir "adminonly" $ dir "makesigauthor" $ hGet0 $ Administration.migrateDocsNoAuthor

--     , dir "adminonly" $ dir "migrateauthor" $ hGet0 $ DocControl.migrateDocSigLinks
     , dir "adminonly" $ dir "unquarantineall" $ hGet0 $ Administration.handleUnquarantineAll

     , dir "services" $ hGet0 $ handleShowServiceList
     , dir "services" $ hGet1 $ handleShowService
     , dir "services" $ dir "ui" $ hPost1 $ handleChangeServiceUI
     , dir "services" $ dir "password" $ hPost1 $ handleChangeServicePassword
     , dir "services" $ dir "settings" $ hPost1 $ handleChangeServiceSettings
     , dir "services" $ dir "logo" $ hGet1 handleServiceLogo
     , dir "services" $ dir "buttons_body" $ hGet1 handleServiceButtonsBody
     , dir "services" $ dir "buttons_rest" $ hGet1 handleServiceButtonsRest
     , dir "dave" $ dir "document" $ hGet1 $ daveDocument
     , dir "dave" $ dir "user"     $ hGet1 $ daveUser

     -- account stuff
     , dir "logout"      $ hGet0  $ handleLogout
     , dir "login"       $ hGet0  $ handleLoginGet
     , dir "login"       $ hPostNoXToken0 $ handleLoginPost
     --, dir "signup"      $ hGet0  $ signupPageGet
     , dir "signup"      $ hPostAllowHttp0 $ signupPagePost
     --, dir "vip"         $ hGet0  $ signupVipPageGet
     --, dir "vip"         $ hPostNoXToken $ signupVipPagePost
     , dir "amnesia"     $ hPostNoXToken0 $ forgotPasswordPagePost
     , dir "amnesia"     $ hGet2  $ UserControl.handlePasswordReminderGet
     , dir "amnesia"     $ hPostNoXToken2 $ UserControl.handlePasswordReminderPost
     , dir "accountsetup"  $ hGet2  $ UserControl.handleAccountSetupGet
     , dir "accountsetup"  $ hPostNoXToken2  $ UserControl.handleAccountSetupPost
     , dir "accountremoval" $ hGet2  $ UserControl.handleAccountRemovalGet
     , dir "accountremoval" $ hPostNoXToken2  $ UserControl.handleAccountRemovalPost

     -- viral invite
     , dir "invite"      $ hPostNoXToken0 $ UserControl.handleViralInvite
     , dir "question"    $ hPostAllowHttp0 $ UserControl.handleQuestion
     -- e-legitimation stuff
     , dir "s" $ hGet4  $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPost3 $ BankID.handleSignPostBankID
     , dir "d" $ hGet2  $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost1 $ BankID.handleIssuePostBankID
     , userAPI
     , integrationAPI
     -- static files
     , allowHttp $ serveHTMLFiles
     , allowHttp $ serveDirectory DisableBrowsing [] "public"
               ]

{- |
   Goes to the front page, or to the main document upload page,
   depending on whether there is a logged in user.
-}
handleHomepage :: Kontra (Either Response (Either KontraLink String))
handleHomepage = do
  ctx@Context{ ctxmaybeuser,ctxservice } <- get
  loginOn <- isFieldSet "logging"
  referer <- getField "referer"
  email   <- getField "email"
  case (ctxmaybeuser, ctxservice) of
    (Just _, _) -> Right <$> (UserControl.checkUserTOSGet DocControl.mainPage)
    (Nothing, Nothing) -> do
      response <- V.simpleResponse =<< (liftIO $ firstPage ctx loginOn referer email)
      clearFlashMsgs
      return $ Left response
    _ -> Left <$> embeddedErrorPage

handleSitemapPage :: Kontra Response
handleSitemapPage = handleWholePage sitemapPage

handlePriceplanPage :: Kontra Response
handlePriceplanPage = handleWholePage priceplanPage

handleSecurityPage :: Kontra Response
handleSecurityPage = handleWholePage securityPage

handleLegalPage :: Kontra Response
handleLegalPage = handleWholePage legalPage

handlePrivacyPolicyPage :: Kontra Response
handlePrivacyPolicyPage = handleWholePage privacyPolicyPage

handleTermsPage :: Kontra Response
handleTermsPage = handleWholePage termsPage

handleAboutPage :: Kontra Response
handleAboutPage = handleWholePage aboutPage

handlePartnersPage :: Kontra Response
handlePartnersPage = handleWholePage partnersPage

handleClientsPage :: Kontra Response
handleClientsPage = handleWholePage clientsPage

handleWholePage :: Kontra String -> Kontra Response
handleWholePage f = do
  content <- f
  response <- V.simpleResponse content
  clearFlashMsgs
  return response

{- |
    Handles an error by displaying the home page with a modal error dialog.
-}
handleError :: Kontra Response
handleError = do
    ctx <- get
    case (ctxservice ctx) of
         Nothing -> do
            addModal $ V.modalError (ctxtemplates ctx)
            sendRedirect LinkMain
         Just _ -> embeddedErrorPage

handleMainReaload :: Kontra KontraLink
handleMainReaload = do
    liftM3 LinkNew DocControl.getDocProcess getListParamsForSearch (isFieldSet "showTemplates")

{- |
   Creates a default amazon configuration based on the
   given AppConf
-}
defaultAWSAction :: AppConf -> AWS.S3Action
defaultAWSAction appConf =
    let (bucket,accessKey,secretKey) = maybe ("","","") id (amazonConfig appConf)
    in
    AWS.S3Action
           { AWS.s3conn = AWS.amazonS3Connection accessKey secretKey
           , AWS.s3bucket = bucket
           , AWS.s3object = ""
           , AWS.s3query = ""
           , AWS.s3metadata = []
           , AWS.s3body = BSL.empty
           , AWS.s3operation = HTTP.GET
           }


maybeReadTemplates :: MVar (ClockTime, KontrakcjaMultilangTemplates)
                      -> IO KontrakcjaMultilangTemplates
maybeReadTemplates mvar = modifyMVar mvar $ \(modtime, templates) -> do
        modtime' <- getTemplatesModTime
        if modtime /= modtime'
            then do
                Log.debug $ "Reloading templates"
                templates' <- readAllLangsTemplates
                return ((modtime', templates'), templates')
            else return ((modtime, templates), templates)

showNamedHeader :: forall t . (t, HeaderPair) -> [Char]
showNamedHeader (_nm,hd) = BS.toString (hName hd) ++ ": [" ++
                      concat (intersperse ", " (map (show . BS.toString) (hValue hd))) ++ "]"

showNamedCookie :: ([Char], Cookie) -> [Char]
showNamedCookie (name,cookie) = name ++ ": " ++ mkCookieHeader Nothing cookie

showNamedInput :: ([Char], Input) -> [Char]
showNamedInput (name,input) = name ++ ": " ++ case inputFilename input of
                                                  Just filename -> filename
                                                  _ -> case inputValue input of
                                                           Left _tmpfilename -> "<<content in /tmp>>"
                                                           Right value -> show (BSL.toString value)

showRequest :: Request -> Maybe [([Char], Input)] -> [Char]
showRequest rq maybeInputsBody =
    show (rqMethod rq) ++ " " ++ rqUri rq ++ rqQuery rq ++ "\n" ++
    "post variables:\n" ++
    maybe "" (unlines . map showNamedInput) maybeInputsBody ++
    "http headers:\n" ++
    (unlines $ map showNamedHeader (Map.toList $ rqHeaders rq)) ++
    "http cookies:\n" ++
    (unlines $ map showNamedCookie (rqCookies rq))

{- |
   Creates a context, routes the request, and handles the session.
-}
appHandler :: AppConf -> AppGlobals -> ServerPartT IO Response
appHandler appConf appGlobals = do
  let quota :: GHC.Int.Int64 = 10000000

  temp <- liftIO $ getTemporaryDirectory
  decodeBody (defaultBodyPolicy temp quota quota quota)

  rq <- askRq
  session <- handleSession
  ctx <- createContext rq session
  handle rq session ctx
  where
    handle :: Request -> Session -> Context -> ServerPartT IO Response
    handle rq session ctx = do
      (res,ctx')<- toIO ctx $
         do
          res <- (handleRoutes) `mplus` do
             rqcontent <- liftIO $ tryTakeMVar (rqInputsBody rq)
             when (isJust rqcontent) $
                 liftIO $ putMVar (rqInputsBody rq) (fromJust rqcontent)
             Log.error $ showRequest rq rqcontent
             response <- handleError
             setRsCode 404 response
          ctx' <- get
          return (res,ctx')

      let newsessionuser = fmap userid $ ctxmaybeuser ctx'
      let newflashmessages = ctxflashmessages ctx'
      let newelegtrans = ctxelegtransactions ctx'
      F.updateFlashCookie (aesConfig appConf) (ctxflashmessages ctx) newflashmessages
      updateSessionWithContextData session newsessionuser newelegtrans
      return res

    createContext :: Request -> Session -> ServerPartT IO Context
    createContext rq session = do
      hostpart <- getHostpart
      -- FIXME: we should read some headers from upstream proxy, if any
      let peerhost = case getHeader "x-real-ip" rq of
                       Just name -> BS.toString name
                       Nothing -> fst (rqPeer rq)

      -- rqPeer hostname comes always from showHostAddress
      -- so it is a bunch of numbers, just read them out
      -- getAddrInfo is strange that it can throw exceptions
      -- if exception is thrown, whole page load fails with
      -- error notification
      let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST] }
      addrs <- liftIO $ getAddrInfo (Just hints) (Just peerhost) Nothing
      let addr = head addrs
      let peerip = case addrAddress addr of
                     SockAddrInet _ hostip -> hostip
                     _ -> 0
      let browserLang = langFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
      minutestime <- liftIO $ getMinutesTime
      muser <- getUserFromSession session
      mcompany <- getCompanyFromSession session
      location <- getLocationFromSession session
      mservice <- query . GetServiceByLocation . toServiceLocation =<< currentLink
      flashmessages <- withDataFn F.flashDataFromCookie $ maybe (return []) $ \fval ->
          case F.fromCookieValue (aesConfig appConf) fval of
               Just flashes -> return flashes
               Nothing -> do
                   Log.error $ "Couldn't read flash messages from value: " ++ fval
                   F.removeFlashCookie
                   return []

      -- do reload templates in non-production code
      templates2 <- liftIO $ maybeReadTemplates (templates appGlobals)

      let elegtrans = getELegTransactions session
          ctx = Context
                { ctxmaybeuser = muser
                , ctxhostpart = hostpart
                , ctxflashmessages = flashmessages
                , ctxtime = minutestime
                , ctxnormalizeddocuments = docscache appGlobals
                , ctxipnumber = peerip
                , ctxdocstore = docstore appConf
                , ctxs3action = defaultAWSAction appConf
                , ctxgscmd = gsCmd appConf
                , ctxproduction = production appConf
                , ctxtemplates = langVersion (fromMaybe browserLang $ lang <$> usersettings <$> muser ) templates2
                , ctxesenforcer = esenforcer appGlobals
                , ctxtwconf = TW.TrustWeaverConf
                              { TW.signConf = trustWeaverSign appConf
                              , TW.adminConf = trustWeaverAdmin appConf
                              , TW.storageConf = trustWeaverStorage appConf
                              , TW.retries = 3
                              , TW.timeout = 60000
                              }
                , ctxelegtransactions = elegtrans
                , ctxfilecache = filecache appGlobals
                , ctxxtoken = getSessionXToken session
                , ctxcompany = mcompany
                , ctxservice = mservice
                , ctxlocation = location
                , ctxadminaccounts = map (Email . BS.fromString) (admins appConf)
                }
      return ctx

{- |
   Handles submission of the password reset form
-}
forgotPasswordPagePost :: Kontra KontraLink
forgotPasswordPagePost = do
  ctx <- get
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- query $ GetUserByEmail Nothing $ Email email
      case muser of
        Nothing -> do
          Log.security $ "ip " ++ (show $ ctxipnumber ctx) ++ " made a failed password reset request for non-existant account " ++ (BS.toString email)
          return LoopBack
        Just user -> do
          now <- liftIO getMinutesTime
          minv <- checkValidity now <$> (query $ GetPasswordReminder $ userid user)
          case minv of
            Just Action{ actionID, actionType = PasswordReminder { prToken, prRemainedEmails, prUserID } } ->
              case prRemainedEmails of
                0 -> addFlashMsg =<< (liftIO $ flashMessageNoRemainedPasswordReminderEmails $ ctxtemplates ctx)
                n -> do
                  -- I had to make it PasswordReminder because it was complaining about not giving cases
                  -- for the constructors of ActionType
                  _ <- update $ UpdateActionType actionID (PasswordReminder { prToken          = prToken
                                                                            , prRemainedEmails = n - 1
                                                                            , prUserID         = prUserID})
                  sendResetPasswordMail ctx (LinkPasswordReminder actionID prToken) user
            _ -> do -- Nothing or other ActionTypes (which should not happen)
              link <- newPasswordReminderLink user
              sendResetPasswordMail ctx link user
          addFlashMsg =<< (liftIO $ flashMessageChangePasswordEmailSend $ ctxtemplates ctx)
          return LinkMain

sendResetPasswordMail :: Context -> KontraLink -> User -> Kontra ()
sendResetPasswordMail ctx link user = do
  mail <- liftIO $ UserView.resetPasswordMail (ctxtemplates ctx) (ctxhostpart ctx) user link
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress { fullname = userfullname user
                                                                      , email = unEmail $ useremail $ userinfo user }]}

{- |
   Handles viewing of the signup page
-}
_signupPageGet :: Kontra Response
_signupPageGet = do
    ctx <- lift get
    content <- liftIO (signupPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja  content


_signupVipPageGet :: Kontra Response
_signupVipPageGet = do
    ctx <- lift get
    content <- liftIO (signupVipPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja content
{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}
signupPagePost :: Kontra KontraLink
signupPagePost = do
    Context { ctxtime } <- get
    signup False $ Just ((60 * 24 * 31) `minutesAfter` ctxtime)

_signupVipPagePost :: Kontra KontraLink
_signupVipPagePost = signup True $ parseMinutesTimeMDY "31-12-2011"

{-
    A comment next to LoopBack says never to use it. Is this function broken?
-}
signup :: Bool -> (Maybe MinutesTime) -> Kontra KontraLink
signup vip _freetill =  do
  ctx@Context{ctxtemplates,ctxhostpart} <- lift get
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- query $ GetUserByEmail Nothing $ Email $ email
      case  muser of
        Just user ->
          if isNothing $ userhasacceptedtermsofservice user
          then do
            al <- newAccountCreatedLink user
            mail <- liftIO $ newUserMail (ctxtemplates) (ctxhostpart) email email al vip
            scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress {fullname = email, email = email}] }
            addFlashMsg =<< (liftIO $ flashMessageNewActivationLinkSend  (ctxtemplates))
            return LoopBack
          else do
            addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
            return LoopBack
        Nothing -> do
          maccount <- liftIO $ UserControl.createUser ctx ctxhostpart (BS.empty, BS.empty) email Nothing vip
          case maccount of
            Just _account ->  do
              addFlashMsg =<< (liftIO $ flashMessageUserSignupDone ctxtemplates)
              return LoopBack
            Nothing -> do
              addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
              return LoopBack

{- |
   Sends a new activation link mail, which is really just a new user mail.
-}
_sendNewActivationLinkMail:: Context -> User -> Kontra ()
_sendNewActivationLinkMail Context{ctxtemplates,ctxhostpart,ctxesenforcer} user = do
    let email = unEmail $ useremail $ userinfo user
    al <- newAccountCreatedLink user
    mail <- liftIO $ newUserMail ctxtemplates ctxhostpart email email al False
    scheduleEmailSendout ctxesenforcer $ mail { to = [MailAddress {fullname = email, email = email}] }

{- |
   Handles viewing of the login page
-}
handleLoginGet :: Kontra Response
handleLoginGet = do
  ctx <- lift get
  case ctxmaybeuser ctx of
       Just _  -> sendRedirect LinkMain
       Nothing -> do
         referer <- getField "referer"
         email   <- getField "email"
         content <- liftIO $ V.pageLogin ctx referer email
         V.renderFromBody V.TopNone V.kontrakcja content

{- |
   Handles submission of a login form.  On failure will redirect back to referer, if there is one.
-}
handleLoginPost :: Kontra KontraLink
handleLoginPost = do
    memail  <- getOptionalField asDirtyEmail    "email"
    mpasswd <- getOptionalField asDirtyPassword "password"
    let linkemail = maybe "" BS.toString memail
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- query $ GetUserByEmail Nothing (Email email)
            case maybeuser of
                Just User{ userid, userpassword }
                    | verifyPassword userpassword passwd -> do
                        logUserToContext maybeuser
                        time <- liftIO getMinutesTime
                        _ <- update $ RecordSuccessfulLogin userid time
                        return BackToReferer
                Just User{userid } -> do
                        time <- liftIO getMinutesTime
                        _ <- update $ RecordFailedLogin userid time
                        return $ LinkLogin $ InvalidLoginInfo linkemail
                Nothing -> return $ LinkLogin $ InvalidLoginInfo linkemail
        _ -> return $ LinkLogin $ InvalidLoginInfo linkemail

{- |
   Handles the logout, and sends user back to main page.
-}
handleLogout :: Kontra Response
handleLogout = do
    logUserToContext Nothing
    sendRedirect LinkMain

{- |
   Serves out the static html files.
-}
serveHTMLFiles:: Kontra Response
serveHTMLFiles =  do
    rq <- askRq
    let fileName = last (rqPaths rq)
    if ((length (rqPaths rq) > 0) && (isSuffixOf ".html" fileName))
        then do
            ms <- liftIO $ catch (fmap Just ( BS.readFile $ "html/"++fileName))
                            (const $ return Nothing)
            case ms of
                (Just s) -> renderFromBody V.TopNone V.kontrakcja $ BS.toString s
                _      -> mzero
        else mzero

{- |
   Ensures logged in as a super user
-}
onlySuperUserGet :: Kontra Response -> Kontra Response
onlySuperUserGet action = do
    Context{ ctxadminaccounts, ctxmaybeuser } <- get
    if isSuperUser ctxadminaccounts ctxmaybeuser
        then action
        else sendRedirect $ LinkLogin NotLoggedAsSuperUser

{- |
   Used by super users to inspect a particular document.
-}
daveDocument :: DocumentID -> Kontra Response
daveDocument documentid = onlySuperUserGet $ do
    document <- queryOrFail $ GetDocumentByDocumentIDAllEvenQuarantinedDocuments documentid
    V.renderFromBody V.TopNone V.kontrakcja $ inspectXML document

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: UserID -> Kontra Response
daveUser userid = onlySuperUserGet $ do
    user <- queryOrFail $ GetUserByUserID userid
    V.renderFromBody V.TopNone V.kontrakcja $ inspectXML user

