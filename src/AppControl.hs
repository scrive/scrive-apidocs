{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module AppControl
    ( module AppConf
    , appHandler
    , AppGlobals(..)
    , defaultAWSAction
    , handleLoginPost
    ) where

import AppConf
import API.IntegrationAPI
import API.Service.Model
import API.Service.ServiceControl
import API.UserAPI
import API.MailAPI

import ActionSchedulerState
import AppView as V
import DB.Classes
import Doc.DocState
import InputValidation
import Kontra
import KontraLink
import Mails.SendGridEvents
import Mails.SendMail
import MinutesTime
import Misc
--import PayEx.PayExInterface ()-- Import so at least we check if it compiles
import Redirect
import Routing
import Session
import Templates.Templates
import User.Model
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
import Util.FlashUtil
import Util.HasSomeUserInfo

import Control.Concurrent
import Control.Monad.Error
import Data.Functor
import Data.List
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import GHC.Int (Int64(..))
import Happstack.Server hiding (simpleHTTP, host)
import Happstack.Server.Internal.Cookie
import Happstack.State (query, update)
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
import Util.MonadUtils
import ForkAction


{- |
  Global application data
-}
data AppGlobals
    = AppGlobals { templates       :: MVar (ClockTime, KontrakcjaGlobalTemplates)
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
     , mailAPI

     -- e-legitimation stuff
     -- I put this stuff up here because someone changed things out from under me
     -- I will rearrange this later
     , dir "s" $ hGet4                        $ toK4 $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPostNoXToken3 $ toK3 $ BankID.handleSignPostBankID
     , dir "d" $ hGet2                        $ toK2 $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost1        $ toK1 $ BankID.handleIssuePostBankID

     , dir "s" $ hGet0 $ toK0 $ sendRedirect $ LinkContracts
     , dir "s" $ hGet3 $ toK3 $ DocControl.handleSignShow
     , dir "s" $ hGet4 $ toK4 $ DocControl.handleAttachmentDownloadForViewer
     , dir "s" $ param "sign"           $ hPostNoXToken3 $ toK3 $ DocControl.signDocument
     , dir "s" $ param "cancel"         $ hPostNoXToken3 $ toK3 $ DocControl.rejectDocument
     , dir "s" $ param "acceptaccount"  $ hPostNoXToken5 $ toK5 $ DocControl.handleAcceptAccountFromSign
     , dir "s" $ param "declineaccount" $ hPostNoXToken5 $ toK5 $ DocControl.handleDeclineAccountFromSign
     , dir "s" $ param "sigattachment"  $ hPostNoXToken3 $ toK3 $ DocControl.handleSigAttach
     , dir "s" $ param "deletesigattachment" $ hPostNoXToken3 $ toK3 $ DocControl.handleDeleteSigAttach

     , dir "sv" $ hGet3 $ toK3 $ DocControl.handleAttachmentViewForViewer

     --Q: This all needs to be done by author. Why we dont check it
     --here? MR

     --A: Because this table only contains routing logic. The logic of
     --what it does/access control is left to the handler. EN
     , dir "a"                     $ hGet0  $ toK0 $ DocControl.showAttachmentList
     , dir "a" $ param "archive"   $ hPost0 $ toK0 $ DocControl.handleAttachmentArchive
     , dir "a" $ param "share"     $ hPost0 $ toK0 $ DocControl.handleAttachmentShare
     , dir "a" $ dir "rename"      $ hPost1 $ toK1 $ DocControl.handleAttachmentRename
     , dir "a"                     $ hPost0 $ toK0 $ DocControl.handleCreateNewAttachment

     , dir "t" $ hGet0  $ toK0 $ DocControl.showTemplatesList
     , dir "t" $ param "archive" $ hPost0 $ toK0 $ DocControl.handleTemplateArchive
     , dir "t" $ param "share" $ hPost0 $ toK0 $ DocControl.handleTemplateShare
     , dir "t" $ param "template" $ hPost0 $ toK0 $ DocControl.handleCreateFromTemplate
     , dir "t" $ hPost0 $ toK0 $ DocControl.handleCreateNewTemplate

     , dir "o" $ hGet0 $ toK0 $ DocControl.showOfferList
     , dir "o" $ param "archive" $ hPost0 $ toK0 $ DocControl.handleOffersArchive
     , dir "o" $ param "remind" $ hPost0 $ toK0 $ DocControl.handleBulkOfferRemind

     , dir "or" $ hGet0  $ toK0 $ DocControl.showOrdersList
     , dir "or" $ param "archive" $ hPost0 $ toK0 $ DocControl.handleOrdersArchive
     , dir "or" $ param "remind" $ hPost0 $ toK0 $ DocControl.handleBulkOrderRemind
     
     , dir "r" $ hGet0 $ toK0 $ DocControl.showRubbishBinList
     , dir "r" $ param "restore" $ hPost0 $ toK0 $ DocControl.handleRubbishRestore
     , dir "r" $ param "reallydelete" $ hPost0 $ toK0 $ DocControl.handleRubbishReallyDelete

     , dir "d"                     $ hGet2  $ toK2 $ DocControl.handleAttachmentDownloadForAuthor
     , dir "d"                     $ hGet0  $ toK0 $ DocControl.showContractsList
     , dir "d"                     $ hGet1  $ toK1 $ DocControl.handleIssueShowGet
     , dir "d"                     $ hGet2  $ toK2 $ DocControl.handleIssueShowTitleGet
     , dir "d"                     $ hGet4  $ toK4 $ DocControl.handleIssueShowTitleGetForSignatory
     , dir "d" $ {- param "doc" $ -} hPost0 $ toK0 $ DocControl.handleIssueNewDocument
     , dir "d" $ param "archive"   $ hPost0 $ toK0 $ DocControl.handleContractArchive
     , dir "d" $ param "remind"    $ hPost0 $ toK0 $ DocControl.handleBulkContractRemind
     , dir "d"                     $ hPost1 $ toK1 $ DocControl.handleIssueShowPost
     , dir "docs"                  $ hGet0  $ toK0 $ DocControl.jsonDocumentsList
     , dir "doc"                   $ hGet1  $ toK1 $ DocControl.jsonDocument
     , dir "mailpreview"           $ hGet2  $ toK2 $ DocControl.prepareEmailPreview 

     , dir "friends"               $ hGet0  $ toK0 $ UserControl.handleFriends

     , dir "df"                    $ hGet2  $ toK2 $ DocControl.handleFileGet
     , dir "dv"                    $ hGet1  $ toK1 $ DocControl.handleAttachmentViewForAuthor

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hPost2 $ toK2 $ DocControl.handleResend
     , dir "changeemail" $ hPost2 $ toK2 $ DocControl.handleChangeSignatoryEmail
     -- , dir "withdrawn" $ hPost0 $ DocControl.handleWithdrawn
     , dir "restart" $ hPost1 $ toK1 $ DocControl.handleRestart
     , dir "cancel"  $ hPost1 $ toK1 $ DocControl.handleCancel

     , dir "pages"  $ hGetAjax3 $ toK3 $ DocControl.showPage
     , dir "pages"  $ hGetAjax5 $ toK5 $ DocControl.showPageForSignatory
     , dir "template"  $ hPost0 $ toK0 $ DocControl.handleCreateFromTemplate

     , dir "filepages" $ hGetAjax2 $  toK2 $ DocControl.handleFilePages
     , dir "pagesofdoc" $ hGetAjax1 $ toK1 $ DocControl.handlePageOfDocument
     , dir "pagesofdoc" $ hGetAjax3 $ toK3 $ DocControl.handlePageOfDocumentForSignatory

     -- UserControl
     , dir "account"                    $ hGet0  $ toK0 $ UserControl.handleUserGet
     , dir "account"                    $ hPost0 $ toK0 $ UserControl.handleUserPost
     , dir "account" $ dir "companyaccounts" $ hGet0  $ toK0 $ UserControl.handleGetCompanyAccounts
     , dir "account" $ dir "companyaccounts" $ hPost0 $ toK0 $ UserControl.handlePostCompanyAccounts
     , dir "account" $ dir "sharing" $ hGet0 $ toK0 $ UserControl.handleGetSharing
     , dir "account" $ dir "sharing" $ hPost0 $ toK0 $ UserControl.handlePostSharing
     , dir "account" $ dir "security" $ hGet0 $ toK0 $ UserControl.handleGetUserSecurity
     , dir "account" $ dir "security" $ hPost0 $ toK0 $ UserControl.handlePostUserSecurity
     , dir "account" $ dir "mailapi" $ hGet0 $ toK0 $ UserControl.handleGetUserMailAPI
     , dir "account" $ dir "mailapi" $ hPost0 $ toK0 $ UserControl.handlePostUserMailAPI
     , dir "account" $ dir "bsa" $ hGet1 $ toK1 $ UserControl.handleGetBecomeCompanyAccount
     , dir "account" $ dir "bsa" $ hPost1 $ toK1 $ UserControl.handlePostBecomeCompanyAccount
     , dir "contacts"  $ hGet0  $ toK0 $ Contacts.showContacts
     , dir "contacts"  $ hPost0 $ toK0 $ Contacts.handleContactsChange
     , dir "accepttos" $ hGet0  $ toK0 $ UserControl.handleAcceptTOSGet
     , dir "accepttos" $ hPost0 $ toK0 $ UserControl.handleAcceptTOSPost

     -- super user only
     , dir "stats"      $ hGet0  $ toK0 $ Administration.showStats
     , dir "createuser" $ hPost0 $ toK0 $ Administration.handleCreateUser
     , dir "sendgrid" $ dir "events" $ hPostNoXToken0 $ toK0 $ handleSendgridEvent
     , dir "adminonly" $ hGet0 $ toK0 $ Administration.showAdminMainPage
     , dir "adminonly" $ dir "advuseradmin" $ hGet0 $ toK0 $ Administration.showAdminUserAdvanced
     , dir "adminonly" $ dir "useradminforsales" $ hGet0 $ toK0 $ Administration.showAdminUsersForSales
     , dir "adminonly" $ dir "useradminforpayments" $ hGet0 $ toK0 $ Administration.showAdminUsersForPayments
     , dir "adminonly" $ dir "useradmin" $ hGet1 $ toK1 $ Administration.showAdminUsers . Just
     , dir "adminonly" $ dir "useradmin" $ hGet0 $ toK0 $ Administration.showAdminUsers Nothing
     , dir "adminonly" $ dir "useradmin" $ dir "usagestats" $ hGet1 $ toK1 $ Administration.showAdminUserUsageStats
     , dir "adminonly" $ dir "useradmin" $ hPost1 $ toK1 $ Administration.handleUserChange
     , dir "adminonly" $ dir "db" $ hGet0 $ toK0 $ Administration.indexDB
     , dir "adminonly" $ dir "db" $ onlySuperUser $ serveDirectory DisableBrowsing [] "_local/kontrakcja_state"

     , dir "adminonly" $ dir "cleanup"           $ hPost0 $ toK0 $ Administration.handleDatabaseCleanup
     , dir "adminonly" $ dir "statistics"        $ hGet0  $ toK0 $ Administration.handleStatistics
     , dir "adminonly" $ dir "skrivapausers.csv" $ hGet0  $ toK0 $ Administration.getUsersDetailsToCSV
     , dir "adminonly" $ dir "payments"          $ hGet0  $ toK0 $ Payments.handlePaymentsModelForViewView
     , dir "adminonly" $ dir "advpayments"       $ hGet0  $ toK0 $ Payments.handlePaymentsModelForEditView
     , dir "adminonly" $ dir "advpayments"       $ hPost0 $ toK0 $ Payments.handleAccountModelsChange

     , dir "adminonly" $ dir "services" $ hGet0 $ toK0 $ Administration.showServicesPage
     , dir "adminonly" $ dir "services" $ param "create" $ hPost0 $ toK0 $ Administration.handleCreateService
     , dir "adminonly" $ dir "translations" $ hGet0 $ toK0 $ Administration.showAdminTranslations

     -- a temporary service to help migration
     --, dir "adminonly" $ dir "migratesigaccounts" $ hGet0 $ toK0 $ Administration.migrateSigAccounts
     --, dir "adminonly" $ dir "migratecompanies" $ hGet0 $ toK0 $ Administration.migrateCompanies

     , dir "adminonly" $ dir "sysdump" $ hGet0 $ toK0 $ sysdump

     , dir "adminonly" $ dir "reseal" $ hPost1 $ toK1 $ Administration.resealFile
       
     , dir "adminonly" $ dir "docproblems" $ hGet0 $ toK0 $ DocControl.handleInvariantViolations

     -- this stuff is for a fix
     , dir "adminonly" $ dir "510bugfix" $ hGet0 $ toK0 $ Administration.handleFixForBug510

     , dir "services" $ hGet0 $ toK0 $ handleShowServiceList
     , dir "services" $ hGet1 $ toK1 $ handleShowService
     , dir "services" $ dir "ui" $ hPost1 $ toK1 $ handleChangeServiceUI
     , dir "services" $ dir "password" $ hPost1 $ toK1 $ handleChangeServicePassword
     , dir "services" $ dir "settings" $ hPost1 $ toK1 $ handleChangeServiceSettings
     , dir "services" $ dir "logo" $ hGet1 $ toK1 $ handleServiceLogo
     , dir "services" $ dir "buttons_body" $ hGet1 $ toK1 $ handleServiceButtonsBody
     , dir "services" $ dir "buttons_rest" $ hGet1 $ toK1 $ handleServiceButtonsRest
     , dir "dave" $ dir "document" $ hGet1 $ toK1 $ daveDocument
     , dir "dave" $ dir "user"     $ hGet1 $ toK1 $ daveUser

     -- account stuff
     , dir "logout"      $ hGet0  $ toK0 $ handleLogout
     , dir "login"       $ hGet0  $ toK0 $ handleLoginGet
     , dir "login"       $ hPostNoXToken0 $ toK0 $ handleLoginPost
     --, dir "signup"      $ hGet0  $ signupPageGet
     , dir "signup"      $ hPostAllowHttp0 $ toK0 $ signupPagePost
     --, dir "vip"         $ hGet0  $ signupVipPageGet
     --, dir "vip"         $ hPostNoXToken $ signupVipPagePost
     , dir "amnesia"     $ hPostNoXToken0 $ toK0 $ forgotPasswordPagePost
     , dir "amnesia"     $ hGet2 $ toK2 $ UserControl.handlePasswordReminderGet
     , dir "amnesia"     $ hPostNoXToken2 $ toK2 UserControl.handlePasswordReminderPost
     , dir "accountsetup"  $ hGet2 $ toK2 $ UserControl.handleAccountSetupGet
     , dir "accountsetup"  $ hPostNoXToken2 $ toK2 $ UserControl.handleAccountSetupPost
     , dir "accountremoval" $ hGet2 $ toK2 $ UserControl.handleAccountRemovalGet
     , dir "accountremoval" $ hPostNoXToken2 $ toK2 $ UserControl.handleAccountRemovalPost

     -- viral invite
     , dir "invite"      $ hPostNoXToken0 $ toK0 $ UserControl.handleViralInvite
     , dir "question"    $ hPostAllowHttp0 $ toK0 $ UserControl.handleQuestion
     -- e-legitimation stuff
     , dir "s" $ hGet4  $ toK4 $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPost3 $ toK3 $ BankID.handleSignPostBankID
     , dir "d" $ hGet2  $ toK2 $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost1 $ toK1 $ BankID.handleIssuePostBankID
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
  ctx@Context{ ctxmaybeuser,ctxservice } <- getContext
  loginOn <- isFieldSet "logging"
  referer <- getField "referer"
  email   <- getField "email"
  case (ctxmaybeuser, ctxservice) of
    (Just _, _) -> Right <$> (UserControl.checkUserTOSGet DocControl.mainPage)
    (Nothing, Nothing) -> do
      response <- V.simpleResponse =<< firstPage ctx loginOn referer email
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
    ctx <- getContext
    case (ctxservice ctx) of
         Nothing -> do
            addFlashM V.modalError
            sendRedirect LinkMain
         Just _ -> embeddedErrorPage

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


maybeReadTemplates :: MVar (ClockTime, KontrakcjaGlobalTemplates)
                      -> IO KontrakcjaGlobalTemplates
maybeReadTemplates mvar = modifyMVar mvar $ \(modtime, templates) -> do
        modtime' <- getTemplatesModTime
        if modtime /= modtime'
            then do
                Log.debug $ "Reloading templates"
                templates' <- readGlobalTemplates
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
  --liftIO $ do
  --    bi <- readInputsBody rq
  --    putStrLn $ show rq
  --    putStrLn $ "INPUTS BODY: " ++ show bi
  session <- handleSession
  ctx <- createContext rq session
  handle rq session ctx
  where
    handle :: Request -> Session -> Context -> ServerPartT IO Response
    handle rq session ctx = do
      (res,ctx') <- toIO ctx . runKontra $
         do
          res <- (handleRoutes) `mplus` do
             rqcontent <- liftIO $ tryTakeMVar (rqInputsBody rq)
             when (isJust rqcontent) $
                 liftIO $ putMVar (rqInputsBody rq) (fromJust rqcontent)
             Log.error $ showRequest rq rqcontent
             response <- handleError
             setRsCode 404 response
          ctx' <- getContext
          return (res,ctx')

      let newsessionuser = fmap userid $ ctxmaybeuser ctx'
      let newflashmessages = ctxflashmessages ctx'
      let newelegtrans = ctxelegtransactions ctx'
      F.updateFlashCookie (aesConfig appConf) (ctxflashmessages ctx) newflashmessages
      updateSessionWithContextData session newsessionuser newelegtrans
      liftIO $ disconnect $ ctxdbconn ctx'
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

      conn <- liftIO $ connectPostgreSQL $ dbConfig appConf
      minutestime <- liftIO getMinutesTime
      muser <- getUserFromSession conn session
      mcompany <- getCompanyFromSession conn session
      location <- getLocationFromSession session
      mservice <- ioRunDB conn . dbQuery . GetServiceByLocation . toServiceLocation =<< currentLink
      flashmessages <- withDataFn F.flashDataFromCookie $ maybe (return []) $ \fval ->
          case F.fromCookieValue (aesConfig appConf) fval of
               Just flashes -> return flashes
               Nothing -> do
                   Log.error $ "Couldn't read flash messages from value: " ++ fval
                   F.removeFlashCookie
                   return []

      -- do reload templates in non-production code
      templates2 <- liftIO $ maybeReadTemplates (templates appGlobals)
      let language = (fromMaybe browserLang $ lang <$> usersettings <$> muser )
      let systemServer = systemServerFromURL hostpart    
      let elegtrans = getELegTransactions session
          ctx = Context
                { ctxmaybeuser = muser
                , ctxhostpart = hostpart
                , ctxflashmessages = flashmessages
                , ctxtime = minutestime
                , ctxnormalizeddocuments = docscache appGlobals
                , ctxipnumber = peerip
                , ctxdbconn = conn
                , ctxdocstore = docstore appConf
                , ctxs3action = defaultAWSAction appConf
                , ctxgscmd = gsCmd appConf
                , ctxproduction = production appConf
                , ctxtemplates = localizedVersion (systemServer,language) templates2
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
                , ctxadminaccounts = admins appConf
                }
      return ctx

{- |
   Handles submission of the password reset form
-}
forgotPasswordPagePost :: Kontrakcja m => m KontraLink
forgotPasswordPagePost = do
  ctx <- getContext
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- runDBQuery $ GetUserByEmail Nothing $ Email email
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
                0 -> addFlashM flashMessageNoRemainedPasswordReminderEmails
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
          addFlashM flashMessageChangePasswordEmailSend
          return LinkMain

sendResetPasswordMail :: Kontrakcja m => Context -> KontraLink -> User -> m ()
sendResetPasswordMail ctx link user = do
  mail <- UserView.resetPasswordMail (ctxhostpart ctx) user link
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress user] }

{- |
   Handles viewing of the signup page
-}
_signupPageGet :: Kontra Response
_signupPageGet = do
    ctx <- getContext
    content <- liftIO (signupPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja  content


_signupVipPageGet :: Kontra Response
_signupVipPageGet = do
    ctx <- getContext
    content <- liftIO (signupVipPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja content
{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}
signupPagePost :: Kontrakcja m => m KontraLink
signupPagePost = do
    Context { ctxtime } <- getContext
    signup False $ Just ((60 * 24 * 31) `minutesAfter` ctxtime)

{-
    A comment next to LoopBack says never to use it. Is this function broken?
-}
signup :: Kontrakcja m => Bool -> Maybe MinutesTime -> m KontraLink
signup vip _freetill =  do
  ctx@Context{ctxhostpart} <- getContext
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- runDBQuery $ GetUserByEmail Nothing $ Email $ email
      case  muser of
        Just user ->
          if isNothing $ userhasacceptedtermsofservice user
          then do
            al <- newAccountCreatedLink user
            mail <- newUserMail ctxhostpart email email al vip
            scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [MailAddress {fullname = email, email = email}] }
            addFlashM flashMessageNewActivationLinkSend
            return LoopBack
          else do
            addFlashM flashMessageUserWithSameEmailExists
            return LoopBack
        Nothing -> do         
          rq <- askRq
          let browserLang = langFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
          maccount <- UserControl.createUser ctx ctxhostpart (BS.empty, BS.empty) email Nothing Nothing vip browserLang
          case maccount of
            Just _account ->  do
              addFlashM flashMessageUserSignupDone
              return LoopBack
            Nothing -> do
              addFlashM flashMessageUserWithSameEmailExists
              return LoopBack

{- |
   Sends a new activation link mail, which is really just a new user mail.
-}
_sendNewActivationLinkMail:: Context -> User -> Kontra ()
_sendNewActivationLinkMail Context{ctxhostpart, ctxesenforcer} user = do
    let email = getEmail user
    al <- newAccountCreatedLink user
    mail <- newUserMail ctxhostpart email email al False
    scheduleEmailSendout ctxesenforcer $ mail { to = [MailAddress {fullname = email, email = email}] }

{- |
   Handles viewing of the login page
-}
handleLoginGet :: Kontrakcja m => m Response
handleLoginGet = do
  ctx <- getContext
  case ctxmaybeuser ctx of
       Just _  -> sendRedirect LinkMain
       Nothing -> do
         referer <- getField "referer"
         email   <- getField "email"
         content <- V.pageLogin referer email
         V.renderFromBody V.TopNone V.kontrakcja content

{- |
   Handles submission of a login form.  On failure will redirect back to referer, if there is one.
-}
handleLoginPost :: Kontrakcja m => m KontraLink
handleLoginPost = do
    memail  <- getOptionalField asDirtyEmail    "email"
    mpasswd <- getOptionalField asDirtyPassword "password"
    let linkemail = maybe "" BS.toString memail
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            Log.debug $ "Logging " ++ show email
            maybeuser <- runDBQuery $ GetUserByEmail Nothing (Email email)
            case maybeuser of
                Just User{userpassword}
                    | verifyPassword userpassword passwd -> do
                        Log.debug $ "Logging: User logged in"
                        logUserToContext maybeuser
                        return BackToReferer
                Just _ -> do
                        Log.debug $ "Logging: User found, Not verified password"
                        return $ LinkLogin $ InvalidLoginInfo linkemail
                Nothing -> do
                    Log.debug $ "Logging: No user matching the email found"  
                    return $ LinkLogin $ InvalidLoginInfo linkemail
        _ -> return $ LinkLogin $ InvalidLoginInfo linkemail

{- |
   Handles the logout, and sends user back to main page.
-}
handleLogout :: Kontrakcja m => m Response
handleLogout = do
    logUserToContext Nothing
    sendRedirect LinkMain

{- |
   Serves out the static html files.
-}
serveHTMLFiles :: Kontra Response
serveHTMLFiles =  do
  rq <- askRq
  let fileName = last (rqPaths rq)
  guard ((length (rqPaths rq) > 0) && (isSuffixOf ".html" fileName))
  s <- guardJustM $ (liftIO $ catch (fmap Just $ BS.readFile ("html/" ++ fileName))
                                      (const $ return Nothing))
  renderFromBody V.TopNone V.kontrakcja $ BS.toString s

{- |
   Ensures logged in as a super user
-}
onlySuperUserGet :: Kontrakcja m => m Response -> m Response
onlySuperUserGet action = do
    Context{ ctxadminaccounts, ctxmaybeuser } <- getContext
    if isSuperUser ctxadminaccounts ctxmaybeuser
        then action
        else sendRedirect $ LinkLogin NotLoggedAsSuperUser

{- |
   Used by super users to inspect a particular document.
-}
daveDocument :: Kontrakcja m => DocumentID -> m Response
daveDocument documentid = onlySuperUserGet $ do
    document <- queryOrFail $ GetDocumentByDocumentID documentid
    V.renderFromBody V.TopNone V.kontrakcja $ inspectXML document

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: Kontrakcja m => UserID -> m Response
daveUser userid = onlySuperUserGet $ do
    user <- runDBOrFail $ dbQuery $ GetUserByID userid
    V.renderFromBody V.TopNone V.kontrakcja $ inspectXML user

sysdump :: Kontrakcja m => m Response
sysdump = onlySuperUserGet $ do
    dump <- liftIO getAllActionAsString
    ok $ addHeader "refresh" "5" $ toResponse dump
