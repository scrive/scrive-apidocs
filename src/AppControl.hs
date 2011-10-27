{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module AppControl
    ( module AppConf
    , appHandler
    , AppGlobals(..)
    , defaultAWSAction
    , handleLoginPost
    , getDocumentLocale
    , getUserLocale
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
import Mails.MailsConfig
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
import qualified Stats.Control as Stats
import qualified Administration.AdministrationControl as Administration
import qualified AppLogger as Log (error, security, debug)
import qualified Contacts.ContactsControl as Contacts
import qualified Doc.DocControl as DocControl
import qualified Archive.Control as ArchiveControl
import qualified ELegitimation.Routes as Elegitimation
import qualified FlashMessage as F
import qualified MemCache
import qualified Payments.PaymentsControl as Payments
import qualified TrustWeaver as TW
import qualified User.UserControl as UserControl
import Util.FlashUtil
import Util.HasSomeUserInfo
import Util.KontraLinkUtils

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
import Util.MonadUtils


{- |
  Global application data
-}
data AppGlobals
    = AppGlobals { templates       :: MVar (ClockTime, KontrakcjaGlobalTemplates)
                 , filecache       :: MemCache.MemCache FileID BS.ByteString
                 , mailer          :: Mailer
                 , appbackdooropen    :: Bool --whether a backdoor used to get email content is open or not
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
handleRoutes :: Locale -> Kontra Response
handleRoutes locale = msum $ 
     [ regionDir locale $ langDir locale $ hGetAllowHttp0 $ handleHomepage
     , hGetAllowHttp0 $ redirectKontraResponse $ LinkHome locale
     
     , publicDir locale "priser" "pricing" LinkPriceplan handlePriceplanPage
     , publicDir locale "sakerhet" "security" LinkSecurity handleSecurityPage
     , publicDir locale "juridik" "legal" LinkLegal handleLegalPage
     , publicDir locale "sekretesspolicy" "privacy-policy" LinkPrivacyPolicy handlePrivacyPolicyPage
     , publicDir locale "allmana-villkor" "terms" LinkTerms handleTermsPage
     , publicDir locale "om-scrive" "about" LinkAbout handleAboutPage
     , publicDir locale "partners" "partners" LinkPartners handlePartnersPage
     , publicDir locale "kunder" "clients" LinkClients handleClientsPage

     -- sitemap
     , dir "webbkarta"       $ hGetAllowHttp0 $ handleSitemapPage
     , dir "sitemap"         $ hGetAllowHttp0 $ handleSitemapPage

     -- this is SMTP to HTTP gateway
     , mailAPI
     ] 
  ++ Elegitimation.handleRoutes
  ++ [ dir "s" $ hGet0 $ toK0 $ sendRedirect $ LinkContracts
     , dir "s" $ hGet3 $ toK3 $ DocControl.handleSignShow
     , dir "s" $ hGet4 $ toK4 $ DocControl.handleAttachmentDownloadForViewer --  This will be droped 
     
     
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
     , dir "upload" $ hGet0 $ toK0 $ DocControl.handleShowUploadPage
     , dir "locale" $ hPost0 $ toK0 $ UserControl.handlePostUserLocale
     , dir "a"                     $ hGet0  $ toK0 $ ArchiveControl.showAttachmentList
     , dir "a" $ param "archive"   $ hPost0 $ toK0 $ ArchiveControl.handleAttachmentArchive
     , dir "a" $ param "share"     $ hPost0 $ toK0 $ DocControl.handleAttachmentShare
     , dir "a" $ dir "rename"      $ hPost1 $ toK1 $ DocControl.handleAttachmentRename
     , dir "a"                     $ hPost0 $ toK0 $ DocControl.handleCreateNewAttachment

     , dir "t" $ hGet0  $ toK0 $ ArchiveControl.showTemplatesList
     , dir "t" $ param "archive" $ hPost0 $ toK0 $ ArchiveControl.handleTemplateArchive
     , dir "t" $ param "share" $ hPost0 $ toK0 $ DocControl.handleTemplateShare
     , dir "t" $ param "template" $ hPost0 $ toK0 $ DocControl.handleCreateFromTemplate
     , dir "t" $ hPost0 $ toK0 $ DocControl.handleCreateNewTemplate

     , dir "o" $ hGet0 $ toK0 $ ArchiveControl.showOfferList
     , dir "o" $ param "archive" $ hPost0 $ toK0 $ ArchiveControl.handleOffersArchive
     , dir "o" $ param "remind" $ hPost0 $ toK0 $ DocControl.handleBulkOfferRemind

     , dir "or" $ hGet0  $ toK0 $ ArchiveControl.showOrdersList
     , dir "or" $ param "archive" $ hPost0 $ toK0 $ ArchiveControl.handleOrdersArchive
     , dir "or" $ param "remind" $ hPost0 $ toK0 $ DocControl.handleBulkOrderRemind
     
     , dir "r" $ hGet0 $ toK0 $ ArchiveControl.showRubbishBinList
     , dir "r" $ param "restore" $ hPost0 $ toK0 $ DocControl.handleRubbishRestore
     , dir "r" $ param "reallydelete" $ hPost0 $ toK0 $ DocControl.handleRubbishReallyDelete

     , dir "d"                     $ hGet2  $ toK2 $ DocControl.handleAttachmentDownloadForAuthor -- This will be droped and unified to one below
     
     , dir "d"                     $ hGet3  $ toK3 $ DocControl.handleDownloadFileLogged -- This + magic hash version will be the only file download possible
     , dir "d"                     $ hGet5 $ toK5 $ DocControl.handleDownloadFileNotLogged 
     
     , dir "d"                     $ hGet0  $ toK0 $ ArchiveControl.showContractsList
     , dir "d"                     $ hGet1  $ toK1 $ DocControl.handleIssueShowGet
     , dir "d"                     $ hGet2  $ toK2 $ DocControl.handleIssueShowTitleGet
     , dir "d"                     $ hGet4  $ toK4 $ DocControl.handleIssueShowTitleGetForSignatory
     , dir "d" $ {- param "doc" $ -} hPost0 $ toK0 $ DocControl.handleIssueNewDocument
     , dir "d" $ param "archive"   $ hPost0 $ toK0 $ ArchiveControl.handleContractArchive
     , dir "d" $ param "remind"    $ hPost0 $ toK0 $ DocControl.handleBulkContractRemind
     , dir "d"                     $ hPost1 $ toK1 $ DocControl.handleIssueShowPost
     , dir "docs"                  $ hGet0  $ toK0 $ DocControl.jsonDocumentsList
     , dir "doc"                   $ hGet1  $ toK1 $ DocControl.jsonDocument
     , dir "mailpreview"           $ hGet2  $ toK2 $ DocControl.prepareEmailPreview 

     , dir "friends"               $ hGet0  $ toK0 $ UserControl.handleFriends
     , dir "companyaccounts"       $ hGet0  $ toK0 $ UserControl.handleCompanyAccounts

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
     -- HTMP emails can have embedded preview image
     , dir "preview" $ hGet2 $ toK2 $ DocControl.showPreview
     , dir "preview" $ hGet4 $ toK4 $ DocControl.showPreviewForSignatory

     , dir "template"  $ hPost0 $ toK0 $ DocControl.handleCreateFromTemplate

     , dir "filepages" $ hGetAjax2 $  toK2 $ DocControl.handleFilePages
     , dir "pagesofdoc" $ hGetAjax1 $ toK1 $ DocControl.handlePageOfDocument
     , dir "pagesofdoc" $ hGetAjax3 $ toK3 $ DocControl.handlePageOfDocumentForSignatory
       
     , dir "csvlandpage" $ hGet1 $ toK1 $ DocControl.handleCSVLandpage

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
     , dir "sendgrid" $ dir "events" $ handleSendgridEvent
     , dir "adminonly" $ hGet0 $ toK0 $ Administration.showAdminMainPage
     , dir "adminonly" $ dir "advuseradmin" $ hGet0 $ toK0 $ Administration.showAdminUserAdvanced
     , dir "adminonly" $ dir "useradminforsales" $ hGet0 $ toK0 $ Administration.showAdminUsersForSales
     , dir "adminonly" $ dir "useradminforpayments" $ hGet0 $ toK0 $ Administration.showAdminUsersForPayments
     , dir "adminonly" $ dir "useradmin" $ hGet1 $ toK1 $ Administration.showAdminUsers . Just
     , dir "adminonly" $ dir "useradmin" $ hGet0 $ toK0 $ Administration.showAdminUsers Nothing
     , dir "adminonly" $ dir "useradmin" $ dir "usagestats" $ hGet1 $ toK1 $ Stats.showAdminUserUsageStats
     , dir "adminonly" $ dir "useradmin" $ hPost1 $ toK1 $ Administration.handleUserChange
     , dir "adminonly" $ dir "companyadmin" $ hGet1 $ toK1 $ Administration.showAdminCompanies . Just
     , dir "adminonly" $ dir "companyadmin" $ hGet0 $ toK0 $ Administration.showAdminCompanies Nothing
     , dir "adminonly" $ dir "companyadmin" $ dir "users" $ hGet1 $ toK1 $ Administration.showAdminCompanyUsers
     , dir "adminonly" $ dir "companyadmin" $ dir "users" $ hPost1 $ toK1 $ Administration.handleCreateCompanyUser
     , dir "adminonly" $ dir "companyadmin" $ dir "usagestats" $ hGet1 $ toK1 $ Stats.showAdminCompanyUsageStats
     , dir "adminonly" $ dir "companyadmin" $ hPost1 $ toK1 $ Administration.handleCompanyChange
     , dir "adminonly" $ dir "functionalitystats" $ hGet0 $ toK0 $ Administration.showFunctionalityStats
     , dir "adminonly" $ dir "db" $ hGet0 $ toK0 $ Administration.indexDB
     , dir "adminonly" $ dir "db" $ onlySuperUser $ serveDirectory DisableBrowsing [] "_local/kontrakcja_state"
     
     , dir "adminonly" $ dir "documents" $ hGet0 $ toK0 $ Administration.showDocumentsDaylyList
     
     , dir "adminonly" $ dir "allstatscsv" $ Stats.handleDocStatsCSV
     , dir "adminonly" $ dir "userstatscsv" $ Stats.handleUserStatsCSV
       
     , dir "adminonly" $ dir "runstatsonalldocs" $ hGet0 $ toK0 $ Stats.addAllDocsToStats
     , dir "adminonly" $ dir "stats1to2" $ hGet0 $ toK0 $ Stats.handleMigrate1To2

     , dir "adminonly" $ dir "runstatsonallusers" $ hGet0 $ toK0 $ Stats.addAllUsersToStats

     , dir "adminonly" $ dir "cleanup"           $ hPost0 $ toK0 $ Administration.handleDatabaseCleanup
     , dir "adminonly" $ dir "statistics"        $ hGet0  $ toK0 $ Stats.showAdminSystemUsageStats
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

     , dir "adminonly" $ dir "sysdump" $ hGet0 $ toK0 $ Administration.sysdump

     , dir "adminonly" $ dir "reseal" $ hPost1 $ toK1 $ Administration.resealFile
     , dir "adminonly" $ dir "replacemainfile" $ hPost1 $ toK1 $ Administration.replaceMainFile
          
     , dir "adminonly" $ dir "docproblems" $ hGet0 $ toK0 $ DocControl.handleInvariantViolations

     , dir "adminonly" $ dir "backdoor" $ hGet1 $ toK1 $ Administration.handleBackdoorQuery

     -- this stuff is for a fix
     , dir "adminonly" $ dir "510bugfix" $ hGet0 $ toK0 $ Administration.handleFixForBug510
     , dir "adminonly" $ dir "adminonlybugfix" $ hGet0 $ toK0 $ Administration.handleFixForAdminOnlyBug

     , dir "adminonly" $ dir "siglinkids_test_uniqueness" $ hGet0 $ toK0 $ Administration.handleCheckSigLinkIDUniqueness

     , dir "services" $ hGet0 $ toK0 $ handleShowServiceList
     , dir "services" $ hGet1 $ toK1 $ handleShowService
     , dir "services" $ dir "ui" $ hPost1 $ toK1 $ handleChangeServiceUI
     , dir "services" $ dir "password" $ hPost1 $ toK1 $ handleChangeServicePassword
     , dir "services" $ dir "settings" $ hPost1 $ toK1 $ handleChangeServiceSettings
     , dir "services" $ dir "logo" $ hGet1 $ toK1 $ handleServiceLogo
     , dir "services" $ dir "buttons_body" $ hGet1 $ toK1 $ handleServiceButtonsBody
     , dir "services" $ dir "buttons_rest" $ hGet1 $ toK1 $ handleServiceButtonsRest
       
     -- never ever use this
     , dir "adminonly" $ dir "neveruser" $ dir "resetservicepassword" $ onlySuperUser $ hGet2 $ toK2 $ handleChangeServicePasswordAdminOnly      
       
     , dir "adminonly" $ dir "log" $ onlySuperUser $ hGet1 $ toK1 $ Administration.serveLogDirectory

  
     , dir "dave" $ dir "document" $ hGet1 $ toK1 $ Administration.daveDocument
     , dir "dave" $ dir "user"     $ hGet1 $ toK1 $ Administration.daveUser
     , dir "dave" $ dir "company"  $ hGet1 $ toK1 $ Administration.daveCompany

     -- account stuff
     , dir "logout"      $ hGet0  $ toK0 $ handleLogout
     , regionDir locale $ langDir locale $ dir "login" $ hGet0  $ toK0 $ handleLoginGet
     , regionDir locale $ langDir locale $ dir "login" $ hPostNoXToken0 $ toK0 $ handleLoginPost
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
       
     -- a general purpose blank page
     --, dir "/blank" $ hGet0 $ toK0 $ simpleResponse ""
       
     , userAPI
     , integrationAPI
     -- static files
     , allowHttp $ serveHTMLFiles
     , allowHttp $ serveDirectory DisableBrowsing [] "public"
     ]

{- |
    This is a helper function for routing a public dir.
-}
publicDir :: Locale -> String -> String -> (Locale -> KontraLink) -> Kontra Response -> Kontra Response
publicDir locale swedish english link handler = msum [
    -- the correct url with region/lang/publicdir where the publicdir must be in the correct lang
    regionDir locale $ langDir locale $ dirByLang locale swedish english $ hGetAllowHttp0 $ handler
    
    -- if they use the swedish name without region/lang we should redirect to the correct swedish locale
  , dir swedish $ hGetAllowHttp0 $ redirectKontraResponse $ link (mkLocaleFromRegion REGION_SE)
  
    -- if they use the english name without region/lang we should redirect to the correct british locale
  , dir english $ hGetAllowHttp0 $ redirectKontraResponse $ link (mkLocaleFromRegion REGION_GB)
  ]

{- |
    If the current request is referring to a document then this will
    return the locale of that document.
-}
getDocumentLocale :: (ServerMonad m, MonadIO m) => m (Maybe Locale)
getDocumentLocale = do
  rq <- askRq
  let docids = catMaybes . map (fmap fst . listToMaybe . reads) $ rqPaths rq
  mdoclocales <- mapM (DocControl.getDocumentLocale . DocumentID) docids
  return . listToMaybe $ catMaybes mdoclocales

{- |
    Determines the locale of the current user (whether they are logged in or not), by checking
    their settings, the request, and cookies.
-}
getUserLocale :: (MonadPlus m, MonadIO m, ServerMonad m, FilterMonad Response m, Functor m, HasRqData m) =>
                   Connection -> Maybe User -> m Locale
getUserLocale conn muser = do
  rq <- askRq
  currentcookielocale <- optional (readCookieValue "locale")
  activationlocale <- getActivationLocale rq
  let userlocale = locale <$> usersettings <$> muser
      urlregion = (listToMaybe $ rqPaths rq) >>= regionFromCode
      urllang = (listToMaybe . drop 1 $ rqPaths rq) >>= langFromCode
      urllocale = case (urlregion, urllang) of
                    (Just region, Just lang) -> Just $ mkLocale region lang
                    _ -> Nothing
  doclocale <- getDocumentLocale
  let browserlocale = getBrowserLocale rq
  let newlocale = firstOf [ activationlocale
                          , userlocale
                          , urllocale
                          , currentcookielocale
                          , doclocale
                          , Just browserlocale
                          ]
  let newlocalecookie = mkCookie "locale" (show newlocale)
  addCookie (MaxAge (60*60*24*366)) newlocalecookie
  return newlocale
  where
    getBrowserLocale rq =
      mkLocaleFromRegion $ regionFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
    -- try and get the locale from the current activation user by checking the path for action ids, and giving them a go
    getActivationLocale rq = do
      let actionids = catMaybes . map (fmap fst . listToMaybe . reads) $ rqPaths rq
      mactionlocales <- mapM (getActivationLocaleFromAction . ActionID) actionids
      return . listToMaybe $ catMaybes mactionlocales
    getActivationLocaleFromAction aid = do
      maction <- query $ GetAction aid
      mactionuser <- case fmap actionType maction of
                       Just (AccountCreatedBySigning _ uid _ _) -> ioRunDB conn . dbQuery $ GetUserByID uid
                       Just (AccountCreated uid _) -> ioRunDB conn . dbQuery $ GetUserByID uid
                       _ -> return Nothing
      return $ fmap (locale . usersettings) mactionuser
    optional c = (liftM Just c) `mplus` (return Nothing)
    firstOf :: Bounded a => [Maybe a] -> a
    firstOf opts =
      case find isJust opts of
        Just val -> fromJust val
        Nothing -> defaultValue

regionDir :: (ServerMonad m, MonadPlus m) => Locale -> m a -> m a
regionDir = dir . codeFromRegion . getRegion

langDir :: (ServerMonad m, MonadPlus m) => Locale -> m a -> m a
langDir = dir . codeFromLang . getLang

dirByLang :: (ServerMonad m, MonadPlus m) => Locale -> String -> String -> m a -> m a
dirByLang locale swedishdir englishdir
  | getLang locale == LANG_SE = dir swedishdir
  | otherwise = dir englishdir

handleHomepage :: Kontra (Either Response (Either KontraLink String))
handleHomepage = do
  ctx@Context{ ctxmaybeuser,ctxservice } <- getContext
  loginOn <- isFieldSet "logging"
  referer <- getField "referer"
  email   <- getField "email"
  case (ctxmaybeuser, ctxservice) of
    (Just _user, _) -> do
      response <- V.simpleResponse =<< firstPage ctx loginOn referer email
      clearFlashMsgs
      return $ Left response
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
            linkmain <- getHomeOrUploadLink
            sendRedirect linkmain
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
  startTime <- liftIO getClockTime

  let quota :: GHC.Int.Int64 = 10000000

  temp <- liftIO $ getTemporaryDirectory
  decodeBody (defaultBodyPolicy temp quota quota quota)

  rq <- askRq

  session <- handleSession
  ctx <- createContext rq session
  response <- handle rq session ctx
  finishTime <- liftIO getClockTime
  let TOD ss sp = startTime
      TOD fs fp = finishTime
      _diff = (fs - ss) * 1000000000000 + fp - sp
  --Log.debug $ "Response time " ++ show (diff `div` 1000000000) ++ "ms"
  return response
  where
    handle :: Request -> Session -> Context -> ServerPartT IO Response
    handle rq session ctx = do
      (res,ctx') <- toIO ctx . runKontra $
         do
          res <- handleRoutes (getLocale ctx) `mplus` do
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
      when (ctxdbconnclose ctx') $
        liftIO $ disconnect $ ctxdbconn ctx'
      return res

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

      -- work out the system, region and language
      let systemServer = systemServerFromURL hostpart
      doclocale <- getDocumentLocale
      userlocale <- getUserLocale conn muser
      let ctxlocale = fromMaybe userlocale doclocale

      let elegtrans = getELegTransactions session
          ctx = Context
                { ctxmaybeuser = muser
                , ctxhostpart = hostpart
                , ctxflashmessages = flashmessages
                , ctxtime = minutestime
                , ctxnormalizeddocuments = docscache appGlobals
                , ctxipnumber = peerip
                , ctxdbconn = conn
                , ctxdbconnclose = True
                , ctxdocstore = docstore appConf
                , ctxs3action = defaultAWSAction appConf
                , ctxgscmd = gsCmd appConf
                , ctxproduction = production appConf
                , ctxbackdooropen = isBackdoorOpen $ mailsConfig appConf
                , ctxtemplates = localizedVersion (systemServer,getRegion ctxlocale, getLang ctxlocale) templates2
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
                , ctxdoclocale = doclocale
                , ctxuserlocale = userlocale
                , ctxdbconnstring = dbConfig appConf
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
          return LinkUpload

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
          maccount <- UserControl.createUser ctx (BS.empty, BS.empty) email Nothing Nothing vip
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
       Just _  -> sendRedirect LinkUpload
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
    ctx <- getContext
    memail  <- getOptionalField asDirtyEmail    "email"
    mpasswd <- getOptionalField asDirtyPassword "password"
    let linkemail = maybe "" BS.toString memail
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- runDBQuery $ GetUserByEmail Nothing (Email email)
            case maybeuser of
                Just user@User{userpassword}
                    | verifyPassword userpassword passwd -> do
                        Log.debug $ "User " ++ show email ++ " logged in"
                        _ <- runDBUpdate $ SetUserSettings (userid user) $ (usersettings user) {
                          locale = ctxuserlocale ctx
                        }
                        muuser <- runDBQuery $ GetUserByID (userid user)
                        logUserToContext muuser
                        return BackToReferer
                Just _ -> do
                        Log.debug $ "User " ++ show email ++ " login failed (invalid password)"
                        return $ LinkLogin (getLocale ctx) $ InvalidLoginInfo linkemail
                Nothing -> do
                    Log.debug $ "User " ++ show email ++ " login failed (user not found)"
                    return $ LinkLogin (getLocale ctx) $ InvalidLoginInfo linkemail
        _ -> return $ LinkLogin (getLocale ctx) $ InvalidLoginInfo linkemail

{- |
   Handles the logout, and sends user back to main page.
-}
handleLogout :: Kontrakcja m => m Response
handleLogout = do
    ctx <- getContext
    logUserToContext Nothing
    sendRedirect $ LinkHome (getLocale ctx)

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

