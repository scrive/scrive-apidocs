{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}

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
import API.WebshopAPI
import ActionSchedulerState
import AppView as V
import Crypto
import Doc.DocState
import InputValidation
import InspectXML
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
import Templates.Templates (readTemplates, KontrakcjaTemplates, getTemplatesModTime)
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
import HSP.XML (cdata)
import Happstack.Server hiding (simpleHTTP, host)
import Happstack.Server.Internal.Cookie
import Happstack.State (query)
import Happstack.State (update)
import ListUtil
import Network.Socket
import System.Directory
import System.Time
import Text.JSON
import Text.JSON.String
import API.Service.ServiceControl
import CoopAPI

import Text.JSON.Types
import Text.StringTemplate.Base (StringTemplate(..))
import Text.StringTemplate.Classes (StFirst(..))
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import qualified Codec.Text.IConv as IConv
import InspectXMLInstances ()

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
    = AppGlobals { templates       :: MVar (ClockTime, KontrakcjaTemplates)
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
       hGetAllowHttp $ handleHomepage
     , hPost $ handleMainReaload

     -- static pages
     , dir "webbkarta" $ hGetAllowHttp $ handleSitemapPage
     , dir "priser" $ hGetAllowHttp $ handlePriceplanPage
     , dir "sakerhet" $ hGetAllowHttp $ handleSecurityPage
     , dir "juridik" $ hGetAllowHttp $ handleLegalPage
     , dir "sekretesspolicy" $ hGetAllowHttp $ handlePrivacyPolicyPage
     , dir "allmana-villkor" $ hGetAllowHttp $ handleTermsPage
     , dir "om-skrivapa" $ hGetAllowHttp $ handleAboutPage
     , dir "partners" $ hGetAllowHttp $ handlePartnersPage
     , dir "kunder" $ hGetAllowHttp $ handleClientsPage
       
     -- this is SMTP to HTTP gateway  
     , dir "mailapi" $ handleMailAPI

     -- e-legitimation stuff
     -- I put this stuff up here because someone changed things out from under me
     -- I will rearrange this later
     , dir "s" $ hGet  $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPostNoXToken $ BankID.handleSignPostBankID
     , dir "d" $ hGet  $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost $ BankID.handleIssuePostBankID

     , dir "s" $ hGet  $ DocControl.handleSTable
     , dir "s" $ hGet  $ DocControl.handleSignShow
     , dir "s" $ param "sign" $ hPostNoXToken $ DocControl.signDocument
     , dir "s" $ param "cancel" $ hPostNoXToken $ DocControl.rejectDocument
     , dir "s" $ param "acceptaccount" $ hPostNoXToken $ DocControl.handleAcceptAccountFromSign
     , dir "s" $ param "declineaccount" $ hPostNoXToken $ DocControl.handleDeclineAccountFromSign
     
     --Q: This all needs to be done by author. Why we dont check it
     --here? MR

     --A: Because this table only contains routing logic. The logic of
     --what it does/access control is left to the handler. EN
     , dir "a" $ hGet $ DocControl.showAttachmentList
     , dir "a" $ param "archive" $ hPost $ DocControl.handleAttachmentArchive
     , dir "a" $ param "share" $ hPost $ DocControl.handleAttachmentShare
     , dir "a" $ dir "rename" $ hPost $ DocControl.handleAttachmentRename
     , dir "a" $ hPost $ DocControl.handleCreateNewAttachment

     , dir "t" $ hGet  $ DocControl.showTemplatesList
     , dir "t" $ param "archive" $ hPost $ DocControl.handleTemplateArchive
     , dir "t" $ param "share" $ hPost $ DocControl.handleTemplateShare
     , dir "t" $ param "template" $ hPost  $ DocControl.handleCreateFromTemplate
     , dir "t" $ hPost  $ DocControl.handleCreateNewTemplate
     
     , dir "o" $ hGet  $ DocControl.showOfferList
     , dir "o" $ param "archive" $ hPost  $ DocControl.handleOffersArchive
     , dir "o" $ param "remind" $ hPost $ DocControl.handleBulkOfferRemind
     , dir "o" $ hPost $ DocControl.handleOffersReload
     
     , dir "d" $ hGet  $ DocControl.showContractsList
     , dir "d" $ hGet  $ DocControl.handleIssueShowGet
     , dir "d" $ hGet  $ DocControl.handleIssueShowTitleGet
     , dir "d" $ hGet  $ DocControl.handleIssueShowTitleGetForSignatory
     , dir "d" $ {- param "doc" $ -} hPost $ DocControl.handleIssueNewDocument
     , dir "d" $ param "archive" $ hPost $ DocControl.handleContractArchive
     , dir "d" $ param "remind" $ hPost $ DocControl.handleBulkContractRemind
     , dir "d" $ hPost $ DocControl.handleContractsReload
     , dir "d" $ hPost $ DocControl.handleIssueShowPost
     , dir "df" $ hGet $ DocControl.handleFileGet

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hPost $ DocControl.handleResend
     , dir "changeemail" $ hPost $ DocControl.handleChangeSignatoryEmail
     -- , dir "withdrawn" $ hPost $ DocControl.handleWithdrawn
     , dir "restart" $ hPost $ DocControl.handleRestart
     , dir "cancel"  $ hPost $ DocControl.handleCancel
     
     , dir "pages"  $ hGetAjax $ DocControl.showPage
     , dir "pages"  $ hGetAjax $ DocControl.showPageForSignatory 
     , dir "templates"  $ hGetAjax $ DocControl.getTemplatesForAjax
     , dir "template"  $ hPost $ DocControl.handleCreateFromTemplate
           
     , dir "pagesofdoc" $ hGetAjax $ DocControl.handlePageOfDocument
     , dir "pagesofdoc" $ hGetAjax $ DocControl.handlePageOfDocumentForSignatory

     -- UserControl
     , dir "account"                    $ hGet  $ UserControl.handleUserGet
     , dir "account"                    $ hPost $ UserControl.handleUserPost
     , dir "account" $ dir "subaccount" $ hGet  $ UserControl.handleGetSubaccount
     , dir "account" $ dir "subaccount" $ hPost $ UserControl.handlePostSubaccount
     , dir "account" $ dir "sharing" $ hGet $ UserControl.handleGetSharing
     , dir "account" $ dir "sharing" $ hPost $ UserControl.handlePostSharing
     , dir "account" $ dir "security" $ hGet $ UserControl.handleGetUserSecurity
     , dir "account" $ dir "security" $ hPost $ UserControl.handlePostUserSecurity
     , dir "account" $ dir "bsa" $ hGet $ UserControl.handleGetBecomeSubaccountOf
     , dir "account" $ dir "bsa" $ hPost $ UserControl.handlePostBecomeSubaccountOf
     , dir "contacts"  $ hGet  $ Contacts.showContacts
     , dir "contacts"  $ hPost $ Contacts.handleContactsChange
     , dir "accepttos" $ hGet  $ UserControl.handleAcceptTOSGet
     , dir "accepttos" $ hPost $ UserControl.handleAcceptTOSPost

     -- super user only
     , dir "stats"      $ hGet  $ Administration.showStats
     , dir "createuser" $ hPost $ Administration.handleCreateUser
     , dir "sendgrid" $ dir "events" $ hPostNoXToken handleSendgridEvent
     , dir "adminonly" $ hGet $ Administration.showAdminMainPage
     , dir "adminonly" $ dir "advuseradmin" $ hGet Administration.showAdminUserAdvanced
     , dir "adminonly" $ dir "useradminforsales" $ hGet $ Administration.showAdminUsersForSales
     , dir "adminonly" $ dir "useradminforpayments" $ hGet $ Administration.showAdminUsersForPayments
     , dir "adminonly" $ dir "useradmin" $ hGet $ Administration.showAdminUsers . Just 
     , dir "adminonly" $ dir "useradmin" $ hGet $ Administration.showAdminUsers Nothing
     , dir "adminonly" $ dir "useradmin" $ dir "usagestats" $ hGet $ Administration.showAdminUserUsageStats
     , dir "adminonly" $ dir "useradmin" $ hPost Administration.handleUserChange
     , dir "adminonly" $ dir "useradmin" $ hPost Administration.handleUserEnableTrustWeaverStorage
     , dir "adminonly" $ dir "db" $ hGet $ Administration.indexDB
     , dir "adminonly" $ dir "db" $ onlySuperUser $ serveDirectory DisableBrowsing [] "_local/kontrakcja_state"
     , dir "adminonly" $ dir "quarantine" $ hGet  $ Administration.handleShowQuarantine
     , dir "adminonly" $ dir "quarantine" $ hPost $ Administration.handleQuarantinePost

     , dir "adminonly" $ dir "cleanup"           $ hPost $ Administration.handleDatabaseCleanup
     , dir "adminonly" $ dir "statistics"        $ hGet  $ Administration.handleStatistics
     , dir "adminonly" $ dir "skrivapausers.csv" $ hGet  $ Administration.getUsersDetailsToCSV
     , dir "adminonly" $ dir "payments"          $ hGet  $ Payments.handlePaymentsModelForViewView
     , dir "adminonly" $ dir "advpayments"       $ hGet  $ Payments.handlePaymentsModelForEditView
     , dir "adminonly" $ dir "advpayments"       $ hPost $ Payments.handleAccountModelsChange

     , dir "adminonly" $ dir "services" $ hGet $ Administration.showServicesPage
     , dir "adminonly" $ dir "services" $ param "create" $ hPost $ Administration.handleCreateService
     , dir "adminonly" $ dir "translations" $ hGet $ Administration.showAdminTranslations
     , dir "adminonly" $ dir "generatepotfiles" $ hPost $ Administration.handleGeneratePOTFiles

     -- a temporary service to help migration

     , dir "adminonly" $ dir "migrate0" $ hGet $ Administration.handleMigrate0
     , dir "adminonly" $ dir "deletemigrate" $ hGet $ Administration.handleMigrateForDeletion
--     , dir "adminonly" $ dir "migrateauthor" $ hGet $ DocControl.migrateDocSigLinks
     , dir "services" $ hGet $ handleShowServiceList
     , dir "services" $ hGet $ handleShowService
     , dir "services" $ hPost $ handleChangeService
     
     , dir "dave" $ dir "document" $ hGet $ daveDocument
     , dir "dave" $ dir "user"     $ hGet $ daveUser
           
     -- account stuff
     , dir "logout"      $ hGet  $ handleLogout
     , dir "login"       $ hGet  $ handleLoginGet
     , dir "login"       $ hPostNoXToken $ handleLoginPost
     --, dir "signup"      $ hGet  $ signupPageGet
     , dir "signup"      $ hPostAllowHttp $ signupPagePost
     --, dir "vip"         $ hGet  $ signupVipPageGet
     --, dir "vip"         $ hPostNoXToken $ signupVipPagePost
     , dir "amnesia"     $ hPostNoXToken $ forgotPasswordPagePost
     , dir "amnesia"     $ hGet  $ UserControl.handlePasswordReminderGet
     , dir "amnesia"     $ hPostNoXToken $ UserControl.handlePasswordReminderPost
     , dir "accountsetup"  $ hGet  $ UserControl.handleAccountSetupGet
     , dir "accountsetup"  $ hPostNoXToken  $ UserControl.handleAccountSetupPost
     , dir "accountremoval" $ hGet  $ UserControl.handleAccountRemovalGet
     , dir "accountremoval" $ hPostNoXToken  $ UserControl.handleAccountRemovalPost

     -- viral invite
     , dir "invite"      $ hPostNoXToken $ UserControl.handleViralInvite
     , dir "question"    $ hPostAllowHttp $ UserControl.handleQuestion
     -- e-legitimation stuff
     , dir "s" $ hGet  $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPost $ BankID.handleSignPostBankID
     , dir "d" $ hGet  $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost $ BankID.handleIssuePostBankID
     , coopAPI
     , webshopAPI
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
    liftM3 LinkNew DocControl.getDocType getListParamsForSearch (isFieldSet "showTemplates")

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
           , AWS.s3body = L.empty
           , AWS.s3operation = HTTP.GET
           }


maybeReadTemplates :: MVar (ClockTime, 
                             String -> Text.StringTemplate.Classes.StFirst
                             (Text.StringTemplate.Base.StringTemplate
                              String))
                       -> IO KontrakcjaTemplates                      
maybeReadTemplates mvar = modifyMVar mvar $ \(modtime, templates) -> do
        modtime' <- getTemplatesModTime
        if modtime /= modtime'
            then do 
                Log.debug $ "Reloading templates"
                templates' <- readTemplates
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
      let newservice =  fmap (\srv -> (serviceid srv,ctxlocation ctx'))  $ ctxservice ctx'
      F.updateFlashCookie (aesConfig appConf) (ctxflashmessages ctx) newflashmessages
      updateSessionWithContextData session newsessionuser newservice newelegtrans
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

      minutestime <- liftIO $ getMinutesTime
      muser <- getUserFromSession session
      mcompany <- getCompanyFromSession session
      mservice <- getServiceFromSession session
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
                , ctxtemplates = templates2
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
                , ctxservice = fst <$> mservice
                , ctxlocation = fromMaybe "" $ snd <$> mservice
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
  scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullname = userfullname user, email = unEmail $ useremail $ userinfo user }

{- |
   Handles viewing of the signup page
-}
_signupPageGet :: Kontra Response
_signupPageGet = do
    ctx <- lift get
    content <- liftIO (signupPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja $ cdata content 

    
_signupVipPageGet :: Kontra Response
_signupVipPageGet = do
    ctx <- lift get
    content <- liftIO (signupVipPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja $ cdata content 
{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}  
signupPagePost :: Kontra KontraLink
signupPagePost = do
    Context { ctxtime = MinutesTime time seconds } <- get
    signup False $ Just (MinutesTime (time + 60 * 24 * 31) seconds)

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
            scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullname = email, email = email }
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
    scheduleEmailSendout ctxesenforcer $ mail { fullname = email, email = email }

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
         V.renderFromBody V.TopNone V.kontrakcja . cdata $ content

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
                (Just s) -> renderFromBody V.TopNone V.kontrakcja (cdata $ BS.toString s)
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
    document <- queryOrFail $ GetDocumentByDocumentID documentid
    V.renderFromBody V.TopNone V.kontrakcja $ inspectXML document

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: UserID -> Kontra Response
daveUser userid = onlySuperUserGet $ do 
    user <- queryOrFail $ GetUserByUserID userid
    V.renderFromBody V.TopNone V.kontrakcja $ inspectXML user



{- 

{ "title": "Title of the document",
  "persons": [ 
           { "firstName": "Gracjan",
             "lastName": "Polak",
             "personNumber": "1412341234"
             "email": "gracjan@skrivapa.se"
           }]
}

-}

parseEmailMessageToParts :: BS.ByteString -> (MIME.MIMEValue, [(MIME.Type, BS.ByteString)])
parseEmailMessageToParts content = (mime, parts mime)
  where
    mime = MIME.parseMIMEMessage (BSC.unpack content)
    parts mimevalue = case MIME.mime_val_content mimevalue of 
        MIME.Single value -> [(MIME.mime_val_type mimevalue, BSC.pack value)]
        MIME.Multi more -> concatMap parts more
  
parseEmailMessage :: (Monad m, MonadIO m) => BS.ByteString -> m (Either String (JSValue,BS.ByteString,BS.ByteString,BS.ByteString))
parseEmailMessage content = runErrorT $ do
  let (mime, allParts) = parseEmailMessageToParts content 
      isPDF (tp,_) = MIME.mimeType tp == MIME.Application "pdf"
      isPlain (tp,_) = MIME.mimeType tp == MIME.Text "plain"
      typesOfParts = map fst allParts
  let pdfs = filter isPDF allParts
  let plains = filter isPlain allParts
  pdf <- case pdfs of
    [pdf] -> return pdf
    _ -> fail $ "Exactly one of attachments should be application/pdf, you have " ++ show typesOfParts
  plain <- case plains of
    [plain] -> return plain
    _ -> fail $ "Exactly one of attachments should be text/plain, you have " ++ show typesOfParts
  let pdfBinary = snd pdf
  let from = maybe BS.empty BS.fromString $ lookup "from" (MIME.mime_val_headers mime)
  let to = maybe BS.empty BS.fromString $ lookup "to" (MIME.mime_val_headers mime)
  
  let charset mimetype = maybe "us-ascii" id $ lookup "charset" (MIME.mimeParams mimetype)
  let recode mimetype content' = 
        case IConv.convertStrictly (charset mimetype) "UTF-8" (BSL.fromChunks [content']) of
          Left result' -> return $ BS.concat (BSL.toChunks (result'))
          Right errmsg -> fail (show $ IConv.reportConversionError errmsg) 
  recodedPlain <- recode (fst plain) (snd plain)   
    
  json <- (ErrorT . return) $ runGetJSON readJSObject (BS.toString recodedPlain)
  return (json,pdfBinary,from,to)
  
maybeFail :: (Monad m) => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return 

handleMailCommand :: JSValue -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Kontra (Either String DocumentID)
handleMailCommand (JSObject json) content from to = runErrorT $ do
  let username = takeWhile (/= '>') $ dropWhile (== '<') $ dropWhile (/= '<') $ BS.toString from
  -- 'extension' is a piece of data that is after + sign in email
  -- addres. example: api+1234@api.skrivapa.se here '1234' is
  -- extension and can be used as for example password
  let extension = takeWhile (/= '@') $ dropWhile (== '+') $ dropWhile (/= '+') $ BS.toString to
    
  Context { ctxtime, ctxipnumber } <- lift get
  maybeUser <- lift $ query $ GetUserByEmail Nothing (Email $ BS.fromString username)
  (user :: User) <- maybeFail ("user '" ++ username ++ "' not found") maybeUser
  
  let title = case get_field json "title" of
        Just (JSString x) -> BS.fromString (fromJSString x)
        _ -> BS.fromString "Untitled document received by email"
  let apikey = case get_field json "apikey" of
        Just (JSString x) -> fromJSString x
        _ -> extension
      extractPerson :: (Monad m) => JSValue -> m SignatoryDetails
      extractPerson (JSObject x) = do
        (JSString email :: JSValue) <- maybeFail "'email' is required string field" $ get_field x "email" 
        JSString firstName <- maybeFail "'firstName' is required string field" $ get_field x "firstName" 
        JSString lastName <- maybeFail "'lastName' is required string field" $ get_field x "lastName" 
        company <- case get_field x "company" of
          Just (JSString v) -> return $ fromJSString v
          _ -> return ""
        personalNumber <- case get_field x "personalNumber" of
          Just (JSString v) -> return $ fromJSString v
          _ -> return ""
        companyNumber <- case get_field x "companyNumber" of
          Just (JSString v) -> return $ fromJSString v
          _ -> return ""
        return $ SignatoryDetails 
                               { signatoryfstname = BS.fromString (fromJSString firstName)
                               , signatorysndname = BS.fromString (fromJSString lastName)
                               , signatorycompany = BS.fromString company
                               , signatorypersonalnumber = BS.fromString personalNumber
                               , signatorycompanynumber = BS.fromString companyNumber
                               , signatoryemail = BS.fromString (fromJSString email)
                               , signatoryfstnameplacements = []
                               , signatorysndnameplacements = []
                               , signatorycompanyplacements = []
                               , signatoryemailplacements = []
                               , signatorypersonalnumberplacements = []
                               , signatorycompanynumberplacements = []
                               , signatoryotherfields = []
                               , signatorysignorder = SignOrder 1
                               }
      extractPerson _ = fail "'persons' is not a JavaScript object"
      
  case apikey of
    "" -> fail $ "Need to specify 'apikey' in JSON or after + sign in email address"
    "998877665544332211" -> return ()
    z -> fail $ "Apikey '" ++ z ++ "' invalid for account '" ++ username ++ "'"

  doctype <- case get_field json "doctype" of
        Just (JSString x) -> case fromJSString x of
          "contract" -> return Contract
          "offer" -> return Offer
          z -> fail $ "Unsupported document type '" ++ z ++ "', should be one of 'contract' or 'offer'"
        _ -> return Contract
      
  JSArray personsField <- maybeFail "need to specify 'persons'" $ get_field json "persons"
  
  (persons :: [SignatoryDetails]) <- mapM extractPerson personsField
  let
      signatories = map (\p -> (p,[SignatoryPartner])) persons
      userDetails = signatoryDetailsFromUser user
        
  doc <- lift $ update $ NewDocument user title doctype ctxtime
  lift $ DocControl.handleDocumentUpload (documentid doc) content title
  _ <- ErrorT $ update $ UpdateDocument ctxtime (documentid doc) title 
    signatories Nothing BS.empty (userDetails, [SignatoryPartner, SignatoryAuthor], getSignatoryAccount user) [EmailIdentification] Nothing AdvancedFunctionality
  
  newdocument <- ErrorT $ update $ AuthorSendDocument (documentid doc) ctxtime ctxipnumber Nothing
  lift $ DocControl.markDocumentAuthorReadAndSeen newdocument ctxtime ctxipnumber
  lift $ DocControl.postDocumentChangeAction newdocument doc Nothing
  return (documentid doc)
handleMailCommand _ _ _ _ = return (Left ("needs JSON object as top level value"))
  
handleMailAPI :: Kontra Response
handleMailAPI = do
    Input contentspec (Just _filename) _contentType <- getDataFnM (lookInput "mail")
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
            
    -- content at this point is basically full MIME email as it was received by SMTP server
    -- can be tested using curl -X POST -F mail=@mail.eml http://url.com/mailapi/
    eresult <- parseEmailMessage (concatChunks content)
    case eresult of
      Left msg -> do
        Log.debug $ "handleMailAPI: " ++ msg
        return $ toResponse msg
      Right (json, pdf, from, to) -> do
        result' <- handleMailCommand json pdf from to
        case result' of
          Right docid -> do
            let rjson = makeObj [ ("status", JSString (toJSString "success"))
                                , ("message", JSString (toJSString ("Document #" ++ show docid ++ " created")))
                                , ("documentid", JSString (toJSString (show docid)))
                                ]
            return $ toResponse $ showJSValue rjson []
          Left msg -> do
            Log.debug $ msg
            let rjson = makeObj [ ("status", JSString (toJSString "error"))
                                , ("message", JSString (toJSString msg))
                                ]
            return $ toResponse $ showJSValue rjson []
