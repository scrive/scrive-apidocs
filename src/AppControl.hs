{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module AppControl
    ( appHandler
    , AppConf(..)
    , AppGlobals(..)
    , defaultAWSAction
    ) where

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
import Happstack.Server hiding (simpleHTTP,host)
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
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as BSL
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
import API.WebshopAPI
import Happstack.Server.Internal.Cookie
import API.IntegrationAPI
import Templates.Templates
import Routing
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
     , dir "prisplan" $ hGetAllowHttp $ handlePriceplanPage
     , dir "sakerhet" $ hGetAllowHttp $ handleSecurityPage
     , dir "juridik" $ hGetAllowHttp $ handleLegalPage
     , dir "sekretesspolicy" $ hGetAllowHttp $ handlePrivacyPolicyPage
     , dir "allmana-villkor" $ hGetAllowHttp $ handleTermsPage
     , dir "om-skrivapa" $ hGetAllowHttp $ handleAboutPage
     , dir "partners" $ hGetAllowHttp $ handlePartnersPage
     , dir "kunder" $ hGetAllowHttp $ handleClientsPage
     

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
     , dir "adminonly" $ dir "useradmin" $ hGet $ Administration.showAdminUsers . Just 
     , dir "adminonly" $ dir "useradmin" $ hGet $ Administration.showAdminUsers Nothing
     , dir "adminonly" $ dir "useradmin" $ hPost Administration.handleUserChange
     , dir "adminonly" $ dir "useradmin" $ hPost Administration.handleUserEnableTrustWeaverStorage
     , dir "adminonly" $ dir "db" $ hGet $ Administration.indexDB
     , dir "adminonly" $ dir "db" $ onlySuperUser $ fileServe [] "_local/kontrakcja_state"

     , dir "adminonly" $ dir "cleanup"           $ hPost $ Administration.handleDatabaseCleanup
     , dir "adminonly" $ dir "statistics"        $ hGet  $ Administration.handleStatistics
     , dir "adminonly" $ dir "skrivapausers.csv" $ hGet  $ Administration.getUsersDetailsToCSV
     , dir "adminonly" $ dir "payments"          $ hGet  $ Payments.handlePaymentsModelForViewView
     , dir "adminonly" $ dir "advpayments"       $ hGet  $ Payments.handlePaymentsModelForEditView
     , dir "adminonly" $ dir "advpayments"       $ hPost $ Payments.handleAccountModelsChange

     , dir "adminonly" $ dir "services" $ hGet $ Administration.showServicesPage
     , dir "adminonly" $ dir "services" $ param "create" $ hPost $ Administration.handleCreateService
     , dir "adminonly" $ dir "services" $ param "add" $ hPost $ Administration.handleAddUserToService
     
     -- a temporary service to help migration

     , dir "adminonly" $ dir "migrate0" $ hGet $ Administration.handleMigrate0

     , dir "dave" $ dir "document" $ hGet $ daveDocument
     , dir "dave" $ dir "user"     $ hGet $ daveUser
           
     -- account stuff
     , dir "logout"      $ hGet  $ handleLogout
     , dir "login"       $ hGet  $ handleLoginGet
     , dir "login"       $ hPostNoXToken $ handleLoginPost
     --, dir "signup"      $ hGet  $ signupPageGet
     , dir "signup"      $ hPostNoXToken $ signupPagePost
     --, dir "vip"         $ hGet  $ signupVipPageGet
     --, dir "vip"         $ hPostNoXToken $ signupVipPagePost
     , dir "amnesia"     $ hPostNoXToken $ forgotPasswordPagePost
     , dir "amnesia"     $ hGet  $ UserControl.handlePasswordReminderGet
     , dir "amnesia"     $ hPostNoXToken $ UserControl.handlePasswordReminderPost
     , dir "accountsetup"  $ hGet  $ UserControl.handleAccountSetupGet
     , dir "accountsetup"  $ hPostNoXToken  $ UserControl.handleAccountSetupPost
     , dir "accountremoval" $ hGet  $ UserControl.handleAccountRemovalGet
     , dir "accountremoval" $ hPostNoXToken  $ UserControl.handleAccountRemovalPost

     , dir "requestaccount" $ hPostAllowHttp $ UserControl.handleRequestAccount
     -- viral invite
     , dir "invite"      $ hPostNoXToken $ UserControl.handleViralInvite
     , dir "question"    $ hPostAllowHttp $ UserControl.handleQuestion
     -- e-legitimation stuff
     , dir "s" $ hGet  $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hPost $ BankID.handleSignPostBankID
     , dir "d" $ hGet  $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hPost $ BankID.handleIssuePostBankID
     , webshopAPI
     , integrationAPI
     -- static files
     , allowHttp $ serveHTMLFiles
     , allowHttp $ fileServe [] "public"
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
    case (ctxmaybeuser,ctxservice) of
        (Just user,_) -> Right <$> (UserControl.checkUserTOSGet DocControl.mainPage)
        (Nothing,Nothing)   -> do
                        resp <- V.simpleResponse =<< (liftIO $ firstPage ctx loginOn referer email)
                        clearFlashMsgs
                        return $ Left resp
        _ -> Left <$> embeddedErrorPage ctx      

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

handleWholePage :: (Context -> Kontra String) -> Kontra Response
handleWholePage f = do
    ctx <- get
    content <- f ctx
    resp <- V.simpleResponse content
    clearFlashMsgs
    return resp

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
         Just _ -> embeddedErrorPage ctx   

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


                      
maybeReadTemplates mvar = modifyMVar mvar $ \(modtime,templates) -> do
        modtime' <- getTemplatesModTime
        if modtime /= modtime'
            then do 
                Log.debug $ "Reloading templates"
                templates' <- readTemplates
                return ((modtime', templates'), templates')
            else return ((modtime, templates), templates)
 
showNamedHeader (_nm,hd) = BS.toString (hName hd) ++ ": [" ++ 
                      concat (intersperse ", " (map (show . BS.toString) (hValue hd))) ++ "]" 

showNamedCookie (name,cookie) = name ++ ": " ++ mkCookieHeader Nothing cookie 

showNamedInput (name,input) = name ++ ": " ++ case inputFilename input of
                                                  Just filename -> filename
                                                  _ -> case inputValue input of
                                                           Left tmpfilename -> "<<content in /tmp>>"
                                                           Right value -> show (BSL.toString value) 
                              

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
  let quota = 10000000
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

      minutestime <- liftIO $ getMinutesTime
      muser <- getUserFromSession session
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
                , ctxservice = mservice
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
        Just email -> do
            muser <- query $ GetUserByEmail $ Email email
            case muser of 
                Nothing -> do
                    Log.security $ "ip " ++ (show $ ctxipnumber ctx) ++ " made a failed password reset request for non-existant account " ++ (BS.toString email)
                Just user -> do
                    now <- liftIO getMinutesTime
                    minv <- checkValidity now <$> (query $ GetPasswordReminder $ userid user)
                    case minv of
                         Just Action{actionID, actionType} -> do
                             case prRemainedEmails actionType of
                                  0 -> do
                                      addFlashMsg =<< (liftIO $ flashMessageNoRemainedPasswordReminderEmails $ ctxtemplates ctx)
                                  n -> do
                                      update $ UpdateActionType actionID $ actionType { prRemainedEmails = n-1 }
                                      sendResetPasswordMail ctx (LinkPasswordReminder actionID $ prToken actionType) user
                         Nothing -> do
                             link <- newPasswordReminderLink user
                             sendResetPasswordMail ctx link user
            addFlashMsg =<< (liftIO $ flashMessageChangePasswordEmailSend $ ctxtemplates ctx)
            return LinkMain
        Nothing -> return LoopBack
    where
        sendResetPasswordMail ctx link user = do
            mail <- liftIO $ UserView.resetPasswordMail (ctxtemplates ctx) (ctxhostpart ctx) user link
            scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [((userfullname user), (unEmail $ useremail $ userinfo user))] }

{- |
   Handles viewing of the signup page
-}
signupPageGet :: Kontra Response
signupPageGet = do
    ctx <- lift get
    content <- liftIO (signupPageView $ ctxtemplates ctx)
    V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata content 

{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}  
    
signupVipPageGet :: Kontra Response
signupVipPageGet = do
    ctx <- lift get
    content <- liftIO (signupVipPageView $ ctxtemplates ctx)
    V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata content 

signupPagePost :: Kontra KontraLink
signupPagePost = do
    Context { ctxtime = MinutesTime time seconds } <- get
    signup False $ Just (MinutesTime (time + 60 * 24 * 31) seconds)

signupVipPagePost :: Kontra KontraLink
signupVipPagePost = signup True $ parseMinutesTimeMDY "31-12-2011"

{- 
    A comment next to LoopBack says never to use it. Is this function broken?
-}                   
signup :: Bool -> (Maybe MinutesTime) -> Kontra KontraLink
signup vip freetill =  do
    ctx@Context{ctxtemplates,ctxhostpart} <- lift get
    memail <- getOptionalField asValidEmail "email"
    case memail of
         Nothing -> return LoopBack
         Just email -> do
             muser <- query $ GetUserByEmail $ Email $ email
             case  muser of
                  Just user ->
                      if isNothing $ userhasacceptedtermsofservice user
                         then do
                             al <- newAccountCreatedLink user
                             mail <- liftIO $ newUserMail (ctxtemplates) (ctxhostpart) email email al vip
                             scheduleEmailSendout (ctxesenforcer ctx) $ mail { fullnameemails = [(email, email)] }
                             addFlashMsg =<< (liftIO $ flashMessageNewActivationLinkSend  (ctxtemplates))
                             return LoopBack
                         else do
                             addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
                             return LoopBack
                  Nothing -> do
                      maccount <- liftIO $ UserControl.createUser ctx ctxhostpart (BS.empty, BS.empty) email Nothing vip
                      case maccount of
                           Just account ->  do
                               addFlashMsg =<< (liftIO $ flashMessageUserSignupDone ctxtemplates)
                               when (isJust freetill) $
                                   update $ FreeUserFromPayments account (fromJust freetill)
                               return LoopBack
                           Nothing -> do
                               addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
                               return LoopBack

{- |
   Sends a new activation link mail, which is really just a new user mail.
-}
sendNewActivationLinkMail:: Context -> User -> Kontra ()
sendNewActivationLinkMail Context{ctxtemplates,ctxhostpart,ctxesenforcer} user = do
    let email = unEmail $ useremail $ userinfo user
    al <- newAccountCreatedLink user
    mail <- liftIO $ newUserMail ctxtemplates ctxhostpart email email al False
    scheduleEmailSendout ctxesenforcer $ mail { fullnameemails = [(email, email)] }

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
         V.renderFromBody ctx V.TopNone V.kontrakcja . cdata $ content

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
            maybeuser <- query $ GetUserByEmail (Email email)
            case maybeuser of
                Just User{ userid, userpassword } 
                    | verifyPassword userpassword passwd -> do
                        logUserToContext maybeuser
                        time <- liftIO getMinutesTime
                        _ <- update $ RecordSuccessfulLogin userid time
                        return BackToReferer
                Just User{ userlogininfo, userid } -> do
                        slug <- liftIO $ getFailedLoginSlug userlogininfo
                        when (slug > 0) $ liftIO . threadDelay $ slug * 1000000
                        time <- liftIO getMinutesTime
                        _ <- update $ RecordFailedLogin userid time
                        return $ LinkLogin $ InvalidLoginInfo linkemail
                Nothing -> return $ LinkLogin $ InvalidLoginInfo linkemail
        _ -> return $ LinkLogin $ InvalidLoginInfo linkemail

{- |
    Works out how many seconds we should wait before
    finishing a failed login.  This will hopefully help
    prevent brute force attacks on user passwords.
    Here the slug is 20s after 5 consecutive fails, and 40s after
    10 consecutive fails.
-}
getFailedLoginSlug :: LoginInfo -> IO Int
getFailedLoginSlug LoginInfo{ lastfailtime, consecutivefails } = do
    now <- getMinutesTime
    let spacing = case consecutivefails of
                    n | (n <  5) ->  0
                    n | (n < 10) -> 20
                    _            -> 40
    case lastfailtime of
        Just lastfail -> return $ max 0 (spacing - (secs now - secs lastfail))
        Nothing       -> return 0
    where secs (MinutesTime m s) = m * 60 + s

-- last fail ---------- time
 
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
    ctx <- get
    rq <- askRq
    let fileName = last (rqPaths rq)
    if ((length (rqPaths rq) > 0) && (isSuffixOf ".html" fileName))
        then do
            ms <- liftIO $ catch (fmap Just ( BS.readFile $ "html/"++fileName)) 
                            (const $ return Nothing)
            case ms of 
                (Just s) -> renderFromBody ctx V.TopNone V.kontrakcja (cdata $ BS.toString s)
                _      -> mzero
        else mzero

{- |
   Ensures logged in as a super user
-}
onlySuperUserGet :: Kontra Response -> Kontra Response  
onlySuperUserGet action = do
    Context{ ctxmaybeuser } <- get 
    if isSuperUser ctxmaybeuser 
        then action
        else sendRedirect $ LinkLogin NotLoggedAsSuperUser

{- |
   Used by super users to inspect a particular document.
-}
daveDocument :: DocumentID -> Kontra Response
daveDocument documentid = onlySuperUserGet $ do
    ctx <- get
    document <- queryOrFail $ GetDocumentByDocumentID documentid
    V.renderFromBody ctx V.TopNone V.kontrakcja $ inspectXML document

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: UserID -> Kontra Response
daveUser userid = onlySuperUserGet $ do 
    ctx <- get
    user <- queryOrFail $ GetUserByUserID userid
    V.renderFromBody ctx V.TopNone V.kontrakcja $ inspectXML user
