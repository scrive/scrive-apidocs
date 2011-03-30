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
import Control.Monad (msum, mzero)
import Control.Monad.State
import Control.Monad.Trans (liftIO,lift)
import Control.Concurrent
import Data.Functor
import AppView as V
import Control.Concurrent
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
import Templates.Templates (readTemplates, renderTemplate, KontrakcjaTemplates)
import qualified Administration.AdministrationControl as Administration
import Mails.MailsConfig
import Mails.SendGridEvents
import Mails.SendMail
import System.Log.Logger (Priority(..), logM)
import qualified AppLogger as Log (error)
import qualified MemCache
import Happstack.State (update)
import Redirect
import PayEx.PayExInterface -- Import so at least we check if it compiles
import InputValidation
import System.Directory
import ListUtil

{- | 
  Defines the application's configuration.  This includes amongst other things
  the http port number, amazon, trust weaver and email configuraton,
  as well as a handy boolean indicating whether this is a production or
  development instance.
-}
data AppConf
    = AppConf { httpPort        :: Int
              , hostpart        :: String
              , store           :: FilePath
              , static          :: FilePath 
              , awsBucket       :: String
              , awsAccessKey    :: String
              , awsSecretKey    :: String
              , production      :: Bool
              , twSignCert      :: FilePath
              , twSignCertPwd   :: String
              , twAdminCert     :: FilePath
              , twAdminCertPwd  :: String
              , twSignUrl       :: String
              , twAdminUrl      :: String
              , twStorageUrl    :: String
              , mailsConfig     :: MailsConfig

              }              
      deriving (Show,Read,Eq,Ord)

{- | 
  Global application data
-}
data AppGlobals 
    = AppGlobals { templates       :: KontrakcjaTemplates
                 , filecache       :: MemCache.MemCache FileID BS.ByteString
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
       hget0_allowHttp $ handleHomepage
     , hpost0 $ handleMainReaload
     , dir "s" $ hget0  $ DocControl.handleSTable
     , dir "s" $ hget3  $ DocControl.handleSignShow
     , dir "s" $ param "sign" $ hpost3 $ DocControl.signDocument
     , dir "s" $ param "cancel" $ hpost3 $ DocControl.rejectDocument
     
     --Q: This all needs to be done by author. Why we dont check it
     --here? MR

     --A: Because this table only contains routing logic. The logic of
     --what it does/access control is left to the handler. EN
     , dir "t" $ hget0  $ DocControl.showTemplatesList
     , dir "t" $ param "archive" $ hpost0 $ DocControl.handleTemplateArchive
     , dir "t" $ param "template" $ hpost0  $ DocControl.handleCreateFromTemplate
     , dir "t" $ hpost0  $ DocControl.handleCreateNewTemplate
     
     
     , dir "d" $ hget0  $ DocControl.showContractsList
     , dir "d" $ hget1  $ DocControl.handleIssueShowGet
     , dir "d" $ hget2  $ DocControl.handleIssueShowTitleGet
     , dir "d" $ {- param "doc" $ -} hpost0 $ DocControl.handleIssueNewDocument
     , dir "d" $ param "archive" $ hpost0 $ DocControl.handleContractArchive
     , dir "d" $ hpost0 $ DocControl.handleContractsReload
     , dir "d" $ hpost1 $ DocControl.handleIssueShowPost
     , dir "df" $ hget2 $ DocControl.handleFileGet

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hpost2 $ DocControl.handleResend
     , dir "changeemail" $ hpost2 $ DocControl.handleChangeSignatoryEmail
     -- , dir "withdrawn" $ hpost1 $ DocControl.handleWithdrawn
     , dir "restart" $ hpost1 $ DocControl.handleRestart
     , dir "cancel"  $ hpost1 $ DocControl.handleCancel
     
     , dir "pages"  $ hget2 $ DocControl.showPage
     , dir "templates"  $ hget0 $ DocControl.getTemplatesForAjax
     , dir "template"  $ hpost0 $ DocControl.handleCreateFromTemplate
     , dir "landpage" $ dir "signcanceleddatamismatch" $ hget2 $ BankID.handleSignCanceledDataMismatch
           
     , dir "pagesofdoc" $ hget1 $ DocControl.handlePageOfDocument

     -- UserControl
     , dir "account"                    $ hget0  $ UserControl.handleUserGet
     , dir "account"                    $ hpost0 $ UserControl.handleUserPost
     , dir "account" $ dir "subaccount" $ hget0  $ UserControl.handleGetSubaccount
     , dir "account" $ dir "subaccount" $ hpost0 $ UserControl.handlePostSubaccount
     , dir "contacts"  $ hget0  $ Contacts.showContacts
     , dir "contacts"  $ hpost0 $ Contacts.handleContactsChange
     , dir "accepttos" $ hget0  $ UserControl.handleAcceptTOSGet
     , dir "accepttos" $ hpost0 $ UserControl.handleAcceptTOSPost

     -- super user only
     , dir "stats"      $ hget0  $ Administration.showStats
     , dir "createuser" $ hpost0 $ Administration.handleCreateUser
     , dir "sendgrid" $ dir "events" $ hpost0 handleSendgridEvent
     , dir "adminonly" $ hget0 $ Administration.showAdminMainPage
     , dir "adminonly" $ dir "advuseradmin" $ hget0 Administration.showAdminUserAdvanced
     , dir "adminonly" $ dir "useradmin" $ hget1m Administration.showAdminUsers
     , dir "adminonly" $ dir "useradmin" $ hpost1 Administration.handleUserChange
     , dir "adminonly" $ dir "useradmin" $ hpost1 Administration.handleUserEnableTrustWeaverStorage
     , dir "adminonly" $ dir "db" $ hget0 $ Administration.indexDB
     , dir "adminonly" $ dir "db" $ onlySuperUser $ fileServe [] "_local/kontrakcja_state"

     , dir "adminonly" $ dir "cleanup"           $ hpost0 $ Administration.handleDatabaseCleanup
     , dir "adminonly" $ dir "alluserstable"     $ hget0  $ Administration.showAllUsersTable
     , dir "adminonly" $ dir "skrivapausers.csv" $ hget0  $ Administration.getUsersDetailsToCSV
     , dir "adminonly" $ dir "payments"          $ hget0  $ Payments.handlePaymentsModelForViewView
     , dir "adminonly" $ dir "advpayments"       $ hget0  $ Payments.handlePaymentsModelForEditView
     , dir "adminonly" $ dir "advpayments"       $ hpost0 $ Payments.handleAccountModelsChange

     -- a temporary service to help migration

     , dir "adminonly" $ dir "migrate0" $ hget0 $ Administration.handleMigrate0

     , dir "dave" $ dir "document" $ hget1 $ daveDocument
     , dir "dave" $ dir "user"     $ hget1 $ daveUser
           
     -- account stuff
     , dir "logout"      $ hget0  $ handleLogout
     , dir "login"       $ hget0  $ handleLoginGet
     , dir "login"       $ hpost0 $ handleLoginPost
     , dir "signup"      $ hget0  $ signupPageGet
     , dir "signup"      $ hpost0 $ signupPagePost
     , dir "vip"         $ hget0  $ signupVipPageGet
     , dir "vip"         $ hpost0 $ signupVipPagePost
     , dir "amnesia"     $ hpost0 $ forgotPasswordPagePost
     , dir "amnesia"     $ hget2  $ UserControl.handlePasswordReminderGet
     , dir "amnesia"     $ hpost2 $ UserControl.handlePasswordReminderPost
     , dir "accountsetup"  $ hget2  $ UserControl.handleAccountSetupGet
     , dir "accountsetup"  $ hpost2  $ UserControl.handleAccountSetupPost
     , dir "accountremoval" $ hget2  $ UserControl.handleAccountRemovalGet
     , dir "accountremoval" $ hpost2  $ UserControl.handleAccountRemovalPost

     , dir "requestaccount" $ hpost0_allowHttp $ UserControl.handleRequestAccount
     -- viral invite
     , dir "invite"      $ hpost0 $ UserControl.handleViralInvite
     , dir "question"    $ hpost0_allowHttp $ UserControl.handleQuestion
     -- e-legitimation stuff
     , dir "s" $ hget4  $ BankID.handleSignBankID
     , dir "s" $ param "eleg" $ hpost3 $ BankID.handleSignPostBankID
     , dir "d" $ hget2  $ BankID.handleIssueBankID
     , dir "d" $ param "eleg" $ hpost1 $ BankID.handleIssuePostBankID

     -- static files
     , allowHttp $ serveHTMLFiles
     , allowHttp $ fileServe [] "public"
               ]

{- |
   Goes to the front page, or to the main document upload page,
   depending on whether there is a logged in user.
-}
handleHomepage :: Kontra Response
handleHomepage = do
    ctx@Context{ ctxmaybeuser } <- get
    loginOn <- isFieldSet "logging"
    referer <- getField "referer"
    case ctxmaybeuser of
        Just user -> UserControl.checkUserTOSGet $ DocControl.showMainPage user 
        Nothing   -> V.simpleResponse =<< (liftIO $ firstPage ctx loginOn referer)

{- |
    Handles an error by displaying the home page with a modal error dialog.
-}
handleError :: Kontra Response
handleError = do
    ctx <- get
    addModal $ V.modalError (ctxtemplates ctx)
    sendRedirect LinkMain

handleMainReaload :: Kontra KontraLink
handleMainReaload = LinkNew <$> getListParamsForSearch

{- |
   Creates a default amazon configuration based on the
   given AppConf
-}
defaultAWSAction :: AppConf -> AWS.S3Action
defaultAWSAction appConf = 
    AWS.S3Action 
           { AWS.s3conn = AWS.amazonS3Connection 
                          (awsAccessKey appConf) 
                          (awsSecretKey appConf)
           , AWS.s3bucket = awsBucket appConf
           , AWS.s3object = ""
           , AWS.s3query = ""
           , AWS.s3metadata = []
           , AWS.s3body = L.empty
           , AWS.s3operation = HTTP.GET
           }

{- |
   Creates a context, routes the request, and handles the session.
-}
appHandler :: AppConf -> AppGlobals -> MVar (Map.Map FileID JpegPages) -> ServerPartT IO Response
appHandler appConf appGlobals docs = do
  let quota = 10000000
  temp <- liftIO $ getTemporaryDirectory
  decodeBody (defaultBodyPolicy temp quota quota quota)

  rq <- askRq
  session <- handleSession
  ctx <- createContext rq session docs
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
             liftIO $ logM "Happstack.Server" ERROR $ "ERROR" ++ (showDateMDY $ ctxtime ctx)++" "++(rqUri rq) ++" "++(show rq) ++ " " ++ show rqcontent
             response <- handleError
             setRsCode 404 response     
          ctx' <- get 
          return (res,ctx')   
      
      let newsessionuser = fmap userid $ ctxmaybeuser ctx'  
      let newflashmessages = ctxflashmessages ctx'
      let newelegtrans = ctxelegtransactions ctx'
      updateSessionWithContextData session newsessionuser newflashmessages newelegtrans
      return res

    createContext :: Request -> Session -> MVar (Map.Map FileID JpegPages) -> ServerPartT IO Context
    createContext rq session docs = do
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
      flashmessages <- getFlashMessagesFromSession session          

      -- do reload templates in non-production code
      templates2 <- if production appConf
                    then return $ templates appGlobals
                    else liftIO $ readTemplates

      let elegtrans = getELegTransactions session
      let
       mailer | sendMails cfg  = createRealMailer cfg
              | otherwise = createDevMailer (ourInfoEmail cfg) (ourInfoEmailNiceName cfg)
         where cfg = mailsConfig appConf
       ctx = Context
                { ctxmaybeuser = muser
                , ctxhostpart = hostpart
                , ctxflashmessages = flashmessages
                , ctxtime = minutestime
                , ctxnormalizeddocuments = docs
                , ctxipnumber = peerip
                , ctxs3action = defaultAWSAction appConf
                , ctxproduction = production appConf
                , ctxtemplates = templates2
                , ctxmailer = mailer
                , ctxtwconf = TW.TrustWeaverConf 
                              { TW.signcert = twSignCert appConf
                              , TW.signcertpwd = twSignCertPwd appConf
                              , TW.admincert = twAdminCert appConf
                              , TW.admincertpwd = twAdminCertPwd appConf
                              , TW.signurl = twSignUrl appConf
                              , TW.adminurl = twAdminUrl appConf
                              , TW.storageurl = twStorageUrl appConf
                              , TW.retries = 3
                              , TW.timeout = 60000
                              }
                , ctxelegtransactions = elegtrans
                , ctxfilecache = filecache appGlobals
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
                    addFlashMsg =<< (liftIO $ flashMessageNoSuchUserExists $ ctxtemplates ctx)
                    return LoopBack
                Just user -> do
                    sendResetPasswordMail user
                    addFlashMsg =<< (liftIO $ flashMessageChangePasswordEmailSend $ ctxtemplates ctx)
                    return LinkMain
        Nothing -> return LoopBack

{- |
   Sends a password reset mail
-}
sendResetPasswordMail::User -> Kontra ()
sendResetPasswordMail user = do
    ctx <- get
    chpwdlink <- newPasswordReminderLink user
    mail <-liftIO $ UserView.resetPasswordMail (ctxtemplates ctx) (ctxhostpart ctx) user chpwdlink
    liftIO $ sendMail (ctxmailer ctx) $ mail { fullnameemails = [((userfullname user), (unEmail $ useremail $ userinfo user))] }

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
signupPagePost = signup False $ parseMinutesTimeMDY "31-05-2011"

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
                             liftIO $ sendMail (ctxmailer ctx) $ mail { fullnameemails = [(email, email)] }
                             addFlashMsg =<< (liftIO $ flashMessageNewActivationLinkSend  (ctxtemplates))
                             return LoopBack
                         else do
                             addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
                             return LoopBack
                  Nothing -> do
                      maccount <- liftIO $ UserControl.createUser ctx ctxhostpart BS.empty email Nothing vip
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
sendNewActivationLinkMail Context{ctxtemplates,ctxhostpart,ctxmailer} user = do
    let email = unEmail $ useremail $ userinfo user
    al <- newAccountCreatedLink user
    mail <- liftIO $ newUserMail ctxtemplates ctxhostpart email email al False
    liftIO $ sendMail ctxmailer $ mail { fullnameemails = [(email, email)] }

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
                        return $ LinkLogin InvalidLoginInfo
                Nothing -> return $ LinkLogin InvalidLoginInfo
        _ -> return $ LinkLogin InvalidLoginInfo

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
   Serves out the static files.
-}
serveHTMLFiles:: Kontra Response  
serveHTMLFiles =  do
    ctx <- get
    rq <- askRq
    let fileName = last (rqPaths rq)
    if ((length (rqPaths rq) > 0) && isSuffixOf ".html" fileName)
        then do
            ms <- liftIO $ catch (fmap Just ( BS.readFile $ "html/"++fileName)) 
                            (const $ return Nothing)
            case ms of 
                Just s -> renderFromBody ctx V.TopNone V.kontrakcja (cdata $ BS.toString s)
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

hpost0 :: Kontra KontraLink -> Kontra Response
hpost0 action = methodM POST >> (https $ do
    (link :: KontraLink) <- action
    sendRedirect link)

hpost1 :: (FromReqURI a) =>  (a -> Kontra KontraLink) -> Kontra Response
hpost1 action = path $ \a1 -> methodM POST >>  (https $ do
    (link :: KontraLink) <- action a1
    sendRedirect link)

hpost2 :: (FromReqURI a, FromReqURI a1) =>   (a -> a1 -> Kontra KontraLink) -> Kontra Response
hpost2 action = path $ \a1 -> path $ \a2 -> methodM POST >>  (https $ do
    (link :: KontraLink) <- action a1 a2
    sendRedirect link)

hpost3 :: (FromReqURI a, FromReqURI a1, FromReqURI a2) =>  
          (a -> a1 -> a2 -> Kontra KontraLink) 
          -> Kontra Response
hpost3 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> methodM POST >> (https $ do
    (link :: KontraLink) <- action a1 a2 a3
    sendRedirect link)

hpost4 :: (FromReqURI a, FromReqURI a1, FromReqURI a2, FromReqURI a3) =>  
          (a -> a1 -> a2 -> a3 -> Kontra KontraLink)
           -> Kontra Response
hpost4 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> path $ \a4 -> methodM POST >>  (https $ do
    (link :: KontraLink) <- action a1 a2 a3 a4
    sendRedirect link)

hget0 :: Kontra Response -> Kontra Response
hget0 action = methodM GET >> (https $ action)


hget1 :: (FromReqURI a) => (a -> Kontra Response) -> Kontra Response
hget1 action = path $ \a1 -> methodM GET >> (https $ action a1)

hget2 :: (FromReqURI a, FromReqURI a1) =>
     (a -> a1 -> Kontra Response) -> Kontra Response
hget2 action = path $ \a1 -> path $ \a2 -> methodM GET >> (https $ action a1 a2)

hget3 :: (FromReqURI a,
            FromReqURI a1,
            FromReqURI a2) =>
            (a -> a1 -> a2 -> Kontra Response) -> Kontra Response
hget3 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> methodM GET >> (https $ action a1 a2 a3)

hget4 :: (FromReqURI a,
            FromReqURI a1,
            FromReqURI a2,
            FromReqURI a3) =>
            (a -> a1 -> a2 -> a3 -> Kontra Response) -> Kontra Response
hget4 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> path $ \a4 -> methodM GET >> (https $ action a1 a2 a3 a4)

{-|
  Version supporting optional path param
  Usually when we want to have similar path when looking at list and when looking at element

 -} 
hget1m :: (Maybe String -> Kontra Response) -> Kontra Response 
hget1m action = (path $ \a1 -> methodM GET >> (action $ Just a1)) 
    `mplus` (methodM GET >> action Nothing)

{- |
   Guard on the existence of a parameter.
 -}
param :: String -> Kontra Response -> Kontra Response
param p action = (getDataFnM $ look p) >> action -- will mzero if parameter is not found

hget0_allowHttp :: Kontra Response -> Kontra Response
hget0_allowHttp action = methodM GET >> (allowHttp $ action)

hpost0_allowHttp :: Kontra KontraLink -> Kontra Response
hpost0_allowHttp action = methodM POST >> (allowHttp $ do
                  (link :: KontraLink) <- action
                  sendRedirect link)
                  

https:: Kontra Response -> Kontra Response
https action = do
    secure <- isSecure
    if secure 
       then action
       else sendSecureLoopBack
              

allowHttp:: Kontra Response -> Kontra Response
allowHttp action = do
    secure <- isSecure
    loging <- isFieldSet "logging"
    logged <- isJust <$> ctxmaybeuser <$> get
    if (secure || (not $ loging || logged))
       then action
       else sendSecureLoopBack
             
