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
import Templates.Templates (readTemplates, renderTemplate, KontrakcjaTemplates)
import qualified Administration.AdministrationControl as Administration
import Mails.MailsConfig
import Mails.SendGridEvents
import Mails.SendMail
import System.Log.Logger (Priority(..), logM)
import qualified AppLogger as Log (error)
import qualified MemCache

{-| 
  Defines the application's configuration.  This includes amongst other things
  the http port number, amazon, trust weaver and email configuraton,
  as well as a handy boolean indicating whether this is a production or
  development instance.
-}
data AppConf
    = AppConf { httpPort        :: Int
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

{-| 
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
       hget0 $ handleHomepage

     , dir "s" $ hget0  $ DocControl.handleSTable
     , dir "s" $ hget3  $ DocControl.handleSignShow
     , dir "s" $ param "sign" $ hpost3 $ DocControl.signDocument
     , dir "s" $ param "cancel" $ hpost3 $ DocControl.rejectDocument
     
     --Q: This all needs to be done by author. Why we dont check it
     --here? MR

     --A: Because this table only contains routing logic. The logic of
     --what it does/access control is left to the handler. EN

     , dir "d" $ hget0  $ DocControl.handleIssueGet
     , dir "d" $ hget1  $ DocControl.handleIssueShowGet
     , dir "d" $ hget2  $ DocControl.handleIssueShowTitleGet
     , dir "d" $ param "doc" $ hpost0 $ DocControl.handleIssueNewDocument
     , dir "d" $ param "archive" $ hpost0 $ DocControl.handleIssueArchive
     , dir "d" $ hpost1 $ DocControl.handleIssueShowPost
     , dir "df" $ hget2 $ DocControl.handleFileGet

     --This are actions on documents. We may integrate it with all the stuff above, but I don't like it. MR
     , dir "resend"  $ hpost2 $ DocControl.handleResend
     , dir "changeemail" $ hpost2 $ DocControl.handleChangeSignatoryEmail
     -- , dir "withdrawn" $ hpost1 $ DocControl.handleWithdrawn
     , dir "restart" $ hpost1 $ DocControl.handleRestart
     , dir "cancel"  $ hpost1 $ DocControl.handleCancel
     
     , dir "pages"  $ hget2 $ DocControl.showPage

     , dir "landpage" $ dir "signinvite" $ hget1 $ DocControl.landpageSignInvite
     , dir "landpage" $ dir "signed"     $ hget2 $ DocControl.landpageSigned 
     , dir "landpage" $ dir "rejected"   $ hget2 $ DocControl.landpageRejected
     , dir "landpage" $ dir "signedsave" $ hget2 $ DocControl.landpageSignedSave
           
     , dir "pagesofdoc" $ hget1 $ DocControl.handlePageOfDocument

     -- UserControl
     , dir "account"                    $ hget0  $ UserControl.handleUserGet
     , dir "account"                    $ hpost0 $ UserControl.handleUserPost
     , dir "account" $ dir "password"   $ hpost0 $ UserControl.handleUserPasswordPost
     , dir "account" $ dir "subaccount" $ hget0  $ UserControl.handleGetSubaccount
     , dir "account" $ dir "subaccount" $ hpost0 $ UserControl.handlePostSubaccount

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
     , dir "signupdone"  $ hget0  $ signupPageDone
     , dir "vip"         $ hget0  $ signupVipPageGet
     , dir "vip"         $ hpost0 $ signupVipPagePost
     , dir "amnesia"     $ hget0  $ forgotPasswordPageGet
     , dir "amnesia"     $ hpost0 $ forgotPasswordPagePost
     , dir "amnesiadone" $ hget0  $ forgotPasswordDonePage
     , dir "accountsetup"  $ hget2  $ UserControl.unloggedActionPage
     , dir "accountsetup"  $ hpost2  $ UserControl.handleUnloggedAction
     , dir "requestaccount" $ hpost0 $ UserControl.handleRequestAccount
     -- viral invite
     , dir "invite"      $ hpost0 $ UserControl.handleViralInvite
     , dir "question"    $ hpost0 $ UserControl.handleQuestion
     -- e-legitimation stuff
     {- Disabled until finished
     , dir "bankid" $ dir "s" $ hget3  $ BankID.handleSignBankID
     , dir "bankid" $ dir "d" $ hget1  $ BankID.handleIssueBankID
     , dir "s" $ param "bankid" $ hpost3 $ BankID.handleSignPostBankID
     , dir "d" $ param "bankid" $ hpost1 $ BankID.handleIssuePostBankID
     

     , dir "nordea" $ dir "s" $ hget3  $ BankID.handleSignNordea
     , dir "nordea" $ dir "d" $ hget1  $ BankID.handleIssueNordea
     , dir "s" $ param "nordea" $ hpost3 $ BankID.handleSignPostNordea
     , dir "d" $ param "nordea" $ hpost1 $ BankID.handleIssuePostNordea

     , dir "telia" $ dir "s" $ hget3  $ BankID.handleSignTelia
     , dir "telia" $ dir "d" $ hget1  $ BankID.handleIssueTelia
     , dir "s" $ param "telia" $ hpost3 $ BankID.handleSignPostTelia
     , dir "d" $ param "telia" $ hpost1 $ BankID.handleIssuePostTelia
     -}

     -- static files
     , serveHTMLFiles
     , fileServe [] "public"
               ]
{- |
   Goes to the front page, or to the main document upload page,
   depending on whether there is a logged in user.
-}
handleHomepage :: ServerPartT (StateT Context IO) Response
handleHomepage = do
  ctx@Context{ctxmaybeuser} <- get
  case ctxmaybeuser of
    Just user -> UserControl.checkUserTOSGet $ DocControl.showMainPage user 
    Nothing -> V.simpleResponse =<< (liftIO $ firstPage ctx)

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
appHandler :: AppConf -> AppGlobals -> ServerPartT IO Response
appHandler appConf appGlobals= do
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
             liftIO $ logM "Happstack.Server" ERROR $ "ERROR" ++ (showDateMDY $ ctxtime ctx)++" "++(rqUri rq) ++" "++(show rq)
             response <- V.renderFromBody ctx V.TopNone V.kontrakcja (fmap cdata $ renderTemplate (ctxtemplates ctx) "errorPage" ())
             setRsCode 404 response     
          ctx' <- get 
          return (res,ctx')   
      
      let newsessionuser = fmap userid $ ctxmaybeuser ctx'  
      let newflashmessages = ctxflashmessages ctx'
      let newelegtrans = ctxelegtransactions ctx'
      updateSessionWithContextData session newsessionuser newflashmessages newelegtrans
      return res

    -- uh uh, how to do that in correct way?
    normalizeddocuments :: MVar (Map.Map FileID JpegPages)
    normalizeddocuments = unsafePerformIO $ newMVar Map.empty


    createContext :: Request -> Session -> ServerPartT IO Context
    createContext rq session = do
      let host = maybe "skrivapa.se" BS.toString $ getHeader "host" rq
      let scheme = maybe "http" BS.toString $ getHeader "scheme" rq
      let hostpart =  scheme ++ "://" ++ host

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
                , ctxnormalizeddocuments = normalizeddocuments
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
   Handles viewing of the password reset page
-}
forgotPasswordPageGet :: Kontra Response
forgotPasswordPageGet = do
    ctx <- lift get
    content <- liftIO $ V.pageForgotPassword (ctxtemplates ctx)
    V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata content

{- |
   Handles submission of the password reset form
-}    
forgotPasswordPagePost :: Kontra KontraLink
forgotPasswordPagePost = do
    ctx <- get
    memail <- getField "email"
    case memail of 
      Just email -> do
                      muser <- query $ GetUserByEmail $ Email (BS.fromString email)                    
                      case muser of 
                       Nothing -> do 
                                   addFlashMsgText =<< (liftIO $ flashMessageNoSuchUserExists $ ctxtemplates ctx)
                                   return LoopBack
                       Just user -> do
                                     sendResetPasswordMail user
                                     addFlashMsgText =<< (liftIO $ flashMessageChangePasswordEmailSend $ ctxtemplates ctx)
                                     return LinkForgotPasswordDone
      Nothing -> return LoopBack

{- |
   Sends a password reset mail
-}
sendResetPasswordMail::User -> Kontra ()
sendResetPasswordMail user = do
                         ctx <- get
                         chpwdlink <- liftIO $ unloggedActionLink user
                         mail <-liftIO $ UserView.resetPasswordMail (ctxtemplates ctx) (ctxhostpart ctx) user chpwdlink     
                         liftIO $ sendMail (ctxmailer ctx) $ mail { fullnameemails = [((userfullname user), (unEmail $ useremail $ userinfo user))]}    

{- |
   Handles viewing of the password reset confirmation page
-}   
forgotPasswordDonePage :: Kontra Response
forgotPasswordDonePage = do
    ctx <- lift get
    content <- liftIO $ V.pageForgotPasswordConfirm (ctxtemplates ctx)
    V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata content

{- |
   Handles viewing of the signup page
-}
signupPageGet :: Kontra Response
signupPageGet = do
    ctx <- lift get
    content <- liftIO (signupPageView $ ctxtemplates ctx)
    V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata content 
    
signupPagePost :: Kontra KontraLink
signupPagePost = signup False $ parseMinutesTimeMDY "31-05-2011"
                    

signupVipPagePost :: Kontra KontraLink
signupVipPagePost = signup True $ parseMinutesTimeMDY "31-12-2011"
                   
signup::Bool -> (Maybe MinutesTime) -> Kontra KontraLink
signup vip freetill =  do 
                ctx@Context{ctxtemplates,ctxhostpart} <- lift get
                memail <- fmap (fmap (BS.fromString)) $ getField "email"
                case memail of
                   Nothing -> return LoopBack
                   Just email -> do
                        muser <- query $ GetUserByEmail $ Email $ email
                        case  muser of
                          Just user -> if (isNothing $ userhasacceptedtermsofservice user) 
                                        then  
                                         do  al <- liftIO $ unloggedActionLink user
                                             mail <-  liftIO $ newUserMail (ctxtemplates) (ctxhostpart) email email al
                                             liftIO $ sendMail (ctxmailer ctx) $ mail { fullnameemails = [(email,email)]}
                                             addFlashMsgText =<< (liftIO $ flashMessageNewActivationLinkSend  (ctxtemplates)) 
                                             return LoopBack
                                          else do
                                             addFlashMsgText =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
                                             return LoopBack
                          Nothing -> do
                            maccount <- liftIO $ UserControl.createUser ctx ctxhostpart BS.empty email Nothing True Nothing
                            if isJust maccount       
                             then do
                              addFlashMsgText =<< (liftIO $ flashMessageUserSignupDone ctxtemplates)
                              return LinkSignup
                             else do
                              addFlashMsgText =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
                              return LinkSignup
                    
{- |
   Handles viewing of the signup confirmation page
-}
signupPageDone :: Kontra Response
signupPageDone = do
  ctx <- get
  V.renderFromBody ctx V.TopNone V.kontrakcja signupConfirmPageView

{- |
   Handles viewing of the login page
-}
handleLoginGet :: Kontra Response
handleLoginGet = do
  ctx <- lift get
  case (ctxmaybeuser ctx) of
    Just _ -> sendRedirect LinkMain   
    Nothing -> do 
      referer <- getField "referer"
      content <- liftIO $ V.pageLogin ctx referer
      V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata $ content

{- |
   Handles submission of a login form.  On failure will redirect back to referer, if there is one.
-}  
handleLoginPost :: Kontra KontraLink
handleLoginPost = do
  email <- getDataFnM $ look "email"
  passwd <- getDataFnM $ look "password"
  whereToGoOnFail <- fmap (maybe LinkLogin $ const LoopBack) $ getField "referer"
  
  -- check the user things here
  maybeuser <- query $ GetUserByEmail (Email $ BS.fromString email)
  case maybeuser of
    Just User{userpassword} ->
        if verifyPassword userpassword (BS.fromString passwd) && passwd/=""
        then do
          logUserToContext maybeuser
          return BackToReferer
        else do
          return whereToGoOnFail
    Nothing -> do
          return whereToGoOnFail

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
         
                   ms <- liftIO $ catch (fmap Just ( BS.readFile $ "html/"++fileName)) (const $ return Nothing)
                   case ms of 
                    Just s -> do 
                               nocolumns  <- getField "nocolumns"
                               content <- liftIO $ staticTemplate ctx (isJust nocolumns) (BS.toString s)
                               simpleResponse  content 
                    _ -> mzero
               
         else mzero

{- |
   Ensures logged in as a super user
-}
onlySuperUserGet :: Kontra Response -> Kontra Response  
onlySuperUserGet action = do
  Context{ctxmaybeuser} <- get 
  if isSuperUser ctxmaybeuser 
   then action
   else sendRedirect LinkLogin

{- |
   Used by super users to inspect a particular document.
-}
daveDocument :: DocumentID -> Kontra Response
daveDocument documentid = onlySuperUserGet $ do
      ctx <- get
      mdocument <- query $ GetDocumentByDocumentID documentid
      case mdocument of
        Nothing -> mzero
        Just document ->
          V.renderFromBody ctx V.TopNone V.kontrakcja $ inspectXML document

{- |
   Used by super users to inspect a particular user.
-}
daveUser :: UserID -> Kontra Response
daveUser userid = onlySuperUserGet $ do 
      ctx <- get
      muser <- query $ GetUserByUserID userid
      case muser of
        Nothing -> mzero
        Just user ->
          V.renderFromBody ctx V.TopNone V.kontrakcja $ inspectXML user




hpost0 action = methodM POST >> do
                  (link :: KontraLink) <- action
                  sendRedirect link

hpost1 action = path $ \a1 -> methodM POST >>  do
                  (link :: KontraLink) <- action a1
                  sendRedirect link

hpost2 action = path $ \a1 -> path $ \a2 -> methodM POST >>  do
                  (link :: KontraLink) <- action a1 a2
                  sendRedirect link

hpost3 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> methodM POST >>  do
                  (link :: KontraLink) <- action a1 a2 a3
                  sendRedirect link

hget0 action = methodM GET >> action
hget1 action = path $ \a1 -> methodM GET >> action a1
hget2 action = path $ \a1 -> path $ \a2 -> methodM GET >> action a1 a2
hget3 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> methodM GET >> action a1 a2 a3

{-|
  Version supporting optional path param
  Usually when we want to have similar path when looking at list and when looking at element

 -} 
hget1m::(Maybe String -> Kontra Response)->Kontra Response 
hget1m action = (path $ \a1 -> methodM GET >> (action $ Just a1)) `mplus`  (methodM GET >> action Nothing)

{- |
   Guard on the existence of a parameter.
 -}
param :: String -> Kontra Response -> Kontra Response
param p action = (getDataFnM $ look p) >> action -- will mzero if parameter is not found

