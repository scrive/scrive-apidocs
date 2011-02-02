module AppControl
    ( appHandler
    , AppConf(..)
    , AppGlobals(..)
    , defaultAWSAction
    ) where

import ELegitimation.BankID as BankID
import Control.Monad (msum, mzero, liftM)
import Control.Monad.Reader (ask)
import Control.Monad.State
import Control.Monad.Trans(liftIO, MonadIO,lift)
import AppState
import AppView as V
import Control.Concurrent
import Data.ByteString.Char8 (ByteString)
import Data.List
import Data.Maybe
import Debug.Trace
import DocState
import qualified DocView as V
import HSP.XML
import Happstack.Data.IxSet ((@=),getOne,size)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.HTTP.FileServe
import Happstack.State (update,query)
import Happstack.Util.Common
import InspectXML
import InspectXMLInstances
import KontraLink
import MinutesTime
import Misc
import Network.Socket
import Session
import System.Directory
import System.IO
import System.IO.Unsafe
import System.Random
import User
import UserControl
import UserView as UserView
import UserState
import qualified UserView as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as Set
import qualified DocControl as DocControl
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

data AppGlobals 
    = AppGlobals { templates       :: KontrakcjaTemplates
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
     , dir "landpage" $ dir "saved"      $ hget2 $ DocControl.landpageSaved --got to check this I belive it is not used. MR
           
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
     , dir "adminonly" $ dir "become"            $ hpost0 $ Administration.handleBecome
     , dir "adminonly" $ dir "takeoverdocuments" $ hpost0 $ Administration.handleTakeOverDocuments
     , dir "adminonly" $ dir "deleteaccount"     $ hpost0 $ Administration.handleDeleteAccount
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
     , dir "amnesia"     $ hget0  $ forgotPasswordPageGet
     , dir "amnesia"     $ hpost0 $ forgotPasswordPagePost
     , dir "amnesiadone" $ hget0  $ forgotPasswordDonePage
     , dir "unlogged"    $ hget2  $ UserControl.unloggedActionPage
     , dir "unlogged"    $ hpost2  $ UserControl.handleUnloggedAction

     -- viral invite
     , dir "invite"      $ hpost0 $ UserControl.handleViralInvite
     
     -- e-legitimation stuff
     {- Disabled until finished
     , dir "bankid" $ dir "s" $ hget3  $ BankID.handleSign
     , dir "bankid" $ dir "d" $ hget1  $ BankID.handleIssue
     , dir "s" $ param "bankid" $ hpost3 $ BankID.handleSignPost
     , dir "d" $ param "bankid" $ hpost1 $ BankID.handleIssuePost
     -}

     -- static files
     , serveHTMLFiles
     , fileServe [] "public"
               ]

{-

This is example of how to use heist. Let it be a comment until we decide either 
we want it or remove from this file.

Needed because Heist uses transformers rather than the old mtl package.

import Text.Templating.Heist
import Text.Templating.Heist.TemplateDirectory
import qualified "monads-fd" Control.Monad.Trans as TRA

instance (MonadIO m) => TRA.MonadIO (ServerPartT m) 
    where liftIO = liftIO

   dir "heist" $ path $ \name -> do
         td <- liftIO $ newTemplateDirectory' "tpl" emptyTemplateState
         let template = BS.fromString name
         ts    <- liftIO $ getDirectoryTS td
         bytes <- renderTemplate ts template
         flip (maybe mzero) bytes $ \x -> do
              return (toResponseBS (BS.fromString "text/html; charset=utf-8") (L.fromChunks [x]))
-}

handleHomepage = do
  ctx@Context{ctxmaybeuser, ctxtemplates} <- get
  case ctxmaybeuser of
    Just user -> checkUserTOSGet $ do
                       content <- liftIO $ V.uploadPage ctxtemplates 
                       V.renderFromBody ctx V.TopNew V.kontrakcja (cdata content)
    Nothing -> do
      text <- liftIO $ renderTemplate ctxtemplates "firstPageContent" [("signuplink",show LinkSignup)]
      V.renderFromBody ctx V.TopNone V.kontrakcja (cdata text)

-- uh uh, how to do that in correct way?
normalizeddocuments :: MVar (Map.Map FileID JpegPages)
normalizeddocuments = unsafePerformIO $ newMVar Map.empty

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

appHandler :: AppConf -> AppGlobals -> ServerPartT IO Response
appHandler appConf appGlobals= do
  rq <- askRq
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
                 SockAddrInet port hostip -> hostip
                 _ -> 0
    
  let peer = rqPeer rq
 

  minutestime <- liftIO $ getMinutesTime
  session <- handleSession
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
            }
  (res,ctx)<- toIO ctx $  
     do
      res <- (handleRoutes) `mplus` do
         liftIO $ logM "Happstack.Server" ERROR $ "ERROR" ++ (showDateMDY minutestime)++" "++(rqUri rq) ++" "++(show rq)
         response <- V.renderFromBody ctx V.TopNone V.kontrakcja (fmap cdata $ renderTemplate (ctxtemplates ctx) "errorPage" [])
         setRsCode 404 response     
      ctx <- get 
      return (res,ctx)   
      
  let newsessionuser = fmap userid $ ctxmaybeuser ctx  
  let newflashmessages = ctxflashmessages ctx
  let newelegtrans = ctxelegtransactions ctx
  updateSessionWithContextData session newsessionuser newflashmessages newelegtrans
  return res
      

forgotPasswordPageGet :: Kontra Response
forgotPasswordPageGet = do
    ctx <- lift get
    content <- liftIO $ V.pageForgotPassword (ctxtemplates ctx)
    V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata content
    
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

sendResetPasswordMail::User -> Kontra ()
sendResetPasswordMail user = do
                         ctx <- get
                         chpwdlink <- liftIO $ unloggedActionLink user
                         mail <-liftIO $ UserView.resetPasswordMail (ctxtemplates ctx) (ctxhostpart ctx) user chpwdlink     
                         liftIO $ sendMail (ctxmailer ctx) $ mail { fullnameemails = [((userfullname user), (unEmail $ useremail $ userinfo user))]}    
                                                        
forgotPasswordDonePage :: Kontra Response
forgotPasswordDonePage = do
    ctx <- lift get
    content <- liftIO $ V.pageForgotPasswordConfirm (ctxtemplates ctx)
    V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata content

signupPageGet :: Kontra Response
signupPageGet = do
    ctx <- lift get
    content <- liftIO (signupPageView $ ctxtemplates ctx)
    V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata content 
    
signupPagePost :: Kontra KontraLink
signupPagePost = do
    ctx@Context{ctxtemplates,ctxhostpart} <- lift get
    memail <- getField "email"
    case memail of
        Nothing ->
            -- V.renderFromBody ctx V.TopNone V.kontrakcja (signupPageView Nothing)
            return LinkSignup
        Just email -> do
                    maccount <- liftIO $ createUser ctx ctxhostpart BS.empty (BS.fromString email) Nothing True Nothing
                    if isJust maccount       
                     then return LinkSignupDone
                     else do
                          addFlashMsgText =<< (liftIO $ flashMessageUserWithSameEmailExists ctxtemplates)
                          return LinkSignup
                    

signupPageDone :: Kontra Response
signupPageDone = do
  ctx <- get
  V.renderFromBody ctx V.TopNone V.kontrakcja signupConfirmPageView

handleLoginGet :: Kontra Response
handleLoginGet = do
  ctx <- lift get
  case (ctxmaybeuser ctx) of
    Just _ -> sendRedirect LinkMain   
    Nothing -> do 
      referer <- getField "referer"
      content <- liftIO $ V.pageLogin ctx referer
      V.renderFromBody ctx V.TopNone V.kontrakcja $ cdata $ content
    
handleLoginPost :: Kontra KontraLink
handleLoginPost = do
  rq <- askRq
  email <- getDataFnM $ look "email"
  passwd <- getDataFnM $ look "password"
  rememberMeMaybe <- getDataFn' $ look "rememberme"
  whereToGoOnFail <- fmap (maybe LinkLogin $ const LoopBack) $ getField "referer"
  let rememberMe = isJust rememberMeMaybe
  
  -- check the user things here
  maybeuser <- query $ GetUserByEmail (Email $ BS.fromString email)
  case maybeuser of
    Just user@User{userpassword} ->
        if verifyPassword userpassword (BS.fromString passwd) && passwd/=""
        then do
          logUserToContext maybeuser
          return BackToReferer
        else do
          return whereToGoOnFail
    Nothing -> do
          return whereToGoOnFail

handleLogout :: Kontra Response
handleLogout = do
  logUserToContext Nothing
  sendRedirect LinkMain
  
serveHTMLFiles:: Kontra Response  
serveHTMLFiles =  do
        ctx <- get
        rq <- askRq
        let fileName = last (rqPaths rq)
        if ((length (rqPaths rq) > 0) && isSuffixOf ".html" fileName)
         then do
         
                   ms <- liftIO $ catch (fmap Just ( BS.readFile $ "html/"++fileName)) (const $ return Nothing)
                   case ms of 
                    Just s -> V.renderFromBody ctx V.TopNone V.kontrakcja (cdata $ BS.toString $ s)
                    _ -> mzero
               
         else mzero
      
onlySuperUserGet :: Kontra Response -> Kontra Response  
onlySuperUserGet action = do
  Context{ctxmaybeuser} <- get 
  if isSuperUser ctxmaybeuser 
   then action
   else sendRedirect LinkLogin

onlySuperUserPost :: Kontra KontraLink -> Kontra KontraLink
onlySuperUserPost action = do
  Context{ctxmaybeuser} <- get 
  if isSuperUser ctxmaybeuser 
   then action
   else return LinkLogin

daveDocument :: DocumentID -> Kontra Response
daveDocument documentid = onlySuperUserGet $ do
      ctx <- get
      mdocument <- query $ GetDocumentByDocumentID documentid
      case mdocument of
        Nothing -> mzero
        Just document ->
          V.renderFromBody ctx V.TopNone V.kontrakcja $ inspectXML document

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

hpost4 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> path $ \a4 -> methodM POST >>  do
                  (link :: KontraLink) <- action a1 a2 a3 a4
                  sendRedirect link

hget0 action = methodM GET >> action
hget1 action = path $ \a1 -> methodM GET >> action a1
hget2 action = path $ \a1 -> path $ \a2 -> methodM GET >> action a1 a2
hget3 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> methodM GET >> action a1 a2 a3
hget4 action = path $ \a1 -> path $ \a2 -> path $ \a3 -> path $ \a4 -> methodM GET >> action a1 a2 a3 a4

{-Version supporting optional path param
  Usualy when we want ta have similar path when looking at list and when looking at element

 -} 
hget1m::(Maybe String -> Kontra Response)->Kontra Response 
hget1m action = (path $ \a1 -> methodM GET >> (action $ Just a1)) `mplus`  (methodM GET >> action Nothing)

{- |
   Guard on the existence of a parameter.
 -}
param :: String -> Kontra Response -> Kontra Response
param p action = do
  getDataFnM $ look p -- will mzero if parameter is not found
  action

