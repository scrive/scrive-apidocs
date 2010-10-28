{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             NamedFieldPuns, ScopedTypeVariables, CPP, RecordWildCards,
             PackageImports
 #-}
module AppControl where

import "base" Control.Monad (msum, mzero, liftM)
import "mtl" Control.Monad.Reader (ask)
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans(liftIO, MonadIO,lift)
import AppState
import AppView
import Control.Concurrent
import Data.ByteString.Char8 (ByteString)
import Data.List
import Data.Maybe
import Data.Object
import Debug.Trace
import DocState
import DocView
import HSP.XML
import Happstack.Data.IxSet ((@=),getOne,size)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.HTTP.FileServe
import Happstack.Server.SimpleHTTP (seeOther)
import Happstack.State (update,query)
import Happstack.Util.Common
import InspectXML
import KontraLink
import MinutesTime
import Misc
import Network.Socket
import Session
import System.Directory
import System.IO
import System.IO.Unsafe
import System.Process
import System.Random
import User
import UserControl
import UserState
import UserView
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as Set
import qualified DocControl as DocControl
import qualified DocView as DocView
import qualified HSP as HSP
import qualified Data.Map as Map

handleRoutes =  
  do
   ctx@Context{ctxmaybeuser,ctxnormalizeddocuments} <- get 
   msum $
    ([nullDir >> if isJust ctxmaybeuser
              then withUserTOS (renderFromBody ctx TopNew kontrakcja (welcomeBody ctx))
              else renderFromBody ctx TopNew kontrakcja (welcomeBody ctx)
     , dir "s" $ DocControl.handleSign
     , {- old -} dir "sign" $ withUserTOS $ DocControl.handleSign
     , dir "d" $ withUserTOS $ DocControl.handleIssue
     , {- old -} dir "issue" $ withUserTOS $ DocControl.handleIssue
     , dir "resend" $ withUserTOS $ DocControl.handleResend
     , dir "pages" $ hget2 $ \fileid pageno -> do
        modminutes <- query $ FileModTime fileid
        DocControl.showPage modminutes fileid pageno
                                                    
     , dir "landpage" $ msum 
               [ dir "signinvite" $ pathdb GetDocumentByDocumentID $ \document -> 
                     DocControl.landpageSignInvite ctx document
               , dir "signed" $ pathdb GetDocumentByDocumentID $ \document -> path $ \signatorylinkid ->
                                                                              DocControl.landpageSigned ctx document signatorylinkid
               , dir "rejected" $ pathdb GetDocumentByDocumentID $ \document -> path $ \signatorylinkid ->
                                                                              DocControl.landpageRejected ctx document signatorylinkid
               , dir "signedsave" $ pathdb GetDocumentByDocumentID $ \document -> 
                   path $ \signatorylinkid ->
                       DocControl.landpageSignedSave ctx document signatorylinkid
               , dir "saved" $ withUser $ pathdb GetDocumentByDocumentID $ \document -> 
                   path $ \signatorylinkid ->
                       DocControl.landpageSaved ctx document signatorylinkid
               ]
           
     , dir "pagesofdoc" $ 
           pathdb GetDocumentByDocumentID $ \document -> 
               DocControl.handlePageOfDocument document
     , dir "account" $ withUser $ UserControl.handleUser ctx
     ]
     
     ++ (if isSuperUser ctxmaybeuser then 
             [ dir "stats" $ statsPage
             , dir "createuser" $ handleCreateUser
             , dir "adminonly" $ msum 
                       [ methodM GET >> AppControl.showAdminOnly
                       , dir "db" $ msum [ methodM GET >> indexDB
                                         , fileServe [] "_local/kontrakcja_state"
                                         ]
                       , dir "cleanup" $ methodM POST >> databaseCleanup
                       , dir "become" $ methodM POST >> handleBecome
                       , dir "takeoverdocuments" $ methodM POST >> handleTakeOverDocuments
                       , dir "deleteaccount" $ methodM POST >> handleDeleteAccount
                       , dir "alluserstable" $ methodM GET >> handleAllUsersTable
                       ]
             , dir "dave" $ msum
                   [ dir "document" $ pathdb GetDocumentByDocumentID $ \document ->
                        renderFromBody ctx TopNew kontrakcja $ inspectXML document
                   , dir "user" $ pathdb GetUserByUserID $ \user ->
                       renderFromBody ctx TopNew kontrakcja $ inspectXML user
                   ]
             ]
         else []))
     ++ 
     [ dir "logout" handleLogout
     , dir "login" loginPage
     -- , dir "signup" signupPage
     , dir "amnesia" forgotPasswordPage
     , dir "amnesiadone" forgotPasswordDonePage
     ]
     ++ [serveHTMLFiles, fileServe [] "public"] 

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


-- uh uh, how to do that in correct way?
normalizeddocuments :: MVar (Map.Map FileID JpegPages)
normalizeddocuments = unsafePerformIO $ newMVar Map.empty

appHandler :: ServerPartT IO Response
appHandler = do
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
  liftIO $ print (peer,peerip)
  minutestime <- liftIO $ getMinutesTime
  session <- handleSession
  muser <- getUserFromSession session
  flashmessages <- getFlashMessagesFromSession session          
  let 
   ctx = Context
            { ctxmaybeuser = muser
            , ctxhostpart = hostpart
            , ctxflashmessages = flashmessages
            , ctxtime = minutestime
            , ctxnormalizeddocuments = normalizeddocuments
            , ctxipnumber = peerip
            }
  (res,ctx)<- toIO ctx $  
     do
      userLogin
      res <- (handleRoutes) `mplus` do
         response <- renderFromBody ctx TopNone kontrakcja (errorReport ctx rq)
         setRsCode 404 response     
      ctx <- get 
      return (res,ctx)   
      
  let newsessionuser = fmap userid $ ctxmaybeuser ctx  
  let newflashmessages = ctxflashmessages ctx
  updateSessionWithContextData session newsessionuser newflashmessages
  return res
      

forgotPasswordPage :: Kontra Response
forgotPasswordPage = hget0 forgotPasswordPageGet `mplus`
                     hpost0 forgotPasswordPagePost

forgotPasswordPageGet :: Kontra Response
forgotPasswordPageGet = do
    ctx <- lift get
    renderFromBody ctx TopNone kontrakcja forgotPasswordPageView
    
forgotPasswordPagePost :: Kontra KontraLink
forgotPasswordPagePost = do
    ctx@Context{..} <- lift get
    email <- getDataFnM $ look "email"
    liftIO $ resetUserPassword ctxhostpart (BS.fromString email)
    return LinkForgotPasswordDone
    --renderFromBody ctx TopNone kontrakcja (forgotPasswordConfirmPageView ctx)

forgotPasswordDonePage :: Kontra Response
forgotPasswordDonePage = do
    ctx <- lift get
    renderFromBody ctx TopNone kontrakcja (forgotPasswordConfirmPageView ctx)

signupPage :: Kontra Response
signupPage = (methodM GET >> signupPageGet) `mplus`
             (methodM POST >> signupPagePost)
             
signupPageGet :: Kontra Response
signupPageGet = do
    ctx <- lift get
    renderFromBody ctx TopNone kontrakcja (signupPageView [] Nothing)

signupPageError :: SignupForm -> Maybe String
signupPageError form
    | signupEmail form == "" = Just "You must enter an email address"
    | signupPassword form /= signupPassword2 form = Just "Passwords must match"
    | not $ isPasswordStrong $ BS.fromString $ signupPassword form = Just "Passwords must be at least 6 characters"
    | otherwise = Nothing
    
signupPagePost :: Kontra Response
signupPagePost = do
    ctx@Context{..} <- lift get
    maybeform <- getData
    
    case maybeform of
        Nothing ->
            renderFromBody ctx TopNone kontrakcja (signupPageView [] Nothing)
        Just form -> do
            case signupPageError form of
                Just error -> renderFromBody ctx TopNone kontrakcja (signupPageView [error] maybeform)
                Nothing -> do
                    -- Create the user, which sends them a welcome email.
                    account <- liftIO $ createUser ctxhostpart (BS.fromString ((signupFirstname form) ++ " " ++ (signupLastname form))) (BS.fromString (signupEmail form)) (Just (BS.fromString (signupPassword form))) Nothing
                    renderFromBody ctx TopNone kontrakcja (signupConfirmPageView ctx)

loginPage :: Kontra Response
loginPage = (methodM GET >> loginPageGet) `mplus` 
            (methodM POST >> loginPagePost)

loginPageGet :: Kontra Response
loginPageGet = do
  ctx <- lift get
  renderFromBody ctx TopNone kontrakcja (loginPageView ctx)

loginPagePost :: Kontra Response
loginPagePost = do
  rq <- askRq
  email <- getDataFnM $ look "email"
  passwd <- getDataFnM $ look "password"
  rememberMeMaybe <- getDataFn $ look "rememberme"
  let rememberMe = isJust rememberMeMaybe
  
  -- check the user things here
  maybeuser <- query $ GetUserByEmail (Email $ BS.fromString email)
  case maybeuser of
    Just user@User{userpassword} ->
        if verifyPassword userpassword (BS.fromString passwd) && passwd/=""
        then do
          setRememberMeCookie (userid user) rememberMe
          logUserToContext maybeuser
          response <- webHSP (seeOtherXML "/")
          seeOther "/" response
        else do
          response <- webHSP (seeOtherXML "/login")
          seeOther "/login" response
    Nothing -> do
          response <- webHSP (seeOtherXML "/login")
          seeOther "/login" response

handleLogout :: Kontra Response
handleLogout = do
  logUserToContext Nothing
  clearRememberMeCookie
  response <- webHSP (seeOtherXML "/")
  seeOther "/" response


#ifndef WINDOWS
read_df = do
  (_,Just handle_out,_,handle_process) <-
      createProcess (proc "df" []) { std_out = CreatePipe, env = Just [("LANG","C")] }
  s <- BS.hGetContents handle_out
  hClose handle_out
  waitForProcess handle_process
  return s
#endif


statsPage :: Kontra Response
statsPage = do
  ndocuments <- query $ GetDocumentStats
  allusers <- query $ GetAllUsers
#ifndef WINDOWS
  df <- liftIO read_df
#else
  let df = BS.empty
#endif
  webHSP (statsPageView (length allusers) ndocuments allusers df)
    

handleBecome :: Kontra Response
handleBecome = do
  (userid :: UserID) <- getDataFnM $ (look "user" >>= readM)
  user <- liftM query GetUserByUserID userid
  logUserToContext user
  response <- webHSP (seeOtherXML "/")
  seeOther "/" response



handleCreateUser :: Kontra Response
handleCreateUser = do
  ctx@Context{..} <- get
  email <- g "email"
  fullname <- g "fullname"
  user <- liftIO $ createUser ctxhostpart fullname email Nothing Nothing
  -- FIXME: where to redirect?
  response <- webHSP (seeOtherXML "/stats")
  seeOther "/stats" response
  
handleDownloadDatabase :: Kontra Response
handleDownloadDatabase = do fail "nothing"
  
indexDB :: Kontra Response
indexDB = do
  contents <- liftIO $ getDirectoryContents "_local/kontrakcja_state"
  webHSP (AppView.databaseContents (sort contents))

databaseCleanupWorker :: IO [FilePath]
databaseCleanupWorker = do
  contents <- getDirectoryContents "_local/kontrakcja_state"
  let checkpoints = filter ("checkpoints-" `isPrefixOf`) contents
  let events = filter ("events-" `isPrefixOf`) contents
  let lastcheckpoint = last (sort checkpoints)
  let cutoffevent = "events-" ++ drop 12 lastcheckpoint
  let eventsToRemove = filter (< cutoffevent) events 
  let checkpointsToRemove = filter (< lastcheckpoint) checkpoints
  mapM_ (\x -> removeFile ("_local/kontrakcja_state/" ++ x)) (eventsToRemove ++ checkpointsToRemove)
  getDirectoryContents "_local/kontrakcja_state"

databaseCleanup :: Kontra Response
databaseCleanup = do
  -- dangerous, cleanup all old files, where old means chechpoints but the last one
  -- and all events that have numbers less than last checkpoint
  contents <- liftIO databaseCleanupWorker
  webHSP (AppView.databaseContents (sort contents))


showAdminOnly :: Kontra Response
showAdminOnly = do
  ctx@Context { ctxflashmessages} <- lift get
  users <- query $ GetAllUsers
  webHSP (AppView.showAdminOnly users ctxflashmessages)
  

handleTakeOverDocuments :: Kontra Response
handleTakeOverDocuments = do
  ctx@Context{ctxmaybeuser = Just ctxuser} <- lift $ get
  (srcuserid :: UserID) <- getDataFnM $ (look "user" >>= readM)
  Just srcuser <- query $ GetUserByUserID srcuserid
  
  update $ FragileTakeOverDocuments (userid ctxuser) srcuserid
  addFlashMsgText $ BS.fromString $ "Took over all documents of '" ++ BS.toString (userfullname srcuser) ++ "'. His account is now empty and can be deleted if you wish so. Show some mercy, though."
  let link = "/adminonly/"
  response <- webHSP (seeOtherXML link)
  seeOther link response
    


handleDeleteAccount :: Kontra Response
handleDeleteAccount = do
  (userid :: UserID) <- getDataFnM $ (look "user" >>= readM)
  Just user <- query $ GetUserByUserID userid
  documents <- query $ GetDocumentsByAuthor userid
  if null documents
     then do
       update $ FragileDeleteUser userid
       addFlashMsgText (BS.fromString ("User deleted. You will not see '" ++ BS.toString (userfullname user) ++ "' here anymore"))
     else do
       addFlashMsgText (BS.fromString ("I cannot delete user. '" ++ BS.toString (userfullname user) ++ "' still has " ++ show (length documents) ++ " documents as author. Take over his documents, then try to delete the account again."))
  
  let link = "/adminonly/"
  response <- webHSP (seeOtherXML link)
  seeOther link response

handleAllUsersTable :: Kontra Response
handleAllUsersTable = do
  users <- query $ GetAllUsers
  let queryNumberOfDocuments user = do
                        documents <- query $ GetDocumentsByAuthor (userid user)
                        return (user,length documents)
                        
  users2 <- mapM queryNumberOfDocuments users

  webHSP $ pageAllUsersTable users2

serveHTMLFiles:: Kontra Response  
serveHTMLFiles =  do
        ctx <- get
        rq <- askRq
        let fileName = last (rqPaths rq)
        if ((length (rqPaths rq) > 0) && isSuffixOf ".html" fileName)
         then do
         
                   ms <- liftIO $ catch (fmap Just ( BS.readFile $ "html/"++fileName)) (const $ return Nothing)
                   case ms of 
                    Just s -> renderFromBody ctx TopNone kontrakcja (cdata $ BS.toString $ s)
                    _ -> mzero
               
         else mzero
      
