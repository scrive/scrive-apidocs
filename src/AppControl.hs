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
import AppView as V
import Control.Concurrent
import Data.ByteString.Char8 (ByteString)
import Data.List
import Data.Maybe
import Data.Object
import Debug.Trace
import DocState
import qualified DocView as V
import HSP.XML
import Happstack.Data.IxSet ((@=),getOne,size)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.HTTP.FileServe
import Happstack.Server.SimpleHTTP (seeOther)
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
import System.Process
import System.Random
import User
import UserControl
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

{- |
   The routing table for the app.
   Routes in this table should be of the form
   dir "segment1" $ dir "segment2" $ .. $ dir "segmentn" $ hgetx $ handler
   OR
   dir "segment1" $ dir "segment2" $ .. $ dir "segmentn" $ hpostx $ handler

   No other logic should be in here and no similar logic should be in the handler.
   That is, all routing logic should be in this table to ensure that we can find
   the function for any given path and method.
-}
handleRoutes :: Kontra Response
handleRoutes = do
   ctx@Context{ctxmaybeuser,ctxnormalizeddocuments} <- get 
   msum $
     [ hget0 $ handleHomepage

     , dir "s" $ hget0  $ DocControl.handleSTable
     , dir "s" $ hget3  $ DocControl.handleSignShow
     , dir "s" $ hpost3 $ DocControl.handleSignPost

     -- OLD
     -- Question: Do we still need these?
     , dir "sign" $ hget0  $ DocControl.handleSTable
     , dir "sign" $ hget3  $ DocControl.handleSignShow
     , dir "sign" $ hpost3 $ DocControl.handleSignPost

     , dir "d" $ hget0  $ DocControl.handleIssueGet
     , dir "d" $ hget1  $ DocControl.handleIssueShowGet
     , dir "d" $ hget2  $ DocControl.handleIssueShowTitleGet
     , dir "d" $ hpost0 $ DocControl.handleIssuePost
     , dir "d" $ hpost1 $ DocControl.handleIssueShowPost

     -- OLD
     -- Question: Do we still need these?
     , dir "issue" $ hget0  $ DocControl.handleIssueGet
     , dir "issue" $ hget1  $ DocControl.handleIssueShowGet
     , dir "issue" $ hget2  $ DocControl.handleIssueShowTitleGet
     , dir "issue" $ hpost0 $ DocControl.handleIssuePost
     , dir "issue" $ hpost1 $ DocControl.handleIssueShowPost

     , dir "resend" $ hpost2 $ DocControl.handleResend

     , dir "pages"  $ hget2  $ DocControl.showPage

     , dir "landpage" $ dir "signinvite" $ hget1 $ DocControl.landpageSignInvite
     , dir "landpage" $ dir "signed"     $ hget2 $ DocControl.landpageSigned 
     , dir "landpage" $ dir "rejected"   $ hget2 $ DocControl.landpageRejected
     , dir "landpage" $ dir "signedsave" $ hget2 $ DocControl.landpageSignedSave
     , dir "landpage" $ dir "saved"      $ hget2 $ DocControl.landpageSaved
           
     , dir "pagesofdoc" $ hget1 $ DocControl.handlePageOfDocument
     , dir "account" $ UserControl.handleUser ctx

     , dir "accepttos" $ hget0 $ UserControl.handleAcceptTOSGet ctx
     , dir "accepttos" $ hpost0 $ UserControl.handleAcceptTOSPost ctx


     -- super user only
     , dir "stats" handleStats
     , dir "createuser" handleCreateUser
     , dir "adminonly" $ nullDir >> AppControl.showAdminOnly
     , dir "adminonly" $ dir "db" $ nullDir >> indexDB
     , dir "adminonly" $ dir "db" $ fileServe [] "_local/kontrakcja_state"
     , dir "adminonly" $ dir "cleanup" $ databaseCleanup
     , dir "adminonly" $ dir "become" $ handleBecome
     , dir "adminonly" $ dir "takeoverdocuments" $ handleTakeOverDocuments
     , dir "adminonly" $ dir "deleteaccount" $ handleDeleteAccount
     , dir "adminonly" $ dir "alluserstable" $ handleAllUsersTable
     , dir "adminonly" $ dir "skrivapausers.csv" $ methodM GET >> getUsersDetailsToCSV
     , dir "dave" $ dir "document" $ daveDocument
     , dir "dave" $ dir "user" $ daveUser
           
     -- account stuff
     , dir "logout" handleLogout
     , dir "login" handleLogin
     , dir "signup" signupPage
     , dir "signupdone" signupPageDone
     , dir "amnesia" forgotPasswordPage
     , dir "amnesiadone" forgotPasswordDonePage
     
     -- static files
     , serveHTMLFiles
     , fileServe [] "public"]

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
  ctx@Context{ctxmaybeuser} <- get
  case ctxmaybeuser of
    Just user -> checkUserTOSGet $
      V.renderFromBody ctx V.TopNew V.kontrakcja (V.pageWelcome ctx)
    Nothing ->
      V.renderFromBody ctx V.TopNew V.kontrakcja (V.pageWelcome ctx)


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
      res <- (handleRoutes) `mplus` do
         response <- V.renderFromBody ctx V.TopNone V.kontrakcja (V.pageErrorReport ctx rq)
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
    V.renderFromBody ctx V.TopNone V.kontrakcja V.pageForgotPassword
    
forgotPasswordPagePost :: Kontra KontraLink
forgotPasswordPagePost = do
    ctx@Context{..} <- lift get
    email <- getDataFnM $ look "email"
    liftIO $ resetUserPassword ctxhostpart (BS.fromString email)
    return LinkForgotPasswordDone

forgotPasswordDonePage :: Kontra Response
forgotPasswordDonePage = do
    ctx <- lift get
    V.renderFromBody ctx V.TopNone V.kontrakcja (V.pageForgotPasswordConfirm ctx)

signupPage :: Kontra Response
signupPage = (hget0 signupPageGet) `mplus`
             (hpost0 signupPagePost)
             
signupPageGet :: Kontra Response
signupPageGet = do
    ctx <- lift get
    V.renderFromBody ctx V.TopNone V.kontrakcja (signupPageView Nothing)

signupPageError :: SignupForm -> Maybe String
signupPageError form
    | signupEmail form == BS.empty = Just "You must enter an email address"
    | signupPassword form /= signupPassword2 form = Just "Passwords must match"
    | not $ isPasswordStrong $ signupPassword form = Just "Passwords must be at least 6 characters"
    | otherwise = Nothing
    
signupPagePost :: Kontra KontraLink
signupPagePost = do
    ctx@Context{..} <- lift get
    maybeform <- getData
    
    case maybeform of
        Nothing ->
            -- V.renderFromBody ctx V.TopNone V.kontrakcja (signupPageView Nothing)
            return LinkSignup
        Just form -> do
            case signupPageError form of
                Just err -> do
                       addFlashMsgText (BS.fromString err)
                       -- V.renderFromBody ctx V.TopNone V.kontrakcja (signupPageView maybeform)
                       return LinkSignup
                Nothing -> do
                    -- Create the user, which sends them a welcome email.
                    account <- liftIO $ createUser ctxhostpart (signupFullname form) (signupEmail form) (Just (signupPassword form)) Nothing
                    return LinkSignupDone

signupPageDone :: Kontra Response
signupPageDone = do
  ctx <- get
  V.renderFromBody ctx V.TopNone V.kontrakcja (signupConfirmPageView ctx)

handleLogin :: Kontra Response
handleLogin = (methodM GET >> handleLoginGet) `mplus` 
            (methodM POST >> handleLoginPost)

handleLoginGet :: Kontra Response
handleLoginGet = do
  ctx <- lift get
  V.renderFromBody ctx V.TopNone V.kontrakcja (V.pageLogin ctx)

handleLoginPost :: Kontra Response
handleLoginPost = do
  rq <- askRq
  email <- getDataFnM $ look "email"
  passwd <- getDataFnM $ look "password"
  rememberMeMaybe <- getDataFn' $ look "rememberme"
  let rememberMe = isJust rememberMeMaybe
  
  -- check the user things here
  maybeuser <- query $ GetUserByEmail (Email $ BS.fromString email)
  case maybeuser of
    Just user@User{userpassword} ->
        if verifyPassword userpassword (BS.fromString passwd) && passwd/=""
        then do
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


handleStats :: Kontra Response
handleStats = onlySuperUserGet $
  do
    ndocuments <- query $ GetDocumentStats
    allusers <- query $ GetAllUsers
#ifndef WINDOWS
    df <- liftIO read_df
#else
    let df = BS.empty
#endif
    webHSP (V.pageStats (length allusers) ndocuments allusers df)
    

handleBecome :: Kontra Response
handleBecome = onlySuperUserGet $
  do
    methodM POST
    (userid :: UserID) <- getDataFnM $ (look "user" >>= readM)
    user <- liftM query GetUserByUserID userid
    logUserToContext user
    response <- webHSP (seeOtherXML "/")
    seeOther "/" response



handleCreateUser :: Kontra Response
handleCreateUser = onlySuperUserGet $
  do
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
indexDB = onlySuperUserGet $
  do
    contents <- liftIO $ getDirectoryContents "_local/kontrakcja_state"
    webHSP (V.databaseContents (sort contents))

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
databaseCleanup =   onlySuperUserGet $
  do
    -- dangerous, cleanup all old files, where old means chechpoints but the last one
    -- and all events that have numbers less than last checkpoint
    methodM POST
    contents <- liftIO databaseCleanupWorker
    webHSP (V.databaseContents (sort contents))


showAdminOnly :: Kontra Response
showAdminOnly = onlySuperUserGet $
  do
    methodM GET
    ctx@Context { ctxflashmessages} <- lift get
    users <- query $ GetAllUsers
    webHSP (V.pageAdminOnly users ctxflashmessages)
  

handleTakeOverDocuments :: Kontra Response
handleTakeOverDocuments = onlySuperUserGet $
  do
    methodM POST
    ctx@Context{ctxmaybeuser = Just ctxuser} <- lift $ get
    (srcuserid :: UserID) <- getDataFnM $ (look "user" >>= readM)
    Just srcuser <- query $ GetUserByUserID srcuserid
  
    update $ FragileTakeOverDocuments (userid ctxuser) srcuserid
    addFlashMsgText $ BS.fromString $ "Took over all documents of '" ++ BS.toString (userfullname srcuser) ++ "'. His account is now empty and can be deleted if you wish so. Show some mercy, though."
    let link = "/adminonly/"
    response <- webHSP (seeOtherXML link)
    seeOther link response
    


handleDeleteAccount :: Kontra Response
handleDeleteAccount = onlySuperUserGet $
  do
    methodM POST
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
handleAllUsersTable = onlySuperUserGet $
  do
    methodM GET
    users <- query $ GetAllUsers
    let queryNumberOfDocuments user = do
          documents <- query $ GetDocumentsByAuthor (userid user)
          return (user,length documents)
                        
    users2 <- mapM queryNumberOfDocuments users

    webHSP $ V.pageAllUsersTable users2

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

getUsersDetailsToCSV :: Kontra Response
getUsersDetailsToCSV = do
  x <- query $ ExportUsersDetailsToCSV
  let response = toResponseBS (B.pack "text/csv") $ L.fromChunks [B.unlines x]
  return response
      
onlySuperUserGet :: Kontra Response -> Kontra Response  
onlySuperUserGet action = do
  Context{ctxmaybeuser} <- get 
  if isSuperUser ctxmaybeuser 
   then action
   else sendRedirect LinkLogin

daveDocument :: Kontra Response
daveDocument = onlySuperUserGet $
    do
      ctx <- get
      pathdb GetDocumentByDocumentID $ \document ->
          V.renderFromBody ctx V.TopNew V.kontrakcja $ inspectXML document

daveUser :: Kontra Response
daveUser = onlySuperUserGet $ 
    do 
      ctx <- get
      pathdb GetUserByUserID $ \user ->
          V.renderFromBody ctx V.TopNew V.kontrakcja $ inspectXML user

