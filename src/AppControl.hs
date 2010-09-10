{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             NamedFieldPuns, ScopedTypeVariables, CPP
 #-}
module AppControl where

import AppState
import AppView
import Control.Monad(msum,liftM)
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import Data.Object
import Happstack.Data.IxSet ((@=),getOne,size)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Session
import User
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Object.Json as Json
import qualified DocView as DocView
import qualified DocControl as DocControl
import Happstack.Server.SimpleHTTP (seeOther)
import Control.Monad.Reader
import DocState
import UserState
import UserView
import Happstack.Util.Common
import UserControl
import Data.Maybe
import Misc
import MinutesTime
import Control.Monad.State
import DocView
import SendMail
import System.Random
import System.Process
import System.IO
import System.Directory
import Data.List
import KontraLink
import Control.Concurrent
import qualified Data.Set as Set
import System.IO.Unsafe
import Debug.Trace

handleRoutes ctx@Context{ctxmaybeuser,ctxnormalizeddocuments} = toIO ctx $ msum $
    ([nullDir >> withTOS ctx (webHSP (pageFromBody ctx TopNew kontrakcja (welcomeBody ctx)))
     , dir "s" $ withTOS ctx (DocControl.handleSign ctx)
     , {- old -} dir "sign" $ (withTOS ctx (DocControl.handleSign ctx))
     , dir "d" $ withUser ctxmaybeuser (withTOS ctx (DocControl.handleIssue ctx))
     , {- old -} dir "issue" $ withUser ctxmaybeuser (withTOS ctx (DocControl.handleIssue ctx))
     , dir "pages" $ withTOS ctx (path $ \fileid -> 
                                               msum [ path $ \pageno -> do
                                                        modminutes <- query $ FileModTime fileid
                                                        DocControl.showPage ctx modminutes fileid pageno
                                                    ])
     , dir "landpage" $ 
           withTOS ctx
                       (msum [ dir "signinvite" $ pathdb GetDocumentByDocumentID $ \document -> 
                                   DocControl.landpageSignInvite ctx document
                             , dir "signed" $ pathdb GetDocumentByDocumentID $ \document -> path $ \signatorylinkid ->
                                                                                            DocControl.landpageSigned ctx document signatorylinkid
                             , dir "signedsave" $ pathdb GetDocumentByDocumentID $ \document -> 
                                 path $ \signatorylinkid ->
                                     DocControl.landpageSignedSave ctx document signatorylinkid
                             , dir "saved" $ withUser ctxmaybeuser $ pathdb GetDocumentByDocumentID $ \document -> 
                                 path $ \signatorylinkid ->
                                     DocControl.landpageSaved ctx document signatorylinkid
                             ])
           
     , dir "pagesofdoc" $ 
           withTOS ctx (pathdb GetDocumentByDocumentID $ \document -> 
                                     DocControl.handlePageOfDocument ctxnormalizeddocuments document)
     , dir "resendemail" $ 
           withTOS ctx (pathdb GetDocumentByDocumentID $ \document -> 
                                     path $ \signatorylinkid -> 
                                         resendEmail ctx document signatorylinkid)
     , dir "account" (withUser ctxmaybeuser (withTOS ctx (UserControl.handleUser ctx)))]
     
     ++ (if isSuperUser ctxmaybeuser then 
             [ dir "stats" $ statsPage
             , dir "createuser" $ handleCreateUser
             , dir "adminonly" $ msum 
                       [ methodM GET >> AppControl.showAdminOnly
                       , dir "db" $ msum [ methodM GET >> indexDB
                                         , fileServe [] "_local/kontrakcja_state"
                                         ]
                       , dir "cleanup" $ methodM POST >> databaseCleanup
                       , dir "removeimages" $ methodM POST >> databaseRemoveImages
                       , dir "become" $ methodM POST >> handleBecome
                       , dir "takeoverdocuments" $ methodM POST >> handleTakeOverDocuments
                       , dir "deleteaccount" $ methodM POST >> handleDeleteAccount
                       , dir "alluserstable" $ methodM GET >> handleAllUsersTable
                       ]
             ]
         else []))
   ++ [dir "logout" (handleLogout)
      , dir "login" loginPage
      , dir "tos" $ tosPage ctx
      ]
   ++ [ fileServe [] "public"] 

-- uh uh, how to do that in correct way?
normalizeddocuments :: MVar (Set.Set FileID)
normalizeddocuments = unsafePerformIO $ newMVar (Set.empty)

appHandler :: ServerPartT IO Response
appHandler = do
  rq <- askRq
  let host = maybe "skrivapa.se" BS.toString $ getHeader "host" rq
  let scheme = maybe "http" BS.toString $ getHeader "scheme" rq
  let hostpart =  scheme ++ "://" ++ host
  
  maybeuser <- userLogin

  flashmessages <- case maybeuser of
                     Just (User{userid}) -> 
                         liftIO $ update $ GetUserFlashMessages userid
                     Nothing -> return []
  minutestime <- liftIO $ getMinutesTime

  let 
   ctx = Context
            { ctxmaybeuser = maybeuser
            , ctxhostpart = hostpart
            , ctxflashmessages = flashmessages              
            , ctxtime = minutestime
            , ctxnormalizeddocuments = normalizeddocuments
            }

  

  handleRoutes ctx `mplus` do
     response <- webHSP (pageFromBody ctx TopNone kontrakcja (errorReport ctx rq))
     setRsCode 404 response

loginPage :: Kontra Response
loginPage = (methodM GET >> loginPageGet) `mplus` 
            (methodM POST >> loginPagePost)

loginPageGet :: Kontra Response
loginPageGet = do
  ctx <- lift get
  webHSP (pageFromBody ctx TopNone kontrakcja (loginPageView ctx))


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
          sessionid <- update $ NewSession (userid user)
          startSession sessionid
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
  endSession
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
  sessionid <- update $ NewSession userid
  setHeaderM "Set-Cookie" ""
  startSession sessionid
  response <- webHSP (seeOtherXML "/")
  seeOther "/" response



handleCreateUser :: Kontra Response
handleCreateUser = do
  email <- g "email"
  fullname <- g "fullname"
  user <- liftIO $ createUser fullname email Nothing
  let letters =['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
  indexes <- liftIO $ replicateM 8 (randomRIO (0,length letters-1))
  let passwd = BS.fromString $ map (letters!!) indexes
  hashedpassword <- liftIO $ createPassword  passwd
  update $ SetUserPassword user hashedpassword
  content <- liftIO $ passwordChangeMail email fullname passwd
  liftIO $ sendMail [(fullname,email)]
               (BS.fromString "SkrivaPa new password") content BS.empty
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

databaseRemoveImages :: Kontra Response
databaseRemoveImages = do
  update $ FixRemoveImages
  let link = "/adminonly/"
  response <- webHSP (seeOtherXML link)
  seeOther link response
  
  
  
resendEmail :: Context -> Document -> SignatoryLinkID -> Kontra Response
resendEmail ctx document@Document{documentsignatorylinks} signatorylinkid1 = do
  let [invitedlink] = filter (\x -> signatorylinkid x == signatorylinkid1) documentsignatorylinks
  liftIO $ forkIO $ DocControl.sendInvitationEmail1 ctx document invitedlink
  let link = LinkIssueDoc document
  response <- webHSP (seeOtherXML (show link))
  seeOther (show link) response

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
  addFlash $ BS.fromString $ "Took over all documents of '" ++ BS.toString (userfullname srcuser) ++ "'. His account is now empty and can be deleted if you wish so. Show some mercy, though."
  let link = "/adminonly/"
  response <- webHSP (seeOtherXML link)
  seeOther link response
    

             
addFlash :: BS.ByteString -> Kontra ()
addFlash msg = do
  ctx <- lift $ get
  case ctxmaybeuser ctx of
    Just user -> update $ AddUserFlashMessage (userid user) (FlashMessage msg)
    Nothing -> return ()
  


handleDeleteAccount :: Kontra Response
handleDeleteAccount = do
  (userid :: UserID) <- getDataFnM $ (look "user" >>= readM)
  Just user <- query $ GetUserByUserID userid
  documents <- query $ GetDocumentsByAuthor userid
  if null documents
     then do
       update $ FragileDeleteUser userid
       addFlash (BS.fromString ("User deleted. You will not see '" ++ BS.toString (userfullname user) ++ "' here anymore"))
     else do
       addFlash (BS.fromString ("I cannot delete user. '" ++ BS.toString (userfullname user) ++ "' still has " ++ show (length documents) ++ " documents as author. Take over his documents, then try to delete the account again."))
  
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



