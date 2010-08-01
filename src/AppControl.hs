{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             NamedFieldPuns, ScopedTypeVariables, CPP
 #-}
module AppControl (appHandler, handleRoutes) where

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
import qualified Network.Curl as Curl
import qualified DocView as DocView
import qualified DocControl as DocControl
import Happstack.Server.SimpleHTTP (seeOther)
import Control.Monad.Reader
import DocState
import UserState
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

handleRoutes ctx@Context{ctxmaybeuser} = msum $
    [ nullDir >> webHSP (pageFromBody ctx TopNew kontrakcja (welcomeBody ctx))
    , toIO ctx $ dir "sign" $ DocControl.handleSign ctx
    , toIO ctx $ dir "issue" (withUser ctxmaybeuser (DocControl.handleIssue ctx))
    , toIO ctx $ dir "pages" $ path $ \fileid -> 
        msum [ path $ \pageno -> do
                 modminutes <- query $ FileModTime fileid
                 DocControl.showPage ctx modminutes fileid pageno
             ]
    , dir "landpage" $ 
          msum [ dir "signinvite" $ pathdb GetDocumentByDocumentID $ \document -> 
                     DocControl.landpageSignInvite ctx document
               , dir "signed" $ pathdb GetDocumentByDocumentID $ \document -> path $ \signatorylinkid ->
                     DocControl.landpageSigned ctx document signatorylinkid
               , dir "signedsave" $ pathdb GetDocumentByDocumentID $ \document -> 
                     path $ \signatorylinkid ->
                     DocControl.landpageSignedSave ctx document signatorylinkid
               , dir "saved" $ withUser ctxmaybeuser $ pathdb GetDocumentByDocumentID $ \document -> 
                     path $ \signatorylinkid ->
                     DocControl.landpageSaved ctx document signatorylinkid
               ]
          
    , dir "pagesofdoc" $ pathdb GetDocumentByDocumentID $ \doc -> do
          case documentfiles doc of
              [] -> notFound (toResponse "temporary unavailable (document has no files)")
              f -> webHSP (DocView.showFilesImages2 f)
    , toIO ctx $ dir "account" (withUser ctxmaybeuser (UserControl.handleUser ctx))
    , toIO ctx $ dir "logout" (handleLogout)
    , toIO ctx $ dir "login" loginPage
    ]
    ++ (if isSuperUser ctxmaybeuser then 
            [ toIO ctx $ dir "stats" $ statsPage
            , toIO ctx $ dir "become" $ handleBecome
            , toIO ctx $ dir "createuser" $ handleCreateUser
            , toIO ctx $ dir "db" $ msum [ methodM GET >> indexDB
                                         , fileServe [] "_local/kontrakcja_state"
                                         ]
            ]
       else [])
    ++ [ fileServe [] "public"] 


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
  email <- getDataFnM (look "email")
  passwd <- getDataFnM (look "password")
  Just user@User{userpassword = Just upasswd} <- query $ GetUserByEmail (BS.fromString email)
  if upasswd==BS.fromString passwd
     then do
      sessionid <- update $ NewSession (userid user)
      startSession sessionid
      response <- webHSP (seeOtherXML "/")
      seeOther "/" response
     else do
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
      createProcess (proc "df" []) { std_out = CreatePipe }
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
  email <- getDataFnM $ (look "email")
  fullname <- getDataFnM $ (look "fullname")
  user <- update $ AddUser (ExternalUserID BS.empty) (BS.fromString fullname) (BS.fromString email)
  let letters =['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
  indexes <- liftIO $ replicateM 8 (randomRIO (0,length letters))
  let passwd = map (letters!!) indexes
  update $ SetUserPassword user (BS.fromString passwd)
  content <- liftIO $ passwordChangeMail (BS.fromString email) (BS.fromString fullname) 
             (BS.fromString passwd)
  liftIO $ sendMail [(BS.fromString fullname,BS.fromString email)]
               (BS.fromString "SkrivaPa new password") content BS.empty
  -- FIXME: where to redirect?
  response <- webHSP (seeOtherXML "/stats")
  seeOther "/stats" response
  
handleDownloadDatabase :: Kontra Response
handleDownloadDatabase = do fail "nothing"
  
indexDB :: Kontra Response
indexDB = do
  contents <- liftIO $ getDirectoryContents "_local/kontrakcja_state"
  webHSP (AppView.databaseContents contents)
