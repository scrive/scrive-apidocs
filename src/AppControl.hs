{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             NamedFieldPuns, ScopedTypeVariables 
 #-}
module AppControl (appHandler) where

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


appHandler :: ServerPartT IO Response
appHandler = do
  rq <- askRq
  let host = maybe "skrivapa.se" BS.toString $ getHeader "host" rq
  let scheme = maybe "http" BS.toString $ getHeader "scheme" rq
  let hostpart =  scheme ++ "://" ++ host
  
  maybeuser <- userLogin
  flashmessages <- case maybeuser of
                     Just (User{userid}) -> liftIO $ update $ GetUserFlashMessages userid
                     Nothing -> return []
  minutestime <- liftIO $ getMinutesTime
  let 
   ctx = Context
            { ctxmaybeuser = maybeuser
            , ctxhostpart = hostpart
            , ctxflashmessages = flashmessages              
            , ctxtime = minutestime
            }
   (routes :: [ServerPartT IO Response]) =
    [ nullDir >> webHSP (pageFromBody ctx TopNew kontrakcja (welcomeBody ctx))
    , toIO ctx $ dir "sign" $ DocControl.handleSign ctx
    , toIO ctx $ dir "issue" (withUser maybeuser (DocControl.handleIssue ctx))
    , toIO ctx $ dir "pages" $ path $ \fileid -> 
        msum [ path $ \pageno -> do
                 modminutes <- query $ FileModTime fileid
                 DocControl.showPage ctx modminutes fileid pageno
             ]
    , dir "landpage" $ 
          msum [ dir "signinvite" $ path $ \documentid -> DocControl.landpageSignInvite ctx documentid
               , dir "signed" $ path $ \documentid -> path $ \signatorylinkid ->
                                                      DocControl.landpageSigned ctx documentid signatorylinkid
               , dir "signedsave" $ path $ \documentid -> path $ \signatorylinkid ->
                                                      DocControl.landpageSignedSave ctx documentid signatorylinkid
               , dir "saved" $ withUser maybeuser $ path $ \documentid -> path $ \signatorylinkid ->
                                                      DocControl.landpageSaved ctx documentid signatorylinkid
               ]
          
    , dir "pagesofdoc" $ path $ \docid -> do
                 maybedoc <- query $ GetDocumentByDocumentID docid
                 case maybedoc of
                   Just doc -> case documentfiles doc of
                                 [] -> notFound (toResponse "temporary unavailable (document has no files)")
                                 f -> webHSP (DocView.showFilesImages2 f)
                   Nothing -> -- why this should ever happen?
                              -- we end up here after document view page is completed
                              -- should have the document there too in the database
                               notFound (toResponse "temporary unavailable (document not found)")
    , toIO ctx $ dir "account" (withUser maybeuser (UserControl.handleUser ctx))
    , toIO ctx $ dir "logout" (handleLogout)
    , toIO ctx $ dir "login" loginPage
    ]
    ++ (if isSuperUser maybeuser then 
            [ toIO ctx $ dir "stats" $ statsPage
            , toIO ctx $ dir "become" $ handleBecome
            , toIO ctx $ dir "createuser" $ handleCreateUser
            ]
       else [])
    ++ [ fileServe [] "public"
    , webHSP (pageFromBody ctx TopNone kontrakcja (errorReport ctx rq))
    ]
  msum routes

loginPage :: Kontra Response
loginPage = (methodM GET >> loginPageGet) `mplus` (methodM POST >> loginPagePost)

loginPageGet :: Kontra Response
loginPageGet = do
  ctx <- lift get
  webHSP (pageFromBody ctx TopNone kontrakcja loginPageView)


loginPagePost :: Kontra Response
loginPagePost = do
  rq <- askRq
  Just email <- getDataFn (look "email")
  Just passwd <- getDataFn (look "password")
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
    

statsPage :: Kontra Response
statsPage = do
  ndocuments <- query $ GetDocumentStats
  allusers <- query $ GetAllUsers
  webHSP (statsPageView (length allusers) ndocuments allusers)
    

handleBecome :: Kontra Response
handleBecome = do
  Just (userid :: UserID) <- getDataFn $ (look "user" >>= readM)
  sessionid <- update $ NewSession userid
  setHeaderM "Set-Cookie" ""
  startSession sessionid
  response <- webHSP (seeOtherXML "/")
  seeOther "/" response


handleCreateUser :: Kontra Response
handleCreateUser = do
  Just email <- getDataFn $ (look "email")
  Just fullname <- getDataFn $ (look "fullname")
  user <- update $ AddUser (ExternalUserID BS.empty) (BS.fromString fullname) (BS.fromString email)
  let passwd = "GH45T7hjK"
  update $ SetUserPassword user (BS.fromString passwd)
  content <- liftIO $ passwordChangeMail (BS.fromString email) (BS.fromString fullname) 
             (BS.fromString passwd)
  liftIO $ sendMail (BS.fromString fullname) (BS.fromString email) 
               (BS.fromString "SkrivaPa new password") content BS.empty
  -- FIXME: where to redirect?
  response <- webHSP (seeOtherXML "/")
  seeOther "/" response
  
