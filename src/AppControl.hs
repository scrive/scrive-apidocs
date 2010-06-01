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
import qualified Data.ByteString as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BSC
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

appHandler :: ServerPartT IO Response
appHandler = do
  rq <- askRq
  let Just host = getHeader "host" rq
  let hostpart = 
          if host == BSC.fromString "127.0.0.1:8000" 
          -- don't know how to get around nginx not passing Host header
          then "http://skrivapa.se" 
          else "http://" ++ BSC.toString host
  
  maybeuser <- userLogin
  let ctx = Context maybeuser hostpart
  
  msum $
    [ nullDir >> webHSP (pageFromBody ctx TopNew kontrakcja (welcomeBody ctx))
    , dir "sign" (withUser maybeuser (DocControl.handleSign ctx))
    , dir "issue" (withUser maybeuser (DocControl.handleIssue ctx))
    , dir "pages" $ path $ \fileid -> path $ \pageno -> 
                                      do
                                        modminutes <- query $ FileModTime fileid
                                        DocControl.showPage ctx modminutes fileid pageno
    , dir "account" (withUser maybeuser (UserControl.handleUser ctx))
    , dir "logout" (handleLogout)]
    ++ (if isSuperUser maybeuser then 
            [ dir "stats" $ statsPage
            , dir "become" $ handleBecome
            ]
       else [])
    ++ [ fileServe [] "public"
    , webHSP (pageFromBody ctx TopNone kontrakcja (errorReport ctx rq))
    ]
    
handleLogout :: ServerPartT IO Response
handleLogout = do
  endSession
  response <- webHSP (seeOtherXML "/")
  seeOther "/" response
    

statsPage :: ServerPartT IO Response
statsPage = do
  ndocuments <- query $ GetDocumentStats
  allusers <- query $ GetAllUsers
  webHSP (statsPageView (length allusers) ndocuments allusers)
    

handleBecome :: ServerPartT IO Response
handleBecome = do
  Just (userid :: UserID) <- getDataFn $ (look "user" >>= readM)
  sessionid <- update $ NewSession userid
  setHeaderM "Set-Cookie" ""
  startSession sessionid
  response <- webHSP (seeOtherXML "/")
  seeOther "/" response
