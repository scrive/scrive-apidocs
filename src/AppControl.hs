{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
module AppControl (appHandler) where

import AppState
import AppView
import Control.Monad(msum,liftM)
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import Data.Object
import Happstack.Data.IxSet ((@=),getOne)
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

appHandler :: ServerPartT IO Response
appHandler = do
  rq <- askRq
  let Just host = getHeader "host" rq
  let hostpart = "http://" ++ BSC.toString host
  
  maybeuser <- userLogin
  
  msum
    [ nullDir >> webHSP (pageFromBody maybeuser hostpart kontrakcja (welcomeBody hostpart))
    , dir "sign" (withUser maybeuser (DocControl.handleSign hostpart))
    , dir "issue" (withUser maybeuser (DocControl.handleIssue hostpart))
    , dir "logout" (handleLogout)
    , fileServe [] "public"
    , webHSP (pageFromBody maybeuser hostpart kontrakcja (errorReport maybeuser rq))
    ]
    
handleLogout :: ServerPartT IO Response
handleLogout = do
  endSession
  response <- webHSP (seeOtherXML "/")
  seeOther "/" response
    
  
