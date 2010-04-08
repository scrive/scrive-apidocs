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

appHandler :: ServerPartT IO Response
appHandler = do
  rq <- askRq
  let Just host = getHeader "host" rq
  let hostpart = "http://" ++ BSC.toString host
  
  maybeuser <- withMSessionDataSP $ \maybeuserid -> do 
    case maybeuserid of
      Just userid -> query $ FindUserByUserID userid
      Nothing -> return Nothing
  
  msum
    [ methodM GET >> webHSP (pageFromBody maybeuser hostpart kontrakcja (welcomeBody hostpart))
    , dir "rpxsignin" (handleRPXLogin hostpart)
    , dir "sign" (withUser maybeuser (DocControl.handleSign hostpart))
    , dir "issue" (withUser maybeuser (DocControl.handleIssue hostpart))
    , fileServe [] "public"
    ]
    
  
handleRPXLogin :: String -> ServerPartT IO Response
handleRPXLogin hostpart = do
    Just token <- getDataFn (look "token") 

    let req = "https://rpxnow.com/api/v2/auth_info" ++ 
              "?apiKey=03bbfc36d54e523b2602af0f95aa173fb96caed9" ++
              "&token=" ++ token
              
    {-
       FIXME: Get the certificate of that server and import it
       to private repository.
    -}
        
    (_code,rpxdata) <- liftIO $ 
                      Curl.curlGetString req [Curl.CurlFollowLocation True
                                             ,Curl.CurlSSLVerifyPeer False] 

    {-
       FIXME: Take care of the situation when not all data is available
    -}
    
    let Just json = Json.decode (BSL.fromString rpxdata) 
        Just jsonMapping = fromMapping json 
        Just profileMapping = lookupMapping (BSC.fromString "profile") jsonMapping
        Just (Json.JsonString verifiedEmail) = lookupScalar (BSC.fromString "verifiedEmail") profileMapping
        Just (Json.JsonString identifier) = lookupScalar (BSC.fromString "identifier") profileMapping
        Just nameMapping = lookupMapping (BSC.fromString "name") profileMapping
        Just (Json.JsonString formatted) = lookupScalar (BSC.fromString "formatted") nameMapping
        
    maybeuser <- query $ FindUserByExternalUserID (ExternalUserID identifier)
    
    user <- case maybeuser of
      Just (user@User{userid}) -> do 
        sessionid <- update $ NewSession userid
        startSession sessionid
        return user
      Nothing -> do
        user@User{userid} <- update $ AddUser (ExternalUserID identifier) (formatted) (verifiedEmail)
        sessionid <- update $ NewSession userid
        startSession sessionid
        return user
    
    webHSP (pageFromBody (Just user) hostpart kontrakcja (handleRPXLoginView json))
    