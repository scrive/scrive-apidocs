module PadQueue.Control (showPadQueuePage, padQueueState, handlePadLogout)
    where

import PadQueue.Model
import DB
import Doc.DocStateData
import Doc.Tokens.Model
import Kontra
import User.Model
import qualified Log
import Util.SignatoryLinkUtils
import Util.MonadUtils
import PadQueue.View

import Control.Monad

import Text.JSON hiding (Result)
import KontraLink
import Doc.Model
import AppView
import Happstack.Server.Types
import Data.Maybe

import Analytics.Include

-- PadQueue STATE
padQueueState :: Kontrakcja m => m JSValue
padQueueState = do
    ctx <- getContext
    Log.debug "Checking state"
    case (ctxmaybeuser ctx `mplus` ctxmaybepaduser ctx) of 
         Nothing  -> do
             Log.debug "Not logged in"
             padQueueStateJSONNotLoggedIn
         Just user -> do
             pq <- dbQuery $ GetPadQueue (userid user)
             msdata <- padQueueToSignatoryData pq
             case msdata of
               Just (_,siglink) -> do
                 dbUpdate $ AddDocumentSessionToken (signatorylinkid siglink) (signatorymagichash siglink)
               Nothing -> return ()
             padQueueStateJSON (isJust $ ctxmaybeuser ctx)  msdata

-- PadQueue Pages
showPadQueuePage::  (Kontrakcja m) =>  m Response
showPadQueuePage = do
    ctx <- getContext
    ad <- getAnalyticsData
    padQueuePage ctx ad >>= simpleHtmlResonseClrFlash


padQueueToSignatoryData :: Kontrakcja m => PadQueue -> m (Maybe (Document,SignatoryLink))
padQueueToSignatoryData Nothing = return Nothing
padQueueToSignatoryData (Just (did,slid)) = do
        Log.debug $ "Some document for padqueue found"
        doc <- guardJustM $ dbQuery $ GetDocumentByDocumentID did
        sl <- guardJust $ getSigLinkFor doc slid
        if (Preparation /= documentstatus doc)
         then return $ Just (doc,sl)
         else return Nothing  

-- PadQueue Logout
handlePadLogout :: Kontrakcja m => m KontraLink
handlePadLogout = do
    Log.debug "Loging out of pad device"
    logoutPadUser
    return LoopBack

logoutPadUser :: Kontrakcja m => m ()
logoutPadUser = do
    -- Some event loging should be done here
    logPadUserToContext Nothing
