module PadQueue.Control (addToQueue,clearQueue,showPadQueuePage, padQueueState, handlePadLogin, handlePadLogout)
    where
        
import PadQueue.Model
import DB.Classes
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocUtils
import Kontra
import Redirect
import User.Model
import qualified Log
import Util.SignatoryLinkUtils
import Util.MonadUtils
import PadQueue.View

import Control.Applicative
import Control.Monad

import Text.JSON hiding (Result)
import Text.JSON.Fields as JSON (json)
import Control.Monad.Trans
import KontraLink
import Misc
import Util.FlashUtil
import User.UserView
import Doc.Model
import AppView
import Happstack.Server.Types
import Data.Char (toLower)
import Data.Maybe
import EvidenceLog.Model
import User.History.Model
-- PadQueue STATE
padQueueState ::  (Kontrakcja m) =>  m JSValue
padQueueState = do
    ctx <- getContext
    Log.debug "Checking state"
    case (ctxmaybeuser ctx `mplus` ctxmaybepaduser ctx) of 
         Nothing  -> do
             Log.debug "Not logged in"
             padQueueStateJSONNotLoggedIn
         Just user -> do
             pq <- runDB $ dbQuery $ GetPadQueue (userid user)
             msdata <- padQueueToSignatoryData pq
             padQueueStateJSON (isJust $ ctxmaybeuser ctx)  msdata

-- PadQueue ACTIONS
addToQueue :: (Kontrakcja m) => DocumentID ->  SignatoryLinkID -> m JSValue
addToQueue did slid = do
    uid <- userid <$> (guardJustM $ ctxmaybeuser <$> getContext)
    doc <- guardRightM $ getDocByDocIDForAuthor did
    _ <- guardJust $ getSigLinkFor doc slid
    if (doc `allowsIdentification` PadIdentification)
        then do
            actor <- guardJustM $ mkAuthorActor <$> getContext
            runDB $ dbUpdate $ AddToPadQueue uid did slid actor
            liftIO $ json $ return ()
        else internalError

clearQueue :: (Kontrakcja m) =>  m JSValue
clearQueue = do
    uid <- userid <$> (guardJustM $ ctxmaybeuser <$> getContext)
    actor <- guardJustM $ mkAuthorActor <$> getContext
    runDB $ dbUpdate $ ClearPadQueue uid actor
    liftIO $ json $ return ()

-- PadQueue Pages
showPadQueuePage::  (Kontrakcja m) =>  m Response
showPadQueuePage = padQueuePage >>= simpleResponse


padQueueToSignatoryData :: (Kontrakcja m) => PadQueue -> m (Maybe (Document,SignatoryLink))
padQueueToSignatoryData Nothing = return Nothing
padQueueToSignatoryData (Just (did,slid)) = do
        Log.debug $ "Some document for padqueue found"
        doc <- guardJustM $ runDBQuery $ GetDocumentByDocumentID did
        sl <- guardJust $ getSigLinkFor doc slid
        if (Preparation /= documentstatus doc)
         then return $ Just (doc,sl)
         else return Nothing  
         
-- PadQueue Login
handlePadLogin :: Kontrakcja m => m KontraLink
handlePadLogin = do
    Log.debug "Loging to pad device"
    memail  <- getField "email"
    mpasswd <- getField "password"
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- runDBQuery $ GetUserByEmail Nothing (Email $ map toLower $ email)
            when_ (isJust maybeuser) $ do
                 ctx <- getContext
                 _ <- runDBUpdate $ LogHistoryPadLoginAttempt (userid $ fromJust maybeuser) (ctxipnumber ctx) (ctxtime ctx)
                 return ();
            case maybeuser of
               Just user@User{userpassword}
                    | verifyPassword userpassword passwd -> do
                       Log.debug "Logged in"
                       loginPadUser user
                       return LoopBack
               _ -> do
                   addFlashM $ flashMessageLoginRedirectReason (InvalidLoginInfo undefined)
                   return $ LoopBack
        _ -> return $ LoopBack              
                    

-- PadQueue Login
handlePadLogout :: Kontrakcja m => m KontraLink
handlePadLogout = do
    Log.debug "Loging out of pad device"
    logoutPadUser
    return LoopBack
                    
loginPadUser :: Kontrakcja m => User -> m ()
loginPadUser user = do
    -- Some event loging should be done here
    ctx <- getContext
    _ <- runDBUpdate $ LogHistoryPadLoginSuccess (userid user) (ctxipnumber ctx) (ctxtime ctx)
    logPadUserToContext (Just user)

logoutPadUser :: Kontrakcja m => m ()
logoutPadUser = do
    -- Some event loging should be done here
    logPadUserToContext Nothing    