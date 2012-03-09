module PadQueue.Control (addToQueue,clearQueue,showPadQueuePage, padQueueState, handlePadLogin)
    where
        
import PadQueue.Model
import DB.Classes
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocUtils
import Kontra
import Redirect
import User.Model
--import qualified Log
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

-- PadQueue STATE
padQueueState ::  (Kontrakcja m) =>  m JSValue
padQueueState = do
    uid <- userid <$> (guardJustM $ ctxmaybeuser <$> getContext)
    pq <- runDB $ dbQuery $ GetPadQueue uid
    msdata <- padQueueToSignatoryData pq
    padQueueStateJSON msdata

-- PadQueue ACTIONS
addToQueue :: (Kontrakcja m) => DocumentID ->  SignatoryLinkID -> m JSValue
addToQueue did slid = do
    uid <- userid <$> (guardJustM $ ctxmaybeuser <$> getContext)
    doc <- guardRightM $ getDocByDocIDForAuthor did
    _ <- guardJust $ getSigLinkFor doc slid
    if (doc `allowsIdentification` PadIdentification)
        then do
            runDB $ dbUpdate $ AddToPadQueue uid did slid
            liftIO $ json $ return ()
        else mzero

clearQueue :: (Kontrakcja m) =>  m JSValue
clearQueue = do
    uid <- userid <$> (guardJustM $ ctxmaybeuser <$> getContext)
    runDB $ dbUpdate $ ClearPadQueue uid 
    liftIO $ json $ return ()

-- PadQueue Pages
showPadQueuePage::  (Kontrakcja m) =>  m String
showPadQueuePage = padQueuePage 


padQueueToSignatoryData :: (Kontrakcja m) => PadQueue -> m (Maybe (Document,SignatoryLink))
padQueueToSignatoryData Nothing = return Nothing
padQueueToSignatoryData (Just (did,slid)) = do
        doc <- guardRightM $ getDocByDocID did
        sl <- guardJust $ getSigLinkFor doc slid
        if (Preparation /= documentstatus doc)
         then return $ Just (doc,sl)
         else return Nothing  
         



-- PadQueue Login
handlePadLogin :: Kontrakcja m => m KontraLink
handlePadLogin = do
    memail  <- getFieldUTF "email"
    mpasswd <- getFieldUTF "password"
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- runDBQuery $ GetUserByEmail Nothing (Email $ email)
            case maybeuser of
               Just user@User{userpassword}
                    | verifyPassword userpassword passwd -> do
                       loginPadUser user
                       return LoopBack
               _ -> return $ LoopBack
        _ -> return $ LoopBack              
                    

loginPadUser :: Kontrakcja m => User -> m ()
loginPadUser user = do
    -- Some event loging should be done here
    logPadUserToContext (Just user)         