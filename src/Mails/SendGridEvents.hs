{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.SendGridEvents
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Sendgrid events interface. 'handleSendgridEvent' is used when sendgrid contacts us. 
-- We do not, and probably will never use full interface, just simlpyfied version.
-- mailinfo param is set when we are sending mails. 
-----------------------------------------------------------------------------
module Mails.SendGridEvents(handleSendgridEvent) where
import Kontra
import Control.Monad.Trans
import KontraLink
import Misc 
import Data.Maybe
import qualified Mails.MailsUtil as Mail
import Doc.DocState
import Happstack.State (update,query)
import Mails.SendMail
import Templates.Templates
import Templates.TemplatesUtils
import Control.Monad.State
import qualified Data.ByteString.UTF8 as BS (fromString,toString)
import Data.List (find)
import System.Log.Logger

data SendGridEventType = Delivered | Undelivered | Other String deriving Show
data SendgridEvent = SendgridEvent {
                            mailId :: Integer
                          , event::SendGridEventType
                          , info::MailInfo
                         
                     } deriving Show 

-- | Handler for receving delivery info from sendgrid
handleSendgridEvent::Kontra KontraLink
handleSendgridEvent = do
                          mid <- readMailId
                          et <- readEventType                   
                          mi <- readMailInfo
                          let ev = SendgridEvent {mailId = mid, event = et, info = mi}
                          liftIO $ logM "Kontrakcja.Mail" NOTICE  $ "Sendgrid event recived " ++ (show ev)
                          routeToHandler ev
                          return $ LinkMain


readMailId:: Kontra Integer
readMailId = fmap (fromMaybe 0) $ readField "id"
                  
             
readEventType::Kontra SendGridEventType
readEventType = do
                  me <- getField "event"
                  liftIO $ putStrLn $ show me
                  case me of
                    Just e ->  return $ fromMaybe (Other e) (lookup e [("delivered", Delivered),("bounce", Undelivered), ("dropped", Undelivered)])
                    Nothing -> return (Other "")
                  
readMailInfo:: Kontra MailInfo
readMailInfo = do
                  mi<- readField "mailinfo"
                  case mi of 
                   Just i -> return i
                   Nothing -> return None
             


-- | Main routing table after getting sendgrid event.
routeToHandler::SendgridEvent -> Kontra ()
routeToHandler (SendgridEvent {event=Other _}) = return ()                        
routeToHandler (SendgridEvent {info= None}) = return ()                        
routeToHandler (SendgridEvent {info=Invitation docid signlinkid,event= Delivered}) = handleDeliveredInvitation docid signlinkid
routeToHandler (SendgridEvent {info=Invitation docid signlinkid,event= Undelivered}) = handleUndeliveredInvitation docid signlinkid



-- | Actions perform that are performed then

handleDeliveredInvitation::DocumentID -> SignatoryLinkID  -> Kontra ()
handleDeliveredInvitation docid signlinkid = do
    _ <- update $ SetInvitationDeliveryStatus docid signlinkid Mail.Delivered
    return ()
    
handleUndeliveredInvitation::DocumentID -> SignatoryLinkID -> Kontra ()
handleUndeliveredInvitation docid signlinkid = do
    mdoc <- update $ SetInvitationDeliveryStatus docid signlinkid Mail.Undelivered
    case mdoc of
     Right doc -> do   
                 ctx <- get
                 title <- liftIO $ renderTemplate (ctxtemplates ctx) "invitationMailUndeliveredTitle" ()
                 Just author <- query $ GetUserByUserID $ unAuthor $ documentauthor doc
                 let documentauthordetails = signatoryDetailsFromUser author
                 let fullnameemails = [(signatoryname $ documentauthordetails, signatoryemail $ documentauthordetails)]
                 let msld = fmap signatorydetails $ find ((==) signlinkid . signatorylinkid ) $ documentsignatorylinks doc
                 case msld of
                  Just sld -> do    
                    content <- liftIO $ wrapHTML (ctxtemplates ctx)=<< renderTemplate (ctxtemplates ctx) "invitationMailUndeliveredContent"
                                                                       [("authorname",BS.toString $ signatoryname $ documentauthordetails),
                                                                        ("email",BS.toString $ signatoryemail sld),
                                                                        ("unsigneddoclink", show $ LinkIssueDoc $ documentid doc),
                                                                        ("ctxhostpart",ctxhostpart ctx)  
                                                                       ]  
                    liftIO $ sendMail (ctxmailer ctx)  $  emptyMail {title=BS.fromString title,content=BS.fromString content,fullnameemails = fullnameemails}
                  Nothing -> return () 
     _ -> return ()                                                                                  
