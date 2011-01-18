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
import User
import Control.Monad.Trans
import KontraLink
import Misc 
import Data.Maybe
import qualified Mails.MailsUtil as Mail
import DocState
import Happstack.State (update,query)
import Mails.SendMail
import Templates.Templates
import Templates.TemplatesUtils
import Control.Monad.State
import qualified Data.ByteString.UTF8 as BS (fromString,toString)
import Data.List (find)
import System.Log.Logger
data SendGridEventType = Delivered | Undelivered | Other deriving Show


readEventType::Kontra SendGridEventType
readEventType = do
                  me <- getField "event"
                  liftIO $ putStrLn $ show me
                  case me of
                    Just e ->  return $ fromMaybe Other (lookup e [("delivered", Delivered),("bounce", Undelivered), ("dropped", Undelivered)])
                    Nothing -> return Other
                  
readMailInfo:: Kontra MailInfo
readMailInfo = do
                  mi<- readField "mailinfo"
                  case mi of 
                   Just i -> return i
                   Nothing -> return None
             
data SendgridEvent = SendgridEvent {
                            event::SendGridEventType
                          , info::MailInfo
                     } deriving Show 
handleSendgridEvent::Kontra KontraLink
handleSendgridEvent = do
                          et <- readEventType                   
                          mi <- readMailInfo
                          let ev = SendgridEvent {event = et, info = mi}
                          liftIO $ logM "Kontrakcja.Mail" NOTICE  $ "Sendgrid event recived " ++ (show ev)
                          handleSendgridEvent' ev
                          return $ LinkMain


-- | Main event handling table, pattern matched for every type of mail and status
handleSendgridEvent'::SendgridEvent -> Kontra ()
handleSendgridEvent' (SendgridEvent {event=Other}) = return ()                        
handleSendgridEvent' (SendgridEvent {info= None}) = return ()                        
handleSendgridEvent' (SendgridEvent {info=Invitation signlinkid,event= Delivered}) = 
  do
   _ <- update $ SetInvitationDeliveryStatus signlinkid Mail.Delivered
   return ()
handleSendgridEvent' (SendgridEvent {info=Invitation signlinkid,event= Undelivered}) = 
  do
   mdoc <- update $ SetInvitationDeliveryStatus signlinkid Mail.Undelivered
   case mdoc of
     Just doc -> do   
                 ctx <- get
                 title <- liftIO $ renderTemplate (ctxtemplates ctx) "invitationMailUndeliveredTitle" []  
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
                    liftIO $ sendMail (ctxmailsconfig ctx)  $  emptyMail {title=BS.fromString title,content=BS.fromString content,fullnameemails = fullnameemails}
                  Nothing -> return () 
     Nothing -> return ()                                                                                  


