-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.SendGridEvents
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Sendgrid events interface. 'handleSendgridEvent' is used when sendgrid contacts us.
-- mailinfo param is set when we are sending mails.
-----------------------------------------------------------------------------
module Mails.SendGridEvents (
        handleSendgridEvent
      , mailUndeliveredInvitation
      , mailDeliveredInvitation
      , mailDeferredInvitation
    ) where

import Control.Applicative
import Kontra
import Control.Monad.Trans
import KontraLink
import Misc
import Data.Maybe
import qualified Mails.MailsUtil as Mail
import Doc.DocState
import ActionSchedulerState
import Happstack.State
import Mails.SendMail
import Mails.MailsData
import Templates.Templates
import Templates.TemplatesUtils
import Control.Monad.State
import qualified Data.ByteString.UTF8 as BS
import Data.List
import qualified AppLogger as Log
import MinutesTime
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Util.MonadUtils
import User.Model
import Happstack.Server

data SendgridEvent =
    SendgridEvent {
          mailId      :: Integer
        , mailAddress :: String
        , event       :: SendGridEventType
        , info        :: MailInfo
        } deriving Show

-- | Handler for receving delivery info from sendgrid
handleSendgridEvent :: Kontrakcja m => m Response
handleSendgridEvent = do
    mid <- readNum "id"
    -- email can be either 'email' or '<email>', so we want to get rid of brackets
    maddr <- filter (not . (`elem` "<>")) <$> readString "email"
    et <- readString "event" >>= readEventType
    mi <- fromMaybe None <$> readField "mailinfo"
    let ev = SendgridEvent {
          mailId = mid
        , mailAddress = maddr
        , event = et
        , info = mi
    }
    Log.mail $ "Sendgrid event received: " ++ show ev
    now <- liftIO getMinutesTime
    maction' <- query $ GetAction $ ActionID mid
    when (isNothing maction') $ do
       Log.mail $ "Action does not exist " ++ show mid
       return ()

    maction <- checkValidity now <$> maction'

    case maction of
         -- we update SentEmailInfo of given email. if email is reported
         -- delivered, dropped or bounced we remove it from the system.
         -- that way we can keep track of emails that were "lost".
         Just Action{actionID, actionType = SentEmailInfo{seiEmail, seiMailInfo, seiBackdoorInfo}} -> do
             when (seiEmail == (Email $ BS.fromString maddr)) $ do
                 Log.mail $ "Mail #" ++ show mid ++ " to " ++ show seiEmail ++ " " ++ show et
                 _ <- update $ UpdateActionType actionID $ SentEmailInfo {
                       seiEmail = seiEmail
                     , seiMailInfo = seiMailInfo
                     , seiEventType = et
                     , seiLastModification = now
                     , seiBackdoorInfo = seiBackdoorInfo
                 }
                 let removeAction = case et of
                      Delivered _  -> True
                      Dropped _    -> True
                      Bounce _ _ _ -> True
                      _            -> False
                 when_ removeAction $ do
                     update $ UpdateActionEvalTime actionID now
         _ -> do
             Log.mail $ "Mail #" ++ show mid ++ " is not known to system, ignoring (not valid)"
             return ()
    routeToHandler ev
    ok $ toResponse "Thanks"

readEventType :: Kontrakcja m => String -> m SendGridEventType
readEventType "processed" = return Processed
readEventType "open" = return Opened
readEventType "dropped" = do
    reason <- readString "reason"
    return $ Dropped reason
readEventType "deferred" = do
    response <- readString "response"
    attempt <- readNum "attempt"
    return $ Deferred response attempt
readEventType "delivered" = do
    response <- readString "response"
    return $ Delivered response
readEventType "bounce" = do
    status <- readString "status"
    reason <- readString "reason"
    type_ <- readString "type"
    return $ Bounce status reason type_
readEventType name = return $ Other name

readNum :: (Num a, Read a, Kontrakcja m) => String -> m a
readNum name = fromMaybe (-1) <$> readField name

readString :: Kontrakcja m => String -> m String
readString name = fromMaybe "" <$> getField name

-- | Main routing table after getting sendgrid event.
routeToHandler :: Kontrakcja m => SendgridEvent -> m ()
routeToHandler (SendgridEvent {mailAddress, info = Invitation docid signlinkid, event}) = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    let signemail = fromMaybe "" (BS.toString . getEmail <$> getSignatoryLinkFromDocumentByID doc signlinkid)
    Log.debug $ signemail ++ " == " ++ mailAddress
    -- since when email is reported deferred author has a possibility to change
    -- email address, we don't want to send him emails reporting success/failure
    -- for old signatory address, so we need to compare addresses here.
    case event of
         Opened       -> handleOpenedInvitation docid signlinkid
         Dropped _    -> if signemail == mailAddress
                            then handleUndeliveredInvitation docid signlinkid
                            else return ()
         Delivered _  -> handleDeliveredInvitation docid signlinkid
         -- we send notification that email is reported deferred after fifth
         -- attempt has failed - this happens after ~10 minutes from sendout
         Deferred _ 5 -> handleDeferredInvitation docid signlinkid
         Bounce _ _ _ -> if signemail == mailAddress
                            then handleUndeliveredInvitation docid signlinkid
                            else return ()
         _            -> return ()
routeToHandler _ = return ()

-- | Actions perform that are performed then
handleDeliveredInvitation :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleDeliveredInvitation docid signlinkid = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    case getSignatoryLinkFromDocumentByID doc signlinkid of
         Just signlink -> do
             -- send it only if email was reported deferred earlier
             when (invitationdeliverystatus signlink == Mail.Deferred) $ do
                 ctx <- getContext
                 mail <- mailDeliveredInvitation doc signlink
                 scheduleEmailSendout (ctxesenforcer ctx) $ mail { to = [getMailAddress $ fromJust $ getAuthorSigLink doc] }
         Nothing -> return ()
    _ <- update $ SetInvitationDeliveryStatus docid signlinkid Mail.Delivered
    return ()

mailDeliveredInvitation :: Kontrakcja m =>  Document -> SignatoryLink -> m Mail
mailDeliveredInvitation doc signlink = kontramail  "invitationMailDeliveredAfterDeferred" $ do
                     field "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
                     field "email" $ getEmail signlink
                     field "documenttitle" $ BS.toString $ documenttitle doc

handleOpenedInvitation :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleOpenedInvitation docid signlinkid = do
    now <- liftIO $ getMinutesTime
    _ <- update $ MarkInvitationRead docid signlinkid now
    return ()

handleDeferredInvitation :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleDeferredInvitation docid signlinkid = do
    mdoc <- update $ SetInvitationDeliveryStatus docid signlinkid Mail.Deferred
    case mdoc of
         Right doc -> do
             ctx <- getContext
             mail <- mailDeferredInvitation ctx doc
             scheduleEmailSendout (ctxesenforcer ctx) $ mail {  to = [getMailAddress $ fromJust $ getAuthorSigLink doc] }
         Left _ -> return ()

mailDeferredInvitation:: Kontrakcja m => Context -> Document -> m Mail
mailDeferredInvitation ctx doc = kontramail "invitationMailDeferred" $ do
    field "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    field "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    field "ctxhostpart" $ ctxhostpart ctx


handleUndeliveredInvitation :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleUndeliveredInvitation docid signlinkid = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    ctx <- getContext
    case getSignatoryLinkFromDocumentByID doc signlinkid of
         Just signlink -> do
             _ <- update $ SetInvitationDeliveryStatus docid signlinkid Mail.Undelivered
             mail <- mailUndeliveredInvitation ctx doc signlink
             scheduleEmailSendout (ctxesenforcer ctx) $ mail {  to = [getMailAddress $ fromJust $ getAuthorSigLink doc]  }
         Nothing -> return ()

mailUndeliveredInvitation :: Kontrakcja m => Context -> Document -> SignatoryLink -> m Mail
mailUndeliveredInvitation ctx doc signlink = kontramail "invitationMailUndelivered" $ do
    field "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    field "documenttitle" $ documenttitle doc
    field "email" $ getEmail signlink
    field "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    field "ctxhostpart" $ ctxhostpart ctx

getSignatoryLinkFromDocumentByID :: Document -> SignatoryLinkID -> Maybe SignatoryLink
getSignatoryLinkFromDocumentByID Document{documentsignatorylinks} signlinkid =
    find ((==) signlinkid . signatorylinkid) documentsignatorylinks
