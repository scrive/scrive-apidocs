{-# LANGUAGE NoImplicitPrelude #-}
module Doc.Action (
    postDocumentPreparationChange
  , postDocumentPendingChange
  , postDocumentRejectedChange
  , postDocumentCanceledChange
  , timeoutDocuments
  ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Logic
import Crypto.RNG
import DB
import Doc.DocSeal
import Doc.Model
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocView
import Doc.DocMails
import Doc.SignatoryLinkID
import InputValidation
import Kontra
import KontraLink
import OurPrelude
import User.Model
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import qualified Log
import Templates (runTemplatesT)
import Text.StringTemplates.Templates (TemplatesMonad)
import Util.Actor
import Util.SignatoryLinkUtils
import Util.MonadUtils
import ThirdPartyStats.Core
import ActionQueue.UserAccountRequest
import ActionQueue.Scheduler
import User.Action
import User.Utils
import Data.Maybe hiding (fromJust)
import ForkAction
import Doc.API.Callback.Model
import Company.Model
import Utils.Default (defaultValue)
import Doc.AutomaticReminder.Model
import DB.TimeZoneName

-- | Log a document event, adding some standard properties.
logDocEvent :: Kontrakcja m => EventName -> Document -> User -> [EventProperty] -> m ()
logDocEvent name doc user extraProps = do
  comp <- getCompanyForUser user
  now <- getMinutesTime
  ip <- ctxipnumber <$> getContext
  let uid = userid user
      email = Email $ getEmail user
      fullname = getFullName user
      deliverymethod = fromMaybe "undefined" $ show . signatorylinkdeliverymethod <$> getSigLinkFor doc uid
  asyncLogEvent name $ extraProps ++ [
    UserIDProp uid,
    DocIDProp  (documentid doc),
    TimeProp   now,
    MailProp   email,
    IPProp     ip,
    NameProp   fullname,
    stringProp "Company Name" $ getCompanyName $ comp,
    stringProp "Delivery Method" deliverymethod,
    stringProp "Type" (show $ documenttype doc),
    stringProp "Language" (show $ documentlang doc),
    numProp "Days to sign" (fromIntegral $ documentdaystosign doc),
    numProp "Signatories" (fromIntegral $ length $ documentsignatorylinks doc),
    stringProp "Signup Method" (show $ usersignupmethod user)]

postDocumentPreparationChange :: Kontrakcja m => Document -> Bool -> TimeZoneName -> m ()
postDocumentPreparationChange doc@Document{documenttitle} skipauthorinvitation tzn = do
  let docid = documentid doc
  triggerAPICallbackIfThereIsOne doc
  unless (isPending doc) $
    stateMismatchError "postDocumentPreparationChange" Pending doc
  Log.docevent $ "Preparation -> Pending; Sending invitation emails: " ++ show docid
  ctx <- getContext
  msaveddoc <- saveDocumentForSignatories doc
  document' <- case msaveddoc of
    Left msg -> do
      Log.error $ "Failed to save document #" ++ (show docid) ++ " for signatories " ++ msg
      return doc
    Right saveddoc -> return saveddoc
  Log.server $ "Sending invitation emails for document #" ++ show docid ++ ": " ++ documenttitle

  -- Stat logging
  now <- getMinutesTime
  author <- getDocAuthor doc
  docssent <- dbQuery $ GetDocsSent (userid author)
  -- Log the current time as the last doc sent time
  asyncLogEvent SetUserProps [UserIDProp (userid author),
                              someProp "Last Doc Sent" now,
                              numProp "Docs sent" (fromIntegral $ docssent)
                              ]
  json <- documentJSON Nothing False True False Nothing Nothing doc
  asyncLogEvent (UploadDocInfo json) [UserIDProp (userid author),
                                      DocIDProp (documentid doc)]
  logDocEvent "Doc Sent" doc author []

  sendInvitationEmails ctx document' skipauthorinvitation
  sendInvitationEmailsToViewers ctx document'
  scheduleAutoreminderIfThereIsOne document' tzn
  return ()

postDocumentPendingChange :: Kontrakcja m => Document -> Document -> m ()
postDocumentPendingChange doc@Document{documentid, documenttitle} olddoc = do
  triggerAPICallbackIfThereIsOne doc
  unless (isPending doc) $
    stateMismatchError "postDocumentPendingChange" Pending doc
  case undefined of
    _ | allSignatoriesSigned doc -> do
      Log.docevent $ "All have signed; " ++ show documentstatus ++ " -> Closed: " ++ show documentid
      ctx <- getContext
      let time = ctxtime ctx
      dbUpdate $ CloseDocument documentid (systemActor time)
      closeddoc <- dbQuery $ GetDocumentByDocumentID documentid

      Log.docevent $ "Pending -> Closed; Sending emails: " ++ show documentid
      author <- getDocAuthor doc
      logDocEvent "Doc Closed" doc author []
      asyncLogEvent SetUserProps [UserIDProp (userid author),
                                  someProp "Last Doc Closed" time]
      kCommit
      forkAction ("Sealing document #" ++ show documentid ++ ": " ++ documenttitle) $ do
        enewdoc <- sealDocument closeddoc
        case enewdoc of
          Just newdoc -> sendClosedEmails ctx newdoc False
          Nothing -> do
            Log.server $ "Sending seal error emails for document #" ++ show documentid ++ ": " ++ documenttitle
            sendDocumentErrorEmail closeddoc author
        return ()
    _ -> when (documentcurrentsignorder doc /= documentcurrentsignorder olddoc) $ do
      ctx <- getContext
      Log.server $ "Resending invitation emails for document #" ++ show documentid ++ ": " ++ documenttitle
      sendInvitationEmails ctx doc False
      return ()
  where
    allSignatoriesSigned = all (isSignatory =>>^ hasSigned) . documentsignatorylinks

postDocumentRejectedChange :: Kontrakcja m => Document -> SignatoryLinkID -> m ()
postDocumentRejectedChange doc@Document{..} siglinkid = do
  triggerAPICallbackIfThereIsOne doc
  unless (isRejected doc) $
    stateMismatchError "postDocumentRejectedChange" Rejected doc
  Log.docevent $ "Pending -> Rejected; send reject emails: " ++ show documentid
  Log.server $ "Sending rejection emails for document #" ++ show documentid ++ ": " ++ documenttitle
  ctx <- getContext
  -- Log the fact that the current user rejected a document.
  maybe (return ())
        (\user -> logDocEvent "Doc Rejected" doc user [])
        (ctxmaybeuser ctx)
  customMessage <- getOptionalField  asValidInviteText "customtext"
  sendRejectEmails customMessage ctx doc ($(fromJust) $ getSigLinkFor doc siglinkid)
  return ()

postDocumentCanceledChange :: Kontrakcja m => Document -> m ()
postDocumentCanceledChange doc@Document{..} = do
  triggerAPICallbackIfThereIsOne doc
  unless (isCanceled doc) $
    stateMismatchError "postDocumentCanceledChange" Canceled doc
  Log.docevent $ "Pending -> Canceled (ElegDataMismatch); Sending cancelation emails: " ++ show documentid
  -- if canceled because of ElegDataMismatch, send out emails
  author <- getDocAuthor doc
  let f sl = do
        msg <- signatorylinkelegdatamismatchmessage sl
        fn <- signatorylinkelegdatamismatchfirstname sl
        ln <- signatorylinkelegdatamismatchlastname sl
        pno <- signatorylinkelegdatamismatchpersonalnumber sl
        return (msg,fn,ln,pno)
  let issues = (catMaybes (map f (documentsignatorylinks)))
  mapM_ (\r -> logDocEvent "Doc Canceled" doc author [reasonProp r]) issues

  when (not (null issues)) $ do
      ctx <- getContext
      Log.server $ "Sending cancelation emails for document #" ++ show documentid ++ ": " ++ documenttitle
      sendElegDataMismatchEmails ctx doc author
  where
    reasonProp = stringProp "Reason" . show

stateMismatchError :: Kontrakcja m => String -> DocumentStatus -> Document -> m a
stateMismatchError funame expected Document{documentstatus, documentid} = do
  Log.debug $ funame ++ ": document #" ++ show documentid ++ " in " ++ show documentstatus ++ " state, expected " ++ show expected
  internalError

getDocAuthor :: Kontrakcja m => Document -> m User
getDocAuthor doc = do
  authorid <- guardJust $ getAuthorSigLink doc >>= maybesignatory
  guardJustM $ dbQuery $ GetUserByID authorid

{- |
    Goes through each signatory, and if a user exists this saves it for that user
    by linking the signatory to the user's account.
-}
saveDocumentForSignatories :: Kontrakcja m => Document -> m (Either String Document)
saveDocumentForSignatories doc =
  foldM foldSaveForSig (Right doc) . filter (not . isAuthor) $ (documentsignatorylinks doc)
  where
    {- |
        Wraps up the saveDocumentForSignatory so we can use it in a fold
    -}
    foldSaveForSig :: Kontrakcja m => (Either String Document) -> SignatoryLink -> m (Either String Document)
    foldSaveForSig (Left msg) _ = return $ Left msg
    foldSaveForSig (Right doc') siglink = saveDocumentForSignatory doc' siglink
    {- |
        Saves the document for the given signatorylink.  It does this by checking to see
        if there is a user with a matching email, and if there is it hooks up the signatory
        link to that user.
    -}
    saveDocumentForSignatory :: Kontrakcja m => Document -> SignatoryLink -> m (Either String Document)
    saveDocumentForSignatory doc' sl = do
      let sigemail = getEmail sl
      muser <- case (sigemail) of
                "" -> return Nothing
                _  -> dbQuery $ GetUserByEmail (Email sigemail)
      case muser of
        Nothing -> return $ Right doc'
        Just user -> do
          udoc <- do
            mdoc <- runMaybeT $ do
              True <- dbUpdate $ SaveDocumentForUser (documentid  doc') user (signatorylinkid sl)
              newdoc <- dbQuery $ GetDocumentByDocumentID (documentid  doc')
              return newdoc
            return $ maybe (Left "saveDocumentForSignatory failed") Right mdoc
          return udoc

{- |
   Try to sign up a new user. Returns the confirmation link for the new user.
   Nothing means there is already an account or there was an error creating the user.
 -}
handlePostSignSignup :: (CryptoRNG m, MonadDB m, TemplatesMonad m, HasMailContext c) => c -> Email -> String -> String -> String -> String -> m (Maybe KontraLink)
handlePostSignSignup ctx email fn ln cnm cnr = do
  let lang = mctxlang (mailContext ctx)
  muser <- dbQuery $ GetUserByEmail email
  case (muser, muser >>= userhasacceptedtermsofservice) of
    (Just user, Nothing) -> do
      -- there is an existing user that hasn't been activated
      -- return the existing link
      l <- newUserAccountRequestLink lang (userid user) BySigning
      return $ Just l
    (Nothing, Nothing) -> do
      -- this email address is new to the system, so create the user
      -- and send an invite
      company <- dbUpdate $ CreateCompany

      _ <- dbUpdate $ SetCompanyInfo (companyid company) $ (companyinfo company) {
                    companyname = cnm
                  , companynumber = cnr
              }
      mnewuser <- createUser' ctx email (fn, ln) (companyid company,True) lang
      case mnewuser of
        Nothing -> return Nothing
        Just newuser -> do
          l <- newUserAccountRequestLink lang (userid newuser) BySigning
          return $ Just l
    (_, _) -> return Nothing


-- | Time out documents once per day after midnight.  Do it in chunks
-- so that we don't choke the server in case there are many documents to time out
timeoutDocuments :: (MonadBaseControl IO m, MonadReader SchedulerData m, MonadIO m, MonadDB m) => m ()
timeoutDocuments = do
  now <- getMinutesTime
  docs <- dbQuery $ GetTimeoutedButPendingDocumentsChunk now 100
  forM_ docs $ \doc -> do
    gt <- getGlobalTemplates
    runTemplatesT (defaultValue, gt) $ dbUpdate $ TimeoutDocument (documentid doc) (systemActor now)
    triggerAPICallbackIfThereIsOne doc
    Log.debug $ "Document timedout " ++ (show $ documentid doc)
  when (not (null docs)) $ do
    kCommit
    timeoutDocuments
