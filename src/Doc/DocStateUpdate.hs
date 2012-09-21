module Doc.DocStateUpdate
    ( restartDocument
    , signDocumentWithEmailOrPad
    , signDocumentWithEleg
    , rejectDocumentWithChecks
    , authorSignDocument
    , authorSendDocument
    , setSigAttachments
    , authorSignDocumentFinal
    , signableFromTemplateWithUpdatedAuthor
    , updateDocAuthorAttachments
    , attachFile
    , newDocument
    ) where

import Control.Monad.Trans.Maybe
import MagicHash (MagicHash)
import DBError
import Doc.Model
import Doc.DocStateData
import Kontra
import Util.SignatoryLinkUtils
import Doc.DocStateQuery
import qualified Data.ByteString as BS
import Doc.DocUtils
import Control.Applicative
import User.Model
import Control.Monad.Trans
import Doc.Rendering
import User.Utils
import File.Model
import Redirect
import DB
import Stats.Control
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

import qualified Log

{- |
   Securely
 -}
restartDocument :: Kontrakcja m => Document -> m (Either DBError Document)
restartDocument doc = withUser $ \user -> do
  actor <- guardJustM $ mkAuthorActor <$> getContext
  if isSigLinkFor user $ getAuthorSigLink doc
    then do
      mnewdoc <- dbUpdate $ RestartDocument doc actor
      case mnewdoc of
        Nothing -> return $ Left DBResourceNotAvailable
        Just doc' -> return $ Right doc'
    else return $ Left DBResourceNotAvailable

{- |
   Sign a document with email identification (typical, non-eleg).
 -}

signDocumentWithEmailOrPad :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(FieldType, String)] -> m (Either DBError (Document, Document))
signDocumentWithEmailOrPad did slid mh fields = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddoc -> do
     switchLocale (getLocale olddoc)
     case olddoc `allowsAuthMethod` ELegAuthentication of
      True -> return $ Left (DBActionNotAvailable "This document does not allow signing using email authentication.")
      False  -> do
        Context{ ctxtime, ctxipnumber } <- getContext
        let Just sl' = getSigLinkFor olddoc slid
        let actor = signatoryActor ctxtime ctxipnumber (maybesignatory sl') (getEmail sl') slid
        mdoc <- runMaybeT $ do
          True <- dbUpdate $ UpdateFields did slid fields actor
          True <- dbUpdate $ SignDocument did slid mh Nothing actor
          Just doc <- dbQuery $ GetDocumentByDocumentID did
          let Just sl = getSigLinkFor doc slid
          _ <- addSignStatSignEvent doc sl
          return doc
        return $ case mdoc of
          Nothing  -> Left $ DBActionNotAvailable "Signing with email/pad failed"
          Just doc -> Right (doc, olddoc)

signDocumentWithEleg :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(FieldType, String)] -> SignatureInfo -> m (Either DBError (Document, Document))
signDocumentWithEleg did slid mh fields sinfo = do
  Context{ ctxtime, ctxipnumber } <- getContext
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddoc -> do
     switchLocale (getLocale olddoc)
     case olddoc `allowsAuthMethod` ELegAuthentication of
      False -> return $ Left (DBActionNotAvailable "This document does not allow signing using eleg authentication.")
      True  -> do
        let Just sl' = getSigLinkFor olddoc slid
        let actor = signatoryActor ctxtime ctxipnumber (maybesignatory sl') (getEmail sl') slid
        mdoc <- runMaybeT $ do
          Log.debug "a"
          True <- dbUpdate $ UpdateFields did slid fields actor
          Log.debug "b"
          True <- dbUpdate $ SignDocument did slid mh (Just sinfo) actor
          Log.debug "c"
          Just doc <- dbQuery $ GetDocumentByDocumentID did
          Log.debug "d"
          let Just sl = getSigLinkFor doc slid
          Log.debug "e"
          _ <- addSignStatSignEvent doc sl
          Log.debug "f"
          return doc
        return $ case mdoc of
          Nothing -> Left $ DBActionNotAvailable "Signing with Eleg failed"
          Just doc -> Right (doc, olddoc)

{- |
   Reject a document with security checks.
 -}
rejectDocumentWithChecks :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> Maybe String -> m (Either DBError Document)
rejectDocumentWithChecks did slid mh customtext = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddocument -> do
      switchLocale (getLocale olddocument)
      Context{ ctxtime, ctxipnumber } <- getContext
      let Just sll = getSigLinkFor olddocument slid
      let sa = signatoryActor ctxtime ctxipnumber (maybesignatory sll) (getEmail sll) slid
      mdoc <- runMaybeT $ do
        True <- dbUpdate $ RejectDocument did slid customtext sa
        Just doc <- dbQuery $ GetDocumentByDocumentID did
        let Just sl = getSigLinkFor doc slid
        _ <- addSignStatRejectEvent doc sl
        return doc
      return $ case mdoc of
        Nothing  -> Left $ DBActionNotAvailable "rejectDocumentWithChecks failed"
        Just doc -> Right doc

{- |
  The Author signs a document with security checks.
 -}
authorSignDocument :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
authorSignDocument did msigninfo = onlyAuthor did $ \olddoc -> do
  ctx <- getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink olddoc
  mdoc <- runMaybeT $ do
    True <- dbUpdate $ PreparationToPending did $ systemActor $ ctxtime ctx
    True <- dbUpdate $ SetDocumentInviteTime did (ctxtime ctx) actor
    -- please delete after Oct 1, 2012 -Eric
    -- True <- dbUpdate $ MarkInvitationRead did signatorylinkid $ systemActor $ ctxtime ctx
    -- True <- dbUpdate $ MarkDocumentSeen did signatorylinkid signatorymagichash actor
    True <- dbUpdate $ SignDocument did signatorylinkid signatorymagichash msigninfo actor
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    let Just sl = getSigLinkFor doc signatorylinkid
    _ <- addSignStatSignEvent doc sl
    return doc
  return $ case mdoc of
    Nothing  -> Left $ DBActionNotAvailable "authorSignDocument failed"
    Just doc -> Right doc

{- |
  The Author sends a document with security checks.
 -}
authorSendDocument :: (Kontrakcja m) => User -> Actor -> DocumentID -> m (Either DBError Document)
authorSendDocument user actor did = do
  ctx <- getContext
  Just doc <- dbQuery $ GetDocumentByDocumentID did
  if not $ isAuthor (doc, user) 
    then return $ Left DBResourceNotAvailable
    else do
        r1 <-  dbUpdate $ PreparationToPending did $ actor
        if (not r1)
           then return $ Left $ DBActionNotAvailable $ "Can't change from draft to pending"
           else do
             r2 <- dbUpdate $ SetDocumentInviteTime did (ctxtime ctx) actor
             if (not r2)
               then return $ Left $ DBActionNotAvailable $ "Can't send proper invitation time on document"
               else do
                 mdoc <- dbQuery $ GetDocumentByDocumentID did
                 return $ case mdoc of
                    Nothing  -> Left $ DBActionNotAvailable "authorSendDocument failed"
                    Just d -> Right d

{- |
  Reseting all signatory attachments when document is in preparation | State of document is not checked
 -}
setSigAttachments :: (Kontrakcja m) => DocumentID -> SignatoryLinkID -> [SignatoryAttachment] -> m (Either DBError ())
setSigAttachments did sid sigatts = onlyAuthor did $ \_ -> do
  actor <- guardJustM $ mkAuthorActor <$> getContext
  transActionNotAvailable <$> Right <$> dbUpdate (SetSigAttachments did sid sigatts actor)

{- |
   Only the author can Close a document when its in AwaitingAuthor status.
 -}
authorSignDocumentFinal :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
authorSignDocumentFinal did msigninfo = onlyAuthor did $ \olddoc -> do
  ctx <- getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink olddoc
  mdoc <- runMaybeT $ do
    True <- dbUpdate $ SignDocument did signatorylinkid signatorymagichash msigninfo actor
    True <- dbUpdate $ CloseDocument did $ systemActor $ ctxtime ctx
    Just doc <- dbQuery $ GetDocumentByDocumentID did
    let Just sl = getSigLinkFor doc signatorylinkid
    _ <- addSignStatSignEvent doc sl
    return doc
  return $ case mdoc of
    Nothing  -> Left $ DBActionNotAvailable "authorSignDocumentFinal failed"
    Just doc -> Right doc

-- | Make sure we're logged in as the author before taking action.
onlyAuthor :: (Kontrakcja m) => DocumentID -> (Document -> m (Either DBError a)) -> m (Either DBError a)
onlyAuthor did action = do
  edoc <- getDocByDocID did -- this makes sure we're the author or someone else with permissions in the company
  case edoc of
    Left e -> return $ Left e -- this checks if we're logged in
    Right doc -> do
      ctx <- getContext
      let Just user = ctxmaybeuser ctx
      if not $ isAuthor (doc, user) -- only the author should be allowed in
        then return $ Left DBResourceNotAvailable
        else action doc

{- |
 Create a signable from template with logged in user as the author.
 -}
signableFromTemplateWithUpdatedAuthor :: (Kontrakcja m) => DocumentID -> m (Either DBError Document)
signableFromTemplateWithUpdatedAuthor did = onlyAuthor did $ \_ -> do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  mcompany <- getCompanyForUser user
  actor <- guardJustM $ mkAuthorActor <$> getContext
  mdoc <- runMaybeT $ do
    Just doc <- dbUpdate $ SignableFromDocumentIDWithUpdatedAuthor user mcompany did actor
    return doc
  return $ case mdoc of
    Nothing  -> Left $ DBActionNotAvailable "signableFromTemplateWithUpdatedAuthor failed"
    Just doc -> Right doc

updateDocAuthorAttachments :: (Kontrakcja m) => DocumentID -> [FileID] -> [FileID] -> m (Either DBError Document)
updateDocAuthorAttachments did adds removes = onlyAuthor did $ \doc -> do
  case (adds ++ removes) of
    [] -> return $ Right doc
    _ -> do
      actor <- guardJustM $ mkAuthorActor <$> getContext
      res1 <- mapM (\a -> dbUpdate $ AddDocumentAttachment    did a actor) adds
      res2 <- mapM (\r -> dbUpdate $ RemoveDocumentAttachment did r actor) removes
      if and res1 && and res2
        then return $ Right doc
        else return err
  where
    err = Left $ DBActionNotAvailable "updateDocAuthorAttachments failed"

attachFile :: (Kontrakcja m) => DocumentID -> String -> BS.ByteString -> m (Either DBError Document)
attachFile docid filename content = onlyAuthor docid $ \_ -> do
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  content14 <- guardRightM $ liftIO $ preCheckPDF (ctxgscmd ctx) content
  file <- dbUpdate $ NewFile filename content14
  actor <- guardJustM $ mkAuthorActor <$> getContext
  mdoc <- runMaybeT $ do
    True <- dbUpdate $ AttachFile docid (fileid file) actor
    Just doc <- dbQuery $ GetDocumentByDocumentID docid
    return doc
  return $ case mdoc of
    Nothing  -> Left $ DBActionNotAvailable "attachFile failed"
    Just doc -> Right doc

newDocument :: (Kontrakcja m) => String -> DocumentType -> Int -> m (Either DBError Document)
newDocument title doctype nrOrOtherSignatories = withUser $ \user -> do
  mcompany <- getCompanyForUser user
  actor <- guardJustM $ mkAuthorActor <$> getContext
  mdoc <- dbUpdate $ NewDocument user mcompany title doctype nrOrOtherSignatories actor
  return $ case mdoc of
    Nothing  -> Left $ DBActionNotAvailable "newDocument failed"
    Just doc -> Right doc

withUser :: Kontrakcja m => (User -> m (Either DBError a)) -> m (Either DBError a)
withUser action = do
  Context{ ctxmaybeuser } <- getContext
  maybe (return $ Left DBNotLoggedIn) action ctxmaybeuser

