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

import MagicHash (MagicHash)
import DBError
import Doc.Model
import Doc.DocStateData
import Kontra
--import MinutesTime
--import Misc
import Util.SignatoryLinkUtils
import Doc.DocStateQuery
import qualified Data.ByteString as BS
import Doc.DocUtils
import Control.Applicative
import User.Model
import Control.Monad.Trans
import Doc.DocStorage
import User.Utils
import File.Model
import Redirect
import DB.Classes
import Data.Either
import Stats.Control
import EvidenceLog.Model
import Util.HasSomeUserInfo
import Util.MonadUtils

{- |
   Securely
 -}
restartDocument :: Kontrakcja m => Document -> m (Either DBError Document)
restartDocument doc = withUser $ \user -> do
  actor <- guardJustM $ mkAuthorActor <$> getContext
  if isSigLinkFor user $ getAuthorSigLink doc
    then do
      enewdoc <- dbUpdate $ RestartDocument doc actor
      case enewdoc of
        Left _ -> return $ Left DBResourceNotAvailable
        Right doc' -> return $ Right doc'
    else return $ Left DBResourceNotAvailable

{- |
   Sign a document with email identification (typical, non-eleg).
 -}

signDocumentWithEmailOrPad :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(String, String)] -> m (Either DBError (Document, Document))
signDocumentWithEmailOrPad did slid mh fields = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddoc -> do
     switchLocale (getLocale olddoc)
     case olddoc `allowsIdentification` EmailIdentification || olddoc `allowsIdentification` PadIdentification of
      False -> return $ Left (DBActionNotAvailable "This document does not allow signing using email identification.")
      True  -> do
        Context{ ctxtime, ctxipnumber } <- getContext
        let Just sl' = getSigLinkFor olddoc slid
        let actor = SignatoryActor ctxtime ctxipnumber (maybesignatory sl') (getEmail sl') slid
        ed1 <- dbUpdate $ UpdateFields did slid fields actor
        case ed1 of
          Left err -> return $ Left $ DBActionNotAvailable err
          Right _ -> do
            newdocument <- dbUpdate $ SignDocument did slid mh Nothing actor
            case newdocument of
              Left message -> return $ Left (DBActionNotAvailable message)
              Right doc -> do
                _ <- case getSigLinkFor doc slid of
                  Just sl -> addSignStatSignEvent doc sl
                  _ -> return False
                return $ Right (doc, olddoc)


signDocumentWithEleg :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(String, String)] -> SignatureInfo -> m (Either DBError (Document, Document))
signDocumentWithEleg did slid mh fields sinfo = do
  Context{ ctxtime, ctxipnumber } <- getContext
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddoc -> do
     switchLocale (getLocale olddoc)
     case olddoc `allowsIdentification` ELegitimationIdentification of
      False -> return $ Left (DBActionNotAvailable "This document does not allow signing using email identification.")
      True  -> do
        let Just sl' = getSigLinkFor olddoc slid
        let actor = SignatoryActor ctxtime ctxipnumber (maybesignatory sl') (getEmail sl') slid
        ed1 <- dbUpdate $ UpdateFields did slid fields actor
        case ed1 of
          Left err -> return $ Left $ DBActionNotAvailable err
          Right _ -> do
            newdocument <- dbUpdate $ SignDocument did slid mh (Just sinfo) actor
            case newdocument of
              Left message -> return $ Left (DBActionNotAvailable message)
              Right doc -> do
                _ <- case getSigLinkFor doc slid of
                  Just sl -> addSignStatSignEvent doc sl
                  _ -> return False
                return $ Right (doc, olddoc)

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
      let sa = SignatoryActor ctxtime ctxipnumber (maybesignatory sll) (getEmail sll) slid
      mdocument <- dbUpdate $ RejectDocument did slid customtext sa
      case mdocument of
        Left msg -> return $ Left (DBActionNotAvailable msg)
        Right document -> do
          _ <- case getSigLinkFor document slid of
            Just sl -> addSignStatRejectEvent document sl
            _       -> return False
          return $ Right document

{- |
  The Author signs a document with security checks.
 -}
authorSignDocument :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
authorSignDocument did msigninfo = onlyAuthor did $ do
  ctx <- getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  edoc <- getDocByDocID did
  case edoc of
    Left m -> return $ Left m
    Right doc -> do
      let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
      ed1 <- dbUpdate (PreparationToPending did (SystemActor (ctxtime ctx)))
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right _ -> do
          _ <- dbUpdate $ SetDocumentInviteTime did (ctxtime ctx) actor
          _ <- dbUpdate $ MarkInvitationRead did signatorylinkid (SystemActor (ctxtime ctx))
          ed2 <- dbUpdate $ MarkDocumentSeen did signatorylinkid signatorymagichash actor
          case ed2 of
            Left m -> return $ Left $ DBActionNotAvailable m
            Right _ -> do
              ed3 <- dbUpdate (SignDocument did signatorylinkid signatorymagichash msigninfo actor)
              case ed3 of
                Left m -> return $ Left $ DBActionNotAvailable m
                Right d3 -> do
                  _ <- case getSigLinkFor d3 signatorylinkid of
                    Just sl -> addSignStatSignEvent d3 sl
                    _ -> return False
                  return $ Right d3

{- |
  The Author sends a document with security checks.
 -}
authorSendDocument :: (Kontrakcja m) => DocumentID -> m (Either DBError Document)
authorSendDocument did = onlyAuthor did $ do
  ctx <- getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  edoc <- getDocByDocID did
  case edoc of
    Left m -> return $ Left m
    Right _ -> do
      ed1 <- dbUpdate (PreparationToPending did (SystemActor (ctxtime ctx)))
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right doc -> do
          let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
          _ <- dbUpdate $ SetDocumentInviteTime did (ctxtime ctx) actor
          _ <- dbUpdate $ MarkInvitationRead did signatorylinkid (SystemActor (ctxtime ctx))
          transActionNotAvailable <$> dbUpdate (MarkDocumentSeen did signatorylinkid signatorymagichash actor)

{- |
  Reseting all signatory attachments when document is in preparation | State of document is not checked
 -}
setSigAttachments :: (Kontrakcja m) => DocumentID -> SignatoryLinkID -> [SignatoryAttachment] -> m (Either DBError ())
setSigAttachments did sid sigatts = onlyAuthor did $ do
  actor <- guardJustM $ mkAuthorActor <$> getContext
  transActionNotAvailable <$> Right <$> dbUpdate (SetSigAttachments did sid sigatts actor)

{- |
   Only the author can Close a document when its in AwaitingAuthor status.
 -}
authorSignDocumentFinal :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
authorSignDocumentFinal did msigninfo = onlyAuthor did $ do
  ctx <- getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  edoc <- getDocByDocID did
  case edoc of
    Left m -> return $ Left m
    Right doc -> do
      let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
      ed1 <- dbUpdate (SignDocument did signatorylinkid signatorymagichash msigninfo actor)
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right d1 -> do
          _ <- case getSigLinkFor d1 signatorylinkid of
            Just sl -> addSignStatSignEvent d1 sl
            _ -> return False
          ed2 <- dbUpdate (CloseDocument did (SystemActor (ctxtime ctx)))
          return $ transActionNotAvailable ed2


-- | Make sure we're logged in as the author before taking action.
onlyAuthor :: (Kontrakcja m) => DocumentID -> m (Either DBError a) -> m (Either DBError a)
onlyAuthor did action = do
  edoc <- getDocByDocID did -- this makes sure we're the author or someone else with permissions in the company
  case edoc of
    Left e -> return $ Left e -- this checks if we're logged in
    Right doc -> do
      ctx <- getContext
      let Just user = ctxmaybeuser ctx
      if not $ isAuthor (doc, user) -- only the author should be allowed in
        then return $ Left DBResourceNotAvailable
        else action

{- |
 Create a signable from template with logged in user as the author.
 -}
signableFromTemplateWithUpdatedAuthor :: (Kontrakcja m) => DocumentID -> m (Either DBError Document)
signableFromTemplateWithUpdatedAuthor did = onlyAuthor did $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  mcompany <- getCompanyForUser user
  actor <- guardJustM $ mkAuthorActor <$> getContext
  transActionNotAvailable <$> dbUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany did actor)

updateDocAuthorAttachments :: (Kontrakcja m) => DocumentID -> [FileID] -> [FileID] -> m (Either DBError Document)
updateDocAuthorAttachments did adds removes = onlyAuthor did $ do
  case (adds ++ removes) of
    [] -> getDocByDocID did
    _ -> do
      actor <- guardJustM $ mkAuthorActor <$> getContext
      res1 <- mapM (\a -> dbUpdate $ AddDocumentAttachment    did a actor) adds
      res2 <- mapM (\r -> dbUpdate $ RemoveDocumentAttachment did r actor) removes
      let ls = lefts (res1 ++ res2)
          rs = rights (res1 ++ res2)
      case ls of
        [] -> return $ Right $ last rs
        (a:_) -> return $ Left $ DBActionNotAvailable a

attachFile :: (Kontrakcja m) => DocumentID -> String -> BS.ByteString -> m (Either DBError Document)
attachFile docid filename content = onlyAuthor docid $ do
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  content14 <- guardRightM $ liftIO $ preCheckPDF (ctxgscmd ctx) content
  file <- dbUpdate $ NewFile filename content14
  actor <- guardJustM $ mkAuthorActor <$> getContext
  transActionNotAvailable <$> dbUpdate (AttachFile docid (fileid file) actor)

newDocument :: (Kontrakcja m) => String -> DocumentType -> Int -> m (Either DBError Document)
newDocument title doctype nrOrOtherSignatories = withUser $ \user -> do
  mcompany <- getCompanyForUser user
  actor <- guardJustM $ mkAuthorActor <$> getContext
  transActionNotAvailable <$> dbUpdate (NewDocument user mcompany title doctype nrOrOtherSignatories actor)

withUser :: Kontrakcja m => (User -> m (Either DBError a)) -> m (Either DBError a)
withUser action = do
  Context{ ctxmaybeuser } <- getContext
  maybe (return $ Left DBNotLoggedIn) action ctxmaybeuser

