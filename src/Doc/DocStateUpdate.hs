module Doc.DocStateUpdate
    ( restartDocument
    , signDocumentWithEmail
    , signDocumentWithEleg
    , rejectDocumentWithChecks
    , authorSignDocument
    , authorSendDocument
    , updateSigAttachments
    , authorSignDocumentFinal
    , signableFromTemplateWithUpdatedAuthor
    , updateDocAuthorAttachments
    , attachFile
    , newDocument
    ) where

import DB.Types
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

import qualified Data.ByteString.UTF8 as BS hiding (length)

{- |
   Securely
 -}
restartDocument :: Kontrakcja m => Document -> m (Either DBError Document)
restartDocument doc = withUser $ \user -> do
  Context { ctxtime
          , ctxipnumber } <- getContext
  if isSigLinkFor user $ getAuthorSigLink doc
    then do
      let actor = AuthorActor ctxtime ctxipnumber (userid user) (BS.toString $ getEmail user)
      enewdoc <- runDBUpdate $ RestartDocument doc actor
      case enewdoc of
        Left _ -> return $ Left DBResourceNotAvailable
        Right doc' -> return $ Right doc'
    else return $ Left DBResourceNotAvailable

{- |
   Sign a document with email identification (typical, non-eleg).
 -}
signDocumentWithEmail :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(BS.ByteString, BS.ByteString)] -> m (Either DBError (Document, Document))
signDocumentWithEmail did slid mh fields = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddoc -> do
     switchLocale (getLocale olddoc)
     case olddoc `allowsIdentification` EmailIdentification of
      False -> return $ Left (DBActionNotAvailable "This document does not allow signing using email identification.")
      True  -> do
        Context{ ctxtime, ctxipnumber } <- getContext
        let Just sl' = getSigLinkFor olddoc slid
        let actor = SignatoryActor ctxtime ctxipnumber (maybesignatory sl') (BS.toString $ getEmail sl') slid
        ed1 <- runDBUpdate $ UpdateFields did slid fields actor
        case ed1 of
          Left err -> return $ Left $ DBActionNotAvailable err
          Right _ -> do
            newdocument <- runDBUpdate $ SignDocument did slid mh Nothing actor
            case newdocument of
              Left message -> return $ Left (DBActionNotAvailable message)
              Right doc -> do
                _ <- case getSigLinkFor doc slid of
                  Just sl -> runDB $ addSignStatSignEvent doc sl
                  _ -> return False
                return $ Right (doc, olddoc)


signDocumentWithEleg :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(BS.ByteString, BS.ByteString)] -> SignatureInfo -> m (Either DBError (Document, Document))
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
        let actor = SignatoryActor ctxtime ctxipnumber (maybesignatory sl') (BS.toString $ getEmail sl') slid
        ed1 <- runDBUpdate $ UpdateFields did slid fields actor
        case ed1 of
          Left err -> return $ Left $ DBActionNotAvailable err
          Right _ -> do
            newdocument <- runDBUpdate $ SignDocument did slid mh (Just sinfo) actor
            case newdocument of
              Left message -> return $ Left (DBActionNotAvailable message)
              Right doc -> do
                _ <- case getSigLinkFor doc slid of
                  Just sl -> runDB $ addSignStatSignEvent doc sl
                  _ -> return False
                return $ Right (doc, olddoc)

{- |
   Reject a document with security checks.
 -}
rejectDocumentWithChecks :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> Maybe BS.ByteString -> m (Either DBError (Document, Document))
rejectDocumentWithChecks did slid mh customtext = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddocument -> do
      switchLocale (getLocale olddocument)
      Context{ ctxtime, ctxipnumber } <- getContext
      let Just sll = getSigLinkFor olddocument slid
      let sa = SignatoryActor ctxtime ctxipnumber (maybesignatory sll) (BS.toString $ getEmail sll) slid
      mdocument <- runDBUpdate $ RejectDocument did slid customtext sa
      case mdocument of
        Left msg -> return $ Left (DBActionNotAvailable msg)
        Right document -> do
          _ <- case getSigLinkFor document slid of
            Just sl -> runDB $ addSignStatRejectEvent document sl
            _       -> return False
          return $ Right (document, olddocument)

{- |
  The Author signs a document with security checks.
 -}
authorSignDocument :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
authorSignDocument did msigninfo = onlyAuthor did $ do
  ctx <- getContext
  let Just author = ctxmaybeuser ctx
  edoc <- getDocByDocID did
  case edoc of
    Left m -> return $ Left m
    Right doc -> do
      let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
      ed1 <- runDBUpdate (PreparationToPending did (SystemActor (ctxtime ctx)))
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right _ -> do
          let actor = AuthorActor (ctxtime ctx) (ctxipnumber ctx) (userid author) (BS.toString $ getEmail author)          
          _ <- runDBUpdate $ SetDocumentInviteTime did (ctxtime ctx) actor
          _ <- runDBUpdate $ MarkInvitationRead did signatorylinkid (SystemActor (ctxtime ctx))
          ed2 <- runDBUpdate $ MarkDocumentSeen did signatorylinkid signatorymagichash actor
          case ed2 of
            Left m -> return $ Left $ DBActionNotAvailable m
            Right _ -> do
              ed3 <- runDBUpdate (SignDocument did signatorylinkid signatorymagichash msigninfo actor)
              case ed3 of
                Left m -> return $ Left $ DBActionNotAvailable m
                Right d3 -> do
                  _ <- case getSigLinkFor d3 signatorylinkid of
                    Just sl -> runDB $ addSignStatSignEvent d3 sl
                    _ -> return False
                  return $ Right d3

{- |
  The Author sends a document with security checks.
 -}
authorSendDocument :: (Kontrakcja m) => DocumentID -> m (Either DBError Document)
authorSendDocument did = onlyAuthor did $ do
  ctx <- getContext
  let Just author = ctxmaybeuser ctx
  edoc <- getDocByDocID did
  case edoc of
    Left m -> return $ Left m
    Right doc -> do
      let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
      ed1 <- runDBUpdate (PreparationToPending did (SystemActor (ctxtime ctx)))
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right _ -> do
          let actor = AuthorActor (ctxtime ctx) (ctxipnumber ctx) (userid author) (BS.toString $ getEmail author)
          _ <- runDBUpdate $ SetDocumentInviteTime did (ctxtime ctx) actor
          _ <- runDBUpdate $ MarkInvitationRead did signatorylinkid (SystemActor (ctxtime ctx))
          transActionNotAvailable <$> runDBUpdate (MarkDocumentSeen did signatorylinkid signatorymagichash actor)

{- |
  The Author can add new SigAttachments.
 -}
updateSigAttachments :: (Kontrakcja m) => DocumentID -> [SignatoryAttachment] -> m (Either DBError Document)
updateSigAttachments did sigatts = onlyAuthor did $ do
  Context{ctxtime, ctxipnumber, ctxmaybeuser = Just author} <- getContext
  let actor = AuthorActor ctxtime ctxipnumber (userid author) (BS.toString $ getEmail author)
  transActionNotAvailable <$> runDBUpdate (UpdateSigAttachments did sigatts actor)

{- |
   Only the author can Close a document when its in AwaitingAuthor status.
 -}
authorSignDocumentFinal :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
authorSignDocumentFinal did msigninfo = onlyAuthor did $ do
  ctx <- getContext
  edoc <- getDocByDocID did
  case edoc of
    Left m -> return $ Left m
    Right doc -> do
      let Just user = ctxmaybeuser ctx
          Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
          actor = AuthorActor (ctxtime ctx) (ctxipnumber ctx) (userid user) (BS.toString $ getEmail user)
      ed1 <- runDBUpdate (SignDocument did signatorylinkid signatorymagichash msigninfo actor)
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right d1 -> do
          _ <- case getSigLinkFor d1 signatorylinkid of
            Just sl -> runDB $ addSignStatSignEvent d1 sl
            _ -> return False
          ed2 <- runDBUpdate (CloseDocument did (SystemActor (ctxtime ctx)))
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
  Context{ ctxmaybeuser = Just user, ctxtime, ctxipnumber} <- getContext
  mcompany <- getCompanyForUser user
  let actor = AuthorActor ctxtime ctxipnumber (userid user) (BS.toString $ getEmail user)
  transActionNotAvailable <$> runDBUpdate (SignableFromDocumentIDWithUpdatedAuthor user mcompany did actor)

updateDocAuthorAttachments :: (Kontrakcja m) => DocumentID -> [FileID] -> [FileID] -> m (Either DBError Document)
updateDocAuthorAttachments did adds removes = onlyAuthor did $ do
  case (adds ++ removes) of
    [] -> getDocByDocID did
    _ -> do
      Just user <- ctxmaybeuser <$> getContext
      time      <- ctxtime      <$> getContext
      ip        <- ctxipnumber  <$> getContext
      let actor = AuthorActor time ip (userid user) (BS.toString $ getEmail user)
      res1 <- mapM (\a -> runDBUpdate $ AddDocumentAttachment    did a actor) adds
      res2 <- mapM (\r -> runDBUpdate $ RemoveDocumentAttachment did r actor) removes
      let ls = lefts (res1 ++ res2)
          rs = rights (res1 ++ res2)
      case ls of
        [] -> return $ Right $ last rs
        (a:_) -> return $ Left $ DBActionNotAvailable a

attachFile :: (Kontrakcja m) => DocumentID -> BS.ByteString -> BS.ByteString -> m (Either DBError Document)
attachFile docid filename content = onlyAuthor docid $ do
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  content14 <- guardRightM $ liftIO $ preCheckPDF (ctxgscmd ctx) content
  file <- runDB $ dbUpdate $ NewFile filename content14
  let Just user = ctxmaybeuser ctx
  let actor = AuthorActor (ctxtime ctx) (ctxipnumber ctx) (userid user) (BS.toString $ getEmail user)
  transActionNotAvailable <$> runDBUpdate (AttachFile docid (fileid file) actor)

newDocument :: (Kontrakcja m) => BS.ByteString -> DocumentType -> m (Either DBError Document)
newDocument title doctype = withUser $ \user -> do
  Context{ ctxtime, ctxipnumber } <- getContext
  mcompany <- getCompanyForUser user
  let aa = AuthorActor ctxtime ctxipnumber (userid user) (BS.toString $ getEmail user)
  transActionNotAvailable <$> runDBUpdate (NewDocument user mcompany title doctype aa)

withUser :: Kontrakcja m => (User -> m (Either DBError a)) -> m (Either DBError a)
withUser action = do
  Context{ ctxmaybeuser } <- getContext
  maybe (return $ Left DBNotLoggedIn) action ctxmaybeuser

