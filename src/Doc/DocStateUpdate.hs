module Doc.DocStateUpdate
    ( restartDocument
    , markDocumentSeen
    , signDocumentWithEmail
    , rejectDocumentWithChecks
    , authorSignDocument
    , authorSendDocument
    , updateSigAttachments
    , closeDocument
    , signableFromTemplateWithUpdatedAuthor
    , updateDocAuthorAttachments
    , attachFile
    , newDocument
    , shareDocuments
    ) where

import DB.Types
import DBError
import Doc.DocState
import Kontra
import Happstack.State     (update)
import MinutesTime
import GHC.Word
import Util.SignatoryLinkUtils
import Doc.DocStateQuery
import qualified Data.ByteString as BS
import Doc.DocUtils
import Control.Applicative
import User.Model
import Control.Monad.Trans
import Doc.DocStorage
import User.Utils
import File.TransState
import DB.Classes

{- |
   Mark document seen securely.
 -}
markDocumentSeen :: Kontrakcja m
                 => DocumentID
                 -> SignatoryLinkID
                 -> MagicHash
                 -> MinutesTime.MinutesTime
                 -> GHC.Word.Word32
                 -> m (Either String Document)
markDocumentSeen docid sigid mh time ipnum =
  update $ MarkDocumentSeen docid sigid mh time ipnum

{- |
   Securely
 -}
restartDocument :: Kontrakcja m => Document -> m (Either DBError Document)
restartDocument doc = withUser $ \user -> do
  Context { ctxtime
          , ctxipnumber } <- getContext
  if isSigLinkFor user $ getAuthorSigLink doc
    then do
      enewdoc <- update $ RestartDocument doc user ctxtime ctxipnumber
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
    Right olddoc -> case olddoc `allowsIdentification` EmailIdentification of
      False -> return $ Left (DBActionNotAvailable "This document does not allow signing using email identification.")
      True  -> do
        Context{ ctxtime, ctxipnumber } <- getContext
        ed1 <- update $ UpdateFields did slid fields
        case ed1 of
          Left err -> return $ Left $ DBActionNotAvailable err
          Right _ -> do
            newdocument <- update $ SignDocument did slid mh ctxtime ctxipnumber Nothing
            case newdocument of
              Left message -> return $ Left (DBActionNotAvailable message)
              Right doc -> return $ Right (doc, olddoc)
            
{- |
   Reject a document with security checks.
 -}
rejectDocumentWithChecks :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> Maybe BS.ByteString -> m (Either DBError (Document, Document))
rejectDocumentWithChecks did slid mh customtext = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddocument -> do
      Context{ ctxtime, ctxipnumber } <- getContext
      mdocument <- update $ RejectDocument did slid ctxtime ctxipnumber customtext
      case mdocument of
        Left msg -> return $ Left (DBActionNotAvailable msg)
        Right document -> return $ Right (document, olddocument)

{- |
  The Author signs a document with security checks.
 -}
authorSignDocument :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
authorSignDocument did msigninfo = onlyAuthor did $ do
  ctx <- getContext
  edoc <- getDocByDocID did
  case edoc of 
    Left m -> return $ Left m
    Right doc -> do
      let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
      ed1 <- update (PreparationToPending did (ctxtime ctx))
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right _ -> do
          update $ MarkInvitationRead did signatorylinkid (ctxtime ctx)
          ed2 <- update $ MarkDocumentSeen did signatorylinkid signatorymagichash (ctxtime ctx) (ctxipnumber ctx)
          case ed2 of
            Left m -> return $ Left $ DBActionNotAvailable m
            Right _ -> 
              transActionNotAvailable <$> update (SignDocument did signatorylinkid signatorymagichash (ctxtime ctx) (ctxipnumber ctx) msigninfo)

{- |
  The Author sends a document with security checks.
 -}
authorSendDocument :: (Kontrakcja m) => DocumentID -> m (Either DBError Document)
authorSendDocument did = onlyAuthor did $ do
  ctx <- getContext
  edoc <- getDocByDocID did
  case edoc of 
    Left m -> return $ Left m
    Right doc -> do
      let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
      ed1 <- update (PreparationToPending did (ctxtime ctx))
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right _ -> do
          update $ MarkInvitationRead did signatorylinkid (ctxtime ctx)
          transActionNotAvailable <$> update (MarkDocumentSeen did signatorylinkid signatorymagichash (ctxtime ctx) (ctxipnumber ctx))
  
{- |
  The Author can add new SigAttachments.
 -}
updateSigAttachments :: (Kontrakcja m) => DocumentID -> [SignatoryAttachment] -> m (Either DBError Document)
updateSigAttachments did sigatts = onlyAuthor did $ do
  transActionNotAvailable <$> update (UpdateSigAttachments did sigatts)
        
{- |
   Only the author can Close a document when its in AwaitingAuthor status.
 -}
closeDocument :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
closeDocument did msigninfo = onlyAuthor did $ do
  ctx <- getContext
  edoc <- getDocByDocID did
  case edoc of
    Left m -> return $ Left m
    Right doc -> do
      let Just (SignatoryLink{signatorylinkid, signatorymagichash}) = getAuthorSigLink doc
      ed1 <- update (SignDocument did signatorylinkid signatorymagichash (ctxtime ctx) (ctxipnumber ctx) msigninfo)
      case ed1 of
        Left m -> return $ Left $ DBActionNotAvailable m
        Right _ -> transActionNotAvailable <$> update (CloseDocument did (ctxtime ctx) (ctxipnumber ctx))
          

-- | Make sure we're logged in as the author before taking action.
onlyAuthor :: (Kontrakcja m) => DocumentID -> m (Either DBError a) -> m (Either DBError a)
onlyAuthor did action = do
  edoc <- getDocByDocID did -- this makes sure we're the author or his friend
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
  Context{ ctxmaybeuser = Just user} <- getContext
  mcompany <- getCompanyForUser user
  transActionNotAvailable <$> update (SignableFromDocumentIDWithUpdatedAuthor user mcompany did)

updateDocAuthorAttachments :: (Kontrakcja m) => DocumentID -> [FileID] -> [FileID] -> m (Either DBError Document)
updateDocAuthorAttachments did adds removes = onlyAuthor did $ do
  transActionNotAvailable <$> update (UpdateDocumentAttachments did adds removes)

attachFile :: (Kontrakcja m) => DocumentID -> BS.ByteString -> BS.ByteString -> m (Either DBError Document)
attachFile docid filename content = onlyAuthor docid $ do
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  content14 <- liftIO $ preprocessPDF ctx content docid
  file <- runDB $ dbUpdate $ NewFile filename content14
  transActionNotAvailable <$> update (AttachFile docid (fileid file))

newDocument :: (Kontrakcja m) => BS.ByteString -> DocumentType -> m (Either DBError Document)
newDocument title doctype = withUser $ \user -> do
  Context{ ctxtime } <- getContext
  mcompany <- getCompanyForUser user
  transActionNotAvailable <$> update (NewDocument user mcompany title doctype ctxtime)

-- | Share documents where logged in user is author
shareDocuments :: Kontrakcja m => [DocumentID] -> m (Either DBError [Document])
shareDocuments dids = sequence <$> mapM shareDocument dids

shareDocument :: Kontrakcja m => DocumentID -> m (Either DBError Document)
shareDocument did = onlyAuthor did $ do
  edoc <- update $ ShareDocument did
  either (\_ -> return $ Left $ DBResourceNotAvailable)
         (return . Right)
         edoc

withUser :: Kontrakcja m => (User -> m (Either DBError a)) -> m (Either DBError a)
withUser action = do
  Context{ ctxmaybeuser } <- getContext
  maybe (return $ Left DBNotLoggedIn) action ctxmaybeuser

