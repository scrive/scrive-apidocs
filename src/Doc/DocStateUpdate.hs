module Doc.DocStateUpdate
    ( restartDocument
    , markDocumentSeen
    , signDocumentWithEmail
    , rejectDocumentWithChecks
    , authorSignDocument
    , authorSendDocument
    , updateSigAttachments
    , closeDocument
    ) where

import DBError
import Doc.DocState
import Kontra
import Misc
import Happstack.State     (update)
import MinutesTime
import GHC.Word
import Util.SignatoryLinkUtils
import Doc.DocStateQuery
import qualified Data.ByteString as BS
import Doc.DocUtils
import Control.Applicative

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
restartDocument doc= do
  Context { ctxtime
          , ctxipnumber
          , ctxmaybeuser
          } <- getContext
  case ctxmaybeuser of
    Nothing   -> return $ Left DBNotLoggedIn
    Just user -> case getAuthorSigLink doc of
      Just authorsiglink | isSigLinkFor user authorsiglink -> do
        enewdoc <- update $ RestartDocument doc user ctxtime ctxipnumber
        case enewdoc of
          Right newdoc -> return $ Right newdoc
          _            -> return $ Left DBResourceNotAvailable
      _ -> return $ Left DBResourceNotAvailable

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
        newdocument <- update $ SignDocument did slid ctxtime ctxipnumber Nothing fields
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
  transActionNotAvailable <$> update (AuthorSignDocument did (ctxtime ctx) (ctxipnumber ctx) msigninfo)

{- |
  The Author sends a document with security checks.
 -}
authorSendDocument :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
authorSendDocument did msigninfo = onlyAuthor did $ do
  ctx <- getContext
  transActionNotAvailable <$> update (AuthorSendDocument did (ctxtime ctx) (ctxipnumber ctx) msigninfo)

{- |
  The Author can add new SigAttachments.
 -}
updateSigAttachments :: (Kontrakcja m) => DocumentID -> [SignatoryAttachment] -> m (Either DBError Document)
updateSigAttachments did sigatts = onlyAuthor did $ do
  transActionNotAvailable <$> update (UpdateSigAttachments did sigatts)
    
    
eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe _ (Just b) = Right b
eitherFromMaybe a Nothing  = Left a
    
{- |
   Only the author can Close a document when its in AwaitingAuthor status.
 -}
closeDocument :: (Kontrakcja m) => DocumentID -> Maybe SignatureInfo -> m (Either DBError Document)
closeDocument did msigninfo = onlyAuthor did $ do
  ctx <- getContext
  eitherFromMaybe DBResourceNotAvailable <$> update (CloseDocument did (ctxtime ctx) (ctxipnumber ctx) msigninfo)

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
