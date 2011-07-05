module Doc.DocStateUpdate
    ( restartDocument
    , markDocumentSeen
    , signDocumentWithEmail
    , rejectDocumentWithChecks
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

