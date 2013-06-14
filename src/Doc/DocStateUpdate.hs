module Doc.DocStateUpdate
    ( restartDocument
    , prolongDocument
    , signDocumentWithEmailOrPad
    , signDocumentWithEleg
    ) where

import Control.Monad.Trans.Maybe
import MagicHash (MagicHash)
import DBError
import Doc.Model
import Doc.DocStateData
import Kontra
import Util.SignatoryLinkUtils
import Doc.DocStateQuery
import Doc.SignatoryLinkID
import Control.Applicative
import Doc.DocumentID
import User.Model
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import DB
import Util.Actor
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
      mnewdoc <- dbUpdate $ RestartDocument doc actor
      case mnewdoc of
        Nothing -> return $ Left DBResourceNotAvailable
        Just doc' -> return $ Right doc'
    else return $ Left DBResourceNotAvailable

{- |
   Securely
 -}
prolongDocument :: Kontrakcja m => Document -> m (Either DBError ())
prolongDocument doc = withUser $ \user -> do
  actor <- guardJustM $ mkAuthorActor <$> getContext
  if isSigLinkFor user $ getAuthorSigLink doc
    then do
      dbUpdate $ ProlongDocument (documentid doc) actor
      return (Right ())
    else return $ Left DBResourceNotAvailable

{- |
   Sign a document with email identification (typical, non-eleg).
 -}

signDocumentWithEmailOrPad :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(FieldType, String)] -> SignatoryScreenshots.SignatoryScreenshots
                           -> m (Either DBError (Document, Document))
signDocumentWithEmailOrPad did slid mh fields screenshots = do
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddoc -> do
     switchLang (getLang olddoc)
     let Just sl' = getSigLinkFor olddoc slid
     case signatorylinkauthenticationmethod sl' == ELegAuthentication of
      True -> return $ Left (DBActionNotAvailable "This document does not allow signing using email authentication.")
      False  -> do
        Context{ ctxtime, ctxipnumber } <- getContext
        let actor = signatoryActor ctxtime ctxipnumber (maybesignatory sl') (getEmail sl') slid
        mdoc <- runMaybeT $ do
          dbUpdate $ UpdateFieldsForSigning did slid fields actor
          dbUpdate $ SignDocument did slid mh Nothing screenshots actor
          Just doc <- dbQuery $ GetDocumentByDocumentID did

          return doc
        return $ case mdoc of
          Nothing  -> Left $ DBActionNotAvailable "Signing with email/pad failed"
          Just doc -> Right (doc, olddoc)

signDocumentWithEleg :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(FieldType, String)] -> SignatureInfo -> SignatoryScreenshots.SignatoryScreenshots
                     -> m (Either DBError (Document, Document))
signDocumentWithEleg did slid mh fields sinfo screenshots = do
  Context{ ctxtime, ctxipnumber } <- getContext
  edoc <- getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left err -> return $ Left err
    Right olddoc -> do
     switchLang (getLang olddoc)
     let Just sl' = getSigLinkFor olddoc slid
     case signatorylinkauthenticationmethod sl' == ELegAuthentication of
      False -> return $ Left (DBActionNotAvailable "This document does not allow signing using eleg authentication.")
      True  -> do
        let actor = signatoryActor ctxtime ctxipnumber (maybesignatory sl') (getEmail sl') slid
        mdoc <- runMaybeT $ do
          dbUpdate $ UpdateFieldsForSigning did slid fields actor
          dbUpdate $ SignDocument did slid mh (Just sinfo) screenshots actor
          Just doc <- dbQuery $ GetDocumentByDocumentID did
          return doc
        return $ case mdoc of
          Nothing -> Left $ DBActionNotAvailable "Signing with Eleg failed"
          Just doc -> Right (doc, olddoc)

withUser :: Kontrakcja m => (User -> m (Either DBError a)) -> m (Either DBError a)
withUser action = do
  Context{ ctxmaybeuser } <- getContext
  maybe (return $ Left DBNotLoggedIn) action ctxmaybeuser
