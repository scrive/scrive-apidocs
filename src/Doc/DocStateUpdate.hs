module Doc.DocStateUpdate
    ( signDocumentWithEmailOrPad
    , signDocumentWithEleg
    ) where

import Control.Monad.Trans.Maybe
import MagicHash (MagicHash)
import DBError
import Doc.Model
import Doc.DocStateData
import Kontra
import Util.SignatoryLinkUtils
import Doc.SignatoryLinkID
import Doc.DocumentID
import User.Model
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import DB
import Util.Actor


signDocumentWithEmailOrPad :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(FieldType, String)] -> SignatoryScreenshots.SignatoryScreenshots
                           -> m (Either DBError (Document, Document))
signDocumentWithEmailOrPad did slid mh fields screenshots = do
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
  do
     switchLang (getLang olddoc)
     let Just sl' = getSigLinkFor olddoc slid
     case signatorylinkauthenticationmethod sl' == ELegAuthentication of
      True -> return $ Left (DBActionNotAvailable "This document does not allow signing using email authentication.")
      False  -> do
        Context{ ctxtime, ctxipnumber } <- getContext
        let actor = signatoryActor ctxtime ctxipnumber sl'
        mdoc <- runMaybeT $ do
          dbUpdate $ UpdateFieldsForSigning did slid fields actor
          dbUpdate $ SignDocument did slid mh Nothing screenshots actor
          doc <- dbQuery $ GetDocumentByDocumentID did

          return doc
        return $ case mdoc of
          Nothing  -> Left $ DBActionNotAvailable "Signing with email/pad failed"
          Just doc -> Right (doc, olddoc)

signDocumentWithEleg :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(FieldType, String)] -> SignatureInfo -> SignatoryScreenshots.SignatoryScreenshots
                     -> m (Either DBError (Document, Document))
signDocumentWithEleg did slid mh fields sinfo screenshots = do
  Context{ ctxtime, ctxipnumber } <- getContext
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
  do
     switchLang (getLang olddoc)
     let Just sl' = getSigLinkFor olddoc slid
     case signatorylinkauthenticationmethod sl' == ELegAuthentication of
      False -> return $ Left (DBActionNotAvailable "This document does not allow signing using eleg authentication.")
      True  -> do
        let actor = signatoryActor ctxtime ctxipnumber sl'
        mdoc <- runMaybeT $ do
          dbUpdate $ UpdateFieldsForSigning did slid fields actor
          dbUpdate $ SignDocument did slid mh (Just sinfo) screenshots actor
          doc <- dbQuery $ GetDocumentByDocumentID did
          return doc
        return $ case mdoc of
          Nothing -> Left $ DBActionNotAvailable "Signing with Eleg failed"
          Just doc -> Right (doc, olddoc)
