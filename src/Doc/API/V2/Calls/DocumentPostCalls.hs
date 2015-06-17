module Doc.API.V2.Calls.DocumentPostCalls (
  docApiV2New
, docApiV2NewFromTemplate
, docApiV2Update
, docApiV2Start
, docApiV2Prolong
, docApiV2Cancel
, docApiV2Trash
, docApiV2Delete
, docApiV2Remind
, docApiV2Forward
, docApiV2SetFile
, docApiV2SetAttachments
, docApiV2SetAutoReminder
, docApiV2Clone
, docApiV2Restart
, docApiV2SigSetAuthentication
) where

import KontraPrelude
import Happstack.Server.Types
import Doc.Model.Update
import DB.TimeZoneName (defaultTimeZoneName)
import Text.StringTemplates.Templates
import MinutesTime

import API.V2
import DB
import Data.Text hiding (reverse, takeWhile)
import Data.Unjson
import Data.Unjson as Unjson
import Doc.API.V2.DocumentAccess
import Doc.API.V2.DocumentUpdateUtils
import Doc.API.V2.Guards
import Doc.API.V2.JSONDocument
import Doc.API.V2.Parameters
import Doc.Action
import Doc.Anchors
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model
import Doc.SignatoryLinkID
import File.File (File(..))
import Kontra
import OAuth.Model

docApiV2New :: Kontrakcja m => m Response
docApiV2New = api $ do
  (user, actor) <- getAPIUser APIDocCreate
  saved <- apiV2Parameter' (ApiV2ParameterBool "saved" (OptionalWithDefault True))
  mFile <- apiV2Parameter (ApiV2ParameterFile "file" Optional)
  title <- case mFile of
    Nothing -> do
      ctx <- getContext
      title <- renderTemplate_ "newDocumentTitle"
      return $ title ++ " " ++ formatTimeSimple (ctxtime ctx)
    Just f -> return $ filename f
  (dbUpdate $ NewDocument user title Signable defaultTimeZoneName 0 actor) `withDocumentM` do
    dbUpdate $ SetDocumentUnsavedDraft (not saved)
    case mFile of
      Nothing -> return ()
      Just f -> do
        dbUpdate $ AttachFile (fileid f) actor
    Created <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2NewFromTemplate :: Kontrakcja m => DocumentID -> m Response
docApiV2NewFromTemplate did = api $ do
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    guardThatUserIsAuthorOrDocumentIsShared user
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocument isTemplate "Document must be a template"
  template <- dbQuery $ GetDocumentByDocumentID $ did
  (apiGuardJustM (serverError "Can't clone given document") (dbUpdate $ CloneDocumentWithUpdatedAuthor user template actor) >>=) $ flip withDocumentID $ do
    dbUpdate $ DocumentFromTemplate actor
    dbUpdate $ SetDocumentUnsavedDraft False
    Created <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2Update :: Kontrakcja m => DocumentID -> m Response
docApiV2Update did = api $ do
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    guardThatUserIsAuthor user
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation
    documentJSON <- apiV2Parameter' (ApiV2ParameterAeson "document" Obligatory)
    doc <- theDocument
    case (Unjson.update doc (unjsonDocument (DocumentAccess did AuthorDocumentAccess)) documentJSON) of
      (Result draftData []) -> do
        applyDraftDataToDocument draftData actor
        Ok <$> (unjsonDocument (DocumentAccess did AuthorDocumentAccess),) <$> theDocument
      (Result _ errs) -> do
        apiError $ requestParameterParseError "document" $ "Errors while parsing document data: " `append` pack (show errs)


docApiV2Start :: Kontrakcja m => DocumentID -> m Response
docApiV2Start did = api $ do
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    guardThatUserIsAuthor user
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation
    guardThatDocumentCanBeStarted
    authorSignsNow <- apiV2Parameter' (ApiV2ParameterBool "author_signs_now" (OptionalWithDefault False))
    t <- ctxtime <$> getContext
    timezone <- documenttimezonename <$> theDocument
    dbUpdate $ PreparationToPending actor timezone
    dbUpdate $ SetDocumentInviteTime t actor
    postDocumentPreparationChange authorSignsNow timezone
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2Prolong :: Kontrakcja m => DocumentID -> m Response
docApiV2Prolong _did = $undefined -- TODO implement


docApiV2Cancel :: Kontrakcja m => DocumentID -> m Response
docApiV2Cancel did = api $ do
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    guardThatUserIsAuthorOrCompanyAdmin user
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Pending
    dbUpdate $ CancelDocument actor
    postDocumentCanceledChange =<< theDocument
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2Trash :: Kontrakcja m => DocumentID -> m Response
docApiV2Trash _did = $undefined -- TODO implement


docApiV2Delete :: Kontrakcja m => DocumentID -> m Response
docApiV2Delete _did = $undefined -- TODO implement


docApiV2Remind :: Kontrakcja m => DocumentID -> m Response
docApiV2Remind _did = $undefined -- TODO implement


docApiV2Forward :: Kontrakcja m => DocumentID -> m Response
docApiV2Forward _did = $undefined -- TODO implement


docApiV2SetFile :: Kontrakcja m => DocumentID -> m Response
docApiV2SetFile did = api $ do
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    guardThatUserIsAuthor user
    guardThatObjectVersionMatchesIfProvided did
    guardDocumentStatus Preparation
    mFile <- apiV2Parameter (ApiV2ParameterFile "file" Optional)
    case mFile of
      Nothing -> dbUpdate $ DetachFile actor
      Just file -> do
        dbUpdate $ AttachFile (fileid file) actor
        moldfileid <- fmap mainfileid <$> documentfile <$> theDocument
        case moldfileid of
          Just oldfileid -> recalcuateAnchoredFieldPlacements oldfileid (fileid file)
          Nothing -> return ()
    Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2SetAttachments :: Kontrakcja m => DocumentID -> m Response
docApiV2SetAttachments _did = $undefined -- TODO implement


docApiV2SetAutoReminder :: Kontrakcja m => DocumentID -> m Response
docApiV2SetAutoReminder _did = $undefined -- TODO implement


docApiV2Clone :: Kontrakcja m => DocumentID -> m Response
docApiV2Clone _did = $undefined -- TODO implement


docApiV2Restart :: Kontrakcja m => DocumentID -> m Response
docApiV2Restart _did = $undefined -- TODO implement

docApiV2SigSetAuthentication :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAuthentication _did _slid = $undefined -- TODO implement
