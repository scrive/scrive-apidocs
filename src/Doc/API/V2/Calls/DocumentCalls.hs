module Doc.API.V2.Calls.DocumentCalls (
  docApiV2New
, docApiV2NewFromTemplate
, docApiV2Available
, docApiV2List
, docApiV2Get
, docApiV2History
, docApiV2EvidenceAttachments
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
, docApiV2FilesMain
, docApiV2FilesGet
, docApiV2Texts
) where

import KontraPrelude
import Happstack.Server.Types
import Doc.Model.Update
import Control.Conditional (unlessM)
import DB.TimeZoneName (defaultTimeZoneName)
import Text.StringTemplates.Templates
import MinutesTime
import File.Model

import Doc.DocStateData
import API.Monad.V2
import Doc.API.V2.JSONDocument
import Doc.DocumentID
import Doc.SignatoryLinkID
import Kontra
import Doc.DocumentMonad
import Data.Unjson
import Data.Unjson as Unjson
import Doc.DocInfo
import DB
import qualified Data.Map as Map hiding (map)
import Data.Text hiding (reverse, takeWhile)
import Doc.API.V2.DocumentUpdateUtils
import Doc.API.V2.DocumentAccess
import Happstack.Fields
import Util.Actor
import qualified Data.Aeson as Aeson
import Doc.Tokens.Model
import Util.SignatoryLinkUtils
import OAuth.Model
import Control.Exception.Lifted
import Doc.DocUtils
import User.Model
import Doc.Model
import Doc.API.V2.Guards
import Doc.Action
import Doc.Anchors
import Doc.API.V2.JSONList
import Doc.API.V2.Parameters
import Doc.API.V2.CallsUtils
-------------------------------------------------------------------------------

docApiV2New :: Kontrakcja m => m Response
docApiV2New = api $ do
  (user, actor) <- getAPIUser APIDocCreate
  mInput <- apiV2Parameter (ApiV2ParameterInput "file" Optional)
  (mFile, title) <- case mInput of
    Nothing -> do
      ctx <- getContext
      title <- renderTemplate_ "newDocumentTitle"
      return (Nothing, title ++ " " ++ formatTimeSimple (ctxtime ctx))
    Just input -> do
        (file, title) <- processPDFParameter "file" input
        return $ (Just file, title)
  saved <- apiV2Parameter' (ApiV2ParameterBool "saved" (OptionalWithDefault (Just true)))
  (dbUpdate $ NewDocument user title Signable defaultTimeZoneName 0 actor) `withDocumentM` do
    dbUpdate $ SetDocumentUnsavedDraft (not saved)
    case mFile of
      Nothing -> return ()
      Just fileid -> do
        dbUpdate $ AttachFile fileid actor
    Created <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2NewFromTemplate :: Kontrakcja m => DocumentID -> m Response
docApiV2NewFromTemplate did = api $ do
  (user, actor) <- getAPIUser APIDocCreate
  template <- dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink template
-- JJ: is this really "invalid_authorisation"? I would think `getAPIUser` checks for this
-- JJ: consider making this some other kind of error
  auser <- apiGuardJustM (invalidAuthorisation "Provided authorization did not match any user") $ dbQuery $ GetUserByIDIncludeDeleted auid
  let haspermission = (userid auser == userid user) ||
                      (usercompany auser == usercompany user &&  isDocumentShared template)
  unless (haspermission) $ do
    throwIO $ SomeKontraException documentActionForbidden
  guardThatObjectVersionMatchesIfProvided did
  withDocumentID did $ guardThatDocument isTemplate "Document must be a template"
  (apiGuardJustM (serverError "Can't clone given document") (dbUpdate $ CloneDocumentWithUpdatedAuthor user template actor) >>=) $ flip withDocumentID $ do
    dbUpdate $ DocumentFromTemplate actor
    dbUpdate $ SetDocumentUnsavedDraft False
    Created <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


-------------------------------------------------------------------------------

docApiV2Available :: Kontrakcja m => m Response
docApiV2Available = $undefined -- TODO implement

docApiV2List :: Kontrakcja m => m Response
docApiV2List = api $ do
  (user, _) <- getAPIUserWithPad APIDocCheck
  offset   <- apiV2Parameter' (ApiV2ParameterInt  "offset"  (OptionalWithDefault (Just 0)))
  maxcount <- apiV2Parameter' (ApiV2ParameterInt  "max"     (OptionalWithDefault (Just 100)))
  filters  <- apiV2Parameter' (ApiV2ParameterJSON "filter"  (OptionalWithDefault (Just [])) unjsonDef)
  sorting  <- apiV2Parameter' (ApiV2ParameterJSON "sorting" (OptionalWithDefault (Just [])) unjsonDef)
  let documentFilters = (DocumentFilterUnsavedDraft False):(join $ toDocumentFilter (userid user) <$> filters)
  let documentSorting = (toDocumentSorting <$> sorting)
  (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit [DocumentsVisibleToUser $ userid user] documentFilters documentSorting (offset,1000,maxcount)
  return $ Ok $ Response 200 Map.empty nullRsFlags (listToJSONBS (allDocsCount,(\d -> (documentAccessForUser user d,d)) <$> allDocs)) Nothing

-------------------------------------------------------------------------------

docApiV2Get :: Kontrakcja m => DocumentID -> m Response
docApiV2Get did = api $ do
  ctx <- getContext
  (msignatorylink :: Maybe SignatoryLinkID) <- readField "signatoryid"
  mmagichashh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  withDocumentID did $ do
    (da,msl) <- case (msignatorylink,mmagichashh) of
      (Just slid,Just mh) -> do
       sl <- apiGuardJustM  (documentNotFound did) $ getSigLinkFor slid <$> theDocument
       when (signatorymagichash sl /= mh) $ throwIO . SomeKontraException $ documentNotFound did
       return (DocumentAccess did $ SignatoryDocumentAccess slid,Just sl)
      _ -> do
        (user,_) <- getAPIUser APIDocCheck
        msiglink <- getSigLinkFor user <$> theDocument
        mauser <- theDocument >>= \d -> case (join $ maybesignatory <$> getAuthorSigLink d) of
                      Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                      _ -> return Nothing
        haspermission <- theDocument >>= \d -> return $
                            isJust msiglink
                        || (isJust mauser && usercompany ($fromJust mauser) == usercompany user && (useriscompanyadmin user || isDocumentShared d))
        if (haspermission)
          then (\d -> (documentAccessForUser user d,msiglink)) <$> theDocument
          else throwIO $ SomeKontraException documentActionForbidden
    case (msl) of
      Just sl -> unlessM ((isTemplate || isPreparation || isClosed) <$> theDocument) $
                  dbUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) =<< signatoryActor ctx sl
      _ -> return ()
    Ok <$> (\d -> (unjsonDocument $ da,d)) <$> theDocument

docApiV2History :: Kontrakcja m => DocumentID -> m Response
docApiV2History _did = $undefined -- TODO implement

docApiV2EvidenceAttachments :: Kontrakcja m => DocumentID -> m Response
docApiV2EvidenceAttachments _did = $undefined -- TODO implement

-------------------------------------------------------------------------------

docApiV2Update :: Kontrakcja m => DocumentID -> m Response
docApiV2Update did = api $ do
  (user, actor) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    guardThatUserIsAuthor user
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocument isPreparation "Document must be draft or template"
    documentJSON <- apiGuardJustM (requestParametersMissing ["document"]) $ getFieldBS "document"
    case Aeson.eitherDecode documentJSON of
      Left _ -> do
       throwIO . SomeKontraException $ requestParametersParseError $ "'document' parameter is not a valid json"
      Right js -> do
        doc <- theDocument
        case (Unjson.update doc (unjsonDocument (DocumentAccess did AuthorDocumentAccess)) js) of
          (Result draftData []) -> do
            applyDraftDataToDocument draftData actor
            Ok <$> (unjsonDocument (DocumentAccess did AuthorDocumentAccess),) <$> theDocument
          (Result _ errs) -> do
            throwIO . SomeKontraException $ requestParametersParseError $ "Errors while parsing document data: " `append` pack (show errs)

docApiV2Start :: Kontrakcja m => DocumentID -> m Response
docApiV2Start did = api $ do
  (user, actor) <- getAPIUser APIDocSend
  withDocumentID did $ do
    guardThatUserIsAuthor user
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocumentCanBeStarted
    t <- ctxtime <$> getContext
    timezone <- documenttimezonename <$> theDocument
    dbUpdate $ PreparationToPending actor timezone
    dbUpdate $ SetDocumentInviteTime t actor
    authorsignsimmediately <- isFieldSet "authorsignsimmediately"
    postDocumentPreparationChange authorsignsimmediately timezone
    Created <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument


docApiV2Prolong :: Kontrakcja m => DocumentID -> m Response
docApiV2Prolong _did = $undefined -- TODO implement

docApiV2Cancel :: Kontrakcja m => DocumentID -> m Response
docApiV2Cancel _did = $undefined -- TODO implement

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
    guardThatDocument isPreparation "Document must be draft or template"
    mInput <- apiV2Parameter (ApiV2ParameterInput "file" Optional)
    case mInput of
      Nothing -> dbUpdate $ DetachFile actor
      Just input -> do
        (fileid, _filename) <- processPDFParameter "file" input
        dbUpdate $ AttachFile fileid actor
        moldfileid <- fmap mainfileid <$> documentfile <$> theDocument
        case moldfileid of
          Just oldfileid -> recalcuateAnchoredFieldPlacements oldfileid fileid
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

-------------------------------------------------------------------------------

docApiV2FilesMain :: Kontrakcja m => DocumentID -> String -> m Response
docApiV2FilesMain _did _filename = $undefined -- TODO implement

docApiV2FilesGet :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
docApiV2FilesGet _did _fid _filename = $undefined -- TODO implement

-------------------------------------------------------------------------------

docApiV2Texts :: Kontrakcja m => DocumentID -> FileID -> m Response
docApiV2Texts _did _fid = $undefined -- TODO implement
