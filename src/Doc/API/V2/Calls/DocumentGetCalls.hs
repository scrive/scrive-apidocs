module Doc.API.V2.Calls.DocumentGetCalls (
  docApiV2Available
, docApiV2List
, docApiV2Get
, docApiV2History
, docApiV2EvidenceAttachments
, docApiV2FilesMain
, docApiV2FilesGet
, docApiV2Texts
) where

import Data.Text (unpack)
import Data.Unjson
import Happstack.Server.Types
import Text.JSON.Types (JSValue(..))
import qualified Data.Map as Map hiding (map)

import API.V2
import DB
import Doc.API.V2.DocumentAccess
import Doc.API.V2.Guards
import Doc.API.V2.JSONDocument
import Doc.API.V2.JSONList
import Doc.API.V2.Parameters
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model
import EvidenceLog.Model
import EvidenceLog.View
import File.Model
import Kontra
import KontraPrelude
import OAuth.Model
import User.Model
import Util.Actor
import Util.SignatoryLinkUtils

docApiV2Available :: Kontrakcja m => m Response
docApiV2Available = api $ do
  (user, _) <- getAPIUser APIDocCheck
  (ids :: [DocumentID]) <- apiV2Parameter' (ApiV2ParameterRead "ids" Obligatory)
  when (length ids > 10000) $ do
    apiError $ requestParameterInvalid "ids" "Can't contain more than 10,000 document ids"
  available <- fmap (map fromDocumentID) $ dbQuery $ GetDocumentsIDs [DocumentsVisibleToUser $ userid user] [DocumentFilterDeleted False,DocumentFilterByDocumentIDs ids] []
  return $ Ok $ Response 200 Map.empty nullRsFlags (unjsonToByteStringLazy unjsonDef available) Nothing

docApiV2List :: Kontrakcja m => m Response
docApiV2List = api $ do
  (user, _) <- getAPIUserWithPad APIDocCheck
  offset   <- apiV2Parameter' (ApiV2ParameterInt  "offset"  (OptionalWithDefault 0))
  maxcount <- apiV2Parameter' (ApiV2ParameterInt  "max"     (OptionalWithDefault 100))
  filters  <- apiV2Parameter' (ApiV2ParameterJSON "filter"  (OptionalWithDefault []) unjsonDef)
  sorting  <- apiV2Parameter' (ApiV2ParameterJSON "sorting" (OptionalWithDefault []) unjsonDef)
  let documentFilters = (DocumentFilterUnsavedDraft False):(join $ toDocumentFilter (userid user) <$> filters)
  let documentSorting = (toDocumentSorting <$> sorting)
  (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit [DocumentsVisibleToUser $ userid user] documentFilters documentSorting (offset,1000,maxcount)
  return $ Ok $ Response 200 Map.empty nullRsFlags (listToJSONBS (allDocsCount,(\d -> (documentAccessForUser user d,d)) <$> allDocs)) Nothing

docApiV2Get :: Kontrakcja m => DocumentID -> m Response
docApiV2Get did = api $ do
  ctx <- getContext
  -- If a 'signatory_id' parameter was given, we first check if the session
  -- has a matching and valid MagicHash for that SignatoryLinkID
  mSessionSignatory <- do
    mslid <- apiV2Parameter (ApiV2ParameterRead "signatory_id" Optional)
    maybe (return Nothing) (getDocumentSignatoryMagicHash did) mslid
  (da, msl) <- case mSessionSignatory of
    Just sl -> do
      let slid = signatorylinkid sl
      return (DocumentAccess did $ SignatoryDocumentAccess slid, Just sl)
  -- If we didn't get a session *only* then we check normally and try to get
  -- a SignatoryLink too as we need to mark if they see the document
    Nothing -> withDocumentID did $ do
      (user,_) <- getAPIUser APIDocCheck
      doc <- theDocument
      let msiglink = getSigLinkFor user doc
      case msiglink of
        Just _ -> return ()
        Nothing -> guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user
      return (documentAccessForUser user doc, msiglink)
  withDocumentID did $ do
    doc <- theDocument
    let canMarkSeen = not ((isTemplate || isPreparation || isClosed) doc)
    case (msl, canMarkSeen) of
      (Just sl, True) -> dbUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl) =<< signatoryActor ctx sl
      _ -> return ()
    Ok <$> (\d -> (unjsonDocument $ da,d)) <$> theDocument

docApiV2History :: Kontrakcja m => DocumentID -> m Response
docApiV2History did = api $ do
  (user,_) <- getAPIUser APIDocCheck
  mLangCode <- apiV2Parameter (ApiV2ParameterText "lang" Optional)
  mLang <- case fmap (langFromCode . unpack) mLangCode of
    Nothing -> return Nothing
    Just Nothing -> do
      apiError $ requestParameterInvalid "lang" "Not a valid or supported language code"
    Just (Just l) -> return $ Just l
  switchLang $ fromMaybe (lang $ usersettings user) mLang
  evidenceLog <- dbQuery $ GetEvidenceLog did
  doc <- dbQuery $ GetDocumentByDocumentID did
  events <- eventsJSListFromEvidenceLog doc evidenceLog
  return $ Ok (JSArray events)

docApiV2EvidenceAttachments :: Kontrakcja m => DocumentID -> m Response
docApiV2EvidenceAttachments _did = $undefined -- TODO implement

docApiV2FilesMain :: Kontrakcja m => DocumentID -> String -> m Response
docApiV2FilesMain _did _filename = $undefined -- TODO implement

docApiV2FilesGet :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
docApiV2FilesGet _did _fid _filename = $undefined -- TODO implement

-------------------------------------------------------------------------------

docApiV2Texts :: Kontrakcja m => DocumentID -> FileID -> m Response
docApiV2Texts _did _fid = $undefined -- TODO implement
