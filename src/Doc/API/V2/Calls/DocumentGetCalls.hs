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

import KontraPrelude
import Happstack.Server.Types
import Doc.Model.Update
import Control.Conditional (unlessM)
import File.Model

import Doc.DocStateData
import API.Monad.V2
import Doc.API.V2.JSONDocument
import Doc.DocumentID
import Doc.SignatoryLinkID
import Kontra
import Doc.DocumentMonad
import Data.Unjson
import Doc.DocInfo
import DB
import qualified Data.Map as Map hiding (map)
import Doc.API.V2.DocumentAccess
import Happstack.Fields
import Util.Actor
import Doc.Tokens.Model
import Util.SignatoryLinkUtils
import OAuth.Model
import Control.Exception.Lifted
import Doc.DocUtils
import User.Model
import Doc.Model
import Doc.API.V2.JSONList
import Doc.API.V2.Parameters

docApiV2Available :: Kontrakcja m => m Response
docApiV2Available = $undefined -- TODO implement

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
  -- JJ: I didn't realise 'get' call can take a 'signatoryid' parameter.
  --     Options I see are:
  --     1. A way around this is to get all sigids for the document, then get all
  --        document session tokens via the sigids (i.e. magic hashes) and we can
  --        check if the given one matches any existing one and then we can unify
  --        this odd doubling of things
  --     2. We need to provide 'signatoryid' because of our DB table structure
  --        would mean high DB costs, or security, or wathever reason
  --        THEN
  --        this really is the work for two different API calls because we're
  --        clearly replicating stuff just with slightly different behaviour
  --        It is just wrong...
  --        So we could have /api/v2/$documentid$/get/$signatoryid$/
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

docApiV2FilesMain :: Kontrakcja m => DocumentID -> String -> m Response
docApiV2FilesMain _did _filename = $undefined -- TODO implement

docApiV2FilesGet :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
docApiV2FilesGet _did _fid _filename = $undefined -- TODO implement

-------------------------------------------------------------------------------

docApiV2Texts :: Kontrakcja m => DocumentID -> FileID -> m Response
docApiV2Texts _did _fid = $undefined -- TODO implement
