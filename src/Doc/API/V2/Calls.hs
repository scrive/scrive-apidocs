module Doc.API.V2.Calls (
    documentAPIV2
  ) where

import KontraPrelude
import Happstack.Server.Types
import Happstack.StaticRouting
import Doc.Model.Update
import Control.Conditional (unlessM)
import qualified Data.ByteString.Char8 as BS
import DB.TimeZoneName (defaultTimeZoneName)
import qualified Data.ByteString.Lazy as BSL
import LiveDocx
import Text.StringTemplates.Templates
import MinutesTime
import Happstack.Server.RqData
import File.Model
import System.FilePath
import Control.Monad.IO.Class


import Doc.DocStateData
import API.Monad.V2
import Doc.API.V2.JSONDocument
import Doc.DocumentID
import Doc.SignatoryLinkID
import Kontra
import Routing
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
--import Log
import Doc.Tokens.Model
import Util.SignatoryLinkUtils
import OAuth.Model
import Control.Exception.Lifted
import Doc.DocUtils
import User.Model
import Doc.Model
import Doc.Rendering
import Doc.API.V2.CallsUtils
import Doc.Action
import Doc.DocControl
import EID.Signature.Model
import Chargeable.Model
import Doc.API.V2.JSONList

-- TODO add 'documents' prefix
documentAPIV2 ::  Route (Kontra Response)
documentAPIV2  = dir "documents" $ choice [
    dir "new"             $ hPost $ toK0 $ docApiV2New
  , dir "newfromtemplate" $ hPost $ toK1 $ docApiV2NewFromTemplate

  , dir "available" $ hGet $ toK0 $ docApiV2Available
  , dir "list"      $ hGet $ toK0 $ docApiV2List

  , param $ dir "get" $ hGet $ toK1 $ docApiV2Get
  , param $ dir "history"             $ hGet $ toK1 $ docApiV2History
  , param $ dir "evidenceattachments" $ hGet $ toK1 $ docApiV2EvidenceAttachments

  , param $ dir "update" $ hPost $ toK1 $ docApiV2Update
  , param $ dir "start"           $ hPost $ toK1 $ docApiV2Start
  , param $ dir "prolong"         $ hPost $ toK1 $ docApiV2Prolong
  , param $ dir "cancel"          $ hPost $ toK1 $ docApiV2Cancel
  , param $ dir "trash"           $ hPost $ toK1 $ docApiV2Trash
  , param $ dir "delete"          $ hPost $ toK1 $ docApiV2Delete
  , param $ dir "remind"          $ hPost $ toK1 $ docApiV2Remind
  , param $ dir "forward"         $ hPost $ toK1 $ docApiV2Forward
  , param $ dir "setfile"         $ hPost $ toK1 $ docApiV2SetFile
  , param $ dir "setattachments"  $ hPost $ toK1 $ docApiV2SetAttachments
  , param $ dir "setautoreminder" $ hPost $ toK1 $ docApiV2SetAutoReminder
  , param $ dir "clone"           $ hPost $ toK1 $ docApiV2Clone
  , param $ dir "restart"         $ hPost $ toK1 $ docApiV2Restart

  , param $ dir "files" $ dir "main" $ hGet $ toK2 $ docApiV2FilesMain
  , param $ dir "files"              $ hGet $ toK3 $ docApiV2FilesGet

  , param $ dir "texts" $ hGet $ toK2 $ docApiV2Texts

  , param $ param $ dir "setauthentication" $ hPost $ toK2 $ docApiV2SigSetAuthentication
  , param $ param $ dir "reject"            $ hPost $ toK2 $ docApiV2SigReject
  , param $ param $ dir "check"             $ hPost $ toK2 $ docApiV2SigCheck
  , param $ param $ dir "sign"              $ hPost $ toK2 $ docApiV2SigSign
  , param $ param $ dir "sendsmspin"        $ hPost $ toK2 $ docApiV2SigSendSmsPin
  , param $ param $ dir "setattachment"     $ hPost $ toK2 $ docApiV2SigSetAttachment
  ]


-------------------------------------------------------------------------------

docApiV2New :: Kontrakcja m => m Response
docApiV2New = api $ do
  (user, actor) <- getAPIUser APIDocCreate
  ctx <- getContext
  minput <- getDataFn' (lookInput "file")
  (mfile, title) <- case minput of
    Nothing -> do
      title <- renderTemplate_ "newDocumentTitle"
      return (Nothing, title ++ " " ++ formatTimeSimple (ctxtime ctx))
    Just (Input _ Nothing _) -> throwIO . SomeKontraException $ requestParameterInvalid "file" "file was empty"
    Just (Input contentspec (Just filename'') _contentType) -> do
      let filename' = reverse . takeWhile (/='\\') . reverse $ filename'' -- Drop filepath for windows
      let mformat = getFileFormatForConversion filename'
      content' <- case contentspec of
        Left filepath -> liftIO $ BS.readFile filepath
        Right content -> return (BS.concat $ BSL.toChunks content)

      (content'', filename) <- case mformat of
        Nothing -> return (content', filename')
        Just format -> do
          eres <- convertToPDF (ctxlivedocxconf ctx) content' format
          case eres of
            Left (LiveDocxIOError e) -> throwIO . SomeKontraException $ requestParameterInvalid "file" $ "LiveDocX conversion IO failed " `append` pack (show e)
            Left (LiveDocxSoapError s)-> throwIO . SomeKontraException $ requestParameterInvalid "file" $ "LiveDocX conversion SOAP failed " `append` pack s
            Right res -> do
              -- change extension from .doc, .docx and others to .pdf
              let filename = takeBaseName filename' ++ ".pdf"
              return $ (res, filename)

      pdfcontent <- do
        res <- preCheckPDF content''
        case res of
          Right r -> return r
          Left _ ->  throwIO . SomeKontraException $ requestParameterInvalid "file" "file is not a valid PDF"
      fileid' <- dbUpdate $ NewFile filename pdfcontent
      return (Just fileid', takeBaseName filename)
  saved <- apiBoolParamWithDefault  True "saved"
  (dbUpdate $ NewDocument user title Signable defaultTimeZoneName 0 actor) `withDocumentM` do
    dbUpdate $ SetDocumentUnsavedDraft (not saved)
    case mfile of
      Nothing -> return ()
      Just fileid' -> do
        dbUpdate $ AttachFile fileid' actor
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
  offset   <- apiIntParamWithDefault 0 "offset"
  maxcount <- apiIntParamWithDefault 100 "max"
  mfilters <- getFieldBS "filter"
  filters <- case mfilters of
    Nothing -> return []
    Just filters' -> case Aeson.eitherDecode  filters' of
      Left _ -> throwIO . SomeKontraException $ requestParametersParseError "Filters are not a valid JSON array"
      Right filtersaeson -> case (Unjson.parse unjsonDef filtersaeson) of
          (Result fs []) -> return fs
          (Result _ errs) -> do
            throwIO . SomeKontraException $ requestParametersParseError $ "Filters don't parse: " `append` pack (show errs)

  msorting <- getFieldBS "sorting"
  sorting <- case msorting of
    Nothing -> return []
    Just sorting' -> case Aeson.eitherDecode  sorting' of
      Left _ -> throwIO . SomeKontraException $ requestParametersParseError " Sorting is not a valid JSON object"
      Right sortingaeson -> case (Unjson.parse unjsonDef sortingaeson) of
          (Result fs []) -> return [fs]
          (Result _ errs) -> do
            throwIO . SomeKontraException $ requestParametersParseError $ "Sorting doesn't parse: " `append` pack (show errs)
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
  withDocumentID did $ case (msignatorylink,mmagichashh) of
    (Just slid,Just mh) -> do
       sl <- apiGuardJustM  (documentNotFound did) $ getSigLinkFor slid <$> theDocument
       when (signatorymagichash sl /= mh) $ throwIO . SomeKontraException $ documentNotFound did
       unlessM ((isTemplate || isPreparation || isClosed) <$> theDocument) $
         dbUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl)
                       =<< signatoryActor ctx sl
       Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
    _ -> do
      (user,_) <- getAPIUser APIDocCheck
      msiglink <- getSigLinkFor user <$> theDocument
      unlessM (((const (isNothing msiglink)) || isPreparation || isClosed  || isTemplate) <$> theDocument) $ do
          let sl = $fromJust msiglink
          dbUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl)
               =<< signatoryActor ctx sl

      mauser <- theDocument >>= \d -> case (join $ maybesignatory <$> getAuthorSigLink d) of
                     Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                     _ -> return Nothing

      haspermission <- theDocument >>= \d -> return $
                          isJust msiglink
                       || (isJust mauser && usercompany ($fromJust mauser) == usercompany user && (useriscompanyadmin user || isDocumentShared d))
      if (haspermission)
        then do
          Ok <$> (\d -> (unjsonDocument $ documentAccessForUser user d,d)) <$> theDocument
        else throwIO $ SomeKontraException documentActionForbidden

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
docApiV2SetFile _did = $undefined -- TODO implement

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

-------------------------------------------------------------------------------

docApiV2SigSetAuthentication :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAuthentication _did _slid = $undefined -- TODO implement

docApiV2SigReject :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigReject _did _slid = $undefined -- TODO implement

docApiV2SigCheck :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigCheck _did _slid = $undefined -- TODO implement

docApiV2SigSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSign did slid = api $ do
  (mh,_mu) <- getMagicHashAndUserForSignatoryAction did slid
  screenshots <- getScreenshots
  fields <- getFieldForSigning
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh -- We store old document, as it is needed by postDocumentXXX calls
  olddoc `withDocument` ( do
    guardThatDocument isPending "Document must be pending"
    guardThatDocument (hasSigned . $fromJust . getSigLinkFor slid ) "Document can't be already signed"
    checkAuthenticationMethodAndValue slid
    authorization <- signatorylinkauthenticationmethod <$> $fromJust . getSigLinkFor slid <$> theDocument

    case authorization of
      StandardAuthentication -> do
        signDocument slid mh fields Nothing Nothing screenshots
        postDocumentPendingChange olddoc
        handleAfterSigning slid
        Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument

      SMSPinAuthentication -> do
        validPin <- getValidPin slid fields
        if (isJust validPin)
          then do
            signDocument slid mh fields Nothing validPin screenshots
            postDocumentPendingChange olddoc
            handleAfterSigning slid
            Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
          else throwIO . SomeKontraException $ documentActionForbidden

      ELegAuthentication -> dbQuery (GetESignature slid) >>= \case
        mesig@(Just _) -> do
          -- charge company of the author of the document for the signature
          dbUpdate $ ChargeCompanyForElegSignature did
          signDocument slid mh fields mesig Nothing screenshots
          postDocumentPendingChange olddoc
          handleAfterSigning slid
          Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
        Nothing -> throwIO . SomeKontraException $ documentActionForbidden
   )






docApiV2SigSendSmsPin :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPin _did _slid = $undefined -- TODO implement

docApiV2SigSetAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAttachment _did _slid = $undefined -- TODO implement


--- Utils

apiBoolParamWithDefault  :: Kontrakcja m => Bool -> Text -> m Bool
apiBoolParamWithDefault  defaultValue name = do
  mvalue <- getField $ unpack name
  case mvalue of
    Just "true" -> return $ True
    Just "false" -> return $  False
    Just _ ->  throwIO . SomeKontraException $ requestParametersParseError $ "Can't parse parameter '" `append` name `append` "'"
    Nothing -> return $  defaultValue


apiIntParamWithDefault :: Kontrakcja m => Int -> Text -> m Int
apiIntParamWithDefault defaultValue name = do
  mvalue <- getField $ unpack name
  case fmap maybeRead mvalue of
    Just (Just v) -> return $ v
    Just (Nothing) ->  throwIO . SomeKontraException $ requestParametersParseError $ "Can't parse parameter '" `append` name `append` "'"
    Nothing -> return $  defaultValue
