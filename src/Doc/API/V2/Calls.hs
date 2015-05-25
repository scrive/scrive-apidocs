module Doc.API.V2.Calls (
    documentAPIV2
  ) where

import KontraPrelude
import Happstack.Server.Types
import Happstack.StaticRouting
import Doc.Model.Update
import Control.Conditional (unlessM)

import Control.Logic
import Doc.DocStateData
import API.Monad
import Doc.API.V2.JSONDocument
import Doc.DocumentID
import Doc.SignatoryLinkID
import File.FileID
import Kontra
import Routing
import Doc.DocumentMonad
import Data.Unjson
import Data.Unjson as Unjson
import AppView
import Doc.DocInfo
import DB
import qualified Data.Map as Map hiding (map)

import Doc.API.V2.DocumentUpdateUtils
import Doc.API.V2.DocumentAccess
import Util.MonadUtils
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

-- TODO add 'documents' prefix
documentAPIV2 ::  Route (Kontra Response)
documentAPIV2  = choice [
    dir "new"             $ hPost $ toK0 $ docApiV2New
  , dir "newfromtemplate" $ hPost $ toK1 $ docApiV2NewFromTemplate

  , dir "available" $ hGet $ toK0 $ docApiV2Available
  , dir "documents" $ dir "list"      $ hGet $ toK0 $ docApiV2List

  , dir "documents" $ param $ dir "get" $ hGet $ toK1 $ docApiV2Get
  , param $ dir "history"             $ hGet $ toK1 $ docApiV2History
  , param $ dir "evidenceattachments" $ hGet $ toK1 $ docApiV2EvidenceAttachments

  , dir "documents" $ param $ dir "update" $ hPost $ toK1 $ docApiV2Update
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
docApiV2New = $undefined -- TODO implement

docApiV2NewFromTemplate :: Kontrakcja m => DocumentID -> m Response
docApiV2NewFromTemplate _did = $undefined -- TODO implement

-------------------------------------------------------------------------------

docApiV2Available :: Kontrakcja m => m Response
docApiV2Available = $undefined -- TODO implement

docApiV2List :: Kontrakcja m => m Response
docApiV2List = api $ do
  (user, _, _) <- getAPIUserWithPad APIDocCheck

  let (domain,filters) = ([DocumentsVisibleToUser $ userid user],[DocumentFilterDeleted False, DocumentFilterSignable, DocumentFilterUnsavedDraft False])

  (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit domain filters [] (0,100,1000)
  return $ Response 200 Map.empty nullRsFlags (listToJSONBS (allDocsCount,(\d -> (documentAccessForUser user d,d)) <$> allDocs)) Nothing

-------------------------------------------------------------------------------

docApiV2Get :: Kontrakcja m => DocumentID -> m Response
docApiV2Get did = api $ do
  ctx <- getContext
  (msignatorylink :: Maybe SignatoryLinkID) <- readField "signatoryid"
  mmagichashh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  withDocumentID did $ case (msignatorylink,mmagichashh) of
    (Just slid,Just mh) -> do
       sl <- apiGuardJustM  (serverError "No document found") $ getSigLinkFor slid <$> theDocument
       when (signatorymagichash sl /= mh) $ throwIO . SomeKontraException $ serverError "No document found"
       unlessM ((isTemplate ||^ isPreparation ||^ isClosed) <$> theDocument) $
         dbUpdate . MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl)
                       =<< signatoryActor ctx sl
       Ok <$> (\d -> (unjsonDocument (DocumentAccess did $ SignatoryDocumentAccess slid),d)) <$> theDocument
    _ -> do
      (user,_,_) <- getAPIUser APIDocCheck
      msiglink <- getSigLinkFor user <$> theDocument
      unlessM (((const (isNothing msiglink)) ||^ isPreparation ||^ isClosed  ||^ isTemplate) <$> theDocument) $ do
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
        else throwIO . SomeKontraException $ serverError "You do not have right to access document"

docApiV2History :: Kontrakcja m => DocumentID -> m Response
docApiV2History _did = $undefined -- TODO implement

docApiV2EvidenceAttachments :: Kontrakcja m => DocumentID -> m Response
docApiV2EvidenceAttachments _did = $undefined -- TODO implement

-------------------------------------------------------------------------------

docApiV2Update :: Kontrakcja m => DocumentID -> m Response
docApiV2Update did = api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    when (not $ (auid == userid user)) $ do
      throwIO . SomeKontraException $ serverError "Permission problem. You are not the author of this document."
    unlessM (isPreparation <$> theDocument) $ do
      throwIO . SomeKontraException $ serverError "Document is not a draft or template"
    documentJSON <- guardJustM $ getFieldBS "document"
    case Aeson.eitherDecode documentJSON of
      Left _ -> do
       throwIO . SomeKontraException $ badInput $ "'document' parameter is not a valid json"
      Right js -> do
        doc <- theDocument
        case (Unjson.update doc (unjsonDocument (DocumentAccess did AuthorDocumentAccess)) js) of
          (Result draftData []) -> do
            applyDraftDataToDocument draftData actor
            simpleUnjsonResponse (unjsonDocument (DocumentAccess did AuthorDocumentAccess)) =<< theDocument
          (Result _ errs) -> do
            throwIO . SomeKontraException $ badInput $ "Errors while parsing document data: " ++ show errs

docApiV2Start :: Kontrakcja m => DocumentID -> m Response
docApiV2Start _did = $undefined -- TODO implement

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
docApiV2SigSign _did _slid = $undefined -- TODO implement

docApiV2SigSendSmsPin :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSendSmsPin _did _slid = $undefined -- TODO implement

docApiV2SigSetAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
docApiV2SigSetAttachment _did _slid = $undefined -- TODO implement
