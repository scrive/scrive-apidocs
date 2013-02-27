module Doc.API (
    documentAPI
  , apiCallCreateFromFile      -- Exported for tests
  , apiCallCreateFromTemplate  -- Exported for tests
  , apiCallGet                 -- Exported for tests
  , apiCallUpdate              -- Exported for tests
  , apiCallReady               -- Exported for tests
  , apiCallSign                -- Exported for tests
  ) where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control (MonadBaseControl)
import Happstack.StaticRouting
import Text.JSON hiding (Ok)
import qualified Text.JSON as J
import KontraMonad
import Happstack.Server.Types
import Routing
import API.APIVersion (APIVersion(..))
import Doc.DocStateQuery
import Doc.DocStateData
import Doc.Model
import Doc.SignatoryLinkID
import Doc.JSON
import Doc.DocumentID
import Doc.Tokens.Model
import Control.Applicative
import Control.Logic
import Control.Monad.Trans
import Happstack.Fields
import Utils.String
import Utils.Monad
import System.FilePath
import Data.Maybe
import qualified Data.String.Utils as String

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Util.Actor
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Happstack.Server.RqData
import Doc.Rendering
import DB
import DB.SQL2
import DB.TimeZoneName (mkTimeZoneName)

import MagicHash (MagicHash)
import Kontra
import Doc.DocUtils
import User.Model
import API.Monad
import Control.Monad.Error
import qualified Log
import Stats.Control
import LiveDocx
import Doc.DocView
import Doc.DocInfo
import Doc.DocStateUpdate
import Doc.Action
import Text.JSON.FromJSValue
import Doc.DocDraft
import PadQueue.Model
import Archive.Control
import OAuth.Model
import InputValidation
import Doc.API.Callback.Model
import qualified Data.ByteString.Base64 as B64
import Text.JSON.Gen
import MinutesTime
import Text.StringTemplates.Templates
import Data.Map as Map

import Doc.DocSeal as DocSeal
import qualified Data.ByteString.UTF8 as BS hiding (length)
import Doc.DocStateCommon
import File.Model
import File.Storage
import qualified PadQueue.API as PadQueue
import Data.String.Utils (replace)
import EvidenceLog.Control
import Control.Exception.Lifted
import Doc.DocControl
import Utils.Tuples
import Utils.Either
import Doc.SignatoryScreenshots

documentAPI :: Route (KontraPlus Response)
documentAPI = dir "api" $ choice
  [ dir "frontend" $ versionedAPI Frontend
  , versionedAPI V1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ versionedAPI V1
  ]

versionedAPI :: APIVersion -> Route (KontraPlus Response)
versionedAPI _version = choice [

  dir "createfromfile"     $ hPostNoXTokenHttp $ toK0 $ apiCallCreateFromFile,
  dir "createfromtemplate" $ hPostNoXTokenHttp $ toK1 $ apiCallCreateFromTemplate,
  dir "update"             $ hPostNoXTokenHttp $ toK1 $ apiCallUpdate,
  dir "ready"              $ hPostNoXTokenHttp $ toK1 $ apiCallReady,
  dir "cancel"             $ hPostNoXTokenHttp $ toK1 $ apiCallCancel,
  dir "reject"             $ hPostNoXTokenHttp $ toK2 $ apiCallReject,
  dir "sign"               $ hPostNoXTokenHttp $ toK2 $ apiCallSign,

  dir "remind"             $ hPostNoXTokenHttp $ toK1 $ apiCallRemind,
  dir "delete"             $ hDeleteAllowHttp  $ toK1 $ apiCallDelete,
  dir "get"                $ hGetAllowHttp $ toK1 $ apiCallGet,
  dir "list"               $ hGetAllowHttp $ apiCallList,
  dir "evidencelog"        $ hGetAllowHttp $ apiCallEvidenceLog,
  dir "downloadmainfile"   $ hGetAllowHttp  $ toK2 $ apiCallDownloadMainFile,

  dir "padqueue"           $ PadQueue.padqueueAPI,
  dir "mainfile" $ hPostNoXTokenHttp $ toK1 $ documentChangeMainFile,

  dir "document" $ hPostNoXTokenHttp $ toK6 $ documentUploadSignatoryAttachment,
  dir "document" $ hDeleteAllowHttp  $ toK6 $ documentDeleteSignatoryAttachment
  ]

-- | Clean a filename from a path (could be windows \) to a base name
cleanFileName :: FilePath -> String
cleanFileName = takeBaseName . String.replace "\\" "/"

{- New API calls-}
apiCallCreateFromFile :: Kontrakcja m => m Response
apiCallCreateFromFile = api $ do
  ctx <- getContext
  (user, actor, external) <- getAPIUser APIDocCreate
  dtype <- lift $ fromMaybe (Contract) <$> readField "type"
  isTpl <- lift $ isFieldSet "template"
  let doctype = (Template <| isTpl |> Signable) dtype
  minput <- lift $ getDataFn' (lookInput "file")
  (mfile, title) <- case minput of
    Nothing -> do
      title <- renderTemplate_ ("newDocumentTitle" <| not isTpl |> "newTemplateTitle")
      return (Nothing,  replace "  " " " $ title ++ " " ++ formatMinutesTimeSimple (ctxtime ctx))
    Just (Input _ Nothing _) -> throwIO . SomeKontraException $ badInput "Missing file"
    Just (Input contentspec (Just filename') _contentType) -> do
      let filename = takeBaseName filename'
      let mformat = getFileFormatForConversion filename'
      content' <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
      content'' <- case mformat of
        Nothing -> return content'
        Just format -> do
          eres <- liftIO $ convertToPDF (ctxlivedocxconf ctx) (BS.concat $ BSL.toChunks content') format
          case eres of
            Left (LiveDocxIOError e) -> throwIO . SomeKontraException $ serverError $ show e
            Left (LiveDocxSoapError s)-> throwIO . SomeKontraException $ serverError s
            Right res -> return $ BSL.fromChunks [res]
      pdfcontent <- apiGuardL (badInput "The PDF is invalid.") $ liftIO $ do
                     cres <- preCheckPDF (concatChunks content'')
                     case cres of
                        Right c -> return (Right c)
                        Left m -> case (B64.decode $ (concatChunks content'')) of -- Salesforce hack. Drop this decoding when happstack-7.0.4 is included.
                                      Right dcontent -> preCheckPDF dcontent
                                      Left _ -> return (Left m)
      file <- dbUpdate $ NewFile filename pdfcontent
      return (Just file, filename)
  Just doc <- dbUpdate $ NewDocument user title doctype 1 actor
  when_ (not $ external) $ dbUpdate $ SetDocumentUnsavedDraft [documentid doc] True
  case mfile of
    Nothing -> return ()
    Just file -> do
      dbUpdate $ AttachFile (documentid doc) (fileid file) actor
      return ()
  let doc' = doc{ documentfile = fileid `fmap` mfile }
  _ <- lift $ addDocumentCreateStatEvents (documentid doc) "web"
  Created <$> documentJSON False True True Nothing Nothing doc'


apiCallCreateFromTemplate :: Kontrakcja m => DocumentID -> m Response
apiCallCreateFromTemplate did =  api $ do
  (user, actor, external) <- getAPIUser APIDocCreate
  template <- apiGuardJustM (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink template
  auser <- apiGuardJustM (serverError "No user found") $ dbQuery $ GetUserByIDIncludeDeleted auid
  let haspermission = (userid auser == userid user) ||
                          ((usercompany auser == usercompany user && (isJust $ usercompany user)) &&  isDocumentShared template)
  enewdoc <- if (isTemplate template && haspermission)
                    then dbUpdate $ SignableFromDocumentIDWithUpdatedAuthor user did actor
                    else throwIO . SomeKontraException $ serverError "Id did not matched template or you do not have right to access document"
  case enewdoc of
      Just newdoc -> do
          _ <- lift $ addDocumentCreateStatEvents (documentid newdoc) "web"
          Log.debug $ show "Document created from template"
          when_ (not $ external) $ dbUpdate $ SetDocumentUnsavedDraft [documentid newdoc] True
          Created <$> documentJSON False True  True Nothing Nothing newdoc
      Nothing -> throwIO . SomeKontraException $ serverError "Create document from template failed"



apiCallUpdate :: Kontrakcja m => DocumentID -> m Response
apiCallUpdate did = api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
  jsons <- apiGuardL (badInput "The MIME part 'json' must exist and must be a JSON.") $ getDataFn' (look "json")
  json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $ case decode jsons of
                                                                               J.Ok js -> Just js
                                                                               _ -> Nothing
  Log.debug $ "Document " ++ show did ++ " updated with JSON:" ++ encode json
  draftData   <-apiGuardJustM (badInput "Given JSON does not represent valid draft data.") $ return $ fromJSValue json
  newdocument <-  apiGuardL (serverError "Could not apply draft data") $ applyDraftDataToDocument doc draftData actor
  triggerAPICallbackIfThereIsOne newdocument
  Ok <$> documentJSON False True True Nothing Nothing newdocument

data ReadyParams = ReadyParams
  { timezonestring :: String
  }
  deriving Show

instance FromJSValue ReadyParams where
  fromJSValue = do
    Just s <- fromJSValueField "timezone"
    return $ Just ReadyParams { timezonestring = s }

apiCallReady :: (MonadBaseControl IO m, Kontrakcja m) => DocumentID -> m Response
apiCallReady did =  api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
  let defaultParams = ReadyParams { timezonestring = "Europe/Stockholm" }
  mjsons <- lift $ getDataFn' (look "json")
  params <- case mjsons of
            Nothing -> return defaultParams
            Just jsons -> do
              json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $
                       case decode jsons of
                         J.Ok js -> Just js
                         _ -> Nothing
              apiGuardJustM (badInput "Given JSON does not represent valid READY parameters.") $
                return $ fromJSValue json
  when (not $ all ((/=EmailDelivery) . signatorylinkdeliverymethod ||^ isGood . asValidEmail . getEmail) (documentsignatorylinks doc)) $ do
        throwIO . SomeKontraException $ serverError "Some signatories don't have a valid email address set."
  when (isNothing $ documentfile doc) $ do
        throwIO . SomeKontraException $ serverError "File must be provided before document can be made ready."
  timezone <- lift $ mkTimeZoneName (timezonestring params)
  mndoc <- lift $ authorSendDocument user actor (documentid doc) timezone
  case mndoc of
          Right newdocument -> do
              lift $ postDocumentPreparationChange newdocument "web"
              newdocument' <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
              Accepted <$> documentJSON False True True Nothing Nothing newdocument'
          Left reason -> throwIO . SomeKontraException $ serverError $ "Operation failed: " ++ show reason


apiCallCancel :: Kontrakcja m => DocumentID -> m Response
apiCallCancel did =  api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
  dbUpdate $ CancelDocument (documentid doc) actor
  newdocument <- apiGuardL (serverError "No document found after cancel") $ dbQuery $ GetDocumentByDocumentID $ did
  lift $ postDocumentCanceledChange newdocument "api"
  newdocument' <- apiGuardL (serverError "No document found after cancel and post actions") $ dbQuery $ GetDocumentByDocumentID $ did
  Accepted <$> documentJSON False True True Nothing Nothing newdocument'


apiCallReject :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
apiCallReject did slid = api $ do
  (mh,method) <- do
    mh' <- dbQuery $ GetDocumentSessionToken slid
    case mh' of
      Just mh'' ->  return (mh'',"web")
      Nothing -> do
         (user, _ , _) <- getAPIUser APIPersonal
         mh'' <- lift $ getMagicHashForDocumentSignatoryWithUser  did slid user
         case mh'' of
           Nothing -> throwError $ serverError "Magic hash for signatory was not provided"
           Just mh''' -> return (mh''',"api")
  edoc <- lift $ getDocByDocIDSigLinkIDAndMagicHash did slid mh
  case edoc of
    Left _ -> throwError $ serverError "Can't find a document that matches signatory"
    Right doc ->  do
      ctx <- getContext
      let Just sll = getSigLinkFor doc slid
          actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sll) (getEmail sll) slid
      customtext <- lift $ getCustomTextField "customtext"
      lift $ switchLang (getLang doc)
      True <- dbUpdate $ RejectDocument did slid customtext actor
      Just doc' <- dbQuery $ GetDocumentByDocumentID did
      let Just sl = getSigLinkFor doc' slid
      _ <- addSignStatRejectEvent doc' sl
      lift $ postDocumentRejectedChange doc' slid method
      Accepted <$> (runJSONGenT $ return ())


apiCallSign :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m Response
apiCallSign  did slid = api $ do
  Log.debug $ "Ready to sign a docment " ++ show did ++ " for signatory " ++ show slid
  (mh,method) <- do
    mh' <- dbQuery $ GetDocumentSessionToken slid
    case mh' of
      Just mh'' ->  return (mh'',"web")
      Nothing -> do
         (user, _ , _) <- getAPIUser APIPersonal
         mh'' <- lift $ getMagicHashForDocumentSignatoryWithUser  did slid user
         case mh'' of
           Nothing -> throwError $ serverError "Can't perform this action. Not authorized."
           Just mh''' -> return (mh''',"api")
  Log.debug "We have magic hash for this operation"
  screenshots <- lift $ (fromMaybe emptySignatoryScreenshots) <$> join <$> fmap fromJSValue <$> getFieldJSON "screenshots"
  fields <- do
      eFieldsJSON <- lift $ getFieldJSON "fields"
      case eFieldsJSON of
           Nothing -> throwError $ serverError "No fields description provided or fields description is not a valid JSON array"
           Just fieldsJSON -> do
             mvalues <- withJSValue fieldsJSON $ fromJSValueCustomMany $ do
                  ft <- fromJSValueM
                  val <- fromJSValueField "value"
                  return $ pairMaybe ft val
             case mvalues of
               Nothing -> throwError $ serverError "Fields description json has invalid format"
               Just values -> return values
  mprovider <- lift $ readField "eleg"
  Log.debug $ "All parameters read and parsed"
  edoc <- case mprovider of
           Nothing -> Right <$> lift (signDocumentWithEmailOrPad did slid mh fields screenshots)
           Just provider -> lift $ handleSignWithEleg did slid mh fields screenshots provider
  Log.debug $ "Signing done, result is " ++ show (isRight edoc)
  case edoc of
    Right (Right (doc, olddoc)) -> do
      lift $ postDocumentPendingChange doc olddoc method
      udoc <- apiGuardJustM (serverError "Can find document after signing") $ dbQuery $ GetDocumentByDocumentID did
      lift $ handleAfterSigning udoc slid
      siglink <- apiGuardJustM (serverError "Can find signatory after signing") $ return $ getSigLinkFor doc slid
      case (signatorylinksignredirecturl siglink) of
           Nothing -> Accepted <$> (runJSONGenT $ value "signed" True)
           Just "" -> Accepted <$> (runJSONGenT $ value "signed" True)
           Just s  -> Accepted <$> (runJSONGenT $ value "signed" True >> value "redirect" s)
    Right (Left err) -> throwError $ serverError  $ "Error: DB action " ++ show err
    Left msg ->  throwError $ serverError  $ "Error: " ++ msg


apiCallRemind :: Kontrakcja m => DocumentID -> m Response
apiCallRemind did =  api $ do
  ctx <- getContext
  (user, actor , _) <- getAPIUser APIDocSend
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  when (documentstatus doc /= Pending) $ do
        throwIO . SomeKontraException $ serverError "Can't send reminder for documents that are not pending"
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
  _ <- lift $ sendAllReminderEmails ctx actor user did
  newdocument <- apiGuardL (serverError "No document found after sending reminder") $ dbQuery $ GetDocumentByDocumentID $ did
  Accepted <$> documentJSON False True True Nothing Nothing newdocument

apiCallDelete :: Kontrakcja m => DocumentID -> m Response
apiCallDelete did =  api $ do
  ctx <- getContext
  (user, actor, _) <- getAPIUser APIDocSend
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
  dbUpdate $ ArchiveDocument (userid user) did actor
  _ <- addSignStatDeleteEvent (signatorylinkid $ fromJust $ getSigLinkFor doc user) (ctxtime ctx)
  case (documentstatus doc) of
       Preparation -> do
         _ <- dbUpdate $ ReallyDeleteDocument (userid user) did actor
         when_ (isJust $ getSigLinkFor doc user) $
             addSignStatPurgeEvent (signatorylinkid $ fromJust $ getSigLinkFor doc user)  (ctxtime ctx)
       _ -> return ()
  Accepted <$> (runJSONGenT $ return ())




apiCallGet :: Kontrakcja m => DocumentID -> m Response
apiCallGet did = api $ do
  ctx <- getContext
  (msignatorylink :: Maybe SignatoryLinkID) <- lift $ readField "signatoryid"
  mmagichashh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  includeEvidenceAttachments <- lift $ (=="true") <$> getField' "evidenceAttachments"
  case (msignatorylink,mmagichashh) of
      (Just slid,Just mh) -> do
         doc <- apiGuardL (serverError "No document found") $  dbQuery $ GetDocumentByDocumentID did
         sl <- apiGuardJustM  (serverError "No document found") $ return $ getMaybeSignatoryLink (doc,slid)
         when (signatorymagichash sl /= mh) $ throwIO . SomeKontraException $ serverError "No document found"
         _ <- dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl)
                         (signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) (getEmail sl) (signatorylinkid sl))
         lift $ switchLang (getLang doc)

         mauser <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                       Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                       _ -> return Nothing
         pq <- case (mauser) of
                Just u -> dbQuery $ GetPadQueue $ (userid u)
                _ -> return Nothing

         Ok <$> documentJSON includeEvidenceAttachments False False pq (Just sl) doc
      _ -> do
        (user, _actor, external) <- getAPIUser APIDocCheck
        doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
        let msiglink = getMaybeSignatoryLink (doc,user)
        when_ (isJust $ msiglink) $ do
            let sl = fromJust msiglink
            dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl)
                 (signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) (getEmail sl) (signatorylinkid sl))

        mauser <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                       Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                       _ -> return Nothing
        pq <- case (mauser) of
                Just u -> dbQuery $ GetPadQueue $ (userid u)
                _ -> return Nothing

        let haspermission = (isJust msiglink)
                         || (isJust mauser && usercompany (fromJust mauser) == usercompany user && (useriscompanyadmin user || isDocumentShared doc))
        if (haspermission)
          then Ok <$> documentJSON includeEvidenceAttachments external ((userid <$> mauser) == (Just $ userid user)) pq msiglink doc
          else throwIO . SomeKontraException $ serverError "You do not have right to access document"

apiCallList :: Kontrakcja m => m Response
apiCallList = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  ctx <- getContext
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
  res <- lift $ jsonDocumentsList
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
  return res

apiCallEvidenceLog :: Kontrakcja m => DocumentID -> m Response
apiCallEvidenceLog did = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  ctx <- getContext
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
  res <- lift $ jsonDocumentEvidenceLog did
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
  return res


-- | This handler downloads main file of document. This means sealed file for Closed documents.
--   or one with preprinted fields if not closed

apiCallDownloadMainFile :: Kontrakcja m => DocumentID -> String -> m Response
apiCallDownloadMainFile did _nameForBrowser = api $ do

  (msid :: Maybe SignatoryLinkID) <- lift $ readField "signatorylinkid"
  mmh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msid

  doc <- do
           case (msid, mmh) of
            (Just sid, Just mh) -> apiGuardL  (forbidden "Access to file is forbiden") $ getDocByDocIDSigLinkIDAndMagicHash did sid mh
            _ ->  do
                  (user, _actor, external) <- getAPIUser APIDocCheck
                  if (external)
                    then do
                      ctx <- getContext
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
                      res <- apiGuardL  (forbidden "Access to file is forbiden")  $ getDocByDocID did
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
                      return res;
                    else apiGuardL  (forbidden "Access to file is forbiden")  $ getDocByDocID did

  content <- case documentstatus doc of
                Closed -> do
                  -- Here we should actually respond with a redirect
                  -- that waits for file to appear. Hopefully nobody
                  -- clicks download that fast.
                  case (documentsealedfile doc) of
                       Just _ -> lift $ documentSealedFileID doc >>= getFileIDContents
                       Nothing -> throwIO . SomeKontraException $ noAvailableYet "Sealed file is not ready yet, please try later"
                _ -> do
                  sourceFile <- (lift $ documentFileID doc) >>= apiGuardJustM  (serverError "No file") . dbQuery . GetFileByFileID
                  apiGuardL  (serverError "Can't get file content")  $ DocSeal.presealDocumentFile doc sourceFile
  respondWithPDF content
    where
      respondWithPDF contents = do
        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
            res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
        return res2


-- this one must be standard post with post params because it needs to
-- be posted from a browser form
-- Change main file, file stored in input "file" OR templateid stored in "template"
documentChangeMainFile :: Kontrakcja m => DocumentID -> m Response
documentChangeMainFile docid = api $ do
  ctx <- getContext
  aa <- apiGuard (forbidden "No user is logged in.") $ mkAuthorActor ctx
  doc <- apiGuardL forbidden' $ getDocByDocID docid
  apiGuard (forbidden "Logged in user is not author of document.") (isAuthor $ getSigLinkFor doc (ctxmaybeuser ctx))

  fileinput <- lift $ getDataFn' (lookInput "file")

  mft <- case fileinput of
    Nothing -> return Nothing
    Just (Input _ Nothing _) -> throwIO . SomeKontraException $ badInput "Missing file"
    Just (Input contentspec (Just filename') _contentType) -> do
      let filename = takeBaseName filename'
      let mformat = getFileFormatForConversion filename'
      content' <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
      content'' <- case mformat of
        Nothing -> return content'
        Just format -> do
          eres <- liftIO $ convertToPDF (ctxlivedocxconf ctx) (BS.concat $ BSL.toChunks content') format
          case eres of
            Left (LiveDocxIOError e) -> throwIO . SomeKontraException $ serverError $ show e
            Left (LiveDocxSoapError s)-> throwIO . SomeKontraException $ serverError s
            Right res -> return $ BSL.fromChunks [res]
      pdfcontent <- apiGuardL (badInput "The PDF is invalid.") $ liftIO $ do
                     cres <- preCheckPDF (concatChunks content'')
                     case cres of
                        Right c -> return (Right c)
                        Left m -> case (B64.decode $ (concatChunks content'')) of -- Salesforce hack. Drop this decoding when happstack-7.0.4 is included.
                                      Right dcontent -> preCheckPDF dcontent
                                      Left _ -> return (Left m)
      file <- dbUpdate $ NewFile filename pdfcontent
      return $ Just (fileid file, filename)

  case mft of
    Just  (fileid,filename) -> do
      dbUpdate $ AttachFile docid fileid aa
      apiGuardL' $ dbUpdate $ SetDocumentTitle docid filename aa
    Nothing -> dbUpdate $ DetachFile docid aa


  return ()



data SignatoryResource = SignatoryResource
instance FromReqURI SignatoryResource where
    fromReqURI s = Just SignatoryResource <| s == "signatory" |> Nothing

data AttachmentResource = AttachmentResource
instance FromReqURI AttachmentResource where
    fromReqURI s = Just AttachmentResource <| s == "attachment" |> Nothing

data FileResource = FileResource
instance FromReqURI FileResource where
    fromReqURI s = Just FileResource <| s == "file" |> Nothing

data MetadataResource = MetadataResource
instance FromReqURI MetadataResource where
    fromReqURI s = Just MetadataResource <| s == "metadata" |> Nothing

getSigLinkID :: Kontrakcja m => APIMonad m (SignatoryLinkID, MagicHash)
getSigLinkID = do
  msignatorylink <- lift $ readField "signatorylinkid"
  mmagichash <- lift $ maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  case (msignatorylink, mmagichash) of
       (Just sl, Just mh) -> return (sl,mh)
       _ -> throwIO . SomeKontraException $ badInput "The signatorylinkid or magichash were missing."

documentUploadSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentUploadSignatoryAttachment did _ sid _ aname _ = api $ do
  Log.debug $ "sigattachment ajax"
  (slid, magichash) <- getSigLinkID
  doc <- apiGuardL' $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  sl  <- apiGuard (forbidden "There is no signatory by that id.") $ getSigLinkFor doc sid
  let email = getEmail sl

  sigattach <- apiGuard (forbidden "There is no signatory attachment request of that name.") $ getSignatoryAttachment doc slid aname

  -- attachment must have no file
  apiGuard (actionNotAvailable "There is already a file attached for that attachment request.") (isNothing $ signatoryattachmentfile sigattach)

  -- pdf exists in input param "file"
  (Input contentspec (Just filename) _contentType) <- apiGuardL (badInput "The attachment PDF must be in the MIME part named 'file'. It was not found.") $ getDataFn' (lookInput "file")

  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content

  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext

  content <- apiGuardL (badInput "The PDF was invalid.") $ liftIO $ preCheckPDF (concatChunks content1)

  file <- dbUpdate $ NewFile (cleanFileName filename) content
  let actor = signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) email slid
  d <- apiGuardL (serverError "documentUploadSignatoryAttachment: SaveSigAttachment failed") . runMaybeT $ do
    dbUpdate $ SaveSigAttachment (documentid doc) sid aname (fileid file) actor
    Just newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
    return newdoc

  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment d sid aname

  return $ Created $ jsonSigAttachmentWithFile sigattach' (Just file)

documentDeleteSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentDeleteSignatoryAttachment did _ sid _ aname _ = api $ do
  Context{ctxtime, ctxipnumber} <- getContext
  (slid, magichash) <- getSigLinkID
  doc <- apiGuardL' $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash

  sl <- apiGuard (forbidden "The signatory does not exist.") $ getSigLinkFor doc sid
  let email = getEmail sl
      muid  = maybesignatory sl


  -- sigattachexists
  sigattach <- apiGuard (forbidden "The attachment with that name does not exist for the signatory.") $ getSignatoryAttachment doc sid aname

  -- attachment must have a file
  fileid <- apiGuard (actionNotAvailable "That signatory attachment request does not have a file uploaded for it, or it has been previously deleted.") $ signatoryattachmentfile sigattach

  d <- apiGuardL (serverError "documentUploadSignatoryAttachment: SaveSigAttachment failed") . runMaybeT $ do
    dbUpdate $ DeleteSigAttachment (documentid doc) sid fileid (signatoryActor ctxtime ctxipnumber muid email sid)
    Just newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
    return newdoc

  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment d sid aname

  return $ jsonSigAttachmentWithFile sigattach' Nothing
