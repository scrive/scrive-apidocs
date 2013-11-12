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
import File.File
import Text.JSON.Pretty
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
import qualified Doc.SignatoryScreenshots as SignatoryScreenshots
import qualified ELegitimation.Control as BankID
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
import File.Model
import File.Storage
import qualified PadQueue.API as PadQueue
import Data.String.Utils (replace)
import EvidenceLog.Control
import Control.Exception.Lifted
import Doc.DocControl
import Utils.Either
import Doc.SignatoryScreenshots
import Doc.Conditions
import Company.Model
import Company.CompanyUI
import User.Utils
import User.UserView

documentAPI :: Route (KontraPlus Response)
documentAPI = dir "api" $ choice
  [ dir "frontend" $ versionedAPI Frontend
  , versionedAPI V1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ versionedAPI V1
  ]

versionedAPI :: APIVersion -> Route (KontraPlus Response)
versionedAPI _version = choice [

  dir "createfromfile"     $ hPost $ toK0 $ apiCallCreateFromFile,
  dir "createfromtemplate" $ hPostAllowHttp $ toK1 $ apiCallCreateFromTemplate,
  dir "clone"              $ hPost $ toK1   $  apiCallClone,
  dir "update"             $ hPost $ toK1 $ apiCallUpdate,
  dir "ready"              $ hPostAllowHttp $ toK1 $ apiCallReady,
  dir "cancel"             $ hPost $ toK1 $ apiCallCancel,
  dir "reject"             $ hPost $ toK2 $ apiCallReject,
  dir "sign"               $ hPost $ toK2 $ apiCallSign,

  dir "restart"            $ hPost $ toK1 $ apiCallRestart,
  dir "prolong"            $ hPost $ toK1 $ apiCallProlong,


  dir "remind"             $ hPost $ toK1 $ apiCallRemind,
  dir "delete"             $ hDeleteAllowHttp  $ toK1 $ apiCallDelete,
  dir "get"                $ hGetAllowHttp $ toK1 $ apiCallGet,
  dir "list"               $ hGetAllowHttp $ apiCallList,
  dir "history"           $ hGetAllowHttp $ apiCallHistory,
  dir "downloadmainfile"   $ hGetAllowHttp  $ toK2 $ apiCallDownloadMainFile,
  dir "downloadfile"       $ hGetAllowHttp  $ toK3 $ apiCallDownloadFile,

  dir "padqueue"           $ PadQueue.padqueueAPI,
  dir "changemainfile"     $ hPost $ toK1 $ apiCallChangeMainFile,

  dir "documentbrandingforsignview" $ hGet $ toK2 $ apiCallGetBrandingForSignView,

  dir "addsignatoryattachment"    $ hPost $ toK3 $ documentUploadSignatoryAttachment,
  dir "deletesignatoryattachment" $ hDeleteAllowHttp  $ toK3 $ documentDeleteSignatoryAttachment
  ]

-- | Windows Explorer set the full path of a file, for example:
--
--    c:\My Documents\Things\Untitle Document.doc
--
-- We drop all up to and including last backslash here.
dropFilePathFromWindows :: FilePath -> FilePath
dropFilePathFromWindows = reverse . takeWhile (/='\\') . reverse

{- New API calls-}
apiCallCreateFromFile :: Kontrakcja m => m Response
apiCallCreateFromFile = api $ do
  ctx <- getContext
  (user, actor, external) <- getAPIUser APIDocCreate
  isTpl <- lift $ isFieldSet "template"
  let doctype = (Template <| isTpl |> Signable)
  minput <- lift $ getDataFn' (lookInput "file")
  (mfile, title) <- case minput of
    Nothing -> do
      title <- renderTemplate_ ("newDocumentTitle" <| not isTpl |> "newTemplateTitle")
      return (Nothing,  replace "  " " " $ title ++ " " ++ formatMinutesTimeSimple (ctxtime ctx))
    Just (Input _ Nothing _) -> throwIO . SomeKontraException $ badInput "Missing file"
    Just (Input contentspec (Just filename'') _contentType) -> do
      let filename' = dropFilePathFromWindows filename''
      let mformat = getFileFormatForConversion filename'
      content' <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
      (content'', filename) <- case mformat of
        Nothing -> return (content', filename')
        Just format -> do
          eres <- liftIO $ convertToPDF (ctxlivedocxconf ctx) (BS.concat $ BSL.toChunks content') format
          case eres of
            Left (LiveDocxIOError e) -> throwIO . SomeKontraException $ serverError $ show e
            Left (LiveDocxSoapError s)-> throwIO . SomeKontraException $ serverError s
            Right res -> do
              -- change extension from .doc, .docx and others to .pdf
              let filename = takeBaseName filename' ++ ".pdf"
              return $ (BSL.fromChunks [res], filename)

      pdfcontent <- apiGuardL (badInput "The PDF is invalid.") $ liftIO $ do
                     cres <- preCheckPDF (concatChunks content'')
                     case cres of
                        Right c -> return (Right c)
                        Left m -> case (B64.decode $ (concatChunks content'')) of -- Salesforce hack. Drop this decoding when happstack-7.0.4 is included.
                                      Right dcontent -> preCheckPDF dcontent
                                      Left _ -> return (Left m)
      fileid' <- dbUpdate $ NewFile filename pdfcontent
      return (Just fileid', takeBaseName filename)
  Just doc <- dbUpdate $ NewDocument user title doctype 0 actor
  when_ (not $ external) $ dbUpdate $ SetDocumentUnsavedDraft [documentid doc] True
  case mfile of
    Nothing -> return ()
    Just fileid' -> do
      dbUpdate $ AttachFile (documentid doc) fileid' actor
      return ()
  doc' <- dbQuery $ GetDocumentByDocumentID (documentid doc)
  Created <$> documentJSON (Just $ userid user) False True True Nothing Nothing doc'


apiCallCreateFromTemplate :: Kontrakcja m => DocumentID -> m Response
apiCallCreateFromTemplate did =  api $ do
  (user, actor, external) <- getAPIUser APIDocCreate
  template <- dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink template
  auser <- apiGuardJustM (serverError "No user found") $ dbQuery $ GetUserByIDIncludeDeleted auid
  let haspermission = (userid auser == userid user) ||
                      (usercompany auser == usercompany user &&  isDocumentShared template)
  newdoc <- if (isTemplate template && haspermission)
                    then do
                      mndid <- dbUpdate $ CloneDocumentWithUpdatedAuthor user template actor
                      when (isNothing mndid) $
                          throwIO . SomeKontraException $ serverError "Can't clone given document"
                      dbUpdate $ DocumentFromTemplate (fromJust mndid) actor
                      dbQuery $ GetDocumentByDocumentID $ (fromJust mndid)
                    else throwIO . SomeKontraException $ serverError "Id did not matched template or you do not have right to access document"
  when_ (not $ external) $ dbUpdate $ SetDocumentUnsavedDraft [documentid newdoc] True
  Created <$> documentJSON (Just $ userid user) False True True Nothing Nothing newdoc


apiCallClone :: Kontrakcja m => DocumentID -> m Response
apiCallClone did =  api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  doc <- dbQuery $ GetDocumentByDocumentID $ did
  if isAuthor (doc,user)
     then do
         mndid <- dbUpdate $ CloneDocumentWithUpdatedAuthor user doc actor
         when (isNothing mndid) $
             throwIO . SomeKontraException $ serverError "Can't clone given document"
         newdoc <- dbQuery $ GetDocumentByDocumentID $ (fromJust mndid)
         Created <$> documentJSON (Just $ userid user) False True  True Nothing Nothing newdoc
     else throwIO . SomeKontraException $ serverError "Id did not matched template or you do not have right to access document"





apiCallUpdate :: Kontrakcja m => DocumentID -> m Response
apiCallUpdate did = api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  doc <- dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (documentstatus doc /= Preparation) $ do
        checkObjectVersionIfProvidedAndThrowError did (serverError "Document is not a draft or template")
  when (not $ (auid == userid user)) $ do
        throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
  jsons <- apiGuardL (badInput "The MIME part 'json' must exist and must be a JSON.") $ getDataFn' (look "json")
  json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $ case decode jsons of
                                                                               J.Ok js -> Just js
                                                                               _ -> Nothing
  Log.debug $ "Document " ++ show did ++ " updated with JSON:\n" ++ show (pp_value json)
  draftData   <- apiGuardJustM (badInput "Given JSON does not represent valid draft data.") $ return $ fromJSValueWithUpdate (Just doc) json
  when (draftIsChangingDocument draftData doc) (checkObjectVersionIfProvided did) -- If we will change document, then we want to be sure that object version is ok.
  newdocument <-  apiGuardL (serverError "Could not apply draft data") $ applyDraftDataToDocument doc draftData actor
  triggerAPICallbackIfThereIsOne newdocument
  Ok <$> documentJSON (Just $ userid user) False True True Nothing Nothing newdocument

apiCallReady :: (MonadBaseControl IO m, Kontrakcja m) => DocumentID -> m Response
apiCallReady did =  api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  doc <- dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  if (documentstatus doc == Pending && not (any hasSigned $ documentsignatorylinks doc))
   then Accepted <$> documentJSON (Just $ userid user) False True True Nothing Nothing doc
   else do
    checkObjectVersionIfProvided did
    when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
    when (documentstatus doc /= Preparation) $ do
          checkObjectVersionIfProvidedAndThrowError did $ (conflictError "Document is not a draft")
    when (isTemplate doc) $ do
          checkObjectVersionIfProvidedAndThrowError did $ (serverError "Document is not a draft")
    timezone <- lift $ mkTimeZoneName =<< (fromMaybe "Europe/Stockholm" <$> getField "timezone")
    when (not $ all ((/=EmailDelivery) . signatorylinkdeliverymethod ||^ isGood . asValidEmail . getEmail) (documentsignatorylinks doc)) $ do
          throwIO . SomeKontraException $ serverError "Some signatories don't have a valid email address set."
    when (isNothing $ documentfile doc) $ do
          throwIO . SomeKontraException $ serverError "File must be provided before document can be made ready."
    newdocument <- lift $ do
              t <- ctxtime <$> getContext
              dbUpdate $ PreparationToPending did actor (Just timezone)
              dbUpdate $ SetDocumentInviteTime did t actor
              dbQuery $ GetDocumentByDocumentID did

    skipauthorinvitation <- lift $ isFieldSet "skipauthorinvitation"
    lift $ postDocumentPreparationChange newdocument skipauthorinvitation
    newdocument' <- dbQuery $ GetDocumentByDocumentID $ did
    Accepted <$> documentJSON (Just $ userid user) False True True Nothing Nothing newdocument'

apiCallCancel :: (MonadBaseControl IO m, Kontrakcja m) =>  DocumentID -> m Response
apiCallCancel did =  api $ do
    checkObjectVersionIfProvided did
    (user, actor, _) <- getAPIUser APIDocSend
    doc <- dbQuery $ GetDocumentByDocumentID $ did
    auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
    when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
    when (documentstatus doc /= Pending ) $ do
          throwIO . SomeKontraException $ (conflictError "Document is not pending")
    dbUpdate $ CancelDocument (documentid doc) actor
    newdocument <- dbQuery $ GetDocumentByDocumentID $ did
    lift $ postDocumentCanceledChange newdocument
    newdocument' <- dbQuery $ GetDocumentByDocumentID $ did
    Accepted <$> documentJSON (Just $ userid user) False True True Nothing Nothing newdocument'


apiCallReject :: (MonadBaseControl IO m, Kontrakcja m) =>  DocumentID -> SignatoryLinkID -> m Response
apiCallReject did slid = api $ do
  checkObjectVersionIfProvided did
  (mh,muid) <- do
    mh' <- dbQuery $ GetDocumentSessionToken slid
    case mh' of
      Just mh'' ->  return (mh'',Nothing)
      Nothing -> do
         (user, _ , _) <- getAPIUser APIPersonal
         mh'' <- lift $ getMagicHashForDocumentSignatoryWithUser  did slid user
         case mh'' of
           Nothing -> throwIO . SomeKontraException $ serverError "Magic hash for signatory was not provided"
           Just mh''' -> return (mh''',Just $ userid user)
  doc <- lift $ dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
  do
      ctx <- getContext
      let Just sll = getSigLinkFor doc slid
          actor = signatoryActor ctx sll
      customtext <- lift $ getOptionalField  asValidInviteText "customtext"
      lift $ switchLang (getLang doc)
      lift $ (dbUpdate $ RejectDocument did slid customtext actor)
          `catchKontra` (\(DocumentStatusShouldBe _ _ i) -> throwIO . SomeKontraException $ conflictError $ "Document not pending but " ++ show i)
          `catchKontra` (\(SignatoryHasAlreadySigned) -> throwIO . SomeKontraException $ conflictError $ "Signatory has already signed")
      doc' <- dbQuery $ GetDocumentByDocumentID did

      lift $ postDocumentRejectedChange doc' slid
      doc'' <- dbQuery $ GetDocumentByDocumentID did
      Accepted <$> documentJSON muid False True True Nothing Nothing doc''


apiCallSign :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m Response
apiCallSign  did slid = api $ do
  checkObjectVersionIfProvided did
  Log.debug $ "Ready to sign a docment " ++ show did ++ " for signatory " ++ show slid
  (mh,muid) <- do
    mh' <- dbQuery $ GetDocumentSessionToken slid
    case mh' of
      Just mh'' ->  return (mh'',Nothing)
      Nothing -> do
         (user, _ , _) <- getAPIUser APIPersonal
         Log.debug $ "User is " ++ show user
         mh'' <- lift $ getMagicHashForDocumentSignatoryWithUser  did slid user
         case mh'' of
           Nothing -> throwIO . SomeKontraException $ serverError "Can't perform this action. Not authorized."
           Just mh''' -> return (mh''',Just $ userid user)
  Log.debug "We have magic hash for this operation"
  screenshots <- lift $ (fromMaybe emptySignatoryScreenshots) <$> join <$> fmap fromJSValue <$> getFieldJSON "screenshots"
  fields <- do
      eFieldsJSON <- lift $ getFieldJSON "fields"
      case eFieldsJSON of
           Nothing -> throwIO . SomeKontraException $ serverError "No fields description provided or fields description is not a valid JSON array"
           Just fieldsJSON -> do
             mvalues <- withJSValue fieldsJSON $ fromJSValueCustomMany $ do
                  ft <- fromJSValueM
                  val <- fromJSValueField "value"
                  return $ case (ft,val) of
                            (Just ft', Just val') -> Just (ft',val')
                            _ -> Nothing
             case mvalues of
               Nothing -> throwIO . SomeKontraException $ serverError "Fields description json has invalid format"
               Just values -> return values
  mprovider <- lift $ readField "eleg"
  Log.debug $ "All parameters read and parsed"
  edoc <- lift $ (case mprovider of
            Nothing -> Right <$> (signDocumentWithEmailOrPad did slid mh fields screenshots)
            Just provider -> handleSignWithEleg did slid mh fields screenshots provider)
              `catchKontra` (\(DocumentStatusShouldBe _ _ i) -> throwIO . SomeKontraException $ conflictError $ "Document not pending but " ++ show i)
              `catchKontra` (\(SignatoryHasAlreadySigned) -> throwIO . SomeKontraException $ conflictError $ "Signatory has already signed")
  Log.debug $ "Signing done, result is " ++ show (isRight edoc)
  case edoc of
    Right (doc, olddoc) -> do
      lift $ postDocumentPendingChange doc olddoc
      udoc <- dbQuery $ GetDocumentByDocumentID did
      lift $ handleAfterSigning udoc slid
      udoc' <- dbQuery $ GetDocumentByDocumentID did
      Accepted <$> documentJSON muid False True True Nothing Nothing udoc'
    Left msg -> do -- On eleg error we return document, but it will have status cancelled instead of closed.
      Log.error $ "Eleg verification for document #" ++ show did ++ " failed with message: " ++ msg
      doc' <- dbQuery $ GetDocumentByDocumentID did
      Accepted <$> documentJSON muid False True True Nothing Nothing doc'


{- | Utils for signing with eleg -}

handleSignWithEleg :: Kontrakcja m => DocumentID -> SignatoryLinkID -> MagicHash -> [(FieldType, String)] -> SignatoryScreenshots.SignatoryScreenshots -> SignatureProvider
                     -> m (Either String (Document, Document))
handleSignWithEleg documentid signatorylinkid magichash fields screenshots provider = do
  transactionid <- getDataFnM $ look "transactionid"
  esigninfo <- case provider of
    MobileBankIDProvider -> BankID.verifySignatureAndGetSignInfoMobile documentid signatorylinkid magichash fields transactionid
    _ -> do
          signature <- getDataFnM $ look "signature"
          BankID.verifySignatureAndGetSignInfo documentid signatorylinkid magichash fields provider signature transactionid
  case esigninfo of
    BankID.Problem msg -> return $ Left msg
    BankID.Mismatch msg sfn sln spn -> do
      document <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash documentid signatorylinkid magichash
      handleMismatch document signatorylinkid msg sfn sln spn
      return $ Left msg
    BankID.Sign sinfo -> Right <$> signDocumentWithEleg documentid signatorylinkid magichash fields sinfo screenshots

handleMismatch :: Kontrakcja m => Document -> SignatoryLinkID -> String -> String -> String -> String -> m ()
handleMismatch doc sid msg sfn sln spn = do
        ctx <- getContext
        let Just sl = getSigLinkFor doc sid
        Log.eleg $ "Information from eleg did not match information stored for signatory in document." ++ show msg
        Just newdoc <- runMaybeT $ do
          dbUpdate $ ELegAbortDocument (documentid doc) sid msg sfn sln spn
           (signatoryActor ctx sl)
          newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
          return newdoc
        postDocumentCanceledChange newdoc

{- End of utils-}


apiCallRestart :: (MonadBaseControl IO m, Kontrakcja m) =>  DocumentID -> m Response
apiCallRestart did =  api $ do
    checkObjectVersionIfProvided did
    (user, actor, _) <- getAPIUser APIDocSend
    doc <- dbQuery $ GetDocumentByDocumentID $ did
    auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
    when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
    when (documentstatus doc `elem` [Pending,Preparation, Closed] ) $ do
          throwIO . SomeKontraException $ (conflictError "Document can not be restarted")
    newdocument <- apiGuardJustM (serverError "Document can't be restarted") $ dbUpdate $ RestartDocument doc actor
    Accepted <$> documentJSON (Just $ userid user) False True True Nothing Nothing newdocument

apiCallProlong :: (MonadBaseControl IO m, Kontrakcja m) =>  DocumentID -> m Response
apiCallProlong did =  api $ do
    checkObjectVersionIfProvided did
    (user, actor, _) <- getAPIUser APIDocSend
    doc <- dbQuery $ GetDocumentByDocumentID $ did
    auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
    when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
    when (documentstatus doc /= Timedout ) $ do
          throwIO . SomeKontraException $ (conflictError "Document is not timedout")
    mdays <- lift $ getDefaultedField 1 asValidNumber "days"
    days <- case mdays of
         Nothing -> throwIO . SomeKontraException $ (badInput "Number of days to sing must be a valid number, between 1 and 90")
         Just n -> if (n < 1 || n > 90)
                            then throwIO . SomeKontraException $ (badInput "Number of days to sing must be a valid number, between 1 and 90")
                            else return n
    dbUpdate $ ProlongDocument (documentid doc) days actor
    triggerAPICallbackIfThereIsOne doc
    newdocument <- dbQuery $ GetDocumentByDocumentID $ did
    Accepted <$> documentJSON (Just $ userid user) False True True Nothing Nothing newdocument


apiCallRemind :: Kontrakcja m => DocumentID -> m Response
apiCallRemind did =  api $ do
  ctx <- getContext
  (user, actor , _) <- getAPIUser APIDocSend
  doc <- dbQuery $ GetDocumentByDocumentID $ did
  when (documentstatus doc /= Pending) $ do
        throwIO . SomeKontraException $ serverError "Can't send reminder for documents that are not pending"
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
  _ <- lift $ sendAllReminderEmails ctx actor user did
  newdocument <- dbQuery $ GetDocumentByDocumentID $ did
  Accepted <$> documentJSON (Just $ userid user) False True True Nothing Nothing newdocument

apiCallDelete :: Kontrakcja m => DocumentID -> m Response
apiCallDelete did =  api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  doc <- dbQuery $ GetDocumentByDocumentID $ did
  mauser <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                       Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                       _ -> return Nothing
  let msl = getSigLinkFor doc user
  let haspermission = (isJust msl)
                   || (isJust mauser && usercompany (fromJust mauser) == usercompany user && (useriscompanyadmin user))
  when (not haspermission) $ do
         throwIO . SomeKontraException $ serverError "Permission problem. Not connected to document."
  dbUpdate $ ArchiveDocument (userid user) did actor

  Accepted <$> (runJSONGenT $ return ())




apiCallGet :: Kontrakcja m => DocumentID -> m Response
apiCallGet did = api $ do
  ctx <- getContext
  (msignatorylink :: Maybe SignatoryLinkID) <- lift $ readField "signatoryid"
  mmagichashh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  includeEvidenceAttachments <- lift $ (=="true") <$> getField' "evidenceAttachments"
  case (msignatorylink,mmagichashh) of
      (Just slid,Just mh) -> do
         doc <- dbQuery $ GetDocumentByDocumentID did
         sl <- apiGuardJustM  (serverError "No document found") $ return $ getMaybeSignatoryLink (doc,slid)
         when (signatorymagichash sl /= mh) $ throwIO . SomeKontraException $ serverError "No document found"
         when (not (isTemplate doc) && (not (isPreparation doc)) && (not (isClosed doc)) ) $
           dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl)
                         (signatoryActor ctx sl)
         lift $ switchLang (getLang doc)

         mauser <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                       Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                       _ -> return Nothing
         pq <- case (mauser) of
                Just u -> dbQuery $ GetPadQueue $ (userid u)
                _ -> return Nothing

         Ok <$> documentJSON Nothing includeEvidenceAttachments False False pq (Just sl) doc
      _ -> do
        (user, _actor, external) <- getAPIUser APIDocCheck
        doc <- dbQuery $ GetDocumentByDocumentID $ did
        let msiglink = getMaybeSignatoryLink (doc,user)
        when_ ((isJust msiglink) && (not (isPreparation doc)) && (not (isClosed doc)) && (not (isTemplate doc))) $ do
            let sl = fromJust msiglink
            dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl)
                 (signatoryActor ctx sl)

        mauser <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                       Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                       _ -> return Nothing
        pq <- case (mauser) of
                Just u -> dbQuery $ GetPadQueue $ (userid u)
                _ -> return Nothing

        let haspermission = (isJust msiglink)
                         || (isJust mauser && usercompany (fromJust mauser) == usercompany user && (useriscompanyadmin user || isDocumentShared doc))
        if (haspermission)
          then do
            Ok <$> documentJSON (Just $ userid user) includeEvidenceAttachments external ((userid <$> mauser) == (Just $ userid user)) pq msiglink doc
          else throwIO . SomeKontraException $ serverError "You do not have right to access document"

apiCallList :: Kontrakcja m => m Response
apiCallList = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  ctx <- getContext
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
  res <- lift $ jsonDocumentsList
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
  return res

apiCallHistory :: Kontrakcja m => DocumentID -> m Response
apiCallHistory did = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  ctx <- getContext
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
  mlang <- lift $  (join . (fmap langFromCode)) <$> getField "lang"
  lift $ switchLang $ fromMaybe (lang $ usersettings user) mlang
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
            (Just sid, Just mh) -> lift $ dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh
            _ ->  do
                  (user, _actor, external) <- getAPIUser APIDocCheck
                  if (external)
                    then do
                      ctx <- getContext
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
                      res <- lift $ getDocByDocID did
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
                      return res;
                    else lift $ getDocByDocID did

  content <- case documentstatus doc of
                Closed -> do
                  file <- apiGuardJustM (noAvailableYet "Not ready, please try later") $ documentsealedfileM doc
                  getFileIDContents $ fileid file
                _ -> do
                  sourceFile <- apiGuardJustM  (serverError "No file") $ documentfileM doc
                  apiGuardL  (serverError "Can't get file content")  $ DocSeal.presealDocumentFile doc sourceFile
  respondWithPDF content
    where
      respondWithPDF contents = do
        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
            res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
        return res2


apiCallDownloadFile :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
apiCallDownloadFile did fileid _nameForBrowser = api $ do
  (msid :: Maybe SignatoryLinkID) <- lift $ readField "signatorylinkid"
  mmh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msid
  doc <- do
           case (msid, mmh) of
            (Just sid, Just mh) -> lift $ dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh
            _ ->  do
                  (user, _actor, external) <- getAPIUser APIDocCheck
                  if (external)
                    then do
                      ctx <- getContext
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
                      res <- lift $ getDocByDocID did
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
                      return res;
                    else lift $ getDocByDocID did
  let allfiles = maybeToList (documentfile doc) ++ maybeToList (documentsealedfile doc) ++
                      (authorattachmentfile <$> documentauthorattachments doc) ++
                      (catMaybes $ Prelude.map signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc)
  if (all (/= fileid) allfiles)
     then throwIO . SomeKontraException $ forbidden "Access to file is forbiden."
     else do
        content <- getFileIDContents fileid
        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [content]) Nothing
            res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
        return res2


-- this one must be standard post with post params because it needs to
-- be posted from a browser form
-- Change main file, file stored in input "file" OR templateid stored in "template"
apiCallChangeMainFile :: Kontrakcja m => DocumentID -> m Response
apiCallChangeMainFile docid = api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  checkObjectVersionIfProvided docid
  doc <- dbQuery $ GetDocumentByDocumentID $ docid
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (documentstatus doc /= Preparation) $ throwIO . SomeKontraException $ (serverError "Document is not a draft or template")
  when (not $ (auid == userid user)) $ do
        throwIO . SomeKontraException $ serverError "Permission problem. Not an author."

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
          ctx <- getContext
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
      fileid' <- dbUpdate $ NewFile filename pdfcontent
      return $ Just (fileid', filename)

  case mft of
    Just  (fileid,filename) -> do
      dbUpdate $ AttachFile docid fileid actor
      apiGuardL' $ dbUpdate $ SetDocumentTitle docid filename actor
    Nothing -> dbUpdate $ DetachFile docid actor
  doc' <- dbQuery $ GetDocumentByDocumentID $ docid
  Accepted <$> documentJSON (Just $ userid user) False True True Nothing Nothing doc'

apiCallGetBrandingForSignView :: Kontrakcja m => DocumentID -> SignatoryLinkID ->  m Response
apiCallGetBrandingForSignView did slid = api $ do
  ctx <- getContext
  magichash <- apiGuardL (serverError "No document found")  $ dbQuery $ GetDocumentSessionToken slid
  doc <- dbQuery $ GetDocumentByDocumentID did
  sl <- apiGuardJustM  (serverError "No document found") $ return $ getMaybeSignatoryLink (doc,slid)
  when (signatorymagichash sl /= magichash) $ throwIO . SomeKontraException $ serverError "No document found"
  authorid <- apiGuardL (serverError "Document problem | No author") $ return $ getAuthorSigLink doc >>= maybesignatory
  user <- apiGuardL (serverError "Document problem | No author in DB") $ dbQuery $ GetUserByID authorid
  company <- getCompanyForUser user
  companyui <- dbQuery $ GetCompanyUI (companyid company)
  Ok <$> signviewBrandingJSON ctx user company companyui

getSigLinkID :: Kontrakcja m => APIMonad m (SignatoryLinkID, MagicHash)
getSigLinkID = do
  msignatorylink <- lift $ readField "signatorylinkid"
  mmagichash <- lift $ maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  case (msignatorylink, mmagichash) of
       (Just sl, Just mh) -> return (sl,mh)
       _ -> throwIO . SomeKontraException $ badInput "The signatorylinkid or magichash were missing."

documentUploadSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> String -> m Response
documentUploadSignatoryAttachment did sid aname = api $ do
  Log.debug $ "sigattachment ajax"
  (slid, magichash) <- getSigLinkID
  doc <- lift $ dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid magichash
  sl  <- apiGuard (forbidden "There is no signatory by that id.") $ getSigLinkFor doc sid

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

  let sanitizedFileName = dropFilePathFromWindows filename
  fileid' <- dbUpdate $ NewFile sanitizedFileName content
  let actor = signatoryActor ctx sl
  d <- apiGuardL (serverError "documentUploadSignatoryAttachment: SaveSigAttachment failed") . runMaybeT $ do
    dbUpdate $ SaveSigAttachment doc sid sigattach fileid' actor
    newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
    return newdoc

  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment d sid aname

  return $ Created $ jsonSigAttachmentWithFile sigattach' (Just (fileid',sanitizedFileName))

documentDeleteSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID ->  String -> m Response
documentDeleteSignatoryAttachment did sid aname = api $ do
  ctx <- getContext
  (slid, magichash) <- getSigLinkID
  doc <- lift $ dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid magichash

  sl <- apiGuard (forbidden "The signatory does not exist.") $ getSigLinkFor doc sid

  -- sigattachexists
  sigattach <- apiGuard (forbidden "The attachment with that name does not exist for the signatory.") $ getSignatoryAttachment doc sid aname

  -- attachment must have a file
  fileid <- apiGuard (actionNotAvailable "That signatory attachment request does not have a file uploaded for it, or it has been previously deleted.") $ signatoryattachmentfile sigattach

  d <- apiGuardL (serverError "documentDeleteSignatoryAttachment: DeleteSigAttachment failed") . runMaybeT $ do
    dbUpdate $ DeleteSigAttachment doc sid fileid (signatoryActor ctx sl)
    newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
    return newdoc

  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment d sid aname

  return $ jsonSigAttachmentWithFile sigattach' Nothing


checkObjectVersionIfProvided ::  (Kontrakcja m) => DocumentID -> APIMonad m ()
checkObjectVersionIfProvided did = lift $ do
    mov <- readField "objectversion"
    case mov of
        Just ov -> dbQuery $ CheckDocumentObjectVersionIs did ov
        Nothing -> return ()
  `catchKontra` (\DocumentObjectVersionDoesNotMatch -> throwIO . SomeKontraException $ conflictError $ "Document object version does not match")

checkObjectVersionIfProvidedAndThrowError ::  (Kontrakcja m) => DocumentID -> APIError -> APIMonad m ()
checkObjectVersionIfProvidedAndThrowError did err = lift $ do
    mov <- readField "objectversion"
    case mov of
        Just ov -> (dbQuery $ CheckDocumentObjectVersionIs did ov)
                      `catchKontra` (\DocumentObjectVersionDoesNotMatch -> throwIO . SomeKontraException $ conflictError $ "Document object version does not match")
        Nothing -> return ()
    throwIO . SomeKontraException $ err


-- I really want to add a url to the file in the json, but the only
-- url at the moment requires a sigid/mh pair
jsonSigAttachmentWithFile :: SignatoryAttachment -> Maybe (FileID,String) -> JSValue
jsonSigAttachmentWithFile sa mfile =
  runJSONGen $ do
    value "name"        $ signatoryattachmentname sa
    value "description" $ signatoryattachmentdescription sa
    case mfile of
      Nothing   -> value "requested" True
      Just (fileid',filename') -> object "file" $ do
        value "id"   $ show fileid'
        value "name" $ filename'
