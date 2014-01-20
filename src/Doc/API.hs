module Doc.API (
    documentAPI
  , apiCallCreateFromFile      -- Exported for tests
  , apiCallCreateFromTemplate  -- Exported for tests
  , apiCallGet                 -- Exported for tests
  , apiCallUpdate              -- Exported for tests
  , apiCallReady               -- Exported for tests
  , apiCallSign                -- Exported for tests
  ) where

import Control.Conditional (whenM, unlessM, ifM)
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
import Doc.SealStatus (SealStatus(..))
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
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Util.Actor
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Util.MonadUtils (guardJustM)
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
import Doc.DocumentMonad (DocumentMonad, withDocument, withDocumentID, withDocumentM, theDocument)
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
import Data.Map as Map hiding (map)

import Doc.DocSeal as DocSeal
import qualified Data.ByteString.UTF8 as BS hiding (length)
import File.Model
import File.Storage
import qualified PadQueue.API as PadQueue
import Data.String.Utils (replace)
import EvidenceLog.Control
import Control.Exception.Lifted
import Doc.DocControl
import Doc.SignatoryScreenshots
import Doc.Conditions
import Company.Model
import Company.CompanyUI
import User.Utils
import User.UserView
import Data.List
import Data.Char
import Attachment.Model
import Utils.Read
import Doc.DocMails
import Doc.AutomaticReminder.Model
import Utils.Monoid

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
  dir "setattachments"     $ hPost $ toK1 $ apiCallSetAuthorAttachemnts,
  dir "ready"              $ hPostAllowHttp $ toK1 $ apiCallReady,
  dir "cancel"             $ hPost $ toK1 $ apiCallCancel,
  dir "reject"             $ hPost $ toK2 $ apiCallReject,
  dir "checksign"          $ hPost $ toK2 $ apiCallCheckSign,
  dir "sign"               $ hPost $ toK2 $ apiCallSign,

  dir "restart"            $ hPost $ toK1 $ apiCallRestart,
  dir "prolong"            $ hPost $ toK1 $ apiCallProlong,
  dir "setautoreminder"    $ hPost $ toK1 $ apiCallSetAutoReminder,


  dir "remind"             $ hPost $ toK1 $ apiCallRemind,
  dir "delete"             $ hDeleteAllowHttp  $ toK1 $ apiCallDelete,
  dir "get"                $ hGetAllowHttp $ toK1 $ apiCallGet,

  dir "list"               $ hGetAllowHttp $ apiCallList,
  dir "checkavailable"     $ hPostAllowHttp $ apiCallCheckAvailable,

  dir "history"           $ hGetAllowHttp $ apiCallHistory,
  dir "downloadmainfile"   $ hGetAllowHttp  $ toK2 $ apiCallDownloadMainFile,
  dir "downloadfile"       $ hGetAllowHttp  $ toK3 $ apiCallDownloadFile,

  dir "padqueue"           $ PadQueue.padqueueAPI,
  dir "changemainfile"     $ hPost $ toK1 $ apiCallChangeMainFile,

  dir "documentbrandingforsignview" $ hGet $ toK2 $ apiCallGetBrandingForSignView,

  dir "setsignatoryattachment"    $ hPost $ toK3 $ apiCallSetSignatoryAttachment
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
  isTpl <- isFieldSet "template"
  let doctype = (Template <| isTpl |> Signable)
  minput <- getDataFn' (lookInput "file")
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
  guardJustM (dbUpdate $ NewDocument user title doctype 0 actor) `withDocumentM` do
    when_ (not $ external) $ dbUpdate $ SetDocumentUnsavedDraft True
    case mfile of
      Nothing -> return ()
      Just fileid' -> do
        dbUpdate $ AttachFile fileid' actor
    Created <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)

apiCallCreateFromTemplate :: Kontrakcja m => DocumentID -> m Response
apiCallCreateFromTemplate did =  api $ do
  (user, actor, external) <- getAPIUser APIDocCreate
  template <- dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink template
  auser <- apiGuardJustM (serverError "No user found") $ dbQuery $ GetUserByIDIncludeDeleted auid
  let haspermission = (userid auser == userid user) ||
                      (usercompany auser == usercompany user &&  isDocumentShared template)
  unless (isTemplate template && haspermission) $ do
    throwIO $ SomeKontraException $ serverError "Id did not matched template or you do not have right to access document"
  (apiGuardJustM (serverError "Can't clone given document") (dbUpdate $ CloneDocumentWithUpdatedAuthor user template actor) >>=) $ flip withDocumentID $ do
    dbUpdate $ DocumentFromTemplate actor
    when_ (not $ external) $ dbUpdate $ SetDocumentUnsavedDraft True
    Created <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)


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
         Created <$> documentJSON (Just $ user) False True  True Nothing Nothing newdoc
     else throwIO . SomeKontraException $ serverError "Id did not matched template or you do not have right to access document"





apiCallUpdate :: Kontrakcja m => DocumentID -> m Response
apiCallUpdate did = api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    unlessM (isPreparation <$> theDocument) $ do
          checkObjectVersionIfProvidedAndThrowError did (serverError "Document is not a draft or template")
    when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
    jsons <- apiGuardL (badInput "The MIME part 'json' must exist and must be a JSON.") $ getDataFn' (look "json")
    json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $ case decode jsons of
                                                                                 J.Ok js -> Just js
                                                                                 _ -> Nothing
    Log.debug $ "Document " ++ show did ++ " updated with JSON:\n" ++ show (pp_value json)
    draftData   <- apiGuardJustM (badInput "Given JSON does not represent valid draft data.") $ flip fromJSValueWithUpdate json . Just <$> theDocument
    whenM (draftIsChangingDocument draftData <$> theDocument) $ do
      checkObjectVersionIfProvided did -- If we will change document, then we want to be sure that object version is ok.
    applyDraftDataToDocument draftData actor
    triggerAPICallbackIfThereIsOne =<< theDocument
    Ok <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)

apiCallSetAuthorAttachemnts  :: Kontrakcja m => DocumentID -> m Response
apiCallSetAuthorAttachemnts did = api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    unlessM (isPreparation <$> theDocument) $ do
          checkObjectVersionIfProvidedAndThrowError did (serverError "Document is not a draft or template")
    when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
    attachments <- getAttachments 0 =<< theDocument
    (documentauthorattachments <$> theDocument >>=) $ mapM_ $ \att -> dbUpdate $ RemoveDocumentAttachment (authorattachmentfile att) actor
    forM_ attachments $ \att -> dbUpdate $ AddDocumentAttachment att actor
    triggerAPICallbackIfThereIsOne =<< theDocument
    Ok <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)
     where
          getAttachments :: Kontrakcja m => Int -> Document -> m [FileID]
          getAttachments i doc = do
              mf <- tryGetFile doc i
              case mf of
                   Just f -> do
                              atts <- getAttachments (i+1) doc
                              if (f `elem` atts)
                                then return atts
                                else return $ f: atts
                   Nothing -> return []
          tryGetFile ::  Kontrakcja m => Document -> Int -> m  (Maybe FileID)
          tryGetFile doc i = do
              inp <- getDataFn' (lookInput $ "attachment_" ++ show i)
              case inp of
                   Just (Input (Left filepath) (Just filename) _contentType) -> do
                       content <- liftIO $ BSL.readFile filepath
                       cres <- liftIO $ preCheckPDF (concatChunks content)
                       case cres of
                         Left _ -> do
                           Log.debug $ "Document #" ++ show did ++ ". File for attachment " ++ show filepath ++ " is broken PDF. Skipping."
                           throwIO . SomeKontraException $ (badInput $ "AttachFile " ++ show i ++ " file is not a valid PDF")
                         Right content' -> do
                           fileid' <- dbUpdate $ NewFile filename content'
                           return (Just fileid')
                   Just (Input  (Right c)  _ _)  -> do
                        case maybeRead (BSL.toString c) of
                            Just fid -> do
                              access <- hasAccess doc fid
                              if access
                                then return (Just fid)
                                else throwIO . SomeKontraException $ (forbidden $ "Access to attachment " ++ show i ++ " forbiden")
                            Nothing -> throwIO . SomeKontraException $ (badInput $ "Can parse attachment id for attachment " ++ show i)
                   _ -> return Nothing

          hasAccess ::  Kontrakcja m => Document -> FileID -> m Bool
          hasAccess doc fid = do
            user <- fromJust <$> ctxmaybeuser <$> getContext
            if (fid `elem` (authorattachmentfile <$> documentauthorattachments doc))
             then return True
             else do
              atts <-  dbQuery $ GetAttachments [  AttachmentsSharedInUsersCompany (userid user)
                                                , AttachmentsOfAuthorDeleteValue (userid user) True
                                                , AttachmentsOfAuthorDeleteValue (userid user) False
                                               ]
                                              [ AttachmentFilterByFileID [fid]]
                                              []
                                              (0,1)
              return $ not $ Prelude.null atts

apiCallReady :: (MonadBaseControl IO m, Kontrakcja m) => DocumentID -> m Response
apiCallReady did =  api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) .getAuthorSigLink) <$> theDocument
    ifM ((isPending  &&^ not . any hasSigned . documentsignatorylinks) <$> theDocument)
     {-then-} (Accepted <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument))
     {-else-} $ do
      checkObjectVersionIfProvided did
      when (not $ (auid == userid user)) $ do
            throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
      unlessM (isPreparation <$> theDocument) $ do
            checkObjectVersionIfProvidedAndThrowError did $ (conflictError "Document is not a draft")
      whenM (isTemplate <$> theDocument) $ do
            checkObjectVersionIfProvidedAndThrowError did $ (serverError "Document is not a draft")
      timezone <- mkTimeZoneName =<< (fromMaybe "Europe/Stockholm" <$> getField "timezone")
      whenM ((\doc -> (not $ all ((/=EmailDelivery) . signatorylinkdeliverymethod ||^ isGood . asValidEmail . getEmail) (documentsignatorylinks doc))) <$> theDocument) $ do
            throwIO . SomeKontraException $ serverError "Some signatories don't have a valid email address set."
      whenM (isNothing . documentfile <$> theDocument) $ do
            throwIO . SomeKontraException $ serverError "File must be provided before document can be made ready."
      t <- ctxtime <$> getContext
      dbUpdate $ PreparationToPending actor timezone
      dbUpdate $ SetDocumentInviteTime t actor

      skipauthorinvitation <- isFieldSet "skipauthorinvitation"
      postDocumentPreparationChange skipauthorinvitation timezone
      Accepted <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)

apiCallCancel :: (MonadBaseControl IO m, Kontrakcja m) =>  DocumentID -> m Response
apiCallCancel did =  api $ do
    checkObjectVersionIfProvided did
    (user, actor, _) <- getAPIUser APIDocSend
    withDocumentID did $ do
      auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) .getAuthorSigLink) <$> theDocument
      when (not $ (auid == userid user)) $ do
            throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
      unlessM (isPending <$> theDocument) $ do
            throwIO . SomeKontraException $ (conflictError "Document is not pending")
      dbUpdate $ CancelDocument actor
      postDocumentCanceledChange =<< theDocument
      Accepted <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)


apiCallReject :: (MonadBaseControl IO m, Kontrakcja m) =>  DocumentID -> SignatoryLinkID -> m Response
apiCallReject did slid = api $ do
  checkObjectVersionIfProvided did
  (mh,mu) <- getMagicHashAndUserForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    ctx <- getContext
    Just sll <- getSigLinkFor slid <$> theDocument
    let  actor = signatoryActor ctx sll
    customtext <- getOptionalField  asValidInviteText "customtext"
    switchLang . getLang =<< theDocument
    (dbUpdate $ RejectDocument slid customtext actor)
        `catchKontra` (\(DocumentStatusShouldBe _ _ i) -> throwIO . SomeKontraException $ conflictError $ "Document not pending but " ++ show i)
        `catchKontra` (\(SignatoryHasAlreadySigned) -> throwIO . SomeKontraException $ conflictError $ "Signatory has already signed")
    postDocumentRejectedChange slid =<< theDocument
    Accepted <$> (documentJSON mu False True True Nothing Nothing =<< theDocument)


apiCallCheckSign :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m Response
apiCallCheckSign did slid = api $ do
  checkObjectVersionIfProvided did
  Log.debug $ "Checking if we can sign a document"
  (mh,_) <- getMagicHashAndUserForSignatoryAction did slid
  Log.debug "We have magic hash for this operation"
  fields <- getFieldForSigning
  mprovider <- readField "eleg"
  doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
  when (not $ isPending doc)
    (throwIO . SomeKontraException $ conflictError $ "Document not pending")
  when (isJust $ join $ maybesigninfo <$> getSigLinkFor slid doc)
    (throwIO . SomeKontraException $ conflictError $ "Document already signed")
  withDocument doc $ do
     case mprovider of
       Just provider -> do
                  transactionid <- getDataFnM $ look "transactionid"
                  esigninfo <- case provider of
                      MobileBankIDProvider -> BankID.verifySignatureAndGetSignInfoMobile did slid mh fields transactionid
                      _ -> do
                            signature <- getDataFnM $ look "signature"
                            BankID.verifySignatureAndGetSignInfo slid mh fields provider signature transactionid
                  case esigninfo of
                        BankID.Sign _ -> return $ Right $ Ok ()
                        BankID.Mismatch (onname,onnumber) _ -> do
                          (Left . Failed) <$> (runJSONGenT $ value "elegProblem" True >> value "mismatch" True >> value "onName" onname >> value "onNumber" onnumber)
                        _ -> do
                          (Left . Failed) <$> (runJSONGenT $ value "elegProblem" True)
       _ -> return $ Right $ Ok ()


apiCallSign :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m Response
apiCallSign  did slid = api $ do
  _ <- internalError
  checkObjectVersionIfProvided did
  Log.debug $ "Ready to sign a document " ++ show did ++ " for signatory " ++ show slid
  (mh,mu) <- getMagicHashAndUserForSignatoryAction did slid
  Log.debug "We have magic hash for this operation"
  screenshots <- fromMaybe emptySignatoryScreenshots <$> join <$> fmap fromJSValue <$> getFieldJSON "screenshots"
  fields <- getFieldForSigning
  mprovider <- readField "eleg"
  Log.debug $ "All parameters read and parsed"

  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
  withDocument olddoc $ do
    (case mprovider of
              Nothing -> do
                  signDocument slid mh fields Nothing screenshots
                  postDocumentPendingChange olddoc
                  handleAfterSigning slid
                  (Right . Accepted) <$> (documentJSON mu False True True Nothing Nothing =<< theDocument)
              Just provider -> do
                  transactionid <- getDataFnM $ look "transactionid"
                  esigninfo <- case provider of
                      MobileBankIDProvider -> BankID.verifySignatureAndGetSignInfoMobile did slid mh fields transactionid
                      _ -> do
                            signature <- getDataFnM $ look "signature"
                            BankID.verifySignatureAndGetSignInfo slid mh fields provider signature transactionid
                  case esigninfo of
                      BankID.Problem msg -> do
                        Log.error $ "Eleg verification for document #" ++ show did ++ " failed with message: " ++ msg
                        (Left . Failed) <$> (runJSONGenT $ return ())
                      BankID.Mismatch (onname,onnumber) (sfn,sln,spn) -> do
                        handleMismatch slid ((\(t,v) -> SignatoryField t v False False []) <$> fields) sfn sln spn
                        Log.error $ "Eleg verification for document #" ++ show did ++ " failed with mismatch " ++ show ((onname,onnumber),(sfn,sln,spn))
                        (Left . Failed) <$> (runJSONGenT $ value "mismatch" True >> value "onName" onname >> value "onNumber" onnumber)
                      BankID.Sign sinfo -> do
                        signDocument slid mh fields (Just sinfo) screenshots
                        postDocumentPendingChange olddoc
                        handleAfterSigning slid
                        (Right . Accepted) <$> (documentJSON mu False True True Nothing Nothing =<< theDocument)
     )
           `catchKontra` (\(DocumentStatusShouldBe _ _ i) -> throwIO . SomeKontraException $ conflictError $ "Document not pending but " ++ show i)
           `catchKontra` (\(SignatoryHasAlreadySigned) -> throwIO . SomeKontraException $ conflictError $ "Signatory has already signed")

{- | Utils for signing with eleg -}


signDocument :: (Kontrakcja m, DocumentMonad m)
             => SignatoryLinkID
             -> MagicHash
             -> [(FieldType, String)]
             -> Maybe SignatureInfo
             -> SignatoryScreenshots.SignatoryScreenshots
             -> m ()
signDocument slid mh fields msinfo screenshots = do
  switchLang =<< getLang <$> theDocument
  Just sl' <- getSigLinkFor slid <$> theDocument
  ctx <- getContext
  let actor = signatoryActor ctx sl'
  dbUpdate $ UpdateFieldsForSigning slid fields actor
  dbUpdate $ SignDocument slid mh msinfo screenshots actor

handleMismatch :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> [SignatoryField] -> String -> String -> String -> m ()
handleMismatch sid sf sfn sln spn = do
        ctx <- getContext
        Just sl <- getSigLinkFor sid <$> theDocument
        Log.eleg $ "Information from eleg did not match information stored for signatory in document."
        dbUpdate $ LogSignWithELegFailureForDocument sid (nothingIfEmpty $ getFullName sf) (nothingIfEmpty $ getPersonalNumber sf)  sfn sln spn (signatoryActor ctx sl)
        triggerAPICallbackIfThereIsOne =<< theDocument

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
    Accepted <$> documentJSON (Just $ user) False True True Nothing Nothing newdocument

apiCallProlong :: (MonadBaseControl IO m, Kontrakcja m) =>  DocumentID -> m Response
apiCallProlong did =  api $ do
    checkObjectVersionIfProvided did
    (user, actor, _) <- getAPIUser APIDocSend
    withDocumentID did $ do
      auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
      when (not $ (auid == userid user)) $ do
            throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
      unlessM (isTimedout <$> theDocument) $ do
            throwIO . SomeKontraException $ (conflictError "Document is not timedout")
      mdays <- getDefaultedField 1 asValidNumber "days"
      days <- case mdays of
           Nothing -> throwIO . SomeKontraException $ (badInput "Number of days to sign must be a valid number, between 1 and 90")
           Just n -> if (n < 1 || n > 90)
                              then throwIO . SomeKontraException $ (badInput "Number of days to sign must be a valid number, between 1 and 90")
                              else return n
      timezone <- mkTimeZoneName =<< (fromMaybe "Europe/Stockholm" <$> getField "timezone")
      dbUpdate $ ProlongDocument days (Just timezone) actor
      triggerAPICallbackIfThereIsOne =<< theDocument
      Accepted <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)


apiCallSetAutoReminder :: (MonadBaseControl IO m, Kontrakcja m) =>  DocumentID -> m Response
apiCallSetAutoReminder did =  api $ do
    ctx <- getContext
    checkObjectVersionIfProvided did
    (user, _actor, _) <- getAPIUser APIDocSend
    withDocumentID did $ do
      auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
      when (not $ (auid == userid user)) $ do
            throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
      unlessM (isPending <$> theDocument) $ do
            throwIO . SomeKontraException $ (conflictError "Document is not pending")
      mdays <- getOptionalField asValidNumber "days"
      days <- case mdays of
           Nothing -> return Nothing
           Just n -> do tot <- documenttimeouttime <$> theDocument
                        if n < 1 || (isJust tot && n `daysAfter` (ctxtime ctx) > fromJust tot)
                          then throwIO . SomeKontraException $ (badInput "Number of days to send autoreminder must be a valid number, between 1 and number of days left till document deadline")
                          else return $ Just n
      timezone <- mkTimeZoneName =<< (fromMaybe "Europe/Stockholm" <$> getField "timezone")
      setAutoreminder did days timezone
      triggerAPICallbackIfThereIsOne =<< theDocument
      Accepted <$> (documentJSON (Just $ user) False True True Nothing Nothing =<< theDocument)




apiCallRemind :: Kontrakcja m => DocumentID -> m Response
apiCallRemind did =  api $ do
  (user, actor , _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    unlessM (isPending <$> theDocument) $ do
          throwIO . SomeKontraException $ serverError "Can't send reminder for documents that are not pending"
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
    _ <- sendAllReminderEmailsExceptAuthor actor False
    Accepted <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)

apiCallDelete :: Kontrakcja m => DocumentID -> m Response
apiCallDelete did =  api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    mauser <- theDocument >>= \d -> case join $ maybesignatory <$> getAuthorSigLink d of
                         Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                         _ -> return Nothing
    msl <- getSigLinkFor user <$> theDocument
    let haspermission = (isJust msl)
                     || (isJust mauser && usercompany (fromJust mauser) == usercompany user && (useriscompanyadmin user))
    when (not haspermission) $ do
           throwIO . SomeKontraException $ serverError "Permission problem. Not connected to document."
    dbUpdate $ ArchiveDocument (userid user) actor

    Accepted <$> (runJSONGenT $ return ())




-- TODO test case to make sure apiCallGet does not update document version (MarkDocumentSeen case)
apiCallGet :: Kontrakcja m => DocumentID -> m Response
apiCallGet did = api $ do
  ctx <- getContext
  (msignatorylink :: Maybe SignatoryLinkID) <- readField "signatoryid"
  mmagichashh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  includeEvidenceAttachments <- (=="true") <$> getField' "evidenceAttachments"
  withDocumentID did $ case (msignatorylink,mmagichashh) of
    (Just slid,Just mh) -> do
       sl <- apiGuardJustM  (serverError "No document found") $ getSigLinkFor slid <$> theDocument
       when (signatorymagichash sl /= mh) $ throwIO . SomeKontraException $ serverError "No document found"
       unlessM ((isTemplate ||^ isPreparation ||^ isClosed) <$> theDocument) $
         dbUpdate $ MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl)
                       (signatoryActor ctx sl)
       switchLang . getLang =<< theDocument

       mauser <- theDocument >>= \d -> case join $ maybesignatory <$> getAuthorSigLink d of
                     Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                     _ -> return Nothing
       pq <- case (mauser) of
              Just u -> dbQuery $ GetPadQueue $ (userid u)
              _ -> return Nothing

       Ok <$> (documentJSON Nothing includeEvidenceAttachments False False pq (Just sl) =<< theDocument)
    _ -> do
      (user, _actor, external) <- getAPIUser APIDocCheck
      msiglink <- getSigLinkFor user <$> theDocument
      unlessM (((const (isNothing msiglink)) ||^ isPreparation ||^ isClosed  ||^ isTemplate) <$> theDocument) $ do
          let sl = fromJust msiglink
          dbUpdate $ MarkDocumentSeen (signatorylinkid sl) (signatorymagichash sl)
               (signatoryActor ctx sl)

      mauser <- theDocument >>= \d -> case (join $ maybesignatory <$> getAuthorSigLink d) of
                     Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                     _ -> return Nothing
      pq <- case (mauser) of
              Just u -> dbQuery $ GetPadQueue $ (userid u)
              _ -> return Nothing

      haspermission <- theDocument >>= \d -> return $
                          isJust msiglink
                       || (isJust mauser && usercompany (fromJust mauser) == usercompany user && (useriscompanyadmin user || isDocumentShared d))
      if (haspermission)
        then do
          Ok <$> (documentJSON (Just user) includeEvidenceAttachments external ((userid <$> mauser) == (Just $ userid user)) pq msiglink =<< theDocument)
        else throwIO . SomeKontraException $ serverError "You do not have right to access document"

apiCallList :: Kontrakcja m => m Response
apiCallList = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  ctx <- getContext
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
  res <- jsonDocumentsList
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
  return res


apiCallCheckAvailable :: Kontrakcja m => m Response
apiCallCheckAvailable = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  (mids :: Maybe [DocumentID]) <- readField "ids"
  when (isNothing mids) $ do
    throwIO . SomeKontraException $ serverError "No ids parameter was provided or it had wrong format"
  let ids = fromJust mids
  when (length ids > 10000) $ do
    throwIO . SomeKontraException $ serverError "This request can't check more then 10000 documents"
  docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser $ userid user] [DocumentFilterDeleted False,DocumentFilterByDocumentIDs ids] [] (0,-1)
  Ok <$> (runJSONGenT $ value "ids" (show . documentid <$> docs))



apiCallHistory :: Kontrakcja m => DocumentID -> m Response
apiCallHistory did = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  ctx <- getContext
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
  mlang <- (join . (fmap langFromCode)) <$> getField "lang"
  switchLang $ fromMaybe (lang $ usersettings user) mlang
  res <- jsonDocumentEvidenceLog did
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
  return res


-- | This handler downloads main file of document. This means sealed file for Closed documents.
--   or one with preprinted fields if not closed

apiCallDownloadMainFile :: Kontrakcja m => DocumentID -> String -> m Response
apiCallDownloadMainFile did _nameForBrowser = api $ do

  (msid :: Maybe SignatoryLinkID) <- readField "signatorylinkid"
  mmh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msid

  doc <- do
           case (msid, mmh) of
            (Just sid, Just mh) -> dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh
            _ ->  do
                  (user, _actor, external) <- getAPIUser APIDocCheck
                  if (external)
                    then do
                      ctx <- getContext
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
                      res <- getDocByDocID did
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
                      return res;
                    else getDocByDocID did

  content <- case documentstatus doc of
                Closed -> do
                  when (documentsealstatus doc == Just Missing) $ do
                    now <- getMinutesTime
                    -- Give Guardtime signing a few seconds to complete before we respond
                    when (toSeconds now - toSeconds (documentmtime doc) < 8) $ do
                      Log.debug $ "Waiting for Guardtime signing, document was modified " ++ show (toSeconds now - toSeconds (getLastSignedTime doc)) ++ " seconds ago"
                      throwIO $ SomeKontraException $ noAvailableYet "Digitally sealed document not ready"
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
apiCallDownloadFile did fileid nameForBrowser = api $ do
  (msid :: Maybe SignatoryLinkID) <- readField "signatorylinkid"
  mmh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msid
  doc <- do
           case (msid, mmh) of
            (Just sid, Just mh) -> dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh
            _ ->  do
                  (user, _actor, external) <- getAPIUser APIDocCheck
                  if (external)
                    then do
                      ctx <- getContext
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
                      res <- getDocByDocID did
                      modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
                      return res;
                    else getDocByDocID did
  let allfiles = maybeToList (documentfile doc) ++ maybeToList (documentsealedfile doc) ++
                      (authorattachmentfile <$> documentauthorattachments doc) ++
                      (catMaybes $ Prelude.map signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc)
  if (all (/= fileid) allfiles)
     then throwIO . SomeKontraException $ forbidden "Access to file is forbiden."
     else do
        content <- getFileIDContents fileid
        let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [content]) Nothing
            ct = if (".pdf" `isSuffixOf` (map toLower nameForBrowser))
                    then  "application/pdf"
                    else if (".png" `isSuffixOf` (map toLower nameForBrowser))
                      then "image/png"
                      else if (".jpg" `isSuffixOf` (map toLower nameForBrowser))
                        then "image/jpeg"
                        else "application/octet-stream"
            res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString ct) res
        return res2


-- this one must be standard post with post params because it needs to
-- be posted from a browser form
-- Change main file, file stored in input "file" OR templateid stored in "template"
apiCallChangeMainFile :: Kontrakcja m => DocumentID -> m Response
apiCallChangeMainFile docid = api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  checkObjectVersionIfProvided docid
  withDocumentID docid $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    unlessM (isPreparation <$> theDocument) $ do
      throwIO . SomeKontraException $ (serverError "Document is not a draft or template")
    when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."

    fileinput <- getDataFn' (lookInput "file")

    mft <- case fileinput of
      Nothing -> return Nothing
      Just (Input _ Nothing _) -> throwIO . SomeKontraException $ badInput "Missing file"
      Just (Input contentspec (Just filename') _contentType) -> do
        let filename = takeBaseName filename' ++ ".pdf"
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
        return $ Just (fileid', takeBaseName filename)

    case mft of
      Just  (fileid,filename) -> do
        dbUpdate $ AttachFile fileid actor
        apiGuardL' $ dbUpdate $ SetDocumentTitle filename actor
      Nothing -> dbUpdate $ DetachFile actor
    Accepted <$> (documentJSON (Just user) False True True Nothing Nothing =<< theDocument)

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

-- Signatory Attachments handling
apiCallSetSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> String -> m Response
apiCallSetSignatoryAttachment did sid aname = api $ do
  checkObjectVersionIfProvided did
  Log.debug $ "Setting signatory attachments" ++ show did ++ " for signatory " ++ show sid ++ " name " ++ aname
  (mh,mu) <- getMagicHashAndUserForSignatoryAction did sid
  Log.debug "We are authorized to set signatory attachment"
  -- We check permission here - because we are able to get a valid magichash here
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh) `withDocumentM` do
    unlessM (isPending <$> theDocument) $ do
            throwIO . SomeKontraException $ (badInput "Document is not pending")
    sl  <- apiGuard (badInput "There is no signatory by that id.") =<< getSigLinkFor sid <$> theDocument
    sigattach <- apiGuard (badInput "The attachment with that name does not exist for the signatory.") =<< getSignatoryAttachment sid aname <$> theDocument
    filedata <- getDataFn' (lookInput "file")
    mfileid <- case filedata of
      (Just (Input contentspec (Just filename) _contentType)) ->  Just <$>  do
                content1 <- case contentspec of
                  Left filepath -> liftIO $ BSL.readFile filepath
                  Right content -> return content
                content <- if (".pdf" `isSuffixOf` (map toLower filename))
                  then apiGuardL (badInput "The PDF was invalid.") $ liftIO $ preCheckPDF (concatChunks content1)
                  else if (".png" `isSuffixOf` (map toLower filename) || ".jpg" `isSuffixOf` (map toLower filename))
                    then return $ Binary $ concatChunks content1
                    else throwIO . SomeKontraException $ badInput "Only pdf files or images can be attached."
                (dbUpdate $ NewFile (dropFilePathFromWindows filename) content)
      _ -> return Nothing
    ctx <- getContext
    case mfileid of
      Just fileid -> (dbUpdate $ SaveSigAttachment sid sigattach fileid (signatoryActor ctx sl))
                       `catchKontra` (\(DBBaseLineConditionIsFalse _) -> throwIO . SomeKontraException $ conflictError $ "Inconsistent state - attachment is already set")
      Nothing -> dbUpdate $ DeleteSigAttachment sid sigattach (signatoryActor ctx sl)

    Accepted <$> (documentJSON mu False True False Nothing (Just sl) =<< theDocument)

checkObjectVersionIfProvided ::  (Kontrakcja m) => DocumentID -> m ()
checkObjectVersionIfProvided did = do
    mov <- readField "objectversion"
    case mov of
        Just ov -> dbQuery $ CheckDocumentObjectVersionIs did ov
        Nothing -> return ()
  `catchKontra` (\DocumentObjectVersionDoesNotMatch -> throwIO . SomeKontraException $ conflictError $ "Document object version does not match")

checkObjectVersionIfProvidedAndThrowError ::  (Kontrakcja m) => DocumentID -> APIError -> m ()
checkObjectVersionIfProvidedAndThrowError did err = do
    mov <- readField "objectversion"
    case mov of
        Just ov -> (dbQuery $ CheckDocumentObjectVersionIs did ov)
                      `catchKontra` (\DocumentObjectVersionDoesNotMatch -> throwIO . SomeKontraException $ conflictError $ "Document object version does not match")
        Nothing -> return ()
    throwIO . SomeKontraException $ err


-- Utils

getMagicHashAndUserForSignatoryAction :: (Kontrakcja m) =>  DocumentID -> SignatoryLinkID -> m (MagicHash,Maybe User)
getMagicHashAndUserForSignatoryAction did sid = do
    mh' <- dbQuery $ GetDocumentSessionToken sid
    case mh' of
      Just mh'' ->  return (mh'',Nothing)
      Nothing -> do
         (user, _ , _) <- getAPIUser APIPersonal
         Log.debug $ "User is " ++ show user
         mh'' <- getMagicHashForDocumentSignatoryWithUser  did sid user
         case mh'' of
           Nothing -> throwIO . SomeKontraException $ serverError "Can't perform this action. Not authorized."
           Just mh''' -> return (mh''',Just $ user)


getFieldForSigning ::(Kontrakcja m) => m [(FieldType,String)]
getFieldForSigning = do
      eFieldsJSON <- getFieldJSON "fields"
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