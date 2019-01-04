module Doc.API.V1.Calls (
    documentAPIV1
  , apiCallV1CreateFromFile      -- Exported for tests
  , apiCallV1CreateFromTemplate  -- Exported for tests
  , apiCallV1Get                 -- Exported for tests
  , apiCallV1GetEvidenceAttachments -- Exported for tests
  , apiCallV1List                -- Exported for tests
  , apiCallV1Update              -- Exported for tests
  , apiCallV1Ready               -- Exported for tests
  , apiCallV1Sign                -- Exported for tests
  , apiCallV1Cancel              -- Exported for tests
  , apiCallV1Reject              -- Exported for tests
  , apiCallV1SetAutoReminder     -- Exported for tests
  , apiCallV1DownloadMainFile    -- Exported for tests
  , apiCallV1DownloadFile        -- Exported for tests
  , apiCallV1ChangeMainFile
  , apiCallV1ChangeAuthenticationToView    -- Exported for tests
  , apiCallV1ChangeAuthenticationToSign    -- Exported for tests
  , apiCallV1SetAuthorAttachemnts -- Exported for tests
  ) where

import Control.Conditional ((<|), (|>), ifM, unlessM, whenM)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson (Value(..))
import Data.Char
import Data.Either.Combinators (rightToMaybe)
import Data.Int
import Data.String.Utils (replace, strip)
import Data.Text (unpack)
import Data.Time
import Happstack.Server.RqData
import Happstack.Server.Types
import Happstack.StaticRouting
import Log
import System.FilePath.Posix (takeBaseName)
import Text.JSON hiding (Ok)
import Text.JSON.FromJSValue
import Text.JSON.String (runGetJSON)
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.HashMap.Strict as Hash
import qualified Data.Map as Map hiding (map)
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified Data.Vector as Vec
import qualified Text.JSON as J
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F

import API.Monad.V1
import AppView (respondWithPDF)
import Attachment.Model
import Chargeable.Model
import DB
import DB.TimeZoneName (defaultTimeZoneName, mkTimeZoneName)
import Doc.Action
import Doc.Anchors
import Doc.API.Callback.Model
import Doc.API.V1.DocumentFromJSON (AuthorAttachmentDetails(..))
import Doc.API.V1.DocumentToJSON
import Doc.API.V1.DocumentUpdateUtils
import Doc.API.V1.ListUtil
import Doc.AutomaticReminder.Model
import Doc.Conditions
import Doc.DocControl
import Doc.DocInfo
import Doc.DocMails
import Doc.DocSeal as DocSeal
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad (DocumentMonad, theDocument, withDocument, withDocumentID, withDocumentM)
import Doc.DocUtils
import Doc.Logging
import Doc.Model
import Doc.Model.OrderBy
import Doc.SealStatus (SealStatus(..))
import Doc.SignatoryLinkID
import Doc.SignatoryScreenshots (SignatoryScreenshots, emptySignatoryScreenshots, resolveReferenceScreenshotNames)
import Doc.SMSPin.Model
import Doc.Texts
import Doc.Tokens.Model
import Doc.Types.SignatoryLink (isValidSignatoryMagicHash)
import EID.Signature.Model
import EvidenceLog.Model
import EvidenceLog.View
import File.File
import File.Model
import File.Storage
import Happstack.Fields
import InputValidation
import Kontra
import Log.Identifier
import MagicHash (MagicHash)
import MinutesTime
import OAuth.Model
import Routing
import Text.JSON.Convert
import User.Model
import Util.Actor
import Util.CSVUtil
import Util.HasSomeUserInfo
import Util.PDFUtil
import Util.SignatoryLinkUtils
import Utils.Monad
import qualified Data.ByteString.RFC2397 as RFC2397

documentAPIV1 ::  Route (Kontra Response)
documentAPIV1  = choice [

  dir "createfromfile"     $ hPost $ toK0 $ apiCallV1CreateFromFile,
  dir "createfromtemplate" $ hPostAllowHttp $ toK1 $ apiCallV1CreateFromTemplate,
  dir "clone"              $ hPost $ toK1   $  apiCallV1Clone,
  dir "update"             $ hPost $ toK1 $ apiCallV1Update,
  dir "setattachments"     $ hPost $ toK1 $ apiCallV1SetAuthorAttachemnts,
  dir "ready"              $ hPostAllowHttp $ toK1 $ apiCallV1Ready,
  dir "cancel"             $ hPost $ toK1 $ apiCallV1Cancel,
  dir "reject"             $ hPost $ toK2 $ apiCallV1Reject,
  dir "checksign"          $ hPost $ toK2 $ apiCallV1CheckSign,
  dir "sign"               $ hPost $ toK2 $ apiCallV1Sign,

  dir "sendsmspin"            $ hPost $ toK2 $ apiCallV1SendSMSPinCode,

  dir "restart"              $ hPost $ toK1 $ apiCallV1Restart,
  dir "prolong"              $ hPost $ toK1 $ apiCallV1Prolong,
  dir "setautoreminder"      $ hPost $ toK1 $ apiCallV1SetAutoReminder,
  dir "changeauthentication" $ hPost $ toK2 $ apiCallV1ChangeAuthenticationToSign,
  dir "changeauthenticationtoview" $ hPost $ toK2 $ apiCallV1ChangeAuthenticationToView,


  dir "remind"             $ hPost $ toK1 $ apiCallV1Remind,
  dir "forward"            $ hPost $ toK1 $ apiCallV1Forward,
  dir "delete"             $ hDeleteAllowHttp  $ toK1 $ apiCallV1Delete,
  dir "reallydelete"       $ hDeleteAllowHttp  $ toK1 $ apiCallV1ReallyDelete,
  dir "get"                $ hGetAllowHttp $ toK1 $ apiCallV1Get,
  dir "getevidenceattachments" $ hGet $ toK1 $ apiCallV1GetEvidenceAttachments,

  dir "list"               $ hGetAllowHttp $ apiCallV1List,
  dir "checkavailable"     $ hPostAllowHttp $ apiCallV1CheckAvailable,
  dir "triggercallback"    $ hPost $ apiCallV1TriggerCallback,

  dir "history"           $ hGetAllowHttp $ apiCallV1History,
  dir "downloadmainfile"   $ hGetAllowHttp  $ toK2 $ apiCallV1DownloadMainFile,
  dir "downloadfile"       $ hGetAllowHttp  $ toK3 $ apiCallV1DownloadFile,
  dir "extracttexts"       $ hGetAllowHttp  $ toK2 $ apiCallV1ExtractTexts,

  dir "changemainfile"     $ hPost $ toK1 $ apiCallV1ChangeMainFile,

  dir "setsignatoryattachment"    $ hPost $ toK3 $ apiCallV1SetSignatoryAttachment
  ]

-- | Windows Explorer set the full path of a file, for example:
--
--    c:\My Documents\Things\Untitle Document.doc
--
-- We drop all up to and including last backslash here.
dropFilePathFromWindows :: FilePath -> FilePath
dropFilePathFromWindows = reverse . takeWhile (/='\\') . reverse

{- New API calls-}
apiCallV1CreateFromFile :: Kontrakcja m => m Response
apiCallV1CreateFromFile = api $ do
  ctx <- getContext
  (user, actor, external) <- getAPIUser APIDocCreate
  isTpl <- isFieldSet "template"
  let doctype = (Template <| isTpl |> Signable)
  minput <- getDataFn' (lookInput "file")
  (mfile, title) <- case minput of
    Nothing -> do
      title <- renderTemplate_ ("newDocumentTitle" <| not isTpl |> "newTemplateTitle")
      return (Nothing,  replace "  " " " $ title ++ " " ++ formatTimeSimple (get ctxtime ctx))
    Just (Input _ Nothing _) -> throwM . SomeDBExtraException $ badInput "Missing file"
    Just (Input contentspec (Just filename') _contentType) -> do
      let filename = dropFilePathFromWindows filename'
      content1' <- case contentspec of
        Left filepath -> liftIO $ BS.readFile filepath
        Right content -> return (BS.concat $ BSL.toChunks content)

      -- This is some kind of Salesforce hack that was supposed to be
      -- dropped with Happstack 7.0.4. It seems to be used till now
      -- for example by Avis.
      let content' = either (const content1') id (B64.decode content1')

      pdfcontent <- apiGuardL (badInput "The PDF is invalid.") $ preCheckPDF content'
      fileid' <- saveNewFile filename pdfcontent
      return (Just fileid', takeBaseName filename)
  mtimezone <- getField "timezone"
  timezone <- fromMaybe defaultTimeZoneName <$> T.sequence (mkTimeZoneName <$> mtimezone)
  (dbUpdate $ NewDocument user title doctype timezone 0 actor) `withDocumentM` do
    when_ (not $ external) $ dbUpdate $ SetDocumentUnsavedDraft True
    case mfile of
      Nothing -> return ()
      Just fileid' -> do
        dbUpdate $ AttachFile fileid' actor
    theDocument >>= \doc -> do
      logInfo "New document created" $ logObject_ doc
      Created <$> (documentJSONV1 (Just user) True True Nothing doc)

apiCallV1CreateFromTemplate :: Kontrakcja m => DocumentID -> m Response
apiCallV1CreateFromTemplate did = logDocument did . api $ do
  (user, actor, external) <- getAPIUser APIDocCreate
  template <- dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink template
  auser <- apiGuardJustM (serverError "No user found") $ dbQuery $ GetUserByIDIncludeDeleted auid
  let haspermission = (userid auser == userid user) ||
                      (usergroupid auser == usergroupid user &&  isDocumentShared template)
  unless (isTemplate template && haspermission) $ do
    throwM $ SomeDBExtraException $ serverError "Id did not matched template or you do not have right to access document"
  when (documentDeletedForUser template $ userid user) $
    throwM $ SomeDBExtraException $ serverError "Template is deleted"
  (apiGuardJustM (serverError "Can't clone given document") (dbUpdate $ CloneDocumentWithUpdatedAuthor (Just user) template actor id) >>=) $ flip withDocumentID $ do
    dbUpdate $ DocumentFromTemplate (documentid template) actor
    when_ (not $ external) $ dbUpdate $ SetDocumentUnsavedDraft True
    newDoc <- theDocument
    logInfo "New document created from template" $ object [
        logPair ("new_"<>)      newDoc
      , logPair ("template_"<>) template
      ]
    Created <$> documentJSONV1 (Just user) True True Nothing newDoc

apiCallV1Clone :: Kontrakcja m => DocumentID -> m Response
apiCallV1Clone did = logDocument did . api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  doc <- dbQuery $ GetDocumentByDocumentID $ did
  if isAuthor (doc,user)
     then do
         mndid <- dbUpdate $ CloneDocumentWithUpdatedAuthor (Just user) doc actor id
         when (isNothing mndid) $
             throwM . SomeDBExtraException $ serverError "Can't clone given document"
         newdoc <- dbQuery $ GetDocumentByDocumentID $ fromJust mndid
         Created <$> documentJSONV1 (Just $ user) True  True Nothing newdoc
     else throwM . SomeDBExtraException $ serverError "Id did not matched template or you do not have right to access document"

apiCallV1Update :: Kontrakcja m => DocumentID -> m Response
apiCallV1Update did = logDocument did . api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    unlessM (isPreparation <$> theDocument) $ do
          checkObjectVersionIfProvidedAndThrowError did (serverError "Document is not a draft or template")
    when (not $ (auid == userid user)) $ do
          throwM . SomeDBExtraException $ serverError "Permission problem. Not an author."
    jsons <- apiGuardL (badInput "The MIME part 'json' must exist and must be a JSON.") $ getDataFn' (look "json")
    json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $ case decode jsons of
                                                                                 J.Ok js -> Just js
                                                                                 _ -> Nothing
    logInfo "Document updated" $ trimDocumentJSON $ jsonToAeson json
    draftData   <- apiGuardJustM (badInput "Given JSON does not represent valid draft data.") $ flip fromJSValueWithUpdate json . Just <$> theDocument
    whenM (draftIsChangingDocument draftData <$> theDocument) $ do
      checkObjectVersionIfProvided did -- If we will change document, then we want to be sure that object version is ok.
    applyDraftDataToDocument draftData actor
    Ok <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)
  where trimDocumentJSON (Object o) = Object $ Hash.adjust trimSignatoriesJSON "signatories" o
        trimDocumentJSON x = x

        trimSignatoriesJSON (Array a) = Array $ (Vec.map trimSignatoryJSON) a
        trimSignatoriesJSON x = x

        trimSignatoryJSON (Object o) = Object $ Hash.adjust trimCSVJSON "csv" o
        trimSignatoryJSON x = x

        trimCSVJSON Null = Null
        trimCSVJSON _ = String $ "(Omitted for logs)"

apiCallV1SetAuthorAttachemnts  :: Kontrakcja m => DocumentID -> m Response
apiCallV1SetAuthorAttachemnts did = logDocument did . api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    unlessM (isPreparation <$> theDocument) $ do
          checkObjectVersionIfProvidedAndThrowError did (serverError "Document is not a draft or template")
    when (not $ (auid == userid user)) $ do
          throwM . SomeDBExtraException $ serverError "Permission problem. Not an author."
    attachmentFilesWithDetails <- precheckNewAttachments =<< getAttachments 0 =<< theDocument
    (documentauthorattachments <$> theDocument >>=) $ mapM_ $ \att -> dbUpdate $ RemoveDocumentAttachments (authorattachmentfileid att) actor
    forM_ attachmentFilesWithDetails $ \(attfile, maad) -> do
      dbUpdate $ AddDocumentAttachment (fromMaybe (T.pack $ filename attfile) (aadName <$> maad))  (fromMaybe False (aadRequired <$> maad)) (fromMaybe True (aadAddToSealedFile <$> maad)) (fileid attfile) actor
    Ok <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)
     where
          getAttachments :: Kontrakcja m => Int -> Document -> m [(Either File (String, BSL.ByteString), Maybe AuthorAttachmentDetails)]
          getAttachments i doc = do
              mf <- tryGetFile doc i
              case mf of
                   Just f -> do
                              atts <- getAttachments (i+1) doc
                              if (f `elem` (map fst atts))
                                then return atts
                                else do
                                  maad <- tryGetAttachmentDetails i
                                  return $ (f,maad): atts
                   Nothing -> return []

          tryGetAttachmentDetails ::  Kontrakcja m => Int -> m (Maybe AuthorAttachmentDetails)
          tryGetAttachmentDetails i = getField ("attachment_details_" ++ show i) >>= \case
            Nothing -> return Nothing
            Just s ->  case decode s of
              J.Ok js -> case (fromJSValue js) of
                 Just aad -> return aad
                 _ -> throwM . SomeDBExtraException $ (badInput $ "Details for author attachment " ++ show i ++ " are invalid")
              _ -> throwM . SomeDBExtraException $ (badInput $ "Details for author attachment " ++ show i ++ " is not a valid JSON")

          precheckNewAttachments :: Kontrakcja m
                                 => [(Either File (String, BSL.ByteString), Maybe AuthorAttachmentDetails)]
                                 -> m [(File, Maybe AuthorAttachmentDetails)]
          precheckNewAttachments xs = do
            -- We will extract all new files (name, bytestring) and preCheck them all at once, because
            -- it's faster. Then we will put them back into the list.
            let (filenames, blobs) = unzip . mapMaybe (rightToMaybe . fst) $ xs
            cres <- preCheckPDFs $ map BSL.toStrict blobs
            newFiles <- case cres of
              Left _ -> throwM . SomeDBExtraException $ (badInput $ "One of Attached files is not a valid PDF")
              Right contents -> forM (zip filenames contents) $ \(filename, content) -> do
                fid <- saveNewFile filename content
                dbQuery $ GetFileByFileID fid
            let putNewFiles ((Left  f, mdetails) : rest) newfs      = (f , mdetails) : putNewFiles rest newfs
                putNewFiles ((Right _, mdetails) : rest) (nf:newfs) = (nf, mdetails) : putNewFiles rest newfs
                putNewFiles []                           []         = []
                putNewFiles _                            _          = unexpectedError "Wrong amount of preChecked new files"
            return $ putNewFiles xs newFiles

          tryGetFile ::  Kontrakcja m => Document -> Int -> m (Maybe (Either File (String, BSL.ByteString)))
          tryGetFile doc i = do
              inp <- getDataFn' (lookInput $ "attachment_" ++ show i)
              case inp of
                   Just (Input (Left filepath) (Just filename) _contentType) -> do
                       content <- liftIO $ BSL.readFile filepath
                       return $ Just $ Right (filename, content)
                   Just (Input  (Right c)  _ _)  -> do
                        case maybeRead (BSL.toString c) of
                            Just fid -> do
                              access <- hasAccess doc fid
                              if access
                                then Just <$> Left <$> dbQuery (GetFileByFileID fid)
                                else throwM . SomeDBExtraException $ (forbidden $ "Access to attachment " ++ show i ++ " forbiden")
                            Nothing -> throwM . SomeDBExtraException $ (badInput $ "Can parse attachment id for attachment " ++ show i)
                   _ -> return Nothing

          hasAccess ::  Kontrakcja m => Document -> FileID -> m Bool
          hasAccess doc fid = do
            user <- fromJust <$> get ctxmaybeuser <$> getContext
            if (fid `elem` (authorattachmentfileid <$> documentauthorattachments doc))
             then return True
             else do
              atts <-  dbQuery $ GetAttachments [  AttachmentsSharedInUsersUserGroup (userid user)
                                                , AttachmentsOfAuthorDeleteValue (userid user) True
                                                , AttachmentsOfAuthorDeleteValue (userid user) False
                                               ]
                                              [ AttachmentFilterByFileID fid]
                                              []
              return $ not $ null atts

apiCallV1Ready :: Kontrakcja m => DocumentID -> m Response
apiCallV1Ready did = logDocument did . api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) .getAuthorSigLink) <$> theDocument
    ifM ((isPending &&
          all (isSignatoryAndHasNotSigned || isApproverAndHasNotApproved) .
          documentsignatorylinks) <$>
          theDocument)
     {-then-} (Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument))
     {-else-} $ do
      checkObjectVersionIfProvided did
      when (not $ (auid == userid user)) $ do
            throwM . SomeDBExtraException $ serverError "Permission problem. Not an author."
      unlessM (isPreparation <$> theDocument) $ do
            checkObjectVersionIfProvidedAndThrowError did $ (conflictError "Document is not a draft")
      whenM (isTemplate <$> theDocument) $ do
            checkObjectVersionIfProvidedAndThrowError did $ (serverError "Document is not a draft")
      unlessM (((all signatoryHasValidDeliverySettings) . documentsignatorylinks) <$> theDocument) $ do
            throwM . SomeDBExtraException $ serverError "Some signatories have invalid email address or phone number, and it is required for invitation delivery."

      -- Clear invalid emails/mobile numbers if confirmation delivery requires it. v1 hack, properly fixed with v2
      -- Check: https://scrive.fogbugz.com/f/cases/2637/Fwd-FW-RA-email-not-sent
      mapM_ fixFieldsForConfirmationDeliveryIfNeeded =<< (documentsignatorylinks <$> theDocument)

      unlessM (((all signatoryHasValidConfirmationSettings) . documentsignatorylinks) <$> theDocument) $ do
            throwM . SomeDBExtraException $ serverError "Some signatories have invalid email address or phone number, and it is required for confirmation delivery."
      unlessM (((all signatoryHasValidAuthSettings) . documentsignatorylinks) <$> theDocument) $ do
            throwM . SomeDBExtraException $ serverError "Some signatories have invalid personal number, and it is required for authentication."
      unlessM (((all signatoryHasValidSSNForIdentifyToView) . documentsignatorylinks) <$> theDocument) $ do
            throwM . SomeDBExtraException $ serverError "Some signatories have invalid personal number and it is required for identification to view document."
      unlessM (((all signatoryHasValidPhoneForIdentifyToView) . documentsignatorylinks) <$> theDocument) $ do
            throwM . SomeDBExtraException $ serverError "Some signatories have invalid phone number and it is required for identification to view document."
      whenM (isNothing . documentfile <$> theDocument) $ do
            throwM . SomeDBExtraException $ serverError "File must be provided before document can be made ready."
      t <- get ctxtime <$> getContext
      timezone <- documenttimezonename <$> theDocument
      dbUpdate $ PreparationToPending actor timezone
      dbUpdate $ SetDocumentInviteTime t actor
      authorsignsimmediately <- isFieldSet "authorsignsimmediately"
      postDocumentPreparationChange authorsignsimmediately timezone
      dbUpdate $ ChargeUserGroupForStartingDocument did
      Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)

  where
    signatoryHasValidDeliverySettings sl = case (signatorylinkdeliverymethod sl) of
      EmailDelivery  ->  isGood $ asValidEmail $ getEmail sl
      MobileDelivery ->  isGood $ asValidPhoneForSMS $ getMobile sl
      EmailAndMobileDelivery -> (isGood $ asValidPhoneForSMS $ getMobile sl) && (isGood $ asValidEmail $ getEmail sl)
      _ -> True

    signatoryHasValidConfirmationSettings sl = case signatorylinkconfirmationdeliverymethod sl of
      EmailConfirmationDelivery -> checkEmailForConfirmation sl
      MobileConfirmationDelivery -> checkMobileForConfirmation sl
      EmailAndMobileConfirmationDelivery ->
        checkEmailAndMobileForConfirmation sl
      NoConfirmationDelivery -> True
      EmailLinkConfirmationDelivery -> checkEmailForConfirmation sl
      EmailLinkAndMobileConfirmationDelivery ->
        checkEmailAndMobileForConfirmation sl

    fixFieldsForConfirmationDeliveryIfNeeded sl = case (signatorylinkconfirmationdeliverymethod sl) of
      EmailConfirmationDelivery -> do
        unless (checkEmailForConfirmation sl) $ do
          dbUpdate $ ClearSignatoryEmail $ signatorylinkid sl
      MobileConfirmationDelivery -> do
        unless (checkMobileForConfirmation sl) $ do
          dbUpdate $ ClearSignatoryMobile $ signatorylinkid sl
      EmailAndMobileConfirmationDelivery -> do
        unless (checkEmailForConfirmation sl) $ do
          dbUpdate $ ClearSignatoryEmail $ signatorylinkid sl
        unless (checkMobileForConfirmation sl) $ do
          dbUpdate $ ClearSignatoryMobile $ signatorylinkid sl
      NoConfirmationDelivery -> return ()
      EmailLinkConfirmationDelivery -> do
        unless (checkEmailForConfirmation sl) $ do
          dbUpdate $ ClearSignatoryEmail $ signatorylinkid sl
      EmailLinkAndMobileConfirmationDelivery -> do
        unless (checkEmailForConfirmation sl) $ do
          dbUpdate $ ClearSignatoryEmail $ signatorylinkid sl
        unless (checkMobileForConfirmation sl) $ do
          dbUpdate $ ClearSignatoryMobile $ signatorylinkid sl

    signatoryHasValidAuthSettings sl = authToSignIsValid sl

    authToSignIsValid sl = getPersonalNumber sl == "" || case signatorylinkauthenticationtosignmethod sl of
      SEBankIDAuthenticationToSign -> isGood $ asValidSEBankIdPersonalNumber $ getPersonalNumber sl
      NOBankIDAuthenticationToSign -> isGood $ asValidNorwegianSSN $ getPersonalNumber sl
      DKNemIDAuthenticationToSign  -> False -- Danish Nets eSigning is not supported in API v1
      StandardAuthenticationToSign -> True
      SMSPinAuthenticationToSign   -> True

    signatoryHasValidSSNForIdentifyToView sl = case (signatorylinkauthenticationtoviewmethod sl) of
      SEBankIDAuthenticationToView -> isGood $ asValidSwedishSSN $ getPersonalNumber sl
      NOBankIDAuthenticationToView -> isGood $ asValidNorwegianSSN $ getPersonalNumber sl
      DKNemIDAuthenticationToView  -> isGood $ asValidDanishSSN $ getPersonalNumber sl
      FITupasAuthenticationToView  -> False -- Finnish TUPAS auth to view is not supported in API v1
      SMSPinAuthenticationToView   -> True
      StandardAuthenticationToView -> True

    signatoryHasValidPhoneForIdentifyToView sl =
      let resultValidPhone = asValidPhoneForNorwegianBankID $ getMobile sl in
      case signatorylinkauthenticationtoviewmethod sl of
        SEBankIDAuthenticationToView -> True
        NOBankIDAuthenticationToView -> isGood resultValidPhone || isEmpty resultValidPhone
        DKNemIDAuthenticationToView  -> True
        FITupasAuthenticationToView  -> False -- Finnish TUPAS auth to view is not supported in API v1
        SMSPinAuthenticationToView   -> isGood $ asValidPhoneForSMS $ getMobile sl
        StandardAuthenticationToView -> True

    checkEmailForConfirmation sl =
      null (getEmail sl) || isGood (asValidEmail $ getEmail sl)
    checkMobileForConfirmation sl =
      null (getMobile sl) || isGood (asValidPhoneForSMS $ getMobile sl)
    checkEmailAndMobileForConfirmation sl =
      checkEmailForConfirmation sl && checkMobileForConfirmation sl

apiCallV1Cancel :: Kontrakcja m =>  DocumentID -> m Response
apiCallV1Cancel did = logDocument did . api $ do
    checkObjectVersionIfProvided did
    (user, actor, _) <- getAPIUser APIDocSend
    withDocumentID did $ do
      guardAuthorOrAuthorsAdmin user "Permission problem. You don't have a permission to cancel this document"
      unlessM (isPending <$> theDocument) $ do
            throwM . SomeDBExtraException $ (conflictError "Document is not pending")
      dbUpdate $ CancelDocument actor
      postDocumentCanceledChange =<< theDocument
      Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)

apiCallV1Reject :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
apiCallV1Reject did slid = logDocumentAndSignatory did slid . api $ do
  checkObjectVersionIfProvided did
  (mh,mu) <- getMagicHashAndUserForSignatoryAction did slid
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    ctx  <- getContext
    msll <- getSigLinkFor slid <$> theDocument
    sll  <- case msll of
      Nothing  -> unexpectedError "apiCallV1Reject: Couldn't get sig link!"
      Just sll -> return sll
    customtext <- fmap strip <$> getField "customtext"
    switchLang . getLang =<< theDocument
    (dbUpdate . RejectDocument slid (isApprover sll) customtext =<< signatoryActor ctx sll)
        `catchDBExtraException` (\(DocumentStatusShouldBe _ _ i) -> throwM . SomeDBExtraException $ conflictError $ "Document not pending but " ++ show i)
        `catchDBExtraException` (\(SignatoryHasAlreadySigned {}) -> throwM . SomeDBExtraException $ conflictError $ "Signatory has already signed")
    postDocumentRejectedChange slid customtext =<< theDocument
    Accepted <$> (documentJSONV1 mu True True Nothing =<< theDocument)

apiCallV1CheckSign :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m Response
apiCallV1CheckSign did slid = logDocumentAndSignatory did slid . api $ do
  checkObjectVersionIfProvided did

  (mh,_) <- getMagicHashAndUserForSignatoryAction did slid

  (dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    whenM (not <$> isPending <$> theDocument ) $ do
      (throwM . SomeDBExtraException $ conflictError $ "Document not pending")
    whenM (isSignatoryAndHasSigned . getSigLinkFor slid <$> theDocument) $ do
      (throwM . SomeDBExtraException $ conflictError $ "Document already signed")
    whenM (theDocument >>= \doc -> signatoryNeedsToIdentifyToView (fromJust $ getSigLinkFor slid doc) doc) $ do
      (throwM . SomeDBExtraException $ forbidden "Authorization to view is needed")
    checkAuthenticationToSignMethodAndValue slid
    authorization <- signatorylinkauthenticationtosignmethod <$> fromJust . getSigLinkFor slid <$> theDocument
    fields <- getFieldForSigning
    unlessM (allRequiredAuthorAttachmentsAreAccepted =<< getAcceptedAuthorAttachments) $ do
      unlessM (isAuthor <$> fromJust . getSigLinkFor slid <$> theDocument) $ do -- Author does not need to accept attachments
        (throwM . SomeDBExtraException $ badInput $ "Some required attachments where not accepted")

    case authorization of
       StandardAuthenticationToSign -> return $ Right $ Ok () -- If we have a document with standard auth, it can be always signed if its not closed and signed
       SMSPinAuthenticationToSign -> do
             validPin <- getValidPin slid fields
             if (isJust validPin)
               then return $ Right $ Ok ()
               else (Left . Failed) <$> (J.runJSONGenT $ J.value "pinProblem" True)
       SEBankIDAuthenticationToSign -> dbQuery (GetESignature slid) >>= \case
         Just _ -> return $ Right $ Ok ()
         Nothing -> do
           logInfo_ "No e-signature found for a signatory"
           return . Left . Failed $ J.runJSONGen $ J.value "noSignature" True
       NOBankIDAuthenticationToSign -> do
         logAttention_ "Norwegian BankID signing attempted in V1 API"
         (Left . Failed) <$> (J.runJSONGenT $ J.value "noBankidNotSupported" True)
       DKNemIDAuthenticationToSign -> do
         logAttention_ "Danish NemID signing attempted in V1 API"
         (Left . Failed) <$> (J.runJSONGenT $ J.value "dkNemidNotSupported" True)

apiCallV1Sign :: Kontrakcja m
             => DocumentID      -- ^ The DocumentID of the document to sign
             -> SignatoryLinkID -- ^ The SignatoryLinkID that is in the URL
             -> m Response
apiCallV1Sign did slid = logDocumentAndSignatory did slid . api $ do
  checkObjectVersionIfProvided did
  logInfo_ "Ready to sign a document for signatory"
  (mh,mu) <- getMagicHashAndUserForSignatoryAction did slid
  screenshots' <- fmap (fromMaybe emptySignatoryScreenshots) $
               (fromJSValue =<<) <$> getFieldJSON "screenshots"
  screenshots <- resolveReferenceScreenshotNames screenshots' >>= \case
                   Nothing -> throwM . SomeDBExtraException $ badInput "Illegal reference screenshot name"
                   Just s -> return s
  fields <- getFieldForSigning
  acceptedAuthorAttachments <- getAcceptedAuthorAttachments
  olddoc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh -- We store old document, as it is needed by postDocumentXXX calls
  olddoc `withDocument` ( do
    whenM (not . isPending <$> theDocument ) $ do
      (throwM . SomeDBExtraException $ conflictError $ "Document not pending")
    whenM (isSignatoryAndHasSigned . getSigLinkFor slid <$> theDocument) $ do
      (throwM . SomeDBExtraException $ conflictError $ "Document already signed")
    whenM (theDocument >>= \doc -> signatoryNeedsToIdentifyToView (fromJust $ getSigLinkFor slid doc) doc) $ do
      (throwM . SomeDBExtraException $ forbidden "Authorization to view is needed")
    unlessM (allRequiredAuthorAttachmentsAreAccepted acceptedAuthorAttachments) $ do
      unlessM (isAuthor <$> fromJust . getSigLinkFor slid <$> theDocument) $ do -- Author does not need to accept attachments
        (throwM . SomeDBExtraException $ badInput $ "Some required attachments where not accepted")
    checkAuthenticationToSignMethodAndValue slid
    authorization <- signatorylinkauthenticationtosignmethod <$> fromJust . getSigLinkFor slid <$> theDocument

    case authorization of
      StandardAuthenticationToSign -> do
        signDocument slid mh fields acceptedAuthorAttachments Nothing Nothing screenshots
        postDocumentPendingChange olddoc
        handleAfterSigning slid
        (Right . Accepted) <$> (documentJSONV1 mu True True Nothing =<< theDocument)

      SMSPinAuthenticationToSign -> do
        validPin <- getValidPin slid fields
        if (isJust validPin)
          then do
            signDocument slid mh fields acceptedAuthorAttachments Nothing validPin screenshots
            postDocumentPendingChange olddoc
            handleAfterSigning slid
            (Right . Accepted) <$> (documentJSONV1 mu True True Nothing =<< theDocument)
          else (Left . Failed) <$> (J.runJSONGenT $ return ())

      SEBankIDAuthenticationToSign -> dbQuery (GetESignature slid) >>= \case
        mesig@(Just _) -> do
          signDocument slid mh fields acceptedAuthorAttachments mesig Nothing screenshots
          postDocumentPendingChange olddoc
          handleAfterSigning slid
          (Right . Accepted) <$> (documentJSONV1 mu True True Nothing =<< theDocument)
        Nothing -> do
          logInfo_ "No e-signature found for a signatory"
          return . Left . Failed $ J.runJSONGen $ J.value "noSignature" True
      NOBankIDAuthenticationToSign -> do
        logAttention_ "Norwegian BankID signing attempted in V1 API"
        (Left . Failed) <$> (J.runJSONGenT $ J.value "noBankidNotSupported" True)
      DKNemIDAuthenticationToSign -> do
        logAttention_ "Danish NemID signing attempted in V1 API"
        (Left . Failed) <$> (J.runJSONGenT $ J.value "dkNemidNotSupported" True)

   )
    `catchDBExtraException` (\(DocumentStatusShouldBe _ _ i) -> throwM . SomeDBExtraException $ conflictError $ "Document not pending but " ++ show i)
    `catchDBExtraException` (\(SignatoryHasAlreadySigned {}) -> throwM . SomeDBExtraException $ conflictError $ "Signatory has already signed")

{- | Utils for signing with eleg -}
checkAuthenticationToSignMethodAndValue :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m ()
checkAuthenticationToSignMethodAndValue slid = do
  mAuthType  :: Maybe String <- getField "authentication_type"
  mAuthValue :: Maybe String <- getField "authentication_value"
  case (mAuthType, mAuthValue) of
       (Just authType, Just authValue) -> do
           let mAuthMethod = fromJSValue $ J.toJSValue authType
           case mAuthMethod of
                Just authMethod -> do
                    siglink <- fromJust . getSigLinkFor slid <$> theDocument
                    let authOK = authMethod == signatorylinkauthenticationtosignmethod siglink
                    case (authOK, authMethod) of
                         (False, _) -> throwM . SomeDBExtraException $
                             conflictError "`authentication_type` does not match"
                         (True, StandardAuthenticationToSign) -> return ()
                         (True, SEBankIDAuthenticationToSign)   ->
                             if (authValue == getPersonalNumber siglink || null (getPersonalNumber siglink))
                                then return ()
                                else throwM . SomeDBExtraException $
                                    conflictError "`authentication_value` for personal number does not match"
                         (True, NOBankIDAuthenticationToSign)   ->
                             throwM . SomeDBExtraException $
                               conflictError "Norwegian BankID signing not supported in API V1"
                         (True, DKNemIDAuthenticationToSign)   ->
                             throwM . SomeDBExtraException $
                               conflictError "Danish NemID signing not supported in API V1"
                         (True, SMSPinAuthenticationToSign) ->
                             if (authValue == getMobile siglink || null (getMobile siglink))
                                then return ()
                                else throwM . SomeDBExtraException $
                                    conflictError "`authentication_value` for phone number does not match"
                Nothing ->
                    throwM . SomeDBExtraException $ badInput "`authentication_type` was not a valid"
       (Nothing, Nothing) -> return ()
       _ -> throwM . SomeDBExtraException $ badInput "Only one of `authentication_type` and `authentication_value` provided"

signDocument :: (Kontrakcja m, DocumentMonad m)
             => SignatoryLinkID
             -> MagicHash
             -> [(FieldIdentity, FieldTmpValue)]
             -> [FileID]
             -> Maybe ESignature
             -> Maybe String
             -> SignatoryScreenshots
             -> m ()
signDocument slid mh fields acceptedAuthorAttachments mesig mpin screenshots = do
  switchLang =<< getLang <$> theDocument
  ctx <- getContext
  -- Note that the second 'getSigLinkFor' call below may return a
  -- different result than the first one due to the field update, so
  -- don't attempt to replace the calls with a single call, or the
  -- actor identities may get wrong in the evidence log.
  fieldsWithFiles <- fieldsToFieldsWithFiles fields
  getSigLinkFor slid <$> theDocument >>= \(Just sl) -> dbUpdate . UpdateFieldsForSigning sl (fst fieldsWithFiles) (snd fieldsWithFiles) =<< signatoryActor ctx sl
  theDocument >>= \doc -> do
    let sl = fromJust (getSigLinkFor slid doc)
    authorAttachmetsWithAcceptanceText <- forM (documentauthorattachments doc) $ \a -> do
      acceptanceText <- renderTemplate "_authorAttachmentsUnderstoodContent" (F.value "attachment_name" $ authorattachmentname a)
      return (acceptanceText,a)
    dbUpdate . AddAcceptedAuthorAttachmentsEvents sl acceptedAuthorAttachments authorAttachmetsWithAcceptanceText  =<< signatoryActor ctx sl
  getSigLinkFor slid <$> theDocument >>= \(Just sl) -> dbUpdate . SignDocument slid mh mesig mpin screenshots =<< signatoryActor ctx sl

{- End of utils-}

apiCallV1Restart :: Kontrakcja m =>  DocumentID -> m Response
apiCallV1Restart did = logDocument did . api $ do
    checkObjectVersionIfProvided did
    (user, actor, _) <- getAPIUser APIDocSend
    doc <- dbQuery $ GetDocumentByDocumentID $ did
    auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
    when (not $ (auid == userid user)) $ do
          throwM . SomeDBExtraException $ serverError "Permission problem. Not an author."
    when (documentstatus doc `elem` [Pending,Preparation, Closed] ) $ do
          throwM . SomeDBExtraException $ (conflictError "Document can not be restarted")
    newdocument <- apiGuardJustM (serverError "Document can't be restarted") $ dbUpdate $ RestartDocument doc actor
    Accepted <$> documentJSONV1 (Just $ user) True True Nothing newdocument

apiCallV1Prolong :: Kontrakcja m =>  DocumentID -> m Response
apiCallV1Prolong did = logDocument did . api $ do
    checkObjectVersionIfProvided did
    (user, actor, _) <- getAPIUser APIDocSend
    withDocumentID did $ do
      hasPermission <- isAuthorOrAuthorsAdmin user <$> theDocument
      when (not hasPermission) $
        throwM . SomeDBExtraException $ serverError "Permission problem. Not an author[s admin]."
      unlessM (isTimedout <$> theDocument) $ do
            throwM . SomeDBExtraException $ (conflictError "Document is not timedout")
      mdays <- getDefaultedField 1 asValidNumber "days"
      days <- case mdays of
           Nothing -> throwM . SomeDBExtraException $ (badInput "Number of days to sign must be a valid number, between 1 and 365")
           Just n -> if (n < 1 || n > 365)
                              then throwM . SomeDBExtraException $ (badInput "Number of days to sign must be a valid number, between 1 and 365")
                              else return n
      timezone <- documenttimezonename <$> theDocument
      dbUpdate $ ProlongDocument days timezone actor
      triggerAPICallbackIfThereIsOne =<< theDocument
      Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)


apiCallV1SetAutoReminder :: Kontrakcja m => DocumentID -> m Response
apiCallV1SetAutoReminder did = logDocument did . api $ do
    ctx <- getContext
    checkObjectVersionIfProvided did
    (user, _actor, _) <- getAPIUser APIDocSend
    withDocumentID did $ do
      auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
      when (not $ (auid == userid user)) $ do
            throwM . SomeDBExtraException $ serverError "Permission problem. Not an author."
      unlessM (isPending <$> theDocument) $ do
            throwM . SomeDBExtraException $ (conflictError "Document is not pending")
      mdays <- getOptionalField asValidNumber "days"
      days <- case mdays of
           Nothing -> return Nothing
           Just n -> do tot <- documenttimeouttime <$> theDocument
                        if n < 1 || (isJust tot && n `daysAfter` (get ctxtime ctx) > fromJust tot)
                          then throwM . SomeDBExtraException $ (badInput "Number of days to send autoreminder must be a valid number, between 1 and number of days left till document deadline")
                          else return $ Just (fromIntegral n :: Int32)
      timezone <- documenttimezonename <$> theDocument
      setAutomaticReminder did days timezone
      triggerAPICallbackIfThereIsOne =<< theDocument
      Accepted <$> (documentJSONV1 (Just $ user) True True Nothing =<< theDocument)

apiCallV1TriggerCallback :: Kontrakcja m => DocumentID -> m Response
apiCallV1TriggerCallback did = logDocument did . api $ do
  (user, _, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    guardAuthorOrAuthorsAdmin user "Permission problem. You don't have a permission to trigger a callback for this document"
    triggerAPICallbackIfThereIsOne =<< theDocument
    Accepted <$> (J.runJSONGenT $ return ())

apiCallV1ChangeAuthenticationToView :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
apiCallV1ChangeAuthenticationToView did slid = logDocumentAndSignatory did slid . api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    guardAuthorOrAuthorsAdmin user "Permission problem. You don't have a permission to change this document"
    -- Document status and input checks
    unlessM (isPending <$> theDocument) $
      throwM . SomeDBExtraException $ badInput "Document status must be pending"
    sl <- getSigLinkFor slid <$> theDocument >>= \case
      Nothing -> throwM . SomeDBExtraException $ badInput $ "Signatory link id " ++ (show slid) ++ " not valid for document id " ++ (show did)
      Just sl -> return sl
    when (not . isSignatory $ sl) $
      throwM . SomeDBExtraException $ badInput $ "Signatory link id " ++ (show slid) ++ " is a viewer or approver and does not sign"
    when (isSignatoryAndHasSigned sl) $
      throwM . SomeDBExtraException $ badInput $ "Signatory link id " ++ (show slid) ++ " has already signed"
    when (isApproverAndHasApproved sl) $
      throwM . SomeDBExtraException $ badInput $ "Signatory link id " ++ (show slid) ++ " has already approved"
    when (signatorylinkidentifiedtoview sl) $
      throwM . SomeDBExtraException $ badInput $ "Signatory link id " ++ (show slid) ++ " has already identified to view"
    -- Get the POST data and check it
    authentication_type <- getField "authentication_type"
    personal_number <- getField "personal_number"
    mobile_number <- getField "mobile_number"
    when (isNothing authentication_type) $
      throwM . SomeDBExtraException $ badInput $
        "`authentication_type` must be given. Supported values are: `standard`, `se_bankid`, `no_bankid`, `dk_nemid`."
    (authtoview, mSSN, mPhone) <- case fromJSValue $ J.toJSValue $ fromMaybe "" authentication_type of
      Nothing -> throwM . SomeDBExtraException $ badInput $
        "Invalid authentication method: `" ++ fromMaybe "" authentication_type ++ "` was given. Supported values are: `standard`, `se_bankid`, `no_bankid`."
      Just StandardAuthenticationToView -> return (StandardAuthenticationToView, Nothing, Nothing)
      Just SMSPinAuthenticationToView   -> return (SMSPinAuthenticationToView  , Nothing, mobile_number)
      Just SEBankIDAuthenticationToView -> return (SEBankIDAuthenticationToView, personal_number, Nothing)
      Just NOBankIDAuthenticationToView -> return (NOBankIDAuthenticationToView, personal_number, mobile_number)
      Just DKNemIDAuthenticationToView  -> return (DKNemIDAuthenticationToView , personal_number, Nothing)
      -- Finnish TUPAS is not supported in API V1
      Just FITupasAuthenticationToView  -> throwM . SomeDBExtraException $ badInput $
        "Invalid `authentication_type`. Supported values are: `standard`, `se_bankid`, `no_bankid`, `dk_nemid`."
    -- Check conditions on signatory
    guardAuthenticationMethodsCanMix authtoview $ signatorylinkauthenticationtosignmethod sl
    case mSSN of
      -- Signatory must already have valid SSN set
      Nothing -> unless (isValidSSNForAuthenticationToView authtoview $ getPersonalNumber sl) $
        throwM . SomeDBExtraException $ badInput "Signatory does not have a valid personal number for the authentication method and you did not provide one"
      Just ssn -> unless (isValidSSNForAuthenticationToView authtoview ssn) $
        throwM . SomeDBExtraException $ badInput "The personal number you provided is not valid for the authentication method"
    case mPhone of
      Nothing -> unless (isValidPhoneForAuthenticationToView authtoview $ getMobile sl) $
        throwM . SomeDBExtraException $ badInput "Signatory does not have a valid phone number set for the authentication method and you did not provide one"
      Just phone -> unless (isValidPhoneForAuthenticationToView authtoview phone) $
        throwM . SomeDBExtraException $ badInput "The phone number you provided is not valid for the authentication method"
    -- Change authentication method and return Document JSON
    dbUpdate $ ChangeAuthenticationToViewMethod slid AuthenticationToView authtoview mSSN mPhone actor
    Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)
  where
    isValidSSNForAuthenticationToView :: AuthenticationToViewMethod -> String -> Bool
    isValidSSNForAuthenticationToView StandardAuthenticationToView _ = True
    isValidSSNForAuthenticationToView SMSPinAuthenticationToView   _ = True
    isValidSSNForAuthenticationToView SEBankIDAuthenticationToView ssn = isGood $ asValidSwedishSSN   ssn
    isValidSSNForAuthenticationToView NOBankIDAuthenticationToView ssn = isGood $ asValidNorwegianSSN ssn
    isValidSSNForAuthenticationToView DKNemIDAuthenticationToView  ssn = isGood $ asValidDanishSSN    ssn
    -- Finnish TUPAS is not supported in API V1
    isValidSSNForAuthenticationToView FITupasAuthenticationToView  _ = False
    isValidPhoneForAuthenticationToView :: AuthenticationToViewMethod -> String -> Bool
    isValidPhoneForAuthenticationToView StandardAuthenticationToView _ = True
    isValidPhoneForAuthenticationToView SMSPinAuthenticationToView phone = isGood (asValidPhoneForSMS phone)
    isValidPhoneForAuthenticationToView SEBankIDAuthenticationToView _ = True
    isValidPhoneForAuthenticationToView DKNemIDAuthenticationToView  _ = True
    isValidPhoneForAuthenticationToView NOBankIDAuthenticationToView phone =
      let phoneValidation = asValidPhoneForNorwegianBankID phone in isGood phoneValidation || isEmpty phoneValidation
    -- Finnish TUPAS is not supported in API V1
    isValidPhoneForAuthenticationToView FITupasAuthenticationToView _ = False

apiCallV1ChangeAuthenticationToSign :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m Response
apiCallV1ChangeAuthenticationToSign did slid = logDocumentAndSignatory did slid . api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
      guardAuthorOrAuthorsAdmin user "Permission problem. You don't have a permission to change this document"
      -- Document status and input checks
      unlessM (isPending <$> theDocument) $
          throwM . SomeDBExtraException $ badInput "Document status must be pending"
      sl <- getSigLinkFor slid <$> theDocument >>= \case
        Nothing ->
          throwM . SomeDBExtraException $ badInput $ "Signatory link id " ++ (show slid) ++ " not valid for document id " ++ (show did)
        Just sl -> return sl
      when (isSignatoryAndHasSigned sl) $
          throwM . SomeDBExtraException $ badInput $ "Signatory link id " ++ (show slid) ++ " has already signed"
      -- Get the POST data and check it
      authentication_type  <- getField "authentication_type"
      authentication_value <- getField "authentication_value"
      when (isNothing authentication_type) $
        throwM . SomeDBExtraException $ badInput
          "`authentication_type` must be given. Supported values are: `standard`, `eleg`, `sms_pin`."
      (authtosignmethod, mSSN, mPhone) <- case fromJSValue $ J.toJSValue $ fromMaybe "" authentication_type of
        Nothing -> throwM . SomeDBExtraException $ badInput
          "`authentication_type` was not valid. Supported values are: `standard`, `eleg`, `sms_pin`."
        Just StandardAuthenticationToSign -> return (StandardAuthenticationToSign, Nothing, Nothing)
        Just SEBankIDAuthenticationToSign -> return (SEBankIDAuthenticationToSign, authentication_value, Nothing)
        Just SMSPinAuthenticationToSign   -> return (SMSPinAuthenticationToSign, Nothing, authentication_value)
        Just NOBankIDAuthenticationToSign -> throwM . SomeDBExtraException $ badInput
          "`authentication_type` was not valid. Supported values are: `standard`, `eleg`, `sms_pin`."
        Just DKNemIDAuthenticationToSign  -> throwM . SomeDBExtraException $ badInput
          "`authentication_type` was not valid. Supported values are: `standard`, `eleg`, `sms_pin`."

      let authtoviewmethod = signatorylinkauthenticationtoviewmethod sl
      -- Check conditions for different authentication to sign methods
      guardAuthenticationMethodsCanMix authtoviewmethod authtosignmethod
      case authtosignmethod of
        StandardAuthenticationToSign -> return ()
        SEBankIDAuthenticationToSign -> do
          case mSSN of
            Nothing -> return ()
            -- If we are given a Swedish SSN
            Just ssn -> do
              when (signatorylinkidentifiedtoview sl && ssn /= getPersonalNumber sl) $
                throwM . SomeDBExtraException $ badInput "The signatory has authenticated to view, therefore you can't change the authentication value"
              case asValidSwedishSSN ssn of
                -- Empty is allowed only if we don't need it for AuthenticationToViewMethod
                Empty -> when (authtoviewmethod == SEBankIDAuthenticationToView) $
                  throwM . SomeDBExtraException $ badInput "You provided an empty authentication value, needs a value for authentication to view"
                Bad -> throwM . SomeDBExtraException $ badInput "The authentication value provided is not a valid for Swedish BankID"
                Good _ -> return ()
        SMSPinAuthenticationToSign -> case mPhone of
          Nothing -> return ()
          Just phone -> do
            -- If the signatory has authenticated to view with NOBankIDAuthenticationToView and a valid number, then we can't change the phone number!
            when (   authtoviewmethod == NOBankIDAuthenticationToView
                  && signatorylinkidentifiedtoview sl
                  && getMobile sl /= ""
                  && phone /= getMobile sl) $
              throwM . SomeDBExtraException $ badInput "The signatory has authenticated to view with Norwegian BankID, therefore you can't change the phone number"
            -- If given a phone number we need to make sure it doesn't invalidate NOBankIDAuthenticationToView
            when (authtoviewmethod == NOBankIDAuthenticationToView) $
              case asValidPhoneForNorwegianBankID phone of
                Bad -> throwM . SomeDBExtraException $ badInput "Phone number needs to be a valid Norwegian number as Norwegian BankID is set as authentication to view"
                Empty -> return ()
                Good _ -> return ()
        NOBankIDAuthenticationToSign ->
          throwM . SomeDBExtraException $ badInput "Norwegian BankID signing is not supported in API V1"
        DKNemIDAuthenticationToSign ->
          throwM . SomeDBExtraException $ badInput "Danish NemID signing is not supported in API V1"

      -- Change authentication to sign method and return Document JSON
      dbUpdate $ ChangeAuthenticationToSignMethod slid authtosignmethod mSSN mPhone actor
      Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)

guardAuthenticationMethodsCanMix :: Kontrakcja m => AuthenticationToViewMethod -> AuthenticationToSignMethod -> m ()
guardAuthenticationMethodsCanMix authtoview authtosign = do
  -- API v1 doesn't deal with auth-to-view-archived
  let authToViewArchived = StandardAuthenticationToView
  when (not $ authenticationMethodsCanMix authtoview authtosign authToViewArchived)
    (throwM . SomeDBExtraException $ badInput $ "Can't mix " <> show authtoview <> " and " <> show authtosign <> ".")

apiCallV1Remind :: Kontrakcja m => DocumentID -> m Response
apiCallV1Remind did = logDocument did . api $ do
  (user, actor , _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    unlessM (isPending <$> theDocument) $ do
          throwM . SomeDBExtraException $ serverError "Can't send reminder for documents that are not pending"
    hasPermission <- isAuthorOrAuthorsAdmin user <$> theDocument
    when (not hasPermission) $
      throwM . SomeDBExtraException $ serverError "Permission problem. Not an author[s admin]."
    void $ sendAllReminderEmailsExceptAuthor actor False
    Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)

apiCallV1Forward :: Kontrakcja m => DocumentID -> m Response
apiCallV1Forward did = logDocument did . api $ do
  (user, _actor , _) <- getAPIUser APIDocCheck
  withDocumentID did $ do
    unlessM (isClosed <$> theDocument) $ do
          throwM . SomeDBExtraException $ badInput "Only document that are signed can be forwarded"
    asiglink <- apiGuardJustM (serverError "No author found") $ getAuthorSigLink <$> theDocument
    auid <- apiGuardJustM (serverError "No author found") $ return $ maybesignatory asiglink
    when (not $ (auid == userid user)) $ do
          throwM . SomeDBExtraException $ serverError "Permission problem. Not an author."
    email <- apiGuardJustM (badInput "Email adress is no valid.") $ getOptionalField  asValidEmail "email"
    noContent <- (== Just "true") <$> getField "nocontent"
    mNoAttachments <- getField "noattachments"
    let noAttachments = case (mNoAttachments, noContent) of
          (Just s, _)      -> s == "true"
          (Nothing, True)  -> True
          (Nothing, False) -> False
    void $ sendForwardEmail email noContent noAttachments asiglink -- Make sure we only send out the document with the author's signatory link when it is closed, otherwise the link may be abused
    Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)

apiCallV1Delete :: Kontrakcja m => DocumentID -> m Response
apiCallV1Delete did = logDocument did . api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    mauser <- theDocument >>= \d -> case join $ maybesignatory <$> getAuthorSigLink d of
                         Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                         _ -> return Nothing
    msl <- getSigLinkFor user <$> theDocument
    let haspermission = (isJust msl)
                     || (isJust mauser && usergroupid (fromJust mauser) == usergroupid user && (useriscompanyadmin user))
    when (not haspermission) $ do
           throwM . SomeDBExtraException $ serverError "Permission problem. Not connected to document."
    dbUpdate $ ArchiveDocument (userid user) actor

    Accepted <$> (J.runJSONGenT $ return ())


apiCallV1ReallyDelete :: Kontrakcja m => DocumentID -> m Response
apiCallV1ReallyDelete did = logDocument did . api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  withDocumentID did $ do
    mauser <- theDocument >>= \d -> case join $ maybesignatory <$> getAuthorSigLink d of
                         Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                         _ -> return Nothing
    msl <- getSigLinkFor user <$> theDocument
    let haspermission = (isJust msl)
                     || (isJust mauser && usergroupid (fromJust mauser) == usergroupid user && (useriscompanyadmin user))
    when (not haspermission) $ do
           throwM . SomeDBExtraException $ serverError "Permission problem. Not connected to document."
    dbUpdate $ ReallyDeleteDocument (userid user) actor

    Accepted <$> (J.runJSONGenT $ return ())



-- TODO test case to make sure apiCallV1Get does not update document version (MarkDocumentSeen case)
-- This function intentionally does not use withDocument function family for db doc locking.
-- This handler is purely read-only, so no need for locking.
-- Because of no locking, this will not have to wait in kontrakcja, while cron is sealing document,
-- which makes signing process a lot faster in retail pos applications
apiCallV1Get :: Kontrakcja m => DocumentID -> m Response
apiCallV1Get did = logDocument did . api $ do
  (msignatorylink :: Maybe SignatoryLinkID) <- readField "signatoryid"
  mmagichashh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  doc <- dbQuery $ GetDocumentByDocumentID did
  case (msignatorylink,mmagichashh) of
    (Just slid,Just mh) -> do
       sl <- maybe (throwM $ SomeDBExtraException $ serverError "No document found") return  $ getSigLinkFor slid doc
       guardThatDocumentIsReadableBySignatories doc
       when (not (isValidSignatoryMagicHash mh sl)) $ throwM . SomeDBExtraException $ serverError "No document found"
       switchLang $ getLang doc

       Ok <$> (documentJSONV1 Nothing False (signatoryisauthor sl) (Just sl) doc)
    _ -> do
      (user, _actor, external) <- getAPIUser APIDocCheck
      let msiglink = getSigLinkFor user doc
      mauser <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                     Just auid -> dbQuery $ GetUserByIDIncludeDeleted auid
                     _ -> return Nothing

      haspermission <- return $ isJust msiglink
                       || (isJust mauser && usergroupid (fromJust mauser) == usergroupid user && (useriscompanyadmin user || isDocumentShared doc))
      if (haspermission)
        then do
          Ok <$> (documentJSONV1 (Just user) external ((userid <$> mauser) == (Just $ userid user)) msiglink doc)
        else throwM . SomeDBExtraException $ serverError "You do not have right to access document"

-- Return evidence attachments for document
apiCallV1GetEvidenceAttachments :: Kontrakcja m => DocumentID -> m Response
apiCallV1GetEvidenceAttachments did = logDocument did . api $ withDocumentID did $ do
  (user, _, _) <- getAPIUser APIDocCheck
  guardAuthorOrAuthorsAdmin user "Permission problem. You don't have a permission to access this document"
  Ok <$> (evidenceAttachmentsJSONV1 =<< theDocument)

apiCallV1List :: Kontrakcja m => m Response
apiCallV1List = api $ do
  (user@User{userid = uid}, _actor, _) <- getAPIUserWithPad APIDocCheck

  doctype <- getField' "documentType"
  params <- getListParams
  let (domain, filters1) = case doctype of
                          "Document"          -> (DocumentsVisibleToUser uid
                                                 ,[DocumentFilterDeleted False, DocumentFilterSignable, DocumentFilterUnsavedDraft False])
                          "Template"          -> (DocumentsVisibleToUser uid
                                                 ,[DocumentFilterDeleted False, DocumentFilterTemplate, DocumentFilterUnsavedDraft False])
                          "MyTemplate"        -> (DocumentsVisibleToUser uid -- Sometimes we want to show only templates that user can change
                                                 ,[DocumentFilterByAuthor uid, DocumentFilterDeleted False, DocumentFilterTemplate, DocumentFilterUnsavedDraft False])
                          "Rubbish"           -> (DocumentsVisibleToUser uid
                                                 ,[DocumentFilterDeleted True, DocumentFilterUnsavedDraft False])
                          "All"               -> (DocumentsVisibleToUser uid,[DocumentFilterUnsavedDraft False])
                          "DocumentsForPad"   -> (DocumentsVisibleToUser uid,[DocumentFilterByAuthor uid, DocumentFilterSignNowOnPad])
                          _ -> (DocumentsVisibleToUser uid,[DocumentFilterDeleted False, DocumentFilterUnsavedDraft False])
      filters2 = concatMap fltSpec (listParamsFilters params)
      fltSpec ("time", tostr) = case reads tostr of
                                    (((Just from',Just to'),""):_) -> [DocumentFilterByMonthYearFrom from',DocumentFilterByMonthYearTo to']
                                    (((Nothing ,Just to'),""):_) -> [DocumentFilterByMonthYearTo to']
                                    (((Just from',Nothing),""):_)   -> [DocumentFilterByMonthYearFrom from']
                                    _ -> []
      fltSpec ("mtime", tostr) = case parseTimeISO tostr of
                                    Just mtime -> [DocumentFilterByModificationTimeAfter mtime]
                                    _ -> []
      fltSpec ("sender", tostr) = case reads tostr of
                                    ((suid,""):_) -> [DocumentFilterByAuthor suid]
                                    _ -> []
      fltSpec ("cansign", tostr) = case reads tostr of
                                    ((suid,""):_) -> [DocumentFilterByCanSign suid]
                                    _ -> []
      fltSpec ("status", scstr) = case reads scstr of
                                    ((statusclasss,""):_) -> [DocumentFilterByStatusClass statusclasss]
                                    _ -> []
      fltSpec _ = []
  tagsstr <- getField' "tags"
  let tagsFilters = case runGetJSON readJSArray tagsstr of
                      Right js ->[DocumentFilterByTags $ join $ maybeToList $ (fromJSValueCustomMany fromJSValue js)]
                      _ -> []
  let sorting    = docSortingFromParams params
      searching  = [processSearchStringToFilter . T.pack . listParamsSearching $ params]
      pagination = (listParamsOffset params, listParamsLimit params, docsPageSize)
      filters = filters1 ++ filters2 ++ tagsFilters

  format <- getField "format"
  case format of
       Just "csv" -> do
          allDocs <- dbQuery $ GetDocuments domain (searching ++ filters) sorting 1000
          let allDocsCustomFields = allCustomTextOrCheckboxOrRadioGroupFields allDocs
              docsCSVs = concatMap (docForListCSVV1 allDocsCustomFields) allDocs
          return $ Left $ CSV { csvFilename = "documents.csv"
                              , csvHeader = docForListCSVHeaderV1 allDocsCustomFields
                              , csvContent = docsCSVs
                              }
       _ -> do
          startQueryTime <- liftIO getCurrentTime
          (allDocsCount, allDocs) <- dbQuery $ GetDocumentsWithSoftLimit domain (searching ++ filters) sorting pagination
          finishQueryTime <- liftIO getCurrentTime
          logInfo "Fetching for apiCallV1List done" $ object [
              "query_time" .= (realToFrac $ diffUTCTime finishQueryTime startQueryTime :: Double)
            ]
          let docs = PagedList {  list       = allDocs
                                , params     = params
                                , pageSize   = docsPageSize
                                -- Backward compatibility. When offset is set, we always return list length equal or greater then offset.
                                , listLength = max (listParamsOffset params) allDocsCount
                                }
          docsJSONs <- mapM (docForListJSONV1 user) $ list docs
          return $ Right $ J.runJSONGen $ do
              J.value "list" docsJSONs
              J.value "paging" $ pagingParamsJSON docs
  where
    docSortingFromParams :: ListParams -> [AscDesc DocumentOrderBy]
    docSortingFromParams params =
      addMTimeSorting . concatMap x $ listParamsSorting params
      where
        x "status"            = [Asc DocumentOrderByStatusClass]
        x "statusREV"         = [Desc DocumentOrderByStatusClass]
        x "title"             = [Asc DocumentOrderByTitle]
        x "titleREV"          = [Desc DocumentOrderByTitle]
        x "time"              = [Asc DocumentOrderByMTime]
        x "timeREV"           = [Desc DocumentOrderByMTime]
        x "party"             = [Asc DocumentOrderByPartners]
        x "partyREV"          = [Desc DocumentOrderByPartners]
        x "partner"           = [Asc DocumentOrderByPartners]
        x "partnerREV"        = [Desc DocumentOrderByPartners]
        x "type"              = [Asc DocumentOrderByType]
        x "typeREV"           = [Desc DocumentOrderByType]
        x "author"            = [Asc DocumentOrderByAuthor]
        x "authorREV"         = [Desc DocumentOrderByAuthor]
        x _                   = []

    docsPageSize :: Int
    docsPageSize = 100


apiCallV1CheckAvailable :: Kontrakcja m => m Response
apiCallV1CheckAvailable = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  readField "ids" >>= \case
    Nothing -> throwM . SomeDBExtraException $ serverError "No ids parameter was provided or it had wrong format"
    Just (ids::[DocumentID]) -> do
      when (length ids > 10000) $ do
        throwM . SomeDBExtraException $ serverError "This request can't check more than 10000 documents"
      logInfo "Checking availability of user's documents" $ object [
          identifier $ userid user
        ]
      docids <- dbQuery $ GetDocumentsIDs (DocumentsVisibleToUser $ userid user) [DocumentFilterDeleted False, DocumentFilterByDocumentIDs ids] []
      Ok <$> (J.runJSONGenT $ J.value "ids" (show <$> docids))


apiCallV1History :: Kontrakcja m => DocumentID -> m Response
apiCallV1History did = logDocument did . api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  ctx <- getContext
  modifyContext $ set ctxmaybeuser (Just user)
  mlang <- (join . (fmap langFromCode)) <$> getField "lang"
  switchLang $ fromMaybe (lang $ usersettings user) mlang

  doc <- getDocByDocID did
  evidenceLog <- dbQuery $ GetEvidenceLog $ did
  events <- eventsJSListFromEvidenceLog  doc evidenceLog
  res <- J.runJSONGenT $ do
      J.value "list" $ for (reverse events) $ J.runJSONGen . (J.value "fields")
      J.value "paging" $ pagingParamsJSON (PagedList events 1000 emptyListParams (length events))

  modifyContext $ set ctxmaybeuser (get ctxmaybeuser ctx)
  return res


-- | This handler downloads main file of document. This means sealed file for Closed documents.
--   or one with preprinted fields if not closed

apiCallV1DownloadMainFile :: Kontrakcja m => DocumentID -> String -> m Response
apiCallV1DownloadMainFile did _nameForBrowser = logDocument did . api $ do

  (msid :: Maybe SignatoryLinkID) <- readField "signatorylinkid"
  (maccesstoken :: Maybe MagicHash) <- readField "accesstoken"
  mmh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msid

  doc <- do
           case (msid, mmh, maccesstoken) of
            (Just sid, Just mh, _) -> do
              (dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh) `withDocumentM` theDocument >>= \doc -> do
                sl <- apiGuardJustM  (serverError "Signatory does not exist") $
                        pure $ getSigLinkFor sid doc
                guardThatDocumentIsReadableBySignatories doc
                whenM (signatoryNeedsToIdentifyToView sl doc) $ do
                  when (isClosed doc || not (isAuthor sl)) $ do
                    throwM . SomeDBExtraException $ forbidden "Authorization to view is needed"
                return doc
            (_, _, Just _) -> getDocByDocIDAndAccessTokenV1 did maccesstoken
            _ ->  do
                  (user, _actor, external) <- getAPIUser APIDocCheck
                  if (external)
                    then do
                      ctx <- getContext
                      modifyContext $ set ctxmaybeuser (Just user)
                      res <- getDocByDocID did
                      modifyContext $ set ctxmaybeuser (get ctxmaybeuser ctx)
                      return res;
                    else getDocByDocID did

  content <- case documentstatus doc of
                Closed -> do
                  when (documentsealstatus doc == Just Missing) $ do
                    now <- currentTime
                    -- Give Guardtime signing a few seconds to complete before we respond
                    when (diffUTCTime now (documentmtime doc) < 8) $ do
                      logInfo "Waiting for Guardtime signing" $ object [
                          "document_last_modified_ago" .=
                            (realToFrac $ diffUTCTime now $ documentmtime doc :: Double)
                        ]
                      throwM $ SomeDBExtraException $ noAvailableYet "Digitally sealed document not ready"
                  file <- apiGuardJustM (noAvailableYet "Not ready, please try later") $ fileFromMainFile (documentsealedfile doc)
                  getFileIDContents $ fileid file
                _ -> do
                  sourceFile <- apiGuardJustM  (serverError "No file") $ fileFromMainFile $ documentfile doc
                  apiGuardL  (serverError "Can't get file content")  $ DocSeal.presealDocumentFile doc sourceFile
  return $ respondWithPDF False content

apiCallV1DownloadFile :: Kontrakcja m => DocumentID -> FileID -> String -> m Response
apiCallV1DownloadFile did fileid nameForBrowser = logDocumentAndFile did fileid . api $ do
  (msid :: Maybe SignatoryLinkID) <- readField "signatorylinkid"
  (maccesstoken :: Maybe MagicHash) <- readField "accesstoken"
  mmh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msid
  doc <- do
           case (msid, mmh, maccesstoken) of
            (Just sid, Just mh, _) -> do
              (dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh) `withDocumentM` theDocument >>= \doc -> do
                sl <- apiGuardJustM  (serverError "Signatory does not exist") $
                        pure (getSigLinkFor sid doc)
                guardThatDocumentIsReadableBySignatories doc
                whenM (signatoryNeedsToIdentifyToView sl doc) $ do
                  when (isClosed doc || not (isAuthor sl)) $ do
                    throwM . SomeDBExtraException $ forbidden "Authorization to view is needed"
                return doc
            (_, _, Just _accesstoken) -> getDocByDocIDAndAccessTokenV1 did maccesstoken
            _ ->  do
                  (user, _actor, external) <- getAPIUser APIDocCheck
                  if (external)
                    then do
                      ctx <- getContext
                      modifyContext $ set ctxmaybeuser (Just user)
                      res <- getDocByDocID did
                      modifyContext $ set ctxmaybeuser (get ctxmaybeuser ctx)
                      return res;
                    else getDocByDocID did
  let allfiles = maybeToList (mainfileid <$> documentfile doc) ++ maybeToList (mainfileid <$> documentsealedfile doc) ++
                      (authorattachmentfileid <$> documentauthorattachments doc) ++
                      (catMaybes $ map signatoryattachmentfile $ concatMap signatoryattachments $ documentsignatorylinks doc)
  if (all (/= fileid) allfiles)
     then throwM . SomeDBExtraException $ forbidden "Access to file is forbiden."
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

apiCallV1ExtractTexts :: Kontrakcja m => DocumentID -> FileID -> m Response
apiCallV1ExtractTexts did fileid = logDocumentAndFile did fileid . api $ do
  (user, _actor , _) <- getAPIUser APIDocCreate
  withDocumentID did $ do
    unlessM (isPreparation <$> theDocument) $ do
      throwM . SomeDBExtraException $ serverError "Can't extract texts from documents that are not in preparation"
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    when (not $ (auid == userid user)) $ do
      throwM . SomeDBExtraException $ serverError "Permission problem. Not an author."


    jsons <- apiGuardL (badInput "The MIME part 'json' must exist and must be a JSON.") $ getDataFn' (look "json")
    json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $ case decode jsons of
                                                                                 J.Ok js -> Just js
                                                                                 _ -> Nothing
    doc <- theDocument
    when (Just fileid /= (mainfileid <$> documentfile doc)) $ do
      throwM . SomeDBExtraException $ serverError "Requested file does not belong to the document"

    content <- getFileIDContents fileid
    eitherResult <- runJavaTextExtract json content
    case eitherResult of
      Left err -> throwM . SomeDBExtraException $ serverError (unpack err)
      Right res -> return $ Ok res

-- this one must be standard post with post params because it needs to
-- be posted from a browser form
-- Change main file, file stored in input "file" OR templateid stored in "template"
apiCallV1ChangeMainFile :: Kontrakcja m => DocumentID -> m Response
apiCallV1ChangeMainFile docid = logDocument docid . api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  checkObjectVersionIfProvided docid
  withDocumentID docid $ do
    auid <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
    unlessM (isPreparation <$> theDocument) $ do
      throwM . SomeDBExtraException $ (serverError "Document is not a draft or template")
    when (not $ (auid == userid user)) $ do
          throwM . SomeDBExtraException $ serverError "Permission problem. Not an author."

    moldfileid <- fmap mainfileid <$> documentfile  <$> theDocument
    fileinput <- getDataFn' (lookInput "file")

    mft <- case fileinput of
      Nothing -> return Nothing
      Just (Input _ Nothing _) -> throwM . SomeDBExtraException $ badInput "Missing file"
      Just (Input contentspec (Just filename') _contentType) -> do
        let filename = takeBaseName filename' ++ ".pdf"
        content1' <- case contentspec of
          Left filepath -> liftIO $ BS.readFile filepath
          Right content -> return (BS.concat (BSL.toChunks content))

        -- This is some kind of Salesforce hack that was supposed to be
        -- dropped with Happstack 7.0.4. It seems to be used till now
        -- for example by Avis.
        let content' = either (const content1') id (B64.decode content1')

        pdfcontent <- apiGuardL (badInput "The PDF is invalid.") $ preCheckPDF content'
        fileid' <- saveNewFile filename pdfcontent
        return $ Just (fileid', takeBaseName filename)

    case mft of
      Just (fileid,filename) -> do
        dbUpdate $ AttachFile fileid actor
        apiGuardL' $ dbUpdate $ SetDocumentTitle filename actor
        case moldfileid of
          Just oldfileid -> do
            start <- liftIO getCurrentTime
            recalculateAnchoredFieldPlacements oldfileid fileid
            stop <- liftIO getCurrentTime
            logInfo "recalculateAnchoredFieldPlacements timing" $ object
                    ["duration" .= (realToFrac $ diffUTCTime stop start :: Double)]
          Nothing -> return ()
      Nothing -> dbUpdate $ DetachFile actor
    Accepted <$> (documentJSONV1 (Just user) True True Nothing =<< theDocument)


apiCallV1SendSMSPinCode :: Kontrakcja m => DocumentID -> SignatoryLinkID ->  m Response
apiCallV1SendSMSPinCode did slid = logDocumentAndSignatory did slid . api $ do
  mh <- apiGuardL (serverError "No document found")  $ dbQuery $ GetDocumentSessionToken slid
  (dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh) `withDocumentM` do
    sl <- apiGuardJustM  (serverError "No document found") $ getSigLinkFor slid <$> theDocument
    whenM (not . isPending <$> theDocument) $ do
       throwM . SomeDBExtraException $ serverError "SMS pin code can't be sent to document that is not pending"
    when (SMSPinAuthenticationToSign /= signatorylinkauthenticationtosignmethod sl) $ do
       throwM . SomeDBExtraException $ serverError "SMS pin code can't be sent to this signatory"
    slidPhone <- getMobile <$> fromJust . getSigLinkFor slid <$> theDocument
    phone <- if not $ null slidPhone
                then return slidPhone
                else apiGuardJustM (badInput "Phone number provided is not valid.") $ getOptionalField asValidPhone "phone"
    pin <- dbQuery $ GetSignatoryPin SMSPinToSign slid phone
    sendPinCode sl phone pin
    Ok <$> (J.runJSONGenT $ J.value "sent" True)

-- Signatory Attachments handling
apiCallV1SetSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryLinkID -> String -> m Response
apiCallV1SetSignatoryAttachment did sid aname = logDocumentAndSignatory did sid . api $ do
  checkObjectVersionIfProvided did
  logInfo "Setting signatory attachments for signatory" $ object [
      "name" .= aname
    ]
  (mh,mu) <- getMagicHashAndUserForSignatoryAction did sid
  logInfo_ "We are authorized to set signatory attachment"
  -- We check permission here - because we are able to get a valid magichash here
  dbQuery (GetDocumentByDocumentIDSignatoryLinkIDMagicHash did sid mh) `withDocumentM` do
    unlessM (isPending <$> theDocument) $ do
            throwM . SomeDBExtraException $ (badInput "Document is not pending")
    sl  <- apiGuard (badInput "There is no signatory by that id.") =<< getSigLinkFor sid <$> theDocument
    sigattach <- apiGuard (badInput "The attachment with that name does not exist for the signatory.") =<< getSignatoryAttachment sid aname <$> theDocument
    filedata <- getDataFn' (lookInput "file")
    mfileid <- case filedata of
      (Just (Input contentspec (Just filename) _contentType)) ->  Just <$>  do
                content1 <- case contentspec of
                  Left filepath -> liftIO $ BSL.readFile filepath
                  Right content -> return content
                content <- if (".pdf" `isSuffixOf` (map toLower filename))
                  then apiGuardL (badInput "The PDF was invalid.") $ preCheckPDF (BSL.toStrict content1)
                  else if ( ".png" `isSuffixOf` (map toLower filename)
                          || ".jpg" `isSuffixOf` (map toLower filename)
                          || ".jpeg" `isSuffixOf` (map toLower filename))
                    then if (not $ BSL.null content1)
                      then return $ BSL.toStrict content1
                      else throwM . SomeDBExtraException $ badInput ("Image can't be empty. Uploaded filename was " ++ filename)
                    else throwM . SomeDBExtraException $ badInput ("Only pdf files or images can be attached. Uploaded filename was " ++ filename)
                (saveNewFile (dropFilePathFromWindows filename) content)
      _ -> return Nothing
    ctx <- getContext
    case mfileid of
      Just fileid -> (dbUpdate . SaveSigAttachment sid sigattach fileid =<< signatoryActor ctx sl)
                       `catchDBExtraException` (\(DBBaseLineConditionIsFalse _) -> throwM . SomeDBExtraException $ conflictError $ "Inconsistent state - attachment is already set")
      Nothing -> dbUpdate . DeleteSigAttachment sid sigattach =<< signatoryActor ctx sl

    Accepted <$> (documentJSONV1 mu True False (Just sl) =<< theDocument)

checkObjectVersionIfProvided :: Kontrakcja m => DocumentID -> m ()
checkObjectVersionIfProvided did = do
    mov <- readField "objectversion"
    case mov of
        Just ov -> dbQuery $ CheckDocumentObjectVersionIs did ov
        Nothing -> return ()
  `catchDBExtraException` (\DocumentObjectVersionDoesNotMatch {} -> throwM . SomeDBExtraException $ conflictError $ "Document object version does not match")

checkObjectVersionIfProvidedAndThrowError ::  (Kontrakcja m) => DocumentID -> APIError -> m ()
checkObjectVersionIfProvidedAndThrowError did err = do
    mov <- readField "objectversion"
    case mov of
        Just ov -> (dbQuery $ CheckDocumentObjectVersionIs did ov)
                      `catchDBExtraException` (\DocumentObjectVersionDoesNotMatch {} -> throwM . SomeDBExtraException $ conflictError $ "Document object version does not match")
        Nothing -> return ()
    throwM . SomeDBExtraException $ err


-- Utils
guardAuthorOrAuthorsAdmin :: (Kontrakcja m,DocumentMonad m) => User -> String -> m ()
guardAuthorOrAuthorsAdmin user forbidenMessage = do
  docUserID <- apiGuardJustM (serverError "No author found") $ ((maybesignatory =<<) . getAuthorSigLink) <$> theDocument
  docUser   <- apiGuardJustM (serverError "No user found for author") $ dbQuery $ GetUserByIDIncludeDeleted docUserID
  let hasPermission = (docUserID == userid user) ||
                          ((usergroupid docUser == usergroupid user)
                            && (useriscompanyadmin user))
  when (not hasPermission) $
    throwM . SomeDBExtraException $ forbidden forbidenMessage

getMagicHashAndUserForSignatoryAction :: (Kontrakcja m) =>  DocumentID -> SignatoryLinkID -> m (MagicHash,Maybe User)
getMagicHashAndUserForSignatoryAction did sid = do
    mh' <- dbQuery $ GetDocumentSessionToken sid
    case mh' of
      Just mh'' ->  return (mh'',Nothing)
      Nothing -> do
        (user, _ , _) <- getAPIUser APIPersonal
        logInfo "Logging user" $ logObject_ user
        mh'' <- getMagicHashForDocumentSignatoryWithUser  did sid user
        case mh'' of
          Nothing -> throwM . SomeDBExtraException $ serverError "Can't perform this action. Not authorized."
          Just mh''' -> return (mh''',Just $ user)

-- Helper type that represents ~field value, but without file reference - and only with file content. Used only locally.
data FieldTmpValue = StringFTV String
  | BoolFTV Bool
  | FileFTV BS.ByteString
    deriving (Eq, Ord, Show)

getValidPin :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> [(FieldIdentity, FieldTmpValue)] -> m (Maybe String)
getValidPin slid fields = do
  pin <- apiGuardJustM (badInput "Pin not provided or invalid.") $ getField "pin"
  slidPhone <- getMobile <$> fromJust . getSigLinkFor slid <$> theDocument
  phone <- case (not $ null slidPhone, lookup MobileFI fields) of
                (True, _) -> return slidPhone
                (False, Just (StringFTV v)) -> return v
                (False, _) -> throwM . SomeDBExtraException $ badInput "Phone number not provided by author, you need to provide it"
  pin' <- dbQuery $ GetSignatoryPin SMSPinToSign slid phone
  if (pin == pin')
    then return $ Just pin
    else return $ Nothing

getFieldForSigning ::(Kontrakcja m) => m [(FieldIdentity, FieldTmpValue)]
getFieldForSigning = do
  eFieldsJSON <- getFieldJSON "fields"
  case eFieldsJSON of
    Nothing -> throwM . SomeDBExtraException $ serverError "No fields description provided or fields description is not a valid JSON array"
    Just fieldsJSON -> do
      let mvalues = flip ($) fieldsJSON $ fromJSValueCustomMany $ do
            mfi <- fromJSValue
            mval <- fromJSValueField "value"
            return $ case (mfi, mval) of
              -- omg, this special case for empty value is such bullshit.
              (Just fi@(CheckboxFI _), Just "")  -> Just (fi, BoolFTV $ False)
              (Just fi@(CheckboxFI _), Just _ )  -> Just (fi, BoolFTV $ True)
              (Just fi@(SignatureFI _), Just "")  -> Just (fi, FileFTV "")
              (Just fi@(SignatureFI _), Just val) -> case (snd <$> RFC2397.decode (BS.pack val)) of
                Just bv -> Just (fi, FileFTV bv)
                _ -> Nothing
              (Just fi, Just val) -> Just (fi, StringFTV val)
              _ -> Nothing
      case mvalues of
        Nothing -> throwM . SomeDBExtraException $ serverError "Fields description json has invalid format"
        Just values -> return values


fieldsToFieldsWithFiles :: (Kontrakcja m)
                           => [(FieldIdentity,FieldTmpValue)]
                           -> m ([(FieldIdentity,FieldValue)],[(FileID,BS.ByteString)])
fieldsToFieldsWithFiles [] = return ([],[])
fieldsToFieldsWithFiles (field:fields) = do
  (changeFields,files') <- fieldsToFieldsWithFiles fields
  case field of
    (fi,StringFTV s) -> return ((fi,StringFV s):changeFields,files')
    (fi,BoolFTV b)   -> return ((fi,BoolFV b):changeFields,files')
    (fi,FileFTV bs)  -> if (BS.null bs)
                          then return $ ((fi,FileFV Nothing):changeFields,files')
                          else do
                            fileid <- saveNewFile "signature.png" bs
                            return $ ((fi,FileFV (Just fileid)):changeFields,(fileid,bs):files')

-- Mandatory attachments parameters
getAcceptedAuthorAttachments :: (Kontrakcja m) => m [FileID]
getAcceptedAuthorAttachments = do
  eAttachmentJSON <- getFieldJSON "accepted_author_attachments"
  case eAttachmentJSON of
    Nothing -> return [] -- Backward compatibility. This field was not required
    Just attachmentJSON -> do
      case (fromJSValueCustomMany ((join . fmap maybeRead) <$> fromJSValueM) attachmentJSON) of
        Nothing -> throwM . SomeDBExtraException $ badInput "accepted_author_attachments is not a valid list of ids"
        Just values -> return values


allRequiredAuthorAttachmentsAreAccepted :: (Kontrakcja m, DocumentMonad m) => [FileID] -> m Bool
allRequiredAuthorAttachmentsAreAccepted acceptedAttachments = allRequiredAttachmentsAreOnList acceptedAttachments <$> theDocument

guardThatDocumentIsReadableBySignatories :: Kontrakcja m => Document -> m ()
guardThatDocumentIsReadableBySignatories doc = do
  unless (isAccessibleBySignatories doc) $ throwM $ SomeDBExtraException $
    forbidden $ "The document has expired or has been withdrawn. (status: "
                ++ show (documentstatus doc) ++ ")"
