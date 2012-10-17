module Doc.API (
    documentAPI
  , apiCallCreateFromFile      -- Exported for tests
  , apiCallCreateFromTemplate  -- Exported for tests
  , apiCallGet                -- Exported for tests
  ) where

import Control.Monad.Trans.Maybe
import Happstack.StaticRouting
import Text.JSON hiding (Ok)
import qualified Text.JSON as J
import KontraMonad
import Happstack.Server.Types
import Routing
import Doc.APIVersion (APIVersion(..))
import Doc.DocStateQuery
import Doc.DocStateData
import Doc.Model
import Doc.JSON
import Doc.Tokens.Model
import Control.Applicative
import Control.Logic
import Control.Monad.Trans
import Happstack.Fields
import Utils.String
import Utils.Read
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

import File.Model
import MagicHash (MagicHash)
import Kontra
import Doc.DocUtils
import User.Model
import Company.Model
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

documentAPI :: Route (KontraPlus Response)
documentAPI = dir "api" $ choice 
  [ dir "frontend" $ versionedAPI Frontend
  , versionedAPI V1 -- Temporary backwards compatibility for clients accessing version-less API
  , dir "v1" $ versionedAPI V1
  ]
 
versionedAPI :: APIVersion -> Route (KontraPlus Response)
versionedAPI _version = choice [

  dir "createfromfile"     $ hPostNoXToken $ toK0 $ apiCallCreateFromFile,
  dir "createfromtemplate" $ hPostNoXToken $ toK1 $ apiCallCreateFromTemplate,
  dir "update"             $ hPostNoXToken $ toK1 $ apiCallUpdate,
  dir "ready"              $ hPostNoXToken $ toK1 $ apiCallReady,
  dir "cancel"             $ hPostNoXToken $ toK1 $ apiCallCancel,
  dir "remind"             $ hPostNoXToken $ toK1 $ apiCallRemind,
  dir "delete"             $ hDelete   $ toK1 $ apiCallDelete,
  dir "get"                $ hGet $ toK1 $ apiCallGet,
  dir "list"               $ hGet $ apiCallList,

  dir "mainfile" $ hPostNoXToken $ toK1 $ documentChangeMainFile,

  dir "document" $ hPostNoXToken $ toK6 $ documentUploadSignatoryAttachment,
  dir "document" $ hDelete       $ toK6 $ documentDeleteSignatoryAttachment
  ]

-- | Clean a filename from a path (could be windows \) to a base name
cleanFileName :: FilePath -> String
cleanFileName = takeBaseName . String.replace "\\" "/"

{- New API calls-}
apiCallCreateFromFile :: Kontrakcja m => m Response
apiCallCreateFromFile = api $ do
  ctx <- getContext
  (user, actor, _) <- getAPIUser APIDocCreate
  mcompany <- case usercompany user of
    Just companyid -> Just <$> (apiGuardL' $ dbQuery $ GetCompany companyid)
    Nothing -> return Nothing
  dtype <- lift $ fromMaybe (Contract) <$> readField "type"
  isTpl <- lift $ isFieldSet "template"
  let doctype = (Template <| isTpl |> Signable) dtype
  minput <- lift $ getDataFn' (lookInput "file")
  (mfile, title) <- case minput of
    Nothing -> return (Nothing, ("New document " <| isTpl |> "New template ") ++ formatMinutesTimeUTC (ctxtime ctx))
    Just (Input _ Nothing _) -> throwError $ badInput "Missing file"
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
            Left (LiveDocxIOError e) -> throwError $ serverError $ show e
            Left (LiveDocxSoapError s)-> throwError $ serverError s
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
  Just doc <- dbUpdate $ NewDocument user mcompany title doctype 1 actor
  case mfile of
    Nothing -> return ()
    Just file -> do
      True <- dbUpdate $ AttachFile (documentid doc) (fileid file) actor
      return ()
  let doc' = doc{ documentfile = fileid `fmap` mfile }
  _ <- lift $ addDocumentCreateStatEvents doc "web"
  Created <$> documentJSON True True Nothing Nothing doc'


apiCallCreateFromTemplate :: Kontrakcja m => DocumentID -> m Response
apiCallCreateFromTemplate did =  api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  mcompany <- case usercompany user of
    Just companyid -> Just <$> (apiGuardL' $ dbQuery $ GetCompany companyid)
    Nothing -> return Nothing
  template <- apiGuardJustM (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink template
  auser <- apiGuardJustM (serverError "No user found") $ dbQuery $ GetUserByID auid
  let haspermission = (userid auser == userid user) ||
                          ((usercompany auser == usercompany user && (isJust $ usercompany user)) &&  isDocumentShared template)
  enewdoc <- if (isTemplate template && haspermission)
                    then dbUpdate $ SignableFromDocumentIDWithUpdatedAuthor user mcompany did actor
                    else throwError $ serverError "Id did not matched template or you do not have right to access document"
  case enewdoc of
      Just newdoc -> do
          _ <- lift $ addDocumentCreateStatEvents newdoc "web"
          Log.debug $ show "Document created from template"
          Created <$> documentJSON True  True Nothing Nothing newdoc
      Nothing -> throwError $ serverError "Create document from template failed"

    

apiCallUpdate :: Kontrakcja m => DocumentID -> m Response
apiCallUpdate did = api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwError $ serverError "Permission problem. Not an author."
  jsons <- apiGuardL (badInput "The MIME part 'json' must exist and must be a JSON.") $ getDataFn' (look "json")
  json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $ case decode jsons of
                                                                               J.Ok js -> Just js
                                                                               _ -> Nothing
  draftData   <-apiGuardJustM (badInput "Given JSON does not represent valid draft data.") $ return $ fromJSValue json
  newdocument <-  apiGuardL (serverError "Could not apply draft data") $ applyDraftDataToDocument doc draftData actor
  triggerAPICallbackIfThereIsOne newdocument
  Ok <$> documentJSON True True Nothing Nothing newdocument

apiCallReady :: Kontrakcja m => DocumentID -> m Response
apiCallReady did =  api $ do
  (user, actor, _) <- getAPIUser APIDocCreate
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwError $ serverError "Permission problem. Not an author."
  when (not $ all (isGood . asValidEmail . getEmail) (documentsignatorylinks doc)) $ do
        throwError $ serverError "Some signatories don't have a valid email adress set."
  mndoc <- lift $ authorSendDocument user actor (documentid doc)
  case mndoc of
          Right newdocument -> do
              lift $ postDocumentPreparationChange newdocument "web"
              newdocument' <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
              Accepted <$> documentJSON True True Nothing Nothing newdocument'
          Left reason -> throwError $ serverError $ "Operation failed: " ++ show reason


apiCallCancel :: Kontrakcja m => DocumentID -> m Response
apiCallCancel did =  api $ do
  (user, actor, _) <- getAPIUser APIDocSend
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwError $ serverError "Permission problem. Not an author."
  cancelled <- dbUpdate $ CancelDocument (documentid doc) ManualCancel actor
  if cancelled 
    then do
      newdocument <- apiGuardL (serverError "No document found after cancel") $ dbQuery $ GetDocumentByDocumentID $ did
      lift $ postDocumentCanceledChange newdocument "api"
      newdocument' <- apiGuardL (serverError "No document found after cancel and post actions") $ dbQuery $ GetDocumentByDocumentID $ did
      Accepted <$> documentJSON True True Nothing Nothing newdocument'
    else throwError $ serverError $ "Operation failed"

apiCallRemind :: Kontrakcja m => DocumentID -> m Response
apiCallRemind did =  api $ do
  ctx <- getContext
  (user, actor , _) <- getAPIUser APIDocSend
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  when (documentstatus doc /= Pending || documentdeliverymethod doc /= EmailDelivery) $ do
        throwError $ serverError "Can't send reminder for documents that are not pending or that don't have email delivery type"
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwError $ serverError "Permission problem. Not an author."
  _ <- lift $ sendAllReminderEmails ctx actor user did
  newdocument <- apiGuardL (serverError "No document found after sending reminder") $ dbQuery $ GetDocumentByDocumentID $ did
  Accepted <$> documentJSON True True Nothing Nothing newdocument

apiCallDelete :: Kontrakcja m => DocumentID -> m Response
apiCallDelete did =  api $ do
  ctx <- getContext
  (user, actor, _) <- getAPIUser APIDocSend
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwError $ serverError "Permission problem. Not an author."
  _ <- apiGuardL (serverError "This document cant be deleted. Maybe it's pending?") $ dbUpdate $ ArchiveDocument user did actor
  doc' <- apiGuardL (serverError "No document found after operation") $ dbQuery $ GetDocumentByDocumentID $ did
  _ <- addSignStatDeleteEvent doc' (fromJust $ getSigLinkFor doc' user) (ctxtime ctx)
  case (documentstatus doc') of
       Preparation -> do
         _ <- dbUpdate $ ReallyDeleteDocument user did actor
         when_ (isJust $ getSigLinkFor doc' user) $
             addSignStatPurgeEvent doc' (fromJust $ getSigLinkFor doc' user)  (ctxtime ctx)
       _ -> return ()
  Accepted <$> (runJSONGenT $ return ())



  
apiCallGet :: Kontrakcja m => DocumentID -> m Response
apiCallGet did = api $ do
  ctx <- getContext
  (msignatorylink :: Maybe SignatoryLinkID) <- lift $ readField "signatoryid"
  mmagichashh <- maybe (return Nothing) (dbQuery . GetDocumentSessionToken) msignatorylink
  case (msignatorylink,mmagichashh) of
      (Just slid,Just mh) -> do
         doc <- apiGuardL (serverError "No document found") $  dbQuery $ GetDocumentByDocumentID did
         sl <- apiGuardJustM  (serverError "No document found") $ return $ getMaybeSignatoryLink (doc,slid)
         when (signatorymagichash sl /= mh) $ throwError $ serverError "No document found"
         _ <- dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl)
                         (signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) (getEmail sl) (signatorylinkid sl))
         lift $ switchLocale (getLocale doc)
         Ok <$> documentJSON False False Nothing (Just sl) doc
      _ -> do        
        (user, _actor, external) <- getAPIUser APIDocCheck
        doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
        let msiglink = getMaybeSignatoryLink (doc,user)
        when_ (isJust $ msiglink) $ do
            let sl = fromJust msiglink
            dbUpdate $ MarkDocumentSeen did (signatorylinkid sl) (signatorymagichash sl)
                 (signatoryActor (ctxtime ctx) (ctxipnumber ctx) (maybesignatory sl) (getEmail sl) (signatorylinkid sl))
                 
        let macmp = join $ maybecompany <$> getAuthorSigLink doc
        mauser <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                       Just auid -> dbQuery $ GetUserByID auid
                       _ -> return Nothing
                       
        pq <- if ((userid <$> mauser) == Just (userid user)) 
                then dbQuery $ GetPadQueue $ userid user
                else return Nothing
        let haspermission = (isJust msiglink)
                         || (isJust macmp && macmp == usercompany user && (useriscompanyadmin user || isDocumentShared doc))
                         || (isJust mauser && usercompany (fromJust mauser) == usercompany user && (useriscompanyadmin user || isDocumentShared doc))
        if (haspermission)
          then Ok <$> documentJSON external ((userid <$> mauser) == (Just $ userid user)) pq msiglink doc
          else throwError $ serverError "You do not have right to access document"

apiCallList :: Kontrakcja m => m Response
apiCallList = api $ do
  (user, _actor, _) <- getAPIUser APIDocCheck
  ctx <- getContext
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = Just user});
  res <- lift $ jsonDocumentsList
  modifyContext (\ctx' -> ctx' {ctxmaybeuser = ctxmaybeuser ctx});
  return res

  
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
  templateinput <- lift $ getDataFn' (look "template")

  mfileid <- case (fileinput, templateinput) of
            (Just (Input contentspec (Just filename') _contentType), _) -> do
              content1 <- case contentspec of
                Left filepath -> liftIO $ BSL.readFile filepath
                Right content -> return content

              -- we need to downgrade the PDF to 1.4 that has uncompressed structure
              -- we use gs to do that of course
              content <- apiGuardL (badInput "PDF precheck failed.") $ liftIO $ preCheckPDF (concatChunks content1)
              let filename = cleanFileName filename'

              Just . fileid <$> (dbUpdate $ NewFile filename content)
            (_, Just templateids) -> do
              templateid <- apiGuard (badInput $ "Template id in bad format: " ++ templateids) $ maybeRead templateids
              temp <- apiGuardL' $ getDocByDocID templateid
              Just <$> (apiGuard (badInput "No template found for that id (or you don't have permissions).") $ documentfile temp)
            _ -> return Nothing
  case mfileid of
    Just  fileid -> apiGuardL' $ dbUpdate $ AttachFile docid fileid aa
    Nothing -> apiGuardL' $ dbUpdate $ DetachFile docid aa

  
  return ()


--documentView :: (Kontrakcja m) => DocumentID -> m Response
--documentView (_ :: DocumentID) = api $  undefined
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
       _ -> throwError $ badInput "The signatorylinkid or magichash were missing."

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
    True <- dbUpdate $ SaveSigAttachment (documentid doc) sid aname (fileid file) actor
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
    True <- dbUpdate $ DeleteSigAttachment (documentid doc) sid fileid (signatoryActor ctxtime ctxipnumber muid email sid)
    Just newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
    return newdoc

  -- let's dig the attachment out again
  sigattach' <- apiGuard' $ getSignatoryAttachment d sid aname

  return $ jsonSigAttachmentWithFile sigattach' Nothing

