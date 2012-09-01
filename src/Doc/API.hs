module Doc.API (documentAPI) where

import Control.Monad.Trans.Maybe
import Happstack.StaticRouting
import Text.JSON hiding (Ok)
import qualified Text.JSON as J
import KontraMonad
import Happstack.Server.Types
import Routing
import Doc.DocStateQuery
import Doc.DocStateData
import Doc.Model
import Doc.JSON
import Control.Applicative
import Control.Logic
import Control.Monad.Trans
import Happstack.Fields
import Utils.String
import Utils.Read
import System.FilePath
import Data.Maybe

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

documentAPI :: Route (KontraPlus Response)
documentAPI = choice [

  dir "api" $ dir "createfromfile" $ hPostNoXToken $ toK0 $ apiCallCreateFromFile,
  dir "api" $ dir "createfromtemplate" $ hPostNoXToken $ toK1 $ apiCallCreateFromTemplate,
  dir "api" $ dir "update" $ hPostNoXToken $ toK1 $ apiCallUpdate,
  dir "api" $ dir "ready" $ hPostNoXToken $ toK1 $ apiCallReady,
  dir "api" $ dir "get" $ hGet $ toK1 $ apiCallGet,


  dir "api" $ dir "document" $ hPostNoXToken $ toK0 $ documentNew,

  -- /api/mainfile/{docid} ==> Change main file
  dir "api" $ dir "mainfile" $ hPostNoXToken $ toK1 $ documentChangeMainFile,

  dir "api" $ dir "document" $ hPostNoXToken $ toK6 $ documentUploadSignatoryAttachment,
  dir "api" $ dir "document" $ hDelete       $ toK6 $ documentDeleteSignatoryAttachment
  ]


{- New API calls-}
apiCallCreateFromFile :: Kontrakcja m => m Response
apiCallCreateFromFile = api $ do
  ctx <- getContext
  (user, actor) <- getAPIUser
  mcompany <- case usercompany user of
    Just companyid -> Just <$> (apiGuardL' $ dbQuery $ GetCompany companyid)
    Nothing -> return Nothing
  Input contentspec (Just filename') _contentType <- apiGuardL (badInput "The main file of the document must be attached in the MIME part 'file'.") $ getDataFn' (lookInput "file")
  let filename = basename filename'
  pdfcontent <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  content <- apiGuardL (badInput "The PDF is invalid.") $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks pdfcontent)
  file <- dbUpdate $ NewFile filename content
  Just doc <- dbUpdate $ NewDocument user mcompany filename (Signable Contract) 1 actor
  True <- dbUpdate $ AttachFile (documentid doc) (fileid file) actor
  Created <$> documentJSON True True Nothing Nothing doc
  

apiCallCreateFromTemplate :: Kontrakcja m => DocumentID -> m Response
apiCallCreateFromTemplate did =  api $ do
  (user, actor) <- getAPIUser
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
  (user, actor) <- getAPIUser
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
  Ok <$> documentJSON True True Nothing Nothing newdocument

apiCallReady :: Kontrakcja m => DocumentID -> m Response
apiCallReady did =  api $ do
  (user, _actor) <- getAPIUser
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  when (not $ (auid == userid user)) $ do
        throwError $ serverError "Permission problem. Not an author."
  mndoc <- lift $ authorSendDocument (documentid doc)
  case mndoc of
          Right newdocument -> do
              lift $ postDocumentPreparationChange newdocument "web"
              newdocument' <- apiGuardL (serverError "No document found") $ getDocByDocID $ did
              Accepted <$> documentJSON True True Nothing Nothing newdocument'
          Left reason -> throwError $ serverError $ "Operation failed: " ++ show reason

  
apiCallGet :: Kontrakcja m => DocumentID -> m Response
apiCallGet did = api $ do
  (user, _actor) <- getAPIUser
  doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
  auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
  auser <- apiGuardJustM (serverError "No user found") $ dbQuery $ GetUserByID auid
  let haspermission = (userid auser == userid user) || ((usercompany auser == usercompany user && (isJust $ usercompany user)) &&  isDocumentShared doc)
  if (haspermission)
      then Ok <$> documentJSON True True Nothing Nothing doc
      else throwError $ serverError "You do not have right to access document"


{- Old API calls-}














  
documentNew :: Kontrakcja m => m Response
documentNew = api $ do
  (user, actor) <- getAPIUser

  mcompany <- case usercompany user of
    Just companyid -> Just <$> (apiGuardL' $ dbQuery $ GetCompany companyid)
    Nothing -> return Nothing

  jsons <- apiGuardL (badInput "The MIME part 'json' must exist and must be a JSON.") $ getDataFn' (look "json")
  json <- apiGuard (badInput "The MIME part 'json' must be a valid JSON.") $ case decode jsons of
                                                                               J.Ok js -> Just js
                                                                               _     -> Nothing

  dcr <- either (throwError . badInput) return $ dcrFromJSON json
  
  let doctype = dcrType dcr
  -- pdf exists
  Input contentspec (Just filename') _contentType <- apiGuardL (badInput "The main file of the document must be attached in the MIME part 'file'.") $ getDataFn' (lookInput "file")

  let filename = takeBaseName filename'

  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content

  -- might need to convert the content to pdf
  Context{ctxlivedocxconf} <- getContext
  let mformat = getFileFormatForConversion filename'
  pdfcontent <- case mformat of
    Nothing -> return content1
    Just format -> do
      eres <- liftIO $ convertToPDF ctxlivedocxconf (BS.concat $ BSL.toChunks content1) format
      case eres of
        Left (LiveDocxIOError e) -> throwError $ serverError $ show e
        Left (LiveDocxSoapError s)-> throwError $ serverError s
        Right res -> return $ BSL.fromChunks [res]

  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext

  content <- apiGuardL (badInput "The PDF is invalid.") $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks pdfcontent)

  doc <- apiGuardL (serverError "documentNew failed") . runMaybeT $ do
    file <- dbUpdate $ NewFile filename content
    Just d1 <- dbUpdate $ NewDocument user mcompany filename doctype 1 actor
    True <- dbUpdate $ AttachFile (documentid d1) (fileid file) actor
    -- we really need to check SignatoryDetails before adding them
    let sigs = map (\ir -> (SignatoryDetails {
                              signatorysignorder = SignOrder (toInteger $ fromMaybe 1 $ irSignOrder ir),
                              signatoryfields = irData ir
                              },
                            irRole ir,
                            irAttachments ir,
                            Nothing,
                            Nothing))
              (dcrInvolved dcr)

    when (not $ null sigs) $ do
      True <- dbUpdate $ ResetSignatoryDetails2 (documentid d1) sigs actor
      return ()

    Just d3 <- dbQuery $ GetDocumentByDocumentID $ documentid d1
    _ <- addDocumentCreateStatEvents d3 "web"
    return d3

  return $ Created $ jsonDocumentForAuthor doc (ctxhostpart ctx)

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

  fileid <- case (fileinput, templateinput) of
            (Just (Input contentspec (Just filename') _contentType), _) -> do
              content1 <- case contentspec of
                Left filepath -> liftIO $ BSL.readFile filepath
                Right content -> return content

              -- we need to downgrade the PDF to 1.4 that has uncompressed structure
              -- we use gs to do that of course
              content <- apiGuardL (badInput "PDF precheck failed.") $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks content1)
              let filename = takeBaseName filename'

              fileid <$> (dbUpdate $ NewFile filename content)
            (_, Just templateids) -> do
              templateid <- apiGuard (badInput $ "Template id in bad format: " ++ templateids) $ maybeRead templateids
              temp <- apiGuardL' $ getDocByDocID templateid
              apiGuard (badInput "No template found for that id (or you don't have permissions).") $ listToMaybe $ documentfiles temp
            _ -> throwError $ badInput "This API call requires one of 'file' or 'template' POST parameters."

  _ <- apiGuardL' $ dbUpdate $ AttachFile docid fileid aa
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
  mmagichash <- lift $ maybe (return Nothing) getMagicHashFromContext msignatorylink
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

  content <- apiGuardL (badInput "The PDF was invalid.") $ liftIO $ preCheckPDF (ctxgscmd ctx) (concatChunks content1)

  file <- dbUpdate $ NewFile (takeBaseName filename) content
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

