module Doc.API (documentAPI) where

import Happstack.StaticRouting
import Text.JSON
import KontraMonad
import Util.JSON
import Happstack.Server.Types
import Routing
import Doc.DocStateQuery
import Doc.DocStateData
import Doc.Transitory
import Doc.JSON
import Control.Monad
import Control.Monad.Trans
import Misc
import Data.Maybe
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Lazy as BSL
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Happstack.Server.RqData
import Doc.DocStorage
import DB.Classes
import File.Model
import DB.Types
import Kontra
import Happstack.Server (FromReqURI(..))
import Doc.DocUtils
import User.Model
import Company.Model
import Happstack.Server.Monads
import API.Monad

documentAPI :: Route (Kontra Response)
documentAPI = choice [
  dir "api" $ dir "document" $ hGet          $ toK0 $ documentList,
  dir "api" $ dir "document" $ hPostNoXToken $ toK0 $ documentNew,
  dir "api" $ dir "document" $ hGet          $ toK1 $ documentView,
  dir "api" $ dir "document" $ hPostNoXToken $ toK6 $ documentUploadSignatoryAttachment,
  dir "api" $ dir "document" $ hDelete       $ toK6 $ documentDeleteSignatoryAttachment,
  dir "api" $ dir "document" $ hPostNoXToken $ toK2 $ documentChangeMetadata
  --dir "api" $ dir "login"    $ hPostNoXToken $ toK0 $ apiLogin
  ]

{-              
_apiLogin :: Kontrakcja m => m Response
_apiLogin = api $ do
  memail  <- getDataFn' (look "email")
  mpasswd <- getDataFn' (look "password")
  case (memail, mpasswd) of
    (Just email, Just passwd) -> do
      -- check the user things here
      maybeuser <- lift $ runDBQuery $ GetUserByEmail Nothing (Email $ BS.fromString email)
      case maybeuser of
        Just user@User{userpassword}
          | verifyPassword userpassword (BS.fromString passwd) -> do
            muuser <- runDBQuery $ GetUserByID (userid user)
            lift $ logUserToContext muuser
            apiOK jsempty
        _ -> apiForbidden
    _ -> apiBadInput
-}              

-- | Return a list of documents the logged in user can see
documentList :: Kontrakcja m => m Response
documentList = api $
  apiGuardL getDocsByLoggedInUser >>=
  lift . mapM jsonDocumentAndFiles >>=
  apiOK . showJSON

documentNew :: Kontrakcja m => m Response
documentNew = api $ do
  user <- getAPIUser
  mcompany <- case usercompany user of
    Nothing -> return Nothing
    Just cid -> do
      a <- apiGuardL $ runDBQuery $ GetCompany cid
      return $ Just a
  
  mdoctypes <- lift $ getDataFn' (look "type")
  when (isNothing mdoctypes)
    apiBadInput
  let Just doctypes = mdoctypes
  let mdoctypei = maybeRead doctypes
  when (isNothing mdoctypei)
    apiBadInput
  let Just doctypei = mdoctypei
  let mdoctype = toSafeEnumInt doctypei
  when (isNothing mdoctype)
    apiBadInput
  let Just doctype = mdoctype
  
  -- pdf exists  
  mfile <- lift $ getDataFn' (lookInput "file")
  when (isNothing mfile) $
    apiBadInput
  let Just (Input contentspec (Just filename') _contentType) = mfile
  let filename = (BS.fromString $ basename filename')
      
  content1 <- case contentspec of
    Left filepath -> lift $ liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- lift $ getContext
  let now = ctxtime ctx
  
  d1 <- apiGuardL $ doc_update $ NewDocument user mcompany filename doctype now
  
  content <- lift $ liftIO $ preprocessPDF ctx (concatChunks content1) (documentid d1)
  file <- lift $ runDB $ dbUpdate $ NewFile filename content

  d2 <- apiGuardL $ doc_update $ AttachFile (documentid d1) (fileid file) now
  
  apiCreated $ jsonDocumentForAuthor d2

documentChangeMetadata :: Kontrakcja m => DocumentID -> MetadataResource -> m Response
documentChangeMetadata docid _ = api $ do
  user <- getAPIUser  
  doc <- apiGuardL $ doc_query $ GetDocumentByDocumentID docid
  
  asl <- apiGuard $ getAuthorSigLink doc
  
  when (Just (userid user) /= maybesignatory asl)
    apiForbidden
    
  rq <- lift askRq
    
  bdy <- apiGuardL $ liftIO $ takeRequestBody rq
  let jstring = BS.toString $ concatChunks $ unBody bdy
  
  json <- case decode jstring of
    Error _ -> apiBadInput
    Ok a -> return a
    
  ctx <- lift $ getContext
  let now = ctxtime ctx
  d <- case jsget "title" json of
    Left _ -> return doc
    Right (JSString s) ->
      apiGuardL $ doc_update $ SetDocumentTitle docid (BS.fromString $ fromJSString s) now
    Right _ -> apiBadInput
      
  apiOK $ jsonDocumentMetadata d

documentView :: (Kontrakcja m) => DocumentID -> m Response
documentView (_ :: DocumentID) = api $  undefined


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
  mmagichash <- lift $ readField "magichash"
  case (msignatorylink, mmagichash) of
       (Just sl, Just mh) -> return (sl,mh)
       _ -> apiBadInput
  
documentUploadSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentUploadSignatoryAttachment did _ sid _ aname _ = api $ do
  (slid, magichash) <- getSigLinkID
  -- doc exists
  -- doc magichash/siglink match
  doc <- apiGuardL $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  
  -- siglink exists
  siglink :: SignatoryLink <- apiGuard $ getSigLinkFor doc sid
  let email = getEmail siglink
  
  -- sigattachexists
  let msigattach = getSignatoryAttachment email (BS.fromString aname) doc

  when (isNothing msigattach)
    apiForbidden

  -- attachment must have no file
  when (isJust $ signatoryattachmentfile $ fromJust msigattach)
    apiActionNotAvailable

  -- pdf exists  
  mfile <- lift $ getDataFn' (lookInput "file")
  when (isNothing mfile) $
    apiBadInput
  let Just (Input contentspec (Just filename) _contentType) = mfile

  content1 <- case contentspec of
    Left filepath -> lift $ liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- lift $ getContext
  content <- lift $ liftIO $ preprocessPDF ctx (concatChunks content1) (documentid doc)
  
  file <- lift $ runDB $ dbUpdate $ NewFile (BS.fromString $ basename filename) content
  d <- apiGuardL $ doc_update $ SaveSigAttachment (documentid doc) (BS.fromString aname) email (fileid file)
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) d
  
  apiCreated $ jsonSigAttachmentWithFile sigattach' (Just file)

documentDeleteSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentDeleteSignatoryAttachment did _ sid _ aname _ = api $ do
  (slid, magichash) <- getSigLinkID
  -- doc exists
  -- doc magichash/siglink match
  doc <- apiGuardL $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  
  -- siglink exists
  siglink :: SignatoryLink <- apiGuard $ getSigLinkFor doc sid
  let email = getEmail siglink
  
  -- sigattachexists
  let msigattach = getSignatoryAttachment email (BS.fromString aname) doc
  when (isNothing msigattach)
    apiForbidden

  let Just sigattach = msigattach

  -- attachment must have a file
  when (isNothing $ signatoryattachmentfile sigattach)
    apiActionNotAvailable

  let Just fileid = signatoryattachmentfile sigattach

  d <- apiGuardL $ doc_update $ DeleteSigAttachment (documentid doc) email fileid
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) d
  
  apiOK $ jsonSigAttachmentWithFile sigattach' Nothing

  
-- helpers

jsonDocumentAndFiles :: Kontrakcja m => Document -> m JSValue
jsonDocumentAndFiles doc = do
  --files <- getFilesByStatus doc
  return $ jsonDocumentForSignatory doc
  
