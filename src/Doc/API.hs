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
import Control.Applicative
--import Control.Monad
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
import Doc.DocUtils
import User.Model
import Company.Model
import Happstack.Server.Monads
import API.Monad
import Control.Monad.Error
import qualified AppLogger as Log
import Stats.Control

documentAPI :: Route (Kontra Response)
documentAPI = choice [
  dir "api" $ dir "document" $ hGet          $ toK0 $ documentList,
  dir "api" $ dir "document" $ hPostNoXToken $ toK0 $ documentNew,
--  dir "api" $ dir "document" $ hGet          $ toK1 $ documentView,
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
documentList = api $ do
  docs <- apiGuardL getDocsByLoggedInUser
  jdocs <- lift $ mapM jsonDocumentAndFiles docs
  return $ showJSON jdocs

-- this one must be standard post with post params because it needs to
-- be posted from a browser form; we will have a better one for more
-- capable clients that follows the api standards
documentNew :: Kontrakcja m => m Response
documentNew = api $ do
  user <- getAPIUser
  mcompany <- case usercompany user of
    Nothing -> return Nothing
    Just cid -> do
      a <- apiGuardL $ runDBQuery $ GetCompany cid
      return $ Just a
  
  doctypes <- apiGuardL' BadInput $ getDataFn' (look "type")
  
  doctypei <- apiGuard' BadInput $ maybeRead doctypes
  
  doctype <- apiGuard' BadInput $ toSafeEnumInt doctypei
  
  -- pdf exists  
  (Input contentspec (Just filename') _contentType) <- apiGuardL' BadInput $ getDataFn' (lookInput "file")
  
  let filename = (BS.fromString $ basename filename')
      
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  let now = ctxtime ctx
  
  d1 <- apiGuardL $ doc_update $ NewDocument user mcompany filename doctype now
  
  content <- liftIO $ preprocessPDF ctx (concatChunks content1) (documentid d1)
  file <- lift $ runDB $ dbUpdate $ NewFile filename content

  d2 <- apiGuardL $ doc_update $ AttachFile (documentid d1) (fileid file) now
  _ <- lift $ addDocumentCreateStatEvents d2
  return $ Created $ jsonDocumentForAuthor d2

documentChangeMetadata :: Kontrakcja m => DocumentID -> MetadataResource -> m Response
documentChangeMetadata docid _ = api $ do
  user <- getAPIUser  
  doc <- apiGuardL $ doc_query $ GetDocumentByDocumentID docid
  
  asl <- apiGuard $ getAuthorSigLink doc
  
  apiGuard' Forbidden (Just (userid user) == maybesignatory asl)
    
  rq <- lift askRq
    
  bdy <- apiGuardL $ liftIO $ takeRequestBody rq
  let jstring = BS.toString $ concatChunks $ unBody bdy
  
  json <- apiGuard $ decode jstring
  
  ctx <- getContext
  let now = ctxtime ctx
  d <- case jsget "title" json of
    Left _ -> return doc
    Right (JSString s) ->
      apiGuardL $ doc_update $ SetDocumentTitle docid (BS.fromString $ fromJSString s) now
    Right _ -> throwError BadInput
      
  return $ jsonDocumentMetadata d

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
  mmagichash <- lift $ readField "magichash"
  case (msignatorylink, mmagichash) of
       (Just sl, Just mh) -> return (sl,mh)
       _ -> throwError BadInput
  
documentUploadSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentUploadSignatoryAttachment did _ sid _ aname _ = api $ do
  Log.debug $ "sigattachment ajax"
  (slid, magichash) <- getSigLinkID
  doc <- apiGuardL $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  email <- apiGuard $ getEmail <$> getSigLinkFor doc sid
  
  sigattach <- apiGuard' Forbidden $ getSignatoryAttachment email (BS.fromString aname) doc
  
  -- attachment must have no file
  apiGuard' ActionNotAvailable (isNothing $ signatoryattachmentfile sigattach)
  
  -- pdf exists in input param "file"
  (Input contentspec (Just filename) _contentType) <- apiGuardL' BadInput $ getDataFn' (lookInput "file")
  
  content1 <- case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  
  -- we need to downgrade the PDF to 1.4 that has uncompressed structure
  -- we use gs to do that of course
  ctx <- getContext
  content <- liftIO $ preprocessPDF ctx (concatChunks content1) (documentid doc)
  
  file <- lift $ runDB $ dbUpdate $ NewFile (BS.fromString $ basename filename) content
  
  d <- apiGuardL $ doc_update $ SaveSigAttachment (documentid doc) (BS.fromString aname) email (fileid file)
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) d
  
  return $ Created $ jsonSigAttachmentWithFile sigattach' (Just file)

documentDeleteSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentDeleteSignatoryAttachment did _ sid _ aname _ = api $ do
  (slid, magichash) <- getSigLinkID
  doc <- apiGuardL $ getDocByDocIDSigLinkIDAndMagicHash did slid magichash
  
  email <- apiGuard $ getEmail <$> getSigLinkFor doc sid
  
  -- sigattachexists
  sigattach <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) doc

  -- attachment must have a file
  fileid <- apiGuard' ActionNotAvailable $ signatoryattachmentfile sigattach

  d <- apiGuardL $ doc_update $ DeleteSigAttachment (documentid doc) email fileid
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) d
  
  return $ jsonSigAttachmentWithFile sigattach' Nothing

  
-- helpers

jsonDocumentAndFiles :: Kontrakcja m => Document -> m JSValue
jsonDocumentAndFiles doc = do
  --files <- getFilesByStatus doc
  return $ jsonDocumentForSignatory doc
  
