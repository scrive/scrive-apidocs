module Doc.API where

import Happstack.StaticRouting
import DBError
import Text.JSON
import KontraMonad
import Util.JSON
import Happstack.Server.Response
import Happstack.Server.Types
import Happstack.State
import Routing
import Doc.DocStateQuery
import Doc.DocStateData
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
import Doc.DocState
import File.Model
import DB.Types
import Kontra
import Happstack.Server (FromReqURI(..))
import Doc.DocUtils

data APIResponse a = Created a
                   | OK a
                   | BadInput
                   | NotLoggedIn
                   | Forbidden
                   | ActionNotAvailable
                   | ServerError

instance Monad APIResponse where
  return = OK
  
  Created x          >>= f = f x
  OK x               >>= f = f x
  BadInput           >>= _ = BadInput 
  NotLoggedIn        >>= _ = NotLoggedIn
  Forbidden          >>= _ = Forbidden 
  ActionNotAvailable >>= _ = ActionNotAvailable 
  ServerError        >>= _ = ServerError 

  fail _ = ServerError
  
newtype (Monad m) => APIMonad m a = AM { runAPIMonad :: m (APIResponse a) }
  
instance Monad m => Monad (APIMonad m) where                                     
  return = AM . return . OK
  
  AM x >>= f = AM $ do
    x' <- x
    case x' of
      Created v          -> runAPIMonad $ f v
      OK v               -> runAPIMonad $ f v
      BadInput           -> return $ BadInput
      NotLoggedIn        -> return $ NotLoggedIn
      Forbidden          -> return $ Forbidden
      ActionNotAvailable -> return $ ActionNotAvailable
      ServerError        -> return $ ServerError
  
apiCreated :: Monad m => a -> APIMonad m a
apiCreated = AM . return . Created

apiOK :: Monad m => a -> APIMonad m a
apiOK = AM . return . OK

apiBadInput :: Monad m => APIMonad m a
apiBadInput = AM $ return BadInput

apiNotLoggedIn :: Monad m => APIMonad m a
apiNotLoggedIn = AM $ return NotLoggedIn

apiForbidden :: Monad m => APIMonad m a
apiForbidden = AM $ return Forbidden

apiActionNotAvailable :: Monad m => APIMonad m a
apiActionNotAvailable = AM $ return ActionNotAvailable

apiServerError :: Monad m => APIMonad m a
apiServerError = AM $ return ServerError

instance MonadTrans APIMonad where
  lift = AM . (liftM OK)

jsonError :: Either String JSValue
jsonError = (Right jsempty) >>=
            jsset "status" "error"
            
setJsonType :: (Monad m) => Response -> m Response
setJsonType r = return $ setHeader "Content-Type" "application/json" r

-- | convert the return type to the appropriate response
-- This defines the possible outputs of the api.
api :: (Kontrakcja m) => APIMonad m JSValue -> m Response
api acc = do
  r <- runAPIMonad acc
  case r of 
    BadInput ->
      badRequest (toResponse $ encode $ fromRight $
                  jsonError >>=
                  jsset "message" "The input sent was invalid. Please try again.") >>=
      setJsonType
    Forbidden ->
      forbidden (toResponse $ encode $ fromRight $ 
                 jsonError >>=
                 jsset "message" "The resource you are trying to access does not exist or you do not have permission to access it.") >>=
      setJsonType
    NotLoggedIn -> 
      unauthorized (toResponse $ encode $ fromRight $
                    jsonError >>=
                    jsset "message" "You must identify yourself to access this resource." >>=
                    jsset "url" "http://scrive.com/api/user/login") >>=
      setJsonType
    ServerError ->
      internalServerError (toResponse $ encode $ fromRight $
                           jsonError >>=
                           jsset "message" "We're sorry. The server just does not know what to do.") >>=
      setJsonType
    ActionNotAvailable ->
      forbidden (toResponse $ toResponse $ encode $ fromRight $
                 jsonError >>=
                 jsset "message" "The action you requested is not available on this resource.") >>=
      setJsonType
    OK v -> ok $ toResponse $ encode v
    Created v -> 
      (ok $ toResponse $ encode v) >>=
      setRsCode 201

{-
apiRightM :: Kontrakcja m => m (Either DBError a) -> APIMonad m a
apiRightM a = do
  a' <- a
  case a' of
    Left e -> 
-}

documentApi :: Route (Kontra Response)
documentApi = choice [
  dir "api" $ dir "document" $ hGet          $ toK0 $ documentList,
  dir "api" $ dir "document" $ hPostNoXToken $ toK0 $ documentNew,
  dir "api" $ dir "document" $ hGet          $ toK1 $ documentView,
  dir "api" $ dir "document" $ hPostNoXToken $ toK6 $ documentUploadSignatoryAttachment,
  dir "api" $ dir "document" $ hDelete       $ toK6 $ documentDeleteSignatoryAttachment
  ]
              
apiResponseFromDBError :: DBError -> APIResponse a
apiResponseFromDBError = undefined

apiMonadFromAPIResponse :: Monad m => APIResponse a -> APIMonad m a
apiMonadFromAPIResponse = AM . return

class Monad m => APIGuard m b a where
  apiGuard  :: a   -> APIMonad m b
  apiGuardL :: m a -> APIMonad m b
  apiGuardL acc = lift acc >>= apiGuard
  
instance Monad m => APIGuard m b (Either DBError b)  where
  apiGuard (Left e)  = AM $ return $ apiResponseFromDBError e
  apiGuard (Right v) = return v
              
instance Monad m => APIGuard m b (Maybe b) where
  apiGuard Nothing = apiForbidden
  apiGuard (Just x) = return x

instance Monad m => APIGuard m b (Either String b) where
  apiGuard (Left _) = apiServerError
  apiGuard (Right v) = return v

-- | Return a list of documents the logged in user can see
documentList :: Kontrakcja m => m Response
documentList = api $
  apiGuardL getDocsByLoggedInUser >>=
  lift . mapM jsonDocumentAndFiles >>=
  apiOK . showJSON

documentNew :: Kontra.Kontra Response
documentNew = api $ undefined

documentView :: (Kontrakcja m) => DocumentID -> m Response
documentView (_ :: DocumentID) = api $  undefined

data SignatoryResource = SignatoryResource

instance Read SignatoryResource where
  readsPrec _ ('s':'i':'g':'n':'a':'t':'o':'r':'y':x) = [(SignatoryResource, x)]
  readsPrec _ _ = error "must be \"signatory\""
  
instance FromReqURI SignatoryResource where
    fromReqURI = maybeRead

data AttachmentResource = AttachmentResource

instance Read AttachmentResource where
  readsPrec _  ('a':'t':'t':'a':'c':'h':'m':'e':'n':'t':x) = [(AttachmentResource, x)]
  readsPrec _ _ = error "must be \"attachment\""

instance FromReqURI AttachmentResource where
    fromReqURI = maybeRead
    
data FileResource = FileResource

instance Read FileResource where
  readsPrec _  ('f':'i':'l':'e':x) = [(FileResource, x)]
  readsPrec _ _ = error "must be \"file\""

instance FromReqURI FileResource where
    fromReqURI = maybeRead

getMagicHash :: Kontrakcja m => APIMonad m MagicHash
getMagicHash = do
  mmagichashs <- lift $ getDataFn' (look "mh")
  when (isNothing mmagichashs) $
    apiNotLoggedIn
  let Just magichashs = mmagichashs
      mmagichash = maybeRead magichashs
  when (isNothing mmagichash) $
    apiBadInput
  let Just magichash = mmagichash
  return magichash
  
getSigLinkID :: Kontrakcja m => APIMonad m SignatoryLinkID
getSigLinkID = do
  msignatorylinks <- lift $ getDataFn' (look "slid")
  when (isNothing msignatorylinks) $
    apiNotLoggedIn
  let Just signatorylinks = msignatorylinks
      msignatorylink = maybeRead signatorylinks
  when (isNothing msignatorylink) $
    apiBadInput
  let Just signatorylink = msignatorylink
  return signatorylink
  
documentUploadSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentUploadSignatoryAttachment did _ sid _ aname _ = api $ do
  magichash <- getMagicHash
  slid <- getSigLinkID
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
  d <- apiGuardL $ update $ SaveSigAttachment (documentid doc) (BS.fromString aname) email (fileid file)
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) d
  
  apiCreated $ jsonSigAttachmentWithFile sigattach' (Just file)

documentDeleteSignatoryAttachment :: Kontrakcja m => DocumentID -> SignatoryResource -> SignatoryLinkID -> AttachmentResource -> String -> FileResource -> m Response
documentDeleteSignatoryAttachment did _ sid _ aname _ = api $ do
  magichash <- getMagicHash
  slid <- getSigLinkID
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

  d <- apiGuardL $ update $ DeleteSigAttachment (documentid doc) email fileid
  
  -- let's dig the attachment out again
  sigattach' <- apiGuard $ getSignatoryAttachment email (BS.fromString aname) d
  
  apiOK $ jsonSigAttachmentWithFile sigattach' Nothing

  
-- helpers

  
jsonDocumentAndFiles :: Kontrakcja m => Document -> m JSValue
jsonDocumentAndFiles doc = do
  --files <- getFilesByStatus doc
  return $ jsonDocumentForSignatory doc
  
