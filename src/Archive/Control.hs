module Archive.Control
       (
       handleDelete,
       handleReallyDelete,
       handleSendReminders,
       handleRestore,
       handleShare,
       handleCancel,
       handleZip,
       showArchive
       )
       where

import Codec.Archive.Zip
import Control.Applicative
import Control.Conditional (unlessM)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Happstack.Server(Response)
import Text.JSON
import Text.JSON.Gen as J
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BSL
import qualified Text.StringTemplates.Fields as F

import AppView
import Archive.View
import Company.Model (GetCompany(..))
import DB
import Doc.Action
import Doc.DocInfo (isPending)
import Doc.DocMails
import Doc.DocStateData
import Doc.DocumentMonad (withDocument, theDocument, DocumentT)
import Doc.Model
import File.Storage as F
import InputValidation
import Kontra
import KontraLink
import User.Model
import User.Utils
import Util.Actor
import Util.MonadUtils
import Util.SignatoryLinkUtils
import Util.ZipUtil
import qualified Log

handleArchiveDocumentsAction :: Kontrakcja m => String -> (User -> Document -> Bool) -> ((User, Actor) -> DocumentT m a) -> m [a]
handleArchiveDocumentsAction actionStr docPermission m = do
  ctx@Context{ctxmaybeuser = Just user} <- getContext
  ids <- getCriticalField asValidDocIDList "documentids"
  docs <- dbQuery $ GetDocuments [DocumentsVisibleToUser $ userid user] [DocumentFilterByDocumentIDs ids] [] (0, 100)
  when (sort (map documentid docs) /= sort ids) $ failWithMsg user ids "Retrieved documents didn't match specified document ids"
  if all (docPermission user) docs
  then do
    let actor = userActor ctx user
    forM docs $ flip withDocument $ m (user, actor)
  else do
    failWithMsg user ids $ "User didn't have permission to " ++ actionStr
  where
    failWithMsg user ids msg = do
      Log.mixlog msg $ do
        J.value "user_id" $ show $ userid user
        J.value "documentids" $ show ids
      internalError

handleArchiveDocumentsAction' :: Kontrakcja m => String -> (User -> Document -> Bool) -> ((User, Actor) -> DocumentT m a) -> m JSValue
handleArchiveDocumentsAction' actionStr docPermission m = do
  _ <- handleArchiveDocumentsAction actionStr docPermission m
  J.runJSONGenT (return ())

handleDelete :: Kontrakcja m => m JSValue
handleDelete = do
  handleArchiveDocumentsAction' "cancel/reject documents" isDocumentVisibleToUser $ \(user, actor) -> do
        doc <- theDocument
        when (isPending doc) $
           if isAuthorOrAuthorsAdmin user doc
           then do
             dbUpdate $ CancelDocument actor
             postDocumentCanceledChange =<< theDocument
           else do
             -- user must be a regular signatory
             let Just (sl@SignatoryLink{signatorylinkid}) = getSigLinkFor user doc
             ctx <- getContext
             sl_actor <- signatoryActor ctx sl
             dbUpdate $ RejectDocument signatorylinkid  Nothing sl_actor
             theDocument >>= postDocumentRejectedChange signatorylinkid Nothing
        dbUpdate $ ArchiveDocument (userid user) actor

handleReallyDelete :: Kontrakcja m => m JSValue
handleReallyDelete = do
  handleArchiveDocumentsAction' "really delete documents" isDocumentVisibleToUser $ \(user, actor) -> do
    dbUpdate $ ReallyDeleteDocument (userid user) actor

handleSendReminders :: Kontrakcja m => m JSValue
handleSendReminders = handleArchiveDocumentsAction' "send reminders" isAuthorOrAuthorsAdmin $ \(_, actor) -> do
  remindedsiglinks <- sendAllReminderEmailsExceptAuthor actor False
  when (null remindedsiglinks) internalError

handleCancel :: Kontrakcja m =>  m JSValue
handleCancel = handleArchiveDocumentsAction' "cancel documents" isAuthorOrAuthorsAdmin $ \(_, actor) -> do
  unlessM (isPending <$> theDocument) internalError
  dbUpdate $ CancelDocument actor
  postDocumentCanceledChange =<< theDocument

handleRestore :: Kontrakcja m => m JSValue
handleRestore = handleArchiveDocumentsAction' "restore documents" isDocumentVisibleToUser $ \(user, actor) -> dbUpdate $ RestoreArchivedDocument user actor

handleShare :: Kontrakcja m => m JSValue
handleShare = handleArchiveDocumentsAction' "share documents" isAuthorOrAuthorsAdmin $ const $ do
  doc <- theDocument
  dbUpdate $ SetDocumentSharing [documentid doc] True

handleZip :: Kontrakcja m => m ZipArchive
handleZip = do
  Log.mixlog_ $ "Downloading zip list"
  mentries <- handleArchiveDocumentsAction "download zipped documents" isDocumentVisibleToUser $ const $ do
               docToEntry =<< theDocument
  return $ ZipArchive "selectedfiles.zip" $ foldr addEntryToArchive emptyArchive $ catMaybes $ mentries

-- | Main view of the archive
showArchive :: Kontrakcja m => m (Either KontraLink Response)
showArchive = checkUserTOSGet $ do
    tostime <- guardJustM $ join <$> fmap userhasacceptedtermsofservice <$> ctxmaybeuser <$> getContext
    user    <- guardJustM $ ctxmaybeuser <$> getContext
    mcompany <- dbQuery $ GetCompany (usercompany user)
    pb <-  pageArchive user mcompany tostime
    renderFromBodyWithFields pb (F.value "archive" True)

-- Zip utils

docToEntry ::  Kontrakcja m => Document -> m (Maybe Entry)
docToEntry doc = do
      let name = filter ((/= ' ')) $ filter (isAscii) $ (documenttitle doc) ++ "_" ++ (show $ documentid doc) ++".pdf"
      case documentsealedfile doc `mplus` documentfile doc of
        Just fid -> do
            content <- getFileIDContents fid
            return $ Just $ toEntry name 0 $ BSL.pack $ BSS.unpack content
        Nothing -> return Nothing
