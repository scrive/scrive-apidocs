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

import Archive.View

import InputValidation
import KontraLink
import Kontra
import DB
import Doc.DocStateData
import Doc.DocumentMonad (withDocument, theDocument, DocumentT)
import Doc.Model
import User.Model
import User.Utils
import Util.MonadUtils
import Data.List

import Control.Applicative
import Control.Conditional (unlessM)
import Util.SignatoryLinkUtils
import Util.Actor
import Text.JSON
import Data.Maybe
import Text.JSON.Gen as J
import Doc.Action
import Doc.DocInfo (isPending)
import Doc.DocMails
import Control.Monad
import Codec.Archive.Zip
import Util.ZipUtil
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import Data.Char
import File.Storage as F
import qualified Log
import AppView
import Happstack.Server(Response)
import qualified Text.StringTemplates.Fields as F

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
             let Just SignatoryLink{signatorylinkid} = getSigLinkFor user doc
             dbUpdate $ RejectDocument signatorylinkid  Nothing actor
             theDocument >>= postDocumentRejectedChange signatorylinkid
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

  {- |
  Main view of the archive
 -}
showArchive :: Kontrakcja m => m (Either KontraLink Response)
showArchive = checkUserTOSGet $ do
    tostime <- guardJustM $ join <$> fmap userhasacceptedtermsofservice <$> ctxmaybeuser <$> getContext
    user    <- guardJustM $ ctxmaybeuser <$> getContext
    pb <-  pageArchive user tostime
    renderFromBodyWithFields kontrakcja pb (F.value "archive" True)

-- Zip utils

docToEntry ::  Kontrakcja m => Document -> m (Maybe Entry)
docToEntry doc = do
      let name = filter ((/= ' ')) $ filter (isAscii) $ (documenttitle doc) ++ "_" ++ (show $ documentid doc) ++".pdf"
      case documentsealedfile doc `mplus` documentfile doc of
        Just fid -> do
            content <- getFileIDContents fid
            return $ Just $ toEntry name 0 $ BSL.pack $ BSS.unpack content
        Nothing -> return Nothing
