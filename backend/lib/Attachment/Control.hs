
module Attachment.Control
  ( handleCreateNew
  , handleShare
  , handleDelete
  , handleDownloadAttachment
  , jsonAttachmentsList
  )
where

import Control.Monad.IO.Class
import Happstack.Server hiding (simpleHTTP)
import Log
import System.FilePath
import Text.JSON
import Text.JSON.Gen as J
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Unjson as Unjson

import AppView (respondWithPDF)
import Attachment.AttachmentID
import Attachment.JSON
import Attachment.Model
import DB
import Doc.Rendering
import File.Storage
import Happstack.Fields
import InputValidation
import Kontra
import KontraLink
import KontraPrelude
import Redirect
import User.Model
import User.Utils
import Util.Actor
import Util.MonadUtils

handleShare :: Kontrakcja m => m JSValue
handleShare =  do
    user <- guardJustM $ ctxmaybeuser <$> getContext
    ids <- getCriticalField asValidAttachmentIDList "attachmentids"
    dbUpdate $ SetAttachmentsSharing (userid user) ids True
    J.runJSONGenT $ return ()

handleDelete :: Kontrakcja m => m JSValue
handleDelete = do
    ctx@(Context { ctxmaybeuser = Just user}) <- getContext
    ids <- getCriticalField asValidAttachmentIDList "attachmentids"
    let actor = userActor ctx user
    dbUpdate $ DeleteAttachments (userid user) ids actor
    J.runJSONGenT $ return ()

-- | This handler downloads a file by file id. As specified in
-- handlePageOfDocument rules of access need to be obeyd. This handler
-- download file as is.
handleDownloadAttachment :: Kontrakcja m => AttachmentID -> String -> m Response
handleDownloadAttachment attid _nameForBrowser = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  atts <- dbQuery $ GetAttachments [ AttachmentsSharedInUsersCompany (userid user)
                                            , AttachmentsOfAuthorDeleteValue (userid user) True
                                            , AttachmentsOfAuthorDeleteValue (userid user) False
                                            ]
                                            [ AttachmentFilterByID attid ]
                                            []
  case atts of
       [att] -> getFileIDContents (attachmentfile att) >>= return . respondWithPDF False
       _ -> internalError

handleCreateNew :: Kontrakcja m => m JSValue
handleCreateNew = do
  guardLoggedIn
  input <- getDataFnM (lookInput "doc")
  _mdoc <- makeAttachmentFromFile input
  J.runJSONGenT $ return ()

jsonAttachmentsList ::  Kontrakcja m => m (Either KontraLink Response)
jsonAttachmentsList = withUserGet $ do
  uid <- userid <$> guardJustM (ctxmaybeuser <$> getContext)

  domain <- getField "domain" >>= \case
    (Just "All") -> return [AttachmentsOfAuthorDeleteValue uid False, AttachmentsSharedInUsersCompany uid]
    _ -> return [AttachmentsOfAuthorDeleteValue uid False]

  filters <- getFieldBS "filter" >>= \case
    Just paramValue -> case Aeson.eitherDecode paramValue of
        Right js -> case (Unjson.parse unjsonAttachmentFiltering js) of
          (Unjson.Result res []) -> return $ res
          _ -> internalError
        Left _ -> internalError
    Nothing -> return []

  sorting <- getFieldBS "sorting" >>= \case
    Just paramValue -> case Aeson.eitherDecode paramValue of
        Right js -> case (Unjson.parse unjsonAttachmentSorting js) of
          (Unjson.Result res []) -> return $ res
          _ -> internalError
        Left _ -> internalError
    Nothing -> return []

  attachments <- dbQuery $ GetAttachments domain filters sorting
  return $ Response 200 Map.empty nullRsFlags (Unjson.unjsonToByteStringLazy unjsonAttachments attachments) Nothing


makeAttachmentFromFile :: Kontrakcja m => Input -> m (Maybe Attachment)
makeAttachmentFromFile (Input contentspec (Just filename) _contentType) = do
    logInfo_ "makeAttachmentFromFile: beggining"
    guardLoggedIn
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    cres <- preCheckPDF (BSL.toStrict content)
    case cres of
      Left _ -> do
         logInfo_ "Attachment file is not a valid PDF"
         internalError
      Right content' -> do
        logInfo_ "Got the content, creating document"
        let title = takeBaseName filename
        actor <- guardJustM $ mkAuthorActor <$> getContext
        ctx <- getContext
        att <- dbUpdate $ NewAttachment (userid $ $fromJust $ ctxmaybeuser ctx) title filename content' actor
        return $ Just att
makeAttachmentFromFile _ = internalError -- to complete the patterns
