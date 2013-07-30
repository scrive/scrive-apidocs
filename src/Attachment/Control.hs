
module Attachment.Control
  ( handleCreateNew
  , handleShare
  , handleRename
  , handleShow
  , handleDelete
  , handleDownloadAttachment
  , jsonAttachment
  , jsonAttachmentsList
  )
where
import Attachment.AttachmentID
import InputValidation
import KontraLink
import Kontra
import DB
import Attachment.Model
import User.Model
import Util.MonadUtils
import Happstack.Server hiding (simpleHTTP)

import Control.Applicative
import Util.Actor
import Util.HasSomeUserInfo
import Text.JSON
import ListUtil
import MinutesTime
import Data.Maybe
import Text.JSON.Gen as J
import Redirect
import System.FilePath

import qualified Data.ByteString.Lazy as BSL
import qualified Log
import Control.Monad.IO.Class
import User.Utils
import Doc.Rendering
import Utils.String
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F
import File.Storage
import File.Model
import qualified Data.Map as Map
import qualified Data.ByteString.UTF8 as BS

handleRename :: Kontrakcja m => AttachmentID -> m JSValue
handleRename attid = do
  _ <- guardJustM $ ctxmaybeuser <$> getContext
  title <- getCriticalField return "docname"
  actor <- guardJustM $ mkAuthorActor <$> getContext
  dbUpdate $ SetAttachmentTitle attid title actor
  J.runJSONGenT $ return ()


handleShare :: Kontrakcja m => m JSValue
handleShare =  do
    user <- guardJustM $ ctxmaybeuser <$> getContext
    ids <- getCriticalFieldList asValidAttachmentID "doccheck"
    dbUpdate $ SetAttachmentsSharing (userid user) ids True
    J.runJSONGenT $ return ()

handleDelete :: Kontrakcja m => m JSValue
handleDelete = do
    Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber } <- getContext
    attids <- getCriticalFieldList asValidAttachmentID "doccheck"
    let actor = userActor ctxtime ctxipnumber (userid user) (getEmail user)
    dbUpdate $ DeleteAttachments (userid user) attids actor
    J.runJSONGenT $ return ()

-- | This handler downloads a file by file id. As specified in
-- handlePageOfDocument rules of access need to be obeyd. This handler
-- download file as is.
handleDownloadAttachment :: Kontrakcja m => AttachmentID -> FileID -> String -> m Response
handleDownloadAttachment attid fid _nameForBrowser = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  atts <- dbQuery $ GetAttachments [ AttachmentsSharedInUsersCompany (userid user)
                                            , AttachmentsOfAuthorDeleteValue (userid user) True
                                            , AttachmentsOfAuthorDeleteValue (userid user) False
                                            ]
                                            [ AttachmentFilterByID [attid]
                                            , AttachmentFilterByFileID [fid]
                                            ]
                                            []
                                            (0,1)
  case atts of
       [att] -> getFileIDContents (attachmentfile att) >>= respondWithPDF
       _ -> internalError
  where
    respondWithPDF contents = do
      let res = Response 200 Map.empty nullRsFlags (BSL.fromChunks [contents]) Nothing
          res2 = setHeaderBS (BS.fromString "Content-Type") (BS.fromString "application/pdf") res
      return res2


handleCreateNew :: Kontrakcja m => m JSValue
handleCreateNew = do
  guardLoggedIn
  input <- getDataFnM (lookInput "doc")
  _mdoc <- makeAttachmentFromFile input
  J.runJSONGenT $ return ()

attachmentJSON :: (TemplatesMonad m, KontraMonad m, MonadDB m) => Attachment -> m JSValue
attachmentJSON att = do
    runJSONGenT $ do
      J.value "id" $ show $ attachmentid att
      J.value "title" $ attachmenttitle att
      J.value "file" $ show $ attachmentfile att
      J.value "user" $ show $ attachmentuser att
      --J.value "ctime" $ attachmentctime att
      --J.value "mtime" $ attachmentmtime att
      J.value "shared" $ attachmentshared att
      J.value "deleted" $ attachmentdeleted att

jsonAttachment :: Kontrakcja m => AttachmentID -> m JSValue
jsonAttachment attid = do
    ctx <- getContext
    let Just user = ctxmaybeuser ctx
    atts <- dbQuery $ GetAttachments [AttachmentsSharedInUsersCompany (userid $ fromJust $ ctxmaybeuser ctx),  AttachmentsOfAuthorDeleteValue  (userid user) False]
            [AttachmentFilterByID [attid]] [] (0,10)
    case atts of
      [att] -> attachmentJSON att
      _ -> error "not found"


jsonAttachmentsList ::  Kontrakcja m => m (Either KontraLink JSValue)
jsonAttachmentsList = withUserGet $ do
  Just user@User{userid = uid} <- ctxmaybeuser <$> getContext
  params <- getListParams

  let (domain,filters) = ([AttachmentsOfAuthorDeleteValue uid False, AttachmentsSharedInUsersCompany uid],[])

  let sorting    = attachmentSortingFromParams params
      searching  = attachmentSearchingFromParams params
      pagination = (listParamsOffset params, listParamsLimit params)
      attachmentsPageSize = 100 :: Int
  allAtts <- dbQuery $ GetAttachments domain (searching ++ filters) sorting pagination
  let atts = PagedList { list       = allAtts
                       , params     = params
                       , pageSize   = attachmentsPageSize
                       , listLength = length allAtts
                       }
  attsJSONs <- mapM (attForListJSON  user) $ take attachmentsPageSize $ list atts
  runJSONGenT $ do
    J.value "list" attsJSONs
    J.value "paging" $ pagingParamsJSON atts

attForListJSON :: TemplatesMonad m => User -> Attachment -> m JSValue
attForListJSON _user att = do
  let link = LinkAttachmentView (attachmentid att)
  runJSONGenT $ do
    J.object "fields" $ attFieldsListForJSON att
    J.value "link" $ show link

attFieldsListForJSON :: TemplatesMonad m =>  Attachment -> JSONGenT m ()
attFieldsListForJSON att = do
    J.value "id" $ show $ attachmentid att
    J.value "title" $ attachmenttitle att
    J.value "time" $ formatMinutesTimeRealISO (attachmentmtime att)
    J.value "shared" $ attachmentshared att
    J.value "file" $ show $ attachmentfile att

attachmentSortingFromParams :: ListParams -> [AscDesc AttachmentOrderBy]
attachmentSortingFromParams params =
   (concatMap x (listParamsSorting params)) ++ [Desc AttachmentOrderByMTime] -- default order by mtime
  where
    x "title"             = [Asc AttachmentOrderByTitle]
    x "titleREV"          = [Desc AttachmentOrderByTitle]
    x "mtime"             = [Asc AttachmentOrderByMTime]
    x "mtimeREV"          = [Desc AttachmentOrderByMTime]
    x "ctime"             = [Asc AttachmentOrderByCTime]
    x "ctimeREV"          = [Desc AttachmentOrderByCTime]
    x _                   = []


attachmentSearchingFromParams :: ListParams -> [AttachmentFilter]
attachmentSearchingFromParams params =
  case listParamsSearching params of
    "" -> []
    x -> [AttachmentFilterByString x]


makeAttachmentFromFile :: Kontrakcja m => Input -> m (Maybe Attachment)
makeAttachmentFromFile (Input contentspec (Just filename) _contentType) = do
    Log.debug $ "makeAttachmentFromFile: beggining"
    guardLoggedIn
    content <- case contentspec of
        Left filepath -> liftIO $ BSL.readFile filepath
        Right content -> return content
    cres <- liftIO $ preCheckPDF (concatChunks content)
    case cres of
      Left _ -> do
         Log.debug "Attachment file is not a valid PDF"
         internalError
      Right content' -> do
        Log.debug "Got the content, creating document"
        let title = takeBaseName filename
        actor <- guardJustM $ mkAuthorActor <$> getContext
        ctx <- getContext
        att <- guardRightM $ dbUpdate $ NewAttachment (userid $ fromJust $ ctxmaybeuser ctx) title filename content' actor
        return $ Just att
makeAttachmentFromFile _ = internalError -- to complete the patterns

{- |
   Handles the request to show an attachment to a logged in user.
   URL: /a/{attachmentid}
   Method: GET
 -}
handleShow :: Kontrakcja m => AttachmentID -> m (Either KontraLink (Either Response String))
handleShow attid = checkUserTOSGet $ do
  ctx <- getContext
  let Just user = ctxmaybeuser ctx
  mattachment <- oneObjectReturnedGuard =<< dbQuery (GetAttachments
    [AttachmentsSharedInUsersCompany (userid user), AttachmentsOfAuthorDeleteValue  (userid user) False]
    [AttachmentFilterByID [attid]]
    []
    (0,1))
  case mattachment of
    Nothing -> respond404
    Just at -> Right <$> pageAttachment' at

pageAttachment' :: TemplatesMonad m
                => Attachment
                -> m String
pageAttachment' Attachment{attachmentid, attachmenttitle} =
    renderTemplate "pageAttachment" $ do
      F.value "id" $ show attachmentid
      F.value "title" attachmenttitle
      F.value "editable" $ True
