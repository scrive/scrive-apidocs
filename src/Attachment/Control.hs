
module Attachment.Control
  ( handleCreateNew
  , handleShare
  , handleRename
  , handleShow
  , handleDelete
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Log
import Control.Monad.IO.Class
import User.Utils
import Templates.Templates
import qualified Templates.Fields as F

handleRename :: Kontrakcja m => AttachmentID -> m JSValue
handleRename attid = do
  _ <- guardJustM $ ctxmaybeuser <$> getContext
  title <- getCriticalField return "docname"
  actor <- guardJustM $ mkAuthorActor <$> getContext
  _ <- guardRightM $ dbUpdate $ SetAttachmentTitle attid title actor
  J.runJSONGenT $ return ()


handleShare :: Kontrakcja m => m JSValue
handleShare =  do
    _ <- guardJustM $ ctxmaybeuser <$> getContext
    ids <- getCriticalFieldList asValidAttachmentID "doccheck"
    _ <- dbUpdate $ SetAttachmentsSharing ids True
    J.runJSONGenT $ return ()

handleDelete :: Kontrakcja m => m JSValue
handleDelete = do
    Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber } <- getContext
    attids <- getCriticalFieldList asValidAttachmentID "doccheck"
    let actor = userActor ctxtime ctxipnumber (userid user) (getEmail user)
    _ <- guardRightM' $ dbUpdate $ DeleteAttachments (userid user) attids actor
    J.runJSONGenT $ return ()

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
    atts <- dbQuery $ GetAttachments [AttachmentsSharedInUsersCompany (userid $ fromJust $ ctxmaybeuser ctx)]
            [AttachmentFilterByID [attid]] [] (0,10)
    case atts of
      [att] -> attachmentJSON att
      _ -> error "not found"


jsonAttachmentsList ::  Kontrakcja m => m (Either KontraLink JSValue)
jsonAttachmentsList = withUserGet $ do
  Just user@User{userid = uid} <- ctxmaybeuser <$> getContext
  lang <- ctxlang <$> getContext

  params <- getListParamsNew

  let (domain,filters) = ([AttachmentsOfAuthorDeleteValue uid False, AttachmentsSharedInUsersCompany uid],[])

  let sorting    = attachmentSortingFromParams params
      searching  = attachmentSearchingFromParams params
      pagination = (listParamsOffset params, listParamsLimit params)
      attachmentsPageSize = 100 :: Int
  cttime <- getMinutesTime
  allAtts <- dbQuery $ GetAttachments domain (searching ++ filters) sorting pagination
  let atts = PagedList { list       = allAtts
                       , params     = params
                       , pageSize   = attachmentsPageSize
                       }
  attsJSONs <- mapM (attForListJSON (timeLocaleForLang lang) cttime user) $ take attachmentsPageSize $ list atts
  runJSONGenT $ do
    J.value "list" attsJSONs
    J.value "paging" $ pagingParamsJSON atts

attForListJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> User -> Attachment -> m JSValue
attForListJSON tl crtime _user att = do
  let link = LinkAttachmentView (attachmentid att)
  runJSONGenT $ do
    J.object "fields" $ attFieldsListForJSON tl crtime att
    J.value "link" $ show link

attFieldsListForJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> Attachment -> JSONGenT m ()
attFieldsListForJSON tl crtime att = do
    J.value "id" $ show $ attachmentid att
    J.value "title" $ attachmenttitle att
    J.value "time" $ showDateAbbrev tl crtime (attachmentmtime att)
    J.value "shared" $ show $ attachmentshared att
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
    if BSL.null content
      then do
        Log.debug "makeAttachmentFromFile: no content"
        return Nothing
      else do
          Log.debug "Got the content, creating document"
          let title = takeBaseName filename
          actor <- guardJustM $ mkAuthorActor <$> getContext
          ctx <- getContext
          att <- guardRightM $ dbUpdate $ NewAttachment (userid $ fromJust $ ctxmaybeuser ctx) title filename (Binary $ BS.concat $ BSL.toChunks content) actor
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
    [AttachmentsSharedInUsersCompany (userid user)]
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
      F.value "renamelink" $ show $ LinkRenameAttachment attachmentid
