
module Attachment.Control
where
import Attachment.AttachmentID
import InputValidation
import KontraLink
import Kontra
import DB
import Attachment.Model
import Doc.Model
import User.Model
import Util.MonadUtils
import Happstack.Server hiding (simpleHTTP)

import Control.Applicative
import Util.Actor
import Util.HasSomeUserInfo
import Text.JSON
import ListUtil
import MinutesTime
import Misc
import Data.Maybe
import Text.JSON.Gen as J
import Redirect

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Log
import Control.Monad.IO.Class
import User.Utils
import Templates.Templates

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
    ids <- getCriticalFieldList asValidDocID "doccheck"
    _ <- dbUpdate $ SetDocumentSharing ids True
    w <- flip mapM ids $ (dbQuery . GetDocumentByDocumentID)
    when_ (null $ catMaybes w) internalError
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

jsonAttachmentsList ::  Kontrakcja m => m (Either KontraLink JSValue)
jsonAttachmentsList = withUserGet $ do
  Just user@User{userid = uid} <- ctxmaybeuser <$> getContext
  lang <- getLang . ctxlocale <$> getContext

  params <- getListParamsNew

  let (domain,filters) = ([AttachmentsOfAuthorDeleteValue uid False, AttachmentsSharedInUsersCompany uid],[])

  let sorting    = attachmentSortingFromParams params
      searching  = attachmentSearchingFromParams params
      pagination = attachmentPaginationFromParams params
      attachmentsPageSize = 100 :: Int
  cttime <- getMinutesTime
  allAtts <- dbQuery $ GetAttachments domain (searching ++ filters) sorting pagination
  let atts = PagedList { list       = allAtts
                       , params     = params
                       , pageSize   = attachmentsPageSize
                       }
  attsJSONs <- mapM (attForListJSON (timeLocaleForLang lang) cttime user) $ take attachmentsPageSize $ list atts
  return $ JSObject $ toJSObject
           [ ("list", JSArray attsJSONs)
           , ("paging", pagingParamsJSON atts)
           ]

attForListJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> User -> Attachment -> m JSValue
attForListJSON tl crtime _user att = do
  let link = "xxx"
  runJSONGenT $ do
    J.object "fields" $ attFieldsListForJSON tl crtime att
    J.value "link" $ show link

attFieldsListForJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> Attachment -> JSONGenT m ()
attFieldsListForJSON tl crtime att = do
    J.value "id" $ show $ attachmentid att
    J.value "title" $ attachmenttitle att
    J.value "time" $ showDateAbbrev tl crtime (attachmentmtime att)
    J.value "shared" $ show $ attachmentshared att

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


attachmentPaginationFromParams :: ListParams -> AttachmentPagination
attachmentPaginationFromParams params = AttachmentPagination (listParamsOffset params) (listParamsLimit params)

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
          let title = basename filename
          actor <- guardJustM $ mkAuthorActor <$> getContext
          ctx <- getContext
          att <- guardRightM $ dbUpdate $ NewAttachment (userid $ fromJust $ ctxmaybeuser ctx) title filename (Binary $ BS.concat $ BSL.toChunks content) actor
          return $ Just att
makeAttachmentFromFile _ = internalError -- to complete the patterns
