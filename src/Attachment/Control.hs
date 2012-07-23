
module Attachment.Control
where
import Attachment.AttachmentID
import InputValidation
import KontraLink
import Kontra
import DB
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

import Archive.View
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Log
import Control.Monad.IO.Class
import User.Utils
import Attachment.Model

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
  allDocs <- dbQuery $ GetDocuments domain (searching ++ filters) sorting pagination
  let docs = PagedList { list       = allDocs
                       , params     = params
                       , pageSize   = attachmentsPageSize
                       }
  docsJSONs <- mapM (docForListJSON (timeLocaleForLang lang) cttime user Nothing) $ take attachmentsPageSize $ list docs
  return $ JSObject $ toJSObject
           [ ("list", JSArray docsJSONs)
           , ("paging", pagingParamsJSON docs)
           ]

attachmentSortingFromParams :: ListParams -> [AscDesc DocumentOrderBy]
attachmentSortingFromParams params =
   (concatMap x (listParamsSorting params)) ++ [Desc DocumentOrderByMTime] -- default order by mtime
  where
    x "status"            = [Asc DocumentOrderByStatusClass]
    x "statusREV"         = [Desc DocumentOrderByStatusClass]
    x "title"             = [Asc DocumentOrderByTitle]
    x "titleREV"          = [Desc DocumentOrderByTitle]
    x "time"              = [Asc DocumentOrderByMTime]
    x "timeREV"           = [Desc DocumentOrderByMTime]
    x "party"             = [Asc DocumentOrderByPartners]
    x "partyREV"          = [Desc DocumentOrderByPartners]
    x "partner"           = [Asc DocumentOrderByPartners]
    x "partnerREV"        = [Desc DocumentOrderByPartners]
    -- x "partnercomp"    = viewComparing partnerComps
    -- x "partnercompREV" = viewComparingRev partnerComps
    x "process"           = [Asc DocumentOrderByProcess]
    x "processREV"        = [Desc DocumentOrderByProcess]
    x "type"              = [Asc DocumentOrderByType]
    x "typeREV"           = [Desc DocumentOrderByType]
    x "author"            = [Asc DocumentOrderByAuthor]
    x "authorRev"         = [Desc DocumentOrderByAuthor]
    x _                   = []



attachmentSearchingFromParams :: ListParams -> [DocumentFilter]
attachmentSearchingFromParams params =
  case listParamsSearching params of
    "" -> []
    x -> [DocumentFilterByString x]


attachmentPaginationFromParams :: ListParams -> DocumentPagination
attachmentPaginationFromParams params = DocumentPagination (listParamsOffset params) (listParamsLimit params)

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
