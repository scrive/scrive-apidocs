module Archive.Control
       (
       handleDelete,
       handleSendReminders,
       handleRestore,
       handleReallyDelete,
       handleShare,
       handleCancel,
       handleZip,
       showArchive,
       jsonDocumentsList
       )
       where

import Archive.View

import InputValidation
import KontraLink
import Kontra
import DB
import Doc.DocStateData
import Doc.Model
import User.Model
import User.Utils
import Utils.Monad
import Util.MonadUtils

import Control.Applicative
import Control.Monad.Trans.Maybe
import Util.SignatoryLinkUtils
import Util.Actor
import Util.HasSomeUserInfo
import Text.JSON
import Util.CSVUtil
import ListUtil
import Happstack.Fields
import PadQueue.Model
import Data.Maybe
import Text.JSON.Gen as J
import Text.JSON.FromJSValue
import Doc.Action
import Doc.DocStateQuery
import Control.Monad
import Codec.Archive.Zip
import Util.ZipUtil
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import Data.Char
import File.Storage as F
import qualified Log as Log
import Control.Logic
import Control.Monad.Identity
import Text.JSON.String (runGetJSON)
import Doc.DocDraft()
import Data.String.Utils (splitWs)
import MinutesTime

handleDelete :: Kontrakcja m => m JSValue
handleDelete = do
    Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber } <- getContext
    docids <- getCriticalFieldList asValidDocID "doccheck"
    let actor = userActor ctxtime ctxipnumber (userid user) (getEmail user)
    docs <- guardRightM' $ getDocsByDocIDs docids
    forM_ docs $ \doc -> do
              let usl = getSigLinkFor doc user
                  csl = (getAuthorSigLink $ documentsignatorylinks doc) <| (useriscompanyadmin user) |> Nothing
                  msl =  usl `mplus` csl
              when (isNothing msl) $ do
                Log.debug $ "User #" ++ show (userid user) ++ " has no rights to deleted document #" ++ show (documentid doc)
                internalError
              case (documentstatus doc) of
                  Pending -> if (isAuthor msl)
                                then do
                                   dbUpdate $ CancelDocument (documentid doc) actor
                                   doc' <- guardRightM' $ getDocByDocID (documentid doc)
                                   postDocumentCanceledChange doc'
                                else do
                                   dbUpdate $ RejectDocument (documentid doc) (signatorylinkid $ fromJust msl) Nothing actor
                                   doc' <- guardRightM' $ getDocByDocID (documentid doc)
                                   postDocumentRejectedChange doc' (signatorylinkid $ fromJust msl)
                  _ -> return ()
              dbUpdate $ ArchiveDocument (userid user) (documentid doc) actor

              case (documentstatus doc) of
                   Preparation -> do
                       _ <- dbUpdate $ ReallyDeleteDocument (userid user) (documentid doc) actor
                       return ()

                   _ -> return ()
    J.runJSONGenT $ return ()



handleSendReminders :: Kontrakcja m => m JSValue
handleSendReminders = do
    ctx@Context{ctxmaybeuser = Just user } <- getContext
    ids <- getCriticalFieldList asValidDocID "doccheck"
    actor <- guardJustM $ fmap mkAuthorActor getContext
    remindedsiglinks <- fmap concat . sequence . map (\docid -> sendAllReminderEmails ctx actor user docid) $ ids
    case (length remindedsiglinks) of
      0 -> internalError
      _ -> J.runJSONGenT $ return ()

handleCancel :: Kontrakcja m =>  m JSValue
handleCancel = do
  docids <- getCriticalFieldList asValidDocID "doccheck"
  forM_ docids $ \docid -> do
      doc <- guardRightM' $ getDocByDocID docid
      actor <- guardJustM $ mkAuthorActor <$> getContext
      if (documentstatus doc == Pending)
        then do
           dbUpdate $ CancelDocument (documentid doc) actor
           doc' <- guardRightM' $ getDocByDocID $ docid
           postDocumentCanceledChange doc'
        else internalError
  J.runJSONGenT $ return ()

handleRestore :: Kontrakcja m => m JSValue
handleRestore = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  mapM_ (\did -> dbUpdate $ RestoreArchivedDocument user did actor) docids
  J.runJSONGenT $ return ()

handleReallyDelete :: Kontrakcja m => m JSValue
handleReallyDelete = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  mapM_ (\did -> do
            _ <- guardJustM . runMaybeT $ do
              True <- dbUpdate $ ReallyDeleteDocument (userid user) did actor
              Just doc <- dbQuery $ GetDocumentByDocumentID did
              return doc
            return ())
    docids
  J.runJSONGenT $ return ()


handleShare :: Kontrakcja m => m JSValue
handleShare =  do
    _ <- guardJustM $ ctxmaybeuser <$> getContext
    ids <- getCriticalFieldList asValidDocID "doccheck"
    _ <- dbUpdate $ SetDocumentSharing ids True
    w <- flip mapM ids $ (dbQuery . GetDocumentByDocumentID)
    when_ (null $ catMaybes w) internalError
    J.runJSONGenT $ return ()

handleZip :: Kontrakcja m => m ZipArchive
handleZip = do
  Log.debug $ "Downloading zip list"
  docids <- take 100 <$> getCriticalFieldList asValidDocID "doccheck"
  mentries <- forM docids $ \did -> do
                Log.debug "Getting file for zip download"
                doc <- guardRightM' $ getDocByDocID did
                docToEntry doc
  return $ ZipArchive "selectedfiles.zip" $ foldr addEntryToArchive emptyArchive $ map fromJust $ filter isJust $ mentries
{- |
   Constructs a list of documents (Arkiv) to show to the user.
 -}
showArchive :: Kontrakcja m => m (Either KontraLink String)
showArchive = checkUserTOSGet $ do
    tostime <- guardJustM $ join <$> fmap userhasacceptedtermsofservice <$> ctxmaybeuser <$> getContext
    user    <- guardJustM $ ctxmaybeuser <$> getContext
    pageArchive user tostime

jsonDocumentsList ::  Kontrakcja m => m (Either CSV JSValue)
jsonDocumentsList = do
  Log.debug $ "Long list " ++ (show $ map fromEnum [SCDraft,SCCancelled,SCRejected,SCTimedout,SCError,SCDeliveryProblem,SCSent,SCDelivered,SCRead,SCOpened,SCSigned])
  user@User{userid = uid} <- guardJustM $ ctxmaybeuser <$> getContext
  doctype <- getField' "documentType"
  params <- getListParams
  let (domain,filters1) = case doctype of
                          "Document"          -> ([DocumentsVisibleToUser uid]
                                                 ,[DocumentFilterDeleted False False, DocumentFilterSignable, DocumentFilterUnsavedDraft False])
                          "Template"          -> ([DocumentsVisibleToUser uid]
                                                 ,[DocumentFilterDeleted False False, DocumentFilterTemplate, DocumentFilterUnsavedDraft False])
                          "Rubbish"           -> ([DocumentsVisibleToUser uid]
                                                 ,[DocumentFilterDeleted True False, DocumentFilterUnsavedDraft False])
                          "All"               -> ([DocumentsVisibleToUser uid],[DocumentFilterUnsavedDraft False])
                          _ -> ([DocumentsVisibleToUser uid],[DocumentFilterDeleted False False, DocumentFilterUnsavedDraft False])
      filters2 = concatMap fltSpec (listParamsFilters params)
      fltSpec ("time", tostr) = case reads tostr of
                                    (((Just from',Just to'),""):_) -> [DocumentFilterByMonthYearFrom from',DocumentFilterByMonthYearTo to']
                                    (((Nothing ,Just to'),""):_) -> [DocumentFilterByMonthYearTo to']
                                    (((Just from',Nothing),""):_)   -> [DocumentFilterByMonthYearFrom from']
                                    _ -> []
      fltSpec ("mtime", tostr) = case parseMinutesTimeRealISO tostr of
                                    Just mtime -> [DocumentFilterByModificationTimeAfter mtime]
                                    _ -> []
      fltSpec ("sender", tostr) = case reads tostr of
                                    ((suid,""):_) -> [DocumentFilterByAuthor suid]
                                    _ -> []
      fltSpec ("cansign", tostr) = case reads tostr of
                                    ((suid,""):_) -> [DocumentFilterByCanSign suid]
                                    _ -> []
      fltSpec ("status", scstr) = case reads scstr of
                                    ((statusclasss,""):_) -> [DocumentFilterByStatusClass statusclasss]
                                    _ -> []
      fltSpec _ = []
  tagsstr <- getField' "tags"
  let tagsFilters = case runGetJSON readJSArray tagsstr of
                      Right js ->[DocumentFilterByTags $ join $ maybeToList $ runIdentity $ withJSValue js $ fromJSValueCustomMany $ fromJSValueM]
                      _ -> []
  let sorting    = docSortingFromParams params
      searching  = docSearchingFromParams params
      pagination2 = ((listParamsOffset params),(listParamsLimit params), Just docsPageSize)
      filters = filters1 ++ filters2 ++ tagsFilters
  Log.debug $ "Filtering with " ++ show filters
  padqueue <- dbQuery $ GetPadQueue $ userid user
  format <- getField "format"
  case format of
       Just "csv" -> do
          allDocs <- dbQuery $ GetDocuments domain (searching ++ filters) sorting (0,-1)
          let docsCSVs = concat $ zipWith docForListCSV  [1..] allDocs
          return $ Left $ CSV { csvFilename = "documents.csv"
                              , csvHeader = docForListCSVHeader
                              , csvContent = docsCSVs
                              }
       _ -> do
          (allDocsCount,allDocs) <- dbQuery $ GetDocuments2 domain (searching ++ filters) sorting pagination2
          let docs = PagedList {  list       = allDocs
                                , params     = params
                                , pageSize   = docsPageSize
                                , listLength = allDocsCount
                                }
          docsJSONs <- mapM (docForListJSON user padqueue) $ list docs
          return $ Right $ runJSONGen $ do
              value "list" docsJSONs
              value "paging" $ pagingParamsJSON docs

docSortingFromParams :: ListParams -> [AscDesc DocumentOrderBy]
docSortingFromParams params =
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
    x "type"              = [Asc DocumentOrderByType]
    x "typeREV"           = [Desc DocumentOrderByType]
    x "author"            = [Asc DocumentOrderByAuthor]
    x "authorRev"         = [Desc DocumentOrderByAuthor]
    x _                   = []



docSearchingFromParams :: ListParams -> [DocumentFilter]
docSearchingFromParams params =
  case listParamsSearching params of
    "" -> []
    x -> map DocumentFilterByString $ take 5 (splitWs x)


docsPageSize :: Int
docsPageSize = 100

-- Zip utils

docToEntry ::  Kontrakcja m => Document -> m (Maybe Entry)
docToEntry doc = do
      let name = filter ((/= ' ')) $ filter (isAscii) $ (documenttitle doc) ++ "_" ++ (show $ documentid doc) ++".pdf"
      case documentsealedfile doc `mplus` documentfile doc of
        Just fid -> do
            content <- getFileIDContents fid
            return $ Just $ toEntry name 0 $ BSL.pack $ BSS.unpack content
        Nothing -> return Nothing
