{-# LANGUAGE CPP #-}
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
       showPadDeviceArchive,
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
import Util.MonadUtils

import Control.Applicative
import Control.Monad.Trans.Maybe
import Util.SignatoryLinkUtils
import Stats.Control
import Util.Actor
import Util.HasSomeUserInfo
import Text.JSON
import Util.CSVUtil
import ListUtil
import MinutesTime
import Misc
import PadQueue.Model
import Data.Maybe
import Text.JSON.Gen as J
import Doc.DocUtils
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
import Data.List (find)
import Control.Logic

handleDelete :: Kontrakcja m => m JSValue
handleDelete = do
    Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber } <- getContext
    docids <- getCriticalFieldList asValidDocID "doccheck"
    let actor = userActor ctxtime ctxipnumber (userid user) (getEmail user)
    forM_ docids $ \did -> do
              doc <- guardRightM' $ getDocByDocID $ did
              let usl = (find (isSigLinkFor user) $ documentsignatorylinks doc)
                  csl = (find (isSigLinkFor (usercompany user)) $ documentsignatorylinks doc) <| (useriscompanyadmin user) |> Nothing
                  msl =  usl `mplus` csl
              when (isNothing msl) internalError    
              case (documentstatus doc) of
                  Pending -> if (isAuthor msl)
                                then do 
                                   guardTrueM $ dbUpdate $ CancelDocument (documentid doc) ManualCancel actor
                                   doc' <- guardRightM' $ getDocByDocID $ did
                                   postDocumentCanceledChange doc' "web+archive"
                                else do
                                   guardTrueM $ dbUpdate $ RejectDocument did (signatorylinkid $ fromJust msl) Nothing actor
                                   doc' <- guardRightM' $ getDocByDocID $ did
                                   postDocumentRejectedChange doc' (signatorylinkid $ fromJust msl) "web+archive"
                  _ -> return ()
              guardTrueM $ dbUpdate $ ArchiveDocument user did actor
              doc' <- guardRightM' $ getDocByDocID $ did
              _ <- addSignStatDeleteEvent doc' (fromJust msl) ctxtime
              case (documentstatus doc') of
                   Preparation -> do
                       _ <- dbUpdate $ ReallyDeleteDocument user did actor
                       when_ (isJust $ getSigLinkFor doc' user) $ 
                            addSignStatPurgeEvent doc' (fromJust $ getSigLinkFor doc' user)  ctxtime
                   _ -> return ()         
    J.runJSONGenT $ return ()
            


handleSendReminders :: Kontrakcja m => m JSValue
handleSendReminders = do
    ctx@Context{ctxmaybeuser = Just user } <- getContext
    ids <- getCriticalFieldList asValidDocID "doccheck"
    remindedsiglinks <- fmap concat . sequence . map (\docid -> docRemind ctx user docid) $ ids
    case (length remindedsiglinks) of
      0 -> internalError
      _ -> J.runJSONGenT $ return ()
    where
      docRemind :: Kontrakcja m => Context -> User -> DocumentID -> m [SignatoryLink]
      docRemind ctx user docid = do
        doc <- guardJustM $ dbQuery $ GetDocumentByDocumentID docid
        case (documentstatus doc) of
          Pending -> do
            let isEligible = isEligibleForReminder user doc
                unsignedsiglinks = filter isEligible $ documentsignatorylinks doc
            sequence . map (sendReminderEmail Nothing ctx doc) $ unsignedsiglinks
          _ -> return []
    
handleCancel :: Kontrakcja m =>  m JSValue
handleCancel = do
  docids <- getCriticalFieldList asValidDocID "doccheck"
  forM_ docids $ \docid -> do
      doc <- guardRightM' $ getDocByDocID docid
      actor <- guardJustM $ mkAuthorActor <$> getContext
      if (documentstatus doc == Pending)
        then do
           guardTrueM $ dbUpdate $ CancelDocument (documentid doc) ManualCancel actor
           doc' <- guardRightM' $ getDocByDocID $ docid
           postDocumentCanceledChange doc' "web+archive"
        else internalError
  J.runJSONGenT $ return ()


  
handleRestore :: Kontrakcja m => m JSValue
handleRestore = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  mapM_ (\did -> guardTrueM $ dbUpdate $ RestoreArchivedDocument user did actor) docids
  J.runJSONGenT $ return ()

handleReallyDelete :: Kontrakcja m => m JSValue
handleReallyDelete = do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  actor <- guardJustM $ mkAuthorActor <$> getContext
  ctx <- getContext
  docids <- getCriticalFieldList asValidDocID "doccheck"
  mapM_ (\did -> do
            doc <- guardJustM . runMaybeT $ do
              True <- dbUpdate $ ReallyDeleteDocument user did actor
              Just doc <- dbQuery $ GetDocumentByDocumentID did
              return doc
            case getSigLinkFor doc user of
              Just sl -> addSignStatPurgeEvent doc sl (ctxtime ctx)
              _ -> return False)
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
    pageArchive tostime

showPadDeviceArchive :: Kontrakcja m => m (Either KontraLink String)
showPadDeviceArchive = checkUserTOSGet $ (guardJustM $ ctxmaybeuser <$> getContext) >> pagePadDeviceArchive

jsonDocumentsList ::  Kontrakcja m => m (Either KontraLink (Either CSV JSValue))
jsonDocumentsList = withUserGet $ do
  Just user@User{userid = uid} <- ctxmaybeuser <$> getContext
  lang <- getLang . ctxlocale <$> getContext
  doctype <- getField' "documentType"
  params <- getListParamsNew
  let (domain,filters1) = case doctype of
                          "Document"          -> ([DocumentsForSignatoryDeleteValue uid False] ++ (maybeCompanyDomain False),[])
                          "Template"          -> ([TemplatesOfAuthorDeleteValue uid False] ++ [TemplatesSharedInUsersCompany uid],[])
                          "Rubbish"           -> ([DocumentsForSignatoryDeleteValue uid True] ++ (maybeCompanyDomain True), [])
                          "Pad"               -> ([DocumentsOfAuthorDeleteValue uid True]  ++ (maybeCompanyDomain False) ,[DocumentFilterByIdentification PadIdentification, DocumentFilterStatuses [Pending,Closed]])
                          _ -> ([DocumentsForSignatoryDeleteValue uid False] ++ [TemplatesOfAuthorDeleteValue uid False],[])
                         where
                             maybeCompanyDomain d = if (useriscompanyadmin user && (isJust $ usercompany user))
                                                   then [DocumentsOfCompany (fromJust $ usercompany user) False d]
                                                   else []
      filters2 = concatMap fltSpec (listParamsFilters params)
      fltSpec ("process", "contract") = [DocumentFilterByProcess [Contract]]
      fltSpec ("process", "order") = [DocumentFilterByProcess [Order]]
      fltSpec ("process", "offer") = [DocumentFilterByProcess [Offer]]
      fltSpec ("time", tostr) = case reads tostr of
                                    (((Just from',Just to'),""):_) -> [DocumentFilterByMonthYearFrom from',DocumentFilterByMonthYearTo to']
                                    (((Nothing ,Just to'),""):_) -> [DocumentFilterByMonthYearTo to']
                                    (((Just from',Nothing),""):_)   -> [DocumentFilterByMonthYearFrom from']
                                    _ -> []
      fltSpec ("status", scstr) = case reads scstr of
                                    ((statusclasss,""):_) -> [DocumentFilterByStatusClass statusclasss]
                                    _ -> []
      fltSpec _ = []
  let sorting    = docSortingFromParams params
      searching  = docSearchingFromParams params
      pagination = docPaginationFromParams params
      filters = filters1 ++ filters2
  cttime <- getMinutesTime
  padqueue <- dbQuery $ GetPadQueue $ userid user
  format <- getField "format"
  case format of
       Just "csv" -> do
          allDocs <- dbQuery $ GetDocuments domain (searching ++ filters) sorting (DocumentPagination 0 maxBound)
          let docsCSVs = concat $ zipWith (docForListCSV (timeLocaleForLang lang)) [1..maxBound] allDocs
          return $ Left $ CSV { csvFilename = "documents.csv"
                              , csvHeader = docForListCSVHeader
                              , csvContent = docsCSVs
                              }
       _ -> do
          allDocs <- dbQuery $ GetDocuments domain (searching ++ filters) sorting pagination
          let docs = PagedList {  list       = allDocs
                                , params     = params
                                , pageSize   = docsPageSize
                                }
          docsJSONs <- mapM (docForListJSON (timeLocaleForLang lang) cttime user padqueue) $ take docsPageSize $ list docs
          return $ Right $ JSObject $ toJSObject [
              ("list", JSArray docsJSONs)
            , ("paging", pagingParamsJSON docs)
            ]

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
    -- x "partnercomp"    = viewComparing partnerComps
    -- x "partnercompREV" = viewComparingRev partnerComps
    x "process"           = [Asc DocumentOrderByProcess]
    x "processREV"        = [Desc DocumentOrderByProcess]
    x "type"              = [Asc DocumentOrderByType]
    x "typeREV"           = [Desc DocumentOrderByType]
    x "author"            = [Asc DocumentOrderByAuthor]
    x "authorRev"         = [Desc DocumentOrderByAuthor]
    x _                   = []



docSearchingFromParams :: ListParams -> [DocumentFilter]
docSearchingFromParams params =
  case listParamsSearching params of
    "" -> []
    x -> [DocumentFilterByString x]


docPaginationFromParams :: ListParams -> DocumentPagination
docPaginationFromParams params = DocumentPagination (listParamsOffset params) (listParamsLimit params)

#if 0
-- this needs to be transferred to SQL
{- |
    Special comparison for partners, because we need to compare each signatory,
    and also then inside the signatory compare the fst and snd names separately.
-}
comparePartners :: Document -> Document -> Ordering
comparePartners doc1 doc2 =
  case (dropWhile isMatch $ zipWith compareSignatory (getSignatoryPartnerLinks doc1) (getSignatoryPartnerLinks doc2)) of
    [] -> EQ
    (x:_) -> x
  where
    isMatch :: Ordering -> Bool
    isMatch EQ = True
    isMatch _ = False
    compareSignatory :: SignatoryLink -> SignatoryLink -> Ordering
    compareSignatory sl1 sl2 =
      let splitUp = span (\c -> c/=' ') . map toUpper . getSmartName
          (fst1, snd1) = splitUp sl1
          (fst2, snd2) = splitUp sl2 in
      case (compare fst1 fst2) of
        EQ -> compare snd1 snd2
        x -> x
#endif

docsPageSize :: Int
docsPageSize = 100

-- Zip utils

docToEntry ::  Kontrakcja m => Document -> m (Maybe Entry)
docToEntry doc = do
      let name = filter ((/= ' ')) $ filter (isAscii) $ (documenttitle doc) ++ "_" ++ (show $ documentid doc) ++".pdf"
      case (documentsealedfiles doc ++ documentfiles doc) of
        (fid:_) -> do
            content <- getFileIDContents fid
            return $ Just $ toEntry name 0 $ BSL.pack $ BSS.unpack content
        [] -> return Nothing
