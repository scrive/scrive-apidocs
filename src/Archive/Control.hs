{-# LANGUAGE CPP #-}
module Archive.Control
       (
       handleAttachmentArchive,
       handleContractArchive,
       handleIssueArchive,
       handleOffersArchive,
       handleOrdersArchive,
       handleSignableArchive,
       handleTemplateArchive,
       showAttachmentList,
       showContractsList,
       showOfferList,
       showOrdersList,
       showRubbishBinList,
       showTemplatesList,
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
import Util.FlashUtil
import Util.MonadUtils

import Control.Applicative
import Util.SignatoryLinkUtils
import Stats.Control
import EvidenceLog.Model
import Util.HasSomeUserInfo
import Text.JSON
import qualified Log as Log
import ListUtil
import MinutesTime
import Misc
import PadQueue.Model
import Data.Maybe

handleContractArchive :: Kontrakcja m => m KontraLink
handleContractArchive = do
    _ <- handleSignableArchive (Signable Contract)
    return $ LinkContracts

handleOffersArchive :: Kontrakcja m => m KontraLink
handleOffersArchive =  do
    _ <- handleSignableArchive (Signable Offer)
    return $ LinkOffers

handleOrdersArchive :: Kontrakcja m => m KontraLink
handleOrdersArchive =  do
    _ <- handleSignableArchive (Signable Order)
    return $ LinkOrders

handleSignableArchive :: Kontrakcja m => DocumentType -> m ()
handleSignableArchive doctype =  do
    handleIssueArchive
    addFlashM $ flashMessageSignableArchiveDone doctype
    return ()

handleTemplateArchive :: Kontrakcja m => m KontraLink
handleTemplateArchive = do
    handleIssueArchive
    addFlashM flashMessageTemplateArchiveDone
    return $ LinkTemplates

handleAttachmentArchive :: Kontrakcja m => m KontraLink
handleAttachmentArchive = do
    handleIssueArchive
    addFlashM flashMessageAttachmentArchiveDone
    return $ LinkAttachments

handleIssueArchive :: Kontrakcja m => m ()
handleIssueArchive = do
    Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber } <- getContext
    docids <- getCriticalFieldList asValidDocID "doccheck"
    let actor = userActor ctxtime ctxipnumber (userid user) (getEmail user)
    mapM_ (\did -> do
              doc <- guardRightM' $ dbUpdate $ ArchiveDocument user did actor
              case getSigLinkFor doc user of
                Just sl -> addSignStatDeleteEvent doc sl ctxtime
                _ -> return False) 
      docids

{- |
   Constructs a list of documents (Arkiv) to show to the user.
 -}
showContractsList :: Kontrakcja m => m (Either KontraLink String)
showContractsList = archivePage pageContractsList

showOfferList :: Kontrakcja m => m (Either KontraLink String)
showOfferList = archivePage pageOffersList

showOrdersList :: Kontrakcja m => m (Either KontraLink String)
showOrdersList = archivePage pageOrdersList

showTemplatesList :: Kontrakcja m => m (Either KontraLink String)
showTemplatesList = archivePage pageTemplatesList

showAttachmentList :: Kontrakcja m => m (Either KontraLink String)
showAttachmentList = archivePage pageAttachmentList

showRubbishBinList :: Kontrakcja m => m (Either KontraLink String)
showRubbishBinList = archivePage pageRubbishBinList

showPadDeviceArchive :: Kontrakcja m => m (Either KontraLink String)
showPadDeviceArchive = archivePage pagePadDeviceArchive

{- |
    Helper function for showing lists of documents.
-}
archivePage :: Kontrakcja m => (User -> m String) -> m (Either KontraLink String)
archivePage page = checkUserTOSGet $ (guardJustM $ ctxmaybeuser <$> getContext) >>= page


jsonDocumentsList ::  Kontrakcja m => m (Either KontraLink JSValue)
jsonDocumentsList = withUserGet $ do
  Just user@User{userid = uid} <- ctxmaybeuser <$> getContext
  lang <- getLang . ctxlocale <$> getContext
  doctype <- getField' "documentType"

#if 0
  allDocs <- case (doctype) of
    "Contract" -> dbQuery $ GetDocumentsBySignatory [Contract] uid
    "Offer" -> dbQuery $ GetDocumentsBySignatory [Offer] uid
    "Order" -> dbQuery $ GetDocumentsBySignatory [Order] uid
    "Template" -> dbQuery $ GetTemplatesByAuthor uid
    "Attachment" -> dbQuery $ GetAttachmentsByAuthor uid
    "Rubbish" -> dbQuery $ GetDeletedDocumentsByUser uid
    "Template|Contract" -> dbQuery $ GetAvailableTemplates uid [Contract]
    "Template|Offer" ->  dbQuery $ GetAvailableTemplates uid [Offer]
    "Template|Order" -> dbQuery $ GetAvailableTemplates uid [Order]
    "Pad" -> (dbQuery $ GetDocumentsByAuthor [Contract,Offer,Order] uid) -- Not working and disabled. It should do filtering of only pad documents.
    _ -> do
      Log.error "Documents list: No valid document type provided"
      return []
#endif

  params <- getListParamsNew

  let (domain,filters) = case doctype of
                          "Contract"          -> ([DocumentsForSignatory uid] ++ (maybeCompanyDomain False),[DocumentFilterByProcess [Contract]])
                          "Offer"             -> ([DocumentsForSignatory uid] ++ (maybeCompanyDomain False),[DocumentFilterByProcess [Offer]])
                          "Order"             -> ([DocumentsForSignatory uid] ++ (maybeCompanyDomain False),[DocumentFilterByProcess [Order]])
                          "Template"          -> ([TemplatesOfAuthor uid],[])
                          "Attachment"        -> ([AttachmentsOfAuthorDeleteValue uid False, AttachmentsSharedInUsersCompany uid],[])
                          "Rubbish"           -> ([DocumentsForSignatoryDeleteValue uid True] ++ (maybeCompanyDomain True), [])
                          "Template|Contract" -> ([TemplatesOfAuthor uid, TemplatesSharedInUsersCompany uid],[DocumentFilterByProcess [Contract]])
                          "Template|Offer"    -> ([TemplatesOfAuthor uid, TemplatesSharedInUsersCompany uid],[DocumentFilterByProcess [Offer]])
                          "Template|Order"    -> ([TemplatesOfAuthor uid, TemplatesSharedInUsersCompany uid],[DocumentFilterByProcess [Order]])
                          "Pad"               -> ([DocumentsOfAuthor uid],[DocumentFilterByIdentification PadIdentification, DocumentFilterStatuses [Pending,Closed]])
                          _ -> ([],[])
                         where
                             maybeCompanyDomain d = if (useriscompanyadmin user && (isJust $ usercompany user))
                                                   then [DocumentsOfCompany (fromJust $ usercompany user) False d]
                                                   else []

  let sorting    = docSortingFromParams params
      searching  = docSearchingFromParams params
      pagination = docPaginationFromParams docsPageSize params

  allDocs <- dbQuery $ GetDocuments domain (searching ++ filters) sorting pagination
  totalCount <- dbQuery $ GetDocumentsCount domain (searching ++ filters)

  Log.debug $ "Documents list: Number of documents found "  ++  (show $ length allDocs)

  let docs = PagedList { list       = allDocs
                       , params     = params
                       , totalCount = totalCount
                       , pageSize   = docsPageSize
                       }

  cttime <- getMinutesTime
  padqueue <- dbQuery $ GetPadQueue $ userid user
  docsJSONs <- mapM (docForListJSON (timeLocaleForLang lang) cttime user padqueue) $ list docs
  return $ JSObject $ toJSObject [
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
    -- x "author"         = viewComparing getAuthorName
    -- x "authorRev"      = viewComparingRev getAuthorName
    x _                   = []



docSearchingFromParams :: ListParams -> [DocumentFilter]
docSearchingFromParams params =
  case listParamsSearching params of
    "" -> []
    x -> [DocumentFilterByString x]


docPaginationFromParams :: Int -> ListParams -> DocumentPagination
docPaginationFromParams pageSize params = DocumentPagination ((listParamsPage params - 1) * pageSize) pageSize

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

