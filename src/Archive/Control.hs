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
       jsonDocumentsList,
       )
       where

import Archive.View

import InputValidation
import KontraLink
import Kontra
import DB.Classes
import Doc.DocStateData
import Doc.Model
import User.Model
import User.Utils
import Util.FlashUtil
import Util.MonadUtils

import Control.Applicative
import Util.SignatoryLinkUtils
import Stats.Control
import Data.Char
import Data.List
import EvidenceLog.Model
import Util.HasSomeUserInfo
import Text.JSON
import qualified Log as Log
import ListUtil
import MinutesTime
import Doc.DocView
import Misc
import Util.HasSomeCompanyInfo

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
    let actor = UserActor ctxtime ctxipnumber (userid user) (getEmail user)
    mapM_ (\did -> do 
              doc <- guardRightM' $ runDBUpdate $ ArchiveDocument user did actor
              case getSigLinkFor doc user of
                Just sl -> runDB $ addSignStatDeleteEvent doc sl ctxtime
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
  allDocs <- case (doctype) of
    "Contract" -> runDBQuery $ GetDocumentsOfTypeBySignatory (Signable Contract) uid
    "Offer" -> runDBQuery $ GetDocumentsOfTypeBySignatory (Signable Offer) uid
    "Order" -> runDBQuery $ GetDocumentsOfTypeBySignatory (Signable Order) uid
    "Template" -> runDBQuery $ GetTemplatesByAuthor uid
    "Attachment" -> runDBQuery $ GetDocumentsOfTypeByAuthor Attachment uid
    "Rubbish" -> runDBQuery $ GetDeletedDocumentsByUser uid
    "Template|Contract" -> filter (\d -> documenttype d == Template Contract) <$> (runDBQuery $ GetAvaibleTemplates  uid)
    "Template|Offer" ->  filter (\d -> documenttype d == Template Offer) <$>  (runDBQuery $ GetAvaibleTemplates  uid)
    "Template|Order" -> filter (\d -> documenttype d == Template Order) <$> (runDBQuery $ GetAvaibleTemplates  uid)
    _ -> do
      Log.error "Documents list: No valid document type provided"
      return []
  Log.debug $ "Documents list: Number of documents found "  ++  (show $ length allDocs)
  params <- getListParamsNew
  let docs = docSortSearchPage params allDocs
  cttime <- getMinutesTime
  docsJSONs <- mapM (fmap JSObject . docForListJSON (timeLocaleForLang lang) cttime user) $ list docs
  return $ JSObject $ toJSObject [
      ("list", JSArray docsJSONs)
    , ("paging", pagingParamsJSON docs)
    ]


-- Searching, sorting and paging
docSortSearchPage :: ListParams -> [Document] -> PagedList Document
docSortSearchPage  = listSortSearchPage docSortFunc docSearchFunc docsPageSize

docSearchFunc::SearchingFunction Document
docSearchFunc s doc =  nameMatch doc || signMatch doc
    where
    match m = isInfixOf (map toUpper s) (map toUpper m)
    nameMatch = match . documenttitle
    signMatch d = any (match . getSmartName) (documentsignatorylinks d)


docSortFunc:: SortingFunction Document
docSortFunc "status" = compareStatus
docSortFunc "statusREV" = revCompareStatus
docSortFunc "title" = viewComparing documenttitle
docSortFunc "titleREV" = viewComparingRev documenttitle
docSortFunc "time" = viewComparing documentmtime
docSortFunc "timeREV" = viewComparingRev documentmtime
docSortFunc "party" = comparePartners
docSortFunc "partyREV" = revComparePartners
docSortFunc "partner" = comparePartners
docSortFunc "partnerREV" = revComparePartners
docSortFunc "partnercomp" = viewComparing partnerComps
docSortFunc "partnercompREV" = viewComparingRev partnerComps
docSortFunc "process" = viewComparing documenttype
docSortFunc "processREV" = viewComparingRev documenttype
docSortFunc "type" = viewComparing documenttype
docSortFunc "typeREV" = viewComparingRev documenttype
docSortFunc "author" = viewComparing getAuthorName
docSortFunc "authorREV" = viewComparingRev getAuthorName
docSortFunc _ = const $ const EQ

partnerComps :: Document -> String
partnerComps = concatMap getCompanyName . getSignatoryPartnerLinks

revCompareStatus :: Document -> Document -> Ordering
revCompareStatus doc1 doc2 = compareStatus doc2 doc1

compareStatus :: Document -> Document -> Ordering
compareStatus doc1 doc2 = compare (documentStatusClass doc1) (documentStatusClass doc2)

revComparePartners :: Document -> Document -> Ordering
revComparePartners doc1 doc2 = comparePartners doc2 doc1

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

docsPageSize :: Int
docsPageSize = 100  