{-# OPTIONS_GHC -Wall #-}

module Doc.DocView (
    emptyDetails
  , showFilesImages2
  , pageDocumentDesign
  , pageDocumentForAuthor
  , pageDocumentForViewer
  , pageDocumentForSignatory
  , docSortSearchPage
  , docAndAuthorSortSearchPage
  , pageContractsList
  , pageTemplatesList
  , pageOffersList
  , modalSignInviteView
  , modalSendInviteView
  , modalContractSignedHasAccount
  , modalContractSignedNoAccount
  , modalLoginForSaveView
  , modalOfferCreated
  , modalOfferSignedHasAccount
  , modalOfferSignedNoAccount
  , modalSignAwaitingAuthorLast
  , modalRejectedView
  , flashRemindMailSent
  , flashMessageCanceled
  , flashDocumentRestarted
  , flashDocumentDraftSaved
  , flashDocumentTemplateSaved
  , flashAuthorSigned
  , flashMessageFailedToParseCSV
  , flashMessageCSVHasTooManyRows
  , flashMessageBulkContractRemindsSent
  , flashMessageNoBulkContractRemindsSent
  , flashMessageBulkOfferRemindsSent
  , flashMessageNoBulkOfferRemindsSent
  , flashMessageContractArchiveDone
  , flashMessageOfferArchiveDone
  , flashMessageTemplateArchiveDone
  , flashMessageInvalidCSV
  , flashMessageCSVSent
  , flashMessageSingleTemplateShareDone
  , flashMessageMultipleTemplateShareDone
  , flashMessageAccountActivatedFromSign
  , flashMessageAccountRemovedFromSign
  , defaultInviteMessage
  , mailDocumentRemind
  , mailDocumentRejected
  , mailDocumentAwaitingForAuthor
  , mailCancelDocumentByAuthorContent
  , mailCancelDocumentByAuthor
  , mailInvitationToSign
  , mailInvitationToSend
  , mailDocumentClosedForSignatories
  , mailDocumentClosedForAuthor
  , isNotLinkForUserID
  , signatoryDetailsFromUser
  , uploadPage
  , templatesForAjax
  , getDataMismatchMessage
  , documentInfoFields
  , documentAuthorInfo
  ) where

import ActionSchedulerState (ActionID)
import Control.Applicative ((<$>))
import Data.List (find, isInfixOf)
import Data.Maybe
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Doc.CSVUtils
import Doc.DocState
import Doc.DocViewMail
import Doc.DocViewUtil
import Doc.DocStateUtils
import Kontra
import KontraLink
import Mails.MailsUtil
import MinutesTime
import Misc
import Templates.Templates
import Templates.TemplatesUtils
import User.UserView (prettyName)
import ListUtil
import Control.Monad.Reader

modalSignAwaitingAuthorLast :: KontraModal
modalSignAwaitingAuthorLast = do
    renderTemplateM "signAwaitingAuthorLast" ()

modalSignInviteView :: Document -> KontraModal
modalSignInviteView document = do
  templates <- ask
  partylist <- lift $ renderListTemplate templates . map (BS.toString . personname') $ partyListButAuthor document
  lift $ renderTemplate templates "modalSignInviteView" $ do
    field "partyListButAuthor" partylist
    documentInfoFields document

modalSendInviteView ::  Document -> KontraModal
modalSendInviteView document = do
  templates <- ask  
  partylist <- lift $ renderListTemplate templates . map (BS.toString . personname') $ partyListButAuthor document
  lift $ renderTemplate templates  "modalSendInviteView" $ do
    field "partyListButAuthor" partylist
    field "documenttitle" . BS.toString $ documenttitle document

modalRejectedView :: Document -> KontraModal
modalRejectedView document = do
  templates <- ask
  partylist <-lift $ renderListTemplate templates . map (BS.toString . personname') $ partyList document
  lift $ renderTemplate templates "modalRejectedView" $ do
    field "partyList" partylist
    field "documenttitle" . BS.toString $ documenttitle document

modalLoginForSaveView :: KontraModal
modalLoginForSaveView = do
  templates <- ask
  lift $ renderTemplate templates "modalLoginForSaveView" ()

modalContractSignedHasAccount ::  Document -> SignatoryLink -> Bool -> KontraModal
modalContractSignedHasAccount document signatorylink isloggedin = do
  modalContractSigned' "modalSignedViewClosedHasAccount" 
                       "modalSignedViewNotClosedHasAccount" 
                       document
                       (loginFields document signatorylink isloggedin)

modalContractSignedNoAccount ::  Document -> SignatoryLink -> ActionID -> MagicHash -> KontraModal
modalContractSignedNoAccount document signatorylink actionid magichash =
  modalContractSigned' "modalSignedViewClosedNoAccount" 
                       "modalSignedViewNotClosedNoAccount" 
                       document
                       (accountFromSignFields document signatorylink actionid magichash)
         
modalContractSigned' ::  String -> String -> Document -> Fields -> KontraModal
modalContractSigned' closedtemplate notclosedtemplate document@Document{documentstatus} extrafields = do
  templates <- ask   
  if documentstatus == Closed
     then
       lift $ renderTemplate templates closedtemplate $ do
         field "partyListString" . renderListTemplate templates . map (BS.toString . personname') $ partyList document
         basicContractSignedFields document
         extrafields
     else
       lift $ renderTemplate templates notclosedtemplate $ do
         field "partyUnsignedListString" . renderListTemplate templates . map (BS.toString . personname') $ partyUnsignedList document
         basicContractSignedFields document
         extrafields

basicContractSignedFields :: Document -> Fields
basicContractSignedFields document@Document{documenttitle} = do
    field "documenttitle" $ BS.toString $ documenttitle

loginFields :: Document -> SignatoryLink -> Bool -> Fields
loginFields document signatorylink isloggedin = do
    field "isloggedin" isloggedin
    field "referer" $ show (LinkSignDoc document signatorylink)
    field "email" . signatoryemail $ signatorydetails signatorylink
    field "linklogin" $ show (LinkLogin LoginTry)

modalOfferCreated::  Document -> KontraModal
modalOfferCreated document = do
    templates <- ask   
    lift $ renderTemplate templates "modalOfferCreated" $ do
        field "documenttitle" . BS.toString $ documenttitle document      
        field "signatory" . listToMaybe $ map (BS.toString . personname') $ partyList document
        
modalOfferSignedHasAccount ::  Document -> KontraModal
modalOfferSignedHasAccount document = do
    templates <- ask   
    lift $ renderTemplate templates "modalOfferSignedHasAccount" $ do
        offerSignedFields document
        
modalOfferSignedNoAccount ::  Document -> SignatoryLink -> ActionID -> MagicHash -> KontraModal
modalOfferSignedNoAccount document siglink actionid magichash = do
    templates <- ask   
    lift $ renderTemplate templates "modalOfferSignedNoAccount" $ do
        offerSignedFields document
        accountFromSignFields document siglink actionid magichash     

offerSignedFields :: Document -> Fields
offerSignedFields document = do
    field "documenttitle" . BS.toString $ documenttitle document      
    field "signatory" . listToMaybe $ map (BS.toString . signatoryemail ) $ partyList document

accountFromSignFields :: Document -> SignatoryLink -> ActionID -> MagicHash -> Fields
accountFromSignFields document signatorylink actionid magichash = do
    field "linkaccountfromsign" $ show (LinkAccountFromSign document signatorylink actionid magichash)

flashDocumentDraftSaved :: KontrakcjaTemplates -> IO FlashMessage
flashDocumentDraftSaved templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashDocumentDraftSaved" ()


flashDocumentTemplateSaved :: KontrakcjaTemplates -> IO FlashMessage
flashDocumentTemplateSaved templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashDocumentTemplateSaved" ()

flashDocumentRestarted :: KontrakcjaTemplates -> Document -> IO FlashMessage
flashDocumentRestarted templates document =
  fmap (toFlashMsg OperationDone) $ 
      renderTemplate templates "flashDocumentRestarted" $ do
      documentInfoFields document

flashRemindMailSent :: KontrakcjaTemplates -> SignatoryLink -> IO FlashMessage
flashRemindMailSent templates signlink@SignatoryLink{maybesigninfo} =
  toFlashMsg OperationDone <$> (renderTemplate templates (template_name maybesigninfo) $ do
    field "personname" . BS.toString $ personname signlink)
  where
    template_name =
      maybe "flashRemindMailSentNotSigned"
      (const "flashRemindMailSentSigned")


flashMessageCanceled :: KontrakcjaTemplates -> Document -> IO FlashMessage
flashMessageCanceled templates document =
  fmap (toFlashMsg SigningRelated) $ 
    renderTemplate templates "flashMessageCanceled" $ do
      documentInfoFields document

flashAuthorSigned :: KontrakcjaTemplates -> IO FlashMessage
flashAuthorSigned templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashAuthorSigned" ()

flashMessageFailedToParseCSV :: KontrakcjaTemplates -> IO FlashMessage
flashMessageFailedToParseCSV templates = 
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageFailedToParseCSV" ()

flashMessageCSVHasTooManyRows :: Int -> KontrakcjaTemplates -> IO FlashMessage
flashMessageCSVHasTooManyRows maxrows templates = 
  toFlashMsg OperationFailed <$> (renderTemplate templates "flashMessageCSVHasTooManyRows" $ field "maxrows" maxrows)

flashMessageBulkContractRemindsSent :: KontrakcjaTemplates -> IO FlashMessage
flashMessageBulkContractRemindsSent templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageBulkContractRemindsSent" ()

flashMessageNoBulkContractRemindsSent :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNoBulkContractRemindsSent templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageNoBulkContractRemindsSent" ()

flashMessageBulkOfferRemindsSent :: KontrakcjaTemplates -> IO FlashMessage
flashMessageBulkOfferRemindsSent templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageBulkOfferRemindsSent" ()

flashMessageNoBulkOfferRemindsSent :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNoBulkOfferRemindsSent templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageNoBulkOfferRemindsSent" ()

flashMessageContractArchiveDone :: KontrakcjaTemplates -> IO FlashMessage
flashMessageContractArchiveDone templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageContractArchiveDone" ()

flashMessageOfferArchiveDone :: KontrakcjaTemplates -> IO FlashMessage
flashMessageOfferArchiveDone templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageOfferArchiveDone" ()

flashMessageTemplateArchiveDone :: KontrakcjaTemplates -> IO FlashMessage
flashMessageTemplateArchiveDone templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageTemplateArchiveDone" ()

flashMessageInvalidCSV :: KontrakcjaTemplates -> IO FlashMessage
flashMessageInvalidCSV templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageInvalidCSV" ()

flashMessageCSVSent :: Int -> KontrakcjaTemplates -> IO FlashMessage
flashMessageCSVSent doccount templates =
  toFlashMsg OperationDone <$> (renderTemplate templates "flashMessageCSVSent" $ field "doccount" doccount)

flashMessageSingleTemplateShareDone :: BS.ByteString -> KontrakcjaTemplates -> IO FlashMessage
flashMessageSingleTemplateShareDone docname templates =
  toFlashMsg OperationDone <$> (renderTemplate templates "flashMessageSingleTemplateShareDone" $ field "docname" docname)

flashMessageMultipleTemplateShareDone :: KontrakcjaTemplates -> IO FlashMessage
flashMessageMultipleTemplateShareDone templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageMultipleTemplateShareDone" ()

flashMessageAccountActivatedFromSign :: KontrakcjaTemplates -> IO FlashMessage
flashMessageAccountActivatedFromSign templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageAccountActivatedFromSign" ()

flashMessageAccountRemovedFromSign :: KontrakcjaTemplates -> IO FlashMessage
flashMessageAccountRemovedFromSign templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageAccountRemovedFromSign" ()


-- All doc view
singlnkFields :: Document -> (MinutesTime -> String) -> SignatoryLink -> Fields
singlnkFields document dateformatter sl = do
  field "id" $ show $ signatorylinkid sl
  field "name" $ BS.toString $ personname sl
  field "email" $  ""
  field "company" $ BS.toString . signatorycompany $ signatorydetails sl
  signatoryStatusFields document sl dateformatter

{- |
    We want the documents to be ordered like the icons in the bottom
    of the document list.  So this means:
    0 Draft - 1 Cancel - 2 Fall due - 3 Sent - 4 Opened - 5 Signed
-}
data StatusClass = SCDraft
                  | SCCancelled
                  | SCTimedout
                  | SCSent
                  | SCRead
                  | SCOpened
                  | SCSigned
                  deriving (Eq, Ord)

instance Show StatusClass where
  show SCDraft = "draft"
  show SCCancelled = "cancelled"
  show SCTimedout = "expired"
  show SCSent = "sent"
  show SCRead = "read"
  show SCOpened = "opened"
  show SCSigned = "signed"


documentStatusClass ::Document -> StatusClass
documentStatusClass doc = 
    case documentstatus doc of
         Preparation    -> SCDraft
         Closed         -> SCSigned
         Canceled       -> SCCancelled
         Timedout       -> SCTimedout
         Rejected       -> SCCancelled
         Pending        -> statusWhenPending
         AwaitingAuthor -> statusWhenPending
         _              -> SCCancelled
    where
      statusWhenPending =
        let allReadInvite = all (isJust . maybereadinvite) $ documentsignatorylinks doc
            allSeen = all (isJust . maybeseeninfo) $ documentsignatorylinks doc in
        case (anyInvitationUndelivered doc, allReadInvite, allSeen) of
          (True, _, _) -> SCCancelled
          (_, _, True) -> SCOpened
          (_, True, _) -> SCRead
          _ -> SCSent
      

documentBasicViewFields :: MinutesTime -> User -> Document -> Fields
documentBasicViewFields crtime user doc = do
    documentInfoFields doc
    field "status" $ show (documentStatusClass doc)
    field "signatories" $ map (singlnkFields doc (showDateAbbrev crtime)) $ filter isSignatory $ documentsignatorylinks doc
    field "anyinvitationundelivered" $ anyInvitationUndelivered doc
    field "doclink"  $ if (unAuthor $ documentauthor doc) == userid user || null signatorylinklist
                        then show . LinkIssueDoc $ documentid doc
                        else show $ LinkSignDoc doc (head signatorylinklist)
    field "davelink" $ if isSuperUser (Just user)
                        then Just $ "/dave/document/" ++ (show $ documentid doc)
                        else Nothing
    field "timeoutdate" $ fromTimeout show
    field "timeoutdaysleft" $ fromTimeout $ show . (dateDiffInDays crtime)
    field "mtime" $ showDateAbbrev crtime (documentmtime doc)
    field "isauthor" $ isAuthor doc user
    field "isviewer" $ isViewer doc user
    field "isshared" $ (documentsharing doc)==Shared
    field "isoffer" $ isOffer doc
  where
    signatorylinklist =
      filter (isMatchingSignatoryLink user) $ documentsignatorylinks doc  
    
    fromTimeout f =
      case (documenttimeouttime doc, documentstatus doc) of
           (Just (TimeoutTime x), Pending) -> Just $ f x
           _                               -> Nothing



-- Searching, sorting and paging
docSortSearchPage :: ListParams -> [Document] -> PagedList Document
docSortSearchPage  = listSortSearchPage docSortFunc docSearchFunc docsPageSize

docSearchFunc::SearchingFunction Document
docSearchFunc s doc =  nameMatch doc || signMatch doc
    where
    match m = isInfixOf (map toUpper s) (map toUpper m)
    nameMatch = match . BS.toString . documenttitle
    signMatch d = any match $ map (BS.toString . personname) (documentsignatorylinks d)
    
   
docSortFunc:: SortingFunction Document
docSortFunc "status" = compareStatus
docSortFunc "statusREV" = revCompareStatus
docSortFunc "title" = viewComparing documenttitle
docSortFunc "titleREV" = viewComparingRev documenttitle
docSortFunc "time" = viewComparing documentmtime
docSortFunc "timeREV" = viewComparingRev documentmtime
docSortFunc "partner" = comparePartners 
docSortFunc "partnerREV" = revComparePartners
docSortFunc "partnercomp" = viewComparing partnerComps
docSortFunc "partnercompREV" = viewComparingRev partnerComps
docSortFunc "type" = viewComparing documenttype
docSortFunc "typeREV" = viewComparingRev documenttype
docSortFunc _ = const $ const EQ

partnerComps :: Document -> BS.ByteString
partnerComps doc = BS.concat . map (signatorycompany . signatorydetails) . documentsignatorylinks $ doc

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
  case (dropWhile isMatch $ zipWith compareSignatory (documentsignatorylinks doc1) (documentsignatorylinks doc2)) of
    [] -> EQ
    (x:_) -> x
  where
    isMatch :: Ordering -> Bool
    isMatch EQ = True
    isMatch _ = False
    compareSignatory :: SignatoryLink -> SignatoryLink -> Ordering
    compareSignatory sl1 sl2 =
      let splitUp sl = span (\c -> c/=' ') . map toUpper . BS.toString $ personname sl
          (fst1, snd1) = splitUp sl1
          (fst2, snd2) = splitUp sl2 in
      case (compare fst1 fst2) of
        EQ -> compare snd1 snd2
        x -> x

docsPageSize :: Int
docsPageSize = 100
 

--

pageContractsList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageContractsList templates ctime user documents =
  renderTemplate templates "pageContractsList" $ do
    field "documents" $ markParity $ map (documentBasicViewFields ctime user) $ list documents
    pagedListFields documents
    field "currentlink" $ show $ LinkContracts $ params documents
        

pageTemplatesList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageTemplatesList templates ctime user documents =
 renderTemplate templates "pageTemplatesList" $ do
    field "documents" $ markParity $ map (documentBasicViewFields ctime user) $ list documents
    pagedListFields documents
    field "currentlink" $ show $ LinkTemplates $ params documents
    
pageOffersList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList (Document, User) -> IO String
pageOffersList templates ctime user documents =
  renderTemplate templates "pageOffersList" $ do
    field "documents" $ markParity $ map (docAndAuthorBasicViewFields ctime user) $ list documents
    pagedListFields documents
    field "currentlink" $ show $ LinkOffers $ params documents
  where
    docAndAuthorBasicViewFields :: MinutesTime -> User -> (Document, User) -> Fields
    docAndAuthorBasicViewFields crtime user (doc, author) = do
      documentBasicViewFields crtime user doc
      field "authorname" $ authorname author

authorname :: User -> BS.ByteString        
authorname author = 
  if (BS.null authorfstname && BS.null authorsndname)
    then authoremail
    else BS.concat [authorfstname, BS.fromString " ", authorsndname]
  where
    authorfstname = userfstname $ userinfo author
    authorsndname = usersndname $ userinfo author
    authoremail = unEmail . useremail $ userinfo author

-- Searching, sorting and paging for document author pairs
docAndAuthorSortSearchPage :: ListParams -> [(Document, User)] -> PagedList (Document, User)
docAndAuthorSortSearchPage  = listSortSearchPage docAndAuthorSortFunc docAndAuthorSearchFunc docsPageSize

docAndAuthorSearchFunc::SearchingFunction (Document, User)
docAndAuthorSearchFunc s (doc, author) =  docSearchFunc s doc || nameMatch author
  where
    match m = isInfixOf (map toUpper s) (map toUpper m)
    nameMatch = match . BS.toString . authorname
   
docAndAuthorSortFunc:: SortingFunction (Document, User)
docAndAuthorSortFunc "author" (_,author1) (_,author2) = viewComparing authorname author1 author2
docAndAuthorSortFunc "authorRev" (_,author1) (_,author2) = viewComparingRev authorname author1 author2
docAndAuthorSortFunc x (doc1,_) (doc2,_) = docSortFunc x doc1 doc2

showFileImages :: KontrakcjaTemplates -> DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> File -> JpegPages -> IO String
showFileImages templates _ _ _ JpegPagesPending =
  renderTemplate templates  "showFileImagesPending" ()

showFileImages templates _ _ _ (JpegPagesError normalizelog) =
  renderTemplate templates "showFileImagesError" $ do
    field "normalizelog" $ BS.toString normalizelog

showFileImages templates docid mtokens File{fileid} (JpegPages jpgpages) =
  renderTemplate templates "showFileImagesReady" $ do
    field "pageurl" $ "/pages/" ++ pageurl mtokens
    field "images" . map page $ zip [1,2..] jpgpages
  where
    pageurl Nothing =  show docid ++ "/" ++ show fileid
    pageurl (Just (siglinkid, sigmagichash)) =
           show docid ++ "/" ++ show siglinkid ++ "/"
        ++ show sigmagichash ++ "/" ++ show fileid
    page :: (Int,(a,Int,Int)) -> Fields
    page (x,(_,w,h)) = do
      field "number" x
      field "width" w
      field "height" h

showFilesImages2 :: KontrakcjaTemplates -> DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> [(File, JpegPages)] -> IO String
showFilesImages2 templates docid mtokens files = do
  filesPages <- sequence $ map (uncurry (showFileImages templates docid mtokens)) files
  renderTemplate templates  "spanNoEscape" $ field "it" (concat filesPages)


{-

   Document is invalid
   Fel filformat
   Vi beklagar, fel filformat

   mp3 -- we cannot do anything with this document
-}

emptyDetails :: SignatoryDetails
emptyDetails =
  SignatoryDetails {
      signatoryfstname                  = BS.empty
    , signatorysndname                  = BS.empty
    , signatorycompany                  = BS.empty
    , signatorypersonalnumber           = BS.empty
    , signatorycompanynumber            = BS.empty
    , signatoryemail                    = BS.empty
    , signatorysignorder                = SignOrder 1
    , signatoryfstnameplacements        = []
    , signatorysndnameplacements        = []
    , signatorycompanyplacements        = []
    , signatorypersonalnumberplacements = []
    , signatorycompanynumberplacements  = []
    , signatoryemailplacements          = []
    , signatoryotherfields              = []
  }

{- |
   link does not belong to user with uid
 -}
isNotLinkForUserID :: UserID
                   -> SignatoryLink
                   -> Bool
isNotLinkForUserID uid link =
    hasNoUserID || notSameUserID
        where hasNoUserID = isNothing $ maybesignatory link
              notSameUserID = uid /= linkuid
              linkuid = fromJust $ maybesignatory link

pageDocumentDesign :: Context 
             -> Document 
             -> User
             -> (Maybe DesignStep)
             -> IO String
pageDocumentDesign ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentdaystosign
    , documentinvitetext
    , documentallowedidtypes
    , documentfunctionality
  }
  author step =
   let
       templates = ctxtemplates ctx
       authorid = userid author
       authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       doc_author_otherfields fields = sequence .
         map (\(fd, i) ->
           renderTemplate templates "customfield" $ do
             field "otherFieldValue" $ fieldvalue fd
             field "otherFieldName"  $ fieldlabel fd
             field "otherFieldID"    $ "field" ++ show i
             field "otherFieldOwner" "author")
             $ zip fields ([1..]::[Int])
   in do
     csvfields <- documentCsvFields templates document
     renderTemplate (ctxtemplates ctx) "pageDocumentDesign" $ do
       field "authorOtherFields" $ doc_author_otherfields $ signatoryotherfields $ documentauthordetails author document
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "authorhaslink" $ authorhaslink
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $  mailInvitationToSignOrViewContent templates False ctx document author Nothing
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue              
       field "docstate" (buildJS (documentauthordetails author document) documentsignatorylinks)
       documentAuthorInfo author
       csvfields
       documentFunctionalityFields author document
       documentInfoFields document
       documentViewFields document
       designViewFields step

documentFunctionalityFields :: User -> Document -> Fields
documentFunctionalityFields User{usersettings} Document{documenttype, documentfunctionality} = do
  field "docfunctionality" $ show documentfunctionality
  field "basiccontract" $ documenttype==Contract && documentfunctionality==BasicFunctionality

documentCsvFields :: KontrakcjaTemplates -> Document -> IO Fields
documentCsvFields templates document@Document{documentallowedidtypes, documentcsvupload} =  do
  let csvcustomfields = either (const [BS.fromString ""]) id $ getCSVCustomFields document
      mcleancsv = fmap (cleanCSVContents documentallowedidtypes (length csvcustomfields) . csvcontents) $ documentcsvupload
      csvproblems = maybe [] fst mcleancsv
      csvdata = maybe [] (csvbody . snd) mcleancsv
      csvPageSize = 10
      csvpages = splitCSVDataIntoPages csvPageSize csvdata
  csvproblemfields <- sequence $ zipWith (csvProblemFields templates (length csvproblems)) [1..] csvproblems   
  return $ do
    field "csvproblems" $ csvproblemfields
    field "csvproblemcount" $ length csvproblems
    field "csvpages" $ zipWith (csvPageFields csvproblems (length csvdata)) [0,csvPageSize..] csvpages
    field "csvrowcount" $ length csvdata
    field "csvcustomfields" $ csvcustomfields
    field "isvalidcsv" $ null csvproblems
    field "csvpersonindex" $ csvPersonIndex document
 
csvPageFields :: [CSVProblem] -> Int -> Int -> [[BS.ByteString]] -> Fields
csvPageFields problems totalrowcount firstrowindex xs = do
  field "csvrows" $ zipWith (csvRowFields problems) [firstrowindex..] xs
  field "isfirstcsvpage" $ firstrowindex==0
  field "islastcsvpage" $ (firstrowindex+(length xs))==totalrowcount

splitCSVDataIntoPages :: Int -> [a] -> [[a]]
splitCSVDataIntoPages n xs =
  case splitAt n xs of
    (y,[]) -> [y]
    (y,ys) -> y : splitCSVDataIntoPages n ys

csvRowFields :: [CSVProblem] -> Int -> [BS.ByteString] -> Fields
csvRowFields problems rowindex xs = do
  field "rownumber" $ rowindex + 1
  field "csvfields" $ zipWith (csvFieldFields problems rowindex) 
                              [0..] 
                              xs 
  field "isproblem" $ any isRelevantProblem problems
  where
    isRelevantProblem CSVProblem{problemrowindex, problemcolindex} =
      case (problemrowindex, problemcolindex) of
        (Just r, Nothing) | rowindex==r -> True
        _ -> False

csvFieldFields :: [CSVProblem] -> Int -> Int -> BS.ByteString -> Fields
csvFieldFields problems rowindex colindex val = do
  field "value" $ val
  field "isproblem" $ any isRelevantProblem problems
  where 
    isRelevantProblem CSVProblem{problemrowindex, problemcolindex} =
      case (problemrowindex, problemcolindex) of
        (Just r, Just c) | rowindex==r && colindex==c -> True
        _ -> False

csvProblemFields :: KontrakcjaTemplates -> Int -> Int -> CSVProblem -> IO Fields
csvProblemFields templates probcount number csvproblem = do
    flashMsg <- (problemdescription csvproblem) templates
    let desc = snd $ unFlashMessage flashMsg
    return $ do
      field "problemnumber" $ number
      field "problemrow" $ fmap (+1) $ problemrowindex csvproblem
      field "problemcol" $ fmap (+1) $ problemcolindex csvproblem
      field "problemdesc" $ desc
      field "isfirstproblem" $ (number==1)
      field "islastproblem" $ (number==probcount)
 
{- | Showing document to author after we are done with design -}

pageDocumentForAuthor :: Context 
             -> Document 
             -> User
             -> IO String
pageDocumentForAuthor ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
  }
  author =
   let
       templates = ctxtemplates ctx
       authorid = userid author
       authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
       isSignatory person = SignatoryPartner `elem` signatoryroles person
   in do
     renderTemplate (ctxtemplates ctx) "pageDocumentForAuthor" $ do
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "authorhaslink" $ authorhaslink
       field "signatories" $ map (signatoryLinkFields ctx document author Nothing) $ signatoriesWithSecretary author document               
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       field "cancelMailContent" $ mailCancelDocumentByAuthorContent templates False Nothing ctx document author
       field "linkcancel" $ show $ LinkCancel document
       field "docstate" (buildJS (documentauthordetails author document) documentsignatorylinks)
       field "linkissuedocpdf" $ show (LinkIssueDocPDF Nothing document)
       field "documentinfotext" $ documentInfoText ctx document (find (isMatchingSignatoryLink author) documentsignatorylinks) author
       documentAuthorInfo author
       documentInfoFields document
       documentViewFields document

{- |
   Show the document for Viewers (friends of author or signatory).
   Show no buttons or other controls
 -}                                                                                                          

pageDocumentForViewer :: Context -> Document -> User -> Maybe SignatoryLink -> IO String
pageDocumentForViewer ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
    , documentdaystosign
    , documentinvitetext
    , documentallowedidtypes
  }
  author msignlink =
    let
        authorid = userid author
        -- the author gets his own space when he's editing
        authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
        documentdaystosignboxvalue = maybe 7 id documentdaystosign
        isSignatory person = SignatoryPartner `elem` signatoryroles person
   in do
     invitationMailContent <- mailInvitationToSignOrViewContent (ctxtemplates ctx) False ctx document author Nothing
     cancelMailContent <- mailCancelDocumentByAuthorContent (ctxtemplates ctx) False Nothing ctx document author
     documentinfotext <- documentInfoText ctx document Nothing author
     renderTemplate (ctxtemplates ctx) "pageDocumentForViewerContent" $  do
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "authorhaslink" $ authorhaslink
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $ invitationMailContent
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "anyinvitationundelivered" $ anyInvitationUndelivered document
       field "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document
       field "signatories" $ map (signatoryLinkFields ctx document author Nothing) $ signatoriesWithSecretary author document
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       field "cancelMailContent" $ cancelMailContent
       field "linkcancel" $ show $ LinkCancel document
       field "emailelegitimation" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "emailonly" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isNothing $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "elegitimationonly" $ (isNothing $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "docstate" (buildJS (documentauthordetails author document) documentsignatorylinks)
       case msignlink of
           Nothing -> return ()
           Just siglink -> do
                          field "siglinkid" $ show $ signatorylinkid siglink
                          field "sigmagichash" $ show $ signatorymagichash siglink
       field "linkissuedocpdf" $ show (LinkIssueDocPDF msignlink document)
       field "documentinfotext" $ documentinfotext
       documentInfoFields document 
       documentViewFields document
       documentAuthorInfo author


pageDocumentForSignatory :: KontraLink 
                    -> Document 
                    -> Context
                    -> SignatoryLink
                    -> User
                    -> IO String 
pageDocumentForSignatory action document ctx invitedlink author =
  let
      localscripts =
           "var docstate = "
        ++ (buildJS (documentauthordetails author document) $ documentsignatorylinks document)
        ++ "; docstate['useremail'] = '"
        ++ (BS.toString $ signatoryemail $ signatorydetails invitedlink)
        ++ "'; offer = " ++ if (isOffer document) then "true" else"false"
        ++ ";"
      magichash = signatorymagichash invitedlink
      allowedtypes = documentallowedidtypes document
      requiresEleg = isJust $ find (== ELegitimationIdentification) allowedtypes
      isSignatory person = SignatoryPartner `elem` signatoryroles person
  in do
    renderTemplate (ctxtemplates ctx) "pageDocumentForSignContent" $ do
      field "localscripts" localscripts
      field "signatories" $ map (signatoryLinkFields ctx document author (Just invitedlink)) $ signatoriesWithSecretary author document
      field "rejectMessage" $  mailRejectMailContent (ctxtemplates ctx) Nothing ctx (prettyName author) document invitedlink
      field "partyUnsigned" $ renderListTemplate (ctxtemplates ctx) $  map (BS.toString . personname') $ partyUnsignedMeAndList magichash document
      field "action" $ show action
      field "linkissuedocpdf" $ show (LinkIssueDocPDF (Just invitedlink) document)
      field "documentinfotext" $  documentInfoText ctx document (Just invitedlink) author
      field "requireseleg" requiresEleg
      field "siglinkid" $ show $ signatorylinkid invitedlink
      field "sigmagichash" $ show $ signatorymagichash invitedlink
      documentInfoFields document
      documentAuthorInfo author
      documentViewFields document
      signedByMeFields document (Just invitedlink)


--- Display of signatory                                                             
signatoryLinkFields :: Context -> Document -> User -> Maybe SignatoryLink -> SignatoryLink -> Fields
signatoryLinkFields
  ctx@Context {
      ctxmaybeuser = muser
    , ctxtemplates
  }
  document
  author
  currentlink
  siglnk@SignatoryLink {
    signatorylinkid
    , signatorydetails
    , signatoryroles
    , invitationdeliverystatus
  } =
  let
   isCurrentUserAuthor = maybe False (isAuthor document) muser
   current = (currentlink == Just siglnk) || (isNothing currentlink && (fmap (unEmail . useremail . userinfo) muser) == (Just $ signatoryemail signatorydetails)) 
   isActiveDoc = not $ (documentstatus document) `elem` [Timedout, Canceled, Rejected]
    in do
      field "current" $ current
      field "fstname" $ packToMString $ signatoryfstname $ signatorydetails
      field "sndname" $ packToMString $ signatorysndname $ signatorydetails
      field "company" $ packToMString $ signatorycompany $ signatorydetails
      field "personalnumber" $ packToMString $ signatorypersonalnumber $ signatorydetails
      field "companynumber"  $ packToMString $ signatorycompanynumber $ signatorydetails
      field "email" $ packToMString $ signatoryemail $ signatorydetails
      field "fields" $ for (signatoryotherfields signatorydetails) $ \sof -> do
        field "fieldlabel" $ fieldlabel sof
        field "fieldvalue" $ fieldvalue sof
      field "signorder" $ unSignOrder $ signatorysignorder signatorydetails
      field "allowRemindForm" $ isEligibleForReminder muser document siglnk            
      field "linkremind" $ show (LinkRemind document siglnk)
      field "linkchangeemail" $  show $ LinkChangeSignatoryEmail (documentid document) signatorylinkid
      field "allowEmailChange" $ (isCurrentUserAuthor && (invitationdeliverystatus == Undelivered) && isActiveDoc)
      field "reminderMessage" $ mailDocumentRemindContent ctxtemplates Nothing ctx document siglnk author 
      field "role" $ if isSignatory siglnk
                     then "signatory"
                     else "viewer"
      field "secretary"  $ (isAuthor document siglnk) &&  not (isSignatory siglnk)              
      field "author" $ (isAuthor document siglnk)
      signatoryStatusFields document siglnk showDateOnly

signatoryStatusFields :: Document -> SignatoryLink -> (MinutesTime -> String) -> Fields
signatoryStatusFields
  document
  siglnk@SignatoryLink {
    signatorylinkid
    , signatorydetails
    , maybesigninfo
    , maybeseeninfo
    , maybereadinvite
    , invitationdeliverystatus
  } 
  dateformatter = 
  let
   wasSigned =  isJust maybesigninfo
   wasSeen = isJust maybeseeninfo
   wasRead = isJust maybereadinvite
   isTimedout = documentstatus document == Timedout
   isCanceled = documentstatus document == Canceled
   isRejected = documentstatus document == Rejected
   isClosed = documentstatus document == Closed
   datamismatch = case documentcancelationreason document of
                    Just (ELegDataMismatch _ sid _ _ _) -> sid == signatorylinkid
                    _                                   -> False
   status = caseOf [
          (invitationdeliverystatus == Undelivered,  SCCancelled)
        , (isCanceled, SCCancelled)
        , (isRejected, SCCancelled)
        , (isTimedout, SCTimedout)
        , (wasSigned,  SCSigned)
        , (wasSeen,    SCOpened)
        , (wasRead,    SCRead)
        ] SCSent 
   -- the date this document was rejected if rejected by this signatory
   rejectedDate = case documentrejectioninfo document of
                    Just (rt, slid, _) 
                        | slid == signatorylinkid -> Just $ dateformatter rt
                    _                             -> Nothing
    in do  
      field "status" $ show status
      field "undeliveredEmail" $ (invitationdeliverystatus == Undelivered)
      field "signdate" $ dateformatter <$> signtime <$> maybesigninfo
      field "datamismatch" datamismatch
      field "seendate" $ dateformatter <$> signtime <$> maybeseeninfo
      field "readdate" $ dateformatter <$> maybereadinvite
      field "rejecteddate" rejectedDate

packToMString :: BS.ByteString -> Maybe String
packToMString x =
  if BS.null x
     then Nothing
     else Just $ BS.toString x

isSignatory:: SignatoryLink -> Bool
isSignatory = (SignatoryPartner `elem`) . signatoryroles
    
documentWithSecretary:: Document-> Bool
documentWithSecretary doc = not $ any (\sl -> isAuthor doc sl && isSignatory sl ) $ documentsignatorylinks doc

documentauthordetails :: User -> Document -> SignatoryDetails
documentauthordetails author document=
          (signatoryDetailsFromUser author) {
            signatoryemailplacements = authoremailplacements document
            , signatoryfstnameplacements = authorfstnameplacements document
            , signatorysndnameplacements = authorsndnameplacements document
            , signatorycompanyplacements = authorcompanyplacements document
            , signatorypersonalnumberplacements = authorpersonalnumberplacements document
            , signatorycompanynumberplacements = authorcompanynumberplacements document
            , signatoryotherfields = authorotherfields document
          }
          
secretarySignatoryLink :: User -> Document -> Maybe SignatoryLink 
secretarySignatoryLink author doc = 
    if (documentWithSecretary doc)
     then Just $ SignatoryLink 
          {   signatorylinkid  = SignatoryLinkID 0
            , signatorydetails  = documentauthordetails author doc
            , signatorymagichash = MagicHash 0 
            , maybesignatory = Just $ userid author
            , maybesigninfo = Nothing
            , maybeseeninfo = Nothing
            , maybereadinvite = Nothing
            , invitationdeliverystatus = Unknown
            , signatorysignatureinfo  = Nothing
            , signatoryroles  = []
            , signatorylinkdeleted = False
        }   
     else Nothing  
signatoriesWithSecretary::User -> Document -> [SignatoryLink] 
signatoriesWithSecretary author doc =
    (filter isSignatory $ documentsignatorylinks doc) ++ 
    (maybeToList $ secretarySignatoryLink author doc) 
    
-- Helper to get document after signing info text
documentInfoText :: Context -> Document -> Maybe SignatoryLink -> User -> IO String
documentInfoText ctx document siglnk author =
  renderTemplate (ctxtemplates ctx) "documentInfoText" $ do
    documentInfoFields document 
    documentAuthorInfo author
    field "signatories" $ map (signatoryLinkFields ctx document author Nothing) $ documentsignatorylinks document
    signedByMeFields document siglnk

-- | Basic info about document , name, id ,author
documentInfoFields :: Document -> Fields
documentInfoFields  document  = do
  field "documenttitle" $ BS.toString $ documenttitle document
  field "title" $ BS.toString $ documenttitle document
  field "name" $ BS.toString $ documenttitle document
  field "id" $ show $ documentid document
  field "documentid" $ show $ documentid document
  field "timetosignset" $  isJust $ documentdaystosign document
  field "template" $  isTemplate document
  field "contract" $  isContract document
  field "offer" $  isOffer document
  field "emailselected" $ document `allowsIdentification` EmailIdentification
  field "elegselected" $ document `allowsIdentification` ELegitimationIdentification
  documentStatusFields document

documentAuthorInfo :: User -> Fields
documentAuthorInfo author =  do
  field "authorfstname" $ nothingIfEmpty $ userfstname $ userinfo author
  field "authorsndname" $ nothingIfEmpty $ usersndname $ userinfo author
  field "authorcompany" $ nothingIfEmpty $ usercompanyname $ userinfo author
  field "authoremail"  $ nothingIfEmpty $ unEmail $ useremail $ userinfo author
  field "authorpersonnumber" $ nothingIfEmpty $ userpersonalnumber $ userinfo author
  field "authorcompanynumber" $ nothingIfEmpty $ usercompanynumber $ userinfo author
  
-- | Fields indication what is a document status 
documentStatusFields :: Document -> Fields    
documentStatusFields document = do
  field "preparation" $ documentstatus document == Preparation
  field "pending" $ documentstatus document == Pending
  field "cancel" $ (documentstatus document == Canceled 
      && documentcancelationreason document == Just ManualCancel)
  field "timedout" $ documentstatus document == Timedout
  field "rejected" $ documentstatus document == Rejected
  field "signed" $ documentstatus document == Closed
  field "awaitingauthor" $ documentstatus document == AwaitingAuthor
  field "datamismatch" $ (documentstatus document == Canceled 
      && case documentcancelationreason document of
           Just (ELegDataMismatch _ _ _ _ _) -> True
           _ -> False)
  
-- | Info about what is my position on a document
signedByMeFields :: Document -> Maybe SignatoryLink -> Fields
signedByMeFields document siglnk = do
  field "notsignedbyme" $ (isJust siglnk) && (isNothing $ maybesigninfo $ fromJust siglnk)
  field "signedbyme" $ (isJust siglnk) && (isJust $ maybesigninfo $ fromJust siglnk)
  field "iamauthor" $ isAuthor document siglnk


documentViewFields:: Document -> Fields
documentViewFields document = do
  field "addSignatoryScript" $ documentstatus document == Pending  || documentstatus document == AwaitingAuthor


designViewFields:: (Maybe DesignStep) -> Fields
designViewFields step = do
    case step of 
        (Just (DesignStep3 _)) -> field "step3" True
        (Just (DesignStep2 _ _ _ )) -> field "step2" True
        (Just (DesignStep1)) -> field "step1" True
        _ -> field "step2" True
    field "initialperson" $ 
      case step of
        (Just (DesignStep2 _ (Just part) _ )) -> part
        _ -> 0
    field "isaftercsvupload" $
      case step of
        (Just (DesignStep2 _ _ (Just AfterCSVUpload))) -> True
        _ -> False


uploadPage :: Context -> ListParams -> (Maybe DocumentType) -> Bool -> IO String
uploadPage ctx params mdoctype showTemplates = renderTemplate (ctxtemplates ctx) "uploadPage" $ do
    field "typeselected" $ isJust mdoctype
    field "contract" $ mdoctype == Just Contract
    field "offer" $ mdoctype == Just Offer
    field "templateslink" $  (\t -> show (LinkAjaxTemplates t params)) <$> mdoctype
    field "showTemplates" showTemplates
       

templatesForAjax::KontrakcjaTemplates ->  MinutesTime -> User -> DocumentType -> PagedList Document -> IO String
templatesForAjax templates ctime user doctype doctemplates = 
    renderTemplate templates "templatesForAjax" $ do
        field "documents" $ markParity $ map (documentBasicViewFields ctime user) (list doctemplates)
        field "currentlink" $ show $ LinkNew (Just doctype) (params doctemplates)  
        field "contract" $ doctype == Contract
        field "offer" $ doctype == Offer
        pagedListFields doctemplates
    
-- We keep this javascript code generation for now
jsArray :: [[Char]] -> [Char]
jsArray xs = "[" ++ (joinWith ", " xs) ++ "]"


buildDefJS :: FieldDefinition -> Int -> [Char]
buildDefJS FieldDefinition {
    fieldlabel
  , fieldvalue
  , fieldplacements
  } i =
     "{ label: "
  ++ jsStringFromBS fieldlabel -- show because we need quotes
  ++ ", value: "
  ++ jsStringFromBS fieldvalue
  ++ ", id: 'field" ++ show i ++ "'"
  ++ ", placements: " ++ (jsArray (map buildPlacementJS fieldplacements))
  ++ " }"


buildPlacementJS :: FieldPlacement -> [Char]
buildPlacementJS FieldPlacement {
    placementx
  , placementy
  , placementpage
  , placementpagewidth
  , placementpageheight
  } =
     "{ x: "
  ++ show placementx
  ++ ", y: " ++ show placementy
  ++ ", page: " ++ show placementpage
  ++ ", h: " ++ show placementpageheight
  ++ ", w: " ++ show placementpagewidth
  ++ " }"


buildSigLinkJS :: SignatoryLink -> [Char]
buildSigLinkJS (SignatoryLink {signatorydetails, signatoryroles}) = 
    "{" ++ 
    buildSigJS' signatorydetails ++ 
    ", role: " ++ (if SignatoryPartner `elem` signatoryroles 
                 then "\"signatory\""
                 else "\"viewer\"") ++
    "}"

buildSigJS :: SignatoryDetails -> [Char]
buildSigJS details = "{" ++ buildSigJS' details ++ "}"

buildSigJS' :: SignatoryDetails -> [Char]
buildSigJS' (SignatoryDetails {
  signatoryfstname
  , signatorysndname
  , signatorycompany
  , signatorypersonalnumber
  , signatorycompanynumber
  , signatoryemail
  , signatorysignorder
  , signatoryfstnameplacements
  , signatorysndnameplacements
  , signatorycompanyplacements
  , signatoryemailplacements
  , signatorypersonalnumberplacements
  , signatorycompanynumberplacements
  , signatoryotherfields
  }) =
     "fstname: "  ++ jsStringFromBS  signatoryfstname
  ++ ", sndname: " ++ jsStringFromBS  signatorysndname
  ++ ", company: " ++ jsStringFromBS  signatorycompany
  ++ ", email: " ++ jsStringFromBS signatoryemail
  ++ ", signorder: " ++ show signatorysignorder
  ++ ", personalnumber: " ++ jsStringFromBS signatorypersonalnumber
  ++ ", companynumber: " ++ jsStringFromBS signatorycompanynumber
  ++ ", fstnameplacements: " ++ (jsArray (map buildPlacementJS signatoryfstnameplacements))
  ++ ", sndnameplacements: " ++ (jsArray (map buildPlacementJS signatorysndnameplacements))
  ++ ", companyplacements: " ++ (jsArray (map buildPlacementJS signatorycompanyplacements))
  ++ ", emailplacements: " ++ (jsArray (map buildPlacementJS signatoryemailplacements))
  ++ ", personalnumberplacements: " ++ (jsArray (map buildPlacementJS signatorypersonalnumberplacements))
  ++ ", companynumberplacements: " ++ (jsArray (map buildPlacementJS signatorycompanynumberplacements))
  ++ ", otherfields: " ++ (jsArray $ zipWith buildDefJS signatoryotherfields [1..])

buildJS :: SignatoryDetails -> [SignatoryLink] -> [Char]
buildJS authordetails signatorydetails =
     "{ signatories: "
  ++ sigs
  ++ ", author: " ++ buildSigJS authordetails
  ++ " }"
  where
    -- no need to insert empty signatory here since it's done
    -- with javascript in doc design.
    sigs = jsArray (map buildSigLinkJS signatorydetails)

defaultInviteMessage :: BS.ByteString
defaultInviteMessage = BS.empty     


jsStringFromBS :: BS.ByteString -> String
jsStringFromBS bs =
  "\"" ++ (encode $ BS.toString bs) ++ "\""
  where
    encode ('"':ss) = "\\\"" ++ (encode ss)
    encode ('>':ss) = "\\>" ++ (encode ss)
    encode ('<':ss) = "\\<" ++ (encode ss)
    encode (s:ss) = s:(encode ss)
    encode [] = []

getDataMismatchMessage :: Maybe CancelationReason -> Maybe String
getDataMismatchMessage (Just (ELegDataMismatch msg _ _ _ _)) = Just msg
getDataMismatchMessage _ = Nothing
