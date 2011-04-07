{-# OPTIONS_GHC -Wall #-}

module Doc.DocView (
    emptyDetails
  , showFilesImages2
  , pageDocumentForAuthor
  , pageDocumentForViewer
  , pageDocumentForSignatory
  , docSortSearchPage
  , pageContractsList
  , pageTemplatesList
  , modalSignInviteView
  , modalSendInviteView
  , modalSignedView
  , modalLoginForSaveView
  , flashRemindMailSent
  , flashMessageCanceled
  , flashDocumentRestarted
  , flashDocumentDraftSaved
  , flashDocumentTemplateSaved
  , flashAuthorSigned
  , flashMessageFailedToParseCSV
  , flashMessageCSVHasTooManyRows
  , flashMessageBulkRemindsSent
  , flashMessageNoBulkRemindsSent
  , modalRejectedView
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
  , modalSignAwaitingAuthorLast
  ) where

import Control.Applicative ((<$>))
import Data.Data
import Data.List (find)
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Ord
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
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.List (isInfixOf,sortBy)
import Data.Monoid
import ListUtil
import Control.Monad.Reader

modalSignAwaitingAuthorLast :: KontraModal
modalSignAwaitingAuthorLast = do
    templates <- ask
    lift $ renderTemplate templates "signAwaitingAuthorLast" ()

modalSignInviteView :: Document -> KontraModal
modalSignInviteView document = do
  templates <- ask
  partylist <- lift $ renderListTemplate templates . map (BS.toString . personname') $ partyListButAuthor document
  lift $ renderTemplate templates "modalSignInviteView" $ do
    field "partyListButAuthor" partylist
    field "documenttitle" . BS.toString $ documenttitle document


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

modalSignedView ::  Document -> SignatoryLink -> Bool -> Bool -> KontraModal
modalSignedView document@Document{documenttitle, documentstatus} signatorylink hasaccount isloggedin = do
  templates <- ask   
  if documentstatus == Closed
     then
       lift $ renderTemplate templates "modalSignedViewClosed" $ do
         field "partyListString" . renderListTemplate templates . map (BS.toString . personname') $ partyList document
         field "documenttitle" $ BS.toString $ documenttitle
         field "willCreateAccountForYou" $ willCreateAccountForYou templates document signatorylink hasaccount isloggedin
     else
       lift $ renderTemplate templates "modalSignedViewNotClosed" $ do
         field "partyUnsignedListString" . renderListTemplate templates . map (BS.toString . personname') $ partyUnsignedList document
         field "documenttitle" . BS.toString $ documenttitle
         field "willCreateAccountForYou" $ willCreateAccountForYou templates document signatorylink hasaccount isloggedin

willCreateAccountForYou :: KontrakcjaTemplates -> Document -> SignatoryLink -> Bool -> Bool -> IO String
willCreateAccountForYou templates document siglink hasAccount isloggedin =
  if (hasAccount)
     then
       renderTemplate templates "willCreateAccountForYouHasAccount" $ do
         field "email" . signatoryemail $ signatorydetails siglink
         field "isloggedin" isloggedin
         field "referer" $ show (LinkSignDoc document siglink)
         field "linklogin" $ show (LinkLogin LoginTry)
     else
       renderTemplate templates "willCreateAccountForYouNoAccount" $ do
         field "documentid" $ show $ unDocumentID $ documentid document
         field "documenttitle" $ BS.toString $ documenttitle document
         field "signatorylinkid" $ unSignatoryLinkID $ signatorylinkid siglink

flashDocumentDraftSaved :: KontrakcjaTemplates -> IO FlashMessage
flashDocumentDraftSaved templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashDocumentDraftSaved" ()


flashDocumentTemplateSaved :: KontrakcjaTemplates -> IO FlashMessage
flashDocumentTemplateSaved templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashDocumentTemplateSaved" ()

flashDocumentRestarted :: KontrakcjaTemplates -> IO FlashMessage
flashDocumentRestarted templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashDocumentRestarted" ()

flashRemindMailSent :: KontrakcjaTemplates -> SignatoryLink -> IO FlashMessage
flashRemindMailSent templates signlink@SignatoryLink{maybesigninfo} =
  toFlashMsg OperationDone <$> (renderTemplate templates (template_name maybesigninfo) $ do
    field "personname" . BS.toString $ personname signlink)
  where
    template_name =
      maybe "flashRemindMailSentNotSigned"
      (const "flashRemindMailSentSigned")


flashMessageCanceled :: KontrakcjaTemplates -> IO FlashMessage
flashMessageCanceled templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashMessageCanceled" ()

flashAuthorSigned :: KontrakcjaTemplates -> IO FlashMessage
flashAuthorSigned templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashAuthorSigned" ()

flashMessageFailedToParseCSV :: KontrakcjaTemplates -> IO FlashMessage
flashMessageFailedToParseCSV templates = 
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageFailedToParseCSV" ()

flashMessageCSVHasTooManyRows :: Int -> KontrakcjaTemplates -> IO FlashMessage
flashMessageCSVHasTooManyRows maxrows templates = 
  toFlashMsg OperationFailed <$> (renderTemplate templates "flashMessageCSVHasTooManyRows" $ field "maxrows" maxrows)

flashMessageBulkRemindsSent :: KontrakcjaTemplates -> IO FlashMessage
flashMessageBulkRemindsSent templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageBulkRemindsSent" ()

flashMessageNoBulkRemindsSent :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNoBulkRemindsSent templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageNoBulkRemindsSent" ()

-- All doc view
singlnkFields :: SignatoryLink -> Fields
singlnkFields sl = do
  field "id" $ show $ signatorylinkid sl
  field "name" $ BS.toString $ personname sl
  field "email" $  ""
  field "company" $ ""

{- |
    We want the documents to be ordered like the icons in the bottom
    of the document list.  So this means:
    0 Draft - 1 Cancel - 2 Fall due - 3 Sent - 4 Opened - 5 Signed
-}
data StatusClass = SCDraft
                  | SCCancelled
                  | SCTimedout
                  | SCSent
                  | SCOpened
                  | SCSigned
                  deriving (Eq, Ord)

instance Show StatusClass where
  show SCDraft = "draft"
  show SCCancelled = "cancelled"
  show SCTimedout = "expired"
  show SCSent = "sent"
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
         Pending        -> if anyInvitationUndelivered doc
                               then SCCancelled
                               else
                                if all (isJust . maybeseeninfo) $ documentsignatorylinks doc
                                 then SCOpened
                                 else SCSent
         AwaitingAuthor -> if anyInvitationUndelivered doc
                                then SCCancelled
                                else
                                  if all (isJust . maybeseeninfo) $ documentsignatorylinks doc
                                    then SCOpened
                                    else SCSent
         _              -> SCCancelled

documentBasicViewFields :: MinutesTime -> User -> Document -> Fields
documentBasicViewFields crtime user doc = do
    documentInfoFields doc
    field "status" $ show (documentStatusClass doc)
    field "signatories" $ map singlnkFields $ documentsignatorylinks doc
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
docSortFunc _ = const $ const EQ

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
    
          
showFileImages :: KontrakcjaTemplates -> File -> JpegPages -> IO String
showFileImages templates _ JpegPagesPending =
  renderTemplate templates  "showFileImagesPending" ()

showFileImages templates _ (JpegPagesError normalizelog) =
  renderTemplate templates "showFileImagesError" $ do
    field "normalizelog" $ BS.toString normalizelog

showFileImages templates File{fileid} (JpegPages jpgpages) =
  renderTemplate templates "showFileImagesReady" $ do
    field "fileid" $ show fileid
    field "images" . map page $ zip [1,2..] jpgpages
  where
    page :: (Int,(a,Int,Int)) -> Fields
    page (x,(_,w,h)) = do
      field "number" x
      field "width" w
      field "height" h


showFilesImages2 :: KontrakcjaTemplates -> [(File, JpegPages)] -> IO String
showFilesImages2 templates files = do
  filesPages <- sequence $ map (uncurry (showFileImages templates)) files
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

pageDocumentForAuthor :: Context 
             -> Document 
             -> User
             -> (Maybe DesignStep)
             -> IO String
pageDocumentForAuthor ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
    , documentdaystosign
    , documentinvitetext
    , documentallowedidtypes
  }
  author 
  step =
   let
       templates = ctxtemplates ctx
       authorid = userid author
       authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       documentauthordetails =
         (signatoryDetailsFromUser author) {
             signatoryemailplacements = authoremailplacements document
           , signatoryfstnameplacements = authorfstnameplacements document
           , signatorysndnameplacements = authorsndnameplacements document
           , signatorycompanyplacements = authorcompanyplacements document
           , signatorypersonalnumberplacements = authorpersonalnumberplacements document
           , signatorycompanynumberplacements = authorcompanynumberplacements document
           , signatoryotherfields = authorotherfields document
         }
       doc_author_otherfields fields = sequence .
         map (\(fd, i) ->
           renderTemplate templates "customfield" $ do
             field "otherFieldValue" $ fieldvalue fd
             field "otherFieldName"  $ fieldlabel fd
             field "otherFieldID"    $ "field" ++ show i
             field "otherFieldOwner" "author")
             $ zip fields ([1..]::[Int])
       csvcustomfields = either (const [BS.fromString ""]) id $ getCSVCustomFields document
       mcleancsv = fmap (cleanCSVContents documentallowedidtypes (length csvcustomfields) . csvcontents) $ documentcsvupload document
       csvproblems = maybe [] fst mcleancsv
       csvdata = maybe [] (csvbody . snd) mcleancsv
       csvPageSize = 10
       csvpages = splitCSVDataIntoPages csvPageSize csvdata
   in do
     validationinput <- if isSuperUser $ Just author
                        then renderTemplate (ctxtemplates ctx) "validationdropdown" ()
                        else renderTemplate (ctxtemplates ctx) "emailhidden" ()
     csvproblemfields <- sequence $ zipWith (csvProblemFields templates (length csvproblems)) [1..] csvproblems
     renderTemplate (ctxtemplates ctx) "pageDocumentForAuthorContent" $ do
       field "authorOtherFields" $ doc_author_otherfields $ signatoryotherfields documentauthordetails
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "authorhaslink" $ authorhaslink
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $  mailInvitationToSignContent templates False ctx document author Nothing
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "anyinvitationundelivered" $ anyInvitationUndelivered document
       field "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document
       field "signatories" $ map (signatoryLinkFields ctx document author Nothing) documentsignatorylinks                    
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       field "cancelMailContent" $ mailCancelDocumentByAuthorContent templates False Nothing ctx document author
       field "linkcancel" $ show $ LinkCancel document
       field "emailelegitimation" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "emailonly" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isNothing $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "elegitimationonly" $ (isNothing $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "docstate" (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks)
       field "linkissuedocpdf" $ show (LinkIssueDocPDF document)
       field "documentinfotext" $ documentInfoText templates document (find (isMatchingSignatoryLink author) documentsignatorylinks) author
       documentAuthorInfo author
       field "validationinput" validationinput
       field "csvfilename" $ maybe BS.empty csvtitle . documentcsvupload $ document
       field "csvpersonindex" $ maybe BS.empty (BS.fromString . show) $ csvPersonIndex document
       field "csvproblems" $ csvproblemfields
       field "csvproblemcount" $ length csvproblems
       field "csvpages" $ zipWith (csvPageFields csvproblems (length csvdata)) [0,csvPageSize..] csvpages
       field "csvrowcount" $ length csvdata
       field "csvcustomfields" $ csvcustomfields
       field "isvalidcsv" $ null csvproblems
       documentInfoFields document
       documentViewFields document
       designViewFields step
       field "datamismatchflash" $ getDataMismatchMessage $ documentcancelationreason document

--all of this csv view stuff is a little scrappy.  sorry about that, i was trying
--to implement a bit quickly (well, quickly for me anyway).  i'll clean up fairly soon!
--emily 
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
 
{- |
   Show the document for Viewers (friends of author or signatory).
   Show no buttons or other controls
 -}                                                                                                          

pageDocumentForViewer :: Context -> Document -> User -> IO String
pageDocumentForViewer ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
    , documentdaystosign
    , documentinvitetext
    , documentallowedidtypes
  }
  author =
    let
        authorid = userid author
        -- the author gets his own space when he's editing
        authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
        documentdaystosignboxvalue = maybe 7 id documentdaystosign
        documentauthordetails =
          (signatoryDetailsFromUser author) {
            signatoryemailplacements = authoremailplacements document
            , signatoryfstnameplacements = authorfstnameplacements document
            , signatorysndnameplacements = authorsndnameplacements document
            , signatorycompanyplacements = authorcompanyplacements document
            , signatorypersonalnumberplacements = authorpersonalnumberplacements document
            , signatorycompanynumberplacements = authorcompanynumberplacements document
            , signatoryotherfields = authorotherfields document
          }
   in do
     helpers <- renderTemplate (ctxtemplates ctx) "pageDocumentForViewerHelpers" [("documentid", show documentid)]
     invitationMailContent <- mailInvitationToSignContent (ctxtemplates ctx) False ctx document author Nothing
     cancelMailContent <- mailCancelDocumentByAuthorContent (ctxtemplates ctx) False Nothing ctx document author
     documentinfotext <- documentInfoText (ctxtemplates ctx) document Nothing author
     renderTemplate (ctxtemplates ctx) "pageDocumentForViewerContent" $  do
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "authorhaslink" $ authorhaslink
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $ invitationMailContent
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "anyinvitationundelivered" $ anyInvitationUndelivered document
       field "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document
       field "signatories" $ map (signatoryLinkFields ctx document author Nothing) documentsignatorylinks
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       field "cancelMailContent" $ cancelMailContent
       field "linkcancel" $ show $ LinkCancel document
       field "emailelegitimation" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "emailonly" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isNothing $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "elegitimationonly" $ (isNothing $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "helpers" helpers
       field "docstate" (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks)
       field "linkissuedocpdf" $ show (LinkIssueDocPDF document)
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
        ++ (buildJS documentauthordetails $ map signatorydetails (documentsignatorylinks document))
        ++ "; docstate['useremail'] = '"
        ++ (BS.toString $ signatoryemail $ signatorydetails invitedlink)
        ++ "';"
      magichash = signatorymagichash invitedlink
      documentauthordetails = signatoryDetailsFromUser author
      allowedtypes = documentallowedidtypes document
      requiresEleg = isJust $ find (== ELegitimationIdentification) allowedtypes
  in do
    renderTemplate (ctxtemplates ctx) "pageDocumentForSignContent" $ do
      field "helpers" $ 
          renderTemplate (ctxtemplates ctx) "pageDocumentForSignHelpers" $ do
              field "documentid" . show $ documentid document
              field "localscripts" localscripts
      field "signatories" $ map (signatoryLinkFields ctx document author (Just invitedlink)) (documentsignatorylinks document)
      field "rejectMessage" $  mailRejectMailContent (ctxtemplates ctx) Nothing ctx (prettyName author) document invitedlink
      field "partyUnsigned" $ renderListTemplate (ctxtemplates ctx) $  map (BS.toString . personname') $ partyUnsignedMeAndList magichash document
      field "action" $ show action
      field "linkissuedocpdf" $ show (LinkIssueDocPDF document)
      field "documentinfotext" $  documentInfoText (ctxtemplates ctx) document (Just invitedlink) author
      field "requireseleg" requiresEleg
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
    , maybesigninfo
    , maybeseeninfo
    , invitationdeliverystatus
  } =
  let
   isCurrentUserAuthor = maybe False (isAuthor document) muser
   isCurrentSignatorAuthor = (unEmail . useremail . userinfo $ author) == (signatoryemail $ signatorydetails siglnk) 
   current = (currentlink == Just siglnk) || (isNothing currentlink && (fmap (unEmail . useremail . userinfo) muser) == (Just $ signatoryemail $ signatorydetails siglnk)) 
   wasSigned =  isJust maybesigninfo && (not $ isCurrentSignatorAuthor && (documentstatus document == AwaitingAuthor))
   wasSeen = isJust maybeseeninfo
   isTimedout = documentstatus document == Timedout
   isCanceled = documentstatus document == Canceled
   isRejected = documentstatus document == Rejected
   isClosed = documentstatus document == Closed
   dontShowAnyReminder = isTimedout || isCanceled || isRejected
   status = caseOf [
          (invitationdeliverystatus == Undelivered,  SCCancelled)
        , (isCanceled, SCCancelled)
        , (isRejected, SCCancelled)
        , (isTimedout, SCTimedout)
        , (wasSigned,  SCSigned)
        , (wasSeen,    SCOpened)
        ] SCSent       
    in do
      field "current" $ current  
      field "status" $ show status
      field "fstname" $ packToMString $ signatoryfstname $ signatorydetails siglnk
      field "sndname" $ packToMString $ signatorysndname $ signatorydetails siglnk
      field "company" $ packToMString $ signatorycompany $ signatorydetails siglnk
      field "personalnumber" $ packToMString $ signatorypersonalnumber $ signatorydetails siglnk
      field "companynumber"  $ packToMString $ signatorycompanynumber $ signatorydetails siglnk
      field "email" $ packToMString $ signatoryemail $ signatorydetails siglnk
      field "fields" $ for (signatoryotherfields $ signatorydetails siglnk) $ \sof -> do
        field "fieldlabel" $ fieldlabel sof
        field "fieldvalue" $ fieldvalue sof
      field "allowRemindForm" $ isCurrentUserAuthor && (not isCurrentSignatorAuthor) && (not dontShowAnyReminder) && (invitationdeliverystatus /= Undelivered) && (isClosed || not wasSigned)             
      field "linkremind" $ show (LinkRemind document siglnk)
      field "linkchangeemail" $  show $ LinkChangeSignatoryEmail (documentid document) signatorylinkid
      field "undeliveredEmail" $ (invitationdeliverystatus == Undelivered)
      field "allowEmailChange" $ (isCurrentUserAuthor && (invitationdeliverystatus == Undelivered) && (not dontShowAnyReminder))
      field "signdate" $ showDateOnly <$> signtime <$> maybesigninfo
      field "seenddate" $ showDateOnly <$> signtime <$> maybeseeninfo
      field "reminderMessage" $ mailDocumentRemindContent ctxtemplates Nothing ctx document siglnk author 


packToMString :: BS.ByteString -> Maybe String
packToMString x =
  if BS.null x
     then Nothing
     else Just $ BS.toString x


-- Helper to get document after signing info text
documentInfoText :: KontrakcjaTemplates -> Document -> Maybe SignatoryLink -> User -> IO String
documentInfoText templates document siglnk author =
  renderTemplate templates "documentInfoText" $ do
    documentInfoFields document 
    documentAuthorInfo author
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
  field "template" $  documenttype document == Template
  field "contract" $  documenttype document == Contract
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
  field "cancel" $ documentstatus document == Canceled
  field "timedout" $ documentstatus document == Timedout
  field "rejected" $ documentstatus document == Rejected
  field "signed" $ documentstatus document == Closed
  field "awaitingauthor" $ documentstatus document == AwaitingAuthor
  field "datamismatch" $ maybe False isELegDataMismatch $ documentcancelationreason document
  
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


uploadPage :: KontrakcjaTemplates -> ListParams -> Bool -> IO String
uploadPage templates params showTemplates = renderTemplate templates "uploadPage" $ do
    field "templateslink" $ show $ LinkAjaxTemplates params
    field "showTemplates" showTemplates
    
    
  

templatesForAjax::KontrakcjaTemplates ->  MinutesTime -> User -> PagedList Document -> IO String
templatesForAjax templates ctime user doctemplates = 
    renderTemplate templates "templatesForAjax" $ do
        field "documents" $ markParity $ map (documentBasicViewFields ctime user) (list doctemplates)
        field "currentlink" $ show $ LinkNew (params doctemplates)    
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


buildSigJS :: SignatoryDetails -> [Char]
buildSigJS siglnk@(SignatoryDetails {
  signatoryfstname
  , signatorysndname
  , signatorycompany
  , signatorypersonalnumber
  , signatorycompanynumber
  , signatoryemail
  , signatoryfstnameplacements
  , signatorysndnameplacements
  , signatorycompanyplacements
  , signatoryemailplacements
  , signatorypersonalnumberplacements
  , signatorycompanynumberplacements
  , signatoryotherfields
  }) =
     "{ fstname: "  ++ jsStringFromBS  signatoryfstname
  ++ ", sndname: " ++ jsStringFromBS  signatorysndname
  ++ ", company: " ++ jsStringFromBS  signatorycompany
  ++ ", email: " ++ jsStringFromBS signatoryemail
  ++ ", personalnumber: " ++ jsStringFromBS signatorypersonalnumber
  ++ ", companynumber: " ++ jsStringFromBS signatorycompanynumber
  ++ ", fstnameplacements: " ++ (jsArray (map buildPlacementJS signatoryfstnameplacements))
  ++ ", sndnameplacements: " ++ (jsArray (map buildPlacementJS signatorysndnameplacements))
  ++ ", companyplacements: " ++ (jsArray (map buildPlacementJS signatorycompanyplacements))
  ++ ", emailplacements: " ++ (jsArray (map buildPlacementJS signatoryemailplacements))
  ++ ", personalnumberplacements: " ++ (jsArray (map buildPlacementJS signatorypersonalnumberplacements))
  ++ ", companynumberplacements: " ++ (jsArray (map buildPlacementJS signatorycompanynumberplacements))
  ++ ", otherfields: " ++ (jsArray $ zipWith buildDefJS signatoryotherfields [1..])
  ++ " }"


buildJS :: SignatoryDetails -> [SignatoryDetails] -> [Char]
buildJS authordetails signatorydetails =
     "{ signatories: "
  ++ sigs
  ++ ", author: " ++ buildSigJS authordetails
  ++ " }"
  where
    sigs =
      if (length signatorydetails) > 0
         then jsArray (map buildSigJS signatorydetails)
         else jsArray [(buildSigJS emptyDetails)]


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

getDataMismatchSignatoryID :: Maybe CancelationReason -> Maybe SignatoryLinkID
getDataMismatchSignatoryID (Just (ELegDataMismatch _ id _ _ _)) = Just id
getDataMismatchSignatoryID _ = Nothing
