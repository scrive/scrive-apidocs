module Doc.DocView (
    defaultInviteMessage
  , docSortSearchPage
  , documentAuthorInfo
  , documentInfoFields
  , emptyDetails
  , flashAuthorSigned
  , flashDocumentDraftSaved
  , flashDocumentRestarted
  , flashDocumentTemplateSaved
  , flashMessageAccountActivatedFromSign
  , flashMessageAccountRemovedFromSign
  , flashMessageAttachmentArchiveDone
  , flashMessageBulkRemindsSent
  , flashMessageCSVHasTooManyRows
  , flashMessageCSVSent
  , flashMessageCanceled
  , flashMessageCannotCancel
  , flashMessageSignableArchiveDone
  , flashMessageFailedToParseCSV
  , flashMessageInvalidCSV
  , flashMessageMultipleAttachmentShareDone
  , flashMessageMultipleTemplateShareDone
  , flashMessageNoBulkRemindsSent
  , flashMessageOnlyHaveRightsToViewDoc
  , flashMessagePleaseSign
  , flashMessagePleaseSignWithEleg
  , flashMessageSingleAttachmentShareDone
  , flashMessageSingleTemplateShareDone
  , flashMessageTemplateArchiveDone
  , flashRemindMailSent
  , getDataMismatchMessage
  , isNotLinkForUserID
  , mailCancelDocumentByAuthor
  , mailCancelDocumentByAuthorContent
  , mailDocumentAwaitingForAuthor
  , mailDocumentClosed
  , mailDocumentRejected
  , mailDocumentRemind
  , mailInvitationToSend
  , mailInvitationToSign
  , modalSignedClosedHasAccount
  , modalSignedNotClosedHasAccount
  , modalSignedClosedNoAccount
  , modalSignedNotClosedNoAccount
  , modalLoginForSaveView
  , modalRejectedView
  , modalSendInviteView
  , modalSignAwaitingAuthorLast
  , modalSendConfirmationView
  , pageAttachmentDesign
  , pageAttachmentForSignatory
  , pageAttachmentList
  , pageAttachmentView
  , pageContractsList
  , pageDocumentDesign
  , pageDocumentForAuthor
  , pageDocumentForSignatory
  , pageDocumentForViewer
  , pageOffersList
  , pageOrdersList
  , pageTemplatesList
  , showFilesImages2
  , signatoryDetailsFromUser
  , templatesForAjax
  , documentsToFixView
  , uploadPage
  ) where

import ActionSchedulerState (ActionID)
import Doc.CSVUtils
import Doc.DocProcess
import Doc.DocState
import Doc.DocUtils
import Doc.DocViewMail
import FlashMessage
import Kontra
import KontraLink
import ListUtil
import Mails.MailsUtil
import MinutesTime
import Misc
import Templates.Templates
import Templates.TemplatesUtils
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Char (toUpper)
import Data.List (find, isInfixOf)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS


modalSignAwaitingAuthorLast :: TemplatesMonad m => m FlashMessage
modalSignAwaitingAuthorLast = toModal <$> renderTemplateM "signAwaitingAuthorLast" ()

modalSendConfirmationView :: TemplatesMonad m => Document -> m FlashMessage
modalSendConfirmationView document = do
  templates <- getTemplates
  partylist <- liftIO $ renderListTemplate templates . map (BS.toString . getSmartName) $ partyListButAuthor document
  toModal <$> (liftIO $ renderTemplateForProcess templates document processmodalsendconfirmation $ do
    field "partyListButAuthor" partylist
    field "signatory" . listToMaybe $ map (BS.toString . getSmartName) $ partyList document
    documentInfoFields document)

modalSendInviteView :: TemplatesMonad m => Document -> m FlashMessage
modalSendInviteView document = do
  templates <- getTemplates
  partylist <- liftIO $ renderListTemplate templates . map (BS.toString . getSmartName) $ partyListButAuthor document
  toModal <$> (renderTemplateM "modalSendInviteView" $ do
    field "partyListButAuthor" partylist
    field "documenttitle" . BS.toString $ documenttitle document)

modalRejectedView :: TemplatesMonad m => Document -> m FlashMessage
modalRejectedView document = do
  templates <- getTemplates
  partylist <- liftIO $ renderListTemplate templates . map (BS.toString . getSmartName) $ partyList document
  toModal <$> (renderTemplateM "modalRejectedView" $ do
    field "partyList" partylist
    field "documenttitle" . BS.toString $ documenttitle document)

modalLoginForSaveView :: TemplatesMonad m => m FlashMessage
modalLoginForSaveView = toModal <$> renderTemplateM "modalLoginForSaveView" ()

modalSignedClosedHasAccount :: TemplatesMonad m => Document -> SignatoryLink -> Bool -> m FlashMessage
modalSignedClosedHasAccount document signatorylink isloggedin = do
  templates <- getTemplates
  toModal <$> (liftIO $ renderTemplateForProcess templates document processmodalsignedviewclosedhasaccount $ do
    modalSignedFields templates document
    loginFields document signatorylink isloggedin)

modalSignedNotClosedHasAccount :: TemplatesMonad m => Document -> SignatoryLink -> Bool -> m FlashMessage
modalSignedNotClosedHasAccount document signatorylink isloggedin = do
  templates <- getTemplates
  toModal <$> (liftIO $ renderTemplateForProcess templates document processmodalsignedviewnotclosedhasaccount $ do
    modalSignedFields templates document
    loginFields document signatorylink isloggedin)

modalSignedClosedNoAccount :: TemplatesMonad m => Document -> SignatoryLink -> ActionID -> MagicHash -> m FlashMessage
modalSignedClosedNoAccount document signatorylink actionid magichash = do
  templates <- getTemplates
  toModal <$> (liftIO $ renderTemplateForProcess templates document processmodalsignedviewclosednoaccount $ do
    modalSignedFields templates document
    accountFromSignFields document signatorylink actionid magichash)

modalSignedNotClosedNoAccount :: TemplatesMonad m => Document -> SignatoryLink -> ActionID -> MagicHash -> m FlashMessage
modalSignedNotClosedNoAccount document signatorylink actionid magichash = do
  templates <- getTemplates
  toModal <$> (liftIO $ renderTemplateForProcess templates document processmodalsignedviewnotclosednoaccount $ do
    modalSignedFields templates document
    accountFromSignFields document signatorylink actionid magichash)

modalSignedFields :: KontrakcjaTemplates -> Document -> Fields
modalSignedFields templates document@Document{ documenttitle } = do
  field "partyUnsignedListString" . renderListTemplate templates . map (BS.toString . getSmartName) $ partyUnsignedList document
  field "partyListString" . renderListTemplate templates . map (BS.toString . getSmartName) $ partyList document
  field "signatory" . listToMaybe $ map (BS.toString . getEmail ) $ partyList document
  field "documenttitle" $ BS.toString documenttitle

loginFields :: Document -> SignatoryLink -> Bool -> Fields
loginFields document signatorylink isloggedin = do
    field "isloggedin" isloggedin
    field "referer" $ show (LinkSignDoc document signatorylink)
    field "email" $ getEmail signatorylink
    field "linklogin" $ show (LinkLogin LoginTry)

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
      renderTemplateForProcess templates document processflashmessagerestarted $ do
      documentInfoFields document

flashRemindMailSent :: KontrakcjaTemplates -> SignatoryLink -> IO FlashMessage
flashRemindMailSent templates signlink@SignatoryLink{maybesigninfo} =
  toFlashMsg OperationDone <$> (renderTemplate templates (template_name maybesigninfo) $ do
    field "personname" . BS.toString $ getSmartName signlink)
  where
    template_name =
      maybe "flashRemindMailSentNotSigned"
      (const "flashRemindMailSentSigned")

flashMessageCannotCancel :: KontrakcjaTemplates -> IO FlashMessage
flashMessageCannotCancel templates =
  fmap (toFlashMsg OperationFailed) $
    renderTemplate templates "flashMessageCannotCancel" ()

flashMessageCanceled :: KontrakcjaTemplates -> Document -> IO FlashMessage
flashMessageCanceled templates document =
  fmap (toFlashMsg SigningRelated) $
    renderTemplateForProcess templates document processflashmessagecanceled $ do
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

flashMessageBulkRemindsSent :: KontrakcjaTemplates -> DocumentType -> IO FlashMessage
flashMessageBulkRemindsSent templates doctype =
  toFlashMsg OperationDone <$> renderTextForProcess templates doctype processflashmessagebulkremindssent

flashMessageNoBulkRemindsSent :: KontrakcjaTemplates -> DocumentType -> IO FlashMessage
flashMessageNoBulkRemindsSent templates doctype =
  toFlashMsg OperationFailed <$> renderTextForProcess templates doctype processflashmessagenobulkremindssent

flashMessageSignableArchiveDone :: KontrakcjaTemplates -> DocumentType -> IO FlashMessage
flashMessageSignableArchiveDone templates doctype =
  toFlashMsg OperationDone <$> renderTextForProcess templates doctype processflashmessagearchivedone

flashMessageTemplateArchiveDone :: KontrakcjaTemplates -> IO FlashMessage
flashMessageTemplateArchiveDone templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageTemplateArchiveDone" ()

flashMessageAttachmentArchiveDone :: KontrakcjaTemplates -> IO FlashMessage
flashMessageAttachmentArchiveDone templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageAttachmentArchiveDone" ()

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

flashMessageSingleAttachmentShareDone :: BS.ByteString -> KontrakcjaTemplates -> IO FlashMessage
flashMessageSingleAttachmentShareDone docname templates =
  toFlashMsg OperationDone <$> (renderTemplate templates "flashMessageSingleAttachmentShareDone" $ field "docname" docname)

flashMessageMultipleAttachmentShareDone :: KontrakcjaTemplates -> IO FlashMessage
flashMessageMultipleAttachmentShareDone templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageMultipleAttachmentShareDone" ()

flashMessageAccountActivatedFromSign :: KontrakcjaTemplates -> IO FlashMessage
flashMessageAccountActivatedFromSign templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageAccountActivatedFromSign" ()

flashMessageAccountRemovedFromSign :: KontrakcjaTemplates -> IO FlashMessage
flashMessageAccountRemovedFromSign templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageAccountRemovedFromSign" ()

flashMessageOnlyHaveRightsToViewDoc :: KontrakcjaTemplates -> IO FlashMessage
flashMessageOnlyHaveRightsToViewDoc templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageOnlyHaveRightsToViewDoc" ()

flashMessagePleaseSignWithEleg :: KontrakcjaTemplates -> IO FlashMessage
flashMessagePleaseSignWithEleg templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessagePleaseSignWithEleg" ()

flashMessagePleaseSign :: Document -> KontrakcjaTemplates -> IO FlashMessage
flashMessagePleaseSign document templates =
  toFlashMsg OperationDone <$> renderTextForProcess templates document processflashmessagepleasesign

-- All doc view
singlnkFields :: Document -> (MinutesTime -> String) -> SignatoryLink -> Fields
singlnkFields document dateformatter sl = do
  field "id" $ show $ signatorylinkid sl
  field "name" $ getSmartName sl
  field "email" $  ""
  field "company" $ getCompanyName sl
  field "author" $ isAuthor sl
  signatoryStatusFields document sl dateformatter

{- |
    We want the documents to be ordered like the icons in the bottom
    of the document list.  So this means:
    0 Draft - 1 Cancel - 2 Fall due - 3 Sent - 4 Opened - 5 Signed
-}
data StatusClass = SCDraft
                  | SCCancelled
                  | SCSent
                  | SCDelivered
                  | SCRead
                  | SCOpened
                  | SCSigned
                  deriving (Eq, Ord)

instance Show StatusClass where
  show SCDraft = "draft"
  show SCCancelled = "cancelled"
  show SCSent = "sent"
  show SCDelivered = "delivered"
  show SCRead = "read"
  show SCOpened = "opened"
  show SCSigned = "signed"


documentStatusClass ::Document -> StatusClass
documentStatusClass doc =
  case (map (signatoryStatusClass doc) $ filter isSignatory $ documentsignatorylinks doc) of
    [] -> SCDraft
    xs -> minimum xs

documentBasicViewFields :: KontrakcjaTemplates -> MinutesTime -> User -> Document -> Fields
documentBasicViewFields templates crtime user doc = do
    documentInfoFields doc
    field "status" $ show (documentStatusClass doc)
    field "authorname" $ getAuthorName doc
    field "signatories" $ map (singlnkFields doc (showDateAbbrev crtime)) $ filter isSignatory $ documentsignatorylinks doc
    field "anyinvitationundelivered" $ anyInvitationUndelivered doc
    field "doclink"  $ if isAuthor (doc, user) || null signatorylinklist
                        then show . LinkIssueDoc $ documentid doc
                        else show $ LinkSignDoc doc (head signatorylinklist)
    {- FIXME: to know if a user is superuser we need to consult Context... we do not have it here
    field "davelink" $ if issuperuser
                        then Just $ "/dave/document/" ++ (show $ documentid doc)
                        else Nothing
    -}
    field "timeoutdate" $ fromTimeout show
    field "timeoutdaysleft" $ fromTimeout $ show . (dateDiffInDays crtime)
    field "mtime" $ showDateAbbrev crtime (documentmtime doc)
    field "isauthor" $ isAuthor (doc, user)
    field "isviewer" $ (not $ isAuthor (doc, user)) && isViewer (doc, user)
    field "isshared" $ (documentsharing doc)==Shared
    field "processname" $ renderTextForProcess templates doc processname
  where
    signatorylinklist =
      filter (isSigLinkFor user) $ documentsignatorylinks doc

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
    signMatch d = any match $ map (BS.toString . getSmartName) (documentsignatorylinks d)


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
docSortFunc "author" = viewComparing getAuthorName
docSortFunc "authorRev" = viewComparingRev getAuthorName
docSortFunc _ = const $ const EQ

partnerComps :: Document -> BS.ByteString
partnerComps doc = BS.concat $ map getCompanyName $ documentsignatorylinks doc

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
      let splitUp sl = span (\c -> c/=' ') . map toUpper . BS.toString $ getSmartName sl
          (fst1, snd1) = splitUp sl1
          (fst2, snd2) = splitUp sl2 in
      case (compare fst1 fst2) of
        EQ -> compare snd1 snd2
        x -> x

docsPageSize :: Int
docsPageSize = 100


--

pageContractsList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageContractsList = pageList' "pageContractsList" LinkContracts

pageTemplatesList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageTemplatesList = pageList' "pageTemplatesList" LinkTemplates

pageAttachmentList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageAttachmentList = pageList' "pageAttachmentList" LinkAttachments

pageOffersList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageOffersList = pageList' "pageOffersList" LinkOffers

pageOrdersList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageOrdersList = pageList' "pageOrdersList" LinkOrders

{- |
    Helper function for list pages
-}
pageList' :: String
             -> (ListParams -> KontraLink)
             -> KontrakcjaTemplates
             -> MinutesTime
             -> User
             -> PagedList Document
             -> IO String
pageList' templatename makeCurrentLink templates ctime user documents =
  renderTemplate templates templatename $ do
    field "documents" $ markParity $ map (documentBasicViewFields templates ctime user) $ list documents
    pagedListFields documents
    field "currentlink" $ show $ currentlink
    field "linkdoclist" $ show $ LinkContracts emptyListParams
    field "documentactive" $ documentactive
    field "linkofferlist" $ show $ LinkOffers emptyListParams
    field "offeractive" $ offeractive
    field "linkorderlist" $ show $ LinkOrders emptyListParams
    field "orderactive" $ orderactive
    field "linktemplatelist" $ show $ LinkTemplates emptyListParams
    field "templateactive" $ templateactive
    field "linkattachmentlist" $ show $ LinkAttachments emptyListParams
    field "attachmentactive" $ attachmentactive
  where
    currentlink = makeCurrentLink $ params documents
    documentactive = case currentlink of
                       (LinkContracts _) -> True
                       _ -> False
    offeractive = case currentlink of
                       (LinkOffers _) -> True
                       _ -> False
    orderactive = case currentlink of
                       (LinkOrders _) -> True
                       _ -> False
    templateactive = case currentlink of
                       (LinkTemplates _) -> True
                       _ -> False
    attachmentactive = case currentlink of
                       (LinkAttachments _) -> True
                       _ -> False

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

pageAttachmentForSignatory :: Context
                              -> Document
                              -> SignatoryLink
                              -> IO String
pageAttachmentForSignatory ctx doc siglink = pageAttachment' False (Just siglink) ctx doc

pageAttachmentView :: Context
                      -> Document
                      -> IO String
pageAttachmentView = pageAttachment' False Nothing

pageAttachmentDesign :: Context
                      -> Document
                      -> IO String
pageAttachmentDesign = pageAttachment' True Nothing

pageAttachment' :: Bool
                 -> Maybe SignatoryLink
                 -> Context
                 -> Document
                 -> IO String
pageAttachment'
  iseditable
  msiglink
  ctx
  doc@Document { documentid, documenttitle } =
    renderTemplate (ctxtemplates ctx) "pageAttachment" $ do
      field "documentid" $ show documentid
      field "documenttitle" $ BS.toString documenttitle
      field "editable" $ iseditable
      field "renamelink" $ show $ LinkRenameAttachment documentid
      field "siglinkid" $ fmap (show . signatorylinkid) msiglink
      field "sigmagichash" $ fmap (show . signatorymagichash) msiglink
      field "linkissuedocpdf" $ show (LinkIssueDocPDF msiglink doc)

pageDocumentDesign :: Context
             -> Document
             -> (Maybe DesignStep)
             -> [Document]
             -> IO String
pageDocumentDesign ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentdaystosign
    , documentinvitetext
  }
  step
  attachments =
   let
       templates = ctxtemplates ctx
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       doc_author_otherfields fields = sequence .
         map (\(fd, i) ->
           renderTemplate templates "customfield" $ do
             field "otherFieldValue" $ fieldvalue fd
             field "otherFieldName"  $ fieldlabel fd
             field "otherFieldID"    $ "field" ++ show i
             field "otherFieldOwner" "author")
             $ zip fields ([1..]::[Int])
       authorsiglink = fromJust $ getAuthorSigLink document
   in do
     csvstring <- renderTemplate (ctxtemplates ctx) "csvsendoutsignatoryattachmentstring" ()     
     csvfields <- documentCsvFields templates document
     renderTemplate (ctxtemplates ctx) "pageDocumentDesign" $ do
       field "authorOtherFields" $ doc_author_otherfields $ signatoryotherfields $ signatorydetails authorsiglink
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $  mailInvitationToSignOrViewContent templates False ctx document Nothing
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "docstate" (buildJS (signatorydetails authorsiglink) documentsignatorylinks)
       documentAuthorInfo document
       csvfields
       documentFunctionalityFields document
       documentInfoFields document
       documentViewFields document
       designViewFields step
       documentAttachmentDesignFields (documentauthorattachments document)
       documentAuthorAttachments attachments
       documentSignatoryAttachments csvstring document (documentsignatoryattachments document)
       field "process" processFields
   where
     getProcessText = renderTextForProcess (ctxtemplates ctx) document
     getProcessValue = getValueForProcess document
     processFields = do
       field "hasadvancedview" $ getProcessValue processadvancedview
       field "isauthorsend" $ getProcessValue processauthorsend
       field "isvalidationchoiceforbasic" $ getProcessValue processvalidationchoiceforbasic
       field "isexpiryforbasic" $ getProcessValue processexpiryforbasic
       field "title" $ getProcessText processtitle
       field "step1text" $ getProcessText processstep1text
       field "expirywarntext" $ getProcessText processexpirywarntext
       field "sendbuttontext" $ getProcessText processsendbuttontext
       field "expirywarntext" $ getProcessText processexpirywarntext
       field "confirmsendtitle" $ getProcessText processconfirmsendtitle
       field "confirmsendtext" $ getProcessText processconfirmsendtext
       field "expirytext" $ getProcessText processexpirytext


documentAttachmentDesignFields :: [AuthorAttachment] -> Fields
documentAttachmentDesignFields atts = do
  field "isattachments" $ not $ null atts
  field "attachmentcount" $ length atts
  field "attachments" $ map attachmentFields atts
  where
    attachmentFields AuthorAttachment{ authorattachmentfile = File { fileid, filename } } = do
      field "attachmentid" $ show fileid
      field "attachmentname" $ filename

documentFunctionalityFields :: Document -> Fields
documentFunctionalityFields Document{documenttype, documentfunctionality} = do
  field "docfunctionality" $ show documentfunctionality
  -- it might not really be basic, it's just if there isn't an advanced mode we pretend we are
  field "isbasic" $ documentfunctionality==BasicFunctionality || (Just False == getValueForProcess documenttype processadvancedview)
  field "featureenabled" $ documentfunctionality==AdvancedFunctionality || (Just False == getValueForProcess documenttype processadvancedview)
  field "isorder" $ documenttype == Signable Order

documentCsvFields :: KontrakcjaTemplates -> Document -> IO Fields
documentCsvFields templates document@Document{documentallowedidtypes, documentcsvupload} =  do
  let csvcustomfields = either (const [BS.fromString ""]) id $ getCSVCustomFields document
      mcleancsv = fmap (cleanCSVContents documentallowedidtypes (length csvcustomfields) . csvcontents) $ documentcsvupload
      csvproblems = maybe [] fst mcleancsv
      csvdata = maybe [] (csvbody . snd) mcleancsv
      csvPageSize :: Int = 10
      csvpages = splitCSVDataIntoPages csvPageSize csvdata
  csvproblemfields <- sequence $ zipWith (csvProblemFields templates (length csvproblems)) [1..] csvproblems
  return $ do
    field "csvproblems" $ csvproblemfields
    field "csvproblemcount" $ length csvproblems
    field "csvpages" $ zipWith (csvPageFields csvproblems (length csvdata)) [0,csvPageSize..] csvpages
    field "csvrowcount" $ length csvdata
    field "csvcustomfields" $ csvcustomfields
    field "isvalidcsv" $ null csvproblems
    field "csvsigindex" $ fmap csvsignatoryindex documentcsvupload

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
    let desc = snd $ fromJust $ unFlashMessage flashMsg
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
             -> IO String
pageDocumentForAuthor ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
  }
  =
   let
       templates = ctxtemplates ctx
       authorsiglink = fromJust $ getAuthorSigLink document
   in do
     csvstring <- renderTemplate (ctxtemplates ctx) "csvsendoutsignatoryattachmentstring" ()
     renderTemplate (ctxtemplates ctx) "pageDocumentForAuthor" $ do
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "signatories" $ map (signatoryLinkFields ctx document Nothing) $ signatoriesWithSecretary document
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       field "cancelMailContent" $ mailCancelDocumentByAuthorContent templates False Nothing ctx document
       field "linkcancel" $ show $ LinkCancel document
       field "docstate" (buildJS (signatorydetails authorsiglink) documentsignatorylinks)
       field "linkissuedocpdf" $ show (LinkIssueDocPDF Nothing document)
       field "documentinfotext" $ documentInfoText ctx document (find isAuthor documentsignatorylinks)
       documentAuthorInfo document
       documentInfoFields document
       documentViewFields document
       documentAttachmentViewFields documentid Nothing (documentauthorattachments document)
       documentSigAttachmentViewFields csvstring documentid documentsignatorylinks Nothing (documentsignatoryattachments document)
       field "process" processFields
   where
     getProcessText = renderTextForProcess (ctxtemplates ctx) document
     processFields = do
       field "title" $ getProcessText processtitle
       field "restartbuttontext" $ getProcessText processrestartbuttontext
       field "cancelbuttontext" $ getProcessText processcancelbuttontext
       field "rejectbuttontext" $ getProcessText processrejectbuttontext
       field "cancelbyauthormodaltitle" $ getProcessText processcancelbyauthormodaltitle
       field "signatorysignedtext" $ getProcessText processsignatorysignedtext
       field "signatorycanceledtext" $ getProcessText processsignatorycanceledtext
       field "authorissecretarytext" $ getProcessText processauthorissecretarytext
       field "remindagainbuttontext" $ getProcessText processremindagainbuttontext
       signatoryMessageProcessFields ctx document

{- |
   Show the document for Viewers (friends of author or signatory).
   Show no buttons or other controls
 -}

pageDocumentForViewer :: Context -> Document -> Maybe SignatoryLink -> IO String
pageDocumentForViewer ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
    , documentdaystosign
    , documentinvitetext
    , documentallowedidtypes
  }
  msignlink =
    let documentdaystosignboxvalue = maybe 7 id documentdaystosign
        authorsiglink = fromJust $ getAuthorSigLink document
   in do
     csvstring <- renderTemplate (ctxtemplates ctx) "csvsendoutsignatoryattachmentstring" ()
     invitationMailContent <- mailInvitationToSignOrViewContent (ctxtemplates ctx) False ctx document Nothing
     cancelMailContent <- mailCancelDocumentByAuthorContent (ctxtemplates ctx) False Nothing ctx document
     documentinfotext <- documentInfoText ctx document Nothing
     renderTemplate (ctxtemplates ctx) "pageDocumentForViewerContent" $  do
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $ invitationMailContent
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "anyinvitationundelivered" $ anyInvitationUndelivered document
       field "undelivered" $ map getEmail $ undeliveredSignatoryLinks document
       field "signatories" $ map (signatoryLinkFields ctx document Nothing) $ signatoriesWithSecretary document
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       field "cancelMailContent" $ cancelMailContent
       field "linkcancel" $ show $ LinkCancel document
       field "emailelegitimation" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "emailonly" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isNothing $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "elegitimationonly" $ (isNothing $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "docstate" (buildJS (signatorydetails authorsiglink) documentsignatorylinks)
       case msignlink of
           Nothing -> return ()
           Just siglink -> do
                          field "siglinkid" $ show $ signatorylinkid siglink
                          field "sigmagichash" $ show $ signatorymagichash siglink
       field "linkissuedocpdf" $ show (LinkIssueDocPDF msignlink document)
       field "documentinfotext" $ documentinfotext
       documentInfoFields document
       documentViewFields document
       documentAttachmentViewFields documentid msignlink (documentauthorattachments document)
       documentSigAttachmentViewFields csvstring documentid documentsignatorylinks msignlink (documentsignatoryattachments document)
       documentAuthorInfo document
       field "process" $ field "title" $ renderTextForProcess (ctxtemplates ctx) document processtitle
       signatoryMessageProcessFields ctx document

signatoryMessageProcessFields :: Context -> Document -> Fields
signatoryMessageProcessFields ctx document = do
  field "signatorysignedtext" $ getProcessText processsignatorysignedtext
  field "signatorycanceledtext" $ getProcessText processsignatorycanceledtext
  field "authorissecretarytext" $ getProcessText processauthorissecretarytext
  field "remindagainbuttontext" $ getProcessText processremindagainbuttontext
  where
    getProcessText = renderTextForProcess (ctxtemplates ctx) document

documentAttachmentViewFields :: DocumentID -> Maybe SignatoryLink -> [AuthorAttachment] -> Fields
documentAttachmentViewFields docid msignlink atts = do
  field "isattachments" $ length atts > 0
  field "attachments" $ map attachmentFields atts
  where
    attachmentFields AuthorAttachment{ authorattachmentfile = File{ filename, fileid } } = do
      field "attachmentname" filename
      field "linkattachment" $ case msignlink of
        Just signlink -> show (LinkAttachmentForViewer docid (signatorylinkid signlink) (signatorymagichash signlink) fileid)
        Nothing -> show (LinkAttachmentForAuthor docid fileid)

documentSigAttachmentViewFields :: String -> DocumentID -> [SignatoryLink] -> Maybe SignatoryLink -> [SignatoryAttachment] -> Fields
documentSigAttachmentViewFields csvstring docid sls msignlink atts = do
  field "hassigattachments" $ length atts > 0
  field "sigattachments" $ map sigAttachmentFields atts
  where
    sigAttachmentFields a = do
      let mattachlink = find (isSigLinkFor (signatoryattachmentemail a)) sls
      if (signatoryattachmentemail a) == BS.fromString "csv" 
        then field "signame" csvstring
        else field "signame" $ maybe "No name" (BS.toString . getFullName) mattachlink
      field "email" $ signatoryattachmentemail a
      field "name" $ signatoryattachmentname a
      field "desc" $ signatoryattachmentdescription a
      field "filename" $ fmap filename $ signatoryattachmentfile a
      field "viewerlink" $
        case (msignlink, maybe Nothing maybesigninfo mattachlink, signatoryattachmentfile a) of
          (_, _, Nothing) -> Nothing
          (_, Nothing, _) -> Nothing
          (Nothing, _, Just file) -> Just $ show $ LinkAttachmentForAuthor docid (fileid file)
          (Just signlink, _, Just file) -> Just $ show $ LinkAttachmentForViewer docid (signatorylinkid signlink) (signatorymagichash signlink) (fileid file)

pageDocumentForSignatory :: KontraLink
                    -> Document
                    -> Context
                    -> SignatoryLink
                    -> IO String
pageDocumentForSignatory action document ctx invitedlink  = do
  csvstring <- renderTemplate (ctxtemplates ctx) "csvsendoutsignatoryattachmentstring" ()
  renderTemplate (ctxtemplates ctx) "pageDocumentForSignContent" $ do
    mainFields csvstring
    field "process" $ processFields csvstring
    where
    mainFields csvstring =
      let authorsiglink = fromJust $ getAuthorSigLink document
          localscripts =
            "var docstate = "
            ++ (buildJS (signatorydetails authorsiglink) $ documentsignatorylinks document)
            ++ "; docstate['useremail'] = '"
            ++ (BS.toString $ getEmail invitedlink)
            ++ "';"
          magichash = signatorymagichash invitedlink
          allowedtypes = documentallowedidtypes document
          requiresEleg = isJust $ find (== ELegitimationIdentification) allowedtypes
          sigattachments = [a | a <- documentsignatoryattachments document
                              , signatoryattachmentemail a == getEmail invitedlink]
          hassigattachments = length sigattachments > 0
      in do
        field "localscripts" localscripts
        field "signatories" $ map (signatoryLinkFields ctx document (Just invitedlink)) $ signatoriesWithSecretary document
        field "rejectMessage" $  mailRejectMailContent (ctxtemplates ctx) Nothing ctx (getSmartName authorsiglink) document invitedlink
        field "partyUnsigned" $ renderListTemplate (ctxtemplates ctx) $  map (BS.toString . getSmartName) $ partyUnsignedMeAndList magichash document
        field "action" $ show action
        field "linkissuedocpdf" $ show (LinkIssueDocPDF (Just invitedlink) document)
        field "documentinfotext" $  documentInfoText ctx document (Just invitedlink)
        field "requireseleg" requiresEleg
        field "siglinkid" $ show $ signatorylinkid invitedlink
        field "sigmagichash" $ show $ signatorymagichash invitedlink
        documentInfoFields document
        documentAuthorInfo document
        documentViewFields document
        signedByMeFields document (Just invitedlink)
        documentAttachmentViewFields (documentid document) (Just invitedlink) (documentauthorattachments document)
        documentSigAttachmentViewFields csvstring (documentid document) (documentsignatorylinks document) (Just invitedlink) (documentsignatoryattachments document)
        documentSingleSignatoryAttachmentsFields (documentid document) (signatorylinkid invitedlink) (signatorymagichash invitedlink) sigattachments
        field "hasmysigattachments" hassigattachments
    getProcessTextWithFields f csvstring = renderTemplateForProcess (ctxtemplates ctx) document f (mainFields csvstring)
    getProcessText = renderTextForProcess (ctxtemplates ctx) document
    processFields csvstring = do
      field "signatorysignmodaltitle" $ getProcessText processsignatorysignmodaltitle
      field "signatorysignmodalcontent" $ getProcessTextWithFields processsignatorysignmodalcontent csvstring
      field "signbuttontext" $ getProcessText processsignbuttontext
      field "signatorycancelmodaltitle" $ getProcessTextWithFields processsignatorycancelmodaltitle csvstring
      field "rejectbuttontext" $ getProcessText processrejectbuttontext
      field "title" $ getProcessText processtitle
      field "requiressignguard" $ getValueForProcess document processrequiressignguard
      field "signguardwarntext" $ getProcessText processsignguardwarntext
      signatoryMessageProcessFields ctx document

documentSingleSignatoryAttachmentsFields :: DocumentID -> SignatoryLinkID -> MagicHash -> [SignatoryAttachment] -> Fields
documentSingleSignatoryAttachmentsFields docid sid mh atts =
  field "mysigattachments" $ for atts
  (\a -> do
      field "name" $ signatoryattachmentname a
      field "desc" $ signatoryattachmentdescription a
      field "filename" $ fmap filename $ signatoryattachmentfile a
      field "viewerlink" $ fmap (show . LinkAttachmentForViewer docid sid mh . fileid) $ signatoryattachmentfile a
  )

--- Display of signatory
signatoryLinkFields :: Context -> Document -> Maybe SignatoryLink -> SignatoryLink -> Fields
signatoryLinkFields
  ctx@Context {
      ctxmaybeuser = muser
    , ctxtemplates
  }
  document
  currentlink
  siglnk@SignatoryLink {
    signatorylinkid
    , signatorydetails
    , invitationdeliverystatus
  } =
  let isCurrentUserAuthor = isAuthor (document, muser)
      current = (currentlink == Just siglnk) || (isNothing currentlink && (fmap getEmail muser) == (Just $ getEmail signatorydetails))
      isActiveDoc = not $ (documentstatus document) `elem` [Timedout, Canceled, Rejected]
    in do
      field "id" $ show signatorylinkid
      field "current" $ current
      field "fstname" $ packToMString $ getFirstName signatorydetails
      field "sndname" $ packToMString $ getLastName  signatorydetails
      field "company" $ packToMString $ signatorycompany $ signatorydetails
      field "personalnumber" $ packToMString $ signatorypersonalnumber $ signatorydetails
      field "companynumber"  $ packToMString $ signatorycompanynumber $ signatorydetails
      field "email" $ packToMString $ getEmail signatorydetails
      field "fields" $ for (signatoryotherfields signatorydetails) $ \sof -> do
        field "fieldlabel" $ fieldlabel sof
        field "fieldvalue" $ fieldvalue sof
      field "signorder" $ unSignOrder $ signatorysignorder signatorydetails
      field "allowRemindForm" $ isEligibleForReminder muser document siglnk
      field "linkremind" $ show (LinkRemind document siglnk)
      field "linkchangeemail" $  show $ LinkChangeSignatoryEmail (documentid document) signatorylinkid
      field "allowEmailChange" $ (isCurrentUserAuthor && (invitationdeliverystatus == Undelivered || invitationdeliverystatus == Deferred) && isActiveDoc)
      field "reminderMessage" $ mailDocumentRemindContent ctxtemplates Nothing ctx document siglnk
      field "role" $ if isSignatory siglnk
                     then "signatory"
                     else "viewer"
      field "secretary"  $ (isAuthor siglnk) &&  not (isSignatory siglnk)
      field "author" $ (isAuthor siglnk)
      signatoryStatusFields document siglnk showDateOnly

signatoryStatusClass :: Document -> SignatoryLink -> StatusClass
signatoryStatusClass
  Document {
    documentstatus
  }
  SignatoryLink {
    maybesigninfo
  , maybeseeninfo
  , maybereadinvite
  , invitationdeliverystatus
  } =
  caseOf [
      (invitationdeliverystatus==Undelivered,  SCCancelled)
    , (documentstatus==Preparation, SCDraft)
    , (documentstatus==Canceled, SCCancelled)
    , (documentstatus==Rejected, SCCancelled)
    , (documentstatus==Timedout, SCCancelled)
    , (isJust maybesigninfo, SCSigned)
    , (isJust maybeseeninfo, SCOpened)
    , (isJust maybereadinvite, SCRead)
    , (invitationdeliverystatus==Delivered, SCDelivered)
    ] SCSent

signatoryStatusFields :: Document -> SignatoryLink -> (MinutesTime -> String) -> Fields
signatoryStatusFields
  document
  siglnk@SignatoryLink {
    signatorylinkid
    , maybesigninfo
    , maybeseeninfo
    , maybereadinvite
    , invitationdeliverystatus
  }
  dateformatter =
  let
   datamismatch = case documentcancelationreason document of
                    Just (ELegDataMismatch _ sid _ _ _) -> sid == signatorylinkid
                    _                                   -> False
   status = signatoryStatusClass document siglnk
   -- the date this document was rejected if rejected by this signatory
   rejectedDate = case documentrejectioninfo document of
                    Just (rt, slid, _)
                        | slid == signatorylinkid -> Just $ dateformatter rt
                    _                             -> Nothing
    in do
      field "status" $ show status
      field "undeliveredEmail" $ (invitationdeliverystatus == Undelivered)
      field "deliveredEmail" $ (invitationdeliverystatus == Delivered)
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

signatoriesWithSecretary :: Document -> [SignatoryLink]
signatoriesWithSecretary doc =
  (filter isSignatory $ documentsignatorylinks doc) ++
  case getAuthorSigLink doc of
    Just sl | not $ isSignatory sl -> [sl]
    _ -> []

-- Helper to get document after signing info text
documentInfoText :: Context -> Document -> Maybe SignatoryLink -> IO String
documentInfoText ctx document siglnk =
  renderTemplate (ctxtemplates ctx) "documentInfoText" $ do
    mainFields
    field "process" processFields
  where
    mainFields = do
      documentInfoFields document
      documentAuthorInfo document
      field "signatories" $ map (signatoryLinkFields ctx document Nothing) $ documentsignatorylinks document
      signedByMeFields document siglnk
    getProcessText = renderTextForProcess (ctxtemplates ctx) document
    getProcessTextWithFields f = renderTemplateForProcess (ctxtemplates ctx) document f mainFields
    processFields = do
      field "pendingauthornotsignedinfoheader" $ getProcessText processpendingauthornotsignedinfoheader
      field "pendingauthornotsignedinfotext" $ getProcessText processpendingauthornotsignedinfotext
      field "pendingauthorinfoheader" $ getProcessText processpendingauthorinfoheader
      field "pendingauthorinfotext" $ getProcessTextWithFields processpendingauthorinfotext
      field "cancelledinfoheader" $ getProcessText processcancelledinfoheader
      field "cancelledinfotext" $ getProcessTextWithFields processcancelledinfotext
      field "signedinfoheader" $ getProcessText processsignedinfoheader
      field "signedinfotext" $ getProcessTextWithFields processsignedinfotext
      field "statusinfotext" $ getProcessTextWithFields processstatusinfotext

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
  field "emailselected" $ document `allowsIdentification` EmailIdentification
  field "elegselected" $ document `allowsIdentification` ELegitimationIdentification
  field "hasanyattachments" $ length (documentauthorattachments document) + length (documentsignatoryattachments document) > 0
  documentStatusFields document

documentAuthorInfo :: Document -> Fields
documentAuthorInfo document =
  case getAuthorSigLink document of
    Nothing -> return ()
    Just siglink -> do
      field "authorfstname"       $ nothingIfEmpty $ getFirstName      siglink
      field "authorsndname"       $ nothingIfEmpty $ getLastName       siglink
      field "authorcompany"       $ nothingIfEmpty $ getCompanyName    siglink
      field "authoremail"         $ nothingIfEmpty $ getEmail          siglink
      field "authorpersonnumber"  $ nothingIfEmpty $ getPersonalNumber siglink
      field "authorcompanynumber" $ nothingIfEmpty $ getCompanyNumber  siglink

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
signedByMeFields _document siglnk = do
  field "notsignedbyme" $ (isJust siglnk) && (isNothing $ maybesigninfo $ fromJust siglnk)
  field "signedbyme" $ (isJust siglnk) && (isJust $ maybesigninfo $ fromJust siglnk)
  field "iamauthor" $ maybe False isAuthor siglnk


documentViewFields:: Document -> Fields
documentViewFields document = do
  field "addSignatoryScript" $ documentstatus document == Pending  || documentstatus document == AwaitingAuthor


designViewFields:: (Maybe DesignStep) -> Fields
designViewFields step = do
    case step of
        (Just (DesignStep3 _ _)) -> field "step3" True
        (Just (DesignStep2 _ _ _ _)) -> field "step2" True
        (Just (DesignStep1)) -> field "step1" True
        _ -> field "step2" True
    field "initialperson" $
      case step of
        (Just (DesignStep2 _ (Just part) _ _)) -> part
        _ -> 0
    field "isaftercsvupload" $
      case step of
        (Just (DesignStep2 _ _ (Just AfterCSVUpload) _)) -> True
        _ -> False
    field "signlast" $
      case step of
        (Just (DesignStep2 _ _ _ True)) -> True
        (Just (DesignStep3 _ True)) -> True
        _ -> False


uploadPage :: Context -> ListParams -> (Maybe DocumentProcess) -> Bool -> IO String
uploadPage ctx params mdocprocess showTemplates = renderTemplate (ctxtemplates ctx) "uploadPage" $ do
    field "isprocessselected" $ isJust mdocprocess
    field "templateslink" $  (\t -> show (LinkAjaxTemplates t params)) <$> mdocprocess
    field "showTemplates" showTemplates
    field "processes" $ map processFields [Contract,Offer,Order]
    case mdocprocess of
      Just selecteprocess -> do
        field "selectedprocess" $ processFields selecteprocess
      _ -> return ()
    where
      processFields process = do
        field "id" $ show process
        field "selected" $ (Just process == mdocprocess)
        field "name" $ renderTextForProcess (ctxtemplates ctx) (Signable process) processuploadname
        field "uploadprompttext" $ renderTextForProcess (ctxtemplates ctx) (Signable process) processuploadprompttext



templatesForAjax::KontrakcjaTemplates ->  MinutesTime -> User -> DocumentProcess -> PagedList Document -> IO String
templatesForAjax templates ctime user docprocess doctemplates =
    renderTemplate templates "templatesForAjax" $ do
        field "documents" $ markParity $ map (documentBasicViewFields templates ctime user) (list doctemplates)
        field "currentlink" $ show $ LinkNew (Just docprocess) (params doctemplates)  True
        field "processid" $ show docprocess
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

-- This is temporary method used to see list of broken documents
documentsToFixView :: KontrakcjaTemplates -> [Document] -> IO String
documentsToFixView templates docs = do
    renderTemplate templates "documentsToFixView" $ do
        field "documents" $ for docs $ \doc -> do
            field "title" $ documenttitle doc
            field "id" $ show $ documentid doc
            field "involved" $ map getEmail  $ documentsignatorylinks doc
            field "cdate" $  show $ documentctime doc

documentAuthorAttachments :: [Document] -> Fields
documentAuthorAttachments attachments =
  field "existingattachments" $
  for attachments (\doc -> do
                      field "attachmentid" $ show (documentid doc)
                      field "attachmentname" $ documenttitle doc)

documentSignatoryAttachments :: String -> Document -> [SignatoryAttachment] -> Fields
documentSignatoryAttachments csvstring doc attachments =
  let ats = buildattach csvstring doc attachments []
  in field "sigattachments" $
     for ats (\(n, d, sigs) -> do
                 field "attachmentname" n
                 field "attachmentdescription" d
                 field "signatories" $
                   for sigs (\(name, email) -> do
                                field "signame" name
                                field "sigemail" email))

