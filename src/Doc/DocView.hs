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
  , flashMessageRubbishRestoreDone
  , flashMessageRubbishHardDeleteDone
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
  , modalPdfTooLarge
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
  , pageRubbishBinList
  , showFilesImages2
  , signatoryDetailsFromUser
  , documentsToFixView
  , uploadPage
  , docForListJSON
  ) where

import ActionSchedulerState (ActionID)
import DB.Types
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
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import User.Model

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Char (toUpper)
import Data.List (find, isInfixOf)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Text.JSON (JSValue(..), toJSObject, JSObject, toJSString)
import Data.List (intercalate)

modalPdfTooLarge :: TemplatesMonad m => m FlashMessage
modalPdfTooLarge = toModal <$> renderTemplateM "pdfTooBigModal" ()

modalSignAwaitingAuthorLast :: TemplatesMonad m => m FlashMessage
modalSignAwaitingAuthorLast = toModal <$> renderTemplateM "signAwaitingAuthorLast" ()

modalSendConfirmationView :: TemplatesMonad m => Document -> m FlashMessage
modalSendConfirmationView document = do
  partylist <- renderListTemplate . map (BS.toString . getSmartName) $ partyListButAuthor document
  toModal <$> (renderTemplateForProcess document processmodalsendconfirmation $ do
    field "partyListButAuthor" partylist
    field "signatory" . listToMaybe $ map (BS.toString . getSmartName) $ partyList document
    documentInfoFields document)

modalSendInviteView :: TemplatesMonad m => Document -> m FlashMessage
modalSendInviteView document = do
  partylist <- renderListTemplate . map (BS.toString . getSmartName) $ partyListButAuthor document
  toModal <$> (renderTemplateFM "modalSendInviteView" $ do
    field "partyListButAuthor" partylist
    field "documenttitle" . BS.toString $ documenttitle document)

modalRejectedView :: TemplatesMonad m => Document -> m FlashMessage
modalRejectedView document = do
  partylist <- renderListTemplate . map (BS.toString . getSmartName) $ partyList document
  toModal <$> (renderTemplateFM "modalRejectedView" $ do
    field "partyList" partylist
    field "documenttitle" . BS.toString $ documenttitle document)

modalLoginForSaveView :: TemplatesMonad m => m FlashMessage
modalLoginForSaveView = toModal <$> renderTemplateM "modalLoginForSaveView" ()

modalSignedClosedHasAccount :: TemplatesMonad m => Document -> SignatoryLink -> Bool -> m FlashMessage
modalSignedClosedHasAccount document signatorylink isloggedin = do
  toModal <$> (renderTemplateForProcess document processmodalsignedviewclosedhasaccount $ do
    modalSignedFields document
    loginFields document signatorylink isloggedin)

modalSignedNotClosedHasAccount :: TemplatesMonad m => Document -> SignatoryLink -> Bool -> m FlashMessage
modalSignedNotClosedHasAccount document signatorylink isloggedin = do
  toModal <$> (renderTemplateForProcess document processmodalsignedviewnotclosedhasaccount $ do
    modalSignedFields document
    loginFields document signatorylink isloggedin)

modalSignedClosedNoAccount :: TemplatesMonad m => Document -> SignatoryLink -> ActionID -> MagicHash -> m FlashMessage
modalSignedClosedNoAccount document signatorylink actionid magichash = do
  toModal <$> (renderTemplateForProcess document processmodalsignedviewclosednoaccount $ do
    modalSignedFields document
    accountFromSignFields document signatorylink actionid magichash)

modalSignedNotClosedNoAccount :: TemplatesMonad m => Document -> SignatoryLink -> ActionID -> MagicHash -> m FlashMessage
modalSignedNotClosedNoAccount document signatorylink actionid magichash = do
  toModal <$> (renderTemplateForProcess document processmodalsignedviewnotclosednoaccount $ do
    modalSignedFields document
    accountFromSignFields document signatorylink actionid magichash)

modalSignedFields :: TemplatesMonad m => Document -> Fields m
modalSignedFields document@Document{ documenttitle } = do
  fieldM "partyUnsignedListString" . renderListTemplate . map (BS.toString . getSmartName) $ partyUnsignedList document
  fieldM "partyListString" . renderListTemplate . map (BS.toString . getSmartName) $ partyList document
  field "signatory" . listToMaybe $ map (BS.toString . getEmail ) $ partyList document
  field "documenttitle" $ BS.toString documenttitle

loginFields :: MonadIO m => Document -> SignatoryLink -> Bool -> Fields m
loginFields document signatorylink isloggedin = do
    field "isloggedin" isloggedin
    field "referer" $ show (LinkSignDoc document signatorylink)
    field "email" $ getEmail signatorylink
    field "linklogin" $ show (LinkLogin LoginTry)

accountFromSignFields :: MonadIO m => Document -> SignatoryLink -> ActionID -> MagicHash -> Fields m
accountFromSignFields document signatorylink actionid magichash = do
    field "linkaccountfromsign" $ show (LinkAccountFromSign document signatorylink actionid magichash)

flashDocumentDraftSaved :: TemplatesMonad m => m FlashMessage
flashDocumentDraftSaved =
  toFlashMsg SigningRelated <$> renderTemplateM "flashDocumentDraftSaved" ()


flashDocumentTemplateSaved :: TemplatesMonad m => m FlashMessage
flashDocumentTemplateSaved =
  toFlashMsg SigningRelated <$> renderTemplateM "flashDocumentTemplateSaved" ()

flashDocumentRestarted :: TemplatesMonad m => Document -> m FlashMessage
flashDocumentRestarted document = do
  toFlashMsg OperationDone <$> (renderTemplateForProcess document processflashmessagerestarted $ documentInfoFields document)

flashRemindMailSent :: TemplatesMonad m => SignatoryLink -> m FlashMessage
flashRemindMailSent signlink@SignatoryLink{maybesigninfo} =
  toFlashMsg OperationDone <$> (renderTemplateFM (template_name maybesigninfo) $ do
    field "personname" . BS.toString $ getSmartName signlink)
  where
    template_name =
      maybe "flashRemindMailSentNotSigned"
      (const "flashRemindMailSentSigned")

flashMessageCannotCancel :: TemplatesMonad m => m FlashMessage
flashMessageCannotCancel =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageCannotCancel" ()

flashMessageCanceled :: TemplatesMonad m => Document -> m FlashMessage
flashMessageCanceled document = do
  toFlashMsg SigningRelated <$> (renderTemplateForProcess document processflashmessagecanceled $ documentInfoFields document)

flashAuthorSigned :: TemplatesMonad m => m FlashMessage
flashAuthorSigned =
  toFlashMsg OperationDone <$> renderTemplateM "flashAuthorSigned" ()

flashMessageFailedToParseCSV :: TemplatesMonad m => m FlashMessage
flashMessageFailedToParseCSV =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageFailedToParseCSV" ()

flashMessageCSVHasTooManyRows :: TemplatesMonad m => Int -> m FlashMessage
flashMessageCSVHasTooManyRows maxrows =
  toFlashMsg OperationFailed <$> (renderTemplateFM "flashMessageCSVHasTooManyRows" $ field "maxrows" maxrows)

flashMessageBulkRemindsSent :: TemplatesMonad m => DocumentType -> m FlashMessage
flashMessageBulkRemindsSent doctype = do
  toFlashMsg OperationDone <$> renderTextForProcess doctype processflashmessagebulkremindssent

flashMessageNoBulkRemindsSent :: TemplatesMonad m => DocumentType -> m FlashMessage
flashMessageNoBulkRemindsSent doctype = do
  toFlashMsg OperationFailed <$> renderTextForProcess doctype processflashmessagenobulkremindssent

flashMessageSignableArchiveDone :: TemplatesMonad m => DocumentType -> m FlashMessage
flashMessageSignableArchiveDone doctype = do
  toFlashMsg OperationDone <$> renderTextForProcess doctype processflashmessagearchivedone

flashMessageTemplateArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageTemplateArchiveDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageTemplateArchiveDone" ()

flashMessageAttachmentArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageAttachmentArchiveDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageAttachmentArchiveDone" ()
  
flashMessageRubbishRestoreDone :: TemplatesMonad m => m FlashMessage
flashMessageRubbishRestoreDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageRubbishRestoreDone" ()
  
flashMessageRubbishHardDeleteDone :: TemplatesMonad m => m FlashMessage
flashMessageRubbishHardDeleteDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageRubbishHardDeleteDone" ()

flashMessageInvalidCSV :: TemplatesMonad m => m FlashMessage
flashMessageInvalidCSV =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageInvalidCSV" ()

flashMessageCSVSent :: TemplatesMonad m => Int -> m FlashMessage
flashMessageCSVSent doccount =
  toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageCSVSent" $ field "doccount" doccount)

flashMessageSingleTemplateShareDone :: TemplatesMonad m => BS.ByteString -> m FlashMessage
flashMessageSingleTemplateShareDone docname =
  toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageSingleTemplateShareDone" $ field "docname" docname)

flashMessageMultipleTemplateShareDone :: TemplatesMonad m => m FlashMessage
flashMessageMultipleTemplateShareDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageMultipleTemplateShareDone" ()

flashMessageSingleAttachmentShareDone :: TemplatesMonad m => BS.ByteString -> m FlashMessage
flashMessageSingleAttachmentShareDone docname =
  toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageSingleAttachmentShareDone" $ field "docname" docname)

flashMessageMultipleAttachmentShareDone :: TemplatesMonad m => m FlashMessage
flashMessageMultipleAttachmentShareDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageMultipleAttachmentShareDone" ()

flashMessageAccountActivatedFromSign :: TemplatesMonad m => m FlashMessage
flashMessageAccountActivatedFromSign =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageAccountActivatedFromSign" ()

flashMessageAccountRemovedFromSign :: TemplatesMonad m => m FlashMessage
flashMessageAccountRemovedFromSign =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageAccountRemovedFromSign" ()

flashMessageOnlyHaveRightsToViewDoc :: TemplatesMonad m => m FlashMessage
flashMessageOnlyHaveRightsToViewDoc =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageOnlyHaveRightsToViewDoc" ()

flashMessagePleaseSignWithEleg :: TemplatesMonad m => m FlashMessage
flashMessagePleaseSignWithEleg =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessagePleaseSignWithEleg" ()

flashMessagePleaseSign :: TemplatesMonad m => Document -> m FlashMessage
flashMessagePleaseSign document = do
  toFlashMsg OperationDone <$> renderTextForProcess document processflashmessagepleasesign

docForListJSON :: (TemplatesMonad m) => MinutesTime -> Document -> m (JSObject JSValue)
docForListJSON crtime doc = (fmap toJSObject) $ propagateMonad  $
                [ ("fields" , jsonPack <$> docFieldsListForJSON crtime doc),
                  ("subfields" , JSArray <$>  fmap jsonPack <$> mapM (signatoryFieldsListForJSON crtime doc) (documentsignatorylinks doc)),
                  ("link", return $ JSString $ toJSString $  show $ LinkIssueDoc $ documentid doc)
                ]


jsonPack :: [(String,String)] -> JSValue
jsonPack = JSObject . toJSObject . (mapSnd (JSString . toJSString))

docFieldsListForJSON :: (TemplatesMonad m) => MinutesTime -> Document -> m [(String,String)]
docFieldsListForJSON crtime doc =  propagateMonad [
    ("id", return $ show $ documentid doc),
    ("title",return $  BS.toString $ documenttitle doc),
    ("status", return $ show $ documentStatusClass doc),
    ("party", return $ intercalate ", " $ map (BS.toString . getSmartName) . filter isSignatory $ documentsignatorylinks doc),
    ("partner", return $ intercalate ", " $ map (BS.toString . getSmartName) $ filter (not . isAuthor) (documentsignatorylinks doc)),
    ("partnercomp", return $ intercalate ", " $ map (BS.toString .  getCompanyName) $ filter (not . isAuthor) (documentsignatorylinks doc)),
    ("author", return $ intercalate ", " $ map (BS.toString . getSmartName) $ filter (isAuthor) $ (documentsignatorylinks doc)),
    ("time", return $ showDateAbbrev crtime (documentmtime doc)),
    ("type", renderTextForProcess doc processname),
    ("shared", return $ show $ (documentsharing doc)==Shared)
    ]

signatoryFieldsListForJSON :: (TemplatesMonad m) => MinutesTime -> Document ->  SignatoryLink -> m [(String,String)]
signatoryFieldsListForJSON crtime doc sl = propagateMonad [
    ("status", return $ show $ signatoryStatusClass doc sl ),
    ("name", return $ BS.toString $ getSmartName sl ),
    ("time", return $ fromMaybe "" $ (showDateAbbrev crtime) <$> (sign `mplus` reject `mplus` seen `mplus` open))
    ]
    where
        sign = signtime <$> maybesigninfo sl
        seen = signtime <$> maybesigninfo sl
        reject = case documentrejectioninfo doc of
                    Just (rt, slid, _)
                        | slid == signatorylinkid sl -> Just $ rt
                    _                             -> Nothing
        open = maybereadinvite sl

  

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


documentStatusClass :: Document -> StatusClass
documentStatusClass doc =
  case (map (signatoryStatusClass doc) $ filter isSignatory $ documentsignatorylinks doc) of
    [] -> SCDraft
    xs -> minimum xs

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
docSortFunc "party" = comparePartners
docSortFunc "partyREV" = revComparePartners
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

pageContractsList :: TemplatesMonad m => User -> m String
pageContractsList = pageList' "pageContractsList" LinkContracts

pageTemplatesList :: TemplatesMonad m => User -> m String
pageTemplatesList = pageList' "pageTemplatesList" LinkTemplates

pageAttachmentList :: TemplatesMonad m =>  User -> m String
pageAttachmentList = pageList' "pageAttachmentList" LinkAttachments

pageOffersList :: TemplatesMonad m => User -> m String
pageOffersList = pageList' "pageOffersList" LinkOffers

pageOrdersList :: TemplatesMonad m => User -> m String
pageOrdersList = pageList' "pageOrdersList" LinkOrders

pageRubbishBinList :: TemplatesMonad m => User ->  m String
pageRubbishBinList = pageList' "pageRubbishBinList" LinkRubbishBin

{- |
    Helper function for list pages
-}
pageList' :: TemplatesMonad m
          => String
          -> KontraLink
          -> User
          -> m String
pageList' templatename currentlink user  =
  renderTemplateFM templatename $ do
    field "canReallyDeleteDocs" $ useriscompanyadmin user || isNothing (usercompany user)
    field "currentlink" $ show $ currentlink
    field "linkdoclist" $ show $ LinkContracts 
    field "documentactive" $ (LinkContracts == currentlink)
    field "linkofferlist" $ show $ LinkOffers 
    field "offeractive" $ (LinkOffers == currentlink)
    field "linkorderlist" $ show $ LinkOrders
    field "orderactive" $ (LinkOrders == currentlink)
    field "linktemplatelist" $ show $ LinkTemplates
    field "templateactive" $ (LinkTemplates == currentlink)
    field "linkattachmentlist" $ show $ LinkAttachments 
    field "attachmentactive" $ (LinkAttachments == currentlink)
    field "linkrubbishbinlist" $ show $ LinkRubbishBin
    field "rubbishbinactive" $ (LinkRubbishBin == currentlink)

showFileImages :: TemplatesMonad m => DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> File -> JpegPages -> m String
showFileImages _ _ _ JpegPagesPending =
  renderTemplateM "showFileImagesPending" ()

showFileImages _ _ _ (JpegPagesError normalizelog) =
  renderTemplateFM "showFileImagesError" $ do
    field "normalizelog" $ BS.toString normalizelog

showFileImages docid mtokens File{fileid} (JpegPages jpgpages) =
  renderTemplateFM "showFileImagesReady" $ do
    field "pageurl" $ "/pages/" ++ pageurl mtokens
    fieldFL "images" . map page $ zip ([1..]::[Int]) jpgpages
  where
    pageurl Nothing =  show docid ++ "/" ++ show fileid
    pageurl (Just (siglinkid, sigmagichash)) =
           show docid ++ "/" ++ show siglinkid ++ "/"
        ++ show sigmagichash ++ "/" ++ show fileid
    page (x,(_,w,h)) = do
      field "number" x
      field "width" w
      field "height" h

showFilesImages2 :: TemplatesMonad m => DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> [(File, JpegPages)] -> m String
showFilesImages2 docid mtokens files = do
  filesPages <- sequence $ map (uncurry (showFileImages docid mtokens)) files
  renderTemplateFM "spanNoEscape" $ field "it" (concat filesPages)


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

pageAttachmentForSignatory :: TemplatesMonad m
                           => Document
                           -> SignatoryLink
                           -> m String
pageAttachmentForSignatory doc siglink = pageAttachment' False (Just siglink) doc

pageAttachmentView :: TemplatesMonad m
                   => Document
                   -> m String
pageAttachmentView = pageAttachment' False Nothing

pageAttachmentDesign :: TemplatesMonad m
                     => Document
                     -> m String
pageAttachmentDesign = pageAttachment' True Nothing

pageAttachment' :: TemplatesMonad m
                => Bool
                -> Maybe SignatoryLink
                -> Document
                -> m String
pageAttachment' iseditable msiglink doc@Document {documentid, documenttitle} =
    renderTemplateFM "pageAttachment" $ do
      field "documentid" $ show documentid
      field "documenttitle" $ BS.toString documenttitle
      field "editable" $ iseditable
      field "renamelink" $ show $ LinkRenameAttachment documentid
      field "siglinkid" $ fmap (show . signatorylinkid) msiglink
      field "sigmagichash" $ fmap (show . signatorymagichash) msiglink
      field "linkissuedocpdf" $ show (LinkIssueDocPDF msiglink doc)

pageDocumentDesign :: TemplatesMonad m
                   => Context
                   -> Document
                   -> (Maybe DesignStep)
                   -> [Document]
                   -> m String
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
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       doc_author_otherfields fields = sequence .
         map (\(fd, i) ->
           renderTemplateFM "customfield" $ do
             field "otherFieldValue" $ fieldvalue fd
             field "otherFieldName"  $ fieldlabel fd
             field "otherFieldID"    $ "field" ++ show i
             field "otherFieldOwner" "author")
             $ zip fields ([1..]::[Int])
       authorsiglink = fromJust $ getAuthorSigLink document
   in do
     csvstring <- renderTemplateM "csvsendoutsignatoryattachmentstring" ()
     csvfields <- documentCsvFields document
     renderTemplateFM "pageDocumentDesign" $ do
       fieldM "authorOtherFields" $ doc_author_otherfields $ signatoryotherfields $ signatorydetails authorsiglink
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "documentinvitetext" $ documentinvitetext
       fieldM "invitationMailContent" $ mailInvitationToSignOrViewContent False ctx document Nothing
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
       fieldF "process" processFields
   where
     getProcessText = renderTextForProcess document
     getProcessValue = getValueForProcess document
     processFields = do
       field "isbasicavailable" $ getProcessValue processbasicavailable
       field "isauthorsend" $ getProcessValue processauthorsend
       field "isvalidationchoiceforbasic" $ getProcessValue processvalidationchoiceforbasic
       field "isexpiryforbasic" $ getProcessValue processexpiryforbasic
       fieldM "title" $ getProcessText processtitle
       fieldM "step1text" $ getProcessText processstep1text
       fieldM "expirywarntext" $ getProcessText processexpirywarntext
       fieldM "sendbuttontext" $ getProcessText processsendbuttontext
       fieldM "expirywarntext" $ getProcessText processexpirywarntext
       fieldM "confirmsendtitle" $ getProcessText processconfirmsendtitle
       fieldM "confirmsendtext" $ getProcessText processconfirmsendtext
       fieldM "expirytext" $ getProcessText processexpirytext


documentAttachmentDesignFields :: (Functor m, MonadIO m) => [AuthorAttachment] -> Fields m
documentAttachmentDesignFields atts = do
  field "isattachments" $ not $ null atts
  field "attachmentcount" $ length atts
  fieldFL "attachments" $ map attachmentFields atts
  where
    attachmentFields AuthorAttachment{authorattachmentfile = File {fileid, filename}} = do
      field "attachmentid" $ show fileid
      field "attachmentname" $ filename

documentFunctionalityFields :: MonadIO m => Document -> Fields m
documentFunctionalityFields Document{documentfunctionality} = do
  field "docfunctionality" $ show documentfunctionality
  field "isbasic" $ documentfunctionality==BasicFunctionality

documentCsvFields :: TemplatesMonad m => Document -> m (Fields m)
documentCsvFields document@Document{documentallowedidtypes, documentcsvupload} =  do
  let csvcustomfields = either (const [BS.fromString ""]) id $ getCSVCustomFields document
      mcleancsv = (cleanCSVContents documentallowedidtypes (length csvcustomfields) . csvcontents) <$> documentcsvupload
      csvproblems = maybe [] fst mcleancsv
      csvdata = maybe [] (csvbody . snd) mcleancsv
      csvPageSize :: Int = 10
      csvpages = splitCSVDataIntoPages csvPageSize csvdata
  csvproblemfields <- sequence $ zipWith (csvProblemFields (length csvproblems)) [1..] csvproblems
  return $ do
    fieldFL "csvproblems" $ csvproblemfields
    field "csvproblemcount" $ length csvproblems
    fieldFL "csvpages" $ zipWith (csvPageFields csvproblems (length csvdata)) [0,csvPageSize..] csvpages
    field "csvrowcount" $ length csvdata
    field "csvcustomfields" $ csvcustomfields
    field "isvalidcsv" $ null csvproblems
    field "csvsigindex" $ fmap csvsignatoryindex documentcsvupload

csvPageFields :: TemplatesMonad m => [CSVProblem] -> Int -> Int -> [[BS.ByteString]] -> Fields m
csvPageFields problems totalrowcount firstrowindex xs = do
  fieldFL "csvrows" $ zipWith (csvRowFields problems) [firstrowindex..] xs
  field "isfirstcsvpage" $ firstrowindex==0
  field "islastcsvpage" $ (firstrowindex+(length xs))==totalrowcount

splitCSVDataIntoPages :: Int -> [a] -> [[a]]
splitCSVDataIntoPages n xs =
  case splitAt n xs of
    (y,[]) -> [y]
    (y,ys) -> y : splitCSVDataIntoPages n ys

csvRowFields :: TemplatesMonad m => [CSVProblem] -> Int -> [BS.ByteString] -> Fields m
csvRowFields problems rowindex xs = do
  field "rownumber" $ rowindex + 1
  fieldFL "csvfields" $ zipWith (csvFieldFields problems rowindex)
                              [0..]
                              xs
  field "isproblem" $ any isRelevantProblem problems
  where
    isRelevantProblem CSVProblem{problemrowindex, problemcolindex} =
      case (problemrowindex, problemcolindex) of
        (Just r, Nothing) | rowindex==r -> True
        _ -> False

csvFieldFields :: TemplatesMonad m => [CSVProblem] -> Int -> Int -> BS.ByteString -> Fields m
csvFieldFields problems rowindex colindex val = do
  field "value" $ val
  field "isproblem" $ any isRelevantProblem problems
  where
    isRelevantProblem CSVProblem{problemrowindex, problemcolindex} =
      case (problemrowindex, problemcolindex) of
        (Just r, Just c) | rowindex==r && colindex==c -> True
        _ -> False

csvProblemFields :: TemplatesMonad m => Int -> Int -> CSVProblem -> m (Fields m)
csvProblemFields probcount number csvproblem = do
    flashMsg <- problemdescription csvproblem
    let desc = snd $ fromJust $ unFlashMessage flashMsg
    return $ do
      field "problemnumber" $ number
      field "problemrow" $ fmap (+1) $ problemrowindex csvproblem
      field "problemcol" $ fmap (+1) $ problemcolindex csvproblem
      field "problemdesc" $ desc
      field "isfirstproblem" $ (number==1)
      field "islastproblem" $ (number==probcount)

{- | Showing document to author after we are done with design -}

pageDocumentForAuthor :: TemplatesMonad m
                      => Context
                      -> Document
                      -> m String
pageDocumentForAuthor ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
  }
  =
   let
       authorsiglink = fromJust $ getAuthorSigLink document
   in do
     csvstring <- renderTemplateM "csvsendoutsignatoryattachmentstring" ()
     renderTemplateFM "pageDocumentForAuthor" $ do
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       fieldFL "signatories" $ map (signatoryLinkFields ctx document Nothing) $ signatoriesWithSecretary document
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       fieldM "cancelMailContent" $ mailCancelDocumentByAuthorContent False Nothing ctx document
       field "linkcancel" $ show $ LinkCancel document
       field "docstate" (buildJS (signatorydetails authorsiglink) documentsignatorylinks)
       field "linkissuedocpdf" $ show (LinkIssueDocPDF Nothing document)
       fieldM "documentinfotext" $ documentInfoText ctx document (find isAuthor documentsignatorylinks)
       documentAuthorInfo document
       documentInfoFields document
       documentViewFields document
       documentAttachmentViewFields documentid Nothing (documentauthorattachments document)
       documentSigAttachmentViewFields csvstring documentid documentsignatorylinks Nothing (documentsignatoryattachments document)
       fieldF "process" processFields
   where
     getProcessText = renderTextForProcess document
     processFields = do
       fieldM "title" $ getProcessText processtitle
       fieldM "restartbuttontext" $ getProcessText processrestartbuttontext
       fieldM "cancelbuttontext" $ getProcessText processcancelbuttontext
       fieldM "rejectbuttontext" $ getProcessText processrejectbuttontext
       fieldM "cancelbyauthormodaltitle" $ getProcessText processcancelbyauthormodaltitle
       fieldM "signatorysignedtext" $ getProcessText processsignatorysignedtext
       fieldM "signatorycanceledtext" $ getProcessText processsignatorycanceledtext
       fieldM "authorissecretarytext" $ getProcessText processauthorissecretarytext
       fieldM "remindagainbuttontext" $ getProcessText processremindagainbuttontext
       signatoryMessageProcessFields document

{- |
   Show the document for Viewers (friends of author or signatory).
   Show no buttons or other controls
 -}

pageDocumentForViewer :: TemplatesMonad m => Context -> Document -> Maybe SignatoryLink -> m String
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
     csvstring <- renderTemplateM "csvsendoutsignatoryattachmentstring" ()
     invitationMailContent <- mailInvitationToSignOrViewContent False ctx document Nothing
     cancelMailContent <- mailCancelDocumentByAuthorContent False Nothing ctx document
     documentinfotext <- documentInfoText ctx document Nothing
     renderTemplateFM "pageDocumentForViewerContent" $  do
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $ invitationMailContent
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "anyinvitationundelivered" $ anyInvitationUndelivered document
       field "undelivered" $ map getEmail $ undeliveredSignatoryLinks document
       fieldFL "signatories" $ map (signatoryLinkFields ctx document Nothing) $ signatoriesWithSecretary document
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
       fieldF "process" $ fieldM "title" $ renderTextForProcess document processtitle
       signatoryMessageProcessFields document

signatoryMessageProcessFields :: TemplatesMonad m => Document -> Fields m
signatoryMessageProcessFields document = do
  fieldM "signatorysignedtext" $ getProcessText processsignatorysignedtext
  fieldM "signatorycanceledtext" $ getProcessText processsignatorycanceledtext
  fieldM "authorissecretarytext" $ getProcessText processauthorissecretarytext
  fieldM "remindagainbuttontext" $ getProcessText processremindagainbuttontext
  where
    getProcessText = renderTextForProcess document

documentAttachmentViewFields :: (Functor m, MonadIO m) => DocumentID -> Maybe SignatoryLink -> [AuthorAttachment] -> Fields m
documentAttachmentViewFields docid msignlink atts = do
  field "isattachments" $ length atts > 0
  fieldFL "attachments" $ map attachmentFields atts
  where
    attachmentFields AuthorAttachment{ authorattachmentfile = File{ filename, fileid } } = do
      field "attachmentname" filename
      field "linkattachment" $ case msignlink of
        Just signlink -> show (LinkAttachmentForViewer docid (signatorylinkid signlink) (signatorymagichash signlink) fileid)
        Nothing -> show (LinkAttachmentForAuthor docid fileid)

documentSigAttachmentViewFields :: (Functor m, MonadIO m) => String -> DocumentID -> [SignatoryLink] -> Maybe SignatoryLink -> [SignatoryAttachment] -> Fields m
documentSigAttachmentViewFields csvstring docid sls msignlink atts = do
  field "hassigattachments" $ length atts > 0
  fieldFL "sigattachments" $ map sigAttachmentFields atts
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

pageDocumentForSignatory :: TemplatesMonad m
                         => KontraLink
                         -> Document
                         -> Context
                         -> SignatoryLink
                         -> m String
pageDocumentForSignatory action document ctx invitedlink  = do
  csvstring <- renderTemplateM "csvsendoutsignatoryattachmentstring" ()
  listyou <- renderTemplateM "listyou" ()
  renderTemplateFM "pageDocumentForSignContent" $ do
    mainFields csvstring listyou
    fieldF "process" $ processFields csvstring listyou
    where
    mainFields csvstring listyou =
      let authorsiglink = fromJust $ getAuthorSigLink document
          localscripts =
            "var docstate = "
            ++ (buildJS (signatorydetails authorsiglink) $ documentsignatorylinks document)
            ++ "; docstate['useremail'] = '"
            ++ (BS.toString $ getEmail invitedlink)
            ++ "';"
          allowedtypes = documentallowedidtypes document
          requiresEleg = isJust $ find (== ELegitimationIdentification) allowedtypes
          invitedemail = getEmail invitedlink
          sigattachments = [a | a <- documentsignatoryattachments document
                              , signatoryattachmentemail a == invitedemail]
          hassigattachments = length sigattachments > 0
          otherunsignedpartynames = map (BS.toString . getSmartName) . filter ((/=) invitedemail . getEmail) $ partyUnsignedList document
          unsignedpartynames = 
            if hasSigned invitedlink
              then otherunsignedpartynames
              else listyou : otherunsignedpartynames --refer to yourself in the list, like "you, bob and jim" or "du, bob och jim"
      in do
        field "localscripts" localscripts
        fieldFL "signatories" $ map (signatoryLinkFields ctx document (Just invitedlink)) $ signatoriesWithSecretary document
        fieldM "rejectMessage" $  mailRejectMailContent Nothing ctx (getSmartName authorsiglink) document invitedlink
        fieldM "partyUnsigned" $ renderListTemplate unsignedpartynames
        field "action" $ show action
        field "linkissuedocpdf" $ show (LinkIssueDocPDF (Just invitedlink) document)
        fieldM "documentinfotext" $ documentInfoText ctx document (Just invitedlink)
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
    getProcessTextWithFields f csvstring listyou = renderTemplateForProcess document f (mainFields csvstring listyou)
    getProcessText = renderTextForProcess document
    processFields csvstring listyou = do
      fieldM "signatorysignmodaltitle" $ getProcessText processsignatorysignmodaltitle
      fieldM "signatorysignmodalcontent" $ getProcessTextWithFields processsignatorysignmodalcontent csvstring listyou
      fieldM "signbuttontext" $ getProcessText processsignbuttontext
      fieldM "signatorycancelmodaltitle" $ getProcessTextWithFields processsignatorycancelmodaltitle csvstring listyou
      fieldM "rejectbuttontext" $ getProcessText processrejectbuttontext
      fieldM "title" $ getProcessText processtitle
      field "requiressignguard" $ getValueForProcess document processrequiressignguard
      fieldM "signguardwarntext" $ getProcessText processsignguardwarntext
      signatoryMessageProcessFields document

documentSingleSignatoryAttachmentsFields :: (Functor m, MonadIO m) => DocumentID -> SignatoryLinkID -> MagicHash -> [SignatoryAttachment] -> Fields m
documentSingleSignatoryAttachmentsFields docid sid mh atts =
  fieldFL "mysigattachments" $ for atts
  (\a -> do
      field "name" $ signatoryattachmentname a
      field "desc" $ signatoryattachmentdescription a
      field "filename" $ fmap filename $ signatoryattachmentfile a
      field "fileid" $ fmap (unFileID . fileid) $ signatoryattachmentfile a
      field "viewerlink" $ fmap (show . LinkAttachmentForViewer docid sid mh . fileid) $ signatoryattachmentfile a
  )

--- Display of signatory
signatoryLinkFields :: TemplatesMonad m => Context -> Document -> Maybe SignatoryLink -> SignatoryLink -> Fields m
signatoryLinkFields
  ctx@Context {ctxmaybeuser = muser}
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
      fieldFL "fields" $ for (signatoryotherfields signatorydetails) $ \sof -> do
        field "fieldlabel" $ fieldlabel sof
        field "fieldvalue" $ fieldvalue sof
      field "signorder" $ unSignOrder $ signatorysignorder signatorydetails
      field "allowRemindForm" $ isEligibleForReminder muser document siglnk
      field "linkremind" $ show (LinkRemind document siglnk)
      field "linkchangeemail" $  show $ LinkChangeSignatoryEmail (documentid document) signatorylinkid
      field "allowEmailChange" $ (isCurrentUserAuthor && (invitationdeliverystatus == Undelivered || invitationdeliverystatus == Deferred) && isActiveDoc)
      fieldM "reminderMessage" $ mailDocumentRemindContent Nothing ctx document siglnk
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

signatoryStatusFields :: MonadIO m => Document -> SignatoryLink -> (MinutesTime -> String) -> Fields m
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
documentInfoText :: TemplatesMonad m => Context -> Document -> Maybe SignatoryLink -> m String
documentInfoText ctx document siglnk =
  renderTemplateFM "documentInfoText" $ do
    mainFields
    fieldF "process" processFields
  where
    mainFields = do
      documentInfoFields document
      documentAuthorInfo document
      fieldFL "signatories" $ map (signatoryLinkFields ctx document Nothing) $ documentsignatorylinks document
      signedByMeFields document siglnk
    getProcessText = renderTextForProcess document
    getProcessTextWithFields f = renderTemplateForProcess document f mainFields
    processFields = do
      fieldM "pendingauthornotsignedinfoheader" $ getProcessText processpendingauthornotsignedinfoheader
      fieldM "pendingauthornotsignedinfotext" $ getProcessText processpendingauthornotsignedinfotext
      fieldM "pendingauthorinfoheader" $ getProcessText processpendingauthorinfoheader
      fieldM "pendingauthorinfotext" $ getProcessTextWithFields processpendingauthorinfotext
      fieldM "cancelledinfoheader" $ getProcessText processcancelledinfoheader
      fieldM "cancelledinfotext" $ getProcessTextWithFields processcancelledinfotext
      fieldM "signedinfoheader" $ getProcessText processsignedinfoheader
      fieldM "signedinfotext" $ getProcessTextWithFields processsignedinfotext
      fieldM "statusinfotext" $ getProcessTextWithFields processstatusinfotext

-- | Basic info about document , name, id ,author
documentInfoFields :: MonadIO m => Document -> Fields m
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

documentAuthorInfo :: MonadIO m => Document -> Fields m
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
documentStatusFields :: MonadIO m => Document -> Fields m
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
signedByMeFields :: MonadIO m => Document -> Maybe SignatoryLink -> Fields m
signedByMeFields _document siglnk = do
  field "notsignedbyme" $ (isJust siglnk) && (isNothing $ maybesigninfo $ fromJust siglnk)
  field "signedbyme" $ (isJust siglnk) && (isJust $ maybesigninfo $ fromJust siglnk)
  field "iamauthor" $ maybe False isAuthor siglnk


documentViewFields :: MonadIO m => Document -> Fields m
documentViewFields Document{documentstatus} = do
  field "addSignatoryScript" $ documentstatus /= Closed


designViewFields :: MonadIO m => Maybe DesignStep -> Fields m
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


uploadPage :: TemplatesMonad m => (Maybe DocumentProcess) -> Bool -> m String
uploadPage mdocprocess showTemplates = renderTemplateFM "uploadPage" $ do
    field "isprocessselected" $ isJust mdocprocess
    field "showTemplates" showTemplates
    fieldFL "processes" $ map processFields [Contract,Offer,Order]
    field "processid" $ show <$> mdocprocess
    case mdocprocess of
      Just selecteprocess -> do
        fieldF "selectedprocess" $ processFields selecteprocess
      _ -> return ()
    where
      processFields process = do
        field "id" $ show process
        field "selected" $ (Just process == mdocprocess)
        fieldM "name" $ renderTextForProcess (Signable process) processuploadname
        fieldM "uploadprompttext" $ renderTextForProcess (Signable process) processuploadprompttext


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
documentsToFixView :: TemplatesMonad m => [Document] -> m String
documentsToFixView docs = do
    renderTemplateFM "documentsToFixView" $ do
        fieldFL "documents" $ for docs $ \doc -> do
            field "title" $ documenttitle doc
            field "id" $ show $ documentid doc
            field "involved" $ map getEmail  $ documentsignatorylinks doc
            field "cdate" $  show $ documentctime doc

documentAuthorAttachments :: (Functor m, MonadIO m) => [Document] -> Fields m
documentAuthorAttachments attachments =
  fieldFL "existingattachments" $
  for attachments (\doc -> do
                      field "attachmentid" $ show (documentid doc)
                      field "attachmentname" $ documenttitle doc)

documentSignatoryAttachments :: (Functor m, MonadIO m) => String -> Document -> [SignatoryAttachment] -> Fields m
documentSignatoryAttachments csvstring doc attachments =
  let ats = buildattach csvstring doc attachments []
  in fieldFL "sigattachments" $
     for ats (\(n, d, sigs) -> do
                 field "attachmentname" n
                 field "attachmentdescription" d
                 fieldFL "signatories" $
                   for sigs (\(name, email) -> do
                                field "signame" name
                                field "sigemail" email))

