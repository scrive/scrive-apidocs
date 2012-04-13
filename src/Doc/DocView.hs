module Doc.DocView (
    documentAuthorInfo
  , documentInfoFields
  , flashAuthorSigned
  , flashDocumentDraftSaved
  , flashDocumentRestarted
  , flashDocumentTemplateSaved
  , flashMessageRubbishRestoreDone
  , flashMessageRubbishHardDeleteDone
  , flashMessageBulkRemindsSent
  , flashMessageCSVSent
  , flashMessageCanceled
  , flashMessageCannotCancel
  , flashMessageInvalidCSV
  , flashMessageMultipleAttachmentShareDone
  , flashMessageMultipleTemplateShareDone
  , flashMessageNoBulkRemindsSent
  , flashMessageSingleAttachmentShareDone
  , flashMessageSingleTemplateShareDone
  , flashRemindMailSent
  , getDataMismatchMessage
  , modalMismatch
  , modalPdfTooLarge
  , mailDocumentAwaitingForAuthor
  , mailDocumentClosed
  , mailDocumentRejected
  , mailDocumentRemind
  , mailInvitation
  , modalLoginForSaveView
  , modalRejectedView
  , modalSendInviteView
  , modalSignAwaitingAuthorLast
  , modalSendConfirmationView
  , pageAttachmentDesign
  , pageDocumentDesign
  , pageDocumentView
  , pageDocumentSignView
  , showFilesImages2
  , signatoryDetailsFromUser
  , documentsToFixView
  , uploadPage
  , documentJSON
  , csvLandPage
  ) where

import AppView (kontrakcja, standardPageFields)
import API.Service.Model
import Company.Model
import Control.Logic
import Doc.DocProcess
import Doc.DocRegion
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocViewMail
import FlashMessage
import Kontra
import KontraLink
import MagicHash (MagicHash)
import MinutesTime
import Misc
import Templates.Templates
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import User.Model
import Doc.JSON()
import Doc.DocInfo
import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Maybe
import Text.JSON
import Data.List (sortBy)
import File.Model
import DB.Classes
import PadQueue.Model
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J

modalMismatch :: TemplatesMonad m => String -> SignatoryLink -> m FlashMessage
modalMismatch msg author = toModal <$>  do
    renderTemplateFM "signCanceledDataMismatchModal" $ do
                    field "authorname"  $ getSmartName author
                    field "authoremail" $ getEmail author
                    field "message"     $ concatMap para $ lines msg

modalPdfTooLarge :: TemplatesMonad m => m FlashMessage
modalPdfTooLarge = toModal <$> renderTemplateM "pdfTooBigModal" ()

modalSignAwaitingAuthorLast :: TemplatesMonad m => m FlashMessage
modalSignAwaitingAuthorLast = toModal <$> renderTemplateM "signAwaitingAuthorLast" ()

modalSendConfirmationView :: TemplatesMonad m => Document -> m FlashMessage
modalSendConfirmationView document = do
  partylist <- renderListTemplate . map getSmartName $ partyListButAuthor document
  toModal <$> (renderTemplateForProcess document processmodalsendconfirmation $ do
    field "partyListButAuthor" partylist
    field "signatory" . listToMaybe $ map getSmartName $ partyList document
    -- field "signed" $ isJust $ join (maybesigninfo <$> getAuthorSigLink document)
    documentInfoFields document)

modalSendInviteView :: TemplatesMonad m => Document -> m FlashMessage
modalSendInviteView document = do
  partylist <- renderListTemplate . map getSmartName $ partyListButAuthor document
  toModal <$> (renderTemplateFM "modalSendInviteView" $ do
    field "partyListButAuthor" partylist
    field "documenttitle" $ documenttitle document)

modalRejectedView :: TemplatesMonad m => Document -> m FlashMessage
modalRejectedView document = do
  partylist <- renderListTemplate . map getSmartName $ partyList document
  toModal <$> (renderTemplateFM "modalRejectedView" $ do
    field "partyList" partylist
    field "documenttitle" $ documenttitle document)

modalLoginForSaveView :: TemplatesMonad m => m FlashMessage
modalLoginForSaveView = toModal <$> renderTemplateM "modalLoginForSaveView" ()

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
    field "personname" $ getSmartName signlink)
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

flashMessageBulkRemindsSent :: TemplatesMonad m => DocumentType -> m FlashMessage
flashMessageBulkRemindsSent doctype = do
  toFlashMsg OperationDone <$> renderTextForProcess doctype processflashmessagebulkremindssent

flashMessageNoBulkRemindsSent :: TemplatesMonad m => DocumentType -> m FlashMessage
flashMessageNoBulkRemindsSent doctype = do
  toFlashMsg OperationFailed <$> renderTextForProcess doctype processflashmessagenobulkremindssent

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

flashMessageSingleTemplateShareDone :: TemplatesMonad m => String -> m FlashMessage
flashMessageSingleTemplateShareDone docname =
  toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageSingleTemplateShareDone" $ field "docname" docname)

flashMessageMultipleTemplateShareDone :: TemplatesMonad m => m FlashMessage
flashMessageMultipleTemplateShareDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageMultipleTemplateShareDone" ()

flashMessageSingleAttachmentShareDone :: TemplatesMonad m => String -> m FlashMessage
flashMessageSingleAttachmentShareDone docname =
  toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageSingleAttachmentShareDone" $ field "docname" docname)

flashMessageMultipleAttachmentShareDone :: TemplatesMonad m => m FlashMessage
flashMessageMultipleAttachmentShareDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageMultipleAttachmentShareDone" ()

documentJSON :: (TemplatesMonad m, KontraMonad m, DBMonad m) => PadQueue -> Maybe SignatoryLink -> MinutesTime -> Document -> m JSValue
documentJSON pq msl _crttime doc = do
    ctx <- getContext
    files <- runDB $ documentfilesM doc
    sealedfiles <- runDB $ documentsealedfilesM doc
    authorattachmentfiles <- mapM (runDBQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments doc)
    let isauthoradmin = maybe False (flip isAuthorAdmin doc) (ctxmaybeuser ctx)
    mauthor <- maybe (return Nothing) (runDBQuery . GetUserByID) (getAuthorSigLink doc >>= maybesignatory)
    mservice <- maybe (return Nothing) (runDBQuery . GetService) (documentservice doc)
    mcompany <- maybe (return Nothing) (runDBQuery . GetCompany) (getAuthorSigLink doc >>= maybecompany)
    let logo  = if (isJust mcompany && isJust (companylogo $ companyui (fromJust mcompany)))
                  then show <$> LinkCompanyLogo <$> companyid <$> mcompany
                  else if ((isJust mservice) &&  (isJust $ servicelogo $ serviceui $ fromJust mservice))
                        then show <$> LinkServiceLogo <$> serviceid <$> mservice
                        else Nothing
    let bbc  = if (isJust mcompany && isJust (companybarsbackground $ companyui (fromJust mcompany)))
                  then companybarsbackground $ companyui (fromJust mcompany)
                  else if ((isJust mservice) &&  (isJust $ servicebarsbackground $ serviceui $ fromJust mservice))
                        then servicebarsbackground $ serviceui $ fromJust mservice
                        else Nothing
    let bbtc  = if (isJust mcompany && isJust (companybarstextcolour $ companyui (fromJust mcompany)))
                  then companybarstextcolour $ companyui (fromJust mcompany)
                  else Nothing
    runJSONGenT $ do
      J.value "title" $ documenttitle doc
      J.value "files" $ map fileJSON files
      J.value "sealedfiles" $ map fileJSON sealedfiles
      J.value "authorattachments" $ map fileJSON (catMaybes authorattachmentfiles)
      J.object "process" $ processJSON doc
      J.value "region" $ regionJSON doc
      J.value "infotext" =<< lift (documentInfoText ctx doc msl)
      J.value "canberestarted" $ isAuthor msl && ((documentstatus doc) `elem` [Canceled, Timedout, Rejected])
      J.value "canbecanceled" $ (isAuthor msl || isauthoradmin) && documentstatus doc == Pending && not (canAuthorSignLast doc) && isNothing (documenttimeouttime doc)
      J.value "canseeallattachments" $ isAuthor msl || isauthoradmin
      J.value "timeouttime" $ jsonDate $ unTimeoutTime <$> documenttimeouttime doc
      J.value "status" $ show $ documentstatus doc
      J.objects "signatories" $ map (signatoryJSON pq doc msl) (documentsignatorylinks doc)
      J.value "signorder" $ unSignOrder $ documentcurrentsignorder doc
      J.value "authorization" $ authorizationJSON $ head $ (documentallowedidtypes doc) ++ [EmailIdentification]
      J.value "template" $ isTemplate doc
      J.value "functionality" $ "basic" <| documentfunctionality doc == BasicFunctionality |> "advanced"
      J.value "daystosign" $ documentdaystosign doc
      J.value "invitationmessage" $ documentinvitetext doc
      J.value "logo" logo
      J.value "barsbackgroundcolor" bbc
      J.value "barsbackgroundtextcolor" bbtc
      J.value "author" $ authorJSON mauthor mcompany
      J.value "whitelabel" $ isJust mservice

authorizationJSON :: IdentificationType -> JSValue
authorizationJSON EmailIdentification = toJSValue "email"
authorizationJSON ELegitimationIdentification = toJSValue "eleg"
authorizationJSON PadIdentification = toJSValue "pad"

signatoryJSON :: (TemplatesMonad m, DBMonad m) => PadQueue -> Document -> Maybe SignatoryLink -> SignatoryLink -> JSONGenT m ()
signatoryJSON pq doc viewer siglink = do
    J.value "id" $ show $ signatorylinkid siglink
    J.value "current" $ isCurrent
    J.value "signorder" $ unSignOrder $ signatorysignorder $ signatorydetails siglink
    J.value "undeliveredEmail" $ invitationdeliverystatus siglink == Undelivered
    J.value "deliveredEmail" $ invitationdeliverystatus siglink == Delivered
    J.value "signs" $ isSignatory siglink
    J.value "author" $ isAuthor siglink
    J.value "saved" $ isJust . maybesignatory $ siglink
    J.value "datamismatch" datamismatch
    J.value "signdate" $ jsonDate $ signtime <$> maybesigninfo siglink
    J.value "seendate" $ jsonDate $ signtime <$> maybeseeninfo siglink
    J.value "readdate" $ jsonDate $ maybereadinvite siglink
    J.value "rejecteddate" $ jsonDate rejectedDate
    J.value "fields" $ signatoryFieldsJSON doc siglink
    J.value "status" $ show $ signatorylinkstatusclass siglink
    J.objects "attachments" $ map signatoryAttachmentJSON (signatoryattachments siglink)
    J.value "csv" $ csvcontents <$> signatorylinkcsvupload siglink
    J.value "inpadqueue"  $ (fmap fst pq == Just (documentid doc)) && (fmap snd pq == Just (signatorylinkid siglink))
    J.value "hasUser" $ isJust (maybesignatory siglink) && isCurrent -- we only inform about current user
    where
      datamismatch = case documentcancelationreason doc of
        Just (ELegDataMismatch _ sid _ _ _) -> sid == signatorylinkid siglink
        _                                   -> False
      isCurrent = (signatorylinkid <$> viewer) == (Just $ signatorylinkid siglink)
      rejectedDate = case documentrejectioninfo doc of
        Just (rt, slid, _) | slid == signatorylinkid siglink -> Just rt
        _                                                    -> Nothing

signatoryAttachmentJSON :: DBMonad m => SignatoryAttachment -> JSONGenT m ()
signatoryAttachmentJSON sa = do
  mfile <- lift $ case (signatoryattachmentfile sa) of
    Just fid -> runDBQuery $ GetFileByFileID fid
    _ -> return Nothing
  J.value "name" $ signatoryattachmentname sa
  J.value "description" $ signatoryattachmentdescription sa
  J.value "file" $ fileJSON <$> mfile

signatoryFieldsJSON :: Document -> SignatoryLink -> JSValue
signatoryFieldsJSON doc sl@(SignatoryLink{signatorydetails = SignatoryDetails{signatoryfields}}) = JSArray $
  for orderedFields $ \sf@SignatoryField{sfType, sfValue, sfPlacements} ->
    case sfType of
      FirstNameFT           -> fieldJSON doc "fstname"   sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      LastNameFT            -> fieldJSON doc "sndname"   sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      EmailFT               -> fieldJSON doc "email"     sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      PersonalNumberFT      -> fieldJSON doc "sigpersnr" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      CompanyFT             -> fieldJSON doc "sigco"     sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      CompanyNumberFT       -> fieldJSON doc "sigcompnr" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      SignatureFT           -> fieldJSON doc "signature" sfValue (closedSignatureF sf  && (not $ isPreparation doc)) sfPlacements
      CustomFT label closed -> fieldJSON doc label       sfValue (closed  && (not $ isPreparation doc))  sfPlacements
  where
    closedF sf = ((not $ null $ sfValue sf) || (null $ sfPlacements sf))
    closedSignatureF sf = ((not $ null $ dropWhile (/= ',') $ sfValue sf) || (null $ sfPlacements sf))
    orderedFields = sortBy (\f1 f2 -> ftOrder (sfType f1) (sfType f2)) signatoryfields
    ftOrder FirstNameFT _ = LT
    ftOrder LastNameFT _ = LT
    ftOrder EmailFT _ = LT
    ftOrder CompanyFT _ = LT
    ftOrder PersonalNumberFT _ = LT
    ftOrder CompanyNumberFT _ = LT
    ftOrder _ _ = EQ

fieldJSON :: Document -> String -> String -> Bool -> [FieldPlacement] -> JSValue
fieldJSON  doc name value closed placements = runJSONGen $ do
    J.value "name" name
    J.value "value" value
    J.value "closed" closed
    J.value "placements" $ map (placementJSON doc) placements

placementJSON :: Document -> FieldPlacement -> JSValue
placementJSON doc placement = runJSONGen $ do
    J.value "x" $ placementx placement
    J.value "y" $ placementy placement
    J.value "page" $ placementpage placement
    J.value "fileid" $ fromMaybe "" $ show <$> (listToMaybe $ documentfiles doc)

jsonDate :: Maybe MinutesTime -> JSValue
jsonDate mdate = toJSValue $ showDateYMD <$> mdate

processJSON :: TemplatesMonad m => Document -> JSONGenT m ()
processJSON doc = do
    J.value "title" =<< text processtitle
    J.value "name" =<< text processname
    J.value "corename" =<< text processcorename
    -- used in the design view
    J.value "basicavailable" $ bool processbasicavailable
    J.value "authorsend" $ bool processauthorsend
    J.value "validationchoiceforbasic" $ bool processvalidationchoiceforbasic
    J.value "expiryforbasic" $ bool processexpiryforbasic
    J.value "step1text" =<< text processstep1text
    J.value "expirywarntext" =<< text processexpirywarntext
    J.value "sendbuttontext" =<< text processsendbuttontext
    J.value "confirmsendtitle" =<< text processconfirmsendtitle
    J.value "confirmsendtext" =<< text processconfirmsendtext
    J.value "expirytext" =<< text processexpirytext
    -- some buttons texts
    J.value "restartbuttontext" =<< text processrestartbuttontext
    J.value "cancelbuttontext" =<< text processcancelbuttontext
    J.value "rejectbuttontext" =<< text processrejectbuttontext
    J.value "cancelmodaltitle" =<< text processcancelmodaltitle
    J.value "cancelmodaltext" =<< text processcancelmodaltext

    J.value "authorissecretarytext" =<< text processauthorissecretarytext
    J.value "remindagainbuttontext" =<< text processremindagainbuttontext
    -- And more
    J.value "requiressignguard" $ bool processrequiressignguard
    J.value "signbuttontext" =<< text processsignbuttontext
    J.value "signatorycancelmodaltitle" =<< text processsignatorycancelmodaltitle
    J.value "signguardwarntext" =<< text processsignguardwarntext
    J.value "signatorysignmodalcontentlast" =<< text processsignatorysignmodalcontentlast
    J.value "signatorysignmodalcontentnotlast" =<< text processsignatorysignmodalcontentnotlast
    J.value "signatorysignmodalcontentauthorlast" =<< text processsignatorysignmodalcontentauthorlast
    J.value "signatorysignmodalcontentdesignvieweleg" =<< text processsignatorysignmodalcontentdesignvieweleg
    J.value "signatorysignmodalcontentsignvieweleg" =<< text processsignatorysignmodalcontentdesignvieweleg

    J.value "signbuttontext" =<< text processsignbuttontext
    J.value "signbuttontextauthor" =<< text processsignbuttontextauthor
    J.value "signatorysignmodaltitle" =<< text processsignatorysignmodaltitle
    J.value "authorsignlastbutton" =<< text processauthorsignlastbuttontext

    J.value "authorname" =<< text processauthorname
    J.value "authorsignatoryname" =<< text processauthorsignatoryname
    J.value "signatoryname" =<< text processsignatoryname
    J.value "nonsignatoryname" =<< text processnonsignatoryname
    J.value "numberedsignatories" $ bool processnumberedsignatories
    where
      text k = lift $ do
                        partylist <- renderListTemplate . map getSmartName $ partyList doc
                        renderTemplateForProcess doc k (do
                                                        field "partylist" partylist
                                                        documentInfoFields doc)
      bool = fromMaybe False . getValueForProcess doc
      

regionJSON :: Document -> JSValue
regionJSON doc = runJSONGen $ do
    J.value "haspeopleids" $ regionhaspeopleids $ getRegionInfo doc
    J.value "iselegavailable" $ regionelegavailable $ getRegionInfo doc
    J.value "gb" $ REGION_GB == getRegion doc
    J.value "se" $ REGION_SE == getRegion doc

fileJSON :: File -> JSValue
fileJSON file = runJSONGen $ do
    J.value "id" $ show $ fileid file
    J.value "name" $ filename file

authorJSON :: Maybe User -> Maybe Company -> JSValue
authorJSON mauthor mcompany = runJSONGen $ do
    J.value "fullname" $ getFullName <$> mauthor
    J.value "email" $ getEmail <$> mauthor
    J.value "company" $ (\a -> getCompanyName (a,mcompany)) <$> mauthor
    J.value "phone" $ userphone <$> userinfo <$> mauthor
    J.value "position" $ usercompanyposition <$> userinfo <$>mauthor



showFileImages :: TemplatesMonad m => DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> FileID -> JpegPages -> m String
showFileImages _ _ _ JpegPagesPending =
  renderTemplateM "showFileImagesPending" ()

showFileImages _ _ _ (JpegPagesError normalizelog) =
  renderTemplateFM "showFileImagesError" $ do
    field "normalizelog" normalizelog

showFileImages docid mtokens fileid (JpegPages jpgpages) =
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

showFilesImages2 :: TemplatesMonad m => DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> [(FileID, JpegPages)] -> m String
showFilesImages2 docid mtokens files = do
  filesPages <- sequence $ map (uncurry (showFileImages docid mtokens)) files
  renderTemplateFM "spanNoEscape" $ field "it" (concat filesPages)


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
      field "documenttitle" documenttitle
      field "editable" $ iseditable
      field "renamelink" $ show $ LinkRenameAttachment documentid
      field "siglinkid" $ fmap (show . signatorylinkid) msiglink
      field "sigmagichash" $ fmap (show . signatorymagichash) msiglink
      field "linkissuedocpdf" $ show (LinkIssueDocPDF msiglink doc)

pageDocumentDesign :: TemplatesMonad m
                   => Document
                   -> m String
pageDocumentDesign document = do
     renderTemplateFM "pageDocumentDesign" $ do
         field "isbasic" $ (documentfunctionality document) ==BasicFunctionality
         field "documentid" $ show $ documentid document

pageDocumentView :: TemplatesMonad m
                    => Document
                    -> Maybe SignatoryLink
                    -> m String
pageDocumentView document msiglink =
  renderTemplateFM "pageDocumentView" $ do
      field "documentid" $ show $ documentid document
      field "siglinkid" $ fmap (show . signatorylinkid) msiglink
      field "sigmagichash" $ fmap (show .  signatorymagichash) msiglink

pageDocumentSignView :: TemplatesMonad m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> m String
pageDocumentSignView ctx document siglink =
  renderTemplateFM "pageDocumentSignView" $ do
      field "documentid" $ show $ documentid document
      field "siglinkid" $ show $ signatorylinkid siglink
      field "sigmagichash" $ show $  signatorymagichash siglink
      field "documenttitle" $ documenttitle document  
      standardPageFields ctx kontrakcja Nothing False False Nothing Nothing

csvLandPage :: TemplatesMonad m => Int -> m String
csvLandPage count = renderTemplateFM "csvlandpage" $ do
  field "doccount" (show count)

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
      fieldFL "signatories" $ for (documentsignatorylinks document) $ \sl -> do
                field "name" $   getSmartName sl
                field "author" $ (isAuthor sl)
      field "notsignedbyme" $ (isJust siglnk) && (isNothing $ maybesigninfo $ fromJust siglnk)
      field "signedbyme" $ (isJust siglnk) && (isJust $ maybesigninfo $ fromJust siglnk)
      field "iamauthor" $ maybe False isAuthor siglnk
      field "isviewonly" $ not $ isAuthor siglnk || maybe False (flip isAuthorAdmin document) (ctxmaybeuser ctx)
    getProcessText = renderTextForProcess document
    getProcessTextWithFields f = renderTemplateForProcess document f mainFields
    processFields = do
      fieldM "pendingauthornotsignedinfoheader" $ getProcessText processpendingauthornotsignedinfoheader
      fieldM "pendingauthornotsignedinfotext" $ getProcessText processpendingauthornotsignedinfotext
      fieldM "pendinginfotext" $ getProcessTextWithFields processpendinginfotext
      fieldM "cancelledinfoheader" $ getProcessText processcancelledinfoheader
      fieldM "cancelledinfotext" $ getProcessTextWithFields processcancelledinfotext
      fieldM "signedinfoheader" $ getProcessText processsignedinfoheader
      fieldM "signedinfotext" $ getProcessTextWithFields processsignedinfotext
      fieldM "statusinfotext" $ getProcessTextWithFields processstatusinfotext

-- | Basic info about document , name, id ,author
documentInfoFields :: MonadIO m => Document -> Fields m
documentInfoFields  document  = do
  field "documenttitle" $ documenttitle document
  field "title" $ documenttitle document
  field "name" $ documenttitle document
  field "id" $ show $ documentid document
  field "documentid" $ show $ documentid document
  field "timetosignset" $  isJust $ documentdaystosign document
  field "template" $  isTemplate document
  field "emailselected" $ document `allowsIdentification` EmailIdentification
  field "elegselected" $ document `allowsIdentification` ELegitimationIdentification
  field "hasanyattachments" $ length (documentauthorattachments document) + length (concatMap signatoryattachments $ documentsignatorylinks document) > 0
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
  field "awaitingauthor" $ canAuthorSignLast document
  field "datamismatch" $ (documentstatus document == Canceled
      && case documentcancelationreason document of
           Just (ELegDataMismatch _ _ _ _ _) -> True
           _ -> False)

uploadPage :: TemplatesMonad m => (Maybe DocumentProcess) -> Bool -> m String
uploadPage mdocprocess showTemplates = renderTemplateFM "uploadPage" $ do
    field "isprocessselected" $ isJust mdocprocess
    field "showTemplates" showTemplates
    fieldFL "processes" $ map processFields [Contract,Offer,Order]
    field "processid" $ show <$> mdocprocess
    field "linkupload" $ show LinkUpload
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
        field "apiid" $ fromSafeEnumInt (Signable process)


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


            
