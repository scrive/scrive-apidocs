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
  , mailDocumentAwaitingForAuthor
  , mailDocumentClosed
  , mailDocumentRejected
  , mailDocumentRemind
  , mailInvitation
  , modalLoginForSaveView
  , modalRejectedView
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
  , gtVerificationPage
  ) where

import AppView (kontrakcja, standardPageFields)
import API.Service.Model
import Company.Model
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
import DB
import PadQueue.Model
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import qualified Templates.Fields as F

modalMismatch :: TemplatesMonad m => String -> SignatoryLink -> m FlashMessage
modalMismatch msg author = toModal <$>  do
    renderTemplate "signCanceledDataMismatchModal" $ do
                    F.value "authorname"  $ getSmartName author
                    F.value "authoremail" $ getEmail author
                    F.value "message"     $ concatMap para $ lines msg

modalSignAwaitingAuthorLast :: TemplatesMonad m => m FlashMessage
modalSignAwaitingAuthorLast = toModal <$> renderTemplate_ "signAwaitingAuthorLast"

modalSendConfirmationView :: TemplatesMonad m => Document -> Bool -> m FlashMessage
modalSendConfirmationView document authorWillSign = do
  partylist <- renderListTemplate . map getSmartName $ partyListButAuthor document
  toModal <$> (renderTemplateForProcess document processmodalsendconfirmation $ do
    F.value "partyListButAuthor" partylist
    F.value "signatory" . listToMaybe $ map getSmartName $ partyList document
    F.value "willBeSigned" (authorWillSign && (not $ hasOtherSignatoriesThenAuthor document))
    documentInfoFields document)


modalRejectedView :: TemplatesMonad m => Document -> m FlashMessage
modalRejectedView document = do
  partylist <- renderListTemplate . map getSmartName $ partyList document
  toModal <$> (renderTemplate "modalRejectedView" $ do
    F.value "partyList" partylist
    F.value "documenttitle" $ documenttitle document)

modalLoginForSaveView :: TemplatesMonad m => m FlashMessage
modalLoginForSaveView = toModal <$> renderTemplate_ "modalLoginForSaveView"

flashDocumentDraftSaved :: TemplatesMonad m => m FlashMessage
flashDocumentDraftSaved =
  toFlashMsg SigningRelated <$> renderTemplate_ "flashDocumentDraftSaved"


flashDocumentTemplateSaved :: TemplatesMonad m => m FlashMessage
flashDocumentTemplateSaved =
  toFlashMsg SigningRelated <$> renderTemplate_ "flashDocumentTemplateSaved"

flashDocumentRestarted :: TemplatesMonad m => Document -> m FlashMessage
flashDocumentRestarted document = do
  toFlashMsg OperationDone <$> (renderTemplateForProcess document processflashmessagerestarted $ documentInfoFields document)

flashRemindMailSent :: TemplatesMonad m => SignatoryLink -> m FlashMessage
flashRemindMailSent signlink@SignatoryLink{maybesigninfo} =
  toFlashMsg OperationDone <$> (renderTemplate (template_name maybesigninfo) $ do
    F.value "personname" $ getSmartName signlink)
  where
    template_name =
      maybe "flashRemindMailSentNotSigned"
      (const "flashRemindMailSentSigned")

flashMessageCannotCancel :: TemplatesMonad m => m FlashMessage
flashMessageCannotCancel =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageCannotCancel"

flashMessageCanceled :: TemplatesMonad m => Document -> m FlashMessage
flashMessageCanceled document = do
  toFlashMsg SigningRelated <$> (renderTemplateForProcess document processflashmessagecanceled $ documentInfoFields document)

flashAuthorSigned :: TemplatesMonad m => m FlashMessage
flashAuthorSigned =
  toFlashMsg OperationDone <$> renderTemplate_ "flashAuthorSigned"

flashMessageBulkRemindsSent :: TemplatesMonad m => m FlashMessage
flashMessageBulkRemindsSent = do
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageBulkDocumentRemindsSent"

flashMessageNoBulkRemindsSent :: TemplatesMonad m => m FlashMessage
flashMessageNoBulkRemindsSent = do
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageNoBulkDocumentRemindsSent"

flashMessageRubbishRestoreDone :: TemplatesMonad m => m FlashMessage
flashMessageRubbishRestoreDone =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageRubbishRestoreDone"

flashMessageRubbishHardDeleteDone :: TemplatesMonad m => m FlashMessage
flashMessageRubbishHardDeleteDone =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageRubbishHardDeleteDone"

flashMessageInvalidCSV :: TemplatesMonad m => m FlashMessage
flashMessageInvalidCSV =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageInvalidCSV"

flashMessageCSVSent :: TemplatesMonad m => Int -> m FlashMessage
flashMessageCSVSent doccount =
  toFlashMsg OperationDone <$> (renderTemplate "flashMessageCSVSent" $ F.value "doccount" doccount)

flashMessageSingleTemplateShareDone :: TemplatesMonad m => String -> m FlashMessage
flashMessageSingleTemplateShareDone docname =
  toFlashMsg OperationDone <$> (renderTemplate "flashMessageSingleTemplateShareDone" $ F.value "docname" docname)

flashMessageMultipleTemplateShareDone :: TemplatesMonad m => m FlashMessage
flashMessageMultipleTemplateShareDone =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageMultipleTemplateShareDone"

flashMessageSingleAttachmentShareDone :: TemplatesMonad m => String -> m FlashMessage
flashMessageSingleAttachmentShareDone docname =
  toFlashMsg OperationDone <$> (renderTemplate "flashMessageSingleAttachmentShareDone" $ F.value "docname" docname)

flashMessageMultipleAttachmentShareDone :: TemplatesMonad m => m FlashMessage
flashMessageMultipleAttachmentShareDone =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageMultipleAttachmentShareDone"

documentJSON :: (TemplatesMonad m, KontraMonad m, MonadDB m) => PadQueue -> Maybe SignatoryLink -> MinutesTime -> Document -> m JSValue
documentJSON pq msl _crttime doc = do
    ctx <- getContext
    files <- documentfilesM doc
    sealedfiles <- documentsealedfilesM doc
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments doc)
    let isauthoradmin = maybe False (flip isAuthorAdmin doc) (ctxmaybeuser ctx)
    mauthor <- maybe (return Nothing) (dbQuery . GetUserByID) (getAuthorSigLink doc >>= maybesignatory)
    mservice <- maybe (return Nothing) (dbQuery . GetService) (documentservice doc)
    mcompany <- maybe (return Nothing) (dbQuery . GetCompany) (getAuthorSigLink doc >>= maybecompany)
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
      J.valueM "infotext" $ documentInfoText ctx doc msl
      J.value "canberestarted" $ isAuthor msl && ((documentstatus doc) `elem` [Canceled, Timedout, Rejected])
      J.value "canbecanceled" $ (isAuthor msl || isauthoradmin) && documentstatus doc == Pending && not (canAuthorSignLast doc)
      J.value "canseeallattachments" $ isAuthor msl || isauthoradmin
      J.value "timeouttime" $ jsonDate $ unTimeoutTime <$> documenttimeouttime doc
      J.value "status" $ show $ documentstatus doc
      J.objects "signatories" $ map (signatoryJSON pq doc msl) (documentsignatorylinks doc)
      J.value "signorder" $ unSignOrder $ documentcurrentsignorder doc
      J.value "authorization" $ authorizationJSON $ head $ (documentallowedidtypes doc) ++ [EmailIdentification]
      J.value "template" $ isTemplate doc
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

signatoryJSON :: (TemplatesMonad m, MonadDB m) => PadQueue -> Document -> Maybe SignatoryLink -> SignatoryLink -> JSONGenT m ()
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

signatoryAttachmentJSON :: MonadDB m => SignatoryAttachment -> JSONGenT m ()
signatoryAttachmentJSON sa = do
  mfile <- lift $ case (signatoryattachmentfile sa) of
    Just fid -> dbQuery $ GetFileByFileID fid
    _ -> return Nothing
  J.value "name" $ signatoryattachmentname sa
  J.value "description" $ signatoryattachmentdescription sa
  J.value "file" $ fileJSON <$> mfile

signatoryFieldsJSON :: Document -> SignatoryLink -> JSValue
signatoryFieldsJSON doc sl@(SignatoryLink{signatorydetails = SignatoryDetails{signatoryfields}}) = JSArray $
  for orderedFields $ \sf@SignatoryField{sfType, sfValue, sfPlacements} ->
    case sfType of
      FirstNameFT             -> fieldJSON doc "standard" "fstname"   sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      LastNameFT              -> fieldJSON doc "standard" "sndname"   sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      EmailFT                 -> fieldJSON doc "standard" "email"     sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      PersonalNumberFT        -> fieldJSON doc "standard" "sigpersnr" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      CompanyFT               -> fieldJSON doc "standard" "sigco"     sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      CompanyNumberFT         -> fieldJSON doc "standard" "sigcompnr" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      SignatureFT             -> fieldJSON doc "signature" "signature" sfValue (closedSignatureF sf  && (not $ isPreparation doc)) sfPlacements
      CustomFT label closed   -> fieldJSON doc "custom" label       sfValue (closed  && (not $ isPreparation doc))  sfPlacements
      CheckboxOptionalFT label -> fieldJSON doc "checkbox-optional" label sfValue False  sfPlacements
      CheckboxObligatoryFT label -> fieldJSON doc "checkbox-obligatory" label sfValue  False  sfPlacements
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

fieldJSON :: Document -> String -> String -> String -> Bool -> [FieldPlacement] -> JSValue
fieldJSON  doc tp name value closed placements = runJSONGen $ do
    J.value "type" tp
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
    J.value "tip" $ case (placementtipside placement) of
                         Just LeftTip -> Just "left"
                         Just RightTip -> Just "right"
                         _ -> Nothing

jsonDate :: Maybe MinutesTime -> JSValue
jsonDate mdate = toJSValue $ showDateYMD <$> mdate

processJSON :: TemplatesMonad m => Document -> JSONGenT m ()
processJSON doc = do
    J.valueM "title" $ text processtitle
    J.valueM "name" $ text processname
    J.valueM "corename" $ text processcorename
    -- used in the design view
    J.value "authorsend" $ bool processauthorsend
    J.valueM "step1text" $ text processstep1text
    J.valueM "sendbuttontext" $ text processsendbuttontext
    J.valueM "confirmsendtitle" $ text processconfirmsendtitle
    J.valueM "confirmsendtext" $ text processconfirmsendtext
    J.valueM "expirytext" $ text processexpirytext
    -- some buttons texts
    J.valueM "restartbuttontext" $ text processrestartbuttontext
    J.valueM "cancelbuttontext" $ text processcancelbuttontext
    J.valueM "rejectbuttontext" $ text processrejectbuttontext
    J.valueM "cancelmodaltitle" $ text processcancelmodaltitle
    J.valueM "cancelmodaltext" $ text processcancelmodaltext

    J.valueM "authorissecretarytext" $ text processauthorissecretarytext
    J.valueM "remindagainbuttontext" $ text processremindagainbuttontext
    -- And more
    J.value "requiressignguard" $ bool processrequiressignguard
    J.valueM "signbuttontext" $ text processsignbuttontext
    J.valueM "signatorycancelmodaltitle" $ text processsignatorycancelmodaltitle
    J.valueM "signguardwarntext" $ text processsignguardwarntext
    J.valueM "signatorysignmodalcontentlast" $ text processsignatorysignmodalcontentlast
    J.valueM "signatorysignmodalcontentnotlast" $ text processsignatorysignmodalcontentnotlast
    J.valueM "signatorysignmodalcontentauthorlast" $ text processsignatorysignmodalcontentauthorlast
    J.valueM "signatorysignmodalcontentdesignvieweleg" $ text processsignatorysignmodalcontentdesignvieweleg
    J.valueM "signatorysignmodalcontentsignvieweleg" $ text processsignatorysignmodalcontentdesignvieweleg
    J.valueM "signatorysignmodalcontentauthoronly" $ text processsignatorysignmodalcontentauthoronly


    J.valueM "signbuttontext" $ text processsignbuttontext
    J.valueM "signbuttontextauthor" $ text processsignbuttontextauthor
    J.valueM "signatorysignmodaltitle" $ text processsignatorysignmodaltitle
    J.valueM "authorsignlastbutton" $ text processauthorsignlastbuttontext

    J.valueM "authorname" $ text processauthorname
    J.valueM "authorsignatoryname" $ text processauthorsignatoryname
    J.valueM "signatoryname" $ text processsignatoryname
    J.valueM "nonsignatoryname" $ text processnonsignatoryname
    J.value "numberedsignatories" $ bool processnumberedsignatories
    where
      text k = do
        partylist <- renderListTemplate . map getSmartName $ partyList doc
        renderTemplateForProcess doc k $ do
          F.value "partylist" partylist
          documentInfoFields doc
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
showFileImages _ _ _ JpegPagesPending = renderTemplate_ "showFileImagesPending"

showFileImages _ _ _ (JpegPagesError normalizelog) =
  renderTemplate "showFileImagesError" $ do
    F.value "normalizelog" normalizelog

showFileImages docid mtokens fileid (JpegPages jpgpages) =
  renderTemplate "showFileImagesReady" $ do
    F.value "pageurl" $ "/pages/" ++ pageurl mtokens
    F.objects "images" . map page $ zip ([1..]::[Int]) jpgpages
  where
    pageurl Nothing =  show docid ++ "/" ++ show fileid
    pageurl (Just (siglinkid, sigmagichash)) =
           show docid ++ "/" ++ show siglinkid ++ "/"
        ++ show sigmagichash ++ "/" ++ show fileid
    page (x,(_,w,h)) = do
      F.value "number" x
      F.value "width" w
      F.value "height" h

showFilesImages2 :: TemplatesMonad m => DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> [(FileID, JpegPages)] -> m String
showFilesImages2 docid mtokens files = do
  filesPages <- sequence $ map (uncurry (showFileImages docid mtokens)) files
  renderTemplate "spanNoEscape" $ F.value "it" (concat filesPages)


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
    renderTemplate "pageAttachment" $ do
      F.value "documentid" $ show documentid
      F.value "documenttitle" documenttitle
      F.value "editable" $ iseditable
      F.value "renamelink" $ show $ LinkRenameAttachment documentid
      F.value "siglinkid" $ fmap (show . signatorylinkid) msiglink
      F.value "sigmagichash" $ fmap (show . signatorymagichash) msiglink
      F.value "linkissuedocpdf" $ show (LinkIssueDocPDF msiglink doc)

pageDocumentDesign :: TemplatesMonad m
                   => Document
                   -> m String
pageDocumentDesign document = do
     renderTemplate "pageDocumentDesign" $ do
         F.value "documentid" $ show $ documentid document

pageDocumentView :: TemplatesMonad m
                    => Document
                    -> Maybe SignatoryLink
                    -> Bool
                    -> m String
pageDocumentView document msiglink authorcompanyadmin =
  renderTemplate "pageDocumentView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ fmap (show . signatorylinkid) msiglink
      F.value "sigmagichash" $ fmap (show .  signatorymagichash) msiglink
      F.value "authorcompanyadmin" $ authorcompanyadmin
      

pageDocumentSignView :: TemplatesMonad m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> m String
pageDocumentSignView ctx document siglink =
  renderTemplate "pageDocumentSignView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "sigmagichash" $ show $  signatorymagichash siglink
      F.value "documenttitle" $ documenttitle document
      standardPageFields ctx kontrakcja Nothing False False Nothing Nothing

csvLandPage :: TemplatesMonad m => Int -> m String
csvLandPage count = renderTemplate "csvlandpage" $ do
  F.value "doccount" (show count)

-- Helper to get document after signing info text
documentInfoText :: TemplatesMonad m => Context -> Document -> Maybe SignatoryLink -> m String
documentInfoText ctx document siglnk =
  renderTemplate "documentInfoText" $ do
    mainFields
    F.object "process" processFields
  where
    mainFields = do
      documentInfoFields document
      documentAuthorInfo document
      F.objects "signatories" $ for (documentsignatorylinks document) $ \sl -> do
                F.value "name" $   getSmartName sl
                F.value "author" $ (isAuthor sl)
      F.value "notsignedbyme" $ (isJust siglnk) && (isNothing $ maybesigninfo $ fromJust siglnk)
      F.value "signedbyme" $ (isJust siglnk) && (isJust $ maybesigninfo $ fromJust siglnk)
      F.value "iamauthor" $ maybe False isAuthor siglnk
      F.value "isviewonly" $ not $ isAuthor siglnk || maybe False (flip isAuthorAdmin document) (ctxmaybeuser ctx)
    getProcessText = renderTextForProcess document
    getProcessTextWithFields f = renderTemplateForProcess document f mainFields
    processFields = do
      F.valueM "pendingauthornotsignedinfoheader" $ getProcessText processpendingauthornotsignedinfoheader
      F.valueM "pendingauthornotsignedinfotext" $ getProcessText processpendingauthornotsignedinfotext
      F.valueM "pendinginfotext" $ getProcessTextWithFields processpendinginfotext
      F.valueM "cancelledinfoheader" $ getProcessText processcancelledinfoheader
      F.valueM "cancelledinfotext" $ getProcessTextWithFields processcancelledinfotext
      F.valueM "signedinfoheader" $ getProcessText processsignedinfoheader
      F.valueM "signedinfotext" $ getProcessTextWithFields processsignedinfotext
      F.valueM "statusinfotext" $ getProcessTextWithFields processstatusinfotext

-- | Basic info about document , name, id ,author
documentInfoFields :: Monad m => Document -> Fields m ()
documentInfoFields  document  = do
  F.value "documenttitle" $ documenttitle document
  F.value "title" $ documenttitle document
  F.value "name" $ documenttitle document
  F.value "id" $ show $ documentid document
  F.value "documentid" $ show $ documentid document
  F.value "timetosignset" $  isJust $ documentdaystosign document
  F.value "template" $  isTemplate document
  F.value "emailselected" $ document `allowsIdentification` EmailIdentification
  F.value "elegselected" $ document `allowsIdentification` ELegitimationIdentification
  F.value "hasanyattachments" $ length (documentauthorattachments document) + length (concatMap signatoryattachments $ documentsignatorylinks document) > 0
  documentStatusFields document

documentAuthorInfo :: Monad m => Document -> Fields m ()
documentAuthorInfo document =
  case getAuthorSigLink document of
    Nothing -> return ()
    Just siglink -> do
      F.value "authorfstname"       $ nothingIfEmpty $ getFirstName      siglink
      F.value "authorsndname"       $ nothingIfEmpty $ getLastName       siglink
      F.value "authorcompany"       $ nothingIfEmpty $ getCompanyName    siglink
      F.value "authoremail"         $ nothingIfEmpty $ getEmail          siglink
      F.value "authorpersonnumber"  $ nothingIfEmpty $ getPersonalNumber siglink
      F.value "authorcompanynumber" $ nothingIfEmpty $ getCompanyNumber  siglink

-- | Fields indication what is a document status
documentStatusFields :: Monad m => Document -> Fields m ()
documentStatusFields document = do
  F.value "preparation" $ documentstatus document == Preparation
  F.value "pending" $ documentstatus document == Pending
  F.value "cancel" $ (documentstatus document == Canceled
      && documentcancelationreason document == Just ManualCancel)
  F.value "timedout" $ documentstatus document == Timedout
  F.value "rejected" $ documentstatus document == Rejected
  F.value "signed" $ documentstatus document == Closed
  F.value "awaitingauthor" $ canAuthorSignLast document
  F.value "datamismatch" $ (documentstatus document == Canceled
      && case documentcancelationreason document of
           Just (ELegDataMismatch _ _ _ _ _) -> True
           _ -> False)

uploadPage :: TemplatesMonad m => m String
uploadPage = renderTemplate_ "uploadPage"


getDataMismatchMessage :: Maybe CancelationReason -> Maybe String
getDataMismatchMessage (Just (ELegDataMismatch msg _ _ _ _)) = Just msg
getDataMismatchMessage _ = Nothing

-- This is temporary method used to see list of broken documents
documentsToFixView :: TemplatesMonad m => [Document] -> m String
documentsToFixView docs = do
    renderTemplate "documentsToFixView" $ do
        F.objects "documents" $ for docs $ \doc -> do
            F.value "title" $ documenttitle doc
            F.value "id" $ show $ documentid doc
            F.value "involved" $ map getEmail  $ documentsignatorylinks doc
            F.value "cdate" $  show $ documentctime doc

-- Page for GT verification
gtVerificationPage :: TemplatesMonad m => m String
gtVerificationPage = renderTemplate_ "gtVerificationPage" 
    
