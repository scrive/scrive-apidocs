{-# LANGUAGE ExtendedDefaultRules #-}
module Doc.DocView (
    pageCreateFromTemplate 
  , documentAuthorInfo
  , documentInfoFields
  , flashAuthorSigned
  , flashDocumentDraftSaved
  , flashDocumentRestarted
  , flashDocumentTemplateSaved
  , flashMessageCSVSent
  , flashMessageInvalidCSV
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
  , modalSendConfirmationView
  , pageDocumentDesign
  , pageDocumentView
  , pageDocumentSignView
  , documentsToFixView
  , documentJSON
  , gtVerificationPage
  ) where

import AppView (kontrakcja, standardPageFields)
import Company.Model
import Doc.DocProcess
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocViewMail
import FlashMessage
import Kontra
import KontraLink
import MinutesTime
import Utils.Prelude
import Utils.Monoid
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
import qualified Data.Set as Set

pageCreateFromTemplate :: TemplatesMonad m => m String
pageCreateFromTemplate = renderTemplate_ "createFromTemplatePage"

modalMismatch :: TemplatesMonad m => String -> SignatoryLink -> m FlashMessage
modalMismatch msg author = toModal <$>  do
    renderTemplate "signCanceledDataMismatchModal" $ do
                    F.value "authorname"  $ getSmartName author
                    F.value "authoremail" $ getEmail author
                    F.value "message"     $ concatMap (\s -> "<p>" ++ s ++ "</p>") $ lines msg

modalSendConfirmationView :: TemplatesMonad m => Document -> Bool -> m FlashMessage
modalSendConfirmationView document authorWillSign = do
  partylist <- renderListTemplate . map getSmartName $ partyListButAuthor document
  toModal <$> (renderTemplateForProcess document processmodalsendconfirmation $ do
    F.value "signedAndCompleted" $ all (hasSigned) $ filter isSignatory $ documentsignatorylinks document
    F.value "partyListButAuthor" partylist
    F.value "signatory" . listToMaybe $ map getSmartName $ partyList document
    F.value "willBeSigned" (authorWillSign && (not $ hasOtherSignatoriesThenAuthor document))
    F.value "signedByAuthor" $ hasSigned $ getAuthorSigLink document
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

flashAuthorSigned :: TemplatesMonad m => m FlashMessage
flashAuthorSigned =
  toFlashMsg OperationDone <$> renderTemplate_ "flashAuthorSigned"

flashMessageInvalidCSV :: TemplatesMonad m => m FlashMessage
flashMessageInvalidCSV =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageInvalidCSV"

flashMessageCSVSent :: TemplatesMonad m => Int -> m FlashMessage
flashMessageCSVSent doccount =
  toFlashMsg OperationDone <$> (renderTemplate "flashMessageCSVSent" $ F.value "doccount" doccount)

documentJSON :: (TemplatesMonad m, KontraMonad m, MonadDB m) => Bool -> Bool -> PadQueue -> Maybe SignatoryLink -> Document -> m JSValue
documentJSON forapi forauthor pq msl doc = do
    ctx <- getContext
    file <- documentfileM doc
    sealedfile <- documentsealedfileM doc
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments doc)
    let isauthoradmin = maybe False (flip isAuthorAdmin doc) (ctxmaybeuser ctx)
    mauthor <- maybe (return Nothing) (dbQuery . GetUserByID) (getAuthorSigLink doc >>= maybesignatory)
    mcompany <- maybe (return Nothing) (dbQuery . GetCompanyByUserID) (getAuthorSigLink doc >>= maybesignatory)
    let logo  = if (isJust mcompany && isJust (companylogo $ companyui (fromJust mcompany)))
                  then show <$> LinkCompanyLogo <$> companyid <$> mcompany
                  else Nothing
    let bbc  = if (isJust mcompany && isJust (companybarsbackground $ companyui (fromJust mcompany)))
                  then companybarsbackground $ companyui (fromJust mcompany)
                  else Nothing
    let bbtc  = if (isJust mcompany && isJust (companybarstextcolour $ companyui (fromJust mcompany)))
                  then companybarstextcolour $ companyui (fromJust mcompany)
                  else Nothing
    runJSONGenT $ do
      J.value "id" $ show $ documentid doc
      J.value "title" $ documenttitle doc
      J.value "file" $ fmap fileJSON file
      J.value "sealedfile" $ fmap fileJSON sealedfile
      J.value "authorattachments" $ map fileJSON (catMaybes authorattachmentfiles)
      J.value "timeouttime" $ jsonDate $ unTimeoutTime <$> documenttimeouttime doc
      J.value "status" $ show $ documentstatus doc
      J.objects "signatories" $ map (signatoryJSON forapi forauthor pq doc msl) (documentsignatorylinks doc)
      J.value "signorder" $ unSignOrder $ documentcurrentsignorder doc
      J.value "authentication" $ authenticationJSON $ documentauthenticationmethod doc
      J.value "delivery" $ deliveryJSON $ documentdeliverymethod doc
      J.value "template" $ isTemplate doc
      J.value "daystosign" $ documentdaystosign doc
      J.value "invitationmessage" $ documentinvitetext doc
      J.value "lang" $  case (getLang doc) of
                             LANG_EN -> "gb"
                             LANG_SV -> "sv"
      J.objects "tags" $ for (Set.toList $ documenttags doc) $ \(DocumentTag n v) -> do
                                    J.value "name"  n
                                    J.value "value" v
      J.value "apicallbackurl" $ documentapicallbackurl doc
      when (not $ forapi) $ do
        J.value "logo" logo
        J.value "barsbackgroundcolor" bbc
        J.value "barsbackgroundtextcolor" bbtc
        J.value "author" $ authorJSON mauthor mcompany
        J.value "process" $ show $ toDocumentProcess (documenttype doc)
        J.valueM "infotext" $ documentInfoText ctx doc msl
        J.value "canberestarted" $ isAuthor msl && ((documentstatus doc) `elem` [Canceled, Timedout, Rejected])
        J.value "canbecanceled" $ (isAuthor msl || isauthoradmin) && documentstatus doc == Pending
        J.value "canseeallattachments" $ isAuthor msl || isauthoradmin

authenticationJSON :: AuthenticationMethod -> JSValue
authenticationJSON StandardAuthentication = toJSValue "standard"
authenticationJSON ELegAuthentication     = toJSValue "eleg"

deliveryJSON :: DeliveryMethod -> JSValue
deliveryJSON EmailDelivery = toJSValue "email"
deliveryJSON PadDelivery = toJSValue "pad"
deliveryJSON APIDelivery = toJSValue "api"

signatoryJSON :: (TemplatesMonad m, MonadDB m) => Bool -> Bool -> PadQueue -> Document -> Maybe SignatoryLink -> SignatoryLink -> JSONGenT m ()
signatoryJSON forapi forauthor pq doc viewer siglink = do
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
    J.value "signsuccessredirect" $ signatorylinksignredirecturl siglink
    when (not (isPreparation doc) && forauthor && forapi && documentdeliverymethod doc == APIDelivery) $ do
        J.value "signlink" $ show $ LinkSignDoc doc siglink
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
signatoryFieldsJSON doc (SignatoryLink{signatorydetails = SignatoryDetails{signatoryfields}}) = JSArray $
  for orderedFields $ \sf@SignatoryField{sfType, sfValue, sfPlacements} ->
    case sfType of
      FirstNameFT             -> fieldJSON "standard" "fstname"   sfValue ((not $ null $ sfValue)  && (not $ isPreparation doc)) sfPlacements
      LastNameFT              -> fieldJSON "standard" "sndname"   sfValue ((not $ null $ sfValue)  && (not $ isPreparation doc)) sfPlacements
      EmailFT                 -> fieldJSON "standard" "email"     sfValue ((not $ null $ sfValue)  && (not $ isPreparation doc)) sfPlacements
      PersonalNumberFT        -> fieldJSON "standard" "sigpersnr" sfValue ((not $ null $ sfValue)  && (not $ isPreparation doc)) sfPlacements
      CompanyFT               -> fieldJSON "standard" "sigco"     sfValue ((not $ null $ sfValue)  && (null sfPlacements) && (ELegAuthentication /= documentauthenticationmethod doc) && (not $ isPreparation doc)) sfPlacements
      CompanyNumberFT         -> fieldJSON "standard" "sigcompnr" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      SignatureFT             -> fieldJSON "signature" "signature" sfValue (closedSignatureF sf  && (not $ isPreparation doc)) sfPlacements
      CustomFT label closed   -> fieldJSON "custom" label       sfValue (closed  && (not $ isPreparation doc))  sfPlacements
      CheckboxOptionalFT label -> fieldJSON "checkbox-optional" label sfValue False  sfPlacements
      CheckboxObligatoryFT label -> fieldJSON "checkbox-obligatory" label sfValue  False  sfPlacements
  where
    closedF sf = ((not $ null $ sfValue sf) || (null $ sfPlacements sf))
    closedSignatureF sf = ((not $ null $ dropWhile (/= ',') $ sfValue sf) && (null $ sfPlacements sf) && ((PadDelivery /= documentdeliverymethod doc)))
    orderedFields = sortBy (\f1 f2 -> ftOrder (sfType f1) (sfType f2)) signatoryfields
    ftOrder FirstNameFT _ = LT
    ftOrder LastNameFT _ = LT
    ftOrder EmailFT _ = LT
    ftOrder CompanyFT _ = LT
    ftOrder PersonalNumberFT _ = LT
    ftOrder CompanyNumberFT _ = LT
    ftOrder _ _ = EQ

fieldJSON :: String -> String -> String -> Bool -> [FieldPlacement] -> JSValue
fieldJSON  tp name value closed placements = runJSONGen $ do
    J.value "type" tp
    J.value "name" name
    J.value "value" value
    J.value "closed" closed
    J.value "placements" $ map placementJSON placements


placementJSON :: FieldPlacement -> JSValue
placementJSON placement = runJSONGen $ do
    J.value "xrel" $ placementxrel placement
    J.value "yrel" $ placementyrel placement
    J.value "wrel" $ placementwrel placement
    J.value "hrel" $ placementhrel placement
    J.value "fsrel" $ placementfsrel placement
    J.value "page" $ placementpage placement
    J.value "tip" $ case (placementtipside placement) of
                         Just LeftTip -> Just "left"
                         Just RightTip -> Just "right"
                         _ -> Nothing

jsonDate :: Maybe MinutesTime -> JSValue
jsonDate mdate = toJSValue $ formatMinutesTimeRealISO <$> mdate

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
  F.value "template" $  isTemplate document
  F.value "emailauthenticationselected" $ document `allowsAuthMethod` StandardAuthentication
  F.value "elegauthenticationselected" $ document `allowsAuthMethod` ELegAuthentication
  F.value "emaildeliveryselected" $ documentdeliverymethod document == EmailDelivery
  F.value "paddeliveryselected" $ documentdeliverymethod document == PadDelivery
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
  -- awaitingauthor is used in old view in old template
  -- remove it when old view is removed
  -- currently it means: is the next turn for author to sign?
  F.value "awaitingauthor" $ canAuthorSignNow document
  F.value "datamismatch" $ (documentstatus document == Canceled
      && case documentcancelationreason document of
           Just (ELegDataMismatch _ _ _ _ _) -> True
           _ -> False)

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
