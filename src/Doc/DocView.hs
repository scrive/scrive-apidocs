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
  , flashMessageRubbishRestoreDone
  , flashMessageRubbishHardDeleteDone
  , flashMessageBulkRemindsSent
  , flashMessageCSVHasTooManyRows
  , flashMessageCSVSent
  , flashMessageCanceled
  , flashMessageCannotCancel
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
  , mailInvitation
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
  , pageAttachmentView
  , pageDocumentDesign
  , pageDocumentForAuthor
  , pageDocumentForSignatory
  , pageDocumentForViewer
  , showFilesImages2
  , signatoryDetailsFromUser
  , documentsToFixView
  , uploadPage
  , docForListJSON
  , documentJSON
  , csvLandPage
  ) where

import ActionSchedulerState (ActionID)
import DB.Types
import Doc.CSVUtils
import Doc.DocProcess
import Doc.DocRegion
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
import Data.List (isInfixOf)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Text.JSON
import Data.List (intercalate)
--import Happstack.State (query)
import File.TransState
import DB.Classes

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

modalSignedClosedHasAccount :: TemplatesMonad m => Locale -> Document -> SignatoryLink -> Bool -> m FlashMessage
modalSignedClosedHasAccount locale document signatorylink isloggedin = do
  toModal <$> (renderTemplateForProcess document processmodalsignedviewclosedhasaccount $ do
    modalSignedFields document
    loginFields locale document signatorylink isloggedin)

modalSignedNotClosedHasAccount :: TemplatesMonad m => Locale -> Document -> SignatoryLink -> Bool -> m FlashMessage
modalSignedNotClosedHasAccount locale document signatorylink isloggedin = do
  toModal <$> (renderTemplateForProcess document processmodalsignedviewnotclosedhasaccount $ do
    modalSignedFields document
    loginFields locale document signatorylink isloggedin)

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
  field "unsignedlistplural" $ length (partyUnsignedList document) /= 1
  field "partylistplural" $ length (partyList document) /= 1

loginFields :: MonadIO m => Locale -> Document -> SignatoryLink -> Bool -> Fields m
loginFields locale document signatorylink isloggedin = do
    field "isloggedin" isloggedin
    field "referer" $ show (LinkSignDoc document signatorylink)
    field "email" $ getEmail signatorylink
    field "linklogin" $ show (LinkLogin locale LoginTry)

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

documentJSON :: (TemplatesMonad m, KontraMonad m, DBMonad m) => Maybe SignatoryLink -> MinutesTime -> Document -> m (JSObject JSValue)
documentJSON msl _crttime doc = do
    ctx <- getContext
    files <- documentfilesM doc
    sealedfiles <- documentsealedfilesM doc
    authorattachmentfiles <- mapM (runDB . dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments doc)
    signatoryattachmentsfiles <- catMaybes <$> (sequence [do
                                             file <- runDB $ dbQuery $ GetFileByFileID fid
                                             case file of
                                               Nothing -> return Nothing
                                               Just f -> return $ Just (fid, f)
                                          | SignatoryAttachment { signatoryattachmentfile = Just fid } <- documentsignatoryattachments doc])

    fmap toJSObject $ propagateMonad  $
     [ ("title",return $ JSString $ toJSString $ BS.toString $ documenttitle doc),
       ("files", return $ JSArray $ jsonPack <$> fileJSON <$> files ),
       ("sealedfiles", return $ JSArray $ jsonPack <$> fileJSON <$> sealedfiles ),
       ("authorattachments", return $ JSArray $ jsonPack <$> fileJSON <$> catMaybes authorattachmentfiles),
       --("signatoryattachments", return $ JSArray $ jsonPack <$> fileJSON <$> catMaybes ),
       ("process", processJSON doc ),
       ("infotext", JSString <$> toJSString <$> documentInfoText ctx doc msl),
       ("canberestarted", return $ JSBool $  isAuthor msl && ((documentstatus doc) `elem` [Canceled, Timedout, Rejected])),
       ("timeouttime", return $ jsonDate $ unTimeoutTime <$> documenttimeouttime doc),
       ("status", return $ JSString $ toJSString $ show $ documentstatus doc),
       ("signatories", JSArray <$>  mapM (signatoryJSON doc msl signatoryattachmentsfiles) (documentsignatorylinks doc)),
       ("signorder", return $ JSRational True (toRational $ unSignOrder $ documentcurrentsignorder doc)),
       ("authorization", return $ authorizationJSON $ head $ (documentallowedidtypes doc) ++ [EmailIdentification])
     ]

authorizationJSON :: IdentificationType -> JSValue
authorizationJSON EmailIdentification = JSString $ toJSString "email"
authorizationJSON ELegitimationIdentification = JSString $ toJSString "eleg"


signatoryJSON :: (TemplatesMonad m) => Document -> Maybe SignatoryLink -> [(FileID, File)] -> SignatoryLink -> m JSValue
signatoryJSON doc viewer files siglink = fmap (JSObject . toJSObject) $ propagateMonad $
    [
        ("id", return $ JSString $ toJSString  $ show $ signatorylinkid siglink)
      , ("current", return $ JSBool $ (signatorylinkid <$> viewer) == (Just $ signatorylinkid siglink))
      , ("signorder",return $ JSRational True (toRational $ unSignOrder $ signatorysignorder $ signatorydetails siglink))
      , ("undeliveredEmail", return $ JSBool $ (invitationdeliverystatus siglink == Undelivered))
      , ("deliveredEmail", return $ JSBool $ (invitationdeliverystatus siglink == Delivered))
      , ("signs", return $ JSBool $ isSignatory siglink)
      , ("author",return $ JSBool $ isAuthor siglink)
      , ("datamismatch", return $ JSBool datamismatch)
      , ("signdate", return $ jsonDate $ signtime <$> maybesigninfo siglink)
      , ("seendate", return $ jsonDate $ signtime <$> maybeseeninfo siglink)
      , ("readdate", return $ jsonDate $ maybereadinvite siglink)
      , ("rejecteddate", return $ jsonDate $ rejectedDate)
      , ("fields", return $ signatoryFieldsJSON doc siglink)
      , ("status", return $ JSString $ toJSString  $ show $ signatoryStatusClass doc siglink)
      , ("attachments", return $ JSArray $ map (signatoryAttachmentJSON files) $
                        filter ((==) (getEmail siglink) . signatoryattachmentemail)  (documentsignatoryattachments doc))
   ]
    where
    datamismatch = case documentcancelationreason doc of
                    Just (ELegDataMismatch _ sid _ _ _) -> sid == signatorylinkid siglink
                    _                                   -> False
    rejectedDate = case documentrejectioninfo doc of
                    Just (rt, slid, _)
                        | slid == signatorylinkid siglink -> Just rt
                    _                             -> Nothing


signatoryAttachmentJSON :: [(FileID, File)] -> SignatoryAttachment -> JSValue
signatoryAttachmentJSON files sa = JSObject $ toJSObject $
  let mfile = maybe Nothing (\fid -> lookup fid files) (signatoryattachmentfile sa)
  in [ ("name", JSString $ toJSString $ BS.toString $ signatoryattachmentname sa)
     , ("description", JSString $ toJSString $ BS.toString $ signatoryattachmentdescription sa)
     , ("file", fromMaybe JSNull $ jsonPack <$> fileJSON <$> mfile)
     ]

signatoryFieldsJSON:: Document -> SignatoryLink -> JSValue
signatoryFieldsJSON doc SignatoryLink{signatorydetails = SignatoryDetails{signatoryfields}} = JSArray $
  for signatoryfields $ \sf@SignatoryField{sfType, sfValue, sfPlacements} ->
    case sfType of
      FirstNameFT -> fieldJSON doc "fstname" sfValue True sfPlacements
      LastNameFT -> fieldJSON doc "sndname" sfValue True sfPlacements
      EmailFT -> fieldJSON doc "email" sfValue True sfPlacements
      PersonalNumberFT -> fieldJSON doc "sigpersnr" sfValue (closedF sf) sfPlacements
      CompanyFT -> fieldJSON doc "sigco" sfValue (closedF sf) sfPlacements
      CompanyNumberFT -> fieldJSON doc "sigco" sfValue (closedF sf) sfPlacements
      CustomFT label closed -> fieldJSON doc (BS.toString label) sfValue closed sfPlacements
  where
    closedF sf = (not $ BS.null $ sfValue sf) || (null $ sfPlacements sf)

fieldJSON :: Document -> String -> BS.ByteString -> Bool -> [FieldPlacement] -> JSValue
fieldJSON  doc name value closed placements = JSObject $ toJSObject $
    [   ("name", JSString $ toJSString name)
      , ("value", jsStringFromBS value)
      , ("closed", JSBool closed)
      , ("placements", JSArray $ map (placementJSON doc) placements)
    ]

placementJSON :: Document -> FieldPlacement -> JSValue
placementJSON doc placement = JSObject $ toJSObject $
    [   ("x", JSRational True (toRational $ placementx  placement))
      , ("y", JSRational True (toRational $ placementy  placement))
      , ("page", JSRational True (toRational $ placementpage  placement))
      , ("fileid", JSString $ toJSString $ fromMaybe "" $ show <$> (listToMaybe $ documentfiles doc))
    ]


jsonDate :: Maybe MinutesTime -> JSValue
jsonDate mdate = fromMaybe JSNull $ JSString <$> toJSString <$> showDateYMD <$> mdate

{-
, signatorysndnameplacements        = []
    , signatorycompanyplacements        = []
    , signatorypersonalnumberplacements = []
    , signatorycompanynumberplacements  = []
    , signatoryemailplacements          = []
    , signatoryotherfields


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
      ]
      -}

processJSON :: (TemplatesMonad m) => Document -> m JSValue
processJSON doc = fmap (JSObject . toJSObject) $ propagateMonad  $
      [
        ("title", text processtitle)
      , ("name", text processname)
        -- used in the design view
      , ("basicavailable", bool processbasicavailable)
      , ("authorsend", bool processauthorsend)
      , ("validationchoiceforbasic", bool processvalidationchoiceforbasic)
      , ("expiryforbasic", bool processexpiryforbasic)
      , ("step1text", text processstep1text )
      , ("expirywarntext ", text processexpirywarntext )
      , ("sendbuttontext", text processsendbuttontext )
      , ("confirmsendtitle", text processconfirmsendtitle )
      , ("confirmsendtext ", text processconfirmsendtext )
      , ("expirytext", text processexpirytext )
      -- some buttons texts
      , ("restartbuttontext", text processrestartbuttontext)
      , ("cancelbuttontext", text processcancelbuttontext)
      , ("rejectbuttontext", text processrejectbuttontext)
      , ("cancelbyauthormodaltitle", text processcancelbyauthormodaltitle)
      , ("authorissecretarytext", text processauthorissecretarytext)
      , ("remindagainbuttontext", text processremindagainbuttontext)
      -- And more
      , ("requiressignguard", bool processrequiressignguard)
      , ("signbuttontext", text processsignbuttontext)
      , ("signatorycancelmodaltitle", text processsignatorycancelmodaltitle)
      , ("signguardwarntext", text processsignguardwarntext)
      , ("signatorysignmodalcontentlast", text processsignatorysignmodalcontentlast)
      , ("signatorysignmodalcontentnotlast", text processsignatorysignmodalcontentnotlast)
      , ("signatorysignmodalcontentauthorlast", text processsignatorysignmodalcontentauthorlast)
      , ("signbuttontext", text processsignbuttontext)
      , ("signbuttontextauthor", text processsignbuttontextauthor)        
      , ("signatorysignmodaltitle", text processsignatorysignmodaltitle)
      , ("authorsignlastbutton", text processauthorsignlastbuttontext)
     ]
    where
        text  k = JSString <$> toJSString <$> renderTextForProcess doc k
        bool k = return $ JSBool <$> fromMaybe False $ getValueForProcess doc k

fileJSON :: File ->  [(String,String)]
fileJSON file =
    [  ("id",   show $ fileid file),
       ("name", BS.toString $ filename file)
    ]

docForListJSON :: (TemplatesMonad m) => KontraTimeLocale -> MinutesTime -> User -> Document -> m (JSObject JSValue)
docForListJSON tl crtime user doc =
  let link = case getSigLinkFor doc user of
        Just sl | not $ isAuthor sl -> LinkSignDoc doc sl
        _                           -> LinkIssueDoc $ documentid doc
      sigFilter sl =   isSignatory sl && (documentstatus doc /= Preparation)
  in fmap toJSObject $ propagateMonad  $
    [ ("fields" , jsonPack <$> docFieldsListForJSON tl crtime doc),
      ("subfields" , JSArray <$>  fmap jsonPack <$> (mapM (signatoryFieldsListForJSON tl crtime doc) (filter sigFilter (documentsignatorylinks doc)))),
      ("link", return $ JSString $ toJSString $  show link)
    ]

jsonPack :: [(String,String)] -> JSValue
jsonPack = JSObject . toJSObject . (mapSnd (JSString . toJSString))

docFieldsListForJSON :: (TemplatesMonad m) => KontraTimeLocale -> MinutesTime -> Document -> m [(String,String)]
docFieldsListForJSON tl crtime doc =  propagateMonad [
    ("id", return $ show $ documentid doc),
    ("title",return $  BS.toString $ documenttitle doc),
    ("status", return $ show $ documentStatusClass doc),
    ("party", return $ intercalate ", " $ map (BS.toString . getSmartName) $ getSignatoryPartnerLinks doc),
    ("partner", return $ intercalate ", " $ map (BS.toString . getSmartName) $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)),
    ("partnercomp", return $ intercalate ", " $ map (BS.toString .  getCompanyName) $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)),
    ("author", return $ intercalate ", " $ map (BS.toString . getSmartName) $ filter (isAuthor) $ (documentsignatorylinks doc)),
    ("time", return $ showDateAbbrev tl crtime (documentmtime doc)),
    ("process", renderTextForProcess doc processname),
    ("type", renderDocType),
    ("anyinvitationundelivered", return $ show $ anyInvitationUndelivered  doc && Pending == documentstatus doc),
    ("shared", return $ show $ (documentsharing doc)==Shared)
    ]
  where
    renderDocType :: (TemplatesMonad m) => m String
    renderDocType = do
      pn <- renderTextForProcess doc processname
      case documenttype doc of
        Attachment -> renderTemplateFM "docListAttachmentLabel" $ do return ()
        AttachmentTemplate -> renderTemplateFM "docListAttachmentLabel" $ do return ()
        Template _ -> renderTemplateFM "docListTemplateLabel" $ do field "processname" pn
        Signable _ -> return pn

signatoryFieldsListForJSON :: (TemplatesMonad m) => KontraTimeLocale -> MinutesTime -> Document ->  SignatoryLink -> m [(String,String)]
signatoryFieldsListForJSON tl crtime doc sl = propagateMonad [
    ("status", return $ show $ signatoryStatusClass doc sl ),
    ("name", return $ BS.toString $ getSmartName sl ),
    ("time", return $ fromMaybe "" $ (showDateAbbrev tl crtime) <$> (sign `mplus` reject `mplus` seen `mplus` open)),
    ("invitationundelivered", return $ show $ isUndelivered sl && Pending == documentstatus doc)
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
  case (map (signatoryStatusClass doc) $ getSignatoryPartnerLinks doc) of
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
    signMatch d = any match $ map (BS.toString . getSmartName) (getSignatoryPartnerLinks d)


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
docSortFunc "authorRev" = viewComparingRev getAuthorName
docSortFunc _ = const $ const EQ

partnerComps :: Document -> BS.ByteString
partnerComps doc = BS.concat $ map getCompanyName $ getSignatoryPartnerLinks doc

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
      let splitUp sl = span (\c -> c/=' ') . map toUpper . BS.toString $ getSmartName sl
          (fst1, snd1) = splitUp sl1
          (fst2, snd2) = splitUp sl2 in
      case (compare fst1 fst2) of
        EQ -> compare snd1 snd2
        x -> x

docsPageSize :: Int
docsPageSize = 100


--
showFileImages :: TemplatesMonad m => DocumentID -> Maybe (SignatoryLinkID, MagicHash) -> FileID -> JpegPages -> m String
showFileImages _ _ _ JpegPagesPending =
  renderTemplateM "showFileImagesPending" ()

showFileImages _ _ _ (JpegPagesError normalizelog) =
  renderTemplateFM "showFileImagesError" $ do
    field "normalizelog" $ BS.toString normalizelog

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


{-

   Document is invalid
   Fel filformat
   Vi beklagar, fel filformat

   mp3 -- we cannot do anything with this document
-}

emptyDetails :: SignatoryDetails
emptyDetails =
  SignatoryDetails {
      signatorysignorder = SignOrder 1
    , signatoryfields    = []
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
                   -> [(FileID, File)]
                   -> m String
pageDocumentDesign ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentdaystosign
    , documentinvitetext
  }
  step
  attachments 
  files =
   let
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       authorotherfields fields = sequence .
         map (\((s, label, _), i) ->
           renderTemplateFM "customfield" $ do
             field "otherFieldValue" $ sfValue s
             field "otherFieldName"  $ label
             field "otherFieldID"    $ "field" ++ show i
             field "otherFieldOwner" "author")
             $ zip fields ([1..]::[Int])
       authorsiglink = fromJust $ getAuthorSigLink document
   in do
     csvstring <- renderTemplateM "csvsendoutsignatoryattachmentstring" ()
     csvfields <- documentCsvFields document
     renderTemplateFM "pageDocumentDesign" $ do
       fieldM "authorOtherFields" $ authorotherfields $ filterCustomField $ signatoryfields $ signatorydetails authorsiglink
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "documentinvitetext" $ documentinvitetext
       fieldM "invitationMailContent" $ mailInvitationContent False ctx Sign document Nothing
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "docstate" (buildDocState (signatorydetails authorsiglink) documentsignatorylinks)
       field "fromservice" (isJust $ ctxservice ctx)
       documentAuthorInfo document
       csvfields
       documentFunctionalityFields document
       documentInfoFields document
       documentViewFields document
       designViewFields step
       documentAttachmentDesignFields documentid (documentauthorattachments document) files
       documentAuthorAttachments attachments
       documentSignatoryAttachments csvstring document (documentsignatoryattachments document)
       fieldF "process" processFields
       documentRegionFields document
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
       fieldM "signbuttontext" $ getProcessText processsignbuttontext
       fieldM "signbuttontextauthor" $ getProcessText processsignbuttontextauthor       
       fieldM "expirywarntext" $ getProcessText processexpirywarntext
       fieldM "confirmsendtitle" $ getProcessText processconfirmsendtitle
       fieldM "confirmsendtext" $ getProcessText processconfirmsendtext
       fieldM "expirytext" $ getProcessText processexpirytext


documentRegionFields :: (Functor m, MonadIO m) => Document -> Fields m
documentRegionFields document = do
  fieldF "region" regionFields
  where
    getRegionValue f = f $ getRegionInfo document
    regionFields = do
      field "haspeopleids" $ getRegionValue regionhaspeopleids
      field "iselegavailable" $ getRegionValue regionelegavailable
      field "gb" $ REGION_GB == getRegion document
      field "se" $ REGION_SE == getRegion document

documentAttachmentDesignFields :: (Functor m, MonadIO m) => DocumentID -> [AuthorAttachment] -> [(FileID, File)] -> Fields m
documentAttachmentDesignFields docid atts files = do
  field "isattachments" $ not $ null atts
  field "attachmentcount" $ length atts
  fieldFL "attachments" $ catMaybes $ map attachmentFields atts
  where
    attachmentFields AuthorAttachment{authorattachmentfile = fileid} = 
      case lookup fileid files of
        Nothing -> Nothing 
        Just file -> Just $ do
          field "attachmentid" $ show fileid
          field "attachmentname" $ filename file
          field "linkattachment" $ show (LinkAttachmentForAuthor docid fileid)

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
pageDocumentForAuthor _ document =
  renderTemplateFM "pageDocumentForAuthor" $ do
      field "documentid" $ show $ documentid document
      field "siglinkid" $ fmap (show . signatorylinkid) $ getAuthorSigLink document
      field "sigmagichash" $ fmap (show .  signatorymagichash) $ getAuthorSigLink document

csvLandPage :: TemplatesMonad m => Int -> m String
csvLandPage count = renderTemplateFM "csvlandpage" $ do
  field "doccount" (show count)

{- |
   Show the document for Viewers (friends of author or signatory).
   Show no buttons or other controls
 -}

pageDocumentForViewer :: TemplatesMonad m => Context -> Document -> Maybe SignatoryLink -> m String
pageDocumentForViewer _ document  msignlink =
  renderTemplateFM "pageDocumentForViewerContent" $  do
       field "documentid" $ show $ documentid document
       field "siglinkid" $ fmap (show . signatorylinkid) msignlink
       field "sigmagichash" $ fmap (show . signatorymagichash) msignlink

pageDocumentForSignatory :: TemplatesMonad m
                         => KontraLink
                         -> Document
                         -> Context
                         -> SignatoryLink
                         -> m String
pageDocumentForSignatory _ document _ invitedlink  = do
  renderTemplateFM "pageDocumentForSignContent" $ do
      field "documentid" $ show $ documentid document
      field "siglinkid" $ (show . signatorylinkid) invitedlink
      field "sigmagichash" $ (show . signatorymagichash) invitedlink

--- Display of signatory
signatoryLinkFields :: TemplatesMonad m => Context -> Document -> Maybe SignatoryLink -> SignatoryLink -> Fields m
signatoryLinkFields
  ctx@Context {ctxmaybeuser = muser}
  document
  currentlink
  siglnk@SignatoryLink {
    signatorylinkid
    , signatorydetails = sigdetails@SignatoryDetails{signatoryfields}
    , invitationdeliverystatus
  } =
  let isCurrentUserAuthor = isAuthor (document, muser)
      current = (currentlink == Just siglnk) || (isNothing currentlink && (fmap getEmail muser) == (Just $ getEmail sigdetails))
      isActiveDoc = not $ (documentstatus document) `elem` [Timedout, Canceled, Rejected]
    in do
      field "id" $ show signatorylinkid
      field "current" $ current
      forM_ signatoryfields $ \SignatoryField{sfType, sfValue} -> case sfType of
        FirstNameFT -> field "fstname" $ packToMString sfValue
        LastNameFT -> field "sndname" $ packToMString sfValue
        CompanyFT -> field "company" $ packToMString sfValue
        PersonalNumberFT -> field "personalnumber" $ packToMString sfValue
        CompanyNumberFT -> field "companynumber" $ packToMString sfValue
        EmailFT -> field "email" $ packToMString sfValue
        CustomFT _ _ -> return ()
      fieldFL "fields" $ for (filterCustomField signatoryfields) $
        \(s, label, _) -> do
          field "fieldlabel" label
          field "fieldvalue" (sfValue s)
      field "signorder" $ unSignOrder $ signatorysignorder sigdetails
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
      (errorStatus documentstatus, SCCancelled)
    , (invitationdeliverystatus==Undelivered,  SCCancelled)
    , (documentstatus==Preparation, SCDraft)
    , (documentstatus==Canceled, SCCancelled)
    , (documentstatus==Rejected, SCCancelled)
    , (documentstatus==Timedout, SCCancelled)
    , (isJust maybesigninfo, SCSigned)
    , (isJust maybeseeninfo, SCOpened)
    , (isJust maybereadinvite, SCRead)
    , (invitationdeliverystatus==Delivered, SCDelivered)
    ] SCSent
  where
      errorStatus (DocumentError _) = True
      errorStatus _ = False
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

buildCustomJS :: SignatoryField -> Int -> JSValue
buildCustomJS SignatoryField{sfType = CustomFT label _, sfValue, sfPlacements} i =
  JSObject $ toJSObject [
      ("label", jsStringFromBS label)
    , ("value", jsStringFromBS $ sfValue)
    , ("id", JSString $ toJSString $ "field" ++ show i)
    , ("placements", JSArray $ map buildPlacementJS sfPlacements)
  ]
buildCustomJS SignatoryField{sfType} _ = error $ "buildCustomJS: field of type " ++ show sfType ++ " passed to function that handles only custom fields"

buildPlacementJS :: FieldPlacement -> JSValue
buildPlacementJS fp = JSObject $ toJSObject [
    ("x", jsString $ placementx fp)
  , ("y", jsString $ placementy fp)
  , ("page", jsString $ placementpage fp)
  , ("h", jsString $ placementpageheight fp)
  , ("w", jsString $ placementpagewidth fp)
  ]
  where
    jsString = JSString . toJSString . show

buildSigLinkJS :: SignatoryLink -> JSValue
buildSigLinkJS SignatoryLink{signatorydetails, signatoryroles} =
  JSObject $ addRole $ buildSigJS signatorydetails
  where
    addRole = toJSObject . (:) ("role", role) . fromJSObject
    role = JSString $ toJSString $ if SignatoryPartner `elem` signatoryroles
              then "signatory"
              else "viewer"

buildSigJS :: SignatoryDetails -> JSObject JSValue
buildSigJS SignatoryDetails{signatorysignorder, signatoryfields} =
  toJSObject $ signorder ++ otherfields ++ fields
  where
    signorder = [("signorder", JSString $ toJSString $ show signatorysignorder)]
    otherfields = [("otherfields", JSArray $ zipWith buildCustomJS (filter isFieldCustom signatoryfields) [1..])]
    fields = concatMap (\sf ->
      case sfType sf of
        FirstNameFT -> sfJS sf "fstname" "fstnameplacements"
        LastNameFT -> sfJS sf "sndname" "sndnameplacements"
        CompanyFT -> sfJS sf "company" "companyplacements"
        PersonalNumberFT -> sfJS sf "personalnumber" "personalnumberplacements"
        CompanyNumberFT -> sfJS sf "companynumber" "companynumberplacements"
        EmailFT -> sfJS sf "email" "emailplacements"
        CustomFT _ _ -> error $ "buildSigJS: impossible happened"
        ) $ filter (not . isFieldCustom) signatoryfields
      where
        sfJS sf name pname = [
            (name, jsStringFromBS $ sfValue sf)
          , (pname, JSArray $ map buildPlacementJS $ sfPlacements sf)
          ]

buildDocState :: SignatoryDetails -> [SignatoryLink] -> String
buildDocState authordetails sigdetails = Text.JSON.encode $ JSObject (
  toJSObject [
      ("signatories", JSArray $ map buildSigLinkJS sigdetails)
    , ("author", JSObject $ buildSigJS authordetails)
    ]
  )

jsStringFromBS :: BS.ByteString -> JSValue
jsStringFromBS = JSString . toJSString . BS.toString

defaultInviteMessage :: BS.ByteString
defaultInviteMessage = BS.empty

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

