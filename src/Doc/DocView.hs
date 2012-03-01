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
  , flashMessageOnlyHaveRightsToViewDoc
  , flashMessagePleaseSign
  , flashMessagePleaseSignWithEleg
  , flashMessageSingleAttachmentShareDone
  , flashMessageSingleTemplateShareDone
  , flashRemindMailSent
  , getDataMismatchMessage
  , isNotLinkForUserID
  , modalMismatch
  , modalPdfTooLarge
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

import Doc.DocProcess
import Doc.DocRegion
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocViewMail
import FlashMessage
import Kontra
import KontraLink
import ListUtil
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
import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Text.JSON
import Data.List (intercalate, sortBy)
import File.Model
import DB.Classes
import Text.JSON.Fields as JSON (json)
import qualified Text.JSON.Fields as JSON (field)



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
  partylist <- renderListTemplate . map (BS.toString . getSmartName) $ partyListButAuthor document
  toModal <$> (renderTemplateForProcess document processmodalsendconfirmation $ do
    field "partyListButAuthor" partylist
    field "signatory" . listToMaybe $ map (BS.toString . getSmartName) $ partyList document
    -- field "signed" $ isJust $ join (maybesigninfo <$> getAuthorSigLink document)
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

modalSignedClosedNoAccount :: TemplatesMonad m => Document -> SignatoryLink -> m FlashMessage
modalSignedClosedNoAccount document signatorylink = do
  toModal <$> (renderTemplateForProcess document processmodalsignedviewclosednoaccount $ do
    modalSignedFields document
    accountFromSignFields document signatorylink)

modalSignedNotClosedNoAccount :: TemplatesMonad m => Document -> SignatoryLink -> m FlashMessage
modalSignedNotClosedNoAccount document signatorylink = do
  toModal <$> (renderTemplateForProcess document processmodalsignedviewnotclosednoaccount $ do
    modalSignedFields document
    accountFromSignFields document signatorylink)

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

accountFromSignFields :: MonadIO m => Document -> SignatoryLink -> Fields m
accountFromSignFields document signatorylink = do
    field "linkaccountfromsign" $ show (LinkAccountFromSign document signatorylink)

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
    files <- runDB $ documentfilesM doc
    sealedfiles <- runDB $ documentsealedfilesM doc
    authorattachmentfiles <- mapM (runDBQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments doc)
    signatoryattachmentsfiles <- catMaybes <$> (sequence [do
                                             file <- runDBQuery $ GetFileByFileID fid
                                             case file of
                                               Nothing -> return Nothing
                                               Just f -> return $ Just (fid, f)
                                          | SignatoryAttachment { signatoryattachmentfile = Just fid } <- documentsignatoryattachments doc])
    let isauthoradmin = maybe False (flip isAuthorAdmin doc) (ctxmaybeuser ctx)
    fmap toJSObject $ propagateMonad  $
     [ ("title",return $ JSString $ toJSString $ BS.toString $ documenttitle doc),
       ("files", return $ JSArray $ jsonPack <$> fileJSON <$> files ),
       ("sealedfiles", return $ JSArray $ jsonPack <$> fileJSON <$> sealedfiles ),
       ("authorattachments", return $ JSArray $ jsonPack <$> fileJSON <$> catMaybes authorattachmentfiles),
       --("signatoryattachments", return $ JSArray $ jsonPack <$> fileJSON <$> catMaybes ),
       ("process", processJSON doc ),
       ("region",  liftIO $ regionJSON doc ),
       ("infotext", JSString <$> toJSString <$> documentInfoText ctx doc msl),
       ("canberestarted", return $ JSBool $  isAuthor msl && ((documentstatus doc) `elem` [Canceled, Timedout, Rejected])),
       ("canbecanceled", return $ JSBool $ (isAuthor msl || isauthoradmin) && documentstatus doc == Pending && isNothing (documenttimeouttime doc)),
       ("timeouttime", return $ jsonDate $ unTimeoutTime <$> documenttimeouttime doc),
       ("status", return $ JSString $ toJSString $ show $ documentstatus doc),
       ("signatories", JSArray <$>  mapM (signatoryJSON doc msl signatoryattachmentsfiles) (documentsignatorylinks doc)),
       ("signorder", return $ JSRational True (toRational $ unSignOrder $ documentcurrentsignorder doc)),
       ("authorization", return $ authorizationJSON $ head $ (documentallowedidtypes doc) ++ [EmailIdentification] ),
       ("template", return $ JSBool $ isTemplate doc),
       ("functionality", return $ JSString $ toJSString $ "basic" <| documentfunctionality doc == BasicFunctionality |> "advanced"),
       ("daystosign", return $ maybe JSNull (JSRational True . toRational) $ documentdaystosign doc),
       ("invitationmessage", return $ if (BS.null $ documentinvitetext doc ) then JSNull else JSString $ toJSString $ BS.toString $ documentinvitetext doc)  
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
      , ("fields", liftIO $ signatoryFieldsJSON doc siglink)
      , ("status", return $ JSString $ toJSString  $ show $ signatoryStatusClass doc siglink)
      , ("attachments", return $ JSArray $ map (signatoryAttachmentJSON files) $
                        filter ((==) (getEmail siglink) . signatoryattachmentemail)  (documentsignatoryattachments doc))
      , ("csv", case (csvcontents <$> signatorylinkcsvupload siglink) of
                     Just a1 ->  return $ JSArray $ for a1 (\a2 -> JSArray $ map (JSString . toJSString . BS.toString) a2 )
                     Nothing -> return $ JSNull) 
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


signatoryFieldsJSON:: Document -> SignatoryLink -> IO JSValue
signatoryFieldsJSON doc sl@(SignatoryLink{signatorydetails = SignatoryDetails{signatoryfields}}) = fmap JSArray $
  forM orderedFields $ \sf@SignatoryField{sfType, sfValue, sfPlacements} ->
    case sfType of
      FirstNameFT -> fieldJSON doc "fstname" sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      LastNameFT -> fieldJSON doc "sndname" sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      EmailFT -> fieldJSON doc "email" sfValue ((not $ isPreparation doc) || isAuthor sl) sfPlacements
      PersonalNumberFT -> fieldJSON doc "sigpersnr" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      CompanyFT -> fieldJSON doc "sigco" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      CompanyNumberFT -> fieldJSON doc "sigcompnr" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      SignatureFT -> fieldJSON doc "signature" sfValue (closedF sf  && (not $ isPreparation doc)) sfPlacements
      CustomFT label closed -> fieldJSON doc (BS.toString label) sfValue (closed  && (not $ isPreparation doc))  sfPlacements
  where
    closedF sf = ((not $ BS.null $ sfValue sf) || (null $ sfPlacements sf))
    orderedFields = sortBy (\f1 f2 -> ftOrder (sfType f1) (sfType f2)) signatoryfields
    ftOrder FirstNameFT _ = LT
    ftOrder LastNameFT _ = LT
    ftOrder EmailFT _ = LT
    ftOrder CompanyFT _ = LT
    ftOrder PersonalNumberFT _ = LT
    ftOrder CompanyNumberFT _ = LT
    ftOrder _ _ = EQ
    
fieldJSON :: Document -> String -> BS.ByteString -> Bool -> [FieldPlacement] -> IO JSValue
fieldJSON  doc name value closed placements = json $ do
    JSON.field "name" name
    JSON.field "value" $ BS.toString value
    JSON.field "closed" closed
    JSON.field "placements"  $ map (placementJSON doc) placements

placementJSON :: Document -> FieldPlacement -> JSValue
placementJSON doc placement = JSObject $ toJSObject $
    [   ("x", JSRational True (toRational $ placementx  placement))
      , ("y", JSRational True (toRational $ placementy  placement))
      , ("page", JSRational True (toRational $ placementpage  placement))
      , ("fileid", JSString $ toJSString $ fromMaybe "" $ show <$> (listToMaybe $ documentfiles doc))
    ]


jsonDate :: Maybe MinutesTime -> JSValue
jsonDate mdate = fromMaybe JSNull $ JSString <$> toJSString <$> showDateYMD <$> mdate


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
      , ("expirywarntext", text processexpirywarntext )
      , ("sendbuttontext", text processsendbuttontext )
      , ("confirmsendtitle", text processconfirmsendtitle )
      , ("confirmsendtext", text processconfirmsendtext )
      , ("expirytext", text processexpirytext )
      -- some buttons texts
      , ("restartbuttontext", text processrestartbuttontext)
      , ("cancelbuttontext", text processcancelbuttontext)
      , ("rejectbuttontext", text processrejectbuttontext)
      , ("cancelmodaltitle", text processcancelmodaltitle)
      , ("cancelmodaltext", text processcancelmodaltext)

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
      
      , ("authorname", text processauthorname)
      , ("authorsignatoryname", text processauthorsignatoryname)
      , ("signatoryname", text processsignatoryname)
      , ("nonsignatoryname", text processnonsignatoryname)
      , ("numberedsignatories", bool processnumberedsignatories)

     ]
    where
        text  k = JSString <$> toJSString <$> renderTextForProcess doc k
        bool k = return $ JSBool <$> fromMaybe False $ getValueForProcess doc k

        
regionJSON  :: Document -> IO JSValue
regionJSON doc = json $ do
      JSON.field "haspeopleids" $ regionhaspeopleids $ getRegionInfo doc 
      JSON.field "iselegavailable" $ regionelegavailable $ getRegionInfo doc 
      JSON.field "gb" $ REGION_GB == getRegion doc
      JSON.field "se" $ REGION_SE == getRegion doc


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
    ("shared", return $ show $ (documentsharing doc)==Shared),
    ("file", return $ fromMaybe "" $ show <$> (listToMaybe $ (documentsealedfiles doc) ++ (documentfiles doc)))
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
    signMatch d = any (match . BS.toString . getSmartName) (documentsignatorylinks d)


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
                   => Document
                   -> m String
pageDocumentDesign document = do
     renderTemplateFM "pageDocumentDesign" $ do
         field "isbasic" $ (documentfunctionality document) ==BasicFunctionality
         field "documentid" $ show $ documentid document


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
   Show the document for Viewers (others in company, author or signatory).
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
        _ -> return ()
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
    , (documentstatus==Preparation, SCDraft)
    , (documentstatus==Canceled, SCCancelled)
    , (documentstatus==Rejected, SCCancelled)
    , (documentstatus==Timedout, SCCancelled)
    , (isJust maybesigninfo, SCSigned)
    , (isJust maybeseeninfo, SCOpened)
    , (isJust maybereadinvite, SCRead)
    , (invitationdeliverystatus==Undelivered,  SCCancelled)
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
