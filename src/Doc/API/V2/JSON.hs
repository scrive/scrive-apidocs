{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.JSON (unjsonDocument) where


import Doc.DocStateData
import KontraPrelude
import Utils.Default
import Utils.Read
import File.FileID
import Data.Unjson
import Doc.DocumentID
import Data.Text
import qualified Data.Aeson as Aeson
import Doc.DocUtils
import User.Lang
import DB.TimeZoneName
import Data.Functor.Invariant
import Doc.SignatoryLinkID
import User.UserID
import Control.Applicative.Free
import Doc.SignatoryFieldID

unjsonDocument :: UnjsonDef Document
unjsonDocument = objectOf $
       pure defaultValue
  <*   fieldReadonly "id" (documentid) "Document id "
  <**> (field "title" documenttitle ("Document title")
        <**> (pure $ \t d -> d { documenttitle = t }))
  <**> (fieldBy "signatories" (documentsignatorylinks) ("Document signatories") unjsonSignatoryArray
        <**> (pure (\sl d ->d { documentsignatorylinks = sl }) ))
  <*   fieldReadonlyBy "file" documentfile "Document main file" unjsonMaybeMainFile
  <*   fieldReadonlyBy "sealed_file" documentsealedfile "Document sealed file" unjsonMaybeMainFile
  <**> (field "author_attachments" documentauthorattachments "Author attachments"
        <**> (pure $ \atts d -> d { documentauthorattachments = atts }))
  <*   fieldReadonly "ctime" documentctime "Document creation time"
  <*   fieldReadonly "mtime" documentmtime "Document modification time"
  <*   fieldReadonlyBy "timeout_time" documenttimeouttime "Document timeout time" unjsonDefWithNull
  <*   fieldReadonlyBy "auto_remind_time" documentautoremindtime "Document autoremind time" unjsonDefWithNull
  <*   fieldReadonly "status" documentstatus "Document status"
  <**> (field "days_to_sign" documentdaystosign ("Days to sign document")
        <**> (pure $ \days d -> d { documentdaystosign = days }))
  <**> (fieldOpt "days_to_remind" documentdaystoremind ("Days to remind about signing")
        <**> (pure $ \mdays d -> d { documentdaystoremind = mdays }))
  <**> (fieldBy "display_options" (documentDisplayOptions) ("Document display options") unjsonDocumentDisplayOptions
        <**> (pure (applyDisplayOptionsToDocument)))
  <**> (field "invitation_message" (documentinvitetext) ("Document invitation text")
        <**> (pure $ \t d -> d { documentinvitetext = t }))
  <**> (field "confirmation_message" (documentconfirmtext) ("Document confirmation text")
        <**> (pure $ \t d -> d { documentconfirmtext = t }))
  <**> (field "lang" (documentlang) ("Document language")
        <**> (pure $ \l d -> d { documentlang = l }))
  <**> (fieldOpt "api_callback_url" documentapicallbackurl ("Document language")
        <**> (pure $ \mcu d -> d { documentapicallbackurl = mcu }))
  <*   fieldReadonly "object_version" documentobjectversion "Document object version token"
  <*   fieldReadonly "access_token" (show . documentmagichash)   "Document access token"
  <**> (field "timezone" documenttimezonename ("Document language")
        <**> (pure $ \tz d -> d { documenttimezonename = tz }))
  <**> (field "tags" documenttags ("Document tags")
        <**> (pure $ \tags d -> d { documenttags = tags }))
  <**> (field "is_template" (isTemplate) ("Document title ")
        <**> (pure (\t d -> if t then d { documenttype = Template } else d)))


unjsonSignatoryArray :: UnjsonDef [SignatoryLink]
unjsonSignatoryArray = arrayWithPrimaryKeyOf (signatorylinkid) (objectOf $ fieldDef "id" (unsafeSignatoryLinkID 0) id "Signatory ID") unjsonSignatory

unjsonSignatory :: UnjsonDef SignatoryLink
unjsonSignatory =  objectOf $
       pure defaultValue
  <**> (fieldDef "id" (signatorylinkid defaultValue) signatorylinkid "Signatory id"
        <**> (pure $ \sid s -> s { signatorylinkid = sid })) -- Feels like readonly - but it's not true - since we need to keep matching
  <*   (fieldReadonlyBy "user_id"  maybesignatory "User connected to document " unjsonDefWithNull)
  <**> (fieldDef "is_author" (signatoryisauthor defaultValue) signatoryisauthor "Is this signatory an actor"
        <**> (pure $ \isa s -> s { signatoryisauthor = isa }))
  <**> (fieldDef "is_signatory" (signatoryispartner defaultValue) signatoryispartner "Is this signatory signing this document"
        <**> (pure $ \isa s -> s { signatoryispartner = isa }))
  <**> (fieldDefBy "fields"  (signatoryfields defaultValue) signatoryfields "Signatory field" unjsonSignatoryFields
        <**> (pure $ \fs s -> s { signatoryfields = fs }))
  <**> (fieldDef "sign_order"  (signatorysignorder defaultValue) signatorysignorder "Signatory sign order"
        <**> (pure $ \so s-> s { signatorysignorder = so }))
  <*   (fieldReadonlyBy "sign_time" (fmap signtime . maybesigninfo) "Time when signatory signed document" unjsonDefWithNull)
  <*   (fieldReadonlyBy "seen_time" (fmap signtime . maybeseeninfo) "Time when signatory opened document in browser" unjsonDefWithNull)
  <*   (fieldReadonlyBy "read_invitation_time" maybereadinvite "Time when signatory read invitation" unjsonDefWithNull)
  <*   (fieldReadonlyBy "rejected_time" signatorylinkrejectiontime "Time when signatory rejected document" unjsonDefWithNull)
  <**> (fieldOpt "sign_success_redirect_url" signatorylinksignredirecturl ("Redirect user when he signed")
        <**> (pure $ \mrd s -> s { signatorylinksignredirecturl = mrd }))
  <**> (fieldOpt "reject_redirect_url" signatorylinkrejectredirecturl ("Redirect user when he rejected")
        <**> (pure $ \mrd s -> s { signatorylinkrejectredirecturl = mrd }))
  <*   (fieldReadonly "email_delivery_status" mailinvitationdeliverystatus "Email invitation delivery status")
  <*   (fieldReadonly "mobile_delivery_status" mailinvitationdeliverystatus "SMS invitation delivery status")
  <**> (fieldOpt "csv" signatorylinkcsvupload ("CSV upload for multipart") <**> (pure $ \mcsv s -> s { signatorylinkcsvupload = mcsv })) -- Check only one csv for whole doc
  <**> (fieldDef "delivery_method" (signatorylinkdeliverymethod defaultValue) signatorylinkdeliverymethod "Signatory invitation delivery method"
        <**> (pure $ \sd s -> s { signatorylinkdeliverymethod = sd }))
  <**> (fieldDef "authentication_method" (signatorylinkauthenticationmethod defaultValue) signatorylinkauthenticationmethod "Signatory authentication method"
        <**> (pure $ \sa s -> s { signatorylinkauthenticationmethod = sa }))
  <**> (fieldDef "confirmation_delivery_method" (signatorylinkconfirmationdeliverymethod defaultValue) signatorylinkconfirmationdeliverymethod "Signatory authentication method"
        <**> (pure $ \scd s -> s { signatorylinkconfirmationdeliverymethod = scd }))
  <**> (fieldDef "attachments"  (signatoryattachments defaultValue) signatoryattachments "Signatory attachments"
        <**> (pure $ \sa s -> s { signatoryattachments = sa }))

instance Unjson FieldPlacement where
  unjsonDef = unsonFieldPlacement

unsonFieldPlacement :: UnjsonDef FieldPlacement
unsonFieldPlacement =  objectOf $ pure FieldPlacement
  <*> field "xrel" placementxrel "Relative x position"
  <*> field "yrel" placementyrel "Relative y position"
  <*> field "wrel" placementwrel "Relative width"
  <*> field "hrel" placementhrel "Relative height"
  <*> field "fsrel" placementfsrel "Relative font size"
  <*> field "page" placementpage "Page of palcement"
  <*> fieldOpt "tip" placementtipside "Should arrow point on field from left or right"
  <*> fieldDef "anchors" [] placementanchors "Should arrow point on field from left or right"

instance Unjson PlacementAnchor where
  unjsonDef = unsonPlacementAnchor

unsonPlacementAnchor :: UnjsonDef PlacementAnchor
unsonPlacementAnchor = objectOf $ pure PlacementAnchor
  <*> field "text" placementanchortext "Text to match with anchor"
  <*> field "index" placementanchorindex "Relative x position"
  <*> fieldOpt "pages" placementanchorpages "Relative x position"

unjsonSignatoryFields :: UnjsonDef [SignatoryField]
unjsonSignatoryFields = arrayOf unjsonSignatoryField

fieldTypeToText :: FieldType -> Text
fieldTypeToText NameFT = "name"
fieldTypeToText CompanyFT = "company"
fieldTypeToText PersonalNumberFT = "ssn"
fieldTypeToText CompanyNumberFT = "comanyno"
fieldTypeToText EmailFT = "email"
fieldTypeToText MobileFT = "mobile"
fieldTypeToText TextFT = "text"
fieldTypeToText SignatureFT = "signature"
fieldTypeToText CheckboxFT = "checkbox"

instance Unjson FieldType where
  unjsonDef = unjsonEnum "FieldType (Readonly)" (\_ -> Nothing) fieldTypeToText

unjsonSignatoryField :: UnjsonDef SignatoryField
unjsonSignatoryField = disjointUnionOf "type" [
    (fieldTypeToText NameFT, (\f -> fieldType f == NameFT), (SignatoryNameField <$> unjsonNameField))
  , (fieldTypeToText CompanyFT, (\f -> fieldType f == CompanyFT), (SignatoryCompanyField <$> unjsonCompanyField))
  , (fieldTypeToText PersonalNumberFT, (\f -> fieldType f == PersonalNumberFT), (SignatoryPersonalNumberField <$> unjsonPersonalNumberField))
  , (fieldTypeToText CompanyNumberFT, (\f -> fieldType f == CompanyNumberFT), (SignatoryCompanyNumberField <$> unjsonCompanyNumberField))
  , (fieldTypeToText EmailFT, (\f -> fieldType f == EmailFT), (SignatoryEmailField <$> unjsonEmailField))
  , (fieldTypeToText MobileFT, (\f -> fieldType f == MobileFT), (SignatoryMobileField <$> unjsonMobileField))
  , (fieldTypeToText TextFT, (\f -> fieldType f == TextFT), (SignatoryTextField <$> unjsonTextField))
  , (fieldTypeToText CheckboxFT, (\f -> fieldType f == CheckboxFT), (SignatoryCheckboxField <$> unjsonCheckboxField))
  , (fieldTypeToText SignatureFT, (\f -> fieldType f == SignatureFT), (SignatorySignatureField <$> unjsonSignatureField))
  ]


unjsonNameField :: Ap (FieldDef SignatoryField) SignatoryNameField
unjsonNameField = pure (\no v ob sfbs ps -> NameField (unsafeSignatoryFieldID 0) no v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> field "order" (unsafeFromNameField snfNameOrder) "Order of name field"
  <*> fieldDef "value" "" (unsafeFromNameField snfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromNameField snfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromNameField snfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromNameField snfPlacements) "Placements"
  where
    unsafeFromNameField :: (SignatoryNameField -> a) -> SignatoryField -> a
    unsafeFromNameField f (SignatoryNameField a) = f a
    unsafeFromNameField _ _ = $unexpectedError "unsafeFromNameField"

unjsonCompanyField :: Ap (FieldDef SignatoryField) SignatoryCompanyField
unjsonCompanyField = pure (\v ob sfbs ps -> CompanyField (unsafeSignatoryFieldID 0) v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromCompanyField scfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromCompanyField scfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromCompanyField scfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromCompanyField scfPlacements) "Placements"
  where
    unsafeFromCompanyField :: (SignatoryCompanyField -> a) -> SignatoryField -> a
    unsafeFromCompanyField f (SignatoryCompanyField a) = f a
    unsafeFromCompanyField _ _ = $unexpectedError "unsafeFromCompanyField"

unjsonPersonalNumberField :: Ap (FieldDef SignatoryField) SignatoryPersonalNumberField
unjsonPersonalNumberField = pure (\v ob sfbs ps-> PersonalNumberField (unsafeSignatoryFieldID 0) v  ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromPersonalNumberField spnfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromPersonalNumberField spnfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromPersonalNumberField spnfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromPersonalNumberField spnfPlacements) "Placements"
  where
    unsafeFromPersonalNumberField :: (SignatoryPersonalNumberField -> a) -> SignatoryField -> a
    unsafeFromPersonalNumberField f (SignatoryPersonalNumberField a) = f a
    unsafeFromPersonalNumberField _ _ = $unexpectedError "unsafeFromPersonalNumberField"

unjsonCompanyNumberField :: Ap (FieldDef SignatoryField) SignatoryCompanyNumberField
unjsonCompanyNumberField = pure (\v  ob sfbs ps -> CompanyNumberField (unsafeSignatoryFieldID 0) v  ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromCompanyNumberField scnfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromCompanyNumberField scnfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromCompanyNumberField scnfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromCompanyNumberField scnfPlacements) "Placements"
  where
    unsafeFromCompanyNumberField :: (SignatoryCompanyNumberField -> a) -> SignatoryField -> a
    unsafeFromCompanyNumberField f (SignatoryCompanyNumberField a) = f a
    unsafeFromCompanyNumberField _ _ = $unexpectedError "unsafeFromCompanyNumberField"


unjsonEmailField :: Ap (FieldDef SignatoryField) SignatoryEmailField
unjsonEmailField = pure (\v  ob sfbs ps -> EmailField (unsafeSignatoryFieldID 0) v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromEmailField sefValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromEmailField sefObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromEmailField sefShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromEmailField sefPlacements) "Placements"
  where
    unsafeFromEmailField :: (SignatoryEmailField -> a) -> SignatoryField -> a
    unsafeFromEmailField f (SignatoryEmailField a) = f a
    unsafeFromEmailField _ _ = $unexpectedError "unsafeFromEmailField"

unjsonMobileField :: Ap (FieldDef SignatoryField) SignatoryMobileField
unjsonMobileField = pure (\v ob sfbs ps -> MobileField (unsafeSignatoryFieldID 0) v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromMobileField smfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromMobileField smfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromMobileField smfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromMobileField smfPlacements) "Placements"

  where
    unsafeFromMobileField :: (SignatoryMobileField -> a) -> SignatoryField -> a
    unsafeFromMobileField f (SignatoryMobileField a) = f a
    unsafeFromMobileField _ _ = $unexpectedError "unsafeFromMobileField"


unjsonTextField :: Ap (FieldDef SignatoryField) SignatoryTextField
unjsonTextField  = pure (\n v  ob sfbs ps -> TextField  (unsafeSignatoryFieldID 0) n (v == "") v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> field "name"  (unsafeFromTextField  stfName) "Name of the field"
  <*> fieldDef "value" "" (unsafeFromTextField  stfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromTextField stfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromTextField stfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromTextField stfPlacements) "Placements"
  where
    unsafeFromTextField  :: (SignatoryTextField  -> a) -> SignatoryField -> a
    unsafeFromTextField  f (SignatoryTextField  a) = f a
    unsafeFromTextField  _ _ = $unexpectedError "unsafeFromTextField "

unjsonCheckboxField :: Ap (FieldDef SignatoryField) SignatoryCheckboxField
unjsonCheckboxField  = pure (\n v ob sfbs ps -> CheckboxField  (unsafeSignatoryFieldID 0) n v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> field "name"  (unsafeFromCheckboxField  schfName)  "Name of the field"
  <*> fieldDef "is_checked" False (unsafeFromCheckboxField  schfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromCheckboxField schfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromCheckboxField schfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromCheckboxField schfPlacements) "Placements"
  where
    unsafeFromCheckboxField  :: (SignatoryCheckboxField  -> a) -> SignatoryField -> a
    unsafeFromCheckboxField  f (SignatoryCheckboxField  a) = f a
    unsafeFromCheckboxField  _ _ = $unexpectedError "unsafeFromCheckboxField "

unjsonSignatureField :: Ap (FieldDef SignatoryField) SignatorySignatureField
unjsonSignatureField  = pure (\n ob sfbs ps -> SignatureField  (unsafeSignatoryFieldID 0) n Nothing ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> field "name"  (unsafeFromSignatureField  ssfName)  "Value of the field"
  <*  fieldReadonlyBy "signature" (unsafeFromSignatureField  ssfValue) "Uploaded file" unjsonDefWithNull
  <*> fieldDef "is_obligatory" True (unsafeFromSignatureField ssfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromSignatureField ssfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromSignatureField ssfPlacements) "Placements"
  where
    unsafeFromSignatureField  :: (SignatorySignatureField  -> a) -> SignatoryField -> a
    unsafeFromSignatureField  f (SignatorySignatureField  a) = f a
    unsafeFromSignatureField  _ _ = $unexpectedError "unsafeFromSignatureField "



{-
NameField {
          snfID                     = sfid
        , snfNameOrder              = fromMaybe ($unexpectedError "Name field has NULL as name_order") mname_order
        , snfValue                  = fromMaybe ($unexpectedError "Name field has NULL as value_text") mvalue_text
        , snfObligatory             = obligatory
        , snfShouldBeFilledBySender = should_be_filled_by_sender
        , snfPlacements             = placements
      }


disjointUnionOf :: Text -> [(Text, k -> Bool, Ap (FieldDef k) k)] -> UnjsonDef k
-}

data DocumentDisplayOptions = DocumentDisplayOptions {
    showHeader :: Bool
  , showPdfDownload :: Bool
  , showReject :: Bool
  , showFooter :: Bool
}

documentDisplayOptions :: Document -> DocumentDisplayOptions
documentDisplayOptions doc = DocumentDisplayOptions {
      showHeader = documentshowheader doc
    , showPdfDownload = documentshowpdfdownload doc
    , showReject = documentshowrejectoption doc
    , showFooter = documentshowfooter doc
  }

applyDisplayOptionsToDocument :: DocumentDisplayOptions -> Document -> Document
applyDisplayOptionsToDocument displayOptions doc = doc {
      documentshowheader = showHeader displayOptions
    , documentshowpdfdownload = showPdfDownload displayOptions
    , documentshowrejectoption = showReject displayOptions
    , documentshowfooter = showFooter displayOptions
  }

unjsonDocumentDisplayOptions :: UnjsonDef DocumentDisplayOptions
unjsonDocumentDisplayOptions = objectOf $ pure DocumentDisplayOptions
  <*> field "show_header" showHeader "Show header while signing"
  <*> field "show_pdf_download" showPdfDownload "Show download option while signing"
  <*> field "show_reject_option" showReject "Show signatories option to reject document"
  <*> field "show_footer" showFooter "Show footer while signing"


unjsonEnumBy :: (Eq a) => Text -> [(a,Text)] -> UnjsonDef a
unjsonEnumBy desc enumDef =
  unjsonEnum desc (parseEnum enumDef) (printEnum enumDef)
  where

    parseEnum :: [(a,Text)] -> Text -> Maybe a
    parseEnum ((m,t):ms) v = if (t == v)
                            then Just m
                            else parseEnum ms v
    parseEnum _ _ = Nothing

    printEnum :: (Eq a) => [(a,Text)] -> a -> Text
    printEnum ((m,t):ms) a =  if (a == m)
                            then t
                            else printEnum ms a
    printEnum _ _ = $unexpectedError $ unpack ("Incompleate printEnum definition for " `append` desc)


unjsonEnum :: Text -> (Text -> Maybe a) -> (a -> Text) -> UnjsonDef a
unjsonEnum desc parseEnum printEnum  =
  SimpleUnjsonDef desc parseFromEnumFromAeson printEnumToAeson
  where
    printEnumToAeson a = Aeson.String $ printEnum a
    parseFromEnumFromAeson (Aeson.String s) = case (parseEnum s) of
                                                Just a -> Result a []
                                                _ -> fail $ unpack $ "cannot parse enum " `append` desc `append` " from " `append` s
    parseFromEnumFromAeson _ = fail $ unpack $ "cannot parse enum " `append` desc `append` " from not string"

instance Unjson DocumentStatus where
  unjsonDef = unjsonEnumBy "DocumentStatus" [
      (Preparation, "Preparation")
    , (Pending, "Pending")
    , (Closed, "Closed")
    , (Canceled, "Canceled")
    , (Timedout, "Timedout")
    , (Rejected, "Rejected")
    , (DocumentError, "DocumentError")
    ]

instance Unjson DeliveryStatus where
  unjsonDef = unjsonEnumBy "DeliveryStatus" [
      (Delivered, "delivered")
    , (Undelivered, "not_delivered")
    , (Unknown, "unknown")
    , (Deferred, "deferred")
    ]

instance Unjson AuthenticationMethod where
  unjsonDef = unjsonEnumBy "AuthenticationMethod" [
      (StandardAuthentication, "standard")
    , (ELegAuthentication, "sv_bankid")
    , (SMSPinAuthentication, "sms_pin")
    ]

instance Unjson DeliveryMethod where
  unjsonDef = unjsonEnumBy "DeliveryMethod" [
      (EmailDelivery, "email")
    , (PadDelivery, "pad")
    , (APIDelivery, "unknown")
    , (MobileDelivery, "api")
    , (EmailAndMobileDelivery, "email_mobile")
    ]

instance Unjson ConfirmationDeliveryMethod where
  unjsonDef = unjsonEnumBy "ConfirmationDeliveryMethod" [
      (EmailConfirmationDelivery, "email")
    , (MobileConfirmationDelivery, "mobile")
    , (EmailAndMobileConfirmationDelivery, "email_mobile")
    , (NoConfirmationDelivery, "none")
    ]

instance Unjson Lang where
  unjsonDef = unjsonEnum "Lang" (langFromCode . unpack) (pack . codeFromLang)

unjsonMaybeMainFile :: UnjsonDef (Maybe MainFile)
unjsonMaybeMainFile = nothingToNullDef $ objectOf $ pure Nothing
  <* fieldOpt "id" (fmap mainfileid)   "Id of file"
  <* fieldOpt "name" (fmap mainfilename) "Name of file"


instance Unjson CSVUpload where
  unjsonDef = unjsonCSVUpload

unjsonCSVUpload :: UnjsonDef CSVUpload
unjsonCSVUpload = invmap (CSVUpload "") csvcontents unjsonDef

instance Unjson AuthorAttachment where
  unjsonDef = unjsonAuthorAttachments

unjsonAuthorAttachments :: UnjsonDef AuthorAttachment
unjsonAuthorAttachments = objectOf $ pure AuthorAttachment
    <*> field "id" authorattachmentfileid  "Id of file"
    <*> field "name" authorattachmentfilename "Name of file"

instance Unjson SignatoryAttachment where
  unjsonDef = unjsonSignatoryAttachment

unjsonSignatoryAttachment :: UnjsonDef SignatoryAttachment
unjsonSignatoryAttachment = objectOf $ pure (SignatoryAttachment Nothing)
    <*> field "name"  signatoryattachmentname  "Name of attachment"
    <*> field "description" signatoryattachmentdescription "Description of attachment"
    <* fieldReadonlyBy "file" signatoryattachmentfile "Uploaded file id" unjsonDefWithNull

instance Unjson DocumentTag where
  unjsonDef = unjsonDocumentTag

unjsonDocumentTag :: UnjsonDef DocumentTag
unjsonDocumentTag = objectOf $ pure DocumentTag
    <*> field "name"  tagname  "Name of tag"
    <*> field "value" tagvalue "Value of tag"

unjsonDocumentID :: UnjsonDef DocumentID
unjsonDocumentID = unjsonInvmapR ((maybe (fail "Can't parse DocumentID")  return) . maybeRead) (show . fromDocumentID :: DocumentID -> String) unjsonDef

instance Unjson DocumentID where
  unjsonDef = unjsonDocumentID

unjsonSignatoryLinkID :: UnjsonDef SignatoryLinkID
unjsonSignatoryLinkID = unjsonInvmapR ((maybe (fail "Can't parse DocumentID")  return) . maybeRead) (show . fromSignatoryLinkID :: SignatoryLinkID -> String) unjsonDef

instance Unjson SignatoryLinkID where
  unjsonDef = unjsonSignatoryLinkID

unjsonFileID :: UnjsonDef FileID
unjsonFileID = unjsonInvmapR ((maybe (fail "Can't parse FileID")  return) . maybeRead) (show . fromFileID :: FileID -> String) unjsonDef

instance Unjson FileID where
  unjsonDef = unjsonFileID

unjsonUserID :: UnjsonDef UserID
unjsonUserID = unjsonInvmapR ((maybe (fail "Can't parse FileID")  return) . maybeRead) (show . unUserID :: UserID -> String) unjsonDef

instance Unjson UserID where
  unjsonDef = unjsonUserID

unjsonTimeZoneName :: UnjsonDef TimeZoneName
unjsonTimeZoneName = invmap unsafeTimeZoneName toString unjsonDef

instance Unjson TimeZoneName where
  unjsonDef = unjsonTimeZoneName

instance Unjson SignOrder where
  unjsonDef = unjsonSignOrder

unjsonSignOrder :: UnjsonDef SignOrder
unjsonSignOrder = invmap SignOrder unSignOrder unjsonDef

instance Unjson NameOrder where
  unjsonDef = unjsonNameOrder

unjsonNameOrder :: UnjsonDef NameOrder
unjsonNameOrder = invmap NameOrder (\(NameOrder i) -> i) unjsonDef


instance Unjson TipSide where
  unjsonDef = unjsonEnumBy "TipSide" [
      (LeftTip, "left")
    , (RightTip, "right")
    ]

-- Utils
nothingToNullDef :: UnjsonDef (Maybe a) ->  UnjsonDef (Maybe a)
nothingToNullDef def = SimpleUnjsonDef "ReadOnly"  (parse def) $ \mv ->
  case mv of
    Nothing -> Aeson.Null
    _ -> unjsonToJSON def mv

unjsonDefWithNull :: Unjson a => UnjsonDef (Maybe a)
unjsonDefWithNull = SimpleUnjsonDef "ReadOnly"  (\v -> Just <$> parse unjsonDef v) $ \mv ->
  case mv of
    Nothing -> Aeson.Null
    Just v -> unjsonToJSON unjsonDef v

