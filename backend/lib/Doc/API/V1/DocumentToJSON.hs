{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V1.DocumentToJSON (
      evidenceAttachmentsJSONV1
    , documentJSONV1
    , allCustomTextOrCheckboxOrRadioGroupFields
    , docForListJSONV1
    , docForListCSVV1
    , docForListCSVHeaderV1
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Log
import Optics (to)
import Text.JSON
import Text.JSON.Gen hiding (value)
import Text.StringTemplates.Templates
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.JSON.Gen as J

import DB
import DB.TimeZoneName
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import Doc.Types.SignatoryAccessToken
import File.File
import File.Model
import File.Storage
import KontraLink
import MinutesTime
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Image
import Utils.String
import qualified Doc.EvidenceAttachments as EvidenceAttachments

evidenceAttachmentsJSONV1
  :: ( MonadDB m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , MonadFileStorage m
     )
  => Document
  -> m JSValue
evidenceAttachmentsJSONV1 doc = do
  evidenceattachments <- EvidenceAttachments.extractAttachmentsList doc
  let evidenceattachments' = sortBy eaSorter evidenceattachments
  runJSONGenT $ do
    J.objects "evidenceattachments" $ for evidenceattachments' $ \name -> do
      J.value "name" $ (T.unpack name)
      J.value "mimetype" $ (Nothing :: Maybe String)
      J.value "downloadLink" $ show $ LinkEvidenceAttachment (documentid doc) name
  where
    eaSorter :: Text -> Text -> Ordering
    eaSorter a b | a == firstAttachmentName = LT
                 | b == firstAttachmentName = GT
                 | otherwise                = compare a b
    firstAttachmentName = "Evidence Quality of Scrive E-signed Documents.html"

documentJSONV1
  :: ( MonadDB m
     , MonadIO m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , MonadFileStorage m
     )
  => (Maybe User)
  -> Bool
  -> Bool
  -> Maybe SignatoryLink
  -> Document
  -> m JSValue
documentJSONV1 muser forapi forauthor msl doc = do
  runJSONGenT $ do
    J.value "id" $ showt $ documentid doc
    J.value "title" $ documenttitle doc
    J.value "file" $ fmap mainfileJSON (documentfile doc)
    J.value "sealedfile" $ fmap mainfileJSON (documentsealedfile doc)
    J.value "authorattachments" $ map authorAttachmentJSON (documentauthorattachments doc)
    J.objects "evidenceattachments" $ ([] :: [JSONGenT m ()])
    J.value "time" $ jsonDate (Just $ documentmtime doc)
    J.value "ctime" $ jsonDate (Just $ documentctime doc)
    J.value "timeouttime" $ jsonDate $ documenttimeouttime doc
    J.value "autoremindtime" $ jsonDate $ documentautoremindtime doc
    J.value "status" $ show $ documentstatus doc
    J.value "state" $ show $ documentstatus doc
    J.objects "signatories"
      $ map (signatoryJSON forauthor doc msl) (documentsignatorylinks doc)
    J.value "signorder" $ unSignOrder $ documentcurrentsignorder doc
    J.value "authentication"
      $ case
          nub (map signatorylinkauthenticationtosignmethod (documentsignatorylinks doc))
        of
          [StandardAuthenticationToSign] -> ("standard" :: String)
          [SEBankIDAuthenticationToSign] -> "eleg"
          [SMSPinAuthenticationToSign] -> "sms_pin"
          [NOBankIDAuthenticationToSign] -> "no_bankid"
          [DKNemIDAuthenticationToSign] -> "dk_nemid"
          _ -> "mixed"
    J.value "delivery"
      $ case nub (map signatorylinkdeliverymethod (documentsignatorylinks doc)) of
          [EmailDelivery] -> ("email" :: String)
          [PadDelivery] -> "pad"
          [APIDelivery] -> "api"
          [MobileDelivery] -> "mobile"
          [EmailAndMobileDelivery] -> "email_mobile"
          [PortalDelivery] -> "portal"
          _ -> "mixed"
    J.value "template" $ isTemplate doc
    J.value "daystosign" $ documentdaystosign doc
    J.value "daystoremind" $ documentdaystoremind doc
    J.value "showheader" $ documentshowheader doc
    J.value "showpdfdownload" $ documentshowpdfdownload doc
    J.value "showrejectoption" $ documentshowrejectoption doc
    J.value "allowrejectreason" $ documentallowrejectreason doc
    J.value "showfooter" $ documentshowfooter doc
    J.value "isreceipt" $ documentisreceipt doc
    J.value "invitationmessage" $ T.unpack $ if (T.null $ documentinvitetext doc) --V1 requires HTML for custom message unless message is empty
      then ""
      else "<p>" <> escapeHTML (documentinvitetext doc) <> "</p>"
    J.value "confirmationmessage" $ T.unpack $ if (T.null $ documentconfirmtext doc) --V1 requires HTML for custom message unless message is empty
      then ""
      else "<p>" <> escapeHTML (documentconfirmtext doc) <> "</p>"
    J.value "lang" $ case (getLang doc) of -- We keep some old lang codes for old integrations. We should drop it on new API release
      LANG_EN -> "gb"
      LANG_SV -> "sv"
      l       -> codeFromLang l
    J.objects "tags" $ for (Set.toList $ documenttags doc) $ \(DocumentTag n v) -> do
      J.value "name" n
      J.value "value" v
    J.value "apicallbackurl" $ documentapiv1callbackurl doc
    J.value "saved" $ not (documentunsaveddraft doc)
    J.value "deleted"
      $ fromMaybe False (muser ^? _Just % #id % to (documentDeletedForUser doc))
    J.value "reallydeleted"
      $   fromMaybe False
      $   documentReallyDeletedForUser doc
      <$> (muser ^? _Just % #id)
    forM_ muser $ \user ->
      J.value "canperformsigning" $ userCanPerformSigningAction (user ^. #id) doc
    J.value "objectversion" $ documentobjectversion doc
    J.value "process" ("Contract" :: Text)
    J.value "isviewedbyauthor" $ isSigLinkFor muser (getAuthorSigLink doc)
    unless forapi $ do
      J.value "canberestarted"
        $  isAuthor msl
        && ((documentstatus doc) `elem` [Canceled, Timedout, Rejected])
      J.value "canbeprolonged" $ isAuthor msl && ((documentstatus doc) `elem` [Timedout])
      J.value "canbecanceled"
        $  (isAuthor msl || fromMaybe False (view #isCompanyAdmin <$> muser))
        && (documentstatus doc == Pending)
      J.value "canseeallattachments" $ isAuthor msl || fromMaybe
        False
        (view #isCompanyAdmin <$> muser)
    J.value "accesstoken"
      $ if (isNothing msl || isAuthor msl) then showt (documentmagichash doc) else ""
    J.value "timezone" $ toString $ documenttimezonename doc

authenticationToViewJSON :: AuthenticationToViewMethod -> JSValue
authenticationToViewJSON StandardAuthenticationToView = toJSValue ("standard" :: String)
authenticationToViewJSON SEBankIDAuthenticationToView = toJSValue ("se_bankid" :: String)
authenticationToViewJSON NOBankIDAuthenticationToView = toJSValue ("no_bankid" :: String)
authenticationToViewJSON DKNemIDAuthenticationToView  = toJSValue ("dk_nemid" :: String)
authenticationToViewJSON SMSPinAuthenticationToView   = toJSValue ("sms_pin" :: String)
authenticationToViewJSON FITupasAuthenticationToView  = toJSValue ("fi_tupas" :: String)
authenticationToViewJSON VerimiAuthenticationToView   = toJSValue ("verimi" :: String)
authenticationToViewJSON IDINAuthenticationToView     = toJSValue ("nl_idin" :: String)

authenticationToSignJSON :: AuthenticationToSignMethod -> JSValue
authenticationToSignJSON StandardAuthenticationToSign = toJSValue ("standard" :: String)
authenticationToSignJSON SEBankIDAuthenticationToSign = toJSValue ("eleg" :: String)
authenticationToSignJSON SMSPinAuthenticationToSign   = toJSValue ("sms_pin" :: String)
authenticationToSignJSON NOBankIDAuthenticationToSign = toJSValue ("no_bankid" :: String)
authenticationToSignJSON DKNemIDAuthenticationToSign  = toJSValue ("dk_nemid" :: String)
authenticationToSignJSON IDINAuthenticationToSign     = toJSValue ("nl_idin" :: String)

signatoryJSON
  :: ( MonadDB m
     , MonadIO m
     , MonadMask m
     , MonadFileStorage m
     , MonadLog m
     , MonadBaseControl IO m
     )
  => Bool
  -> Document
  -> Maybe SignatoryLink
  -> SignatoryLink
  -> JSONGenT m ()
signatoryJSON forauthor doc viewer siglink = do
  J.value "id" $ show $ signatorylinkid siglink
  J.value "current" $ isCurrent
  J.value "signorder" $ unSignOrder $ signatorysignorder siglink
  J.value "undeliveredInvitation"
    $  Undelivered
    == mailinvitationdeliverystatus siglink
    || Undelivered
    == smsinvitationdeliverystatus siglink
  J.value "undeliveredMailInvitation"
    $  Undelivered
    == mailinvitationdeliverystatus siglink
  J.value "undeliveredSMSInvitation" $ Undelivered == smsinvitationdeliverystatus siglink
  J.value "deliveredInvitation"
    $  Delivered
    == mailinvitationdeliverystatus siglink
    || Delivered
    == smsinvitationdeliverystatus siglink
  J.value "delivery" $ signatorylinkdeliverymethod siglink
  J.value "confirmationdelivery" $ signatorylinkconfirmationdeliverymethod siglink
  J.value "signs" $ isSignatory siglink
  J.value "author" $ isAuthor siglink
  J.value "saved" $ isJust . maybesignatory $ siglink
  J.value "datamismatch" (Nothing :: Maybe String)
  J.value "signdate" $ jsonDate $ signtime <$> maybesigninfo siglink
  J.value "seendate" $ jsonDate $ signtime <$> maybeseeninfo siglink
  J.value "readdate" $ jsonDate $ maybereadinvite siglink
  J.value "rejecteddate" $ jsonDate rejectedDate
  J.value "rejectionreason" $ signatorylinkrejectionreason siglink
  J.valueM "fields" $ signatoryFieldsJSON doc siglink
  J.value "status" $ show $ signatoryStatusClass doc siglink
  J.objects "attachments" $ map signatoryAttachmentJSON (signatoryattachments siglink)
  J.value "csv" $ csvcontents <$> signatorylinkcsvupload siglink
  J.value "inpadqueue" $ False
  J.value "userid" $ show <$> maybesignatory siglink
  J.value "signsuccessredirect" $ signatorylinksignredirecturl siglink
  J.value "rejectredirect" $ signatorylinkrejectredirecturl siglink
  J.value "authenticationToView"
    $ authenticationToViewJSON
    $ signatorylinkauthenticationtoviewmethod siglink
  J.value "hasAuthenticatedToView" $ signatorylinkidentifiedtoview siglink
  J.value "authentication"
    $ authenticationToSignJSON
    $ signatorylinkauthenticationtosignmethod siglink
  J.value "allowshighlighting" $ signatorylinkallowshighlighting siglink
  when
      (  not (isPreparation doc)
      && forauthor
      && signatorylinkdeliverymethod siglink
      == APIDelivery
      )
    $ do
        let msat = find ((== SignatoryAccessTokenForAPI) . signatoryAccessTokenReason)
                        (signatoryaccesstokens siglink)
        J.value "signlink" $ case signatoryAccessTokenHash <$> msat of
          Just mh ->
            show $ LinkSignDocMagicHash (documentid doc) (signatorylinkid siglink) mh
          Nothing ->
            "API delivery link not available. Please contact us for more details."
  where
    isCurrent =
      (signatorylinkid <$> viewer)
        == (Just $ signatorylinkid siglink)
        || (forauthor && isAuthor siglink)
    rejectedDate = signatorylinkrejectiontime siglink

signatoryAttachmentJSON
  :: (MonadDB m, MonadThrow m) => SignatoryAttachment -> JSONGenT m ()
signatoryAttachmentJSON sa = do
  mfile <- lift $ case (signatoryattachmentfile sa) of
    Just fid -> fmap Just $ dbQuery $ GetFileByFileID fid
    _        -> return Nothing
  J.value "name" $ signatoryattachmentname sa
  J.value "description" $ signatoryattachmentdescription sa
  J.value "file" $ fileJSON <$> mfile

signatoryFieldsJSON
  :: ( MonadDB m
     , MonadIO m
     , MonadMask m
     , MonadFileStorage m
     , MonadLog m
     , MonadBaseControl IO m
     )
  => Document
  -> SignatoryLink
  -> m JSValue
signatoryFieldsJSON doc sl = fmap (JSArray . catMaybes) $ forM orderedFields $ \sf -> do
  case sf of
    SignatoryNameField nf@(NameField { snfNameOrder = NameOrder 1 }) ->
      return $ Just $ fieldJSON
        "standard"
        "fstname"
        (Left (snfValue nf))
        ((not $ T.null $ snfValue nf) && (not $ isPreparation doc))
        (fieldIsObligatory sf)
        (fieldShouldBeFilledBySender sf)
        (fieldPlacements sf)
    SignatoryNameField nf@(NameField { snfNameOrder = NameOrder 2 }) ->
      return $ Just $ fieldJSON
        "standard"
        "sndname"
        (Left (snfValue nf))
        ((not $ T.null $ snfValue nf) && (not $ isPreparation doc))
        (fieldIsObligatory sf)
        (fieldShouldBeFilledBySender sf)
        (fieldPlacements sf)
    SignatoryNameField _ -> unexpectedError "Name field with order different then 1 or 2"
    SignatoryCompanyField cf -> return $ Just $ fieldJSON
      "standard"
      "sigco"
      (Left (scfValue cf))
      ((not $ T.null $ scfValue $ cf) && (not $ isPreparation doc))
      (fieldIsObligatory sf)
      (fieldShouldBeFilledBySender sf)
      (fieldPlacements sf)
    SignatoryPersonalNumberField pnf -> return $ Just $ fieldJSON
      "standard"
      "sigpersnr"
      (Left (spnfValue pnf))
      ((not $ T.null $ spnfValue $ pnf) && (not $ isPreparation doc))
      (fieldIsObligatory sf)
      (fieldShouldBeFilledBySender sf)
      (fieldPlacements sf)
    SignatoryCompanyNumberField cnf -> return $ Just $ fieldJSON
      "standard"
      "sigcompnr"
      (Left (scnfValue cnf))
      ((not $ T.null $ scnfValue $ cnf) && (not $ isPreparation doc))
      (fieldIsObligatory sf)
      (fieldShouldBeFilledBySender sf)
      (fieldPlacements sf)
    SignatoryEmailField ef -> return $ Just $ fieldJSON
      "standard"
      "email"
      (Left (sefValue ef))
      ((not $ T.null $ sefValue $ ef) && (not $ isPreparation doc))
      (fieldIsObligatory sf)
      (fieldShouldBeFilledBySender sf)
      (fieldPlacements sf)
    SignatoryMobileField mf -> return $ Just $ fieldJSON
      "standard"
      "mobile"
      (Left (smfValue mf))
      ((not $ T.null $ smfValue $ mf) && (not $ isPreparation doc))
      (fieldIsObligatory sf)
      (fieldShouldBeFilledBySender sf)
      (fieldPlacements sf)
    SignatoryTextField tf -> return $ Just $ fieldJSON
      "custom"
      (stfName tf)
      (Left (stfValue tf))
      ((stfFilledByAuthor tf && (not $ isPreparation doc)))
      (fieldIsObligatory sf)
      (fieldShouldBeFilledBySender sf)
      (fieldPlacements sf)
    SignatoryCheckboxField chf -> return $ Just $ fieldJSON
      "checkbox"
      (schfName chf)
      (Left (if (schfValue chf) then "checked" else ""))
      False
      (fieldIsObligatory sf)
      (fieldShouldBeFilledBySender sf)
      (fieldPlacements sf)
    SignatorySignatureField ssf -> do
      bs <- case (ssfValue ssf) of
        Nothing -> return BSC.empty
        Just fi -> getFileIDContents fi
      return $ Just $ fieldJSON "signature"
                                (ssfName ssf)
                                (Right $ bs)
                                (closedSignatureF ssf && (not $ isPreparation doc))
                                (fieldIsObligatory sf)
                                (fieldShouldBeFilledBySender sf)
                                (fieldPlacements sf)
    SignatoryRadioGroupField _ -> return Nothing -- Radio button group is doesn't have v1 representation
  where
    closedSignatureF ssf =
      (  (not $ isNothing $ ssfValue ssf)
      && (null $ ssfPlacements ssf)
      && ((PadDelivery /= signatorylinkdeliverymethod sl))
      )
    orderedFields = sortBy (\f1 f2 -> ftOrder (fieldIdentity f1) (fieldIdentity f2))
                           (signatoryfields sl)
    ftOrder (NameFI (NameOrder o1)) (NameFI (NameOrder o2)) = compare o1 o2
    ftOrder (NameFI (NameOrder _ )) _                = LT
    ftOrder _ (NameFI (NameOrder _))                 = LT
    ftOrder EmailFI                 _                = LT
    ftOrder _                       EmailFI          = LT
    ftOrder MobileFI                _                = LT
    ftOrder _                       MobileFI         = LT
    ftOrder CompanyFI               _                = LT
    ftOrder _                       CompanyFI        = LT
    ftOrder PersonalNumberFI        _                = LT
    ftOrder _                       PersonalNumberFI = LT
    ftOrder CompanyNumberFI         _                = LT
    ftOrder _                       CompanyNumberFI  = LT
    ftOrder _                       _                = EQ

fieldJSON
  :: Text
  -> Text
  -> Either Text BSC.ByteString
  -> Bool
  -> Bool
  -> Bool
  -> [FieldPlacement]
  -> JSValue
fieldJSON tp name v closed obligatory filledbysender placements = runJSONGen $ do
  J.value "type" $ T.unpack tp
  J.value "name" $ T.unpack name
  J.value "value" $ case v of
    Left  s -> T.unpack s
    Right s -> if (BSC.null s) then "" else BSC.unpack $ imgEncodeRFC2397 $ s
  J.value "closed" closed
  J.value "obligatory" obligatory
  J.value "shouldbefilledbysender" filledbysender
  J.value "placements" $ map placementJSON placements


placementJSON :: FieldPlacement -> JSValue
placementJSON placement = runJSONGen $ do
  J.value "xrel" $ placementxrel placement
  J.value "yrel" $ placementyrel placement
  J.value "wrel" $ placementwrel placement
  J.value "hrel" $ placementhrel placement
  J.value "fsrel" $ placementfsrel placement
  J.value "page" $ placementpage placement
  unless (null . placementanchors $ placement) $ do
    J.value "anchors" $ placementanchors placement
  J.value "tip" $ case (placementtipside placement) of
    Just LeftTip  -> Just ("left" :: String)
    Just RightTip -> Just "right"
    _             -> Nothing

instance ToJSValue PlacementAnchor where
  toJSValue anchor = runJSONGen $ do
    J.value "text" $ T.unpack $ placementanchortext anchor
    when (placementanchorindex anchor /= 1) $ do
      J.value "index" (placementanchorindex anchor)

instance ToJSValue ConfirmationDeliveryMethod where
  toJSValue EmailConfirmationDelivery          = toJSValue ("email" :: String)
  toJSValue MobileConfirmationDelivery         = toJSValue ("mobile" :: String)
  toJSValue EmailAndMobileConfirmationDelivery = toJSValue ("email_mobile" :: String)
  toJSValue NoConfirmationDelivery             = toJSValue ("none" :: String)
  toJSValue EmailLinkConfirmationDelivery      = toJSValue ("email_link" :: String)
  toJSValue EmailLinkAndMobileConfirmationDelivery =
    toJSValue ("email_link_mobile" :: String)

instance ToJSValue DeliveryMethod where
  toJSValue EmailDelivery          = toJSValue ("email" :: String)
  toJSValue PadDelivery            = toJSValue ("pad" :: String)
  toJSValue APIDelivery            = toJSValue ("api" :: String)
  toJSValue MobileDelivery         = toJSValue ("mobile" :: String)
  toJSValue EmailAndMobileDelivery = toJSValue ("email_mobile" :: String)
  toJSValue PortalDelivery         = toJSValue ("portal" :: String)

jsonDate :: Maybe UTCTime -> JSValue
jsonDate mdate = toJSValue $ formatTimeISO <$> mdate

fileJSON :: File -> JSValue
fileJSON file = runJSONGen $ do
  J.value "id" $ show $ fileid file
  J.value "name" $ T.unpack $ filename file

mainfileJSON :: MainFile -> JSValue
mainfileJSON file = runJSONGen $ do
  J.value "id" $ show $ mainfileid file
  J.value "name" $ mainfilename file

authorAttachmentJSON :: AuthorAttachment -> JSValue
authorAttachmentJSON att = runJSONGen $ do
  J.value "id" $ show $ authorattachmentfileid att
  J.value "name" $ T.unpack $ authorattachmentname att
  J.value "required" $ authorattachmentrequired att
  J.value "add_to_sealed_file" $ authorattachmentaddtosealedfile att

-- Converting document to JSON/CSV for lists
docForListJSONV1 :: TemplatesMonad m => User -> Document -> m JSValue
docForListJSONV1 user doc = do
  let link = LinkIssueDoc $ documentid doc
      sigFilter sl = isSignatory sl && (documentstatus doc /= Preparation)
  runJSONGenT $ do
    J.object "fields" $ docFieldsListForJSON (user ^. #id) doc
    J.objects "subfields" $ map (signatoryFieldsListForJSON doc)
                                (filter sigFilter (documentsignatorylinks doc))
    J.value "link" $ show link
    J.value "isauthor" $ fromMaybe False (isAuthor <$> getSigLinkFor user doc)
    J.value "docauthorcompanysameasuser"
      $  Just (user ^. #groupID)
      == documentauthorugid doc

docFieldsListForJSON :: TemplatesMonad m => UserID -> Document -> JSONGenT m ()
docFieldsListForJSON userid doc = do
  J.value "id" $ show $ documentid doc
  J.value "title" $ T.unpack $ documenttitle doc
  J.value "status" $ show $ documentstatusclass doc
  J.value "state" $ show $ documentstatus doc
  signingParties <- lift $ mapM getSmartNameOrPlaceholder $ filter
    isSignatory
    (documentsignatorylinks doc)
  J.value "party" $ T.unpack $ T.intercalate ", " $ signingParties
  J.value "partner" $ T.unpack $ T.intercalate ", " $ map getSmartName $ filter
    (\sl -> isSignatory sl && not (isAuthor sl))
    (documentsignatorylinks doc)
  J.value "partnercomp" $ T.unpack $ T.intercalate ", " $ map getCompanyName $ filter
    (\sl -> isSignatory sl && not (isAuthor sl))
    (documentsignatorylinks doc)
  J.value "author"
    $ T.unpack
    $ T.intercalate ", "
    $ map getSmartName
    $ filter isAuthor
    $ (documentsignatorylinks doc)
  J.value "time" $ formatTimeISO (documentmtime doc)
  J.value "ctime" $ formatTimeISO (documentctime doc)
  J.value "timeouttime" $ formatTimeISO <$> documenttimeouttime doc
  J.value "template" $ isTemplate doc
  J.value "partiescount" $ length $ (documentsignatorylinks doc)
  J.value "type" $ case documenttype doc of
    Template -> ("template" :: String)
    Signable -> "signable"
  J.value "process" ("contract" :: String) -- Constant. Need to leave it till we will change API version
  J.value "authentication"
    $ case
        nub (map signatorylinkauthenticationtosignmethod (documentsignatorylinks doc))
      of
        [StandardAuthenticationToSign] -> ("standard" :: String)
        [SEBankIDAuthenticationToSign] -> "eleg"
        [SMSPinAuthenticationToSign] -> "sms_pin"
        [NOBankIDAuthenticationToSign] -> "no_bankid"
        [DKNemIDAuthenticationToSign] -> "dk_nemid"
        _ -> "mixed"
  J.value "delivery"
    $ case nub (map signatorylinkdeliverymethod (documentsignatorylinks doc)) of
        [EmailDelivery ] -> ("email" :: String)
        [PadDelivery   ] -> "pad"
        [APIDelivery   ] -> "api"
        [MobileDelivery] -> "mobile"
        _                -> "mixed"
  J.value "deliveryMethods" $ nub
    [ signatorylinkdeliverymethod s
    | s <- documentsignatorylinks doc
    , not (isLastViewer doc s)
    ]
  J.value "anyinvitationundelivered"
    $  Pending
    == documentstatus doc
    && (  (   any (== Undelivered)
          $   mailinvitationdeliverystatus
          <$> documentsignatorylinks doc
          )
       || (   any (== Undelivered)
          $   smsinvitationdeliverystatus
          <$> documentsignatorylinks doc
          )
       )
  J.value "shared" $ isDocumentShared doc
  J.value "file"
    $   show
    <$> mainfileid
    <$> (documentsealedfile doc `mplus` documentfile doc)
  J.value "inpadqueue" $ False
  J.value "deleted" $ documentDeletedForUser doc userid
  J.value "reallydeleted" $ documentReallyDeletedForUser doc userid
  J.value "canperformsigning" $ userCanPerformSigningAction userid doc
  J.value "isviewedbyauthor" $ isSigLinkFor userid (getAuthorSigLink doc)
  J.value "objectversion" $ documentobjectversion doc

signatoryFieldsListForJSON
  :: TemplatesMonad m => Document -> SignatoryLink -> JSONGenT m ()
signatoryFieldsListForJSON doc sl = do
  J.value "id" $ show $ signatorylinkid sl
  J.value "status" $ show $ signatoryStatusClass doc sl
  J.value "name" $ T.unpack $ case T.strip (getCompanyName sl) of
    "" -> getSmartName sl
    _  -> getSmartName sl <> " (" <> getCompanyName sl <> ")"
  J.value "time"
    $   fromMaybe ""
    $   formatTimeISO
    <$> (sign `mplus` reject `mplus` seen `mplus` open)
  J.value "invitationundelivered"
    $  Pending
    == documentstatus doc
    && (  Undelivered
       == mailinvitationdeliverystatus sl
       || Undelivered
       == smsinvitationdeliverystatus sl
       )
  J.value "inpadqueue" $ False
  J.value "isauthor" $ isAuthor sl
  J.value "cansignnow" $ canSignatorySignNow doc sl
  J.value "authentication" $ case signatorylinkauthenticationtosignmethod sl of
    StandardAuthenticationToSign -> ("standard" :: String)
    SEBankIDAuthenticationToSign -> "eleg"
    SMSPinAuthenticationToSign   -> "sms_pin"
    NOBankIDAuthenticationToSign -> "no_bankid"
    DKNemIDAuthenticationToSign  -> "dk_nemid"
    IDINAuthenticationToSign     -> "nl_idin"

  J.value "delivery" $ signatorylinkdeliverymethod sl
  where
    sign   = signtime <$> maybesigninfo sl
    seen   = signtime <$> maybeseeninfo sl
    reject = signatorylinkrejectiontime sl
    open   = maybereadinvite sl

-- Haskell version of statusClassCaseExpression. We don't sort of filter signatories based on that, so there is no need to compute it in DB
signatoryStatusClass :: Document -> SignatoryLink -> StatusClass
signatoryStatusClass doc sl =
  case
      ( documentstatus doc
      , maybesigninfo sl
      , maybeseeninfo sl
      , maybereadinvite sl
      , mailinvitationdeliverystatus sl
      , smsinvitationdeliverystatus sl
      )
    of
      (DocumentError, _, _, _, _, _) -> SCError
      (Preparation, _, _, _, _, _) -> SCDraft
      (_, Just _, _, _, _, _) -> SCSigned
      (Canceled, _, _, _, _, _) -> SCCancelled
      (Timedout, _, _, _, _, _) -> SCTimedout
      (Rejected, _, _, _, _, _) -> SCRejected
      (_, _, Just _, _, _, _) -> SCOpened
      (_, _, _, Just _, _, _) -> SCRead
      (_, _, _, _, Undelivered, _) -> SCDeliveryProblem
      (_, _, _, _, _, Undelivered) -> SCDeliveryProblem
      (_, _, _, _, Delivered, _) -> SCDelivered
      (_, _, _, _, _, Delivered) -> SCDelivered
      _ -> SCSent

-- Converting document into entries in CSV
allCustomTextOrCheckboxOrRadioGroupFields :: [Document] -> [FieldIdentity]
allCustomTextOrCheckboxOrRadioGroupFields =
  sortBy fieldNameSort
    . nub
    . map fieldIdentity
    . filter isCustomOrCheckboxOrRadioGroup
    . concatMap signatoryfields
    . concatMap documentsignatorylinks
  where
    fieldNameSort fi1 fi2 = case (fi1, fi2) of
      (TextFI n1, TextFI n2) -> compare n1 n2
      (TextFI _, _) -> GT
      (SignatureFI n1, SignatureFI n2) -> compare n1 n2
      (SignatureFI _, _) -> GT
      (CheckboxFI n1, CheckboxFI n2) -> compare n1 n2
      (CheckboxFI _, _) -> GT
      (RadioGroupFI n1, RadioGroupFI n2) -> compare n1 n2
      _ -> EQ
    isCustomOrCheckboxOrRadioGroup sf = case fieldType sf of
      TextFT       -> True
      CheckboxFT   -> True
      RadioGroupFT -> True
      _            -> False

docForListCSVV1 :: [FieldIdentity] -> Document -> [[Text]]
docForListCSVV1 customFields doc =
  map (signatoryForListCSV customFields doc) $ documentsignatorylinks doc

signatoryForListCSV :: [FieldIdentity] -> Document -> SignatoryLink -> [Text]
signatoryForListCSV customFields doc sl =
  [ ("'" <> (showt (documentid doc)) <> "'" -- Excel trick
                                           )
    , documenttitle doc
    , showt $ documentstatusclass doc
    , getAuthorName $ doc
    , T.pack $ formatTimeSimple $ (documentctime doc)
    , T.pack $ maybe "" formatTimeSimple $ signtime <$> documentinvitetime doc
    , T.pack $ maybe "" formatTimeSimple $ documenttimeouttime doc
    , T.pack $ maybe "" formatTimeSimple $ maybereadinvite sl
    , T.pack $ maybe "" formatTimeSimple $ signtime <$> maybeseeninfo sl
    , T.pack $ maybe "" formatTimeSimple $ signtime <$> maybesigninfo sl
    , case signatoryrole sl of
      SignatoryRoleSigningParty          -> "Signing party"
      SignatoryRoleViewer                -> "Viewer"
      SignatoryRoleApprover              -> "Approver"
      SignatoryRoleForwardedSigningParty -> "Forwarded party"
      SignatoryRoleForwardedApprover     -> "Forwarded party"
    , getFullName sl
    , getEmail sl
    , getPersonalNumber sl
    , getCompanyName sl
    , getCompanyNumber sl
    , getMobile sl
    ]
    <> map (\fi -> maybe "" fieldValue $ getFieldByIdentity fi $ signatoryfields sl)
           customFields
  where
    fieldValue sf = case sf of
      SignatoryTextField     tf  -> stfValue tf
      SignatoryCheckboxField chf -> if (schfValue chf)
        then schfName chf <> " : checked"
        else schfName chf <> " : not checked"
      SignatoryRadioGroupField rgf -> fromMaybe "" (srgfSelectedValue rgf)
      _ -> ""

docForListCSVHeaderV1 :: [FieldIdentity] -> [Text]
docForListCSVHeaderV1 customFields =
  [ "Id"
    , "Title"
    , "Status"
    , "Author"
    , "Creation"
    , "Started"
    , "Signing deadline"
    , "Party read invitation"
    , "Party seen document"
    , "Party signed document"
    , "Party role"
    , "Party name"
    , "Party mail"
    , "Party personal number"
    , "Party company name"
    , "Party company number"
    , "Party mobile number"
    ]
    <> map fieldName customFields
  where
    fieldName (NameFI _)       = ""
    fieldName CompanyFI        = ""
    fieldName PersonalNumberFI = ""
    fieldName CompanyNumberFI  = ""
    fieldName EmailFI          = ""
    fieldName MobileFI         = ""
    fieldName (TextFI       n) = n
    fieldName (SignatureFI  n) = n
    fieldName (CheckboxFI   n) = n
    fieldName (RadioGroupFI n) = n
