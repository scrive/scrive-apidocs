{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.API.V1.DocumentFromJSON (AuthorAttachmentDetails(..)) where

import Data.List.Extra (nubOrdOn, trim)
import Text.JSON.FromJSValue
import qualified Data.Set as Set
import qualified Data.Text as T

import DB.TimeZoneName
import Doc.CheckboxPlacementsUtils
import Doc.DocStateData
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import InputValidation
import User.Lang

-- JSON instances

instance FromJSValue AuthenticationToViewMethod where
  fromJSValue = do
    j <- fromJSValue
    return $ case j of
      Just "standard"  -> Just StandardAuthenticationToView
      Just "se_bankid" -> Just SEBankIDAuthenticationToView
      Just "no_bankid" -> Just NOBankIDAuthenticationToView
      Just "dk_nemid"  -> Just DKNemIDAuthenticationToView
      Just "fi_tupas"  -> Just FITupasAuthenticationToView
      _                -> Nothing


instance FromJSValue AuthenticationToSignMethod where
  fromJSValue = do
    j <- fromJSValue
    return $ case j of
      Just "standard" -> Just StandardAuthenticationToSign
      Just "eleg"     -> Just SEBankIDAuthenticationToSign
      Just "sms_pin"  -> Just SMSPinAuthenticationToSign
      _               -> Nothing

instance FromJSValue DeliveryMethod where
  fromJSValue = do
    j <- fromJSValue
    return $ case j of
      Just "email"        -> Just EmailDelivery
      Just "pad"          -> Just PadDelivery
      Just "api"          -> Just APIDelivery
      Just "mobile"       -> Just MobileDelivery
      Just "email_mobile" -> Just EmailAndMobileDelivery
      Just "portal"       -> Just PortalDelivery
      _                   -> Nothing

instance FromJSValue ConfirmationDeliveryMethod where
  fromJSValue = do
    j <- fromJSValue
    return $ case j of
      Just "email" -> Just EmailConfirmationDelivery
      Just "mobile" -> Just MobileConfirmationDelivery
      Just "email_mobile" -> Just EmailAndMobileConfirmationDelivery
      Just "none" -> Just NoConfirmationDelivery
      Just "email_link" -> Just EmailLinkConfirmationDelivery
      Just "email_link_mobile" -> Just EmailLinkAndMobileConfirmationDelivery
      _ -> Nothing


instance FromJSValue TipSide where
  fromJSValue = do
    s <- fromJSValue
    case s of
      Just "left"  -> return $ Just LeftTip
      Just "right" -> return $ Just RightTip
      _            -> return Nothing

instance FromJSValue PlacementAnchor where
  fromJSValue = do
    text  <- fromJSValueField "text"
    index <- fromMaybe (Just 1) <$> fromJSValueField "index"
    return (PlacementAnchor <$> text <*> index)

instance FromJSValue FieldPlacement where
  fromJSValue = do
    xrel    <- fromJSValueField "xrel"
    yrel    <- fromJSValueField "yrel"
    wrel    <- fromJSValueField "wrel"
    hrel    <- fromJSValueField "hrel"
    fsrel   <- fromJSValueField "fsrel"
    page    <- fromJSValueField "page"
    side    <- fromJSValueField "tip"
    anchors <- fromMaybe (Just []) <$> fromJSValueField "anchors"
    return
      (   FieldPlacement
      <$> pure tempPlacementID
      <*> xrel
      <*> yrel
      <*> wrel
      <*> hrel
      <*> fsrel
      <*> page
      <*> Just side
      <*> anchors
      )


instance MatchWithJSValue SignatoryLink where
  matchesWithJSValue s = do
    mid <- fromJSValueField "id"
    return (Just (signatorylinkid s) == (maybeRead =<< mid))

instance FromJSValueWithUpdate SignatoryLink where
  fromJSValueWithUpdate ms = do
    author  <- fromJSValueField "author"
    role    <- fmap signatoryRoleFromBool <$> fromJSValueField "signs"
    mfields <- fromJSValueFieldCustom
      "fields"
      (fromJSValueManyWithUpdate $ maybe [] signatoryfields ms)
    signorder   <- fromJSValueField "signorder"
    attachments <- fromJSValueField "attachments"
    (csv :: Maybe (Maybe CSVUpload)       ) <- fromJSValueField "csv"
    (sredirecturl' :: Maybe (Maybe String)) <- fromJSValueField "signsuccessredirect"
    (rredirecturl' :: Maybe (Maybe String)) <- fromJSValueField "rejectredirect"
    let emptyIfNaughty url =
          if any (`isPrefixOf` trim url) ["javascript:", "data:"] then "" else url
        sredirecturl = fmap (fmap emptyIfNaughty) sredirecturl'
        rredirecturl = fmap (fmap emptyIfNaughty) rredirecturl'
    authenticationToView' <- fromJSValueField "authenticationToView"
    authenticationToSign' <- fromJSValueField "authentication"
    delivery'             <- fromJSValueField "delivery"
    confirmationdelivery' <- fromJSValueField "confirmationdelivery"
    allowshighlighting'   <- fromJSValueField "allowshighlighting"
    case mfields of
      (Just fields) -> return . Just $ defaultSignatoryLink
        { signatorylinkid = maybe (unsafeSignatoryLinkID 0) signatorylinkid ms
        , signatorysignorder = updateWithDefaultAndField (SignOrder 1)
                                                         signatorysignorder
                                                         (SignOrder <$> signorder)
           -- nubBy comment: At some point, we accepted
           -- documents with non-unique fields for signatory.
           -- To keep the current workflow for some clients, we
           -- have to manually clean fields, else the DB will
           -- reject update with constraints error.
        , signatoryfields = nubOrdOn fieldIdentity fields
        , signatoryisauthor = updateWithDefaultAndField False signatoryisauthor author
        , signatoryrole = updateWithDefaultAndField SignatoryRoleViewer signatoryrole role
        , signatorylinkcsvupload = updateWithDefaultAndField Nothing
                                                             signatorylinkcsvupload
                                                             csv
        , signatoryattachments = updateSignatoryAttachmentList
                                   (fmap signatoryattachments ms)
                                   attachments
        , signatorylinksignredirecturl = updateWithDefaultAndField
                                           Nothing
                                           signatorylinksignredirecturl
                                           sredirecturl
        , signatorylinkrejectredirecturl = updateWithDefaultAndField
                                             Nothing
                                             signatorylinkrejectredirecturl
                                             rredirecturl
        , signatorylinkauthenticationtoviewmethod =
          updateWithDefaultAndField StandardAuthenticationToView
                                    signatorylinkauthenticationtoviewmethod
                                    authenticationToView'
        , signatorylinkauthenticationtosignmethod =
          updateWithDefaultAndField StandardAuthenticationToSign
                                    signatorylinkauthenticationtosignmethod
                                    authenticationToSign'
        , signatorylinkdeliverymethod = updateWithDefaultAndField
                                          EmailDelivery
                                          signatorylinkdeliverymethod
                                          delivery'
        , signatorylinkconfirmationdeliverymethod =
          updateWithDefaultAndField EmailConfirmationDelivery
                                    signatorylinkconfirmationdeliverymethod
                                    confirmationdelivery'
        , signatorylinkallowshighlighting = updateWithDefaultAndField
                                              False
                                              signatorylinkallowshighlighting
                                              allowshighlighting'
        }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatoryLink -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf ms)
      updateSignatoryAttachmentList
        :: Maybe [SignatoryAttachment]
        -> Maybe [SignatoryAttachment]
        -> [SignatoryAttachment]
      updateSignatoryAttachmentList existingAtts Nothing = fromMaybe [] existingAtts
      updateSignatoryAttachmentList Nothing (Just newAtts) = newAtts
      updateSignatoryAttachmentList (Just existingAtts) (Just newAtts) = map
        (requiredFromExisting existingAtts)
        newAtts
        where
          requiredFromExisting
            :: [SignatoryAttachment] -> SignatoryAttachment -> SignatoryAttachment
          requiredFromExisting existing new =
            case
                find (\e -> signatoryattachmentname e == signatoryattachmentname new)
                     existing
              of
                Nothing -> new
                Just e ->
                  new { signatoryattachmentrequired = signatoryattachmentrequired e }

instance FromJSValue SignatoryField where
  fromJSValue = fromJSValueWithUpdate Nothing


instance FromJSValueWithUpdate SignatoryField where
  fromJSValueWithUpdate msf = do
    fidentity <- fromJSValue
    case (fieldTypeFromFieldIdentity <$> fidentity, msf) of
      (Just NameFT, Just (SignatoryNameField snf)) ->
        fmap SignatoryNameField <$> fromJSValueWithUpdate (Just snf)
      (Just NameFT, _) -> fmap SignatoryNameField <$> fromJSValueWithUpdate Nothing
      (Just CompanyFT, Just (SignatoryCompanyField scf)) ->
        fmap SignatoryCompanyField <$> fromJSValueWithUpdate (Just scf)
      (Just CompanyFT, _) -> fmap SignatoryCompanyField <$> fromJSValueWithUpdate Nothing
      (Just PersonalNumberFT, Just (SignatoryPersonalNumberField spnf)) ->
        fmap SignatoryPersonalNumberField <$> fromJSValueWithUpdate (Just spnf)
      (Just PersonalNumberFT, _) ->
        fmap SignatoryPersonalNumberField <$> fromJSValueWithUpdate Nothing
      (Just CompanyNumberFT, Just (SignatoryCompanyNumberField scnf)) ->
        fmap SignatoryCompanyNumberField <$> fromJSValueWithUpdate (Just scnf)
      (Just CompanyNumberFT, _) ->
        fmap SignatoryCompanyNumberField <$> fromJSValueWithUpdate Nothing
      (Just EmailFT, Just (SignatoryEmailField sef)) ->
        fmap SignatoryEmailField <$> fromJSValueWithUpdate (Just sef)
      (Just EmailFT, _) -> fmap SignatoryEmailField <$> fromJSValueWithUpdate Nothing
      (Just MobileFT, Just (SignatoryMobileField smf)) ->
        fmap SignatoryMobileField <$> fromJSValueWithUpdate (Just smf)
      (Just MobileFT, _) -> fmap SignatoryMobileField <$> fromJSValueWithUpdate Nothing
      (Just TextFT, Just (SignatoryTextField stf)) ->
        fmap SignatoryTextField <$> fromJSValueWithUpdate (Just stf)
      (Just TextFT, _) -> fmap SignatoryTextField <$> fromJSValueWithUpdate Nothing
      (Just SignatureFT, Just (SignatorySignatureField ssf)) ->
        fmap SignatorySignatureField <$> fromJSValueWithUpdate (Just ssf)
      (Just SignatureFT, _) ->
        fmap SignatorySignatureField <$> fromJSValueWithUpdate Nothing
      (Just CheckboxFT, Just (SignatoryCheckboxField schf)) ->
        fmap SignatoryCheckboxField <$> fromJSValueWithUpdate (Just schf)
      (Just CheckboxFT, _) ->
        fmap SignatoryCheckboxField <$> fromJSValueWithUpdate Nothing
      _ -> return Nothing

instance FromJSValueWithUpdate SignatoryNameField where
  fromJSValueWithUpdate msf = do
    fidentity  <- fromJSValue
    value      <- fromJSValueField "value"
    obligatory <- updateWithDefaultAndField True snfObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False snfShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] snfPlacements
      <$> fromJSValueField "placements"
    case (fidentity, value) of
      (Just (NameFI no), Just v) -> do
        return . Just $ NameField { snfID = maybe (unsafeSignatoryFieldID 0) snfID msf
                                  , snfNameOrder              = no
                                  , snfValue                  = v
                                  , snfObligatory             = obligatory
                                  , snfShouldBeFilledBySender = filledbysender
                                  , snfPlacements             = placements
                                  }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatoryNameField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)


instance FromJSValueWithUpdate SignatoryCompanyField where
  fromJSValueWithUpdate msf = do
    value      <- fromJSValueField "value"
    obligatory <- updateWithDefaultAndField True scfObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False scfShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] scfPlacements
      <$> fromJSValueField "placements"
    case value of
      (Just v) -> do
        return . Just $ CompanyField { scfID = maybe (unsafeSignatoryFieldID 0) scfID msf
                                     , scfValue                  = v
                                     , scfObligatory             = obligatory
                                     , scfShouldBeFilledBySender = filledbysender
                                     , scfPlacements             = placements
                                     }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatoryCompanyField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)

instance FromJSValueWithUpdate SignatoryPersonalNumberField where
  fromJSValueWithUpdate msf = do
    value      <- fromJSValueField "value"
    obligatory <- updateWithDefaultAndField True spnfObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False spnfShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] spnfPlacements
      <$> fromJSValueField "placements"
    case value of
      (Just v) -> do
        return . Just $ PersonalNumberField
          { spnfID                     = maybe (unsafeSignatoryFieldID 0) spnfID msf
          , spnfValue                  = T.strip v
          , spnfObligatory             = obligatory
          , spnfShouldBeFilledBySender = filledbysender
          , spnfPlacements             = placements
          }
      _ -> return Nothing
    where
      updateWithDefaultAndField
        :: a -> (SignatoryPersonalNumberField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)

instance FromJSValueWithUpdate SignatoryCompanyNumberField where
  fromJSValueWithUpdate msf = do
    value      <- fromJSValueField "value"
    obligatory <- updateWithDefaultAndField True scnfObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False scnfShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] scnfPlacements
      <$> fromJSValueField "placements"
    case value of
      (Just v) -> do
        return . Just $ CompanyNumberField
          { scnfID                     = maybe (unsafeSignatoryFieldID 0) scnfID msf
          , scnfValue                  = v
          , scnfObligatory             = obligatory
          , scnfShouldBeFilledBySender = filledbysender
          , scnfPlacements             = placements
          }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatoryCompanyNumberField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)

instance FromJSValueWithUpdate SignatoryEmailField where
  fromJSValueWithUpdate msf = do
    value      <- fromJSValueField "value"
    obligatory <- updateWithDefaultAndField True sefObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False sefShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] sefPlacements
      <$> fromJSValueField "placements"
    case value of
      (Just v) -> do
        return . Just $ EmailField { sefID = maybe (unsafeSignatoryFieldID 0) sefID msf
                                   , sefValue                  = T.strip v
                                   , sefObligatory             = obligatory
                                   , sefShouldBeFilledBySender = filledbysender
                                   , sefEditableBySignatory    = False
                                   , sefPlacements             = placements
                                   }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatoryEmailField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)

instance FromJSValueWithUpdate SignatoryMobileField where
  fromJSValueWithUpdate msf = do
    value      <- fromJSValueField "value"
    obligatory <- updateWithDefaultAndField True smfObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False smfShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] smfPlacements
      <$> fromJSValueField "placements"
    case value of
      (Just v) -> do
        return . Just $ MobileField { smfID = maybe (unsafeSignatoryFieldID 0) smfID msf
                                    , smfValue                  = v
                                    , smfObligatory             = obligatory
                                    , smfShouldBeFilledBySender = filledbysender
                                    , smfEditableBySignatory    = False
                                    , smfPlacements             = placements
                                    }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatoryMobileField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)

instance FromJSValueWithUpdate SignatoryTextField where
  fromJSValueWithUpdate msf = do
    fidentity  <- fromJSValue
    value      <- fromJSValueField "value"
    obligatory <- updateWithDefaultAndField True stfObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False stfShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] stfPlacements
      <$> fromJSValueField "placements"
    case (fidentity, value) of
      (Just (TextFI n), Just v) -> do
        return . Just $ TextField { stfID = maybe (unsafeSignatoryFieldID 0) stfID msf
                                  , stfName                   = n
                                  , stfFilledByAuthor         = not $ null v
                                  , stfValue                  = T.pack v
                                  , stfObligatory             = obligatory
                                  , stfShouldBeFilledBySender = filledbysender
                                  , stfPlacements             = placements
                                  , stfCustomValidation = msf >>= stfCustomValidation -- Custom validations are not supported on V1 API, so we will not parse them from JSON
                                  }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatoryTextField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)

instance FromJSValueWithUpdate SignatoryCheckboxField where
  fromJSValueWithUpdate msf = do
    fidentity               <- fromJSValue
    (value :: Maybe String) <- fromJSValueField "value"
    obligatory              <- updateWithDefaultAndField True schfObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False schfShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] schfPlacements
      <$> fromJSValueField "placements"
    case (fidentity, value) of
      (Just (CheckboxFI n), Just v) -> do
        return . Just $ CheckboxField
          { schfID                     = maybe (unsafeSignatoryFieldID 0) schfID msf
          , schfName                   = n
          , schfValue                  = not $ null v
          , schfObligatory             = obligatory
          , schfShouldBeFilledBySender = filledbysender
          , schfPlacements             = map fixCheckboxPlacementRatioIfInvalid placements
          }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatoryCheckboxField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)

instance FromJSValueWithUpdate SignatorySignatureField where
  fromJSValueWithUpdate msf = do
    fidentity               <- fromJSValue
    (value :: Maybe String) <- fromJSValueField "value"
    obligatory              <- updateWithDefaultAndField True ssfObligatory
      <$> fromJSValueField "obligatory"
    filledbysender <- updateWithDefaultAndField False ssfShouldBeFilledBySender
      <$> fromJSValueField "shouldbefilledbysender"
    placements <- updateWithDefaultAndField [] ssfPlacements
      <$> fromJSValueField "placements"
    case (fidentity, value) of
      (Just (SignatureFI n), Just _) -> do
        return . Just $ SignatureField
          { ssfID                     = maybe (unsafeSignatoryFieldID 0) ssfID msf
          , ssfName                   = n
          , ssfValue                  = Nothing -- We ignore value. Signature can't be provided in design view
          , ssfObligatory             = obligatory
          , ssfShouldBeFilledBySender = filledbysender
          , ssfPlacements             = placements
          }
      _ -> return Nothing
    where
      updateWithDefaultAndField :: a -> (SignatorySignatureField -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf msf)

instance MatchWithJSValue SignatoryField where
  matchesWithJSValue sf = do
    ftype <- fromJSValue
    return (ftype == Just (fieldIdentity sf))

instance FromJSValue SignatoryAttachment where
  fromJSValue = do
    name        <- fromJSValueField "name"
    description <- fromJSValueField "description"
    case (name, description) of
      (Just n, Just d) -> return . Just $ SignatoryAttachment
        { signatoryattachmentname        = n
        , signatoryattachmentdescription = d
        , signatoryattachmentfile        = Nothing
        , signatoryattachmentfilename    = Nothing
        , signatoryattachmentrequired    = True
        }
      _ -> return Nothing

instance FromJSValue FieldIdentity where
  fromJSValue = do
    s <- fromJSValueField "name"
    t <- fromJSValueField "type"
    return $ case (fromMaybe "standard" t, s) of
      ("standard", Just "fstname") -> Just $ NameFI (NameOrder 1)
      ("standard", Just "sndname") -> Just $ NameFI (NameOrder 2)
      ("standard", Just "email") -> Just EmailFI
      ("standard", Just "mobile") -> Just MobileFI
      ("standard", Just "sigpersnr") -> Just PersonalNumberFI
      ("standard", Just "sigco") -> Just CompanyFI
      ("standard", Just "sigcompnr") -> Just CompanyNumberFI
      ("signature", Just name) -> Just $ SignatureFI name
      ("custom", Just name) -> Just $ TextFI name
      ("checkbox", Just name) -> Just $ CheckboxFI name
      _ -> Nothing


instance FromJSValue CSVUpload  where
  fromJSValue = do
    rows <- fromJSValue
    case rows of
      Just rs -> return . Just $ CSVUpload rs
      _       -> return Nothing

instance FromJSValue DocumentTag where
  fromJSValue = do
    name  <- fromJSValueField "name"
    value <- fromJSValueField "value"
    case (name, value) of
      (Just n, Just v) -> return . Just $ DocumentTag n v
      _                -> return Nothing

instance FromJSValue Lang where
  fromJSValue = do
    j <- fromJSValue
    return $ case j of -- Due to documentation inconsistency we need to support gb and en for a while.
      Just "se" -> Just LANG_SV
      Just "sv" -> Just LANG_SV
      Just "en" -> Just LANG_EN
      Just "gb" -> Just LANG_EN
      Just l    -> langFromCode l
      _         -> Nothing

instance FromJSValueWithUpdate Document where
  fromJSValueWithUpdate mdoc = do
    (title :: Maybe Text                    ) <- fromJSValueField "title"
    (invitationmessage :: Maybe (Maybe Text)) <- fromJSValueField "invitationmessage"
    (confirmationmessage :: Maybe (Maybe Text)) <- fromJSValueField "confirmationmessage"
    daystosign        <- fromJSValueField "daystosign"
    daystoremind      <- fromJSValueField "daystoremind"
    showheader        <- fromJSValueField "showheader"
    showpdfdownload   <- fromJSValueField "showpdfdownload"
    showrejectoption  <- fromJSValueField "showrejectoption"
    allowrejectreason <- fromJSValueField "allowrejectreason"
    showfooter        <- fromJSValueField "showfooter"
    isreceipt         <- fromJSValueField "isreceipt"
    authentication    <- fromJSValueField "authentication"
    delivery          <- fromJSValueField "delivery"
    signatories       <- fromJSValueFieldCustom
      "signatories"
      (fromJSValueManyWithUpdate (maybe [] documentsignatorylinks mdoc))
    lang      <- fromJSValueField "lang"
    mtimezone <- fromJSValueField "timezone"
    doctype   <- fmap (\t -> if t then Template else Signable)
      <$> fromJSValueField "template"
    tags              <- fromJSValueFieldCustom "tags" $ fromJSValueCustomMany fromJSValue
    (apicallbackurl :: Maybe (Maybe Text)) <- fromJSValueField "apicallbackurl"
    saved             <- fromJSValueField "saved"
    authorattachments <- fromJSValueFieldCustom "authorattachments"
      $ fromJSValueCustomMany ((maybeRead =<<) <$> fromJSValueField "id")
    let daystosign' =
          min 365 . max 1 $ updateWithDefaultAndField 14 documentdaystosign daystosign
    let daystoremind' =
          min daystosign'
            .   max 1
            <$> updateWithDefaultAndField Nothing documentdaystoremind daystoremind

    return $ Just defaultDocument
      { documenttitle             = updateWithDefaultAndField "" documenttitle title
      , documentlang              = updateWithDefaultAndField LANG_SV documentlang lang
      , documentinvitetext        =
        case invitationmessage of
          Nothing       -> maybe "" documentinvitetext mdoc
          Just Nothing  -> ""
          Just (Just s) -> fromMaybe "" (resultToMaybe $ asValidInviteText s)
      , documentconfirmtext       =
        case confirmationmessage of
          Nothing       -> maybe "" documentconfirmtext mdoc
          Just Nothing  -> ""
          Just (Just s) -> fromMaybe "" (resultToMaybe $ asValidInviteText s)
      , documentdaystosign        = daystosign'
      , documentdaystoremind      = daystoremind'
      , documentshowheader = updateWithDefaultAndField True documentshowheader showheader
      , documentshowpdfdownload   = updateWithDefaultAndField True
                                                              documentshowpdfdownload
                                                              showpdfdownload
      , documentallowrejectreason = updateWithDefaultAndField True
                                                              documentallowrejectreason
                                                              allowrejectreason
      , documentshowrejectoption  = updateWithDefaultAndField True
                                                              documentshowrejectoption
                                                              showrejectoption
      , documentshowfooter = updateWithDefaultAndField True documentshowfooter showfooter
      , documentisreceipt = updateWithDefaultAndField False documentisreceipt isreceipt
      , documentsignatorylinks    = mapAuth authentication
                                    . mapDL delivery
                                    $ updateWithDefaultAndField []
                                                                documentsignatorylinks
                                                                signatories
        -- Author attachments read by V1 for update call can be only used for deletion - this is why we can actually set name to "-" and required to False
      , documentauthorattachments = updateWithDefaultAndField
                                      []
                                      documentauthorattachments
                                      (   fmap (AuthorAttachment "-" False True)
                                      <$> authorattachments
                                      )
      , documenttags              = updateWithDefaultAndField Set.empty
                                                              documenttags
                                                              (Set.fromList <$> tags)
      , documenttype = updateWithDefaultAndField Signable documenttype doctype
      , documentapiv1callbackurl  = updateWithDefaultAndField Nothing
                                                              documentapiv1callbackurl
                                                              apicallbackurl
      , documentunsaveddraft      = updateWithDefaultAndField False
                                                              documentunsaveddraft
                                                              (fmap not saved)
      , documenttimezonename      = updateWithDefaultAndField
                                      defaultTimeZoneName
                                      documenttimezonename
                                      (unsafeTimeZoneName <$> mtimezone)
      }
    where
      updateWithDefaultAndField :: a -> (Document -> a) -> Maybe a -> a
      updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` fmap uf mdoc)
      mapDL :: Maybe DeliveryMethod -> [SignatoryLink] -> [SignatoryLink]
      mapDL Nothing   sls = sls
      mapDL (Just dl) sls = map (\sl -> sl { signatorylinkdeliverymethod = dl }) sls
      mapAuth :: Maybe AuthenticationToSignMethod -> [SignatoryLink] -> [SignatoryLink]
      mapAuth Nothing sls = sls
      mapAuth (Just au) sls =
        map (\sl -> sl { signatorylinkauthenticationtosignmethod = au }) sls


-- Author attachment utils. Used only for set author attachment call.
data AuthorAttachmentDetails = AuthorAttachmentDetails
  { aadName :: Text
  , aadRequired :: Bool
  , aadAddToSealedFile :: Bool
  } deriving (Eq,Show)

instance FromJSValue AuthorAttachmentDetails where
  fromJSValue = do
    name     <- fromJSValueField "name"
    required <- fromJSValueField "required"
    added    <- fromJSValueField "add_to_sealed_file"
    case (name, required) of
      (Just n, Just r) ->
        return . Just $ AuthorAttachmentDetails (T.pack n) r (fromMaybe True added)
      _ -> return Nothing
