{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.API.V1.DocumentFromJSON () where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.String.Utils (strip)
import Text.JSON.FromJSValue
import qualified Data.Set as Set

import DB.TimeZoneName
import Doc.DocStateData
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import InputValidation
import User.Lang
import Utils.Default
import Utils.Read

-- JSON instances

instance FromJSValue AuthenticationMethod where
  fromJSValue = do
    j <- fromJSValue
    return $ case j of
      Just "standard" -> Just StandardAuthentication
      Just "eleg"     -> Just ELegAuthentication
      Just "sms_pin"  -> Just SMSPinAuthentication
      _               -> Nothing

instance FromJSValue DeliveryMethod where
  fromJSValue = do
    j <- fromJSValue
    return $ case j of
      Just "email" -> Just EmailDelivery
      Just "pad"   -> Just PadDelivery
      Just "api"   -> Just APIDelivery
      Just "mobile"-> Just MobileDelivery
      Just "email_mobile"-> Just EmailAndMobileDelivery
      _            -> Nothing

instance FromJSValue ConfirmationDeliveryMethod where
  fromJSValue = do
    j <- fromJSValue
    return $ case j of
      Just "email" -> Just EmailConfirmationDelivery
      Just "mobile"-> Just MobileConfirmationDelivery
      Just "email_mobile"-> Just EmailAndMobileConfirmationDelivery
      Just "none"-> Just NoConfirmationDelivery
      _            -> Nothing


instance FromJSValue TipSide where
    fromJSValue = do
      s <- fromJSValue
      case s of
          Just "left"  -> return $ Just LeftTip
          Just "right" -> return $ Just RightTip
          _ ->            return $ Nothing

instance FromJSValue PlacementAnchor where
  fromJSValue = do
    text        <- fromJSValueField "text"
    index       <- fromMaybe (Just 1) <$> fromJSValueField "index"
    pages       <- fromJSValueField "pages"
    return (PlacementAnchor <$> text
            <*> index
            <*> pages)

instance FromJSValue FieldPlacement where
  fromJSValue = do
                  xrel       <- fromJSValueField "xrel"
                  yrel       <- fromJSValueField "yrel"
                  wrel       <- fromJSValueField "wrel"
                  hrel       <- fromJSValueField "hrel"
                  fsrel      <- fromJSValueField "fsrel"
                  page       <- fromJSValueField "page"
                  side       <- fromJSValueField "tip"
                  anchors    <- fromMaybe (Just []) <$> fromJSValueField "anchors"
                  return (FieldPlacement <$> xrel <*> yrel
                                         <*> wrel <*> hrel <*> fsrel
                                         <*> page <*> Just side
                                         <*> anchors)


instance MatchWithJSValue SignatoryLink where
    matchesWithJSValue s = do
      mid    <- fromJSValueField "id"
      return (Just (signatorylinkid s) == (maybeRead =<< mid))

instance FromJSValueWithUpdate SignatoryLink where
    fromJSValueWithUpdate ms = do
        author <- fromJSValueField "author"
        signs  <- fromJSValueField "signs"
        mfields <- fromJSValueFieldCustom "fields" (fromJSValueManyWithUpdate $ fromMaybe [] (signatoryfields <$> ms))
        signorder <- fromJSValueField "signorder"
        attachments <- fromJSValueField "attachments"
        (csv :: Maybe (Maybe CSVUpload)) <- fromJSValueField "csv"
        (sredirecturl :: Maybe (Maybe String)) <- fromJSValueField "signsuccessredirect"
        (rredirecturl :: Maybe (Maybe String)) <- fromJSValueField "rejectredirect"
        authentication' <-  fromJSValueField "authentication"
        delivery' <-  fromJSValueField "delivery"
        confirmationdelivery' <-  fromJSValueField "confirmationdelivery"
        case (mfields) of
             (Just fields) -> return $ Just $ defaultValue {
                    signatorylinkid            = fromMaybe (unsafeSignatoryLinkID 0) (signatorylinkid <$> ms)
                  , signatorysignorder     = updateWithDefaultAndField (SignOrder 1) signatorysignorder (SignOrder <$> signorder)
                  -- nubBy comment: We accepted at some point documents with not uniqueue fields for signatory.
                  -- To keep current workflow for some clients, we have to manually clean fields, else DB will reject update with contraints error
                  , signatoryfields        = nubBy (\f1 f2 -> fieldIdentity f1 == fieldIdentity f2) fields
                  , signatoryisauthor      = updateWithDefaultAndField False signatoryisauthor author
                  , signatoryispartner     = updateWithDefaultAndField False signatoryispartner signs
                  , signatorylinkcsvupload       = updateWithDefaultAndField Nothing signatorylinkcsvupload csv
                  , signatoryattachments         = updateWithDefaultAndField [] signatoryattachments attachments
                  , signatorylinksignredirecturl = updateWithDefaultAndField Nothing signatorylinksignredirecturl sredirecturl
                  , signatorylinkrejectredirecturl = updateWithDefaultAndField Nothing signatorylinkrejectredirecturl rredirecturl
                  , signatorylinkauthenticationmethod = updateWithDefaultAndField StandardAuthentication signatorylinkauthenticationmethod authentication'
                  , signatorylinkdeliverymethod       = updateWithDefaultAndField EmailDelivery signatorylinkdeliverymethod delivery'
                  , signatorylinkconfirmationdeliverymethod = updateWithDefaultAndField EmailConfirmationDelivery signatorylinkconfirmationdeliverymethod confirmationdelivery'
                }
             _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryLink -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf ms))

instance FromJSValue SignatoryField where
    fromJSValue = fromJSValueWithUpdate Nothing


instance FromJSValueWithUpdate SignatoryField where
    fromJSValueWithUpdate msf =  do
        fidentity<- fromJSValue
        case (fieldTypeFromFieldIdentity <$> fidentity, msf) of
          (Just NameFT,           Just (SignatoryNameField snf))            -> fmap SignatoryNameField <$> fromJSValueWithUpdate (Just snf)
          (Just NameFT,           _)                                        -> fmap SignatoryNameField <$> fromJSValueWithUpdate Nothing
          (Just CompanyFT,        Just (SignatoryCompanyField scf))         -> fmap SignatoryCompanyField <$> fromJSValueWithUpdate (Just scf)
          (Just CompanyFT,        _)                                        -> fmap SignatoryCompanyField <$> fromJSValueWithUpdate Nothing
          (Just PersonalNumberFT, Just (SignatoryPersonalNumberField spnf)) -> fmap SignatoryPersonalNumberField <$> fromJSValueWithUpdate (Just spnf)
          (Just PersonalNumberFT,_)                                         -> fmap SignatoryPersonalNumberField <$> fromJSValueWithUpdate Nothing
          (Just CompanyNumberFT,  Just (SignatoryCompanyNumberField scnf))  -> fmap SignatoryCompanyNumberField <$> fromJSValueWithUpdate (Just scnf)
          (Just CompanyNumberFT,  _)                                        -> fmap SignatoryCompanyNumberField <$> fromJSValueWithUpdate Nothing
          (Just EmailFT,          Just (SignatoryEmailField sef))           -> fmap SignatoryEmailField <$> fromJSValueWithUpdate (Just sef)
          (Just EmailFT,          _)                                        -> fmap SignatoryEmailField <$> fromJSValueWithUpdate Nothing
          (Just MobileFT,         Just (SignatoryMobileField smf))          -> fmap SignatoryMobileField <$> fromJSValueWithUpdate (Just smf)
          (Just MobileFT,         _)                                        -> fmap SignatoryMobileField <$> fromJSValueWithUpdate Nothing
          (Just TextFT,           Just (SignatoryTextField stf))            -> fmap SignatoryTextField <$> fromJSValueWithUpdate (Just stf)
          (Just TextFT,           _)                                        -> fmap SignatoryTextField <$> fromJSValueWithUpdate Nothing
          (Just SignatureFT,      Just (SignatorySignatureField ssf))       -> fmap SignatorySignatureField <$> fromJSValueWithUpdate (Just ssf)
          (Just SignatureFT,      _)                                        -> fmap SignatorySignatureField <$> fromJSValueWithUpdate Nothing
          (Just CheckboxFT,       Just (SignatoryCheckboxField schf))       -> fmap SignatoryCheckboxField <$> fromJSValueWithUpdate (Just schf)
          (Just CheckboxFT,       _)                                        -> fmap SignatoryCheckboxField <$> fromJSValueWithUpdate Nothing
          _ -> return Nothing

instance FromJSValueWithUpdate SignatoryNameField where
    fromJSValueWithUpdate msf =  do
        fidentity<- fromJSValue
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True snfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False snfShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] snfPlacements <$> fromJSValueField "placements"
        case (fidentity,value) of
          (Just (NameFI no), Just v) -> do
              return $ Just $ NameField {
                  snfID = (maybe (unsafeSignatoryFieldID 0) snfID msf)
                , snfNameOrder = no
                , snfValue = v
                , snfObligatory = obligatory
                , snfShouldBeFilledBySender = filledbysender
                , snfPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryNameField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))


instance FromJSValueWithUpdate SignatoryCompanyField where
    fromJSValueWithUpdate msf =  do
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True scfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False scfShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] scfPlacements <$> fromJSValueField "placements"
        case (value) of
          (Just v) -> do
              return $ Just $ CompanyField {
                  scfID = (maybe (unsafeSignatoryFieldID 0) scfID msf)
                , scfValue = v
                , scfObligatory = obligatory
                , scfShouldBeFilledBySender = filledbysender
                , scfPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryCompanyField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))

instance FromJSValueWithUpdate SignatoryPersonalNumberField where
    fromJSValueWithUpdate msf =  do
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True spnfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False spnfShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] spnfPlacements <$> fromJSValueField "placements"
        case (value) of
          (Just v) -> do
              return $ Just $ PersonalNumberField {
                  spnfID = (maybe (unsafeSignatoryFieldID 0) spnfID msf)
                , spnfValue = v
                , spnfObligatory = obligatory
                , spnfShouldBeFilledBySender = filledbysender
                , spnfPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryPersonalNumberField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))

instance FromJSValueWithUpdate SignatoryCompanyNumberField where
    fromJSValueWithUpdate msf =  do
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True scnfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False scnfShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] scnfPlacements <$> fromJSValueField "placements"
        case (value) of
          (Just v) -> do
              return $ Just $ CompanyNumberField {
                  scnfID = (maybe (unsafeSignatoryFieldID 0) scnfID msf)
                , scnfValue = v
                , scnfObligatory = obligatory
                , scnfShouldBeFilledBySender = filledbysender
                , scnfPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryCompanyNumberField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))

instance FromJSValueWithUpdate SignatoryEmailField where
    fromJSValueWithUpdate msf =  do
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True sefObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False sefShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] sefPlacements <$> fromJSValueField "placements"
        case (value) of
          (Just v) -> do
              return $ Just $ EmailField {
                  sefID = (maybe (unsafeSignatoryFieldID 0) sefID msf)
                , sefValue = strip v
                , sefObligatory = obligatory
                , sefShouldBeFilledBySender = filledbysender
                , sefPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryEmailField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))

instance FromJSValueWithUpdate SignatoryMobileField where
    fromJSValueWithUpdate msf =  do
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True smfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False smfShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] smfPlacements <$> fromJSValueField "placements"
        case (value) of
          (Just v) -> do
              return $ Just $ MobileField {
                  smfID = (maybe (unsafeSignatoryFieldID 0) smfID msf)
                , smfValue = v
                , smfObligatory = obligatory
                , smfShouldBeFilledBySender = filledbysender
                , smfPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryMobileField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))

instance FromJSValueWithUpdate SignatoryTextField where
    fromJSValueWithUpdate msf =  do
        fidentity<- fromJSValue
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True stfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False stfShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] stfPlacements <$> fromJSValueField "placements"
        case (fidentity,value) of
          (Just (TextFI n), Just v) -> do
              return $ Just $ TextField {
                  stfID = (maybe (unsafeSignatoryFieldID 0) stfID msf)
                , stfName = n
                , stfFilledByAuthor = not $ null $ v
                , stfValue = v
                , stfObligatory = obligatory
                , stfShouldBeFilledBySender = filledbysender
                , stfPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryTextField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))

instance FromJSValueWithUpdate SignatoryCheckboxField where
    fromJSValueWithUpdate msf =  do
        fidentity<- fromJSValue
        (value :: Maybe String)  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True schfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False schfShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] schfPlacements <$> fromJSValueField "placements"
        case (fidentity,value) of
          (Just (CheckboxFI n), Just v) -> do
              return $ Just $ CheckboxField {
                  schfID = (maybe (unsafeSignatoryFieldID 0) schfID msf)
                , schfName = n
                , schfValue = not $ null $ v
                , schfObligatory = obligatory
                , schfShouldBeFilledBySender = filledbysender
                , schfPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryCheckboxField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))

instance FromJSValueWithUpdate SignatorySignatureField where
    fromJSValueWithUpdate msf =  do
        fidentity<- fromJSValue
        (value :: Maybe String)  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True ssfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False ssfShouldBeFilledBySender <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] ssfPlacements <$> fromJSValueField "placements"
        case (fidentity,value) of
          (Just (SignatureFI n), Just _) -> do
              return $ Just $ SignatureField {
                  ssfID = (maybe (unsafeSignatoryFieldID 0) ssfID msf)
                , ssfName = n
                , ssfValue = Nothing -- We ignore value. Signature can't be provided in design view
                , ssfObligatory = obligatory
                , ssfShouldBeFilledBySender = filledbysender
                , ssfPlacements = placements
              }
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatorySignatureField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))

instance MatchWithJSValue SignatoryField where
    matchesWithJSValue sf = do
      ftype <- fromJSValue
      return (ftype ==  Just(fieldIdentity sf))

instance FromJSValue SignatoryAttachment where
    fromJSValue = do
        name<- fromJSValueField "name"
        description  <- fromJSValueField "description"
        case (name,description) of
             (Just n, Just d) -> return $ Just $ SignatoryAttachment {signatoryattachmentname  = n ,
                                                                      signatoryattachmentdescription = d,
                                                                      signatoryattachmentfile = Nothing}
             _ -> return Nothing

instance FromJSValue FieldIdentity where
   fromJSValue = do
    s <- fromJSValueField "name"
    t <- fromJSValueField "type"
    return $ case (fromMaybe "standard" t,s) of
         ("standard",  Just "fstname")    -> Just $ NameFI (NameOrder 1)
         ("standard",  Just "sndname")    -> Just $ NameFI (NameOrder 2)
         ("standard",  Just "email")      -> Just $ EmailFI
         ("standard",  Just "mobile")     -> Just $ MobileFI
         ("standard",  Just "sigpersnr")  -> Just $ PersonalNumberFI
         ("standard",  Just "sigco")      -> Just $ CompanyFI
         ("standard",  Just "sigcompnr")  -> Just $ CompanyNumberFI
         ("signature", Just name       )  -> Just $ SignatureFI name
         ("custom",    Just name       )  -> Just $ TextFI name
         ("checkbox",  Just name       )  -> Just $ CheckboxFI name
         _ -> Nothing


instance FromJSValue CSVUpload  where
    fromJSValue = do
        rows <- fromJSValue
        case rows of
             Just rs -> return $ Just $ CSVUpload
                        { csvtitle = ""
                        , csvcontents = rs
                        }
             _ -> return Nothing

instance FromJSValue DocumentTag where
    fromJSValue = do
        name   <- fromJSValueField "name"
        value  <- fromJSValueField "value"
        case (name, value) of
             (Just n, Just v) -> return $ Just $ DocumentTag n v
             _ -> return Nothing

instance FromJSValue Lang where
    fromJSValue = do
      j <- fromJSValue
      return $ case j of -- Due to documentation inconsistency we need to support gb and en for a while.
        Just "se"    -> Just LANG_SV
        Just "sv"    -> Just LANG_SV
        Just "en"    -> Just LANG_EN
        Just "gb"    -> Just LANG_EN
        Just l       -> langFromCode l
        _            -> Nothing


instance FromJSValueWithUpdate Document where
    fromJSValueWithUpdate mdoc = do
        title <- fromJSValueField "title"
        (invitationmessage :: Maybe (Maybe String)) <-  fromJSValueField "invitationmessage"
        (confirmationmessage :: Maybe (Maybe String)) <-  fromJSValueField "confirmationmessage"
        daystosign <- fromJSValueField "daystosign"
        daystoremind <- fromJSValueField "daystoremind"
        showheader <- fromJSValueField "showheader"
        showpdfdownload <- fromJSValueField "showpdfdownload"
        showrejectoption <- fromJSValueField "showrejectoption"
        showfooter <- fromJSValueField "showfooter"
        authentication <-  fromJSValueField "authentication"
        delivery <-  fromJSValueField "delivery"
        signatories <-  fromJSValueFieldCustom "signatories" (fromJSValueManyWithUpdate (fromMaybe [] $ documentsignatorylinks <$> mdoc))
        lang <- fromJSValueField "lang"
        mtimezone <- fromJSValueField "timezone"
        doctype <- fmap (\t -> if t then Template else Signable) <$> fromJSValueField "template"
        tags <- fromJSValueFieldCustom "tags" $ fromJSValueCustomMany  fromJSValue
        (apicallbackurl :: Maybe (Maybe String)) <- fromJSValueField "apicallbackurl"
        saved <- fromJSValueField "saved"
        authorattachments <- fromJSValueFieldCustom "authorattachments" $ fromJSValueCustomMany $ fmap (join . (fmap maybeRead)) $ (fromJSValueField "id")
        let daystosign'  = min 90 $ max 1 $ updateWithDefaultAndField 14 documentdaystosign daystosign
        let daystoremind' = min daystosign' <$> max 1 <$> updateWithDefaultAndField Nothing documentdaystoremind daystoremind

        return $ Just defaultValue {
            documenttitle = updateWithDefaultAndField "" documenttitle title,
            documentlang  = updateWithDefaultAndField LANG_SV documentlang lang,
            documentinvitetext = case (invitationmessage) of
                                     Nothing -> fromMaybe "" $ documentinvitetext <$> mdoc
                                     Just Nothing -> ""
                                     Just (Just s) -> fromMaybe "" (resultToMaybe $ asValidInviteText s),
            documentconfirmtext = case (confirmationmessage) of
                                     Nothing -> fromMaybe "" $ documentconfirmtext <$> mdoc
                                     Just Nothing -> ""
                                     Just (Just s) -> fromMaybe "" (resultToMaybe $ asValidInviteText s),
            documentdaystosign   = daystosign',
            documentdaystoremind = daystoremind',
            documentshowheader = updateWithDefaultAndField True documentshowheader showheader,
            documentshowpdfdownload = updateWithDefaultAndField True documentshowpdfdownload showpdfdownload,
            documentshowrejectoption = updateWithDefaultAndField True documentshowrejectoption showrejectoption,
            documentshowfooter = updateWithDefaultAndField True documentshowfooter showfooter,
            documentsignatorylinks = mapAuth authentication $ mapDL delivery $ updateWithDefaultAndField [] documentsignatorylinks signatories,
            documentauthorattachments = updateWithDefaultAndField [] documentauthorattachments (fmap AuthorAttachment <$> authorattachments),
            documenttags = updateWithDefaultAndField Set.empty documenttags (Set.fromList <$> tags),
            documenttype = updateWithDefaultAndField Signable documenttype doctype,
            documentapicallbackurl = updateWithDefaultAndField Nothing documentapicallbackurl apicallbackurl,
            documentunsaveddraft = updateWithDefaultAndField False documentunsaveddraft (fmap not saved),
            documenttimezonename = updateWithDefaultAndField defaultTimeZoneName documenttimezonename (unsafeTimeZoneName <$> mtimezone)
          }
      where
       updateWithDefaultAndField :: a -> (Document -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf mdoc))
       mapDL :: Maybe DeliveryMethod -> [SignatoryLink] -> [SignatoryLink]
       mapDL Nothing sls = sls
       mapDL (Just dl) sls = map (\sl -> sl {signatorylinkdeliverymethod = dl}) sls
       mapAuth :: Maybe AuthenticationMethod -> [SignatoryLink] -> [SignatoryLink]
       mapAuth Nothing sls = sls
       mapAuth (Just au) sls = map (\sl -> sl {signatorylinkauthenticationmethod = au}) sls
