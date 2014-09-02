{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.API.V1.DocumentFromJSON () where

import Doc.DocStateData
import Doc.SignatoryFieldID
import Data.Maybe
import User.Lang
import DB.TimeZoneName
import Text.JSON.FromJSValue
import qualified Data.Set as Set
import Utils.Read
import Control.Monad
import InputValidation
import Data.Functor
import Utils.Default
import Data.String.Utils (strip)
import Doc.SignatoryLinkID
import Control.Applicative
import Data.List
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
                  , signatoryfields        = nubBy (\f1 f2 -> sfType f1 == sfType f2) fields
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
        ftype <- fromJSValue
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True sfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False sfObligatory <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] sfPlacements <$> fromJSValueField "placements"
        case (ftype,value) of
          (Just ft, Just v) -> do
              let v' = case ft of
                        EmailFT -> strip v
                        SignatureFT _ -> ""
                        _ -> v
              return $ Just $ SignatoryField (maybe (unsafeSignatoryFieldID 0) sfID msf) ft v' obligatory filledbysender placements
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))


instance MatchWithJSValue SignatoryField where
    matchesWithJSValue sf = do
      ftype <- fromJSValue
      return (ftype ==  Just(sfType sf))

instance FromJSValue SignatoryAttachment where
    fromJSValue = do
        name<- fromJSValueField "name"
        description  <- fromJSValueField "description"
        case (name,description) of
             (Just n, Just d) -> return $ Just $ SignatoryAttachment {signatoryattachmentname  = n ,
                                                                      signatoryattachmentdescription = d,
                                                                      signatoryattachmentfile = Nothing}
             _ -> return Nothing

instance FromJSValue FieldType where
   fromJSValue = do
    s <- fromJSValueField "name"
    t <- fromJSValueField "type"
    filled <- (not . null) <$> fromMaybe ("" :: String) <$> fromJSValueField "value"
    return $ case (fromMaybe "standard" t,s) of
         ("standard",  Just "fstname")    -> Just $ FirstNameFT
         ("standard",  Just "sndname")    -> Just $ LastNameFT
         ("standard",  Just "email")      -> Just $ EmailFT
         ("standard",  Just "mobile")     -> Just $ MobileFT
         ("standard",  Just "sigpersnr")  -> Just $ PersonalNumberFT
         ("standard",  Just "sigco")      -> Just $ CompanyFT
         ("standard",  Just "sigcompnr")  -> Just $ CompanyNumberFT
         ("signature", Just name       )  -> Just $ SignatureFT name
         ("custom",    Just name       )  -> Just $ CustomFT name filled
         ("checkbox",  Just name       )  -> Just $ CheckboxFT name
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
