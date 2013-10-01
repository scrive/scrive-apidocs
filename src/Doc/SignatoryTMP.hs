{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
    Signatory TMP is an data structure used to represend signatory data
    for external comunication. It is build over standard signatory but most of stuff is hidden
    so it's quite easy to use it and be sure that a lot of important data stays there.
-}

module Doc.SignatoryTMP (
      toSignatoryDetails
) where

import Doc.DocStateData
import Data.Maybe
import Control.Monad
import Text.JSON.FromJSValue
import Control.Applicative
import Data.String.Utils
import Doc.SignatoryLinkID
import Utils.Default


 -- To SignatoryLink or SignatoryDetails conversion
toSignatoryDetails :: SignatoryLink -> (Maybe SignatoryLinkID, SignatoryDetails, [SignatoryAttachment], Maybe CSVUpload,Maybe String, Maybe String, AuthenticationMethod, DeliveryMethod)
toSignatoryDetails sig  =
  (  Just $ signatorylinkid sig
   , signatorydetails sig
   , signatoryattachments  sig
   , signatorylinkcsvupload  sig
   , signatorylinksignredirecturl  sig
   , signatorylinkrejectredirecturl sig
   , signatorylinkauthenticationmethod  sig
   , signatorylinkdeliverymethod  sig
  )

instance MatchWithJSValue SignatoryLink where
    matchesWithJSValue s = do
      mid    <- fromJSValueField "id"
      return (Just (signatorylinkid s) == (maybeRead =<< mid))

instance FromJSValueWithUpdate SignatoryLink where
    fromJSValueWithUpdate ms = do
        author <- fromJSValueField "author"
        signs  <- fromJSValueField "signs"
        mfields <- fromJSValueFieldCustom "fields" (fromJSValueManyWithUpdate $ fromMaybe [] (signatoryfields <$> signatorydetails <$> ms))
        signorder <- fromJSValueField "signorder"
        attachments <- fromJSValueField "attachments"
        csv <- fromJSValueField "csv"
        sredirecturl <- fromJSValueField "signsuccessredirect"
        rredirecturl <- fromJSValueField "rejectredirect"
        authentication' <-  fromJSValueField "authentication"
        delivery' <-  fromJSValueField "delivery"
        case (mfields) of
             (Just fields) -> return $ Just $ defaultValue {
                    signatorylinkid            = fromMaybe (unsafeSignatoryLinkID 0) (signatorylinkid <$> ms)
                  , signatorydetails           = defaultValue {
                        signatorysignorder     = updateWithDefaultAndField (SignOrder 1) (signatorysignorder . signatorydetails) (SignOrder <$> signorder)
                      , signatoryfields        = fields
                      , signatoryisauthor      = updateWithDefaultAndField False (signatoryisauthor . signatorydetails) author
                      , signatoryispartner     = updateWithDefaultAndField False (signatoryispartner . signatorydetails) signs
                                                 }
                  , signatorylinkcsvupload       = updateWithDefaultAndField Nothing signatorylinkcsvupload csv
                  , signatoryattachments         = updateWithDefaultAndField [] signatoryattachments attachments
                  , signatorylinksignredirecturl = updateWithDefaultAndField Nothing signatorylinksignredirecturl sredirecturl
                  , signatorylinkrejectredirecturl = updateWithDefaultAndField Nothing signatorylinkrejectredirecturl rredirecturl
                  , signatorylinkauthenticationmethod = updateWithDefaultAndField StandardAuthentication signatorylinkauthenticationmethod authentication'
                  , signatorylinkdeliverymethod       = updateWithDefaultAndField EmailDelivery signatorylinkdeliverymethod delivery'
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
              return $ Just $ SignatoryField ft v' obligatory filledbysender placements
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
