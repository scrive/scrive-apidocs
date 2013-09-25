{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
    Signatory TMP is an data structure used to represend signatory data
    for external comunication. It is build over standard signatory but most of stuff is hidden
    so it's quite easy to use it and be sure that a lot of important data stays there.
-}

module Doc.SignatoryTMP (
      SignatoryTMP -- do not expose attachments, there is a getter for that
    , maybeSignatoryTMPid
    , setSignatoryTMPid
    , fstname
    , setFstname
    , sndname
    , setSndname
    , company
    , setCompany
    , personalnumber
    , setPersonalnumber
    , companynumber
    , setCompanynumber
    , email
    , setEmail
    , mobile
    , setMobile
    , makeAuthor
    , makePartner
    , customField
    , setCustomField
    , signatoryDetails
    , setSignatoryDetails
    , toSignatoryDetails
    , isAuthorTMP
    , isSignatoryTMP
    , getAttachments
    , signatoryTMPIsChangingSignatoryLink
) where

import Control.Logic
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import Doc.DocStateData
import Doc.DocUtils
--import Data.Foldable hiding (concat, elem)
import Data.Maybe
import Utils.Monoid
import Utils.Prelude
import Text.JSON.FromJSValue
import Control.Applicative
import Data.String.Utils
import Doc.SignatoryLinkID

-- Structure definition + pointed
data SignatoryTMP = SignatoryTMP {
        mid :: Maybe SignatoryLinkID,
        details :: SignatoryDetails,
        attachments :: [SignatoryAttachment], -- Do not expose it as durring runtime this does not have email set
        csvupload :: Maybe CSVUpload,
        signredirecturl :: Maybe String,
        rejectredirecturl :: Maybe String,
        authentication :: AuthenticationMethod,
        delivery :: DeliveryMethod
    } deriving (Show,Eq)


instance Ord SignatoryTMP where
  compare s1 s2 | isAuthorTMP s1 = LT
                | isAuthorTMP s2 = GT
                | mid s2 == Nothing = LT
                | mid s1 == Nothing = GT
                | otherwise = compare (mid s1) (mid s2)

instance HasFields SignatoryTMP where
    replaceField f s = s {details = replaceField f (details s)}
    getAllFields = getAllFields . details

emptySignatoryTMP :: SignatoryTMP
emptySignatoryTMP = SignatoryTMP {
    mid = Nothing,
    details = SignatoryDetails {
      signatorysignorder = SignOrder 1
    , signatoryfields   = []
    , signatoryispartner = False
    , signatoryisauthor = False
    }
  , attachments = []
  , csvupload = Nothing
  , signredirecturl = Nothing
  , rejectredirecturl = Nothing
  , authentication = StandardAuthentication
  , delivery = EmailDelivery
  }

-- Basic operations

liftTMP ::  (SignatoryDetails -> SignatoryDetails) -> SignatoryTMP -> SignatoryTMP
liftTMP f s = s {details  =  f $ details s}

maybeSignatoryTMPid :: SignatoryTMP -> Maybe SignatoryLinkID
maybeSignatoryTMPid = mid

setSignatoryTMPid :: SignatoryLinkID -> SignatoryTMP -> SignatoryTMP
setSignatoryTMPid sid stmp = stmp {mid = Just sid}

fstname:: SignatoryTMP -> Maybe String
fstname = nothingIfEmpty . getFirstName . details

setFstname:: String -> SignatoryTMP -> SignatoryTMP
setFstname = replaceFieldValue FirstNameFT

sndname::SignatoryTMP -> Maybe String
sndname = nothingIfEmpty . getLastName . details

setSndname:: String -> SignatoryTMP -> SignatoryTMP
setSndname = replaceFieldValue LastNameFT

company::SignatoryTMP -> Maybe String
company = nothingIfEmpty . getCompanyName . details

setCompany:: String -> SignatoryTMP -> SignatoryTMP
setCompany = replaceFieldValue CompanyFT

mobile::SignatoryTMP -> Maybe String
mobile = nothingIfEmpty . getMobile . details

setMobile:: String -> SignatoryTMP -> SignatoryTMP
setMobile = replaceFieldValue MobileFT

personalnumber::SignatoryTMP -> Maybe String
personalnumber = nothingIfEmpty . getPersonalNumber . details

setPersonalnumber:: String -> SignatoryTMP -> SignatoryTMP
setPersonalnumber =  replaceFieldValue PersonalNumberFT

companynumber::SignatoryTMP -> Maybe String
companynumber = nothingIfEmpty . getCompanyNumber . details

setCompanynumber:: String -> SignatoryTMP -> SignatoryTMP
setCompanynumber =  replaceFieldValue CompanyNumberFT

customField :: String  -> SignatoryTMP -> Maybe String
customField name = (fmap sfValue) . (findCustomField name) . details

setCustomField :: String -> String -> SignatoryTMP -> SignatoryTMP
setCustomField name = replaceFieldValue (CustomFT name False)

email::SignatoryTMP -> Maybe String
email = nothingIfEmpty . getEmail . details

setEmail:: String -> SignatoryTMP -> SignatoryTMP
setEmail =  replaceFieldValue EmailFT

setCSV :: (Maybe CSVUpload) -> SignatoryTMP -> SignatoryTMP
setCSV mcsv s = s {csvupload = mcsv}

makeAuthor :: SignatoryTMP -> SignatoryTMP
makeAuthor s =  s {details = (details s) { signatoryisauthor = True }}

makePartner :: SignatoryTMP -> SignatoryTMP
makePartner s = s {details = (details s) { signatoryispartner = True }}

isAuthorTMP :: SignatoryTMP -> Bool
isAuthorTMP = signatoryisauthor . details

isSignatoryTMP :: SignatoryTMP -> Bool
isSignatoryTMP = signatoryispartner . details

setSignOrder :: SignOrder -> SignatoryTMP -> SignatoryTMP
setSignOrder i =  liftTMP $  \s -> s {signatorysignorder = i}

signatoryDetails :: SignatoryTMP -> SignatoryDetails
signatoryDetails = details

setSignatoryDetails :: SignatoryDetails -> SignatoryTMP -> SignatoryTMP
setSignatoryDetails d = liftTMP $  \_ -> d

--signOrder :: SignatoryTMP -> SignOrder
--signOrder = signatorysignorder . details

addAttachment :: SignatoryAttachment -> SignatoryTMP -> SignatoryTMP
addAttachment a s = if (signatoryattachmentname a `elem `(signatoryattachmentname <$> attachments s))
                       then s
                       else s {attachments = a : (attachments s)}

getAttachments :: SignatoryTMP -> [SignatoryAttachment]
getAttachments s = attachments s

setSignredirecturl :: Maybe String -> SignatoryTMP -> SignatoryTMP
setSignredirecturl rurl s = s {signredirecturl = rurl}

setRejectredirecturl :: Maybe String -> SignatoryTMP -> SignatoryTMP
setRejectredirecturl rurl s = s {rejectredirecturl = rurl}

 -- To SignatoryLink or SignatoryDetails conversion
toSignatoryDetails :: SignatoryTMP -> (Maybe SignatoryLinkID, SignatoryDetails, [SignatoryAttachment], Maybe CSVUpload,Maybe String, Maybe String, AuthenticationMethod, DeliveryMethod)
toSignatoryDetails sTMP  =
  ( maybeSignatoryTMPid sTMP
  , details sTMP
  , attachments $ sTMP
  , csvupload $ sTMP
  , signredirecturl $ sTMP
  , rejectredirecturl $ sTMP
  , authentication $ sTMP
  , delivery $ sTMP)

instance FromJSValue SignatoryTMP where
    fromJSValue = do
        mid'    <- fromJSValueField "id"
        author <- fromJSValueField "author"
        signs  <- fromJSValueField "signs"
        mfields <- fromJSValueField "fields"
        signorder <- fromJSValueField "signorder"
        attachments <- fromMaybe [] <$> fromJSValueField "attachments"
        csv <- fromJSValueField "csv"
        sredirecturl <- fromJSValueField "signsuccessredirect"
        rredirecturl <- fromJSValueField "rejectredirect"
        authentication' <-  fromJSValueField "authentication"
        delivery' <-  fromJSValueField "delivery"
        case (mfields) of
          (Just fields) ->
            return $ Just $
                (setSignOrder (SignOrder $ fromMaybe 1 signorder)) $
                (makeAuthor  <| joinB author |> id) $
                (makePartner <| joinB signs  |> id) $
                (setCSV $ csv) $
                (setSignredirecturl $ sredirecturl) $
                (setRejectredirecturl $ rredirecturl) $
                (map replaceField fields) $^^
                (map addAttachment attachments) $^^
                (SignatoryTMP {
                    mid = maybeRead =<< mid'
                  , details = SignatoryDetails {
                      signatorysignorder = SignOrder 1
                    , signatoryfields   = []
                    , signatoryispartner = False
                    , signatoryisauthor = False
                    }
                  , attachments = []
                  , csvupload = Nothing
                  , signredirecturl = Nothing
                  , rejectredirecturl = Nothing
                  , authentication = fromMaybe StandardAuthentication authentication'
                  , delivery = fromMaybe EmailDelivery delivery'
                  })
          _ -> return Nothing

instance FromJSValue SignatoryField where
    fromJSValue = do
        ftype <- fromJSValue -- We read field type at this from two different fields, so we can't use fromJSValueField
        value  <- fromJSValueField "value"
        obligatory <- fromMaybe True <$> fromJSValueField "obligatory"
        filledbysender <- fromMaybe False <$> fromJSValueField "shouldbefilledbysender"
        placements <- fromMaybe [] <$> fromJSValueField "placements"
        case (ftype,value) of
          (Just ft, Just v) -> do
              return $ Just $ SignatoryField ft (v <| ft /= EmailFT|> strip v) obligatory filledbysender placements
          _ -> return Nothing


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



signatoryTMPIsChangingSignatoryLink :: SignatoryTMP -> SignatoryLink -> Bool
signatoryTMPIsChangingSignatoryLink SignatoryTMP{..} SignatoryLink{..} =
        (details /= signatorydetails)
     || (attachments /= signatoryattachments)
     || (csvupload /= signatorylinkcsvupload)
     || (signredirecturl /= signatorylinksignredirecturl)
     || (rejectredirecturl /= signatorylinkrejectredirecturl)
     || (authentication /= signatorylinkauthenticationmethod)
     || (delivery /= signatorylinkdeliverymethod)
