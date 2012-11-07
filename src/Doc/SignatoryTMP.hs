{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- 
    Signatory TMP is an data structure used to represend signatory data
    for external comunication. It is build over standard signatory but most of stuff is hidden 
    so it's quite easy to use it and be sure that a lot of important data stays there.
-}

module Doc.SignatoryTMP (
      SignatoryTMP -- do not expose attachments, there is a getter for that
    , emptySignatoryTMP
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
    , makeAuthor
    , makePartner
    , customField
    , setCustomField
    , toSignatoryDetails1
    , toSignatoryDetails2
    , isAuthorTMP
    , isSignatoryTMP
    , getAttachments
    
) where

import Control.Logic
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo
import Doc.DocStateData
import Doc.DocUtils
import Data.Foldable hiding (concat, elem)
import Data.Maybe
import Utils.Monoid
import Utils.Prelude
import Text.JSON.FromJSValue
import Control.Applicative

-- Structure definition + pointed
data SignatoryTMP = SignatoryTMP {
        details :: SignatoryDetails,
        attachments :: [SignatoryAttachment], -- Do not expose it as durring runtime this does not have email set
        csvupload :: Maybe CSVUpload,
        signredirecturl :: Maybe String
    } deriving (Show)

instance HasFields SignatoryTMP where
    replaceField f s = s {details = replaceField f (details s)}
    getAllFields = getAllFields . details
    
emptySignatoryTMP :: SignatoryTMP 
emptySignatoryTMP = SignatoryTMP {
    details = SignatoryDetails {
      signatorysignorder = SignOrder 1
    , signatoryfields   = []
    , signatoryispartner = False
    , signatoryisauthor = False
    }
  , attachments = []
  , csvupload = Nothing
  , signredirecturl = Nothing
  }

-- Basic operations

liftTMP ::  (SignatoryDetails -> SignatoryDetails) -> SignatoryTMP -> SignatoryTMP
liftTMP f s = s {details  =  f $ details s}

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

signOrder :: SignatoryTMP -> SignOrder
signOrder = signatorysignorder . details

addAttachment :: SignatoryAttachment -> SignatoryTMP -> SignatoryTMP
addAttachment a s = s {attachments = a : (attachments s)}

getAttachments :: SignatoryTMP -> [SignatoryAttachment]
getAttachments s = attachments s

setSignredirecturl :: Maybe String -> SignatoryTMP -> SignatoryTMP
setSignredirecturl rurl s = s {signredirecturl = rurl}

toSignatoryDetails1 :: SignatoryTMP -> SignatoryDetails
toSignatoryDetails1 sTMP = (\(x,_,_,_) -> x) (toSignatoryDetails2 sTMP)
-- To SignatoryLink or SignatoryDetails conversion
toSignatoryDetails2 :: SignatoryTMP -> (SignatoryDetails, [SignatoryAttachment], Maybe CSVUpload, Maybe String)
toSignatoryDetails2 sTMP  = 
    let sig = makeSignatory [] [] ""
                 (fold $ fstname sTMP)
                 (fold $ sndname sTMP)
                 (fold $ email sTMP)
                 (signOrder sTMP)
                 (signatoryisauthor $ details sTMP)
                 (signatoryispartner $ details sTMP)
                 (fold $ company sTMP)
                 (fold $ personalnumber sTMP)
                 (fold $ companynumber sTMP)
    in withRolesAndAttsAndCSV $ sig { 
            signatoryfields =  mergeFields (getAllFields sTMP)  (signatoryfields sig),
            signatorysignorder = signatorysignorder $ details sTMP }
  where
   mergeFields [] l = l
   mergeFields (f:fs) l = mergeFields fs (replaceField f l)
   withRolesAndAttsAndCSV x = (x, attachments $ sTMP, csvupload $ sTMP, signredirecturl $ sTMP)
   
instance FromJSValue SignatoryTMP where
    fromJSValue = do
        author <- fromJSValueField "author"
        signs  <- fromJSValueField "signs"
        mfields <- fromJSValueField "fields"
        signorder <- fromJSValueField "signorder"
        mattachments <- fromJSValueField "attachments"
        csv <- fromJSValueField "csv"
        sredirecturl <- fromJSValueField "signsuccessredirect"
        case (mfields, mattachments) of
          (Just fields, Just attachments) -> 
            return $ Just $
                (setSignOrder (SignOrder $ fromMaybe 1 signorder)) $
                (makeAuthor  <| joinB author |> id) $
                (makePartner <| joinB signs  |> id) $
                (setCSV $ csv) $
                (setSignredirecturl $ sredirecturl) $
                (map replaceField fields) $^^
                (map addAttachment attachments) $^^
                emptySignatoryTMP
          _ -> return Nothing
            
instance FromJSValue SignatoryField where
    fromJSValue = do
        ftype <- fromJSValue -- We read field type at this from two different fields, so we can't use fromJSValueField
        value  <- fromJSValueField "value"
        mplacements <- fromJSValueField "placements"
        case (ftype,value,mplacements) of 
          (Just ft, Just v, Just placements) -> do
              return $ Just $ SignatoryField ft v placements
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
         ("standard",            Just "fstname")    -> Just $ FirstNameFT
         ("standard",            Just "sndname")    -> Just $ LastNameFT
         ("standard",            Just "email")      -> Just $ EmailFT
         ("standard",            Just "sigpersnr")  -> Just $ PersonalNumberFT
         ("standard",            Just "sigco")      -> Just $ CompanyFT
         ("standard",            Just "sigcompnr")  -> Just $ CompanyNumberFT
         ("signature",           Just "signature")  -> Just $ SignatureFT
         ("custom",              Just name       )  -> Just $ CustomFT  name filled
         ("checkbox-optional",   Just name       )  -> Just $ CheckboxOptionalFT  name 
         ("checkbox-obligatory", Just name       )  -> Just $ CheckboxObligatoryFT  name 
         _ -> Nothing

         
instance FromJSValue CSVUpload  where
    fromJSValue = do
        rows <- fromJSValue
        case rows of
             Just rs -> return $ Just $ CSVUpload {
                    csvtitle = ""
                    , csvcontents = rs
                    , csvsignatoryindex = 0}
             _ -> return Nothing
