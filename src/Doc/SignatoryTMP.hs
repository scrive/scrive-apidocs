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
    , makeSigns
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
import Misc
import Data.List
import Doc.DocUtils
import Data.Foldable hiding (concat, elem)
import Util.JSON
import Data.Maybe
import Control.Monad

-- Structure definition + pointed
data SignatoryTMP = SignatoryTMP {
        details :: SignatoryDetails,
        roles   :: [SignatoryRole],
        attachments :: [SignatoryAttachment], -- Do not expose it as durring runtime this does not have email set
        csvupload :: Maybe CSVUpload
    } deriving (Show)

instance HasFields SignatoryTMP where
    replaceField f s = s {details = replaceField f (details s)}
    getAllFields = getAllFields . details
    
emptySignatoryTMP :: SignatoryTMP 
emptySignatoryTMP = SignatoryTMP 
                { 
                  details         = SignatoryDetails
                                                { signatorysignorder = SignOrder 1
                                                 , signatoryfields   = []
                                                }
                , roles  = []
                , attachments = []
                , csvupload = Nothing
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
makeAuthor s =  s {roles = roles s `union` [SignatoryAuthor]}
    
makeSigns :: SignatoryTMP -> SignatoryTMP
makeSigns s =  s {roles = roles s `union` [SignatoryPartner]}

isAuthorTMP :: SignatoryTMP -> Bool
isAuthorTMP s = SignatoryAuthor `elem` (roles s)

isSignatoryTMP :: SignatoryTMP -> Bool
isSignatoryTMP s = SignatoryPartner `elem` (roles s)

setSignOrder :: SignOrder -> SignatoryTMP -> SignatoryTMP
setSignOrder i =  liftTMP $  \s -> s {signatorysignorder = i}

signOrder :: SignatoryTMP -> SignOrder
signOrder = signatorysignorder . details

addAttachment :: SignatoryAttachment -> SignatoryTMP -> SignatoryTMP
addAttachment a s = s {attachments = a : (attachments s)}

getAttachments :: SignatoryTMP -> [SignatoryAttachment]
getAttachments s = attachments s


toSignatoryDetails1 :: SignatoryTMP -> (SignatoryDetails,[SignatoryRole])
toSignatoryDetails1 sTMP = (\(x,y,_,_) ->(x,y)) (toSignatoryDetails2 sTMP)
-- To SignatoryLink or SignatoryDetails conversion
toSignatoryDetails2 :: SignatoryTMP -> (SignatoryDetails,[SignatoryRole], [SignatoryAttachment], Maybe CSVUpload)
toSignatoryDetails2 sTMP  = 
    let sig = makeSignatory [] [] ""
                 (fold $ fstname sTMP)
                 (fold $ sndname sTMP)
                 (fold $ email sTMP)
                 (signOrder sTMP)
                 (fold $ company sTMP)
                 (fold $ personalnumber sTMP)
                 (fold $ companynumber sTMP)
    in withRolesAndAttsAndCSV $ sig { 
            signatoryfields =  mergeFields (getAllFields sTMP)  (signatoryfields sig),
            signatorysignorder = signatorysignorder $ details sTMP }
  where
   mergeFields [] l = l
   mergeFields (f:fs) l = mergeFields fs (replaceField f l)
   withRolesAndAttsAndCSV x = (x,roles $ sTMP, attachments $ sTMP, csvupload $ sTMP)
   

instance FromJSON SignatoryTMP where
    fromJSON = do
        author <- fromJSONField "author"
        signs  <- fromJSONField "signs" 
        fields <- fromJSONField "fields"
        signorder <- fromJSONField "signorder"
        attachments <- fromJSONField "attachments"
        csv <- fromJSONField "csv"

        return $ Just $
            (setSignOrder (SignOrder $ fromMaybe 1 signorder)) $
            (makeAuthor  <| joinB author |> id) $
            (makeSigns   <| joinB signs  |> id) $
            (setCSV $ csv) $ 
            (map replaceField $ concat $ maybeToList fields) $^^
            (map addAttachment $ concat $ maybeToList attachments) $^^
            emptySignatoryTMP
            
instance FromJSON SignatoryField where
    fromJSON = do
        ftype <- fromJSONField "name"
        value  <- fromJSONField "value" 
        placements <- fromJSONField "placements" 
        case (ftype,value) of 
          (Just ft, Just v) -> do
              let fixFT (CustomFT name _)= CustomFT name (not $ null v)
                  fixFT t = t
              return $ Just $ SignatoryField (fixFT ft) v (concat $ maybeToList placements)
          _ -> return Nothing
        

instance FromJSON SignatoryAttachment where
    fromJSON = do
        name<- fromJSONField "name"
        description  <- fromJSONField "description"
        case (name,description) of
             (Just n, Just d) -> return $ Just $ SignatoryAttachment {signatoryattachmentname  = n ,
                                                                      signatoryattachmentdescription = d,
                                                                      signatoryattachmentfile = Nothing}
             _ -> return Nothing  

instance FromJSON FieldType where
   fromJSON = do 
    s <- fromJSON
    return $ case s of
         Just "fstname"   -> Just $ FirstNameFT
         Just "sndname"   -> Just $ LastNameFT
         Just "email"     -> Just $ EmailFT
         Just "sigpersnr" -> Just $ PersonalNumberFT
         Just "sigco"     -> Just $ CompanyFT
         Just "sigcompnr" -> Just $ CompanyNumberFT
         Just "signature" -> Just $ SignatureFT
         Just name        -> Just $ CustomFT  name False
         _ -> Nothing

instance FromJSON FieldPlacement where
    fromJSON = do
         x          <- fromJSONField "x"
         y          <- fromJSONField "y"  
         page       <- fromJSONField "page"
         pagewidth  <- fromJSONField "pagewidth"
         pageheight <- fromJSONField "pageheight"
         return $ liftM5 FieldPlacement x y page pagewidth pageheight

instance FromJSON CSVUpload  where
    fromJSON = do
        rows <- fromJSON
        case rows of
             Just rs -> return $ Just $ CSVUpload {
                    csvtitle = ""
                    , csvcontents = rs
                    , csvsignatoryindex = 0}
             _ -> return Nothing
