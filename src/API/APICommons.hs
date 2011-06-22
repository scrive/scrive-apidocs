{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -XTupleSections#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  API.APICommons
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Constans and data structures used by many other API's
-- Some low level helpers.
--
-- !! Constants described here should be used between all API's
-- There are constants for stuff like document type, document state, to say thet a signatory is viewer etc.
-- Don't invent them yourself. Don't return a 'human readable' string. Use stuff from here.
--
-- !! JSON priners (like the one api_documents) should be shared as much as posible
--
-- Also if there is a common reader for more then one api 
-- , like for files or signatories it should be put here
-----------------------------------------------------------------------------
module API.APICommons (
            api_document
          , SignatoryTMP(..)
          , getSignatoryTMP
          , mergeSignatoryWithTMP
          , toSignatoryDetails
          , fillFields
          , toDocumentType
          , getFiles
        ) where


import Doc.DocState
import Text.JSON
import MinutesTime
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified  Codec.Binary.Base64 as BASE64
import API.API
import Doc.DocStorage
import Doc.DocControl
import Control.Monad.Trans
import Misc
import Data.Maybe
import Data.Foldable (fold)
import Data.Functor
import Control.Monad


{- -}

data DOCUMENT_AUTHORISATION = 
      DOCUMENT_AUTHORISATION_BASIC_EMAIL 
    | DOCUMENT_AUTHORISATION_BASIC_ELEG
     deriving (Bounded,Ord,Eq)
    
    
instance SafeEnum DOCUMENT_AUTHORISATION where
    fromSafeEnum DOCUMENT_AUTHORISATION_BASIC_EMAIL = 1 
    fromSafeEnum DOCUMENT_AUTHORISATION_BASIC_ELEG = 10
    toSafeEnum 1 =  Just DOCUMENT_AUTHORISATION_BASIC_EMAIL
    toSafeEnum 10 = Just DOCUMENT_AUTHORISATION_BASIC_ELEG
    toSafeEnum _ = Nothing
    
    
data DOCUMENT_RELATION = 
      DOCUMENT_RELATION_AUTHOR_SECRETARY
    | DOCUMENT_RELATION_AUTHOR_SIGNATORY
    | DOCUMENT_RELATION_SIGNATORY
    | DOCUMENT_RELATION_VIEWER
    | DOCUMENT_RELATION_OTHER
      
instance SafeEnum DOCUMENT_RELATION where
    fromSafeEnum DOCUMENT_RELATION_AUTHOR_SECRETARY = 1
    fromSafeEnum DOCUMENT_RELATION_AUTHOR_SIGNATORY = 2
    fromSafeEnum DOCUMENT_RELATION_SIGNATORY = 5
    fromSafeEnum DOCUMENT_RELATION_VIEWER = 10
    fromSafeEnum DOCUMENT_RELATION_OTHER = 20
    toSafeEnum _ = Nothing
    
data DOCUMENT_STATUS = 
      DOCUMENT_STATUS_PREPARATION
    | DOCUMENT_STATUS_PENDING
    | DOCUMENT_STATUS_STOPED
    | DOCUMENT_STATUS_CLOSED
    | DOCUMENT_STATUS_ERROR
    
instance SafeEnum DOCUMENT_STATUS where
    fromSafeEnum DOCUMENT_STATUS_PREPARATION = 0
    fromSafeEnum DOCUMENT_STATUS_PENDING = 10
    fromSafeEnum DOCUMENT_STATUS_STOPED = 20
    fromSafeEnum DOCUMENT_STATUS_CLOSED = 30
    fromSafeEnum DOCUMENT_STATUS_ERROR = 40
    toSafeEnum _ = Nothing

data DOCUMENT_TYPE = 
      DOCUMENT_TYPE_CONTRACT
    | DOCUMENT_TYPE_OFFER
    | DOCUMENT_TYPE_CONTRACT_TEMPLATE
    | DOCUMENT_TYPE_OFFER_TEMPLATE
    deriving (Bounded,Ord,Eq)

instance SafeEnum DOCUMENT_TYPE where
    fromSafeEnum DOCUMENT_TYPE_CONTRACT = 1
    fromSafeEnum DOCUMENT_TYPE_OFFER = 2
    fromSafeEnum DOCUMENT_TYPE_CONTRACT_TEMPLATE = 3
    fromSafeEnum DOCUMENT_TYPE_OFFER_TEMPLATE = 4
    toSafeEnum 1 = Just DOCUMENT_TYPE_CONTRACT
    toSafeEnum 2 = Just DOCUMENT_TYPE_OFFER
    toSafeEnum 3 = Just DOCUMENT_TYPE_CONTRACT_TEMPLATE
    toSafeEnum 4 = Just DOCUMENT_TYPE_OFFER_TEMPLATE
    toSafeEnum _ = Nothing
    
toDocumentType::DOCUMENT_TYPE -> DocumentType    
toDocumentType DOCUMENT_TYPE_CONTRACT = Signable Contract
toDocumentType DOCUMENT_TYPE_OFFER = Signable Offer
toDocumentType DOCUMENT_TYPE_CONTRACT_TEMPLATE = Template Contract
toDocumentType DOCUMENT_TYPE_OFFER_TEMPLATE = Template Offer

{- Building JSON structure representing object in any API response 
   TODO: Something is WRONG FOR ATTACHMENTS HERE
-} 
api_document_type::Document ->  DOCUMENT_TYPE
api_document_type doc   
    | Template Contract == documenttype doc  = DOCUMENT_TYPE_CONTRACT_TEMPLATE
    | Template Offer == documenttype doc = DOCUMENT_TYPE_OFFER_TEMPLATE
    | Signable Contract == documenttype doc = DOCUMENT_TYPE_CONTRACT
    | Signable Offer == documenttype doc = DOCUMENT_TYPE_OFFER
    | otherwise = error "Not matching type" -- TO DO WITH NEXT INTEGRATION API FIXES
    
    
api_document_status::Document -> DOCUMENT_STATUS 
api_document_status doc = 
    case (documentstatus doc) of
         Preparation        -> DOCUMENT_STATUS_PREPARATION
         Pending            -> DOCUMENT_STATUS_PENDING
         AwaitingAuthor     -> DOCUMENT_STATUS_PENDING
         Closed             -> DOCUMENT_STATUS_CLOSED
         Timedout           -> DOCUMENT_STATUS_STOPED
         Rejected           -> DOCUMENT_STATUS_STOPED
         Canceled           -> DOCUMENT_STATUS_STOPED
         DocumentError _    -> DOCUMENT_STATUS_ERROR
                               
api_document_authorisation::Document -> DOCUMENT_AUTHORISATION
api_document_authorisation doc 
    | (ELegitimationIdentification `elem` documentallowedidtypes doc) = DOCUMENT_AUTHORISATION_BASIC_ELEG
    | otherwise = DOCUMENT_AUTHORISATION_BASIC_EMAIL
    
api_document_relation::SignatoryLink -> DOCUMENT_RELATION
api_document_relation sl 
    | (SignatoryAuthor `elem` signatoryroles sl && SignatoryPartner `elem` signatoryroles sl) =  DOCUMENT_RELATION_AUTHOR_SIGNATORY
    | (SignatoryAuthor `elem` signatoryroles sl) = DOCUMENT_RELATION_AUTHOR_SECRETARY
    | (SignatoryPartner `elem` signatoryroles sl) = DOCUMENT_RELATION_SIGNATORY
    | otherwise =  DOCUMENT_RELATION_VIEWER
    
api_signatory::SignatoryLink -> JSValue
api_signatory sl = JSObject $ toJSObject $  [ 
      ("email", showJSON  $ BS.toString $ signatoryemail $ signatorydetails sl)
    , ("fstname", showJSON  $ BS.toString $ signatoryfstname $ signatorydetails sl)
    , ("sndname", showJSON  $ BS.toString $ signatorysndname $ signatorydetails sl)
    , ("personalnr", showJSON $ BS.toString $ signatorypersonalnumber $ signatorydetails sl)
    , ("company", showJSON $ BS.toString $ signatorycompany $ signatorydetails sl)
    , ("companynr", showJSON  $ BS.toString $ signatorycompanynumber $ signatorydetails sl)
    ]
    ++ 
    case (maybeseeninfo sl) of 
     Just seeninfo ->  [("seen", api_date $ signtime seeninfo)]
     Nothing -> []
    ++ 
    case (maybesigninfo sl) of 
     Just signinfo ->  [("sign", api_date $ signtime signinfo)]
     Nothing -> []
    ++ 
    [("relation",showJSON $ fromSafeEnum $ api_document_relation sl)]  
    ++ 
    [("fields", JSArray $ for (signatoryotherfields $ signatorydetails sl) $ \fd -> 
                    JSObject $ toJSObject $ [ ("name", JSString $ toJSString $ BS.toString $ fieldlabel fd)
                                            , ("value", JSString $ toJSString $ BS.toString $ fieldvalue fd)]
    )]
    
api_document_tag::DocumentTag -> JSValue
api_document_tag tag = JSObject $ toJSObject $ [
      ("name", showJSON $ BS.toString $ tagname tag)
    , ("value", showJSON $ BS.toString $ tagvalue tag)]

api_document_file::(APIContext c) => File -> APIFunction c JSValue
api_document_file file = do
    ctx <- askKontraContext 
    content <- liftIO $ getFileContents ctx file
    let base64data = BASE64.encode (BS.unpack content) 
    return $ JSObject $ toJSObject $ [
          ("name", showJSON $ BS.toString $ filename file)
        , ("content", showJSON $ base64data)]
    
    
api_document::(APIContext c) => Bool -> Document -> APIFunction c JSValue
api_document addFiles doc = do
    files <- if addFiles
              then do           
              files <- sequence $ map api_document_file $ 
                case (documentstatus doc) of 
                    Closed -> documentsealedfiles doc
                    _ -> documentfiles doc
              return $ [("files",JSArray files)] 
              else return [] 
    return $ JSObject $ toJSObject $  [ 
       ("document_id", showJSON  $ show $ unDocumentID $ documentid doc)
     , ("title", showJSON  $ BS.toString $ documenttitle doc)
     , ("type", showJSON  $ fromSafeEnum $ api_document_type doc)
     , ("state", showJSON  $ fromSafeEnum $ api_document_status doc)
     , ("involved", JSArray $ map api_signatory $ documentsignatorylinks doc)
     , ("tags", JSArray $ map api_document_tag $ documenttags doc)
     , ("authorization", showJSON  $ fromSafeEnum $ api_document_authorisation doc)
     , ("mdate", api_date $ documentmtime doc)
     ]    
     ++ files
    
       
api_date :: MinutesTime -> JSValue 
api_date = showJSON  . showMinutesTimeForAPI


data SignatoryTMP = SignatoryTMP {
                fstname::Maybe BS.ByteString,
                sndname::Maybe BS.ByteString,
                company::Maybe BS.ByteString,
                personalnumber::Maybe BS.ByteString,
                companynumber::Maybe BS.ByteString,
                email::Maybe BS.ByteString,
                fields :: [(BS.ByteString,Maybe BS.ByteString)]    
            } deriving Show      

getSignatoryTMP::(APIContext c) => APIFunction c (Maybe SignatoryTMP)
getSignatoryTMP = do 
    fstname <- apiAskBS "fstname"
    sndname <- apiAskBS "sndname"
    company <- apiAskBS "company"
    personalnumber <- apiAskBS "personalnumber"
    companynumber <- apiAskBS "companynumber"
    email <- apiAskBS "email"
    fields <- apiLocal "fields" $ apiMapLocal $ do
                                        name <- apiAskBS "name"
                                        value <- apiAskBS "value"
                                        return $ (, value) <$> name
    return $ Just $ SignatoryTMP
                { fstname = fstname
                , sndname = sndname
                , company = company
                , personalnumber = personalnumber
                , companynumber = companynumber
                , email = email 
                , fields = concat $ maybeToList fields
                }

toSignatoryDetails::SignatoryTMP -> SignatoryDetails
toSignatoryDetails sTMP = 
    let sig = makeSignatoryNoPlacements 
                 (fold $ fstname sTMP)  
                 (fold $ sndname sTMP)
                 (fold $ email sTMP)
                 (SignOrder 1)
                 (fold $ company sTMP)
                 (fold $ personalnumber sTMP)
                 (fold $ companynumber sTMP)
    in sig  {signatoryotherfields = for (fields sTMP) $ \(name, mvalue) -> 
                                        FieldDefinition { fieldlabel = name,
                                                          fieldvalue = maybe BS.empty id mvalue,
                                                          fieldplacements = [],
                                                          fieldfilledbyauthor = isJust mvalue
                                                        }
            }                                            

mergeSignatoryWithTMP::(APIContext c) => SignatoryTMP  -> SignatoryLink-> APIFunction c SignatoryLink
mergeSignatoryWithTMP sTMP sl@(SignatoryLink{signatorydetails=sd})  =  do              
  return $ sl { signatorydetails =  sd {
      signatoryfstname = fromMaybe (signatoryfstname sd) (fstname sTMP)
    , signatorysndname = fromMaybe (signatorysndname sd) (sndname sTMP)
    , signatorycompany = fromMaybe (signatorycompany sd) (company sTMP)
    , signatorycompanynumber = fromMaybe (signatorycompanynumber sd) (companynumber sTMP)
    , signatorypersonalnumber = fromMaybe (signatorypersonalnumber sd) (personalnumber sTMP) 
    , signatoryemail = fromMaybe (signatoryemail  sd) (email sTMP)
    , signatoryotherfields = fillFields (signatoryotherfields sd) (fields sTMP)
  }}

fillFields:: [FieldDefinition] -> [(BS.ByteString,Maybe BS.ByteString)] ->  [FieldDefinition]
fillFields (f:fs) nv = (f {fieldvalue = fromMaybe (fieldvalue f) $ join $ lookup (fieldlabel f) nv}) : fillFields fs nv
fillFields [] _ = []                



-- High level commons. Used buy some simmilar API's, but not all of them
getFiles :: (APIContext c) => APIFunction c [(BS.ByteString, BS.ByteString)]
getFiles = fmap (fromMaybe []) $ apiLocal "files" $ apiMapLocal $ do
    name    <- apiAskBS     "name"
    content <- apiAskBase64 "content"
    when (isNothing name || isNothing content) $ throwApiError API_ERROR_MISSING_VALUE "Problems with files upload."
    return $ Just (fromJust name, fromJust content)    