{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  API.IntegrationAPIUtils
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Constans and data structures used by integration API
-----------------------------------------------------------------------------
module API.IntegrationAPIUtils (
          api_document
        ) where


import Doc.DocState
import Text.JSON
import MinutesTime
import qualified Data.ByteString.UTF8 as BS


data DOCUMENT_AUTHORISATION = 
      DOCUMENT_AUTHORISATION_BASIC_EMAIL 
    | DOCUMENT_AUTHORISATION_BASIC_ELEG
    
    
instance Enum DOCUMENT_AUTHORISATION where
    fromEnum DOCUMENT_AUTHORISATION_BASIC_EMAIL = 1 
    fromEnum DOCUMENT_AUTHORISATION_BASIC_ELEG = 10
    toEnum _ = error "Enum instance for DOCUMENT_AUTHORISATION is one way only"
    
    
data DOCUMENT_RELATION = 
      DOCUMENT_RELATION_AUTHOR_SECRETARY
    | DOCUMENT_RELATION_AUTHOR_SIGNATORY
    | DOCUMENT_RELATION_SIGNATORY
    | DOCUMENT_RELATION_VIEWER
    | DOCUMENT_RELATION_OTHER
      
instance Enum DOCUMENT_RELATION where
    fromEnum DOCUMENT_RELATION_AUTHOR_SECRETARY = 1
    fromEnum DOCUMENT_RELATION_AUTHOR_SIGNATORY = 2
    fromEnum DOCUMENT_RELATION_SIGNATORY = 5
    fromEnum DOCUMENT_RELATION_VIEWER = 10
    fromEnum DOCUMENT_RELATION_OTHER = 20
    toEnum _ = error "Enum instance for DOCUMENT_RELATION is one way only"
    
data DOCUMENT_STATUS = 
      DOCUMENT_STATUS_PREPARATION
    | DOCUMENT_STATUS_PENDING
    | DOCUMENT_STATUS_STOPED
    | DOCUMENT_STATUS_CLOSED
    | DOCUMENT_STATUS_ERROR
    
instance Enum DOCUMENT_STATUS where
    fromEnum DOCUMENT_STATUS_PREPARATION = 0
    fromEnum DOCUMENT_STATUS_PENDING = 10
    fromEnum DOCUMENT_STATUS_STOPED = 20
    fromEnum DOCUMENT_STATUS_CLOSED = 30
    fromEnum DOCUMENT_STATUS_ERROR = 40
    toEnum _ = error "Enum instance for DOCUMENT_STATUS is one way only"

data DOCUMENT_TYPE = 
      DOCUMENT_TYPE_CONTRACT
    | DOCUMENT_TYPE_OFFER
    | DOCUMENT_TYPE_CONTRACT_TEMPLATE
    | DOCUMENT_TYPE_OFFER_TEMPLATE

instance Enum DOCUMENT_TYPE where
    fromEnum DOCUMENT_TYPE_CONTRACT = 1
    fromEnum DOCUMENT_TYPE_OFFER = 2
    fromEnum DOCUMENT_TYPE_CONTRACT_TEMPLATE = 3
    fromEnum DOCUMENT_TYPE_OFFER_TEMPLATE = 4
    toEnum _ = error "Enum instance for DOCUMENT_TYPE is one way only"
    
api_document_type::Document ->  DOCUMENT_TYPE
api_document_type doc   
    | isTemplate doc && isContract doc = DOCUMENT_TYPE_CONTRACT_TEMPLATE
    | isTemplate doc && isOffer doc = DOCUMENT_TYPE_OFFER_TEMPLATE
    | isContract doc = DOCUMENT_TYPE_CONTRACT
    | otherwise  = DOCUMENT_TYPE_OFFER 
    
    
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
     Just signinfo ->  [("seen", api_date $ signtime signinfo)]
     Nothing -> []
    ++ 
    [("sign",showJSON $ fromEnum $ api_document_relation sl)]                     
    
api_document_tag::DocumentTag -> JSValue
api_document_tag tag = JSObject $ toJSObject $ [
      ("name", showJSON $ BS.toString $ tagname tag)
    , ("value", showJSON $ BS.toString $ tagvalue tag)]


api_document::Bool -> Document -> JSValue
api_document files doc = JSObject $ toJSObject $  [ 
      ("document_id", showJSON  $ show $ unDocumentID $ documentid doc)
    , ("title", showJSON  $ BS.toString $ documenttitle doc)
    , ("type", showJSON  $ fromEnum $ api_document_type doc)
    , ("state", showJSON  $ fromEnum $ api_document_status doc)
    , ("involved", JSArray $ map api_signatory $ documentsignatorylinks doc)
    , ("tags", JSArray $ map api_document_tag $ documenttags doc)
    , ("authorization", showJSON  $ fromEnum $ api_document_authorisation doc)
    , ("mdate", api_date $ documentmtime doc)
    ]    
    ++ 
    if files
     then [("files",JSArray [])] 
     else []
       
api_date :: MinutesTime -> JSValue 
api_date = showJSON  . show
