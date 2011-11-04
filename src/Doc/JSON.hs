module Doc.JSON 
       (
         jsonDocumentForSignatory
       , jsonDocumentID
       , jsonDocumentType
       )
where

import Doc.DocStateData
import Text.JSON
import Util.JSON
import Misc


jsonDocumentType :: DocumentType -> JSValue
jsonDocumentType (Signable Contract) = showJSON (1 :: Int)
jsonDocumentType (Signable Offer)    = showJSON (3 :: Int)
jsonDocumentType (Template Contract) = showJSON (2 :: Int)
jsonDocumentType (Template Offer)    = showJSON (4 :: Int)
jsonDocumentType x                   = error $ "Unsupported document type " ++ show x

jsonDocumentID :: DocumentID -> JSValue
jsonDocumentID = showJSON . show

jsonDocumentStatus :: DocumentStatus -> JSValue
jsonDocumentStatus Preparation       = showJSON (0  :: Int)
jsonDocumentStatus Pending           = showJSON (10 :: Int)
jsonDocumentStatus AwaitingAuthor    = showJSON (10 :: Int)
jsonDocumentStatus Rejected          = showJSON (20 :: Int)
jsonDocumentStatus Canceled          = showJSON (20 :: Int)
jsonDocumentStatus Timedout          = showJSON (20 :: Int)
jsonDocumentStatus Closed            = showJSON (30 :: Int)
jsonDocumentStatus (DocumentError _) = showJSON (40 :: Int)

jsonIdentificationType :: [IdentificationType] -> JSValue
jsonIdentificationType [EmailIdentification] = showJSON (1 :: Int)
jsonIdentificationType [ELegitimationIdentification] = showJSON (10 :: Int)
jsonIdentificationType x = error $ "Unknown id type " ++ show x

jsonDocumentForSignatory :: Document -> JSValue
jsonDocumentForSignatory doc = 
  fromRight            $ (Right jsempty)                          >>=                                
  (jsset "document_id" $ jsonDocumentID $ documentid doc)         >>=    
  (jsset "title"       $ documenttitle doc)                       >>=                        
  (jsset "type"        $ jsonDocumentType $ documenttype doc)     >>=       
  (jsset "status"      $ jsonDocumentStatus $ documentstatus doc) >>=
  (jsset "authorization" $ jsonIdentificationType $ documentallowedidtypes doc)
