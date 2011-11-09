module Doc.JSON 
       (
         jsonDocumentForSignatory
       , jsonDocumentID
       , jsonDocumentType
       , jsonSigAttachmentWithFile
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

-- I really want to add a url to the file in the json, but the only
-- url at the moment requires a sigid/mh pair
jsonSigAttachmentWithFile :: SignatoryAttachment -> Maybe File -> JSValue
jsonSigAttachmentWithFile sa mfile =
  fromRight $ (Right jsempty) >>=
  (jsset "name" $ signatoryattachmentname sa) >>=
  (jsset "description" $ signatoryattachmentdescription sa) >>=
  (case mfile of
      Nothing   -> jsset "requested" True
      Just file -> jsset "file" $ fromRight ((Right jsempty) >>=
                                             (jsset "id" $ show (fileid file)) >>=
                                             (jsset "name" $ (filename file))))
