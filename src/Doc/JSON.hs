module Doc.JSON 
       (
         jsonDocumentForSignatory
       , jsonDocumentID
       , jsonDocumentType
       , jsonSigAttachmentWithFile
       , jsonDocumentMetadata
       , jsonDocumentForAuthor
       , apiDocumentType
       )
where

import Doc.DocStateData
import Text.JSON
import Util.JSON
import Misc
import KontraLink

apiDocumentType :: DocumentType -> Int
apiDocumentType (Signable Contract) = 1
apiDocumentType (Signable Offer)    = 3
apiDocumentType (Template Contract) = 2
apiDocumentType (Template Offer)    = 4
apiDocumentType (Signable Order)    = 5
apiDocumentType (Template Order)    = 6
apiDocumentType (Attachment)        = 20
apiDocumentType (AttachmentTemplate)= error $ "Unsupported document type AttachmentTemplate"

jsonDocumentType :: DocumentType -> JSValue
jsonDocumentType = showJSON . apiDocumentType

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

jsonDocumentMetadata :: Document -> JSValue
jsonDocumentMetadata doc = fromRight $
                           (Right jsempty) >>=
                           (jsset "url" $ show $ LinkAPIDocumentMetadata (documentid doc)) >>=
                           (jsset "title" $ documenttitle doc)

jsonDocumentForAuthor :: Document -> JSValue
jsonDocumentForAuthor doc = 
  fromRight            $ (Right jsempty)                          >>=                                
  (jsset "designurl"   $ show $ LinkIssueDoc (documentid doc))    >>=
  (jsset "document_id" $ jsonDocumentID $ documentid doc)         >>=    
  (jsset "title"       $ documenttitle doc)                       >>=                        
  (jsset "type"        $ apiDocumentType $ documenttype doc)     >>=       
  (jsset "status"      $ jsonDocumentStatus $ documentstatus doc) >>=
  (jsset "metadata"    $ jsonDocumentMetadata doc) >>=
  (jsset "authorization" $ jsonIdentificationType $ documentallowedidtypes doc)

jsonDocumentForSignatory :: Document -> JSValue
jsonDocumentForSignatory doc = 
  fromRight            $ (Right jsempty)                          >>=                                
  (jsset "document_id" $ jsonDocumentID $ documentid doc)         >>=    
  (jsset "title"       $ documenttitle doc)                       >>=                        
  (jsset "type"        $ apiDocumentType $ documenttype doc)     >>=       
  (jsset "status"      $ jsonDocumentStatus $ documentstatus doc) >>=
  (jsset "metadata"    $ jsonDocumentMetadata doc) >>=
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
