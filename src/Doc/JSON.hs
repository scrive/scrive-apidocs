{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.JSON 
       (
         jsonDocumentForSignatory
       , jsonDocumentID
       , jsonDocumentType
       , jsonSigAttachmentWithFile
       , jsonDocumentMetadata
       , jsonDocumentForAuthor
       )
where

import Doc.DocStateData
import Text.JSON
import Util.JSON
import Misc
import KontraLink

instance SafeEnum DocumentType where
  fromSafeEnum (Signable Contract) = 1
  fromSafeEnum (Template Contract) = 2
  fromSafeEnum (Signable Offer)    = 3
  fromSafeEnum (Template Offer)    = 4
  fromSafeEnum (Signable Order)    = 5
  fromSafeEnum (Template Order)    = 6
  fromSafeEnum (Attachment)        = 20
  -- what to do with AttachmentTemplate?
  fromSafeEnum (AttachmentTemplate)= 21
  
  toSafeEnum 1  = Just (Signable Contract)
  toSafeEnum 2  = Just (Template Contract)
  toSafeEnum 3  = Just (Signable Offer)
  toSafeEnum 4  = Just (Template Offer)
  toSafeEnum 5  = Just (Signable Order)
  toSafeEnum 6  = Just (Template Order)
  toSafeEnum 20 = Just (Attachment)
  toSafeEnum 21 = Just (AttachmentTemplate)
  toSafeEnum _  = Nothing

jsonDocumentType :: DocumentType -> JSValue
jsonDocumentType = showJSON . fromSafeEnumInt

jsonDocumentID :: DocumentID -> JSValue
jsonDocumentID = showJSON . show

instance SafeEnum DocumentStatus where
    fromSafeEnum Preparation       = 0
    fromSafeEnum Pending           = 10
    fromSafeEnum AwaitingAuthor    = 10
    fromSafeEnum Timedout          = 20
    fromSafeEnum Rejected          = 20
    fromSafeEnum Canceled          = 20
    fromSafeEnum Closed            = 30
    fromSafeEnum (DocumentError _) = 40
    toSafeEnum _                   = Nothing

instance SafeEnum [IdentificationType] where
    fromSafeEnum [EmailIdentification]            = 1
    fromSafeEnum [ELegitimationIdentification]    = 10
    fromSafeEnum ls                               = 1
    toSafeEnum 1  =  Just [EmailIdentification]
    toSafeEnum 10 = Just [ELegitimationIdentification]
    toSafeEnum _  = Nothing

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
  (jsset "type"        $ fromSafeEnumInt $ documenttype doc)     >>=       
  (jsset "status"      $ fromSafeEnumInt $ documentstatus doc) >>=
  (jsset "metadata"    $ jsonDocumentMetadata doc) >>=
  (jsset "authorization" $ fromSafeEnumInt $ documentallowedidtypes doc)

jsonDocumentForSignatory :: Document -> JSValue
jsonDocumentForSignatory doc = 
  fromRight            $ (Right jsempty)                          >>=                                
  (jsset "document_id" $ jsonDocumentID $ documentid doc)         >>=    
  (jsset "title"       $ documenttitle doc)                       >>=                        
  (jsset "type"        $ fromSafeEnumInt $ documenttype doc)     >>=       
  (jsset "status"      $ fromSafeEnumInt $ documentstatus doc) >>=
  (jsset "metadata"    $ jsonDocumentMetadata doc) >>=
  (jsset "authorization" $ fromSafeEnumInt $ documentallowedidtypes doc)


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
