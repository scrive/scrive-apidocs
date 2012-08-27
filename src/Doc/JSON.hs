{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.JSON
       (
         jsonDocumentForSignatory
       , jsonDocumentID
       , jsonDocumentType
       , jsonSigAttachmentWithFile
       , jsonDocumentForAuthor
       , fileNameFromJSON
       , dcrFromJSON
       , DocumentCreationRequest(..)
       , InvolvedRequest(..)
       )
where

import Doc.DocStateData
import Text.JSON
import Text.JSON.Types
import Text.JSON.FromJSValue
import Util.JSON
import Misc
import KontraLink

import Control.Applicative

instance SafeEnum [SignatoryRole] where
  fromSafeEnum srs =
    case srs of
      [SignatoryAuthor]                   -> 1
      [SignatoryPartner, SignatoryAuthor] -> 2
      [SignatoryAuthor, SignatoryPartner] -> 2
      [SignatoryPartner]                  -> 5
      []                                  -> 10
      _                                   -> 20
  toSafeEnum 1  = Just [SignatoryAuthor]
  toSafeEnum 2  = Just [SignatoryPartner, SignatoryAuthor]
  toSafeEnum 5  = Just [SignatoryPartner]
  toSafeEnum 10 = Just []
  toSafeEnum 20 = Just []
  toSafeEnum _  = Nothing

instance SafeEnum DocumentType where
  fromSafeEnum (Signable Contract) = 1
  fromSafeEnum (Template Contract) = 2
  fromSafeEnum (Signable Offer)    = 3
  fromSafeEnum (Template Offer)    = 4
  fromSafeEnum (Signable Order)    = 5
  fromSafeEnum (Template Order)    = 6

  toSafeEnum 1  = Just (Signable Contract)
  toSafeEnum 2  = Just (Template Contract)
  toSafeEnum 3  = Just (Signable Offer)
  toSafeEnum 4  = Just (Template Offer)
  toSafeEnum 5  = Just (Signable Order)
  toSafeEnum 6  = Just (Template Order)
  toSafeEnum _  = Nothing

jsonDocumentType :: DocumentType -> JSValue
jsonDocumentType = showJSON . fromSafeEnumInt

jsonDocumentID :: DocumentID -> JSValue
jsonDocumentID = showJSON . show

instance SafeEnum DocumentStatus where
    fromSafeEnum Preparation       = 0
    fromSafeEnum Pending           = 10
    fromSafeEnum Timedout          = 20
    fromSafeEnum Rejected          = 20
    fromSafeEnum Canceled          = 20
    fromSafeEnum Closed            = 30
    fromSafeEnum (DocumentError _) = 40
    toSafeEnum _                   = Nothing

instance SafeEnum AuthenticationMethod where
    fromSafeEnum EmailAuthentication = 1
    fromSafeEnum ELegAuthentication  = 2
    toSafeEnum 1 = Just EmailAuthentication
    toSafeEnum 2 = Just ELegAuthentication
    toSafeEnum _  = Nothing

instance SafeEnum DeliveryMethod where
    fromSafeEnum EmailDelivery = 1
    fromSafeEnum PadDelivery   = 2
    toSafeEnum 1 = Just EmailDelivery
    toSafeEnum 2 = Just PadDelivery
    toSafeEnum _  = Nothing

jsonDocumentForAuthor :: Document -> String -> JSValue
jsonDocumentForAuthor doc hostpart = 
  fromRight              $ (Right jsempty)                       >>=                    

  (jsset "url"           $ hostpart ++ show (LinkIssueDoc (documentid doc))) >>=
  (jsset "document_id"   $ jsonDocumentID $ documentid doc)      >>=                    
  (jsset "title"         $ documenttitle doc)                    >>=                    
  (jsset "type"          $ fromSafeEnumInt $ documenttype doc)   >>=                    
  (jsset "status"        $ fromSafeEnumInt $ documentstatus doc) >>=                    

  (jsset "designurl"     $ show $ LinkIssueDoc (documentid doc)) >>= -- up for deletion 
  (jsset "authentication" $ fromSafeEnumInt $ documentauthenticationmethod doc) >>=
  (jsset "delivery"      $ fromSafeEnumInt $ documentdeliverymethod doc)

jsonDocumentForSignatory :: Document -> JSValue
jsonDocumentForSignatory doc =
  fromRight              $ (Right jsempty)                       >>=                                
  (jsset "document_id"   $ jsonDocumentID $ documentid doc)      >>=    
  (jsset "title"         $ documenttitle doc)                    >>=                        
  (jsset "type"          $ fromSafeEnumInt $ documenttype doc)   >>=       
  (jsset "status"        $ fromSafeEnumInt $ documentstatus doc) >>=
  (jsset "authentication" $ fromSafeEnumInt $ documentauthenticationmethod doc) >>=
  (jsset "delivery"      $ fromSafeEnumInt $ documentdeliverymethod doc)

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
                                             (jsset "name" $ filename file)))

data DocumentCreationRequest = DocumentCreationRequest {
  dcrTitle       :: Maybe String,
  dcrType        :: DocumentType,
  dcrTags        :: [DocumentTag],
  dcrInvolved    :: [InvolvedRequest],
  dcrMainFile    :: Maybe String, -- filename
  dcrAttachments :: [String] -- filenames
  }
                             deriving (Show, Eq)

data InvolvedRequest = InvolvedRequest {
  irRole        :: [SignatoryRole],
  irData        :: [SignatoryField],
  irAttachments :: [SignatoryAttachment],
  irSignOrder   :: Maybe Int
  }
                     deriving (Show, Eq)

arFromJSON :: JSValue -> Either String SignatoryAttachment
arFromJSON jsv = do
  JSString (JSONString name) <- jsget "name" jsv
  JSString (JSONString description) <- jsgetdef "description" (showJSON "") jsv
  return $ SignatoryAttachment {
                                signatoryattachmentfile        = Nothing
                               ,signatoryattachmentname        = name
                               ,signatoryattachmentdescription = description
                               }

sfFromJSON :: (String, JSValue) -> Either String SignatoryField
sfFromJSON (name, jsv) = do
  JSString (JSONString value) <- jsgetdef "value" (showJSON "") jsv
  JSBool req <- jsgetdef "requested" (showJSON False) jsv
  tp <- case name of
    "email"      -> Right EmailFT
    "fstname"    -> Right FirstNameFT
    "sndname"    -> Right LastNameFT
    "company"    -> Right CompanyFT
    "companynr"  -> Right CompanyNumberFT
    "personalnr" -> Right PersonalNumberFT
    s            -> Right $ CustomFT s req
  -- do placements later /Eric
  return $ SignatoryField { sfType = tp, sfValue = value, sfPlacements = [] }

irFromJSON :: JSValue -> Either String InvolvedRequest
irFromJSON jsv = do
  i'@(JSRational _ _) <- jsgetdef "role" (showJSON (5::Int)) jsv
  let Just (i::Int) = fromJSValue i'
  JSObject dat' <- jsget "data" jsv
  let dat = fromJSObject dat'
  JSArray attachmentjs <- jsgetdef "attachments" (showJSON ([]::[JSValue])) jsv
  attachments <- mapM arFromJSON attachmentjs
  role <- maybe (Left $ "Not a valid role: " ++ show i)
          Right $ toSafeEnum i
  hisData <- mapM sfFromJSON dat
  let mso = either (const Nothing) fromJSValue $ jsget "signorder" jsv
  return $ InvolvedRequest { irRole = role, irData = hisData, irAttachments = attachments, irSignOrder = mso }

fileNameFromJSON :: JSValue -> Either String String
fileNameFromJSON jsv = do
  JSString (JSONString name) <- jsget "name" jsv
  return name

tagFromJSON :: JSValue -> Either String DocumentTag
tagFromJSON jsv = do
  JSString (JSONString name)  <- jsget "name"  jsv
  JSString (JSONString value) <- jsget "value" jsv
  return $ DocumentTag { tagname = name, tagvalue = value }

-- make this only require type; everything else is implied
dcrFromJSON :: JSValue -> Either String DocumentCreationRequest
dcrFromJSON jsv = do
  tp''@(JSRational _ _) <- jsgetdef "type" (showJSON (1::Int)) jsv
  let Just (tp'::Int) = fromJSValue tp''
  tp <- maybe (Left $ "Unrecognized document type: " ++ show tp') Right $ toSafeEnum tp'
  JSArray tags' <- jsgetdef "tags" (showJSON ([]::[JSValue])) jsv
  tags <- mapM tagFromJSON tags'
  JSArray inv' <- jsgetdef "involved" (showJSON ([]::[JSValue])) jsv
  inv <- mapM irFromJSON inv'
  let mtitle = case jsget "title" jsv of
                 Right (JSString s) -> Just $ fromJSString s
                 _ -> Nothing
  mmainfile <- case jsget "mainfile" jsv of
                 Left _ -> return Nothing
                 Right o -> Just <$> fileNameFromJSON o
  return $ DocumentCreationRequest { dcrTitle    = mtitle
                                   , dcrType     = tp
                                   , dcrTags     = tags
                                   , dcrInvolved = inv
                                   , dcrMainFile = mmainfile
                                   , dcrAttachments = []
                                   }
