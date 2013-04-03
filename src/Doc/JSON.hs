{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.JSON
       (
         jsonDocumentForSignatory
       , jsonDocumentID
       , jsonDocumentType
       , jsonSigAttachmentWithFile
       , jsonDocumentForAuthor
       , dcrFromJSON
       , DocumentCreationRequest(..)
       , InvolvedRequest(..)
       )
where

import Doc.DocStateData
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen
import Doc.DocumentID
import Utils.Enum
import KontraLink
import Control.Monad.Identity
import Data.Maybe
import Data.Functor
import Control.Monad.Reader
import Doc.DocDraft()
import Data.List (nub)

parseIsAuthor :: Int -> Maybe Bool
parseIsAuthor n
  | n == 1    = Just True
  | n == 2    = Just True
  | n == 5    = Just False
  | n == 10   = Just False
  | otherwise = Nothing

parseIsPartner :: Int -> Maybe Bool
parseIsPartner n
  | n == 1    = Just False
  | n == 2    = Just True
  | n == 5    = Just True
  | n == 10   = Just False
  | otherwise = Nothing

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
    fromSafeEnum StandardAuthentication = 1
    fromSafeEnum ELegAuthentication  = 2
    toSafeEnum 1 = Just StandardAuthentication
    toSafeEnum 2 = Just ELegAuthentication
    toSafeEnum _  = Nothing

instance SafeEnum DeliveryMethod where
    fromSafeEnum EmailDelivery = 1
    fromSafeEnum PadDelivery   = 2
    fromSafeEnum APIDelivery   = 3
    toSafeEnum 1 = Just EmailDelivery
    toSafeEnum 2 = Just PadDelivery
    toSafeEnum 3 = Just APIDelivery
    toSafeEnum _  = Nothing

jsonDocumentForAuthor :: Document -> String -> JSValue
jsonDocumentForAuthor doc hostpart =
  runJSONGen $ do
    value "url"            $ hostpart ++ show (LinkIssueDoc (documentid doc))
    value "document_id"    $ jsonDocumentID $ documentid doc
    value "title"          $ documenttitle doc
    value "type"           $ fromSafeEnumInt $ documenttype doc
    value "status"         $ fromSafeEnumInt $ documentstatus doc
    value "designurl"      $ show $ LinkIssueDoc (documentid doc)
    value "authentication" $ case nub (map signatorylinkauthenticationmethod (documentsignatorylinks doc)) of
                                   [StandardAuthentication] -> (1 :: Int)
                                   [ELegAuthentication]     -> 2
                                   _                        -> 0
    value "delivery"       $ fromSafeEnumInt $ documentdeliverymethod doc

jsonDocumentForSignatory :: Document -> JSValue
jsonDocumentForSignatory doc =
  runJSONGen $ do
    value "document_id"    $ jsonDocumentID $ documentid doc
    value "title"          $ documenttitle doc
    value "type"           $ fromSafeEnumInt $ documenttype doc
    value "status"         $ fromSafeEnumInt $ documentstatus doc
    value "authentication" $ case nub (map signatorylinkauthenticationmethod (documentsignatorylinks doc)) of
                                   [StandardAuthentication] -> (1 :: Int)
                                   [ELegAuthentication]     -> 2
                                   _                        -> 0
    value "delivery"       $ fromSafeEnumInt $ documentdeliverymethod doc

-- I really want to add a url to the file in the json, but the only
-- url at the moment requires a sigid/mh pair
jsonSigAttachmentWithFile :: SignatoryAttachment -> Maybe File -> JSValue
jsonSigAttachmentWithFile sa mfile =
  runJSONGen $ do
    value "name"        $ signatoryattachmentname sa
    value "description" $ signatoryattachmentdescription sa
    case mfile of
      Nothing   -> value "requested" True
      Just file -> object "file" $ do
        value "id"   $ show (fileid file)
        value "name" $ filename file

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
  irIsAuthor    :: Bool,
  irIsPartner   :: Bool,
  irData        :: [SignatoryField],
  irAttachments :: [SignatoryAttachment],
  irSignOrder   :: Maybe Int
  }
                     deriving (Show, Eq)

arFromJSON :: (MonadReader JSValue m, Functor m) => m (Maybe SignatoryAttachment)
arFromJSON = do
      mname <- fromJSValueField "name"
      desc <- fromMaybe "" <$> (fromJSValueField "description")
      case mname of
           Just name -> return $ Just SignatoryAttachment {
                signatoryattachmentfile        = Nothing,
                signatoryattachmentname        = name,
                signatoryattachmentdescription = desc
              }
           Nothing -> return Nothing

sfFromJSON :: (String, JSValue) -> Maybe SignatoryField
sfFromJSON (name, jsv) =
  Just $ SignatoryField {
      sfType = tp,
      sfValue = val,
      sfObligatory = obl,
      sfPlacements = [] -- do placements later /Eric
    }
  where
    val = maybe "" id (fromJSValueField "value" jsv)
    req = maybe False id (fromJSValueField "requested" jsv)
    obl = maybe True id (fromJSValueField "obligatory" jsv)
    tp  = case name of
            "email"      -> EmailFT
            "fstname"    -> FirstNameFT
            "sndname"    -> LastNameFT
            "company"    -> CompanyFT
            "companynr"  -> CompanyNumberFT
            "personalnr" -> PersonalNumberFT
            s            -> CustomFT s req

irFromJSON :: (MonadReader JSValue m, Functor m) => m (Maybe InvolvedRequest)
irFromJSON = do
  mso <- fromJSValueField "signorder"
  mattachments <- fromJSValueFieldCustom "attachments" $ fromJSValueCustomMany arFromJSON
  role <- fromJSValueField "role"
  mData <- fromJSValueFieldCustom "data" $ do
            dat <- fmap readJSON <$> fromJSValueM
            case dat of
              Just (Ok d) -> return $ sequence $ map sfFromJSON (fromJSObject d)
              _ -> return $ Nothing
  return $ case mData of
    (Just hisData) -> return $ InvolvedRequest {
          irIsAuthor = fromMaybe False $ join $ parseIsAuthor <$> role,
          irIsPartner = fromMaybe True $ join $ parseIsPartner <$> role,
          irData = hisData,
          irAttachments = fromMaybe [] mattachments,
          irSignOrder = mso
        }
    _ -> Nothing

-- make this only require type; everything else is implied
dcrFromJSON :: JSValue -> Either String DocumentCreationRequest
dcrFromJSON jsv = runIdentity $ withJSValue jsv $ do
  tp <- (fromJSValueField "type")
  mtitle <- fromJSValueField "title"
  mmainfile <- fromJSValueFieldCustom "mainfile" $ fromJSValueField "name"
  tags <- fromJSValueField "tags"
  inv <-  fromJSValueFieldCustom "involved" $ fromJSValueCustomMany irFromJSON
  case inv of
       Nothing -> return (Left $ "Parsing error (no parse)" ++ encode jsv)
       Just [] -> return (Left $ "Parsing error (empty)" ++ encode jsv)
       Just inv'-> return $ Right $ DocumentCreationRequest { dcrTitle    = mtitle
                                   , dcrType     = fromMaybe (Signable Contract) $ join $ (toSafeEnum :: Int -> Maybe DocumentType) <$> tp
                                   , dcrTags     = fromMaybe [] tags
                                   , dcrInvolved = inv'
                                   , dcrMainFile = mmainfile
                                   , dcrAttachments = []
                                   }
