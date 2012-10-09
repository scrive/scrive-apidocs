{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocStateData (
    module Doc.DocumentID
  , module Doc.SignatoryLinkID
  , CSVUpload(..)
  , CancelationReason(..)
  , DocStats(..)
  , Document(..)
  , DocumentSharing(..)
  , DocumentStatus(..)
  , DocumentTag(..)
  , DocumentUI(..)
  , DocumentType(..)
  , DocumentProcess(..)
  , FieldPlacement(..)
  , File(..)
  , FileStorage(..)
  , AuthenticationMethod(..)
  , DeliveryMethod(..)
  , JpegPages(..)
  , MailsDeliveryStatus(..)
  , SignatureProvider(..)
  , SignInfo(..)
  , SignOrder(..)
  , SignatoryField(..)
  , FieldType(..)
  , TipSide(..)
  , SignatoryDetails(..)
  , SignatoryLink(..)
  , SignatoryRole(..)
  , SignatureInfo(..)
  , TimeoutTime(..)
  , AuthorAttachment(..)
  , SignatoryAttachment(..)
  , StatusClass(..)
  , getFieldOfType
  , getValueOfType
  , emptyDocumentUI
  , documentType
  , toDocumentProcess
  , doctypeFromString
  ) where

import Company.Model
import Data.Data
import Data.Maybe
import DB.Derive
import IPAddress
import MagicHash
import MinutesTime
import User.UserID
import User.Region
import User.Locale
import File.FileID
import File.File
import Doc.DocumentID
import Doc.JpegPages
import Doc.SignatoryLinkID
import Database.HDBC
import Data.List
import ELegitimation.SignatureProvider
import Text.JSON.FromJSValue
import Text.JSON.Gen
import Text.JSON
import Control.Applicative
import Utils.Read
import Control.Monad
import qualified Data.Set as S

newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''TimeoutTime)

instance Show TimeoutTime where
  show (TimeoutTime t) = show t

newtype SignOrder = SignOrder { unSignOrder :: Integer }
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''SignOrder)


{- |
    We want the documents to be ordered like the icons in the bottom
    of the document list.  So this means:
    0 Draft - 1 Cancel - 2 Fall due - 3 Sent - 4 Opened - 5 Signed
-}

data StatusClass = SCDraft
                  | SCCancelled
                  | SCRejected
                  | SCTimedout
                  | SCError
                  | SCDeliveryProblem -- Order is important for SQL's
                  | SCSent
                  | SCDelivered
                  | SCRead
                  | SCOpened
                  | SCSigned
                  deriving (Eq, Ord, Enum, Bounded) 
                  
instance Show StatusClass where
  show SCDraft = "draft"
  show SCCancelled = "cancelled"
  show SCRejected  = "rejected"
  show SCTimedout  = "timeouted"
  show SCError = "problem"
  show SCDeliveryProblem = "deliveryproblem"
  show SCSent = "sent"
  show SCDelivered = "delivered"
  show SCRead = "read"
  show SCOpened = "opened"
  show SCSigned = "signed"

instance Read StatusClass where
  readsPrec _ str =
    [(v,drop (length (show v)) str) | v <- [minBound .. maxBound], show v `isPrefixOf` str]

data AuthenticationMethod = StandardAuthentication
                          | ELegAuthentication
  deriving (Eq, Ord, Show)

data DeliveryMethod = EmailDelivery
                    | PadDelivery
                    | APIDelivery
  deriving (Eq, Ord, Show)

data SignatureInfo = SignatureInfo {
    signatureinfotext        :: String
  , signatureinfosignature   :: String
  , signatureinfocertificate :: String
  , signatureinfoprovider    :: SignatureProvider
  , signaturefstnameverified :: Bool
  , signaturelstnameverified :: Bool
  , signaturepersnumverified :: Bool
  , signatureinfoocspresponse :: Maybe String -- verified legal evidence issued by BankID
  } deriving (Eq, Ord, Show)

data FieldType = FirstNameFT
               | LastNameFT
               | CompanyFT
               | PersonalNumberFT
               | CompanyNumberFT
               | EmailFT
               | CustomFT String Bool -- label filledbyauthor
               | SignatureFT
               | CheckboxOptionalFT String 
               | CheckboxObligatoryFT String 
  deriving (Eq, Ord, Show, Data, Typeable)

data SignatoryField = SignatoryField {
    sfType       :: FieldType
  , sfValue      :: String
  , sfPlacements :: [FieldPlacement]
  } deriving (Eq, Ord, Show, Data, Typeable)

data FieldPlacement = FieldPlacement
  { placementxrel       :: Double
  , placementyrel       :: Double
  , placementwrel       :: Double
  , placementhrel       :: Double
  , placementfsrel      :: Double
  , placementpage       :: Int
  , placementtipside    :: Maybe TipSide
  } deriving (Ord, Show, Data, Typeable)


instance Eq FieldPlacement where
    (==) _ _ = True -- Temporary fix to deal with double conversion in DB
  
data TipSide = LeftTip | RightTip
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data SignatoryDetails = SignatoryDetails {
    signatorysignorder :: SignOrder
  , signatoryfields    :: [SignatoryField]
  } deriving (Ord, Show)

instance Eq SignatoryDetails where
  SignatoryDetails {signatorysignorder=so1, signatoryfields=sf1} ==
    SignatoryDetails {signatorysignorder=so2, signatoryfields=sf2} =
      so1 == so2 && (sort sf2) == (sort sf1)

data SignatoryLink = SignatoryLink {
    signatorylinkid            :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
  , signatorydetails           :: SignatoryDetails    -- ^ details of this person as filled in invitation
  , signatorymagichash         :: MagicHash           -- ^ authentication code
  , maybesignatory             :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
  , maybecompany               :: Maybe CompanyID     -- ^ if this document has been saved to a company account this is the companyid
  , maybesigninfo              :: Maybe SignInfo      -- ^ when a person has signed this document
  , maybeseeninfo              :: Maybe SignInfo      -- ^ when a person has first seen this document
  , maybereadinvite            :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read
  , invitationdeliverystatus   :: MailsDeliveryStatus -- ^ status of email delivery
  , signatorysignatureinfo     :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
  , signatoryroles             :: [SignatoryRole]
  , signatorylinkdeleted       :: Bool -- ^ when true sends the doc to the recycle bin for that sig
  , signatorylinkreallydeleted :: Bool -- ^ when true it means that the doc has been removed from the recycle bin
  , signatorylinkcsvupload     :: Maybe CSVUpload
  , signatoryattachments       :: [SignatoryAttachment]
  , signatorylinkstatusclass   :: StatusClass
  , signatorylinksignredirecturl  :: Maybe String
  } deriving (Eq, Ord, Show)

data SignatoryRole = SignatoryPartner | SignatoryAuthor
    deriving (Eq, Ord, Bounded, Enum, Show)

data CSVUpload = CSVUpload {
    csvtitle :: String
  , csvcontents  :: [[String]]
  , csvsignatoryindex :: Int
  } deriving (Eq, Ord, Show)

data SignInfo = SignInfo {
    signtime :: MinutesTime
  , signipnumber :: IPAddress
  } deriving (Eq, Ord, Show)

{-
   Document start in Preparation state.

   Meaning:
   * Preparation: Only author can see it. He's still editing.
   * Pending: People can sign document. Could be timed out.
   * AwaitingAuthor: Everyone has signed but the author.
   * Closed: Everybody signed. This is final state.
   * Canceled: Author has canceled the document.
   * Timedout: This works as autocancel and has exactly same
     properties.

   Transitions:
   * Preparation to Pending: When invitations are sent.
   * Preparation to Cancel: mail about cancel to
     all who have signed it already is sent.
     TODO: Should other parties get an email?
   * Preparation to Timedout: mail about timeout to
     all who have signed it already is sent.
   * Pending to Closed: When everyone has signed.
     Info about closed deal is sent to everybody involved.
   * Pending to AwaitingAuthor: When all signatories have signed and there were fields.
     Info is sent to author.
   * AwaitingAuthor to Closed: Author signs it.
   * Pending to Cancel: Send no emails.
   * Pending to Timeout: TODO: No action?

   Allowed actions:
   * Preparation: change document, change title, add/rem signatories
   * Pending: change email of a signatory, signatory can sign
   * AwaitingAuthor: autho can sign.
   * Closed: nothing
   * Canceled: edit back to Preparation
   * Timedout: edit back to Preparation

   Archived bit:
   * This bit just moves document out of main view.
 -}

data DocumentStatus = Preparation
                    | Pending
                    | Closed
                    | Canceled
                    | Timedout
                    | Rejected
                    | DocumentError String
  deriving (Eq, Ord, Show)

data DocumentProcess = Contract | Offer | Order
  deriving (Eq, Ord, Show, Read)

data DocumentType = Signable DocumentProcess
                  | Template DocumentProcess
  deriving (Eq, Ord, Show, Read)

instance Convertible DocumentType SqlValue where
  safeConvert v = Right . SqlInteger $ case v of
    Signable _ -> 1
    Template _ -> 2

documentType :: (Int, Maybe DocumentProcess) -> DocumentType
documentType (1, Just p) = Signable p
documentType (2, Just p) = Template p
documentType v = error $ "documentType: wrong values: " ++ show v

toDocumentProcess :: DocumentType -> DocumentProcess
toDocumentProcess (Signable p) = p
toDocumentProcess (Template p) = p

-- | Terrible, I know. Better idea?
-- | TODO: to be KILLED.
-- | 
-- | Changed to return Maybe
doctypeFromString :: String -> Maybe DocumentType
doctypeFromString "Signable Contract"  = Just $ Signable Contract
doctypeFromString "Signable Offer"     = Just $ Signable Offer
doctypeFromString "Signable Order"     = Just $ Signable Order
doctypeFromString "Template Contract"  = Just $ Template Contract
doctypeFromString "Template Offer"     = Just $ Template Offer
doctypeFromString "Template Order"     = Just $ Template Order
doctypeFromString _                    = Nothing

data DocumentSharing = Private
                     | Shared -- means that the document is shared with subaccounts, and those with same parent accounts
  deriving (Eq, Ord, Show)

data DocumentTag = DocumentTag {
    tagname :: String
  , tagvalue :: String
  } deriving (Eq, Show, Data, Typeable)

 -- for inserting into set
instance Ord DocumentTag where
  a `compare` b = tagname a `compare` tagname b

data DocumentUI = DocumentUI {
    documentmailfooter :: Maybe String
  } deriving (Eq, Ord, Show)

emptyDocumentUI :: DocumentUI
emptyDocumentUI = DocumentUI { documentmailfooter = Nothing }


getFieldOfType :: FieldType -> [SignatoryField] -> Maybe SignatoryField
getFieldOfType _ [] = Nothing
getFieldOfType t (sf:rest) =
  if sfType sf == t then Just sf else getFieldOfType t rest

getValueOfType :: FieldType -> SignatoryDetails -> String
getValueOfType t = fromMaybe "" . fmap sfValue . getFieldOfType t . signatoryfields


data DocStats = DocStats {
    doccount          :: !Int
  , signaturecount    :: !Int
  , signaturecount1m  :: !Int
  , signaturecount2m  :: !Int
  , signaturecount3m  :: !Int
  , signaturecount6m  :: !Int
  , signaturecount12m :: !Int
  } deriving (Eq, Ord, Show, Data, Typeable) -- Data instance used for View modules (quite incorrectly there, please remove ASAP)

data Document = Document {
    documentid                     :: DocumentID
  , documenttitle                  :: String
  , documentsignatorylinks         :: [SignatoryLink]
  , documentfiles                  :: [FileID]
  , documentsealedfiles            :: [FileID]
  , documentstatus                 :: DocumentStatus
  , documenttype                   :: DocumentType
  , documentctime                  :: MinutesTime
  , documentmtime                  :: MinutesTime
  , documentdaystosign             :: Maybe Int
  , documenttimeouttime            :: Maybe TimeoutTime
  , documentinvitetime             :: Maybe SignInfo
  , documentinvitetext             :: String
  , documentauthenticationmethod   :: AuthenticationMethod
  , documentdeliverymethod         :: DeliveryMethod
  , documentcancelationreason      :: Maybe CancelationReason -- When a document is cancelled, there are two (for the moment) possible explanations. Manually cancelled by the author and automatically cancelled by the eleg service because the wrong person was signing.
  , documentsharing                :: DocumentSharing
  , documentrejectioninfo          :: Maybe (MinutesTime, SignatoryLinkID, String)
  , documenttags                   :: S.Set DocumentTag
  , documentdeleted                :: Bool -- set to true when doc is deleted - the other fields will be cleared too, so it is really truely deleting, it's just we want to avoid re-using the docid.
  , documentauthorattachments      :: [AuthorAttachment]
  , documentui                     :: DocumentUI
  , documentregion                 :: Region
  , documentstatusclass            :: StatusClass
  , documentapicallbackurl         :: Maybe String
  } deriving (Eq, Ord, Show)

instance HasLocale Document where
  getLocale = mkLocaleFromRegion . documentregion

data CancelationReason = ManualCancel
                        -- The data returned by ELeg server
                        --                 msg                    fn            ln            num
                        | ELegDataMismatch String SignatoryLinkID String String String
  deriving (Eq, Ord, Show, Data, Typeable)


newtype AuthorAttachment = AuthorAttachment { authorattachmentfile :: FileID }
  deriving (Eq, Ord, Show)

data SignatoryAttachment = SignatoryAttachment {
    signatoryattachmentfile            :: Maybe FileID
  , signatoryattachmentname            :: String
  , signatoryattachmentdescription     :: String
  } deriving (Eq, Ord, Show)

data MailsDeliveryStatus = Delivered
                         | Undelivered
                         | Unknown
                         | Deferred
                           deriving (Eq, Ord, Show)

-- stuff for converting to pgsql

$(bitfieldDeriveConvertible ''SignatoryRole)
$(enumDeriveConvertible ''MailsDeliveryStatus)
$(newtypeDeriveConvertible ''SignOrder)
$(enumDeriveConvertible ''DocumentProcess)
$(enumDeriveConvertible ''StatusClass)
$(enumDeriveConvertibleIgnoreFields ''DocumentStatus)
$(enumDeriveConvertibleIgnoreFields ''FieldType)
$(enumDeriveConvertible ''AuthenticationMethod)
$(enumDeriveConvertible ''DeliveryMethod)
$(enumDeriveConvertible ''DocumentSharing)
$(jsonableDeriveConvertible [t| [DocumentTag] |])
$(jsonableDeriveConvertible [t| CancelationReason |])
$(jsonableDeriveConvertible [t| [[String]] |])

-- this should go to fields-json
instance FromJSValue Double where
    fromJSValue (JSRational _ r) = Just $ fromRational r
    fromJSValue _ = Nothing

instance FromJSValue FieldPlacement where
  -- Here we do three cases of json representation of FieldPlacement.
  -- First one is new relative representation, second is old json
  -- representation for javascript and ajax, third is database
  -- representation. After ajax is changed and database is migrated
  -- only first on representation should be left in place.
  fromJSValue js = msum $ fmap ($ js) 
                     [ do xrel       <- fromJSValueField "xrel"
                          yrel       <- fromJSValueField "yrel"
                          wrel       <- fromJSValueField "wrel"
                          hrel       <- fromJSValueField "hrel"
                          fsrel      <- fromJSValueField "fsrel"
                          page       <- fromJSValueField "page"
                          side       <- fromJSValueField "tip"
                          return (FieldPlacement <$> xrel <*> yrel
                                                 <*> wrel <*> hrel <*> fsrel
                                                 <*> page <*> Just side)
                     , do x          <- fromJSValueField "x"
                          y          <- fromJSValueField "y"
                          page       <- fromJSValueField "page"
                          pagewidth  <- fromJSValueField "pagewidth"
                          pageheight <- fromJSValueField "pageheight"
                          side       <- fromJSValueField "tip"
                          let xrel  = (/) <$> x <*> pagewidth
                          let yrel  = (/) <$> y <*> pageheight
                          let wrel  = Just 0
                          let hrel  = Just 0
                          let fsrel = Just 0
                          return (FieldPlacement <$> xrel <*> yrel
                                                 <*> wrel <*> hrel <*> fsrel
                                                 <*> page <*> Just side)
                     , do x          <- fromJSValueField "placementx"
                          y          <- fromJSValueField "placementy"
                          page       <- fromJSValueField "placementpage"
                          pagewidth  <- fromJSValueField "placementpagewidth"
                          pageheight <- fromJSValueField "placementpageheight"
                          tipside    <- fromJSValueField "placementtipside"
                          let xrel  = (/) <$> x <*> pagewidth
                          let yrel  = (/) <$> y <*> pageheight
                          let wrel  = Just 0
                          let hrel  = Just 0
                          let fsrel = Just 0
                          return (FieldPlacement <$> xrel <*> yrel
                                                 <*> wrel <*> hrel <*> fsrel
                                                 <*> page <*> (Just $ join $ maybeRead <$> tipside))
                     ]

instance FromJSValue TipSide where
    fromJSValue js = case fromJSValue js of
          Just "left"  -> Just LeftTip
          Just "right" -> Just RightTip
          _ ->            Nothing


instance Convertible  [FieldPlacement] SqlValue where
    safeConvert = jsonToSqlValueCustom $ JSArray . (map placementJSON) 
        where
         placementJSON placement = runJSONGen $ do 
                             value "xrel" $ placementxrel placement
                             value "yrel" $ placementyrel placement
                             value "wrel" $ placementwrel placement
                             value "hrel" $ placementhrel placement
                             value "fsrel" $ placementfsrel placement
                             value "page" $ placementpage placement
                             value "tip" $ case (placementtipside placement) of
                                             Just LeftTip -> Just "left"
                                             Just RightTip -> Just "right"
                                             _ -> Nothing

instance Convertible  SqlValue [FieldPlacement] where
    safeConvert = jsonFromSqlValueCustom $ nothingToResult . (fromJSValueCustomMany fromJSValue)
