{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocStateData (
    CSVUpload(..)
  , CancelationReason(..)
  , DocStats(..)
  , Document(..)
  , DocumentSharing(..)
  , DocumentStatus(..)
  , DocumentTag(..)
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
  , SignatureInfo(..)
  , TimeoutTime(..)
  , AuthorAttachment(..)
  , SignatoryAttachment(..)
  , StatusClass(..)
  , getFieldOfType
  , getValueOfType
  , documentType
  , toDocumentProcess
  ) where

import Data.Data
import Data.Maybe
import DB.Derive
import IPAddress
import MagicHash
import MinutesTime
import User.UserID
import User.Lang
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
import Utils.Default
import qualified Data.Set as S

newtype TimeoutTime = TimeoutTime { unTimeoutTime :: MinutesTime }
  deriving (Eq, Ord, Typeable)
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

instance FromJSValue AuthenticationMethod where
  fromJSValue j = case fromJSValue j of
    Just "standard" -> Just StandardAuthentication
    Just "eleg"     -> Just ELegAuthentication
    _               -> Nothing

instance FromJSValue DeliveryMethod where
  fromJSValue j = case fromJSValue j of
    Just "email" -> Just EmailDelivery
    Just "pad"   -> Just PadDelivery
    Just "api"   -> Just APIDelivery
    _            -> Nothing

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
               | SignatureFT String
               | CheckboxFT String
  deriving (Eq, Ord, Show, Data, Typeable)

data SignatoryField = SignatoryField
  { sfType       :: FieldType
  , sfValue      :: String
  , sfObligatory :: Bool
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
    (==) a b = eqByEpsilon placementxrel &&
               eqByEpsilon placementyrel &&
               eqByEpsilon placementwrel &&
               eqByEpsilon placementhrel &&
               eqByEpsilon placementfsrel &&
               placementpage a == placementpage b &&
               placementtipside a == placementtipside b
      where
        eqByEpsilon func = abs (func a - func b) < 0.00001

data TipSide = LeftTip | RightTip
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data SignatoryDetails = SignatoryDetails {
    signatorysignorder :: SignOrder
  , signatoryfields    :: [SignatoryField]
  , signatoryisauthor  :: Bool -- ^ True if signatory is an author of the document
  , signatoryispartner :: Bool -- ^ True if signatory participates in signing process
  } deriving (Ord, Show)

instance Eq SignatoryDetails where
  SignatoryDetails {signatorysignorder=so1, signatoryfields=sf1, signatoryisauthor = isauthor1, signatoryispartner = ispartner1} ==
    SignatoryDetails {signatorysignorder=so2, signatoryfields=sf2, signatoryisauthor = isauthor2, signatoryispartner = ispartner2} =
      so1 == so2 && (sort sf2) == (sort sf1) && isauthor1 == isauthor2 && ispartner1 == ispartner2

instance HasDefaultValue SignatoryDetails where
  defaultValue = SignatoryDetails
                 { signatorysignorder = SignOrder 1
                 , signatoryfields    = []
                 , signatoryisauthor  = False
                 , signatoryispartner = False
                 }

data SignatoryLink = SignatoryLink {
    signatorylinkid            :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
  , signatorydetails           :: SignatoryDetails    -- ^ details of this person as filled in invitation
  , signatorymagichash         :: MagicHash           -- ^ authentication code
  , maybesignatory             :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
  , maybesigninfo              :: Maybe SignInfo      -- ^ when a person has signed this document
  , maybeseeninfo              :: Maybe SignInfo      -- ^ when a person has first seen this document
  , maybereadinvite            :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read
  , invitationdeliverystatus   :: MailsDeliveryStatus -- ^ status of email delivery
  , signatorysignatureinfo     :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
  , signatorylinkdeleted       :: Bool -- ^ when true sends the doc to the recycle bin for that sig
  , signatorylinkreallydeleted :: Bool -- ^ when true it means that the doc has been removed from the recycle bin
  , signatorylinkcsvupload     :: Maybe CSVUpload
  , signatoryattachments       :: [SignatoryAttachment]
  , signatorylinkstatusclass   :: StatusClass
  , signatorylinksignredirecturl :: Maybe String
  , signatorylinkrejectiontime   :: Maybe MinutesTime
  , signatorylinkrejectionreason :: Maybe String
  , signatorylinkauthenticationmethod   :: AuthenticationMethod
  , signatorylinkelegdatamismatchmessage        :: Maybe String
  , signatorylinkelegdatamismatchfirstname      :: Maybe String
  , signatorylinkelegdatamismatchlastname       :: Maybe String
  , signatorylinkelegdatamismatchpersonalnumber :: Maybe String
  , signatorylinkdeliverymethod         :: DeliveryMethod
  } deriving (Eq, Ord, Show)

instance HasDefaultValue SignatoryLink where
  defaultValue =  SignatoryLink
                  { signatorylinkid              = unsafeSignatoryLinkID 0
                  , signatorydetails             = defaultValue
                  , signatorymagichash           = unsafeMagicHash 0
                  , maybesignatory               = Nothing
                  , maybesigninfo                = Nothing
                  , maybeseeninfo                = Nothing
                  , maybereadinvite              = Nothing
                  , invitationdeliverystatus     = Unknown
                  , signatorysignatureinfo       = Nothing
                  , signatorylinkdeleted         = False
                  , signatorylinkreallydeleted   = False
                  , signatorylinkcsvupload       = Nothing
                  , signatoryattachments         = []
                  , signatorylinkstatusclass     = SCDraft
                  , signatorylinksignredirecturl = Nothing
                  , signatorylinkrejectiontime   = Nothing
                  , signatorylinkrejectionreason = Nothing
                  , signatorylinkauthenticationmethod = StandardAuthentication
                  , signatorylinkelegdatamismatchmessage = Nothing
                  , signatorylinkelegdatamismatchfirstname = Nothing
                  , signatorylinkelegdatamismatchlastname = Nothing
                  , signatorylinkelegdatamismatchpersonalnumber = Nothing
                  , signatorylinkdeliverymethod       = EmailDelivery
                  }

data CSVUpload = CSVUpload {
    csvtitle :: String
  , csvcontents  :: [[String]]
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
  deriving (Eq, Ord)


{- Used by API -}
instance Show DocumentStatus where
  show Preparation = "Preparation"
  show Pending = "Pending"
  show Closed  = "Closed"
  show Canceled  = "Canceled"
  show Timedout  = "Timedout"
  show Rejected = "Rejected"
  show (DocumentError _) = "DocumentError"

data DocumentProcess = Contract | Offer | Order
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data DocumentType = Signable DocumentProcess
                  | Template DocumentProcess
  deriving (Eq, Ord, Show, Read)

instance Convertible DocumentType SqlValue where
  safeConvert v = Right . SqlInt32 $ case v of
    Signable _ -> 1
    Template _ -> 2

documentType :: (Int, DocumentProcess) -> DocumentType
documentType (1, p) = Signable p
documentType (2, p) = Template p
documentType v = error $ "documentType: wrong values: " ++ show v

instance Convertible SqlValue DocumentType where
  safeConvert v = do
    (val :: Int) <- safeConvert v
    case val of
      1 -> return (Signable undefined)
      2 -> return (Template undefined)
      x -> Left (ConvertError { convSourceValue = show v
                              , convSourceType = show (typeOf v)
                              , convDestType = "DocumentType"
                              , convErrorMessage = "Value " ++ show x ++ " is not in [1,2]"
                              })


toDocumentProcess :: DocumentType -> DocumentProcess
toDocumentProcess (Signable p) = p
toDocumentProcess (Template p) = p

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
  , documentfile                   :: Maybe FileID
  , documentsealedfile             :: Maybe FileID
  , documentstatus                 :: DocumentStatus
  , documenttype                   :: DocumentType
  , documentctime                  :: MinutesTime
  , documentmtime                  :: MinutesTime
  , documentdaystosign             :: Int
  , documenttimeouttime            :: Maybe TimeoutTime
  , documentinvitetime             :: Maybe SignInfo
  , documentinvitetext             :: String
  , documentsharing                :: DocumentSharing
  , documenttags                   :: S.Set DocumentTag
  , documentauthorattachments      :: [AuthorAttachment]
  , documentlang                   :: Lang
  , documentstatusclass            :: StatusClass
  , documentapicallbackurl         :: Maybe String
  , documentobjectversion          :: Int
  } deriving (Eq, Ord, Show)


instance HasDefaultValue Document where
  defaultValue = Document
          { documentid                   = unsafeDocumentID 0
          , documenttitle                = ""
          , documentsignatorylinks       = []
          , documentfile                 = Nothing
          , documentstatus               = Preparation
          , documenttype                 = Signable Contract
          , documentctime                = fromSeconds 0
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = 14
          , documenttimeouttime          = Nothing
          , documentinvitetext           = ""
          , documentsealedfile           = Nothing
          , documentinvitetime           = Nothing
          , documentsharing              = Private
          , documenttags                 = S.empty
          , documentauthorattachments    = []
          , documentlang                 = defaultValue
          , documentstatusclass          = SCDraft
          , documentapicallbackurl       = Nothing
          , documentobjectversion        = 0
          }

instance HasLang Document where
  getLang = documentlang

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
  fromJSValueM = do
                  xrel       <- fromJSValueField "xrel"
                  yrel       <- fromJSValueField "yrel"
                  wrel       <- fromJSValueField "wrel"
                  hrel       <- fromJSValueField "hrel"
                  fsrel      <- fromJSValueField "fsrel"
                  page       <- fromJSValueField "page"
                  side       <- fromJSValueField "tip"
                  return (FieldPlacement <$> xrel <*> yrel
                                         <*> wrel <*> hrel <*> fsrel
                                         <*> page <*> Just side)

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
                                             Just LeftTip -> Just ("left" :: String)
                                             Just RightTip -> Just "right"
                                             _ -> Nothing

instance Convertible  SqlValue [FieldPlacement] where
    safeConvert = jsonFromSqlValueCustom $ nothingToResult . (fromJSValueCustomMany fromJSValue)
