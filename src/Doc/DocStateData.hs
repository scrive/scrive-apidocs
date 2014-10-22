{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocStateData (
    CSVUpload(..)
  , CancelationReason(..)
  , Document(..)
  , MainFile(..)
  , DocumentSharing(..)
  , DocumentStatus(..)
  , DocumentTag(..)
  , DocumentType(..)
  , FieldPlacement(..)
  , AuthenticationMethod(..)
  , DeliveryMethod(..)
  , ConfirmationDeliveryMethod(..)
  , DeliveryStatus(..)
  , SignatureProvider(..)
  , SignInfo(..)
  , SignOrder(..)
  , SignatoryFieldValue(..)
  , sfvNull
  , getTextField
  , getBinaryField
  , sfvEncode
  , SignatoryField(..)
  , FieldType(..)
  , TipSide(..)
  , PlacementAnchor(..)
  , SignatoryLink(..)
  , SignatureInfo(..)
  , AuthorAttachment(..)
  , SignatoryAttachment(..)
  , StatusClass(..)
  , getFieldOfType
  , getValueOfType
  , getTextValueOfType
  , documentfile
  , documentsealedfile
  , documentsealstatus
  ) where

import Control.Applicative
import Control.Monad
import Data.Data
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Monoid.Space
import Data.String
import Database.PostgreSQL.PQTypes
import Text.JSON
import Text.JSON.FromJSValue
import Text.JSON.Gen
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S

import API.APIVersion
import Company.CompanyID
import DB.Derive
import DB.RowCache (ID, HasID(..))
import DB.TimeZoneName
import Doc.DocumentID
import Doc.SealStatus (SealStatus, HasGuardtimeSignature(..))
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import ELegitimation.SignatureProvider
import File.FileID
import IPAddress
import MagicHash
import MinutesTime
import User.Lang
import User.UserID
import Utils.Default
import Utils.Image

newtype SignOrder = SignOrder { unSignOrder :: Int32 }
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''SignOrder)

instance FromSQL SignOrder where
  type PQBase SignOrder = PQBase Int32
  fromSQL mbase = SignOrder <$> fromSQL mbase
instance ToSQL SignOrder where
  type PQDest SignOrder = PQDest Int32
  toSQL (SignOrder n) = toSQL n

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
                  | SCProlonged
                  | SCSealed -- has a digital seal
                  | SCExtended -- has an extended digital seal
                  | SCInitiated
                  deriving (Eq, Ord, Enum, Bounded)

instance PQFormat StatusClass where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL StatusClass where
  type PQBase StatusClass = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return SCDraft
      2 -> return SCCancelled
      3 -> return SCRejected
      4 -> return SCTimedout
      5 -> return SCError
      6 -> return SCDeliveryProblem
      7 -> return SCSent
      8 -> return SCDelivered
      9 -> return SCRead
      10 -> return SCOpened
      11 -> return SCSigned
      12 -> return SCProlonged
      13 -> return SCSealed
      14 -> return SCExtended
      15 -> return SCInitiated
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 15)]
      , reValue = n
      }

instance ToSQL StatusClass where
  type PQDest StatusClass = PQDest Int16
  toSQL SCDraft           = toSQL (1::Int16)
  toSQL SCCancelled       = toSQL (2::Int16)
  toSQL SCRejected        = toSQL (3::Int16)
  toSQL SCTimedout        = toSQL (4::Int16)
  toSQL SCError           = toSQL (5::Int16)
  toSQL SCDeliveryProblem = toSQL (6::Int16)
  toSQL SCSent            = toSQL (7::Int16)
  toSQL SCDelivered       = toSQL (8::Int16)
  toSQL SCRead            = toSQL (9::Int16)
  toSQL SCOpened          = toSQL (10::Int16)
  toSQL SCSigned          = toSQL (11::Int16)
  toSQL SCProlonged       = toSQL (12::Int16)
  toSQL SCSealed          = toSQL (13::Int16)
  toSQL SCExtended        = toSQL (14::Int16)
  toSQL SCInitiated       = toSQL (15::Int16)

instance Show StatusClass where
  show SCInitiated = "initiated"
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
  show SCProlonged = "prolonged"
  show SCSealed = "sealed"
  show SCExtended = "extended"

instance Read StatusClass where
  readsPrec _ str =
    [(v,drop (length (show v)) str) | v <- [minBound .. maxBound], show v `isPrefixOf` str]

data AuthenticationMethod = StandardAuthentication
                          | ELegAuthentication
                          | SMSPinAuthentication
  deriving (Eq, Ord, Show)

instance PQFormat AuthenticationMethod where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL AuthenticationMethod where
  type PQBase AuthenticationMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return StandardAuthentication
      2 -> return ELegAuthentication
      3 -> return SMSPinAuthentication
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 3)]
      , reValue = n
      }

instance ToSQL AuthenticationMethod where
  type PQDest AuthenticationMethod = PQDest Int16
  toSQL StandardAuthentication = toSQL (1::Int16)
  toSQL ELegAuthentication     = toSQL (2::Int16)
  toSQL SMSPinAuthentication   = toSQL (3::Int16)

data DeliveryMethod = EmailDelivery
                    | PadDelivery
                    | APIDelivery
                    | MobileDelivery
                    | EmailAndMobileDelivery
  deriving (Eq, Ord, Show)

instance PQFormat DeliveryMethod where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL DeliveryMethod where
  type PQBase DeliveryMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EmailDelivery
      2 -> return PadDelivery
      3 -> return APIDelivery
      4 -> return MobileDelivery
      5 -> return EmailAndMobileDelivery
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL DeliveryMethod where
  type PQDest DeliveryMethod = PQDest Int16
  toSQL EmailDelivery          = toSQL (1::Int16)
  toSQL PadDelivery            = toSQL (2::Int16)
  toSQL APIDelivery            = toSQL (3::Int16)
  toSQL MobileDelivery         = toSQL (4::Int16)
  toSQL EmailAndMobileDelivery = toSQL (5::Int16)

data ConfirmationDeliveryMethod = EmailConfirmationDelivery
                    | MobileConfirmationDelivery
                    | EmailAndMobileConfirmationDelivery
                    | NoConfirmationDelivery
  deriving (Eq, Ord, Show)

instance PQFormat ConfirmationDeliveryMethod where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL ConfirmationDeliveryMethod where
  type PQBase ConfirmationDeliveryMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return EmailConfirmationDelivery
      2 -> return MobileConfirmationDelivery
      3 -> return EmailAndMobileConfirmationDelivery
      4 -> return NoConfirmationDelivery
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 4)]
      , reValue = n
      }

instance ToSQL ConfirmationDeliveryMethod where
  type PQDest ConfirmationDeliveryMethod = PQDest Int16
  toSQL EmailConfirmationDelivery           = toSQL (1::Int16)
  toSQL MobileConfirmationDelivery          = toSQL (2::Int16)
  toSQL EmailAndMobileConfirmationDelivery  = toSQL (3::Int16)
  toSQL NoConfirmationDelivery              = toSQL (4::Int16)

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
               | MobileFT
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat FieldType where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL FieldType where
  type PQBase FieldType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return FirstNameFT
      2 -> return LastNameFT
      3 -> return CompanyFT
      4 -> return PersonalNumberFT
      5 -> return CompanyNumberFT
      6 -> return EmailFT
      7 -> return $ CustomFT undefinedField undefinedField
      8 -> return $ SignatureFT undefinedField
      9 -> return $ CheckboxFT undefinedField
      10 -> return MobileFT
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 10)]
      , reValue = n
      }
    where
      undefinedField = error "fromSQL (FieldType): field undefined"

instance ToSQL FieldType where
  type PQDest FieldType = PQDest Int16
  toSQL FirstNameFT      = toSQL (1::Int16)
  toSQL LastNameFT       = toSQL (2::Int16)
  toSQL CompanyFT        = toSQL (3::Int16)
  toSQL PersonalNumberFT = toSQL (4::Int16)
  toSQL CompanyNumberFT  = toSQL (5::Int16)
  toSQL EmailFT          = toSQL (6::Int16)
  toSQL CustomFT{}       = toSQL (7::Int16)
  toSQL SignatureFT{}    = toSQL (8::Int16)
  toSQL CheckboxFT{}     = toSQL (9::Int16)
  toSQL MobileFT         = toSQL (10::Int16)

data SignatoryFieldValue = TextField String | BinaryField BS.ByteString
  deriving (Ord, Eq, Show, Data, Typeable)

instance IsString SignatoryFieldValue where
  fromString = TextField

sfvNull :: SignatoryFieldValue -> Bool
sfvNull (TextField s) = null s
sfvNull (BinaryField bs) = BS.null bs

getTextField :: SignatoryFieldValue -> Maybe String
getTextField (TextField s) = Just s
getTextField _ = Nothing

getBinaryField :: SignatoryFieldValue -> Maybe BS.ByteString
getBinaryField (BinaryField bs) = Just bs
getBinaryField _ = Nothing

-- | Encode 'SignatoryFieldValue' to be used in a template
sfvEncode :: FieldType -> SignatoryFieldValue -> String
sfvEncode ft v = case (ft, v) of
  (SignatureFT{}, BinaryField "") -> "" -- FIXME: this case shouldn't be needed
  (SignatureFT{}, BinaryField bs) -> BS.unpack $ imgEncodeRFC2397 bs
  (SignatureFT{}, TextField{})    -> failure
  (_, TextField s)                -> s
  (_, BinaryField{})              -> failure
  where
    failure = error $ "sfvEncode: field of type" <+> show ft <+> "has the following value:" <+> show v <> ", this is not supposed to happen"

data SignatoryField = SignatoryField
  { sfID                     :: SignatoryFieldID
  , sfType                   :: FieldType
  , sfValue                  :: SignatoryFieldValue
  , sfObligatory             :: Bool
  , sfShouldBeFilledBySender :: Bool
  , sfPlacements             :: [FieldPlacement]
  } deriving (Ord, Show, Data, Typeable)

instance Eq SignatoryField where
  a == b = sfType a == sfType b &&
           sfValue a == sfValue b &&
           sfObligatory a == sfObligatory b &&
           sfShouldBeFilledBySender a == sfShouldBeFilledBySender b &&
           sfPlacements a == sfPlacements b

data PlacementAnchor = PlacementAnchor
  { placementanchortext  :: String
  , placementanchorindex :: Int
  , placementanchorpages :: [Int]
  } deriving (Eq, Ord, Show, Data, Typeable)

data FieldPlacement = FieldPlacement
  { placementxrel       :: Double
  , placementyrel       :: Double
  , placementwrel       :: Double
  , placementhrel       :: Double
  , placementfsrel      :: Double
  , placementpage       :: Int
  , placementtipside    :: Maybe TipSide
  , placementanchors    :: [PlacementAnchor]
  } deriving (Ord, Show, Data, Typeable)

instance Eq FieldPlacement where
    (==) a b = all id [ eqByEpsilon placementxrel
                      , eqByEpsilon placementyrel
                      , eqByEpsilon placementwrel
                      , eqByEpsilon placementhrel
                      , eqByEpsilon placementfsrel
                      , placementpage a == placementpage b
                      , placementtipside a == placementtipside b
                      , placementanchors a == placementanchors b
                      ]
      where
        eqByEpsilon func = abs (func a - func b) < 0.00001

data TipSide = LeftTip | RightTip
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data SignatoryLink = SignatoryLink {
    signatorylinkid            :: SignatoryLinkID     -- ^ a random number id, unique in th escope of a document only
  , signatoryfields            :: [SignatoryField]
  , signatoryisauthor          :: Bool -- ^ True if signatory is an author of the document
  , signatoryispartner         :: Bool -- ^ True if signatory participates in signing process
  , signatorysignorder         :: SignOrder
  , signatorymagichash         :: MagicHash           -- ^ authentication code
  , maybesignatory             :: Maybe UserID        -- ^ if this document has been saved to an account, that is the user id
  , maybesigninfo              :: Maybe SignInfo      -- ^ when a person has signed this document
  , maybeseeninfo              :: Maybe SignInfo      -- ^ when a person has first seen this document
  , maybereadinvite            :: Maybe UTCTime   -- ^ when we receive confirmation that a user has read
  , mailinvitationdeliverystatus  :: DeliveryStatus -- ^ status of email delivery
  , smsinvitationdeliverystatus   :: DeliveryStatus -- ^ status of email delivery
  , signatorysignatureinfo     :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
  , signatorylinkdeleted       :: Maybe UTCTime   -- ^ when was put in recycle bin
  , signatorylinkreallydeleted :: Maybe UTCTime   -- ^ when was purged from the system
  , signatorylinkcsvupload     :: Maybe CSVUpload
  , signatoryattachments       :: [SignatoryAttachment]
  , signatorylinkstatusclass   :: StatusClass
  , signatorylinksignredirecturl :: Maybe String
  , signatorylinkrejectredirecturl :: Maybe String
  , signatorylinkrejectiontime   :: Maybe UTCTime
  , signatorylinkrejectionreason :: Maybe String
  , signatorylinkauthenticationmethod   :: AuthenticationMethod
  , signatorylinkelegdatamismatchmessage        :: Maybe String
  , signatorylinkelegdatamismatchfirstname      :: Maybe String
  , signatorylinkelegdatamismatchlastname       :: Maybe String
  , signatorylinkelegdatamismatchpersonalnumber :: Maybe String
  , signatorylinkdeliverymethod         :: DeliveryMethod
  , signatorylinkconfirmationdeliverymethod :: ConfirmationDeliveryMethod
  } deriving (Ord, Show)

-- | Drop this instance when we introduce Set SignatoryFields intead of list
instance Eq SignatoryLink where
  s1 == s2 =
    signatorylinkid s1 == signatorylinkid s2 &&
    sort (signatoryfields s1) == sort (signatoryfields s2) &&
    signatoryisauthor s1 == signatoryisauthor s2 &&
    signatoryispartner s1 == signatoryispartner s2 &&
    signatorysignorder s1 == signatorysignorder s2 &&
    signatorymagichash s1 == signatorymagichash s2 &&
    maybesignatory s1 == maybesignatory s2 &&
    maybesigninfo s1 == maybesigninfo s2 &&
    maybeseeninfo s1 == maybeseeninfo s2 &&
    maybereadinvite s1 == maybereadinvite s2 &&
    mailinvitationdeliverystatus s1 == mailinvitationdeliverystatus s2 &&
    smsinvitationdeliverystatus s1 == smsinvitationdeliverystatus s2 &&
    signatorysignatureinfo s1 == signatorysignatureinfo s2 &&
    signatorylinkdeleted s1 == signatorylinkdeleted s2 &&
    signatorylinkreallydeleted s1 == signatorylinkreallydeleted s2 &&
    signatorylinkcsvupload s1 == signatorylinkcsvupload s2 &&
    signatoryattachments s1 == signatoryattachments s2 &&
    signatorylinkstatusclass s1 == signatorylinkstatusclass s2 &&
    signatorylinksignredirecturl s1 == signatorylinksignredirecturl s2 &&
    signatorylinkrejectiontime s1 == signatorylinkrejectiontime s2 &&
    signatorylinkrejectionreason s1 == signatorylinkrejectionreason s2 &&
    signatorylinkauthenticationmethod s1 == signatorylinkauthenticationmethod s2 &&
    signatorylinkelegdatamismatchmessage s1 == signatorylinkelegdatamismatchmessage s2 &&
    signatorylinkelegdatamismatchfirstname s1 == signatorylinkelegdatamismatchfirstname s2 &&
    signatorylinkelegdatamismatchlastname s1 == signatorylinkelegdatamismatchlastname s2 &&
    signatorylinkelegdatamismatchpersonalnumber s1 == signatorylinkelegdatamismatchpersonalnumber s2 &&
    signatorylinkdeliverymethod s1 == signatorylinkdeliverymethod s2 &&
    signatorylinkconfirmationdeliverymethod s1 == signatorylinkconfirmationdeliverymethod s2

instance HasDefaultValue SignatoryLink where
  defaultValue =  SignatoryLink
                  { signatorylinkid              = unsafeSignatoryLinkID 0
                  , signatoryfields              = []
                  , signatoryisauthor            = False
                  , signatoryispartner           = False
                  , signatorysignorder           = SignOrder 1
                  , signatorymagichash           = unsafeMagicHash 0
                  , maybesignatory               = Nothing
                  , maybesigninfo                = Nothing
                  , maybeseeninfo                = Nothing
                  , maybereadinvite              = Nothing
                  , mailinvitationdeliverystatus = Unknown
                  , smsinvitationdeliverystatus  = Unknown
                  , signatorysignatureinfo       = Nothing
                  , signatorylinkdeleted         = Nothing
                  , signatorylinkreallydeleted   = Nothing
                  , signatorylinkcsvupload       = Nothing
                  , signatoryattachments         = []
                  , signatorylinkstatusclass     = SCDraft
                  , signatorylinksignredirecturl = Nothing
                  , signatorylinkrejectredirecturl = Nothing
                  , signatorylinkrejectiontime   = Nothing
                  , signatorylinkrejectionreason = Nothing
                  , signatorylinkauthenticationmethod = StandardAuthentication
                  , signatorylinkelegdatamismatchmessage = Nothing
                  , signatorylinkelegdatamismatchfirstname = Nothing
                  , signatorylinkelegdatamismatchlastname = Nothing
                  , signatorylinkelegdatamismatchpersonalnumber = Nothing
                  , signatorylinkdeliverymethod       = EmailDelivery
                  , signatorylinkconfirmationdeliverymethod = EmailConfirmationDelivery
                  }

data CSVUpload = CSVUpload {
    csvtitle :: String
  , csvcontents  :: [[String]]
  } deriving (Eq, Ord, Show)

data SignInfo = SignInfo {
    signtime :: UTCTime
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

instance PQFormat DocumentStatus where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL DocumentStatus where
  type PQBase DocumentStatus = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Preparation
      2 -> return Pending
      3 -> return Closed
      4 -> return Canceled
      5 -> return Timedout
      6 -> return Rejected
      7 -> return . DocumentError $ error "fromSQL (DocumentStatus): field undefined"
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 7)]
      , reValue = n
      }

instance ToSQL DocumentStatus where
  type PQDest DocumentStatus = PQDest Int16
  toSQL Preparation     = toSQL (1::Int16)
  toSQL Pending         = toSQL (2::Int16)
  toSQL Closed          = toSQL (3::Int16)
  toSQL Canceled        = toSQL (4::Int16)
  toSQL Timedout        = toSQL (5::Int16)
  toSQL Rejected        = toSQL (6::Int16)
  toSQL DocumentError{} = toSQL (7::Int16)

{- Used by API -}
instance Show DocumentStatus where
  show Preparation = "Preparation"
  show Pending = "Pending"
  show Closed  = "Closed"
  show Canceled  = "Canceled"
  show Timedout  = "Timedout"
  show Rejected = "Rejected"
  show (DocumentError _) = "DocumentError"


data DocumentType = Signable | Template
  deriving (Eq, Ord, Show, Read)

instance PQFormat DocumentType where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL DocumentType where
  type PQBase DocumentType = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Signable
      2 -> return Template
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL DocumentType where
  type PQDest DocumentType = PQDest Int16
  toSQL Signable = toSQL (1::Int16)
  toSQL Template = toSQL (2::Int16)

data DocumentSharing = Private
                     | Shared -- means that the document is shared with subaccounts, and those with same parent accounts
  deriving (Eq, Ord, Show)

instance PQFormat DocumentSharing where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL DocumentSharing where
  type PQBase DocumentSharing = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Private
      2 -> return Shared
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL DocumentSharing where
  type PQDest DocumentSharing = PQDest Int16
  toSQL Private = toSQL (1::Int16)
  toSQL Shared  = toSQL (2::Int16)

data DocumentTag = DocumentTag {
    tagname :: String
  , tagvalue :: String
  } deriving (Eq, Show, Data, Typeable)

instance PQFormat [DocumentTag] where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL [DocumentTag] where
  type PQBase [DocumentTag] = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL [DocumentTag] where
  type PQDest [DocumentTag] = PQDest String
  toSQL = jsonToSQL

 -- for inserting into set
instance Ord DocumentTag where
  a `compare` b = tagname a `compare` tagname b

getFieldOfType :: FieldType -> [SignatoryField] -> Maybe SignatoryField
getFieldOfType t = find ((t ==) . sfType)

getValueOfType :: FieldType -> [SignatoryField] -> SignatoryFieldValue
getValueOfType t = fromMaybe def . fmap sfValue . getFieldOfType t
  where
    def = case t of
      SignatureFT{} -> BinaryField BS.empty
      _             -> TextField ""

getTextValueOfType :: FieldType -> [SignatoryField] -> String
getTextValueOfType t = fromMaybe "" . getTextField . getValueOfType t

data Document = Document {
    documentid                     :: DocumentID
  , documenttitle                  :: String
  , documentsignatorylinks         :: [SignatoryLink]
  , documentmainfiles              :: [MainFile] -- order: most recently added files first
  , documentstatus                 :: DocumentStatus
  , documenttype                   :: DocumentType
  , documentctime                  :: UTCTime
  , documentmtime                  :: UTCTime
  , documentdaystosign             :: Int32
  , documentdaystoremind           :: Maybe Int32
  , documenttimeouttime            :: Maybe UTCTime
  , documentautoremindtime         :: Maybe UTCTime
  , documentinvitetime             :: Maybe SignInfo
  , documentinvitetext             :: String
  , documentconfirmtext            :: String
  , documentshowheader             :: Bool
  , documentshowpdfdownload        :: Bool
  , documentshowrejectoption       :: Bool
  , documentshowfooter             :: Bool
  , documentsharing                :: DocumentSharing
  , documenttags                   :: S.Set DocumentTag
  , documentauthorattachments      :: [AuthorAttachment]
  , documentlang                   :: Lang
  , documentstatusclass            :: StatusClass
  , documentapicallbackurl         :: Maybe String
  , documentunsaveddraft           :: Bool
  , documentobjectversion          :: Int64
  , documentmagichash              :: MagicHash
  , documentauthorcompanyid        :: Maybe CompanyID
  , documenttimezonename           :: TimeZoneName
  , documentapiversion             :: APIVersion
  } deriving (Eq, Ord, Show)


instance HasDefaultValue Document where
  defaultValue = Document
          { documentid                   = unsafeDocumentID 0
          , documenttitle                = ""
          , documentsignatorylinks       = []
          , documentmainfiles            = []
          , documentstatus               = Preparation
          , documenttype                 = Signable
          , documentctime                = unixEpoch
          , documentmtime                = unixEpoch
          , documentdaystosign           = 14
          , documentdaystoremind         = Nothing
          , documenttimeouttime          = Nothing
          , documentautoremindtime       = Nothing
          , documentshowheader           = True
          , documentshowpdfdownload      = True
          , documentshowrejectoption     = True
          , documentshowfooter           = True
          , documentinvitetext           = ""
          , documentconfirmtext          = ""
          , documentinvitetime           = Nothing
          , documentsharing              = Private
          , documenttags                 = S.empty
          , documentauthorattachments    = []
          , documentlang                 = defaultValue
          , documentstatusclass          = SCDraft
          , documentapicallbackurl       = Nothing
          , documentunsaveddraft         = False
          , documentobjectversion        = 0
          , documentmagichash            = unsafeMagicHash 0
          , documentauthorcompanyid      = Nothing
          , documenttimezonename         = defaultTimeZoneName
          , documentapiversion           = defaultValue
          }

instance HasLang Document where
  getLang = documentlang

data MainFile = MainFile
  { mainfileid             :: FileID         -- ^ pointer to the actual file
  , mainfiledocumentstatus :: DocumentStatus -- ^ Preparation if and only if this is not a sealed file
  , mainfilesealstatus     :: SealStatus     -- ^ for files in Preparation: Missing.
  } deriving (Eq, Ord, Show)

documentfile :: Document -> Maybe FileID
documentfile = fmap mainfileid . find ((==Preparation) . mainfiledocumentstatus) . documentmainfiles

-- Here, we assume that the most recently sealed file is closest to the head of the list
documentsealedfile' :: Document -> Maybe MainFile
documentsealedfile' = find ((/=Preparation) . mainfiledocumentstatus) . documentmainfiles

documentsealedfile :: Document -> Maybe FileID
documentsealedfile = fmap mainfileid . documentsealedfile'

documentsealstatus :: Document -> Maybe SealStatus
documentsealstatus = fmap mainfilesealstatus . documentsealedfile'

instance HasGuardtimeSignature Document where
  hasGuardtimeSignature doc = (hasGuardtimeSignature <$> documentsealstatus doc) == Just True

data CancelationReason = ManualCancel
                        -- The data returned by ELeg server
                        --                 msg                    fn            ln            num
                        | ELegDataMismatch String SignatoryLinkID String String String
  deriving (Eq, Ord, Show, Data, Typeable)

instance PQFormat CancelationReason where
  pqFormat _ = pqFormat (undefined::String)

instance FromSQL CancelationReason where
  type PQBase CancelationReason = PQBase String
  fromSQL = jsonFromSQL

instance ToSQL CancelationReason where
  type PQDest CancelationReason = PQDest String
  toSQL = jsonToSQL

newtype AuthorAttachment = AuthorAttachment { authorattachmentfile :: FileID }
  deriving (Eq, Ord, Show)

data SignatoryAttachment = SignatoryAttachment {
    signatoryattachmentfile            :: Maybe FileID
  , signatoryattachmentname            :: String
  , signatoryattachmentdescription     :: String
  } deriving (Eq, Ord, Show)

data DeliveryStatus = Delivered
                         | Undelivered
                         | Unknown
                         | Deferred
                           deriving (Eq, Ord, Show)

instance PQFormat DeliveryStatus where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL DeliveryStatus where
  type PQBase DeliveryStatus = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Delivered
      2 -> return Undelivered
      3 -> return Unknown
      4 -> return Deferred
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 4)]
      , reValue = n
      }

instance ToSQL DeliveryStatus where
  type PQDest DeliveryStatus = PQDest Int16
  toSQL Delivered = toSQL (1::Int16)
  toSQL Undelivered = toSQL (2::Int16)
  toSQL Unknown = toSQL (3::Int16)
  toSQL Deferred = toSQL (4::Int16)



instance PQFormat [FieldPlacement] where
  pqFormat _ = pqFormat (undefined::String)

instance FromSQL [FieldPlacement] where
  type PQBase [FieldPlacement] = PQBase String
  fromSQL = jsonFromSQL' $ fmap nothingToResult $ fromJSValueCustomMany $ do
                  xrel       <- fromJSValueField "xrel"
                  yrel       <- fromJSValueField "yrel"
                  wrel       <- fromJSValueField "wrel"
                  hrel       <- fromJSValueField "hrel"
                  fsrel      <- fromJSValueField "fsrel"
                  page       <- fromJSValueField "page"
                  side       <- fromJSValueFieldCustom "tip" $ do
                                  s <- fromJSValue
                                  case s of
                                      Just "left"  -> return $ Just LeftTip
                                      Just "right" -> return $ Just RightTip
                                      _ ->            return $ Nothing
                  anchors    <- fmap (fromMaybe []) <$> fromJSValueFieldCustom "anchors" $ fromJSValueCustomMany $ do
                                  text        <- fromJSValueField "text"
                                  index       <- fromMaybe (Just 1) <$> fromJSValueField "index"
                                  pages       <- fromJSValueField "pages"
                                  return (PlacementAnchor <$> text
                                                          <*> index
                                                          <*> pages)
                  return (FieldPlacement <$> xrel <*> yrel
                                         <*> wrel <*> hrel <*> fsrel
                                         <*> page <*> Just side
                                         <*> Just anchors)

instance ToSQL [FieldPlacement] where
  type PQDest [FieldPlacement] = PQDest String
  toSQL = jsonToSQL' $ JSArray . (map placementJSON)
    where
      placementJSON placement = runJSONGen $ do
        value "xrel" $ placementxrel placement
        value "yrel" $ placementyrel placement
        value "wrel" $ placementwrel placement
        value "hrel" $ placementhrel placement
        value "fsrel" $ placementfsrel placement
        value "page" $ placementpage placement
        when (not (null (placementanchors placement))) $ do
          value "anchors" $ (flip map) (placementanchors placement) $ \anchor -> runJSONGen $ do
              value "text" (placementanchortext anchor)
              when (placementanchorindex anchor /=1 ) $ do
                value "index" (placementanchorindex anchor)
              value "pages" (placementanchorpages anchor)
        value "tip" $ case (placementtipside placement) of
          Just LeftTip -> Just ("left" :: String)
          Just RightTip -> Just "right"
          _ -> Nothing

type instance ID Document = DocumentID

instance HasID Document where
  getID = documentid

---------------------------------

instance PQFormat [[String]] where
  pqFormat _ = pqFormat (undefined::String)
instance FromSQL [[String]] where
  type PQBase [[String]] = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL [[String]] where
  type PQDest [[String]] = PQDest String
  toSQL = jsonToSQL
