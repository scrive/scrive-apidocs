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
  , DeliveryStatus(..)
  , SignatureProvider(..)
  , SignInfo(..)
  , SignOrder(..)
  , SignatoryField(..)
  , FieldType(..)
  , TipSide(..)
  , SignatoryLink(..)
  , SignatureInfo(..)
  , AuthorAttachment(..)
  , SignatoryAttachment(..)
  , StatusClass(..)
  , getFieldOfType
  , getValueOfType
  , documentfile
  , documentsealedfile
  , documentsealstatus
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
import Doc.SealStatus (SealStatus)
import Doc.DocumentID
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
                  | SCProlonged
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
  show SCProlonged = "prolonged"

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
    Just "mobile"-> Just MobileDelivery
    Just "email_mobile"-> Just EmailAndMobileDelivery
    _            -> Nothing

instance ToJSValue DeliveryMethod where
  toJSValue EmailDelivery  = toJSValue "email"
  toJSValue PadDelivery    = toJSValue "pad"
  toJSValue APIDelivery    = toJSValue "api"
  toJSValue MobileDelivery = toJSValue "mobile"
  toJSValue EmailAndMobileDelivery = toJSValue "email_mobile"

data DeliveryMethod = EmailDelivery
                    | PadDelivery
                    | APIDelivery
                    | MobileDelivery
                    | EmailAndMobileDelivery
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
               | MobileFT
  deriving (Eq, Ord, Show, Data, Typeable)

data SignatoryField = SignatoryField
  { sfType       :: FieldType
  , sfValue      :: String
  , sfObligatory :: Bool
  , sfShouldBeFilledBySender :: Bool
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
  , maybereadinvite            :: Maybe MinutesTime   -- ^ when we receive confirmation that a user has read
  , mailinvitationdeliverystatus  :: DeliveryStatus -- ^ status of email delivery
  , smsinvitationdeliverystatus   :: DeliveryStatus -- ^ status of email delivery
  , signatorysignatureinfo     :: Maybe SignatureInfo -- ^ info about what fields have been filled for this person
  , signatorylinkdeleted       :: Maybe MinutesTime   -- ^ when was put in recycle bin
  , signatorylinkreallydeleted :: Maybe MinutesTime   -- ^ when was purged from the system
  , signatorylinkcsvupload     :: Maybe CSVUpload
  , signatoryattachments       :: [SignatoryAttachment]
  , signatorylinkstatusclass   :: StatusClass
  , signatorylinksignredirecturl :: Maybe String
  , signatorylinkrejectredirecturl :: Maybe String
  , signatorylinkrejectiontime   :: Maybe MinutesTime
  , signatorylinkrejectionreason :: Maybe String
  , signatorylinkauthenticationmethod   :: AuthenticationMethod
  , signatorylinkelegdatamismatchmessage        :: Maybe String
  , signatorylinkelegdatamismatchfirstname      :: Maybe String
  , signatorylinkelegdatamismatchlastname       :: Maybe String
  , signatorylinkelegdatamismatchpersonalnumber :: Maybe String
  , signatorylinkdeliverymethod         :: DeliveryMethod
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
    signatorylinkdeliverymethod s1 == signatorylinkdeliverymethod s2

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


data DocumentType = Signable | Template
  deriving (Eq, Ord, Show, Read)

instance Convertible DocumentType SqlValue where
  safeConvert v = Right . SqlInt32 $ case v of
    Signable -> 1
    Template -> 2

instance Convertible SqlValue DocumentType where
  safeConvert v = do
    (val :: Int) <- safeConvert v
    case val of
      1 -> return Signable
      2 -> return Template
      x -> Left (ConvertError { convSourceValue = show v
                              , convSourceType = show (typeOf v)
                              , convDestType = "DocumentType"
                              , convErrorMessage = "Value " ++ show x ++ " is not in [1,2]"
                              })

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

getValueOfType :: FieldType -> SignatoryLink -> String
getValueOfType t = fromMaybe "" . fmap sfValue . getFieldOfType t . signatoryfields

data Document = Document {
    documentid                     :: DocumentID
  , documenttitle                  :: String
  , documentsignatorylinks         :: [SignatoryLink]
  , documentmainfiles              :: [MainFile] -- sorted in descending order w.r.t. FileID
  , documentstatus                 :: DocumentStatus
  , documenttype                   :: DocumentType
  , documentctime                  :: MinutesTime
  , documentmtime                  :: MinutesTime
  , documentdaystosign             :: Int
  , documenttimeouttime            :: Maybe MinutesTime
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
          , documentmainfiles            = []
          , documentstatus               = Preparation
          , documenttype                 = Signable
          , documentctime                = fromSeconds 0
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = 14
          , documenttimeouttime          = Nothing
          , documentinvitetext           = ""
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

data MainFile = MainFile
  { mainfileid             :: FileID           -- ^ pointer to the actual file
  , mainfiledocumentstatus :: DocumentStatus   -- ^ Preparation if and only if this is not a sealed file
  , mainfilesealstatus     :: Maybe SealStatus -- ^ for files in Preparation and sealed files with unknown status: Nothing
  } deriving (Eq, Ord, Show)

documentfile :: Document -> Maybe FileID
documentfile = fmap mainfileid . find ((==Preparation) . mainfiledocumentstatus) . documentmainfiles

-- Here, we assume that the most recently sealed file is closed to the head of the list
documentsealedfile' :: Document -> Maybe MainFile
documentsealedfile' = find ((/=Preparation) . mainfiledocumentstatus) . documentmainfiles

documentsealedfile :: Document -> Maybe FileID
documentsealedfile = fmap mainfileid . documentsealedfile'

documentsealstatus :: Document -> Maybe SealStatus
documentsealstatus = (>>=mainfilesealstatus) . documentsealedfile'

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

data DeliveryStatus = Delivered
                         | Undelivered
                         | Unknown
                         | Deferred
                           deriving (Eq, Ord, Show)

-- stuff for converting to pgsql

$(enumDeriveConvertible ''DeliveryStatus)
$(newtypeDeriveConvertible ''SignOrder)
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
