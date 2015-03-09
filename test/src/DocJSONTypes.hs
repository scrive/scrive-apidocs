module DocJSONTypes where

import Data.Int
import Control.Applicative
import Data.Unjson
import Data.Time
import System.Locale

import DB.Derive
import Utils.Read

import MagicHash

newtype Int64AsString = Int64AsString Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''Int64AsString)

fromInt64AsString :: Int64AsString -> Int64
fromInt64AsString (Int64AsString i) = i

unjsonInt64AsString :: UnjsonDef Int64AsString
unjsonInt64AsString = unjsonInvmapR
                        ((maybe (fail "Can't parse Int64AsString") return) . maybeRead)
                        (show . fromInt64AsString)
                        unjsonDef
instance Unjson Int64AsString where
  unjsonDef = unjsonInt64AsString

unjsonMagicHash :: UnjsonDef MagicHash
unjsonMagicHash = unjsonInvmapR
                    ((maybe (fail "Can't parse MagicHash") return) . maybeRead)
                    (show . unMagicHash)
                    unjsonDef

data Doc = Doc
  { docid                  :: Int64AsString
  , doctitle               :: String
  , docfile                :: Maybe File
  , docsealedfile          :: Maybe File
  , docauthorattachments   :: [File]
  , docevidenceattachments :: [File]
  , docmtime               :: UTCTime
  , docctime               :: UTCTime
  , doctimeouttime         :: Maybe UTCTime
  , docautoremindtime      :: Maybe UTCTime
  , docstatus              :: DocStatus
  , docstate               :: DocStatus
  , docsignatorylinks      :: [SigLink]
  , docsignorder           :: Int32
  , docauthentication      :: DocAuth
  , docdelivery            :: DocDelivery
  , doctemplate            :: Bool
  , docdaystosign        :: Int32
  , docdaystoremind      :: Maybe Int32
  , docshowheader        :: Bool
  , docshowpdfdownload   :: Bool
  , docshowrejectoption  :: Bool
  , docshowfooter        :: Bool
  , docinvitetext        :: String
  , docconfirmtext       :: String
  , doclang              :: DocLang
  , doctags              :: [DocTag]
  , docapicallbackurl    :: (Maybe String)
  , docunsaveddraft      :: Bool
  , docdeleted           :: Bool
  , docreallydeleted     :: Bool
  , doccanperformsigning :: Bool
  , docobjectversion     :: Int64
  , docprocess           :: DocProcess
  , docisviewedbyauthor  :: Bool
  , docaccesstoken       :: MagicHash
  -- FIXME check for this? we depend on SQL stuff to generate this anyway...
  , doctimezonename      :: String
  }

docUnjsonDef :: UnjsonDef Doc
docUnjsonDef = objectOf $ pure Doc
  <*> field "id"                  docid                   "Doc id"
  <*> field "title"               doctitle                "Doc title"
  <*> fieldOpt "file"             docfile                 ""
  <*> fieldOpt "sealedfile"       docsealedfile           ""
  <*> field "authorattachments"   docauthorattachments    ""
  <*> field "evidenceattachments" docevidenceattachments  ""
  <*> fieldBy "time"              docmtime                "" unjsonISOTime
  <*> fieldBy "ctime"             docctime                "" unjsonISOTime
  <*> fieldOptBy "timeouttime"    doctimeouttime          "" unjsonISOTime
  <*> fieldOptBy "autoremindtime" docautoremindtime       "" unjsonISOTime
  <*> field "status"              docstatus               ""
  <*> field "state"               docstate                ""
  <*> field "signatories"         docsignatorylinks "signatories"
  <*> field "signorder"           docsignorder            ""
  <*> field "authentication"      docauthentication       ""
  <*> field "delivery"            docdelivery             ""
  <*> field "template"            doctemplate             ""
  <*> field "daystosign"   docdaystosign   "daystosign"
  <*> fieldOpt "daystoremind" docdaystoremind "daystoremind"
  <*> field "showheader"       docshowheader       "showheader"
  <*> field "showpdfdownload"  docshowpdfdownload  "showpdfdownload"
  <*> field "showrejectoption" docshowrejectoption "showrejectoption"
  <*> field "showfooter"       docshowfooter       "showfooter"
  <*> field "invitationmessage"   docinvitetext  "docinvitetext"    -- FIXME check for <p></p>?
  <*> field "confirmationmessage" docconfirmtext "docconfirmtext"   -- FIXMe check for <p></p>?
  <*> field "lang"                  doclang                ""
  <*> field "tags"                  doctags                ""
  <*> fieldOpt "apicallbackurl" docapicallbackurl "apicallbackurl"
  <*> field "saved"                 docunsaveddraft "saved"
  <*> field "deleted"               docdeleted             ""
  <*> field "reallydeleted"         docreallydeleted       ""
  <*> field "canperformsigning"     doccanperformsigning   ""
  <*> field "objectversion"         docobjectversion       "objectversion"
  <*> field "process"               docprocess             ""
  <*> field "isviewedbyauthor"      docisviewedbyauthor    ""
  <*> fieldBy "accesstoken"         docaccesstoken         "" unjsonMagicHash
  <*> field "timezone"              doctimezonename        ""

data DocStatus = DocStatusPreparation | DocStatusPending |DocStatusClosed
               | DocStatusCanceled | DocStatusTimedout | DocStatusRejected
               | DocStatusDocumentError
  deriving (Show)
instance Unjson DocStatus where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocStatus") return) . readDocStatus)
                show
                unjsonDef
    where readDocStatus :: String -> Maybe DocStatus
          readDocStatus "Preparation"   = Just DocStatusPreparation
          readDocStatus "Pending"       = Just DocStatusPending
          readDocStatus "Closed"        = Just DocStatusClosed
          readDocStatus "Canceled"      = Just DocStatusCanceled
          readDocStatus "Timedout"      = Just DocStatusTimedout
          readDocStatus "Rejected"      = Just DocStatusRejected
          readDocStatus "DocumentError" = Just DocStatusDocumentError
          readDocStatus _ = Nothing

data DocAuth = DocAuthStandard | DocAuthELeg | DocAuthSMS | DocAuthMixed
  deriving (Show)
instance Unjson DocAuth where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocAuth") return) . readDocAuth)
                show
                unjsonDef
    where readDocAuth :: String -> Maybe DocAuth
          readDocAuth "standard"  = Just DocAuthStandard
          readDocAuth "eleg"      = Just DocAuthELeg
          readDocAuth "sms_pin"   = Just DocAuthSMS
          readDocAuth "mixed"     = Just DocAuthMixed
          readDocAuth _ = Nothing

data DocDelivery = DocDeliveryEmail | DocDeliveryPad | DocDeliveryAPI
                 | DocDeliveryMobile | DocDeliveryEmailMobile | DocDeliveryMixed
  deriving (Show)
instance Unjson DocDelivery where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocDelivery") return) . readDocDelivery)
                show
                unjsonDef
    where readDocDelivery :: String -> Maybe DocDelivery
          readDocDelivery "email"         = Just DocDeliveryEmail
          readDocDelivery "pad"           = Just DocDeliveryPad
          readDocDelivery "api"           = Just DocDeliveryAPI
          readDocDelivery "mobile"        = Just DocDeliveryMobile
          readDocDelivery "email_mobile"  = Just DocDeliveryEmailMobile
          readDocDelivery "mixed"         = Just DocDeliveryMixed
          readDocDelivery _ = Nothing

data DocProcess = DocProcess deriving (Show)
instance Unjson DocProcess where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocProcess") return) . (\s ->
                    case s of
                         "Contract" -> Just DocProcess
                         _ -> Nothing
                )) show unjsonDef

-- FIXME Make all these data types with only one constructor?
-- i.e. make it 'data DocLang = ValidDocLang' and then in the instance this is
-- the only one that is generated, I can then also use 'elems' instead of all
-- this pattern matching taking up so much space!
data DocLang = DocLang_SV | DocLang_EN | DocLang_DE | DocLang_FR | DocLang_IT
             | DocLang_ES | DocLang_PT | DocLang_NL | DocLang_DA | DocLang_NO
             | DocLang_GR | DocLang_FI
  deriving (Show)
instance Unjson DocLang where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocLang") return) . readDocLang)
                show
                unjsonDef
    where readDocLang :: String -> Maybe DocLang
          readDocLang "sv" = Just DocLang_SV
          readDocLang "gb" = Just DocLang_EN
          --readDocLang "en" = Just DocLang_EN
          readDocLang "de" = Just DocLang_DE
          readDocLang "fr" = Just DocLang_FR
          readDocLang "it" = Just DocLang_IT
          readDocLang "es" = Just DocLang_ES
          readDocLang "pt" = Just DocLang_PT
          readDocLang "nl" = Just DocLang_NL
          readDocLang "da" = Just DocLang_DA
          readDocLang "no" = Just DocLang_NO
          readDocLang "el" = Just DocLang_GR
          readDocLang "fi" = Just DocLang_FI
          readDocLang _ = Nothing

data DocTag = DocTag { docTagName :: String, docTagValue :: String }
instance Unjson DocTag where
  unjsonDef = objectOf $ pure DocTag
    <*> field "name"  docTagName ""
    <*> field "value" docTagValue ""

data SigLink = SigLink
  { siglinkId                         :: Int64AsString
  , siglinkCurrent                    :: Bool
  , siglinkSignorder                  :: Int
  , siglinkUndeliveredInvitation      :: Bool
  , siglinkUndeliveredMailInvitation  :: Bool
  , siglinkUndeliveredSMSInvitation   :: Bool
  , siglinkDeliveredInvitation        :: Bool
  , siglinkDelivery                   :: Delivery
  , siglinkConfirmationdelivery       :: Confirmation
  , siglinkSigns                      :: Bool
  , siglinkAuthor                     :: Bool
  , siglinkSaved                      :: Bool
  , siglinkDatamismatch               :: Maybe String
  , siglinkSigndate                   :: Maybe UTCTime
  , siglinkSeendate                   :: Maybe UTCTime
  , siglinkReaddate                   :: Maybe UTCTime
  , siglinkRejecteddate               :: Maybe UTCTime
  , siglinkRejectionreason            :: Maybe String
  , siglinkFields                     :: [Field]
  , siglinkStatus                     :: SigLinkStatus
  , siglinkAttachments                :: [Attachment]
  , siglinkCsv                        :: Maybe [[String]]
  , siglinkInpadqueue                 :: Bool
  , siglinkUserid                     :: Maybe Int64AsString
  , siglinkSignsuccessredirect        :: Maybe String
  , siglinkRejectredirect             :: Maybe String
  , siglinkAuthentication             :: Authentication
  , siglinkSignlink                   :: Maybe String
  }
instance Unjson SigLink where
  unjsonDef = objectOf $ pure SigLink
    <*> field "id"                        siglinkId                             "signatories id"
    <*> field "current"                   siglinkCurrent                        "signatories current"
    <*> field "signorder"                 siglinkSignorder                      "signatories signorder"
    <*> field "undeliveredInvitation"     siglinkUndeliveredInvitation          "signatories undeliveredInvitation"
    <*> field "undeliveredMailInvitation" siglinkUndeliveredMailInvitation      ""
    <*> field "undeliveredSMSInvitation"  siglinkUndeliveredSMSInvitation       ""
    <*> field "deliveredInvitation"       siglinkDeliveredInvitation            ""
    <*> field "delivery"                  siglinkDelivery                       ""
    <*> field "confirmationdelivery"      siglinkConfirmationdelivery           ""
    <*> field "signs"                     siglinkSigns                          ""
    <*> field "author"                    siglinkAuthor                         ""
    <*> field "saved"                     siglinkSaved                          ""
    <*> fieldOpt "datamismatch"           siglinkDatamismatch                   ""
    <*> fieldOptBy "signdate"             siglinkSigndate                       "" unjsonISOTime
    <*> fieldOptBy "seendate"             siglinkSeendate                       "" unjsonISOTime
    <*> fieldOptBy "readdate"             siglinkReaddate                       "" unjsonISOTime
    <*> fieldOptBy "rejecteddate"         siglinkRejecteddate                   "" unjsonISOTime
    <*> fieldOpt "rejectionreason"        siglinkRejectionreason                ""
    <*> field "fields"                    siglinkFields                         ""
    <*> field "status"                    siglinkStatus                         ""
    <*> field "attachments"               siglinkAttachments                    ""
    <*> fieldOpt "csv"                    siglinkCsv                            ""
    <*> field "inpadqueue"                siglinkInpadqueue                     ""
    <*> fieldOpt "userid"                 siglinkUserid                         ""
    <*> fieldOpt "signsuccessredirect"    siglinkSignsuccessredirect            ""
    <*> fieldOpt "rejectredirect"         siglinkRejectredirect                 ""
    <*> field "authentication"            siglinkAuthentication                 ""
    <*> fieldOpt "signlink"               siglinkSignlink                       ""

-- FIXME make this stronger with a newtype?
readISOTime :: String -> Maybe UTCTime
readISOTime = parseTime defaultTimeLocale "%0Y-%0m-%0dT%0H:%0M:%0SZ"
unjsonISOTime :: UnjsonDef UTCTime
unjsonISOTime = unjsonInvmapR
                  ((maybe (fail "Can't parse ISO time format") return) . readISOTime)
                  show
                  unjsonDef

data Delivery = DeliveryEmail | DeliveryPad | DeliveryAPI | DeliveryMobile
              | DeliveryEmailMobile
  deriving (Show)
instance Unjson Delivery where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse Delivery") return) . readDelivery)
                show
                unjsonDef
    where readDelivery :: String -> Maybe Delivery
          readDelivery "email"        = Just DeliveryEmail
          readDelivery "pad"          = Just DeliveryPad
          readDelivery "api"          = Just DeliveryAPI
          readDelivery "mobile"       = Just DeliveryMobile
          readDelivery "email_mobile" = Just DeliveryEmailMobile
          readDelivery _ = Nothing

data Confirmation = ConfirmationEmail | ConfirmationMobile
                  | ConfirmationEmailMobile | ConfirmationNone
  deriving (Show)
instance Unjson Confirmation where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse Confirmation") return) . readConfirmation)
                show
                unjsonDef
    where readConfirmation :: String -> Maybe Confirmation
          readConfirmation "email"        = Just ConfirmationEmail
          readConfirmation "mobile"       = Just ConfirmationMobile
          readConfirmation "email_mobile" = Just ConfirmationEmailMobile
          readConfirmation "none"         = Just ConfirmationNone
          readConfirmation _ = Nothing

data Authentication = AuthenticationStandard | AuthenticationELeg | AuthenticationSMS
  deriving (Show)
instance Unjson Authentication where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse Authentication") return) . readAuthentication)
                show
                unjsonDef
    where readAuthentication :: String -> Maybe Authentication
          readAuthentication "standard" = Just AuthenticationStandard
          readAuthentication "eleg"     = Just AuthenticationELeg
          readAuthentication "sms_pin"  = Just AuthenticationSMS
          readAuthentication _ = Nothing

data Field = Field
  { fieldType                   :: FieldType
  -- FIXME this doesn't capture field name well...
  , fieldName                   :: String
  , fieldValue                  :: String
  , fieldClosed                 :: Bool
  , fieldObligatory             :: Bool
  , fieldShouldbefilledbysender :: Bool
  , fieldPlacements             :: [Placement]
  }
instance Unjson Field where
  unjsonDef = objectOf $ pure Field
    <*> field "type"                   fieldType                   ""
    -- FIXME this doesn't capture field name well...
    <*> field "name"                   fieldName                   ""
    <*> field "value"                  fieldValue                  ""
    <*> field "closed"                 fieldClosed                 ""
    <*> field "obligatory"             fieldObligatory             ""
    <*> field "shouldbefilledbysender" fieldShouldbefilledbysender ""
    <*> field "placements"             fieldPlacements             ""

data FieldType = FieldTypeStandard | FieldTypeSignature | FieldTypeCheckbox
               | FieldTypeCustom
  deriving (Show)
instance Unjson FieldType where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse FieldType") return) . readFieldType)
                show
                unjsonDef
    where readFieldType :: String -> Maybe FieldType
          readFieldType "standard"  = Just FieldTypeStandard
          readFieldType "signature" = Just FieldTypeSignature
          readFieldType "checkbox"  = Just FieldTypeCheckbox
          readFieldType "custom"    = Just FieldTypeCustom
          readFieldType _ = Nothing

data FieldName = FieldName String -- FIXME

data Placement = Placement
  { placementXrel     :: Double
  , placementYrel     :: Double
  , placementWrel     :: Double
  , placementHrel     :: Double
  , placementFSrel    :: Double
  , placementPage     :: Int
  , placementAnchors  :: Maybe [PlacementAnchor]
  , placementTip      :: Maybe PlacementTip
  }
instance Unjson Placement where
  unjsonDef = objectOf $ pure Placement
    <*> field "xrel"       placementXrel         ""
    <*> field "yrel"       placementYrel         ""
    <*> field "wrel"       placementWrel         ""
    <*> field "hrel"       placementHrel         ""
    <*> field "fsrel"      placementFSrel        ""
    <*> field "page"       placementPage         ""
    <*> fieldOpt "anchors" placementAnchors      ""
    <*> fieldOpt "tip"     placementTip          ""

data PlacementTip = PlacementTipLeft | PlacementTipRight
  deriving (Show)
instance Unjson PlacementTip where
  unjsonDef = unjsonInvmapR
                  ((maybe (fail "Can't parse PlacementTip") return) . readPlacementTip)
                  show
                  unjsonDef
    where readPlacementTip :: String -> Maybe PlacementTip
          readPlacementTip "left"  = Just PlacementTipLeft
          readPlacementTip "right" = Just PlacementTipRight
          readPlacementTip _ = Nothing

data PlacementAnchor = PlacementAnchor
  { placementanchorText  :: String
  , placementanchorIndex :: Maybe Int
  , placementanchorPage  :: [Int]
  }
instance Unjson PlacementAnchor where
  unjsonDef = objectOf $ pure PlacementAnchor
    <*> field "text"     placementanchorText  ""
    <*> fieldOpt "index" placementanchorIndex ""
    <*> field "pages"    placementanchorPage  ""

data SigLinkStatus = SigLinkStatusCancelled | SigLinkStatusDelivered
                   | SigLinkStatusDeliveryProblem | SigLinkStatusDraft
                   | SigLinkStatusError | SigLinkStatusOpened
                   | SigLinkStatusRead | SigLinkStatusRejected
                   | SigLinkStatusSent | SigLinkStatusSigned
                   | SigLinkStatusTimedout
  deriving (Show)
instance Unjson SigLinkStatus where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse SigLinkStatus") return) . readSigLinkStatus)
                show
                unjsonDef
    where readSigLinkStatus :: String -> Maybe SigLinkStatus
          readSigLinkStatus "cancelled"       = Just SigLinkStatusCancelled
          readSigLinkStatus "delivered"       = Just SigLinkStatusDelivered
          readSigLinkStatus "deliveryproblem" = Just SigLinkStatusDeliveryProblem
          readSigLinkStatus "draft"           = Just SigLinkStatusDraft
          readSigLinkStatus "problem"         = Just SigLinkStatusError
          readSigLinkStatus "opened"          = Just SigLinkStatusOpened
          readSigLinkStatus "read"            = Just SigLinkStatusRead
          readSigLinkStatus "rejected"        = Just SigLinkStatusRejected
          readSigLinkStatus "sent"            = Just SigLinkStatusSent
          readSigLinkStatus "signed"          = Just SigLinkStatusSigned
          readSigLinkStatus "timeouted"       = Just SigLinkStatusTimedout
          readSigLinkStatus _ = Nothing

data Attachment = Attachment
  { attachmentName        :: String
  , attachmentDescription :: String
  , attachmentFile        :: File
  }
instance Unjson Attachment where
  unjsonDef = objectOf $ pure Attachment
    <*> field "name"        attachmentName        ""
    <*> field "description" attachmentDescription ""
    <*> field "file"        attachmentFile        ""

data File = File
  { fileId    :: Int64AsString
  , fileName  :: String
  }
instance Unjson File where
  unjsonDef = objectOf $ pure File
    <*> field "id"    fileId    ""
    <*> field "name"  fileName  ""
