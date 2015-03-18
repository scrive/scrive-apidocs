module Doc.API.V1.JSONTypes where

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

data V1_Doc = V1_Doc
  { v1_docid                  :: Int64AsString
  , v1_doctitle               :: String
  , v1_docfile                :: Maybe V1_File
  , v1_docsealedfile          :: Maybe V1_File
  , v1_docauthorattachments   :: [V1_File]
  , v1_docevidenceattachments :: [V1_File]
  , v1_docmtime               :: UTCTime
  , v1_docctime               :: UTCTime
  , v1_doctimeouttime         :: Maybe UTCTime
  , v1_docautoremindtime      :: Maybe UTCTime
  , v1_docstatus              :: V1_DocStatus
  , v1_docstate               :: V1_DocStatus
  , v1_docsignatorylinks      :: [V1_SigLink]
  , v1_docsignorder           :: Int32
  , v1_docauthentication      :: V1_DocAuth
  , v1_docdelivery            :: V1_DocDelivery
  , v1_doctemplate            :: Bool
  , v1_docdaystosign          :: Int32
  , v1_docdaystoremind        :: Maybe Int32
  , v1_docshowheader          :: Bool
  , v1_docshowpdfdownload     :: Bool
  , v1_docshowrejectoption    :: Bool
  , v1_docshowfooter          :: Bool
  , v1_docinvitetext          :: String
  , v1_docconfirmtext         :: String
  , v1_doclang                :: V1_DocLang
  , v1_doctags                :: [V1_DocTag]
  , v1_docapicallbackurl      :: (Maybe String)
  , v1_docunsaveddraft        :: Bool
  , v1_docdeleted             :: Bool
  , v1_docreallydeleted       :: Bool
  , v1_doccanperformsigning   :: Bool
  , v1_docobjectversion       :: Int64
  , v1_docprocess             :: V1_DocProcess
  , v1_docisviewedbyauthor    :: Bool
  , v1_docaccesstoken         :: MagicHash
  , v1_doctimezonename        :: String
  }

v1_docUnjsonDef :: UnjsonDef V1_Doc
v1_docUnjsonDef = objectOf $ pure V1_Doc
  <*> field "id"                  v1_docid                   "Doc id"
  <*> field "title"               v1_doctitle                "Doc title"
  <*> fieldOpt "file"             v1_docfile                 ""
  <*> fieldOpt "sealedfile"       v1_docsealedfile           ""
  <*> field "authorattachments"   v1_docauthorattachments    ""
  <*> field "evidenceattachments" v1_docevidenceattachments  ""
  <*> fieldBy "time"              v1_docmtime                "" unjsonISOTime
  <*> fieldBy "ctime"             v1_docctime                "" unjsonISOTime
  <*> fieldOptBy "timeouttime"    v1_doctimeouttime          "" unjsonISOTime
  <*> fieldOptBy "autoremindtime" v1_docautoremindtime       "" unjsonISOTime
  <*> field "status"              v1_docstatus               ""
  <*> field "state"               v1_docstate                ""
  <*> field "signatories"         v1_docsignatorylinks       "signatories"
  <*> field "signorder"           v1_docsignorder            ""
  <*> field "authentication"      v1_docauthentication       ""
  <*> field "delivery"            v1_docdelivery             ""
  <*> field "template"            v1_doctemplate             ""
  <*> field "daystosign"          v1_docdaystosign           "daystosign"
  <*> fieldOpt "daystoremind"     v1_docdaystoremind         "daystoremind"
  <*> field "showheader"          v1_docshowheader           "showheader"
  <*> field "showpdfdownload"     v1_docshowpdfdownload      "showpdfdownload"
  <*> field "showrejectoption"    v1_docshowrejectoption     "showrejectoption"
  <*> field "showfooter"          v1_docshowfooter           "showfooter"
  <*> field "invitationmessage"   v1_docinvitetext           "docinvitetext"
  <*> field "confirmationmessage" v1_docconfirmtext          "docconfirmtext"
  <*> field "lang"                v1_doclang                 ""
  <*> field "tags"                v1_doctags                 ""
  <*> fieldOpt "apicallbackurl"   v1_docapicallbackurl       "apicallbackurl"
  <*> field "saved"               v1_docunsaveddraft         "saved"
  <*> field "deleted"             v1_docdeleted              ""
  <*> field "reallydeleted"       v1_docreallydeleted        ""
  <*> field "canperformsigning"   v1_doccanperformsigning    ""
  <*> field "objectversion"       v1_docobjectversion        "objectversion"
  <*> field "process"             v1_docprocess              ""
  <*> field "isviewedbyauthor"    v1_docisviewedbyauthor     ""
  <*> fieldBy "accesstoken"       v1_docaccesstoken          "" unjsonMagicHash
  <*> field "timezone"            v1_doctimezonename         ""

data V1_DocStatus = V1_DocStatusPreparation | V1_DocStatusPending | V1_DocStatusClosed
               | V1_DocStatusCanceled | V1_DocStatusTimedout | V1_DocStatusRejected
               | V1_DocStatusDocumentError
  deriving (Show)
instance Unjson V1_DocStatus where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocStatus") return) . readDocStatus)
                show
                unjsonDef
    where readDocStatus :: String -> Maybe V1_DocStatus
          readDocStatus "Preparation"   = Just V1_DocStatusPreparation
          readDocStatus "Pending"       = Just V1_DocStatusPending
          readDocStatus "Closed"        = Just V1_DocStatusClosed
          readDocStatus "Canceled"      = Just V1_DocStatusCanceled
          readDocStatus "Timedout"      = Just V1_DocStatusTimedout
          readDocStatus "Rejected"      = Just V1_DocStatusRejected
          readDocStatus "DocumentError" = Just V1_DocStatusDocumentError
          readDocStatus _ = Nothing

data V1_DocAuth = V1_DocAuthStandard | V1_DocAuthELeg | V1_DocAuthSMS | V1_DocAuthMixed
  deriving (Show)
instance Unjson V1_DocAuth where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocAuth") return) . readDocAuth)
                show
                unjsonDef
    where readDocAuth :: String -> Maybe V1_DocAuth
          readDocAuth "standard"  = Just V1_DocAuthStandard
          readDocAuth "eleg"      = Just V1_DocAuthELeg
          readDocAuth "sms_pin"   = Just V1_DocAuthSMS
          readDocAuth "mixed"     = Just V1_DocAuthMixed
          readDocAuth _ = Nothing

data V1_DocDelivery = V1_DocDeliveryEmail | V1_DocDeliveryPad | V1_DocDeliveryAPI
                 | V1_DocDeliveryMobile | V1_DocDeliveryEmailMobile | V1_DocDeliveryMixed
  deriving (Show)
instance Unjson V1_DocDelivery where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocDelivery") return) . readDocDelivery)
                show
                unjsonDef
    where readDocDelivery :: String -> Maybe V1_DocDelivery
          readDocDelivery "email"         = Just V1_DocDeliveryEmail
          readDocDelivery "pad"           = Just V1_DocDeliveryPad
          readDocDelivery "api"           = Just V1_DocDeliveryAPI
          readDocDelivery "mobile"        = Just V1_DocDeliveryMobile
          readDocDelivery "email_mobile"  = Just V1_DocDeliveryEmailMobile
          readDocDelivery "mixed"         = Just V1_DocDeliveryMixed
          readDocDelivery _ = Nothing

data V1_DocProcess = V1_DocProcess deriving (Show)
instance Unjson V1_DocProcess where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocProcess") return) . (\s ->
                    case s of
                         "Contract" -> Just V1_DocProcess
                         _ -> Nothing
                )) show unjsonDef

data V1_DocLang = V1_DocLang String
  deriving (Show)
instance Unjson V1_DocLang where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse DocLang") return) . readDocLang)
                show
                unjsonDef
    where readDocLang :: String -> Maybe V1_DocLang
          readDocLang "sv" = Just $ V1_DocLang "sv"
          readDocLang "gb" = Just $ V1_DocLang "gb"
          readDocLang "en" = Just $ V1_DocLang "en"
          readDocLang "de" = Just $ V1_DocLang "de"
          readDocLang "fr" = Just $ V1_DocLang "fr"
          readDocLang "it" = Just $ V1_DocLang "it"
          readDocLang "es" = Just $ V1_DocLang "es"
          readDocLang "pt" = Just $ V1_DocLang "pt"
          readDocLang "nl" = Just $ V1_DocLang "nl"
          readDocLang "da" = Just $ V1_DocLang "da"
          readDocLang "no" = Just $ V1_DocLang "no"
          readDocLang "el" = Just $ V1_DocLang "el"
          readDocLang "fi" = Just $ V1_DocLang "fi"
          readDocLang _ = Nothing

data V1_DocTag = V1_DocTag { v1_docTagName :: String, v1_docTagValue :: String }
instance Unjson V1_DocTag where
  unjsonDef = objectOf $ pure V1_DocTag
    <*> field "name"  v1_docTagName ""
    <*> field "value" v1_docTagValue ""

data V1_SigLink = V1_SigLink
  { v1_siglinkId                         :: Int64AsString
  , v1_siglinkCurrent                    :: Bool
  , v1_siglinkSignorder                  :: Int
  , v1_siglinkUndeliveredInvitation      :: Bool
  , v1_siglinkUndeliveredMailInvitation  :: Bool
  , v1_siglinkUndeliveredSMSInvitation   :: Bool
  , v1_siglinkDeliveredInvitation        :: Bool
  , v1_siglinkDelivery                   :: V1_Delivery
  , v1_siglinkConfirmationdelivery       :: V1_Confirmation
  , v1_siglinkSigns                      :: Bool
  , v1_siglinkAuthor                     :: Bool
  , v1_siglinkSaved                      :: Bool
  , v1_siglinkDatamismatch               :: Maybe String
  , v1_siglinkSigndate                   :: Maybe UTCTime
  , v1_siglinkSeendate                   :: Maybe UTCTime
  , v1_siglinkReaddate                   :: Maybe UTCTime
  , v1_siglinkRejecteddate               :: Maybe UTCTime
  , v1_siglinkRejectionreason            :: Maybe String
  , v1_siglinkFields                     :: [V1_Field]
  , v1_siglinkStatus                     :: V1_SigLinkStatus
  , v1_siglinkAttachments                :: [V1_Attachment]
  , v1_siglinkCsv                        :: Maybe [[String]]
  , v1_siglinkInpadqueue                 :: Bool
  , v1_siglinkUserid                     :: Maybe Int64AsString
  , v1_siglinkSignsuccessredirect        :: Maybe String
  , v1_siglinkRejectredirect             :: Maybe String
  , v1_siglinkAuthentication             :: V1_Authentication
  , v1_siglinkSignlink                   :: Maybe String
  }
instance Unjson V1_SigLink where
  unjsonDef = objectOf $ pure V1_SigLink
    <*> field "id"                        v1_siglinkId                             "signatories id"
    <*> field "current"                   v1_siglinkCurrent                        "signatories current"
    <*> field "signorder"                 v1_siglinkSignorder                      "signatories signorder"
    <*> field "undeliveredInvitation"     v1_siglinkUndeliveredInvitation          "signatories undeliveredInvitation"
    <*> field "undeliveredMailInvitation" v1_siglinkUndeliveredMailInvitation      ""
    <*> field "undeliveredSMSInvitation"  v1_siglinkUndeliveredSMSInvitation       ""
    <*> field "deliveredInvitation"       v1_siglinkDeliveredInvitation            ""
    <*> field "delivery"                  v1_siglinkDelivery                       ""
    <*> field "confirmationdelivery"      v1_siglinkConfirmationdelivery           ""
    <*> field "signs"                     v1_siglinkSigns                          ""
    <*> field "author"                    v1_siglinkAuthor                         ""
    <*> field "saved"                     v1_siglinkSaved                          ""
    <*> fieldOpt "datamismatch"           v1_siglinkDatamismatch                   ""
    <*> fieldOptBy "signdate"             v1_siglinkSigndate                       "" unjsonISOTime
    <*> fieldOptBy "seendate"             v1_siglinkSeendate                       "" unjsonISOTime
    <*> fieldOptBy "readdate"             v1_siglinkReaddate                       "" unjsonISOTime
    <*> fieldOptBy "rejecteddate"         v1_siglinkRejecteddate                   "" unjsonISOTime
    <*> fieldOpt "rejectionreason"        v1_siglinkRejectionreason                ""
    <*> field "fields"                    v1_siglinkFields                         ""
    <*> field "status"                    v1_siglinkStatus                         ""
    <*> field "attachments"               v1_siglinkAttachments                    ""
    <*> fieldOpt "csv"                    v1_siglinkCsv                            ""
    <*> field "inpadqueue"                v1_siglinkInpadqueue                     ""
    <*> fieldOpt "userid"                 v1_siglinkUserid                         ""
    <*> fieldOpt "signsuccessredirect"    v1_siglinkSignsuccessredirect            ""
    <*> fieldOpt "rejectredirect"         v1_siglinkRejectredirect                 ""
    <*> field "authentication"            v1_siglinkAuthentication                 ""
    <*> fieldOpt "signlink"               v1_siglinkSignlink                       ""

readISOTime :: String -> Maybe UTCTime
readISOTime = parseTime defaultTimeLocale "%0Y-%0m-%0dT%0H:%0M:%0SZ"

unjsonISOTime :: UnjsonDef UTCTime
unjsonISOTime = unjsonInvmapR
                  ((maybe (fail "Can't parse ISO time format") return) . readISOTime)
                  show
                  unjsonDef

data V1_Delivery = V1_DeliveryEmail | V1_DeliveryPad | V1_DeliveryAPI | V1_DeliveryMobile
              | V1_DeliveryEmailMobile
  deriving (Show)
instance Unjson V1_Delivery where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse Delivery") return) . readDelivery)
                show
                unjsonDef
    where readDelivery :: String -> Maybe V1_Delivery
          readDelivery "email"        = Just V1_DeliveryEmail
          readDelivery "pad"          = Just V1_DeliveryPad
          readDelivery "api"          = Just V1_DeliveryAPI
          readDelivery "mobile"       = Just V1_DeliveryMobile
          readDelivery "email_mobile" = Just V1_DeliveryEmailMobile
          readDelivery _ = Nothing

data V1_Confirmation = V1_ConfirmationEmail | V1_ConfirmationMobile
                  | V1_ConfirmationEmailMobile | V1_ConfirmationNone
  deriving (Show)
instance Unjson V1_Confirmation where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse Confirmation") return) . readConfirmation)
                show
                unjsonDef
    where readConfirmation :: String -> Maybe V1_Confirmation
          readConfirmation "email"        = Just V1_ConfirmationEmail
          readConfirmation "mobile"       = Just V1_ConfirmationMobile
          readConfirmation "email_mobile" = Just V1_ConfirmationEmailMobile
          readConfirmation "none"         = Just V1_ConfirmationNone
          readConfirmation _ = Nothing

data V1_Authentication = V1_AuthenticationStandard | V1_AuthenticationELeg | V1_AuthenticationSMS
  deriving (Show)
instance Unjson V1_Authentication where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse Authentication") return) . readAuthentication)
                show
                unjsonDef
    where readAuthentication :: String -> Maybe V1_Authentication
          readAuthentication "standard" = Just V1_AuthenticationStandard
          readAuthentication "eleg"     = Just V1_AuthenticationELeg
          readAuthentication "sms_pin"  = Just V1_AuthenticationSMS
          readAuthentication _ = Nothing

data V1_Field = V1_Field
  { v1_fieldType                   :: V1_FieldType
  , v1_fieldName                   :: String
  , v1_fieldValue                  :: String
  , v1_fieldClosed                 :: Bool
  , v1_fieldObligatory             :: Bool
  , v1_fieldShouldbefilledbysender :: Bool
  , v1_fieldPlacements             :: [V1_Placement]
  }
instance Unjson V1_Field where
  unjsonDef = objectOf $ pure V1_Field
    <*> field "type"                   v1_fieldType                   ""
    <*> field "name"                   v1_fieldName                   ""
    <*> field "value"                  v1_fieldValue                  ""
    <*> field "closed"                 v1_fieldClosed                 ""
    <*> field "obligatory"             v1_fieldObligatory             ""
    <*> field "shouldbefilledbysender" v1_fieldShouldbefilledbysender ""
    <*> field "placements"             v1_fieldPlacements             ""

data V1_FieldType = V1_FieldTypeStandard | V1_FieldTypeSignature | V1_FieldTypeCheckbox
               | V1_FieldTypeCustom
  deriving (Show)
instance Unjson V1_FieldType where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse FieldType") return) . readFieldType)
                show
                unjsonDef
    where readFieldType :: String -> Maybe V1_FieldType
          readFieldType "standard"  = Just V1_FieldTypeStandard
          readFieldType "signature" = Just V1_FieldTypeSignature
          readFieldType "checkbox"  = Just V1_FieldTypeCheckbox
          readFieldType "custom"    = Just V1_FieldTypeCustom
          readFieldType _ = Nothing

data V1_Placement = V1_Placement
  { v1_placementXrel     :: Double
  , v1_placementYrel     :: Double
  , v1_placementWrel     :: Double
  , v1_placementHrel     :: Double
  , v1_placementFSrel    :: Double
  , v1_placementPage     :: Int
  , v1_placementAnchors  :: Maybe [V1_PlacementAnchor]
  , v1_placementTip      :: Maybe V1_PlacementTip
  }
instance Unjson V1_Placement where
  unjsonDef = objectOf $ pure V1_Placement
    <*> field "xrel"       v1_placementXrel         ""
    <*> field "yrel"       v1_placementYrel         ""
    <*> field "wrel"       v1_placementWrel         ""
    <*> field "hrel"       v1_placementHrel         ""
    <*> field "fsrel"      v1_placementFSrel        ""
    <*> field "page"       v1_placementPage         ""
    <*> fieldOpt "anchors" v1_placementAnchors      ""
    <*> fieldOpt "tip"     v1_placementTip          ""

data V1_PlacementTip = V1_PlacementTipLeft | V1_PlacementTipRight
  deriving (Show)
instance Unjson V1_PlacementTip where
  unjsonDef = unjsonInvmapR
                  ((maybe (fail "Can't parse PlacementTip") return) . readPlacementTip)
                  show
                  unjsonDef
    where readPlacementTip :: String -> Maybe V1_PlacementTip
          readPlacementTip "left"  = Just V1_PlacementTipLeft
          readPlacementTip "right" = Just V1_PlacementTipRight
          readPlacementTip _ = Nothing

data V1_PlacementAnchor = V1_PlacementAnchor
  { v1_placementanchorText  :: String
  , v1_placementanchorIndex :: Maybe Int
  , v1_placementanchorPage  :: [Int]
  }
instance Unjson V1_PlacementAnchor where
  unjsonDef = objectOf $ pure V1_PlacementAnchor
    <*> field "text"     v1_placementanchorText  ""
    <*> fieldOpt "index" v1_placementanchorIndex ""
    <*> field "pages"    v1_placementanchorPage  ""

data V1_SigLinkStatus = V1_SigLinkStatusCancelled | V1_SigLinkStatusDelivered
                   | V1_SigLinkStatusDeliveryProblem | V1_SigLinkStatusDraft
                   | V1_SigLinkStatusError | V1_SigLinkStatusOpened
                   | V1_SigLinkStatusRead | V1_SigLinkStatusRejected
                   | V1_SigLinkStatusSent | V1_SigLinkStatusSigned
                   | V1_SigLinkStatusTimedout
  deriving (Show)
instance Unjson V1_SigLinkStatus where
  unjsonDef = unjsonInvmapR
                ((maybe (fail "Can't parse SigLinkStatus") return) . readSigLinkStatus)
                show
                unjsonDef
    where readSigLinkStatus :: String -> Maybe V1_SigLinkStatus
          readSigLinkStatus "cancelled"       = Just V1_SigLinkStatusCancelled
          readSigLinkStatus "delivered"       = Just V1_SigLinkStatusDelivered
          readSigLinkStatus "deliveryproblem" = Just V1_SigLinkStatusDeliveryProblem
          readSigLinkStatus "draft"           = Just V1_SigLinkStatusDraft
          readSigLinkStatus "problem"         = Just V1_SigLinkStatusError
          readSigLinkStatus "opened"          = Just V1_SigLinkStatusOpened
          readSigLinkStatus "read"            = Just V1_SigLinkStatusRead
          readSigLinkStatus "rejected"        = Just V1_SigLinkStatusRejected
          readSigLinkStatus "sent"            = Just V1_SigLinkStatusSent
          readSigLinkStatus "signed"          = Just V1_SigLinkStatusSigned
          readSigLinkStatus "timeouted"       = Just V1_SigLinkStatusTimedout
          readSigLinkStatus _ = Nothing

data V1_Attachment = V1_Attachment
  { v1_attachmentName        :: String
  , v1_attachmentDescription :: String
  , v1_attachmentFile        :: V1_File
  }
instance Unjson V1_Attachment where
  unjsonDef = objectOf $ pure V1_Attachment
    <*> field "name"        v1_attachmentName        ""
    <*> field "description" v1_attachmentDescription ""
    <*> field "file"        v1_attachmentFile        ""

data V1_File = V1_File
  { v1_fileId    :: Int64AsString
  , v1_fileName  :: String
  }
instance Unjson V1_File where
  unjsonDef = objectOf $ pure V1_File
    <*> field "id"    v1_fileId    ""
    <*> field "name"  v1_fileName  ""
