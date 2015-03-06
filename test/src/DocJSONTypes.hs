module DocJSONTypes where

import Data.Int
import Control.Applicative
import Data.Unjson
import Data.Time
import System.Locale

import DB.Derive
import Utils.Read

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

data Doc = Doc
  { docid                :: Int64AsString
  , doctitle             :: String
  -- TODO , docmainfiles         :: ![MainFile]
  -- TODO sealed file?
  -- TODO , docauthorattachments :: ![AuthorAttachment]
  -- TODO evidence attachments?
  , docmtime             :: UTCTime
  , docctime             :: UTCTime
  , doctimeouttime       :: Maybe UTCTime
  , docautoremindtime    :: Maybe UTCTime
  -- TODO , docstatus            :: !DocumentStatus
  -- TODO state = docstatus
  , docsignatorylinks    :: [SigLink]
  -- TODO signorder
  -- TODO authentication
  -- TODO delivery
  -- TODO , doctype              :: !DocumentType
  , docdaystosign        :: Int32
  , docdaystoremind      :: Maybe Int32
  , docshowheader        :: Bool
  , docshowpdfdownload   :: Bool
  , docshowrejectoption  :: Bool
  , docshowfooter        :: Bool
  , docinvitetext        :: String
  , docconfirmtext       :: String
  -- TODO , doclang              :: !Lang
  -- TODO , doctags              :: !(S.Set DocumentTag)
  , docapicallbackurl    :: (Maybe String)
  , docunsaveddraft      :: Bool
  -- TODO deleted
  -- TODO reallydeleted
  -- TODO canperformsigning
  , docobjectversion     :: Int64
  -- TODO process
  -- TODO isviewedbyauthor
  -- TODO canberestarted
  -- TODO canbeprolonged
  -- TODO canbecanceled
  -- TODO canseeallattachments
  -- TODO accesstoken (AKA MagicHash)
  -- TODO , doctimezonename      :: !TimeZoneName
  }

docUnjsonDef :: UnjsonDef Doc
docUnjsonDef = objectOf $ pure Doc
  <*> field "id"         docid         "Doc id"
  <*> field "title"      doctitle      "Doc title"
  -- TODO , docmainfiles         :: ![MainFile]
  -- TODO sealed file?
  -- TODO , docauthorattachments :: ![AuthorAttachment]
  -- TODO evidence attachments?
  <*> fieldBy "time"              docmtime             "" unjsonISOTime
  <*> fieldBy "ctime"             docctime             "" unjsonISOTime
  <*> fieldOptBy "timeouttime"    doctimeouttime       "" unjsonISOTime
  <*> fieldOptBy "autoremindtime" docautoremindtime    "" unjsonISOTime
  -- TODO , docstatus            :: !DocumentStatus
  -- TODO state = docstatus
  <*> field "signatories" docsignatorylinks "signatories"
  -- TODO signorder
  -- TODO authentication
  -- TODO delivery
  -- TODO , doctype              :: !DocumentType
  <*> field "daystosign"   docdaystosign   "daystosign"
  <*> fieldOpt "daystoremind" docdaystoremind "daystoremind"
  <*> field "showheader"       docshowheader       "showheader"
  <*> field "showpdfdownload"  docshowpdfdownload  "showpdfdownload"
  <*> field "showrejectoption" docshowrejectoption "showrejectoption"
  <*> field "showfooter"       docshowfooter       "showfooter"
  <*> field "invitationmessage"   docinvitetext  "docinvitetext"    -- FIXME check for <p></p>?
  <*> field "confirmationmessage" docconfirmtext "docconfirmtext"   -- FIXMe check for <p></p>?
  -- TODO , doclang              :: !Lang
  -- TODO , doctags              :: !(S.Set DocumentTag)
  <*> fieldOpt "apicallbackurl" docapicallbackurl "apicallbackurl"
  <*> field "saved" docunsaveddraft "saved"
  -- TODO deleted
  -- TODO reallydeleted
  -- TODO canperformsigning
  <*> field "objectversion" docobjectversion "objectversion"
  -- TODO process
  -- TODO isviewedbyauthor
  -- TODO canberestarted
  -- TODO canbeprolonged
  -- TODO canbecanceled
  -- TODO canseeallattachments
  -- TODO accesstoken (AKA MagicHash)
  -- TODO , doctimezonename      :: !TimeZoneName

data SigLink = SigLink
  { siglinkId :: Int64AsString
  , siglinkCurrent :: Bool
  , siglinkSignorder :: Int
  , siglinkUndeliveredInvitation :: Bool
  , siglinkUndeliveredMailInvitation :: Bool
  , siglinkUndeliveredSMSInvitation :: Bool
  , siglinkDeliveredInvitation :: Bool
  , siglinkDelivery :: Delivery
  , siglinkConfirmationdelivery :: Confirmation
  , siglinkSigns :: Bool
  , siglinkAuthor :: Bool
  , siglinkSaved :: Bool
  , siglinkDatamismatch ::     Maybe String
  , siglinkSigndate ::         Maybe UTCTime
  , siglinkSeendate ::         Maybe UTCTime
  , siglinkReaddate ::         Maybe UTCTime
  , siglinkRejecteddate ::     Maybe UTCTime
  , siglinkRejectionreason ::  Maybe String
  , siglinkFields :: [Field]
  -- TODO , siglinkStatus ::  "draft"
  -- TODO , siglinkAttachments ::  []
  -- TODO , siglinkCsv ::  null
  , siglinkInpadqueue :: Bool
  , siglinkUserid ::  Maybe Int64AsString
  , siglinkSignsuccessredirect :: Maybe String
  , siglinkRejectredirect ::  Maybe String
  , siglinkAuthentication :: Authentication
  , siglinkSignlink :: Maybe String
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
    -- TODO , siglinkStatus ::  "draft"
    -- TODO , siglinkAttachments ::  []
    -- TODO , siglinkCsv ::  null
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
  -- TODO , fieldName                   :: FieldName
  -- TODO , fieldValue :: String, but look at `sfvEncode`
  , fieldClosed                 :: Bool
  , fieldObligatory             :: Bool
  , fieldShouldbefilledbysender :: Bool
  , fieldPlacements             :: [Placement]
  }
instance Unjson Field where
  unjsonDef = objectOf $ pure Field
    <*> field "type"                   fieldType                   ""
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
