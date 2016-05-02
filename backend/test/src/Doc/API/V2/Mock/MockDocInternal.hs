module Doc.API.V2.Mock.MockDocInternal where

import Data.Unjson

import KontraPrelude

data MockDoc = MockDoc {
    mockDocId                 :: String
  , mockDocTitle              :: String
  , mockDocParties            :: [MockSigLink]
  , mockDocFile               :: Maybe MockMainFile
  , mockDocSealedFile         :: Maybe MockMainFile
  , mockDocAuthorAttachments  :: [MockAuthorAttachment]
  , mockDocCTime              :: String
  , mockDocMTime              :: String
  , mockDocTimeoutTime        :: Maybe String
  , mockDocAutoRemindTime     :: Maybe String
  , mockDocStatus             :: String
  , mockDocDaysToSign         :: Int
  , mockDocDaysToRemind       :: Maybe Int
  , mockDocDisplayOptions     :: MockDocDisplayOptions
  , mockDocInvitationMsg      :: String
  , mockDocConfirmationMsg    :: String
  , mockDocLang               :: String
  , mockDocAPICallbackURL     :: Maybe String
  , mockDocObjectVersion      :: Int
  , mockDocAccessToken        :: Maybe String
  , mockDocTimezone           :: String
  , mockDocTags               :: [(String, String)]
  , mockDocIsTemplate         :: Bool
  , mockDocIsSaved            :: Bool
  , mockDocIsShared           :: Bool
  , mockDocIsTrashed          :: Bool
  , mockDocIsDeleted          :: Bool
  , mockDocViewer             :: MockViewer
} deriving (Show, Eq)

mockDocUnjson :: UnjsonDef MockDoc
mockDocUnjson = objectOf $ pure MockDoc
  <*> field "id" mockDocId                                "MockDoc ID"
  <*> field "title" mockDocTitle                          "MockDoc Title"
  <*> field "parties" mockDocParties                      "MockDoc Signatory Links"
  <*> fieldOpt "file" mockDocFile                         "MockDoc File"
  <*> fieldOpt "sealed_file" mockDocSealedFile            "MockDoc Sealed File"
  <*> field "author_attachments" mockDocAuthorAttachments "MockDoc AuthorAttachments"
  <*> field "ctime" mockDocCTime                          "MockDoc CTime"
  <*> field "mtime" mockDocMTime                          "MockDoc MTime"
  <*> fieldOpt "timeout_time" mockDocTimeoutTime          "MockDoc TimeoutTime"
  <*> fieldOpt "auto_remind_time" mockDocAutoRemindTime   "MockDoc AutoRemindTime"
  <*> field "status" mockDocStatus                        "MockDoc Status"
  <*> field "days_to_sign" mockDocDaysToSign              "MockDoc DaysToSign"
  <*> fieldOpt "days_to_remind" mockDocDaysToRemind       "MockDoc DaysToRemind"
  <*> field "display_options" mockDocDisplayOptions       "MockDoc DisplayOptions"
  <*> field "invitation_message" mockDocInvitationMsg     "MockDoc InvitationMsg"
  <*> field "confirmation_message" mockDocConfirmationMsg "MockDoc ConfirmationMsg"
  <*> field "lang" mockDocLang                            "MockDoc Lang"
  <*> fieldOpt "api_callback_url" mockDocAPICallbackURL   "MockDoc APICallbackURL"
  <*> field "object_version" mockDocObjectVersion         "MockDoc ObjectVersion"
  <*> fieldOpt "access_token" mockDocAccessToken          "MockDoc AccessToken"
  <*> field "timezone" mockDocTimezone                    "MockDoc Timezone"
  <*> field "tags" mockDocTags                            "MockDoc Tags"
  <*> field "is_template" mockDocIsTemplate               "MockDoc IsTemplate"
  <*> field "is_saved" mockDocIsSaved                     "MockDoc IsSaved"
  <*> field "is_shared" mockDocIsShared                   "MockDoc IsShared"
  <*> field "is_trashed" mockDocIsTrashed                 "MockDoc IsTrashed"
  <*> field "is_deleted" mockDocIsDeleted                 "MockDoc IsDeleted"
  <*> field "viewer" mockDocViewer                        "MockDoc Viewer"

data MockMainFile = MockMainFile {
    mockMainFileId   :: String
  , mockMainFileName :: String
} deriving (Show, Eq)
instance Unjson MockMainFile where
  unjsonDef = objectOf $ pure MockMainFile
    <*> field "id"    mockMainFileId    "MockMainFile ID"
    <*> field "name"  mockMainFileName  "MockMainFile Name"

data MockAuthorAttachment = MockAuthorAttachment {
    mockAuthorAttachmentName              :: String
  , mockAuthorAttachmentRequired          :: Bool
  , mockAuthorAttachmentAddedToSealedFile :: Bool
  , mockAuthorAttachmentFileId            :: String
} deriving (Show, Eq)
instance Unjson MockAuthorAttachment where
  unjsonDef = objectOf $ pure MockAuthorAttachment
    <*> field "name"     mockAuthorAttachmentName     "MockAuthorAttachment Name"
    <*> field "required" mockAuthorAttachmentRequired "MockAuthorAttachment Required"
    <*> field "add_to_sealed_file" mockAuthorAttachmentAddedToSealedFile "MockAuthorAttachment AddedToSealedFile"
    <*> field "file_id"  mockAuthorAttachmentFileId   "MockAuthorAttachment FileID"

data MockDocDisplayOptions = MockDocDisplayOptions {
    mockDocDisplayOptionsShowHeader        :: Bool
  , mockDocDisplayOptionsShowPDFDownload   :: Bool
  , mockDocDisplayOptionsShowRejectButton  :: Bool
  , mockDocDisplayOptionsAllowRejectReason :: Bool
  , mockDocDisplayOptionsShowFooter        :: Bool
} deriving (Show, Eq)
instance Unjson MockDocDisplayOptions where
  unjsonDef = objectOf $ pure MockDocDisplayOptions
    <*> field "show_header"         mockDocDisplayOptionsShowHeader        "MockDocDisplayOptions ShowHeader"
    <*> field "show_pdf_download"   mockDocDisplayOptionsShowPDFDownload   "MockDocDisplayOptions ShowPDFDownload"
    <*> field "show_reject_option"  mockDocDisplayOptionsShowRejectButton  "MockDocDisplayOptions ShowRejectButton"
    <*> field "allow_reject_reason" mockDocDisplayOptionsAllowRejectReason "MockDocDisplayOptions AllowRejectReason"
    <*> field "show_footer"         mockDocDisplayOptionsShowFooter        "MockDocDisplayOptions ShowFooter"

data MockViewer = MockViewer {
    mockViewerRole   :: String
  , mockViewerSigId  :: Maybe String
} deriving (Show, Eq)
instance Unjson MockViewer where
  unjsonDef = objectOf $ pure MockViewer
    <*> field     "role"          mockViewerRole   "MockViewer Role"
    <*> fieldOpt  "signatory_id"  mockViewerSigId  "MockViewer SigId"

data MockSigLink = MockSigLink {
    mockSigLinkId                     :: String
  , mockSigLinkUserId                 :: Maybe String
  , mockSigLinkIsAuthor               :: Bool
  , mockSigLinkIsSignatory            :: Bool
  , mockSigLinkFields                 :: [MockSigField]
  , mockSigLinkSignOrder              :: Int
  , mockSigLinkSignTime               :: Maybe String
  , mockSigLinkSeenTime               :: Maybe String
  , mockSigLinkReadInvitationTime     :: Maybe String
  , mockSigLinkRejectedTime           :: Maybe String
  , mockSigLinkSignRedirectURL        :: Maybe String
  , mockSigLinkRejectRedirectURL      :: Maybe String
  , mockSigLinkEmailDeliveryStatus    :: String
  , mockSigLinkMobileDeliveryStatus   :: String
  , mockSigLinkCSV                    :: Maybe String
  , mockSigLinkDeliveryMethod         :: String
  , mockSigLinkAuthMethodToView       :: String
  , mockSigLinkAuthMethodToSign       :: String
  , mockSigLinkConfirmationDelivery   :: String
  , mockSigLinkAttachments            :: [MockSigAttachment]
  , mockSigLinkAPIDeliveryURL         :: Maybe String
} deriving (Show, Eq)

instance Ord MockSigLink where
  msl1 `compare` msl2 = mockSigLinkId msl1 `compare` mockSigLinkId msl2

instance Unjson MockSigLink where
  unjsonDef = objectOf $ pure MockSigLink
    <*> field "id" mockSigLinkId ""
    <*> fieldOpt "user_id" mockSigLinkUserId ""
    <*> field "is_author" mockSigLinkIsAuthor ""
    <*> field "is_signatory" mockSigLinkIsSignatory ""
    <*> field "fields" mockSigLinkFields ""
    <*> field "sign_order" mockSigLinkSignOrder ""
    <*> fieldOpt "sign_time" mockSigLinkSignTime ""
    <*> fieldOpt "seen_time" mockSigLinkSeenTime ""
    <*> fieldOpt "read_invitation_time" mockSigLinkReadInvitationTime ""
    <*> fieldOpt "rejected_time" mockSigLinkRejectedTime ""
    <*> fieldOpt "sign_success_redirect_url" mockSigLinkSignRedirectURL ""
    <*> fieldOpt "reject_redirect_url" mockSigLinkRejectRedirectURL ""
    <*> field "email_delivery_status" mockSigLinkEmailDeliveryStatus ""
    <*> field "mobile_delivery_status" mockSigLinkMobileDeliveryStatus ""
    <*> fieldOpt "csv" mockSigLinkCSV ""
    <*> field "delivery_method" mockSigLinkDeliveryMethod ""
    <*> field "authentication_method_to_view" mockSigLinkAuthMethodToView ""
    <*> field "authentication_method_to_sign" mockSigLinkAuthMethodToSign ""
    <*> field "confirmation_delivery_method" mockSigLinkConfirmationDelivery ""
    <*> field "attachments" mockSigLinkAttachments ""
    <*> fieldOpt "api_delivery_url" mockSigLinkAPIDeliveryURL ""

data MockSigField = MockSigField {
    mockSigFieldType                   :: String
  , mockSigFieldIsObligatory           :: Bool
  , mockSigFieldShouldBeFilledBySender :: Bool
  , mockSigFieldPlacements             :: [MockFieldPlacement]
  , mockSigFieldValue                  :: Maybe String
  , mockSigFieldIsChecked              :: Maybe Bool
  , mockSigFieldOrder                  :: Maybe Int
  , mockSigFieldName                   :: Maybe String
  , mockSigFieldSignature              :: Maybe String
} deriving (Show, Eq)

instance Unjson MockSigField where
  unjsonDef = objectOf $ pure MockSigField
    <*> field "type" mockSigFieldType ""
    <*> field "is_obligatory" mockSigFieldIsObligatory ""
    <*> field "should_be_filled_by_sender" mockSigFieldShouldBeFilledBySender ""
    <*> field "placements" mockSigFieldPlacements ""
    <*> fieldOpt "value" mockSigFieldValue ""
    <*> fieldOpt "is_checked" mockSigFieldIsChecked ""
    <*> fieldOpt "order" mockSigFieldOrder ""
    <*> fieldOpt "name" mockSigFieldName ""
    <*> fieldOpt "signature" mockSigFieldSignature ""

data MockFieldPlacement = MockFieldPlacement {
    mockFieldPlacementXrel     :: Double
  , mockFieldPlacementYrel     :: Double
  , mockFieldPlacementWrel     :: Double
  , mockFieldPlacementHrel     :: Double
  , mockFieldPlacementFSrel    :: Double
  , mockFieldPlacementPage     :: Int
  , mockFieldPlacementTip      :: Maybe String
  , mockFieldPlacementAnchors  :: [MockAnchor]
} deriving (Show, Eq)
instance Unjson MockFieldPlacement where
  unjsonDef = objectOf $ pure MockFieldPlacement
    <*> field "xrel"    mockFieldPlacementXrel    "MockFieldPlacement Xrel"
    <*> field "yrel"    mockFieldPlacementYrel    "MockFieldPlacement Yrel"
    <*> field "wrel"    mockFieldPlacementWrel    "MockFieldPlacement Wrel"
    <*> field "hrel"    mockFieldPlacementHrel    "MockFieldPlacement Hrel"
    <*> field "fsrel"   mockFieldPlacementFSrel   "MockFieldPlacement FSrel"
    <*> field "page"    mockFieldPlacementPage    "MockFieldPlacement Page"
    <*> fieldOpt "tip"  mockFieldPlacementTip     "MockFieldPlacement Tip"
    <*> field "anchors" mockFieldPlacementAnchors "MockFieldPlacement Anchors"

data MockAnchor = MockAnchor {
    mockAnchorText    :: String
  , mockAnchorIndex   :: Int
} deriving (Show, Eq)
instance Unjson MockAnchor where
  unjsonDef = objectOf $ pure MockAnchor
    <*> field "text"  mockAnchorText  "MockAnchor Text"
    <*> field "index" mockAnchorIndex "MockAnchor Index"

data MockSigAttachment = MockSigAttachment {
    mockSigAttachmentName         :: String
  , mockSigAttachmentDescription  :: String
  , mockSigAttachmentFileId       :: Maybe String
  , mockSigAttachmentFileName     :: Maybe String
} deriving (Show, Eq)
instance Unjson MockSigAttachment where
  unjsonDef = objectOf $ pure MockSigAttachment
    <*> field "name"        mockSigAttachmentName         "MockSigAttachment Name"
    <*> field "description" mockSigAttachmentDescription  "MockSigAttachment Description"
    <*> fieldOpt "file_id"    mockSigAttachmentFileId     "MockSigAttachment File ID"
    <*> fieldOpt "file_name"  mockSigAttachmentFileName   "MockSigAttachment File Name"
