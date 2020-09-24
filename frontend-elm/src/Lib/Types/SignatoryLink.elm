module Lib.Types.SignatoryLink exposing (..)

import Time

import Lib.Types.ID exposing (ID)

type alias UTCTime = Time.Posix

type SignatoryRole
  = Viewer
  | Approver
  | SigningParty
  | ForwardedParty

type DeliveryStatus
  = Delivered
  | Undelivered
  | Unknown
  | Deferred

type DeliveryMethod
  = EmailDelivery
  | PadDelivery
  | APIDelivery
  | MobileDelivery
  | EmailAndMobileDelivery
  | PortalDelivery

type AuthenticationToViewMethod
  = StandardAuthenticationToView
  | SEBankIDAuthenticationToView
  | NOBankIDAuthenticationToView
  | LegacyDKNemIDAuthenticationToView
  | DKNemIDCPRAuthenticationToView
  | DKNemIDPIDAuthenticationToView
  | DKNemIDCVRAuthenticationToView
  | FITupasAuthenticationToView
  | SMSPinAuthenticationToView
  | VerimiAuthenticationToView
  | IDINAuthenticationToView
  | OnfidoDocumentCheckAuthenticationToView
  | OnfidoDocumentAndPhotoCheckAuthenticationToView

type SignatoryField
  = SignatoryNameField
      { nameOrder : Int
      , value : String
      }
  | SignatoryPersonalNumberField
      { value : String
      }
  | SignatoryEmailField
      { value : String
      }
  | SignatoryMobileField
      { value : String
      }
  | SignatoryOtherField  -- I didn't bother to implement all fields
      { sfType : String
      }

-- the type alias is used when parsing
type SignatoryLink = SignatoryLink SignatoryLinkRecord
type alias SignatoryLinkRecord = {
    id : ID SignatoryLink
  -- , userId : Maybe (ID User)
  , isAuthor : Bool
  -- , isSignatory : Bool
  , signatoryRole : SignatoryRole
  , fields : List SignatoryField
  -- , consentModule : Maybe SignatoryConsentModule
  -- , signOrder : SignOrder
  , signTime : Maybe UTCTime
  , seenTime : Maybe UTCTime
  , readInvitationTime : Maybe UTCTime
  -- , rejectedTime : Maybe UTCTime
  -- , rejectionReason : Maybe String
  -- , signSuccessRedirectUrl : Maybe Url
  -- , rejectRedirectUrl : Maybe Url
  , emailDeliveryStatus : DeliveryStatus
  , mobileDeliveryStatus : DeliveryStatus
  -- , confirmationEmailDeliveryStatus : DeliveryStatus
  -- , hasAuthenticatedToView : Bool
  -- , csv : Maybe CSVUpload
  , deliveryMethod : DeliveryMethod
  , authenticationMethodToView : AuthenticationToViewMethod
  , authenticationMethodToViewArchived  : AuthenticationToViewMethod
  -- , authenticationMethodToSign : AuthenticationToSignMethod
  -- , confirmationDeliveryMethod : ConfirmationDeliveryMethod
  -- , notificationDeliveryMethod : NotificationDeliveryMethod
  -- , allowsHighlighting : Bool
  -- , hidePersonalNumber : Bool
  -- , canForward : Bool
  -- , attachments : List SignatoryAttachment
  -- , highlightedPages : List HighlightedPage
  -- , apiDeliveryUrl : Maybe Url
  }



{-
type TipSide = LeftTip | RightTip

type PlacementAnchor = PlacementAnchor {
    text : String
  , index : Int
  }

type FieldPlacement = FieldPlacement {
    xrel : Float
  , yrel : Float
  , wrel : Float
  , hrel : Float
  , fsrel : Float
  , tip : Maybe TipSide
  , anchors : List PlacementAnchor
  }

type SignatoryField
  = SignatoryNameField {
      order : Int
    , value : String
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , placements : List FieldPlacement
    }
  | SignatoryCompanyField {
      value : String
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , placements : List FieldPlacement
    }
  | SignatoryPersonalNumberField {
      value : String
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , placements : List FieldPlacement
    }
  | SignatoryCompanyNumberField {
    value : String
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , placements : List FieldPlacement
    }
  | SignatoryEmailField {
      value : String
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , editableBySignatory : Bool
    , placements : List FieldPlacement
    }
  | SignatoryMobileField {
      value : String
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , editableBySignatory : Bool
    , placements : List FieldPlacement
    }
  | SignatoryTextField {
      name : String
    , value : String
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , placements : List FieldPlacement
    , customValidation : Maybe {
        pattern : String
      , positiveExample : String
      , tooltip : String
      }
    }
  | SignatoryCheckboxField {
      name : String
    , isChecked : Bool
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , placements : List FieldPlacement
    }
  | SignatorySignatureField {
      name : String
    , signature : Maybe (ID File)
    , isObligatory : Bool
    , shouldBeFilledBySender : Bool
    , placements : List FieldPlacement
    }
  | SignatoryRadioGroupField {
      name : String
    , selectedValue : Maybe String
    , placements : List FieldPlacement
    , values : List String
    }

type AuthenticationToSignMethod
  = StandardAuthenticationToSign
  | SEBankIDAuthenticationToSign
  | SMSPinAuthenticationToSign
  | NOBankIDAuthenticationToSign
  | DKNemIDAuthenticationToSign
  | IDINAuthenticationToSign
  | FITupasAuthenticationToSign
  | OnfidoDocumentCheckAuthenticationToSign
  | OnfidoDocumentAndPhotoCheckAuthenticationToSign

type ConfirmationDeliveryMethod
  = EmailConfirmationDelivery
  | MobileConfirmationDelivery
  | EmailAndMobileConfirmationDelivery
  | NoConfirmationDelivery
  | EmailLinkConfirmationDelivery
  | EmailLinkAndMobileConfirmationDelivery

type NotificationDeliveryMethod
  = EmailNotificationDelivery
  | MobileNotificationDelivery
  | EmailAndMobileNotificationDelivery
  | NoNotificationDelivery

type SignatoryAttachment = SignatoryAttachment {
    name : String
  , description : String
  , required : Bool
  , fileId : Maybe (ID File)
  , fileName : Maybe String
  }

type HighlightedPage = HighlightedPage {
    page : Int
  , fileId : ID File
  }

-- what the hell is this for?
type CSVUpload = CSVUpload {
    contents : List (List String)
  }

type SignatoryConsentQuestion = SignatoryConsentQuestion {
    id : ID SignatoryConsentQuestion
  , title : String
  , positiveOption : String
  , negativeOption : String
  , response : Maybe Bool
  , detailedDescription : Maybe {
      title : String
    , contents : String
    }
  }

type SignatoryConsentModule = SignatoryConsentModule {
    title : Maybe String
  , questions : List SignatoryConsentQuestion
  }

type SignOrder = SignOrder Int
-}
