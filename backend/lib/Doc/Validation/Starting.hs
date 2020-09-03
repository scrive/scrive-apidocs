module Doc.Validation.Starting
  ( ApproverErrorType(..)
  , AuthType(..)
  , MessageType(..)
  , StartingError(..)
  , errorExplanation
  , validateDocumentForStarting
  )
where

import Control.Monad.Writer
import Data.Foldable (traverse_)
import qualified Data.Text as T

import Doc.DocInfo
import Doc.SignatoryFieldUtils
import Doc.SignatoryLinkID
import Doc.Types.Document
import Doc.Types.SignatoryField
import Doc.Types.SignatoryLink
import InputValidation
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

data MessageType = InvitationMessage | ConfirmationMessage | NotificationMessage
data AuthType = BasicAuth | AuthToView | AuthToSign | AuthToViewArchived
data ApproverErrorType = ApproverNonStandardAuth | ApproverPlacedField

data StartingError
  = TemplateError
  | AuthorApproverError
  | NoSigningParty
  | FileError
  | ContactError MessageType SignatoryLinkID
  | AuthError AuthType SignatoryLinkID
  | ApproverError ApproverErrorType SignatoryLinkID

errorExplanation :: StartingError -> Text
errorExplanation = \case
  TemplateError       -> "Document is a template, templates can not be started"
  AuthorApproverError -> "Author can't be an approver"
  NoSigningParty      -> "Document has to have at least one signing party"
  FileError           -> "Document must have a file before it can be started"
  ContactError InvitationMessage _
    -> "Some parties have an invalid email address or mobile number, their invitation 'delivery_method' requires it to be valid and not empty."
  ContactError ConfirmationMessage _
    -> "Some parties have an invalid email address or mobile number, their 'confirmation_delivery_method' requires it to be valid or empty."
  ContactError NotificationMessage _
    -> "Some parties have an invalid email address or mobile number, their 'notification_delivery_method' requires it to be valid or empty."
  AuthError BasicAuth _
    -> "Some parties have an invalid personal numbers/emails, their 'authentication_to_view' requires it to be valid and not empty."
  AuthError AuthToSign _
    -> "Some parties have an invalid personal/mobile numbers, their 'authentication_to_sign' requires it to be valid or empty."
  AuthError AuthToView _
    -> "Some parties have an invalid mobile number and it is required for identification to view document."
  AuthError AuthToViewArchived _
    -> "Some parties have an invalid mobile number and it is required for identification to view archived document."
  ApproverError ApproverNonStandardAuth _ ->
    "All approvers have to have standard authentication to sign"
  ApproverError ApproverPlacedField _ -> "No approver can have placed fields"

validateDocumentForStarting :: Document -> [StartingError]
validateDocumentForStarting doc = execWriter $ do
  when (isTemplate doc) $ addError TemplateError
  when (any (isAuthor && isApprover) signatories) $ addError AuthorApproverError
  unless (any isSignatory signatories) $ addError NoSigningParty
  when (isNothing $ documentfile doc) $ addError FileError

  validateAll signatoryHasValidDeliverySettings $ ContactError InvitationMessage
  validateAll signatoryHasValidConfirmationSettings $ ContactError ConfirmationMessage
  validateAll signatoryHasValidNotificationSettings $ ContactError NotificationMessage
  validateAll signatoryHasValidSSNOrEmailForIdentifyToView $ AuthError BasicAuth
  validateAll signatoryHasValidAuthSettings $ AuthError AuthToSign
  validateAll
      (signatoryHasValidMobileForIdentifyToView signatorylinkauthenticationtoviewmethod)
    $ AuthError AuthToView
  validateAll signatoryThatIsApproverHasStandardAuthToSign
    $ ApproverError ApproverNonStandardAuth
  validateAll signatoryThatIsApproverHasNoPlacements $ ApproverError ApproverPlacedField
  where
    addError = tell . pure

    -- Report a custom error for every signatory violating `cond`.
    validateAll cond errorForSigId = traverse_ reportError invalidSignatories
      where
        invalidSignatories = filter (not . cond) signatories
        reportError        = addError . errorForSigId . signatorylinkid

    signatories = documentsignatorylinks doc

signatoryHasValidDeliverySettings :: SignatoryLink -> Bool
signatoryHasValidDeliverySettings sl =
  isAuthor sl || case signatorylinkdeliverymethod sl of
    EmailDelivery  -> isValidEmail $ getEmail sl
    MobileDelivery -> isValidPhoneForSMS $ getMobile sl
    EmailAndMobileDelivery ->
      isValidPhoneForSMS (getMobile sl) && isValidEmail (getEmail sl)
    PortalDelivery -> isValidEmail $ getEmail sl
    _              -> True

signatoryHasValidConfirmationSettings :: SignatoryLink -> Bool
signatoryHasValidConfirmationSettings sl =
  isAuthor sl || case signatorylinkconfirmationdeliverymethod sl of
    EmailConfirmationDelivery              -> isEmailValidOrEmpty sl
    EmailLinkConfirmationDelivery          -> isEmailValidOrEmpty sl
    MobileConfirmationDelivery             -> isMobileValidOrEmpty sl
    EmailAndMobileConfirmationDelivery     -> isEmailAndMobileValidOrEmpty sl
    EmailLinkAndMobileConfirmationDelivery -> isEmailAndMobileValidOrEmpty sl
    NoConfirmationDelivery                 -> True

signatoryHasValidNotificationSettings :: SignatoryLink -> Bool
signatoryHasValidNotificationSettings sl =
  isAuthor sl || case signatorylinknotificationdeliverymethod sl of
    NoNotificationDelivery             -> True
    EmailNotificationDelivery          -> isEmailValidOrEmpty sl
    MobileNotificationDelivery         -> isMobileValidOrEmpty sl
    EmailAndMobileNotificationDelivery -> isEmailAndMobileValidOrEmpty sl

signatoryHasValidAuthSettings :: SignatoryLink -> Bool
signatoryHasValidAuthSettings sl = authToSignIsValid sl && authToViewIsValid sl

authToViewIsValid :: SignatoryLink -> Bool
authToViewIsValid sl = case signatorylinkauthenticationtoviewmethod sl of
  SEBankIDAuthenticationToView -> True
  NOBankIDAuthenticationToView ->
    isJust (getFieldByIdentity PersonalNumberFI $ signatoryfields sl)
  LegacyDKNemIDAuthenticationToView -> True
  DKNemIDCPRAuthenticationToView    -> True
  DKNemIDPIDAuthenticationToView    -> True
  DKNemIDCVRAuthenticationToView    -> True
  SMSPinAuthenticationToView        -> True
  StandardAuthenticationToView      -> True
  IDINAuthenticationToView          -> True
  FITupasAuthenticationToView       -> True
  VerimiAuthenticationToView        -> True

-- checking for viewership is a temporary fix to help Telia. should be reverted when they get their shit together
authToSignIsValid :: SignatoryLink -> Bool
authToSignIsValid sl =
  signatoryrole sl
    == SignatoryRoleViewer
    || case signatorylinkauthenticationtosignmethod sl of
         SEBankIDAuthenticationToSign ->
           isJust (getFieldByIdentity PersonalNumberFI $ signatoryfields sl)
             && (  T.null (getPersonalNumber sl)
                || isGood (asValidSEBankIdPersonalNumber $ getPersonalNumber sl)
                )
         NOBankIDAuthenticationToSign -> T.null (getPersonalNumber sl)
           || isGood (asValidNOBankIdPersonalNumber $ getPersonalNumber sl)
         -- How does `T.null (getPersonalNumber sl)` square with
         -- `authToSignNeedsPersonalNumber DKNemIDAuthenticationToSign = True`?
         DKNemIDAuthenticationToSign -> T.null (getPersonalNumber sl)
           || isGood (asValidDanishSSN $ getPersonalNumber sl)
         SMSPinAuthenticationToSign ->
           isJust (getFieldByIdentity MobileFI $ signatoryfields sl)
             && (T.null (getMobile sl) || isGood (asValidPhoneForSMS $ getMobile sl))
         StandardAuthenticationToSign -> True
         IDINAuthenticationToSign     -> True
         FITupasAuthenticationToSign  -> T.null (getPersonalNumber sl)
           || isGood (asValidFinnishSSN $ getPersonalNumber sl)
         OnfidoDocumentCheckAuthenticationToSign         -> True
         OnfidoDocumentAndPhotoCheckAuthenticationToSign -> True

signatoryHasValidSSNOrEmailForIdentifyToView :: SignatoryLink -> Bool
signatoryHasValidSSNOrEmailForIdentifyToView sl =
  case signatorylinkauthenticationtoviewmethod sl of
    SEBankIDAuthenticationToView      -> isGood . asValidSwedishSSN $ getPersonalNumber sl
    NOBankIDAuthenticationToView -> isGood . asValidNorwegianSSN $ getPersonalNumber sl
    LegacyDKNemIDAuthenticationToView -> isGood . asValidDanishSSN $ getPersonalNumber sl
    DKNemIDCPRAuthenticationToView    -> isGood . asValidDanishSSN $ getPersonalNumber sl
    DKNemIDPIDAuthenticationToView    -> isGood . asValidDanishSSN $ getPersonalNumber sl
    DKNemIDCVRAuthenticationToView    -> isGood . asValidDanishCVR $ getPersonalNumber sl
    FITupasAuthenticationToView ->
      T.null (getPersonalNumber sl) || isGood (asValidFinnishSSN $ getPersonalNumber sl)
    SMSPinAuthenticationToView   -> True
    StandardAuthenticationToView -> True
    VerimiAuthenticationToView   -> isValidEmail $ getEmail sl
    IDINAuthenticationToView     -> isValidEmail $ getEmail sl

signatoryHasValidMobileForIdentifyToView
  :: HasSomeUserInfo a => (a -> AuthenticationToViewMethod) -> a -> Bool
signatoryHasValidMobileForIdentifyToView viewmethod sl = case viewmethod sl of
  NOBankIDAuthenticationToView -> isGood (asValidPhoneForNorwegianBankID (getMobile sl))
    || isEmpty (asValidPhoneForNorwegianBankID (getMobile sl))
  SMSPinAuthenticationToView -> isGood $ asValidPhoneForSMS (getMobile sl)
  _ -> True

signatoryThatIsApproverHasStandardAuthToSign :: SignatoryLink -> Bool
signatoryThatIsApproverHasStandardAuthToSign sl =
  not (isApprover sl)
    || (signatorylinkauthenticationtosignmethod sl == StandardAuthenticationToSign)

signatoryThatIsApproverHasNoPlacements :: SignatoryLink -> Bool
signatoryThatIsApproverHasNoPlacements sl =
  not (isApprover sl) || null (concat $ fieldPlacements <$> signatoryfields sl)

isEmailValidOrEmpty :: HasSomeUserInfo a => a -> Bool
isEmailValidOrEmpty sl = T.null (getEmail sl) || isValidEmail (getEmail sl)

isMobileValidOrEmpty :: HasSomeUserInfo a => a -> Bool
isMobileValidOrEmpty sl = T.null (getMobile sl) || isValidPhoneForSMS (getMobile sl)

isEmailAndMobileValidOrEmpty :: HasSomeUserInfo a => a -> Bool
isEmailAndMobileValidOrEmpty sl = isEmailValidOrEmpty sl && isMobileValidOrEmpty sl
