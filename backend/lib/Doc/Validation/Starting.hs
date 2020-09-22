module Doc.Validation.Starting
  ( ApproverErrorType(..)
  , AuthType(..)
  , MessageType(..)
  , StartingError(..)
  , errorExplanation
  , validateDocumentForStarting
  , CanDocBeStarted(..)
  )
where

import Control.Arrow ((&&&))
import Control.Monad.Writer
import Data.Aeson
import Data.Foldable (traverse_)
import Data.List.Extra (groupSort)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import GHC.Generics
import qualified Data.Text as T

import Doc.DocInfo
import Doc.SignatoryFieldUtils
import Doc.SignatoryLinkID
import Doc.Types.Document
import Doc.Types.SignatoryAttachment
import Doc.Types.SignatoryField
import Doc.Types.SignatoryLink
import InputValidation
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

data MessageType = InvitationMessage | ConfirmationMessage | NotificationMessage deriving Generic
data AuthType = BasicAuth | AuthToView | AuthToSign | AuthToViewArchived deriving Generic
data ApproverErrorType = ApproverNonStandardAuth | ApproverPlacedField deriving Generic

data VerimiQesError
  = IncompatibleSigningMethod SignatoryLinkID
  | NonAuthorField SignatoryLinkID
  | FieldEditableBySignatory SignatoryLinkID
  | OrderConflict [SignatoryLinkID]
  | MissingAttachment SignatoryLinkID Text
  deriving Generic

data StartingError
  = TemplateError
  | AuthorApproverError
  | NoSigningParty
  | FileError
  | ContactError MessageType SignatoryLinkID
  | AuthError AuthType SignatoryLinkID
  | ApproverError ApproverErrorType SignatoryLinkID
  | VerimiQesError VerimiQesError
  deriving Generic

data CanDocBeStarted = CanBeStarted | CannotBeStarted (NonEmpty StartingError)

genericToJsonOptions :: Options
genericToJsonOptions = defaultOptions { fieldLabelModifier     = camelTo2 '_'
                                      , constructorTagModifier = camelTo2 '_'
                                      }

instance ToJSON CanDocBeStarted where
  toJSON CanBeStarted           = object ["can_start" .= True]
  toJSON (CannotBeStarted errs) = object ["can_start" .= False, "errors" .= errs]

instance ToJSON MessageType where
  toJSON = genericToJSON genericToJsonOptions

instance ToJSON AuthType where
  toJSON = genericToJSON genericToJsonOptions

instance ToJSON ApproverErrorType where
  toJSON = genericToJSON genericToJsonOptions

instance ToJSON VerimiQesError where
  toJSON = genericToJSON genericToJsonOptions

instance ToJSON StartingError where
  toJSON = genericToJSON genericToJsonOptions

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
  VerimiQesError (IncompatibleSigningMethod slid) ->
    "Signatory "
      <> showt slid
      <> " has a signing method other than Verimi QES. Only Verimi QES signing is allowed."
  VerimiQesError (NonAuthorField slid) ->
    "There is a field assigned to be filled by signatory "
      <> showt slid
      <> ", but only the author is allowed to fill fields in documents using Verimi QES signing."
  VerimiQesError (FieldEditableBySignatory slid) ->
    "There is a field that is set to be editable by signatory "
      <> showt slid
      <> ", but only the author is allowed to edit fields in documents that Verimi QES signing."
  VerimiQesError (OrderConflict slids) ->
    "Conflicting signatories: "
      <> T.intercalate ", " (map showt slids)
      <> " are configured to sign at the same time, but Verimi QES requires each signatory to have their own 'sign order'."
  VerimiQesError (MissingAttachment _ name) ->
    "Attachment '"
      <> name
      <> "' is missing. Verimi QES requires all attachments to be present when starting the document."

validateDocumentForStarting :: Document -> CanDocBeStarted
validateDocumentForStarting doc = maybe CanBeStarted CannotBeStarted
  $ nonEmpty startingErrors
  where
    addError    = tell . pure
    signatories = documentsignatorylinks doc
    -- Report a custom error for every signatory violating `cond`.
    validateAll cond errorForSigId = traverse_ reportError invalidSignatories
      where
        invalidSignatories = filter (not . cond) signatories
        reportError        = addError . errorForSigId . signatorylinkid

    startingErrors :: [StartingError]
    startingErrors = execWriter $ do
      when (isTemplate doc) $ addError TemplateError
      when (any (isAuthor && isApprover) signatories) $ addError AuthorApproverError
      unless (any isSignatory signatories) $ addError NoSigningParty
      when (isNothing $ documentfile doc) $ addError FileError
      forM_ (validateVerimiQesUsage doc) $ addError . VerimiQesError

      validateAll signatoryHasValidDeliverySettings $ ContactError InvitationMessage
      validateAll signatoryHasValidConfirmationSettings $ ContactError ConfirmationMessage
      validateAll signatoryHasValidNotificationSettings $ ContactError NotificationMessage
      validateAll signatoryHasValidSSNOrEmailForIdentifyToView $ AuthError BasicAuth
      validateAll signatoryHasValidAuthSettings $ AuthError AuthToSign
      validateAll
          (signatoryHasValidMobileForIdentifyToView
            signatorylinkauthenticationtoviewmethod
          )
        $ AuthError AuthToView
      validateAll signatoryThatIsApproverHasStandardAuthToSign
        $ ApproverError ApproverNonStandardAuth
      validateAll signatoryThatIsApproverHasNoPlacements
        $ ApproverError ApproverPlacedField

validateVerimiQesUsage :: Document -> [VerimiQesError]
validateVerimiQesUsage doc
  | documentUsesVerimiQes doc = execWriter $ do
    let signatories        = documentsignatorylinks doc
        signingSignatories = filter isSignatory signatories

    -- No two _signing_ signatories can have the same sign order.
    let orderconflicts = filter ((> 1) . length) . map snd . groupSort $ map
          (signatorysignorder &&& signatorylinkid)
          signingSignatories
    forM_ orderconflicts $ write . OrderConflict

    forM_ signatories $ \sig -> do
      let slid             = signatorylinkid sig

      -- All _signing_ signatories have to use the Verimi QES signing method.
      let authtosignmethod = signatorylinkauthenticationtosignmethod sig
      unless (not isSignatory sig || VerimiQesAuthenticationToSign == authtosignmethod)
        $ do
            write $ IncompatibleSigningMethod slid

      -- No _visible_ signatory fields can be changed after the document has been started.
      let visiblefields = filter (not . null . fieldPlacements) $ signatoryfields sig
      when (any fieldShouldBeFilledBySender visiblefields) $ do
        write $ NonAuthorField slid
      when (any ((== Just True) . fieldEditableBySignatory) visiblefields) $ do
        write $ FieldEditableBySignatory slid

      -- All attachments have to be present when the document is started, i.e.
      -- attachments can't be requested as part of the signing process.
      forM_ (signatoryattachments sig) $ \att -> do
        let name = signatoryattachmentname att
        unless (isJust $ signatoryattachmentfile att) . write $ MissingAttachment slid
                                                                                  name
  | otherwise = []
  where
    -- In particular, write :: (MonadWriter [a] m) => a -> m ()
    write :: (Applicative f, MonadWriter (f a) m) => a -> m ()
    write = tell . pure

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
  LegacyDKNemIDAuthenticationToView       -> True
  DKNemIDCPRAuthenticationToView          -> True
  DKNemIDPIDAuthenticationToView          -> True
  DKNemIDCVRAuthenticationToView          -> True
  SMSPinAuthenticationToView              -> True
  StandardAuthenticationToView            -> True
  IDINAuthenticationToView                -> True
  FITupasAuthenticationToView             -> True
  VerimiAuthenticationToView              -> True
  OnfidoDocumentCheckAuthenticationToView -> True
  OnfidoDocumentAndPhotoCheckAuthenticationToView -> True

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
         VerimiQesAuthenticationToSign                   -> isValidEmail $ getEmail sl

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
    SMSPinAuthenticationToView              -> True
    StandardAuthenticationToView            -> True
    VerimiAuthenticationToView              -> isValidEmail $ getEmail sl
    IDINAuthenticationToView                -> isValidEmail $ getEmail sl
    -- TODO do we need some validation for Onfido?
    OnfidoDocumentCheckAuthenticationToView -> True
    OnfidoDocumentAndPhotoCheckAuthenticationToView -> True

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
