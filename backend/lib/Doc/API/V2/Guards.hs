module Doc.API.V2.Guards (
-- * Document guards
  guardThatDocumentIs
, guardDocumentStatus
, guardThatDocumentCanBeStarted
, guardThatDocumentCanBeTrashedByUser
, guardThatDocumentCanBeDeletedByUser
, documentCanBeStarted
, guardThatObjectVersionMatchesIfProvided
, guardThatConsentModulesAreOnSigningParties
, guardThatAttachmentDetailsAreConsistent
, guardDocumentMoveIsAllowed
, guardDocumentCreateInFolderIsAllowed
-- * User guards
, guardThatUserExists
, guardThatUserIsAuthor
, guardUserMayImpersonateUserGroupForEid
, guardThatUserHasActionPermission
-- * Signatory guards
, guardGetSignatoryFromIdForDocument
, guardSignatoryNeedsToIdentifyToView
, guardSignatoryHasNotIdentifiedToView
, guardCanSetAuthenticationToViewForSignatoryWithValues
, guardCanSetAuthenticationToSignForSignatoryWithValue
, guardSignatoryRoleIs
, guardApproverHasNotApproved
, guardSignatoryHasNotSigned
, guardSigningPartyHasNeitherSignedNorApproved
, guardThatDocumentHasntBeenForwadedTooManyTimes
, guardThatAllAttachmentsAreAcceptedOrIsAuthor
, guardThatAllSignatoryAttachmentsAreUploadedOrMarked
, guardThatRadioButtonValuesAreValid
, guardThatSignaturesAreFilled
, guardThatAllConsentQuestionsHaveResponse
, guardThatAuthorIsNotApprover
-- * Joined guard for read-only functions
, guardThatDocumentIsReadableBySignatories
, guardAccessToDocumentWithSignatory
) where

import Control.Conditional (whenM)
import Data.Either (rights)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import AccessControl.Check
import AccessControl.Types
import API.V2
import API.V2.Errors
import API.V2.Parameters
import API.V2.Utils
import DB
import Doc.API.V2.JSON.AttachmentDetails
import Doc.API.V2.JSON.Fields
import Doc.API.V2.JSON.SignatoryConsentQuestion
import Doc.Conditions
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad (DocumentMonad, theDocument, withDocumentID)
import Doc.DocUtils
import Doc.Model.Query
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import Doc.Validation.Starting
import File.FileID
import Folder.Types
import InputValidation
import Kontra
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

-- | Guard a given condition on the document, throws an error with
-- `documentStateError` when this does not match.
--
-- Prefer to use a more specific guard if this satisfies your need.
guardThatDocumentIs :: Kontrakcja m => (Document -> Bool) -> Text -> Document -> m ()
guardThatDocumentIs f text doc = unless (f doc) . apiError $ documentStateError text

-- | Guard that the document status matches, otherwise throw a `documentStateError`
guardDocumentStatus :: Kontrakcja m => DocumentStatus -> Document -> m ()
guardDocumentStatus s doc = unless (documentstatus doc == s)
                                   (apiError . documentStateError $ errorMsg)
  where errorMsg = "The document status should be '" <> showt s <> "'."


guardThatDocumentCanBeTrashedByUser
  :: Kontrakcja m => User -> [AccessRole] -> DocumentID -> m ()
guardThatDocumentCanBeTrashedByUser =
  guardThatDocumentCanBeTrashedOrDeletedByUserWithCond
    (isNothing . signatorylinkdeleted)
    "The document is in Trash"

guardThatDocumentCanBeDeletedByUser
  :: Kontrakcja m => User -> [AccessRole] -> DocumentID -> m ()
guardThatDocumentCanBeDeletedByUser =
  guardThatDocumentCanBeTrashedOrDeletedByUserWithCond
    (isNothing . signatorylinkreallydeleted)
    "Document was purged"

guardThatDocumentCanBeTrashedOrDeletedByUserWithCond
  :: Kontrakcja m
  => (SignatoryLink -> Bool)
  -> Text
  -> User
  -> [AccessRole]
  -> DocumentID
  -> m ()
guardThatDocumentCanBeTrashedOrDeletedByUserWithCond cond errorMsg user allUserRoles did
  = withDocumentID did $ do
    let condAPIError = apiError . documentStateError $ errorMsg
    msl <- getSigLinkFor user <$> theDocument
    case msl of -- This might be a user with an account
      (Just sl) -> do
        unless (cond sl) condAPIError
      Nothing -> do
        let msgNoAuthor =
              "Document doesn't have author signatory link connected with user account"
        asl <-
          apiGuardJust (serverError msgNoAuthor) =<< (getAuthorSigLink <$> theDocument)
        guardThatUserHasActionPermissionWithAllRoles DeleteA allUserRoles =<< theDocument
        unless (cond asl) condAPIError
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocumentIs (not . isPending)
                        "Pending documents can not be trashed or deleted"
      =<< theDocument

-- | Internal function used in guardDocumentAuthorIs
-- Helps code reuse and keep error messages consistent
getDocumentAuthor :: Kontrakcja m => Document -> m User
getDocumentAuthor doc = do
  let msgNoAuthor =
        "Document doesn't have author signatory link connected with user account"
  authorUserId <- apiGuardJust (serverError msgNoAuthor) $ getAuthorUserId doc
  let msgNoUser =
        "Document doesn't have author user account for the author signatory link"
  apiGuardJustM (serverError msgNoUser) . dbQuery $ GetUserByIDIncludeDeleted authorUserId

-- | Internal function used in all guards on User
-- Helps code reuse and keep error messages consistent
guardDocumentAuthorIs :: Kontrakcja m => (User -> Bool) -> Document -> m ()
guardDocumentAuthorIs condition doc = do
  author <- getDocumentAuthor doc
  unless (condition author) $ do
    apiError documentActionForbidden

guardThatUserExists :: Kontrakcja m => UserID -> m User
guardThatUserExists uid = do
  mUser <- dbQuery $ GetUserByID uid
  case mUser of
    Nothing   -> apiError $ resourceNotFound "A user with that ID was not found"
    Just user -> return user

guardThatUserIsAuthor :: Kontrakcja m => User -> Document -> m ()
guardThatUserIsAuthor user = guardDocumentAuthorIs (\a -> user ^. #id == a ^. #id)

guardThatUserHasActionPermission
  :: Kontrakcja m => AccessAction -> User -> Document -> m ()
guardThatUserHasActionPermission action user doc = do
  requiredPerm <- apiRequireAnyPermission [ canDo action res | res <- docResources doc ]
  apiAccessControlWithError user
                            requiredPerm
                            (apiError documentActionForbidden)
                            (return ())


guardThatUserHasActionPermissionWithAllRoles
  :: (Kontrakcja m) => AccessAction -> [AccessRole] -> Document -> m ()
guardThatUserHasActionPermissionWithAllRoles action allUserRoles doc = do
  if accessControlCheckAny allUserRoles [ canDo action res | res <- docResources doc ]
    then return ()
    else apiError documentActionForbidden

guardThatObjectVersionMatchesIfProvided :: Kontrakcja m => DocumentID -> m ()
guardThatObjectVersionMatchesIfProvided did = do
  reqObjectVersion <- apiV2ParameterOptional (ApiV2ParameterInt "object_version")
  case reqObjectVersion of
    Nothing -> return ()
    Just ov ->
      dbQuery (CheckDocumentObjectVersionIs did (fromIntegral ov))
        `catchDBExtraException` (\e@DocumentObjectVersionDoesNotMatch{} ->
                                  apiError $ documentObjectVersionMismatch e
                                )

guardThatAttachmentDetailsAreConsistent :: Kontrakcja m => [AttachmentDetails] -> m ()
guardThatAttachmentDetailsAreConsistent ads = do
  guardUnique . rights $ map aadFileOrFileParam ads
  guardUnique $ map aadName ads

  where
    guardUnique :: (Kontrakcja m, Eq a) => [a] -> m ()
    guardUnique xs
      | hasDuplicates xs = apiError $ requestParameterInvalid
        "attachments"
        "Attachments must have a unique name and/or file parameter."
      | otherwise = return ()

    hasDuplicates :: Eq a => [a] -> Bool
    hasDuplicates []       = False
    hasDuplicates (x : xs) = x `elem` xs || hasDuplicates xs

guardFolderActionIsAllowed :: Kontrakcja m => User -> [(AccessAction, FolderID)] -> m ()
guardFolderActionIsAllowed user acts_fids = do
  requiredPerm <- apiRequireAllPermissions
    [ canDo act $ DocumentInFolderR fid | (act, fid) <- acts_fids ]
  apiAccessControl user requiredPerm $ return ()

guardDocumentCreateInFolderIsAllowed :: Kontrakcja m => User -> FolderID -> m ()
guardDocumentCreateInFolderIsAllowed user location =
  guardFolderActionIsAllowed user [(CreateA, location)]

guardDocumentMoveIsAllowed :: Kontrakcja m => User -> FolderID -> FolderID -> m ()
guardDocumentMoveIsAllowed user oldLocation newLocation =
  when (oldLocation /= newLocation)
    $ guardFolderActionIsAllowed user [(CreateA, newLocation), (DeleteA, oldLocation)]

-- | Make sure the given user (the document author) is allowed to use the
-- display name and service id of the given user group.
guardUserMayImpersonateUserGroupForEid :: Kontrakcja m => User -> Document -> m ()
guardUserMayImpersonateUserGroupForEid user doc
  | Just ugid <- documentusergroupforeid doc = do
    requiredPerm <- apiRequirePermission . canDo ReadA $ EidIdentityR ugid
    apiAccessControl user requiredPerm $ return ()
guardUserMayImpersonateUserGroupForEid _ _ = return ()

guardGetSignatoryFromIdForDocument
  :: (Kontrakcja m, DocumentMonad m) => SignatoryLinkID -> m SignatoryLink
guardGetSignatoryFromIdForDocument slid = do
  doc <- theDocument
  apiGuardJust (signatoryLinkForDocumentNotFound (documentid doc) slid)
    . getSigLinkFor slid
    $ doc

guardSignatoryNeedsToIdentifyToView :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSignatoryNeedsToIdentifyToView slid doc = do
  let msl = getSigLinkFor slid doc
  -- We let author bypass authentication to view, but only if the document was
  -- not yet closed (i.e. we're not dealing with authenticate to view archived).
  unless (isAuthor msl && not (isClosed doc)) $ do
    whenM (signatoryNeedsToIdentifyToView (fromJust msl) doc) $ do
      apiError $ signatoryStateError "Authorization to view needed before signing"

guardSignatoryHasNotIdentifiedToView
  :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSignatoryHasNotIdentifiedToView slid doc = when
  (signatorylinkidentifiedtoview . fromJust $ getSigLinkFor slid doc)
  (apiError $ signatoryStateError "The party has already identified to view")

guardCanSetAuthenticationToViewForSignatoryWithValues
  :: Kontrakcja m
  => SignatoryLinkID
  -> AuthenticationKind
  -> AuthenticationToViewMethod
  -> Maybe Text
  -> Maybe Text
  -> Document
  -> m ()
guardCanSetAuthenticationToViewForSignatoryWithValues slid authKind authType mSSN mMobile doc
  = do
    guardAuthenticationMethodsCanMix authToView authToSign authToViewArchived
    -- Check if either a valid SSN for authToView is set or is provided
    case mSSN of
      Nothing -> unless
        (isValidSSNForAuthenticationToView authType $ getPersonalNumber sl)
        (  apiStateError
        $  "Signatory does not have a valid personal number "
        <> "for the authentication method and you did not provide one"
        )

      Just ssn -> unless
        (isValidSSNForAuthenticationToView authType ssn)
        (  apiStateError
        $  "The personal number you provided is not valid "
        <> "for the authentication method"
        )
    -- Check if either a valid mobile for authToView is set or is provided
    case mMobile of
      Nothing -> unless
        (isValidMobileForAuthenticationToView authType authKind $ getMobile sl)
        (  apiStateError
        $  "Party does not have a valid mobile number set "
        <> "for the authentication method and you did not provide one"
        )

      Just mobile -> unless
        (isValidMobileForAuthenticationToView authType authKind mobile)
        (  apiStateError
        $  "The mobile number you provided is not valid "
        <> "for the authentication method"
        )
  where
    sl            = fromJust $ getSigLinkFor slid doc
    apiStateError = apiError . signatoryStateError

    authToView    = case authKind of
      AuthenticationToView         -> authType
      AuthenticationToViewArchived -> signatorylinkauthenticationtoviewmethod sl
    authToViewArchived = case authKind of
      AuthenticationToView         -> signatorylinkauthenticationtoviewarchivedmethod sl
      AuthenticationToViewArchived -> authType
    authToSign = signatorylinkauthenticationtosignmethod sl

    isValidSSNForAuthenticationToView :: AuthenticationToViewMethod -> Text -> Bool
    isValidSSNForAuthenticationToView StandardAuthenticationToView _ = True
    isValidSSNForAuthenticationToView SMSPinAuthenticationToView   _ = True
    isValidSSNForAuthenticationToView SEBankIDAuthenticationToView ssn =
      isGood . asValidSwedishSSN $ ssn
    isValidSSNForAuthenticationToView NOBankIDAuthenticationToView ssn =
      isGood . asValidNorwegianSSN $ ssn
    isValidSSNForAuthenticationToView DKNemIDAuthenticationToView ssn =
      isGood . asValidDanishSSN $ ssn
    isValidSSNForAuthenticationToView FITupasAuthenticationToView ssn =
      (isEmpty || isGood) $ asValidFinnishSSN ssn
    isValidSSNForAuthenticationToView VerimiAuthenticationToView _ = True
    isValidSSNForAuthenticationToView IDINAuthenticationToView   _ = True
    isValidMobileForAuthenticationToView
      :: AuthenticationToViewMethod -> AuthenticationKind -> Text -> Bool
    isValidMobileForAuthenticationToView StandardAuthenticationToView _ _ = True
    isValidMobileForAuthenticationToView SMSPinAuthenticationToView AuthenticationToViewArchived mobile
      = (isGood || isEmpty) . asValidPhoneForSMS $ mobile
    isValidMobileForAuthenticationToView SMSPinAuthenticationToView AuthenticationToView mobile
      = isGood . asValidPhoneForSMS $ mobile
    isValidMobileForAuthenticationToView SEBankIDAuthenticationToView _ _ = True
    isValidMobileForAuthenticationToView DKNemIDAuthenticationToView  _ _ = True
    isValidMobileForAuthenticationToView NOBankIDAuthenticationToView _ mobile =
      (isGood || isEmpty) $ asValidPhoneForNorwegianBankID mobile
    isValidMobileForAuthenticationToView FITupasAuthenticationToView _ _ = True
    isValidMobileForAuthenticationToView VerimiAuthenticationToView  _ _ = True
    isValidMobileForAuthenticationToView IDINAuthenticationToView    _ _ = True

-- | `guardCanSetAuthenticationToSignForSignatoryWithValue _ authToSign mSSN
-- mMobile _` makes sure we can set the sign method to the given method. If the
-- sign method requires a personal number, then we may also set an SSN as part
-- of changing the signing method; if we aren't given an SSN and the signatory
-- hasn't already set one then it is going to be supplied as part of the signing
-- process and we needn't worry about it here. The same holds for the mobile
-- number.
--
-- Bugs: plenty! We allow changing between two different sign methods without
-- changing the personal number, in which case we don't validate that the set
-- personal number is still valid! This is not a big issue, since then the ssn
-- is invalid anyway, and we can't get the provider to sign for it.
guardCanSetAuthenticationToSignForSignatoryWithValue
  :: Kontrakcja m
  => SignatoryLinkID
  -> AuthenticationToSignMethod
  -> Maybe Text
  -> Maybe Text
  -> Document
  -> m ()
guardCanSetAuthenticationToSignForSignatoryWithValue slid authToSign mSSN mMobile doc =
  do
    let sl                 = fromJust $ getSigLinkFor slid doc
        authToView         = signatorylinkauthenticationtoviewmethod sl
        authToViewArchived = signatorylinkauthenticationtoviewarchivedmethod sl
    guardAuthenticationMethodsCanMix authToView authToSign authToViewArchived

    -- Note that we have mSSN = Just ssn only if authToSignNeedsPersonalNumber
    -- authToSign = True.
    whenJust mSSN $ \ssn -> do
      when (signatorylinkidentifiedtoview sl && ssn /= getPersonalNumber sl)
        . apiError
        $ signatoryStateError
            "The party has authenticated to view, therefore you can't change the authentication value"

      -- validate is non-trivial iff authToSignNeedsPersonalNumber authToSign = True
      let validate = case authToSign of
            SEBankIDAuthenticationToSign            -> asValidSEBankIdPersonalNumber
            DKNemIDAuthenticationToSign             -> asValidDanishSSN
            StandardAuthenticationToSign            -> Good
            SMSPinAuthenticationToSign              -> Good
            NOBankIDAuthenticationToSign            -> Good
            FITupasAuthenticationToSign             -> Good
            IDINAuthenticationToSign                -> Good
            OnfidoDocumentCheckAuthenticationToSign -> Good
            OnfidoDocumentAndPhotoCheckAuthenticationToSign -> Good

      -- Empty is allowed only if we don't need it for
      -- AuthenticationToViewMethod
      case validate ssn of
        Empty -> do
          when (authToViewNeedsPersonalNumber authToView)
            . apiError
            $ signatoryStateError
                "You provided an empty authentication value, needs a value for authentication to view"
        Bad -> do
          let
            name = case authToSign of
              SEBankIDAuthenticationToSign -> "Swedish BankID"
              DKNemIDAuthenticationToSign  -> "Danish NemID"
              auth ->
                unexpectedError $ "unexpected authentication to sign: " <> showt auth
          apiError
            .  signatoryStateError
            $  "The authentication value provided is not valid for "
            <> name
        Good _ -> return ()

    -- Note that we have mMobile = Just mobile only if
    -- authToSignNeedsMobileNumer authToSign = True, which is only the case for
    -- SMSPinAuthenticationToSign.
    whenJust mMobile $ \mobile -> case authToSign of
      SMSPinAuthenticationToSign -> do
        -- If the signatory has authenticated to view with
        -- NOBankIDAuthenticationToView and a valid number, then we can't
        -- change the mobile number!
        when
            (and
              [ authToView == NOBankIDAuthenticationToView
              , signatorylinkidentifiedtoview sl
              , getMobile sl /= ""
              , mobile /= getMobile sl
              ]
            )
          . apiError
          $ signatoryStateError
              "The party has authenticated to view with Norwegian BankID, therefore you can't change the mobile number"

        let
          -- If we are given a mobile number we need to make sure it doesn't
          -- invalidate NOBankIDAuthenticationToView
          validate
            | signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView
            = asValidPhoneForNorwegianBankID
            | otherwise
            = asValidPhoneForSMS

          validateError
            | signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView
            = "Mobile number needs to be a valid Norwegian number as Norwegian BankID is set as authentication to view"
            | otherwise
            = "Mobile number needs to be a valid phone number."

        case validate mobile of
          Bad    -> apiError $ signatoryStateError validateError
          Empty  -> return ()
          Good _ -> return ()

      StandardAuthenticationToSign            -> return ()
      SEBankIDAuthenticationToSign            -> return ()
      NOBankIDAuthenticationToSign            -> return ()
      DKNemIDAuthenticationToSign             -> return ()
      IDINAuthenticationToSign                -> return ()
      FITupasAuthenticationToSign             -> return ()
      OnfidoDocumentCheckAuthenticationToSign -> return ()
      OnfidoDocumentAndPhotoCheckAuthenticationToSign -> return ()


guardAuthenticationMethodsCanMix
  :: Kontrakcja m
  => AuthenticationToViewMethod
  -> AuthenticationToSignMethod
  -> AuthenticationToViewMethod
  -> m ()
guardAuthenticationMethodsCanMix authToView authToSign authToViewArchived = do
  unless
    (authenticationMethodsCanMix authToView authToSign authToViewArchived)
    (apiError . signatoryStateError $ mconcat
      [ "Can't mix "
      , showt authToView
      , ", "
      , showt authToSign
      , " and "
      , showt authToViewArchived
      , " (archived)."
      ]
    )

guardSignatoryRoleIs
  :: Kontrakcja m => SignatoryRole -> SignatoryLinkID -> Document -> m ()
guardSignatoryRoleIs roleExpected slid doc = do
  let msl            = getSigLinkFor slid doc
      roleActual     = signatoryrole <$> msl
      showRoleActual = maybe "<none>" show roleActual
  when
    (maybe True (roleExpected /=) roleActual)
    (  apiError
    .  signatoryStateError
    .  T.pack
    $  "Wrong signatory role, expected '"
    ++ show roleExpected
    ++ "', but got '"
    ++ showRoleActual
    ++ "'"
    )

guardApproverHasNotApproved :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardApproverHasNotApproved slid doc = when
  (isApproverAndHasApproved $ getSigLinkFor slid doc)
  (apiError . signatoryStateError $ "The approver has already approved")

guardSignatoryHasNotSigned :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSignatoryHasNotSigned slid doc = when
  (isSignatoryAndHasSigned $ getSigLinkFor slid doc)
  (apiError . signatoryStateError $ "The signatory has already signed")

guardSigningPartyHasNeitherSignedNorApproved
  :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSigningPartyHasNeitherSignedNorApproved slid doc = when
  ((isApproverAndHasApproved || isSignatoryAndHasSigned) $ getSigLinkFor slid doc)
  ( apiError
  . signatoryStateError
  $ "The signing party has either signed or approved already"
  )

guardThatDocumentHasntBeenForwadedTooManyTimes :: Kontrakcja m => Document -> m ()
guardThatDocumentHasntBeenForwadedTooManyTimes doc = when
  (length (filter isForwarded $ documentsignatorylinks doc) > 100)
  ( apiError
  . documentStateError
  $ "This document has been forwarded too many times already"
  )

-- Checks if document can be started. Throws matching API exception if it does not
guardThatDocumentCanBeStarted :: Kontrakcja m => Document -> m ()
guardThatDocumentCanBeStarted = maybe (return ()) apiError . documentCanBeStarted

documentCanBeStarted :: Document -> Maybe APIError
documentCanBeStarted = fmap toApiError . listToMaybe . validateDocumentForStarting
  where toApiError = documentStateError . errorExplanation

guardThatRadioButtonValuesAreValid
  :: Kontrakcja m
  => SignatoryLinkID
  -> SignatoryFieldsValuesForSigning
  -> Document
  -> m ()
guardThatRadioButtonValuesAreValid slid (SignatoryFieldsValuesForSigning signfields) doc
  = do
    let sl = fromJust $ getSigLinkFor slid doc
        radioValIsValid (fi@(RadioGroupFI _), StringFTV signval) = fromMaybe False $ do
          SignatoryRadioGroupField srgf <- getFieldByIdentity fi $ signatoryfields sl
          return $ signval `elem` srgfValues srgf
        radioValIsValid _ = True -- non radio group fields are skipped
    unless (all radioValIsValid signfields) . apiError $ signatoryStateError
      "RadioGroup selected value is not in allowed values."

guardThatSignaturesAreFilled
  :: Kontrakcja m
  => SignatoryLinkID
  -> SignatoryFieldsValuesForSigning
  -> Document
  -> m ()
guardThatSignaturesAreFilled slid (SignatoryFieldsValuesForSigning signfields) doc = do
  let sl = fromJust $ getSigLinkFor slid doc
      signatureIsFilled (fi@(SignatureFI _), FileFTV contents) = fromMaybe False $ do
        SignatorySignatureField ssf <- getFieldByIdentity fi $ signatoryfields sl
        return
          $  not (ssfObligatory ssf)
          || null (ssfPlacements ssf)
          || BS.length contents
          >  0
      signatureIsFilled _ = True -- non signature fields are skipped
  unless (all signatureIsFilled signfields) . apiError $ signatoryStateError
    "Signature missing."

guardThatAllAttachmentsAreAcceptedOrIsAuthor
  :: Kontrakcja m => SignatoryLinkID -> [FileID] -> Document -> m ()
guardThatAllAttachmentsAreAcceptedOrIsAuthor slid acceptedAttachments doc = do
  unless (allRequiredAttachmentsAreOnList acceptedAttachments doc)
    . unless (isAuthor . fromJust $ getSigLinkFor slid doc)
    $ -- Author does not need to accept attachments
      apiError (signatoryStateError "Some mandatory author attachments aren't accepted")

guardThatAllSignatoryAttachmentsAreUploadedOrMarked
  :: Kontrakcja m => SignatoryLinkID -> [Text] -> Document -> m ()
guardThatAllSignatoryAttachmentsAreUploadedOrMarked slid notUploadedSignatoryAttachments doc
  = do
    let
      sigAttachments = signatoryattachments . fromJust $ getSigLinkFor slid doc
      requiredSigAttachments = filter signatoryattachmentrequired sigAttachments
      optionalSigAttachments = filter (not . signatoryattachmentrequired) sigAttachments
      optionalSigAttachmentsNames = map signatoryattachmentname optionalSigAttachments
    -- all not uploaded signatory attachment names must exist
    when (any (`notElem` optionalSigAttachmentsNames) notUploadedSignatoryAttachments)
      . apiError
      $ signatoryStateError "Optional signatory attachment name does not exist"
    -- all required signatory attachments must be uploaded
    when (any (isNothing . signatoryattachmentfile) requiredSigAttachments)
      . apiError
      $ signatoryStateError "Some mandatory signatory attachments aren't uploaded"
    -- all optional signatory attachments must be uploaded XOR marked as not uploaded
    when
        (any
          (\sa ->
            isJust (signatoryattachmentfile sa)
              &&     signatoryattachmentname sa
              `elem` notUploadedSignatoryAttachments
          )
          optionalSigAttachments
        )
      . apiError
      $ signatoryStateError
          "Some optional signatory attachments are uploaded but are marked as not uploaded"
    when
        (any
          (\sa ->
            isNothing (signatoryattachmentfile sa)
              &&        signatoryattachmentname sa
              `notElem` notUploadedSignatoryAttachments
          )
          optionalSigAttachments
        )
      . apiError
      $ signatoryStateError
          "Some optional signatory attachments are not uploaded and are not marked as such"

guardThatConsentModulesAreOnSigningParties :: Kontrakcja m => Document -> m ()
guardThatConsentModulesAreOnSigningParties doc =
  forM_ (documentsignatorylinks doc) $ \signatory -> do
    case signatorylinkconsenttitle signatory of
      Just _ | not (isSignatory signatory) -> apiError $ requestParameterInvalid
        "consent_module"
        "Consent module not allowed on non-signing party"
      _ -> return ()

guardThatAuthorIsNotApprover :: Kontrakcja m => Document -> m ()
guardThatAuthorIsNotApprover doc = do
  when (any (isAuthor && isApprover) (documentsignatorylinks doc)) $ do
    apiError $ requestParameterInvalid "document" "Author can't be an approver"

guardThatAllConsentQuestionsHaveResponse
  :: Kontrakcja m
  => SignatoryLinkID
  -> SignatoryConsentResponsesForSigning
  -> Document
  -> m ()
guardThatAllConsentQuestionsHaveResponse slid (SignatoryConsentResponsesForSigning responses) doc
  = do
    let sl          = fromJust $ getSigLinkFor slid doc
        qs          = signatorylinkconsentquestions sl
        questionIDs = map scqID qs
        responseIDs = map fst responses
    unless (all (`elem` responseIDs) questionIDs) . apiError $ requestParameterInvalid
      "consent_responses"
      "Some consent questions have not been answered"
    unless (all (`elem` questionIDs) responseIDs) . apiError $ requestParameterInvalid
      "consent_responses"
      "Consent responses are corrupted"

guardThatDocumentIsReadableBySignatories :: Kontrakcja m => Document -> m ()
guardThatDocumentIsReadableBySignatories doc = do
  unless (isAccessibleBySignatories doc) . apiError $ documentStateErrorWithCode
    410
    (  "The document has expired or has been withdrawn. (status: "
    <> showt (documentstatus doc)
    <> ")"
    )

-- | Check the session for the `DocumentID` and `SignatoryLinkID`
--
-- If there are no matching sessions then try with `getApiUser APIPersonal`.
guardAccessToDocumentWithSignatory
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
guardAccessToDocumentWithSignatory did slid = do
  sid   <- view #sessionID <$> getContext
  check <- dbQuery $ CheckDocumentSession sid slid
  unless check $ do
    user   <- getAPIUserWithAPIPersonal
    check' <- checkIfUserCanAccessDocumentAsSignatory user did slid
    unless check' $ apiError documentActionForbidden
