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
, guardThatUserIsAuthorOrCompanyAdmin
, guardThatUserIsAuthorOrDocumentIsShared
, guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared
, guardUserMayImpersonateUserGroupForEid
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
, guardDocumentReadAccess
, guardThatDocumentIsReadableBySignatories
, guardAccessToDocumentWithSignatory
) where

import Control.Conditional (whenM)
import Data.Either (rights)
import Log
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import AccessControl.Check
import AccessControl.Types
import API.V2
import API.V2.Errors
import API.V2.Parameters
import API.V2.Utils
import DB
import Doc.API.V2.DocumentAccess
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
import File.FileID
import Folder.Types
import InputValidation
import Kontra
import OAuth.Model (APIPrivilege(..))
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

-- | Guard a given condition on the document, throws an error with
-- `documentStateError` when this does not match.
--
-- Prefer to use a more specific guard if this satisfies your need.
guardThatDocumentIs :: Kontrakcja m => (Document -> Bool) -> Text -> Document -> m ()
guardThatDocumentIs f text doc = unless (f doc) $ apiError $ documentStateError text

-- | Guard that the document status matches, otherwise throw a `documentStateError`
guardDocumentStatus :: Kontrakcja m => DocumentStatus -> Document -> m ()
guardDocumentStatus s doc =
  unless (documentstatus doc == s) $ (apiError . documentStateError $ errorMsg)
  where errorMsg = "The document status should be '" <> (showt s) <> "'."


guardThatDocumentCanBeTrashedByUser :: Kontrakcja m => User -> DocumentID -> m ()
guardThatDocumentCanBeTrashedByUser =
  guardThatDocumentCanBeTrashedOrDeletedByUserWithCond
    (not . isJust . signatorylinkdeleted)
    "The document is in Trash"

guardThatDocumentCanBeDeletedByUser :: Kontrakcja m => User -> DocumentID -> m ()
guardThatDocumentCanBeDeletedByUser =
  guardThatDocumentCanBeTrashedOrDeletedByUserWithCond
    (not . isJust . signatorylinkreallydeleted)
    "Document was purged"

guardThatDocumentCanBeTrashedOrDeletedByUserWithCond
  :: Kontrakcja m => (SignatoryLink -> Bool) -> Text -> User -> DocumentID -> m ()
guardThatDocumentCanBeTrashedOrDeletedByUserWithCond cond errorMsg user did =
  withDocumentID did $ do
    let condAPIError = apiError . documentStateError $ errorMsg
    msl <- getSigLinkFor user <$> theDocument
    case (msl) of -- This might be a user with an account
      (Just sl) -> do
        unless (cond sl) $ condAPIError
      Nothing -> do
        let msgNoAuthor =
              "Document doesn't have author signatory link connected with user account"
        asl <-
          apiGuardJust (serverError msgNoAuthor) =<< (getAuthorSigLink <$> theDocument)
        guardThatUserIsAuthorOrCompanyAdmin user =<< theDocument
        unless (cond asl) $ condAPIError
    guardThatObjectVersionMatchesIfProvided did
    guardThatDocumentIs (not . isPending)
                        "Pending documents can not be trashed or deleted"
      =<< theDocument

-- | Internal function used in guardDocumentAuthorIs
-- Helps code reuse and keep error messages consistent
getAuthor :: Kontrakcja m => Document -> m User
getAuthor doc = do
  let msgNoAuthor =
        "Document doesn't have author signatory link connected with user account"
  authorUserId <-
    apiGuardJust (serverError msgNoAuthor) $ (getAuthorSigLink doc >>= maybesignatory)
  let msgNoUser =
        "Document doesn't have author user account for the author signatory link"
  author <- apiGuardJustM (serverError msgNoUser) $ dbQuery $ GetUserByIDIncludeDeleted
    authorUserId
  return author

-- | Internal function used in all guards on User
-- Helps code reuse and keep error messages consistent
guardDocumentAuthorIs :: Kontrakcja m => (User -> Bool) -> Document -> m ()
guardDocumentAuthorIs condition doc = do
  author <- getAuthor doc
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

guardThatUserIsAuthorOrCompanyAdmin :: Kontrakcja m => User -> Document -> m ()
guardThatUserIsAuthorOrCompanyAdmin user = guardDocumentAuthorIs
  (\a ->
    (user ^. #id == a ^. #id)
      || (user ^. #groupID == a ^. #groupID && user ^. #isCompanyAdmin)
  )

guardThatUserIsAuthorOrDocumentIsShared :: Kontrakcja m => User -> Document -> m ()
guardThatUserIsAuthorOrDocumentIsShared user doc = do
  guardDocumentAuthorIs
    (\a ->
      (user ^. #id == a ^. #id)
        || (user ^. #groupID == a ^. #groupID && isDocumentShared doc)
    )
    doc

guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared
  :: Kontrakcja m => User -> Document -> m ()
guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared user doc =
  guardDocumentAuthorIs (userIsAuthorOrCompanyAdminOrDocumentIsShared user doc) doc

userIsAuthorOrCompanyAdminOrDocumentIsShared :: User -> Document -> User -> Bool
userIsAuthorOrCompanyAdminOrDocumentIsShared user doc author =
  (user ^. #id == author ^. #id)
    || (  (user ^. #groupID == author ^. #groupID)
       && (user ^. #isCompanyAdmin || isDocumentShared doc)
       )

guardThatObjectVersionMatchesIfProvided :: Kontrakcja m => DocumentID -> m ()
guardThatObjectVersionMatchesIfProvided did = do
  reqObjectVersion <- apiV2ParameterOptional (ApiV2ParameterInt "object_version")
  case reqObjectVersion of
    Nothing -> return ()
    Just ov ->
      (dbQuery $ CheckDocumentObjectVersionIs did (fromIntegral ov))
        `catchDBExtraException` (\e@DocumentObjectVersionDoesNotMatch{} ->
                                  apiError $ documentObjectVersionMismatch e
                                )

guardThatAttachmentDetailsAreConsistent :: Kontrakcja m => [AttachmentDetails] -> m ()
guardThatAttachmentDetailsAreConsistent ads = do
  guardUnique $ rights $ map aadFileOrFileParam ads
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
  apiAccessControl user [ canDo act $ DocumentInFolderR fid | (act, fid) <- acts_fids ]
    $ return ()

guardDocumentCreateInFolderIsAllowed :: Kontrakcja m => User -> FolderID -> m ()
guardDocumentCreateInFolderIsAllowed user location =
  guardFolderActionIsAllowed user [(CreateA, location)]

guardDocumentMoveIsAllowed :: Kontrakcja m => User -> FolderID -> FolderID -> m ()
guardDocumentMoveIsAllowed user oldLocation newLocation =
  when (oldLocation /= newLocation)
    $ guardFolderActionIsAllowed user [(CreateA, oldLocation), (DeleteA, oldLocation)]

-- | Make sure the given user (the document author) is allowed to use the
-- display name and service id of the given user group.
guardUserMayImpersonateUserGroupForEid :: Kontrakcja m => User -> Document -> m ()
guardUserMayImpersonateUserGroupForEid user doc
  | Just ugid <- documentusergroupforeid doc = do
    let policy = [canDo ReadA $ EidIdentityR ugid]
    apiAccessControl user policy $ return ()
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
      Nothing ->
        unless (isValidSSNForAuthenticationToView authType $ getPersonalNumber sl)
          $ (  apiStateError
            $  "Signatory does not have a valid personal number "
            <> "for the authentication method and you did not provide one"
            )

      Just ssn ->
        unless (isValidSSNForAuthenticationToView authType ssn)
          $ (  apiStateError
            $  "The personal number you provided is not valid "
            <> "for the authentication method"
            )
    -- Check if either a valid mobile for authToView is set or is provided
    case mMobile of
      Nothing ->
        unless (isValidMobileForAuthenticationToView authType $ getMobile sl)
          $ (  apiStateError
            $  "Party does not have a valid mobile number set "
            <> "for the authentication method and you did not provide one"
            )

      Just mobile ->
        unless (isValidMobileForAuthenticationToView authType mobile)
          $ (  apiStateError
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
      isGood . asValidFinnishSSN $ ssn
    isValidSSNForAuthenticationToView VerimiAuthenticationToView _ = True
    isValidSSNForAuthenticationToView IDINAuthenticationToView   _ = True
    isValidMobileForAuthenticationToView :: AuthenticationToViewMethod -> Text -> Bool
    isValidMobileForAuthenticationToView StandardAuthenticationToView _ = True
    isValidMobileForAuthenticationToView SMSPinAuthenticationToView mobile =
      isGood . asValidPhoneForSMS $ mobile
    isValidMobileForAuthenticationToView SEBankIDAuthenticationToView _ = True
    isValidMobileForAuthenticationToView DKNemIDAuthenticationToView  _ = True
    isValidMobileForAuthenticationToView NOBankIDAuthenticationToView mobile =
      (isGood || isEmpty) $ asValidPhoneForNorwegianBankID mobile
    isValidMobileForAuthenticationToView FITupasAuthenticationToView _ = True
    isValidMobileForAuthenticationToView VerimiAuthenticationToView  _ = True
    isValidMobileForAuthenticationToView IDINAuthenticationToView    _ = True

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

    -- Note that we have mSSN = Just ssn iff authToSignNeedsPersonalNumber
    -- authToSign = True.
    whenJust mSSN $ \ssn -> do
      when (signatorylinkidentifiedtoview sl && ssn /= getPersonalNumber sl)
        . apiError
        $ signatoryStateError
            "The party has authenticated to view, therefore you can't change the authentication value"

      let validate = case authToSign of
            SEBankIDAuthenticationToSign -> asValidSEBankIdPersonalNumber
            DKNemIDAuthenticationToSign -> asValidDanishSSN
            _ -> Good

      -- Empty is allowed only if we don't need it for
      -- AuthenticationToViewMethod
      case validate ssn of
        Empty -> do
          when (authToViewNeedsPersonalNumber authToView)
            . apiError
            $ signatoryStateError
                "You provided an empty authentication value, needs a value for authentication to view"
        Bad -> do
          let name = case authToSign of
                SEBankIDAuthenticationToSign -> "Swedish BankID"
                DKNemIDAuthenticationToSign -> "Danish NemID"
                _ -> ""  -- never happens
          apiError
            .  signatoryStateError
            $  "The authentication value provided is not a valid for "
            <> name
        Good _ -> return ()


    -- Note that we have mMobile = Just mobile iff authToSignNeedsMobileNumer
    -- authToSign = True.
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
          $ apiError
          $ signatoryStateError
              "The party has authenticated to view with Norwegian BankID, therefore you can't change the mobile number"
        -- If given a mobile number we need to make sure it doesn't invalidate
        -- NOBankIDAuthenticationToView
        when (signatorylinkauthenticationtoviewmethod sl == NOBankIDAuthenticationToView)
          $ case asValidPhoneForNorwegianBankID mobile of
              Bad ->
                apiError
                  $ signatoryStateError
                      "Mobile number needs to be a valid Norwegian number as Norwegian BankID is set as authentication to view"
              Empty  -> return ()
              Good _ -> return ()
      _ -> return ()


guardAuthenticationMethodsCanMix
  :: Kontrakcja m
  => AuthenticationToViewMethod
  -> AuthenticationToSignMethod
  -> AuthenticationToViewMethod
  -> m ()
guardAuthenticationMethodsCanMix authToView authToSign authToViewArchived = do
  unless
    (authenticationMethodsCanMix authToView authToSign authToViewArchived)
    (apiError $ signatoryStateError $ mconcat
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
  when (maybe True ((/=) roleExpected) roleActual)
    $ (  apiError
      .  signatoryStateError
      .  T.pack
      $  "Wrong signatory role, expected '"
      ++ show roleExpected
      ++ "', but got '"
      ++ showRoleActual
      ++ "'"
      )

guardApproverHasNotApproved :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardApproverHasNotApproved slid doc =
  when (isApproverAndHasApproved $ getSigLinkFor slid doc)
    $ (apiError . signatoryStateError $ "The approver has already approved")

guardSignatoryHasNotSigned :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSignatoryHasNotSigned slid doc =
  when (isSignatoryAndHasSigned $ getSigLinkFor slid doc)
    $ (apiError . signatoryStateError $ "The signatory has already signed")

guardSigningPartyHasNeitherSignedNorApproved
  :: Kontrakcja m => SignatoryLinkID -> Document -> m ()
guardSigningPartyHasNeitherSignedNorApproved slid doc =
  when ((isApproverAndHasApproved || isSignatoryAndHasSigned) $ getSigLinkFor slid doc)
    $ ( apiError
      . signatoryStateError
      $ "The signing party has either signed or approved already"
      )

guardThatDocumentHasntBeenForwadedTooManyTimes :: Kontrakcja m => Document -> m ()
guardThatDocumentHasntBeenForwadedTooManyTimes doc =
  when (length (filter isForwarded $ documentsignatorylinks doc) > 100)
    $ ( apiError
      . documentStateError
      $ "This document has been forwarded too many times already"
      )

-- Checks if document can be started. Throws matching API exception if it does not
guardThatDocumentCanBeStarted :: Kontrakcja m => Document -> m ()
guardThatDocumentCanBeStarted = maybe (return ()) apiError . documentCanBeStarted

documentCanBeStarted :: Document -> Maybe APIError
documentCanBeStarted doc = either Just (const Nothing) $ do
  when (isTemplate doc) $ do
    Left $ (documentStateError "Document is a template, templates can not be started")
  unless (all signatoryHasValidDeliverySettings $ documentsignatorylinks doc) $ do
    Left
      $ documentStateError
          "Some parties have an invalid email address or mobile number, their invitation 'delivery_method' requires it to be valid and not empty."
  unless (all signatoryHasValidConfirmationSettings $ documentsignatorylinks doc) $ do
    Left
      $ documentStateError
          "Some parties have an invalid email address or mobile number, their 'confirmation_delivery_method' requires it to be valid or empty."
  unless (all signatoryHasValidNotificationSettings $ documentsignatorylinks doc) $ do
    Left
      $ documentStateError
          "Some parties have an invalid email address or mobile number, their 'notification_delivery_method' requires it to be valid or empty."
  unless (all signatoryHasValidSSNOrEmailForIdentifyToView $ documentsignatorylinks doc)
    $ do
        Left
          $ documentStateError
              "Some parties have an invalid personal numbers/emails, their 'authentication_to_view' requires it to be valid and not empty."
  unless (all signatoryHasValidAuthSettings $ documentsignatorylinks doc) $ do
    Left
      $ documentStateError
          "Some parties have an invalid personal/mobile numbers, their 'authentication_to_sign' requires it to be valid or empty."
  unless
      ( all
          (signatoryHasValidMobileForIdentifyToView
            signatorylinkauthenticationtoviewmethod
          )
      $ documentsignatorylinks doc
      )
    $ do
        Left
          $ documentStateError
              "Some parties have an invalid mobile number and it is required for identification to view document."
  unless
      ( all
          (signatoryHasValidMobileForIdentifyToView
            signatorylinkauthenticationtoviewarchivedmethod
          )
      $ documentsignatorylinks doc
      )
    $ do
        Left
          $ documentStateError
              "Some parties have an invalid mobile number and it is required for identification to view archived document."
  when (any (isAuthor && isApprover) $ documentsignatorylinks doc) $ do
    Left $ documentStateError "Author can't be an approver"
  unless (all signatoryThatIsApproverHasStandardAuthToSign $ documentsignatorylinks doc)
    $ do
        Left $ documentStateError
          "All approvers have to have standard authentication to sign"
  unless (all signatoryThatIsApproverHasNoPlacements $ documentsignatorylinks doc) $ do
    Left $ documentStateError "No approver can have placed fields"
  unless (any isSignatory $ documentsignatorylinks doc) $ do
    Left $ documentStateError "Document has to have at least one signing party"
  when (isNothing $ documentfile doc) $ do
    Left $ documentStateError "Document must have a file before it can be started"
  return ()

  where
    signatoryHasValidDeliverySettings sl =
      (isAuthor sl) || case (signatorylinkdeliverymethod sl) of
        EmailDelivery  -> isValidEmail $ getEmail sl
        MobileDelivery -> isValidPhoneForSMS $ getMobile sl
        EmailAndMobileDelivery ->
          (isValidPhoneForSMS $ getMobile sl) && (isValidEmail $ getEmail sl)
        PortalDelivery -> isValidEmail $ getEmail sl
        _              -> True

    signatoryHasValidConfirmationSettings sl =
      isAuthor sl || case signatorylinkconfirmationdeliverymethod sl of
        EmailConfirmationDelivery              -> isEmailValidOrEmpty sl
        EmailLinkConfirmationDelivery          -> isEmailValidOrEmpty sl
        MobileConfirmationDelivery             -> isMobileValidOrEmpty sl
        EmailAndMobileConfirmationDelivery     -> isEmailAndMobileValidOrEmpty sl
        EmailLinkAndMobileConfirmationDelivery -> isEmailAndMobileValidOrEmpty sl
        NoConfirmationDelivery                 -> True

    signatoryHasValidNotificationSettings sl =
      isAuthor sl || case signatorylinknotificationdeliverymethod sl of
        NoNotificationDelivery             -> True
        EmailNotificationDelivery          -> isEmailValidOrEmpty sl
        MobileNotificationDelivery         -> isMobileValidOrEmpty sl
        EmailAndMobileNotificationDelivery -> isEmailAndMobileValidOrEmpty sl

    signatoryHasValidAuthSettings sl = authToSignIsValid sl && authToViewIsValid sl

    authToViewIsValid sl = case signatorylinkauthenticationtoviewmethod sl of
      SEBankIDAuthenticationToView -> True
      NOBankIDAuthenticationToView ->
        isJust (getFieldByIdentity PersonalNumberFI $ signatoryfields sl)
      DKNemIDAuthenticationToView  -> True
      SMSPinAuthenticationToView   -> True
      StandardAuthenticationToView -> True
      IDINAuthenticationToView     -> True
      FITupasAuthenticationToView  -> True
      VerimiAuthenticationToView   -> True

    authToSignIsValid sl = case signatorylinkauthenticationtosignmethod sl of
      SEBankIDAuthenticationToSign ->
        isJust (getFieldByIdentity PersonalNumberFI $ signatoryfields sl)
          && (  T.null (getPersonalNumber sl)
             || (isGood $ asValidSEBankIdPersonalNumber $ getPersonalNumber sl)
             )
      NOBankIDAuthenticationToSign ->
        T.null (getPersonalNumber sl)
          || (isGood $ asValidNOBankIdPersonalNumber $ getPersonalNumber sl)
      -- How does `T.null (getPersonalNumber sl)` square with
      -- `authToSignNeedsPersonalNumber DKNemIDAuthenticationToSign = True`?
      DKNemIDAuthenticationToSign ->
        T.null (getPersonalNumber sl)
          || (isGood $ asValidDanishSSN $ getPersonalNumber sl)
      SMSPinAuthenticationToSign ->
        isJust (getFieldByIdentity MobileFI $ signatoryfields sl)
          && (T.null (getMobile sl) || isGood (asValidPhoneForSMS $ getMobile sl))
      StandardAuthenticationToSign -> True
      IDINAuthenticationToSign     -> True
      FITupasAuthenticationToSign ->
        T.null (getPersonalNumber sl) || isGood (asValidFinnishSSN $ getPersonalNumber sl)

    signatoryHasValidSSNOrEmailForIdentifyToView sl =
      case (signatorylinkauthenticationtoviewmethod sl) of
        SEBankIDAuthenticationToView -> isGood $ asValidSwedishSSN $ getPersonalNumber sl
        NOBankIDAuthenticationToView ->
          isGood $ asValidNorwegianSSN $ getPersonalNumber sl
        DKNemIDAuthenticationToView -> isGood $ asValidDanishSSN $ getPersonalNumber sl
        FITupasAuthenticationToView ->
          T.null (getPersonalNumber sl)
            || (isGood $ asValidFinnishSSN $ getPersonalNumber sl)
        SMSPinAuthenticationToView   -> True
        StandardAuthenticationToView -> True
        VerimiAuthenticationToView   -> isValidEmail $ getEmail sl
        IDINAuthenticationToView     -> isValidEmail $ getEmail sl

    signatoryHasValidMobileForIdentifyToView viewmethod sl = case viewmethod sl of
      NOBankIDAuthenticationToView ->
        (isGood $ asValidPhoneForNorwegianBankID (getMobile sl))
          || (isEmpty $ asValidPhoneForNorwegianBankID (getMobile sl))
      SMSPinAuthenticationToView -> isGood $ asValidPhoneForSMS (getMobile sl)
      _ -> True

    signatoryThatIsApproverHasStandardAuthToSign sl =
      (not $ isApprover sl)
        || (signatorylinkauthenticationtosignmethod sl == StandardAuthenticationToSign)

    signatoryThatIsApproverHasNoPlacements sl =
      (not $ isApprover sl) || (null $ concat $ fieldPlacements <$> signatoryfields sl)

    isEmailValidOrEmpty sl = T.null (getEmail sl) || isValidEmail (getEmail sl)
    isMobileValidOrEmpty sl = T.null (getMobile sl) || isValidPhoneForSMS (getMobile sl)
    isEmailAndMobileValidOrEmpty sl = isEmailValidOrEmpty sl && isMobileValidOrEmpty sl

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
    unless (all radioValIsValid signfields) $ apiError $ signatoryStateError
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
          || length (ssfPlacements ssf)
          == 0
          || BS.length contents
          >  0
      signatureIsFilled _ = True -- non signature fields are skipped
  unless (all signatureIsFilled signfields) $ apiError $ signatoryStateError
    "Signature missing."

guardThatAllAttachmentsAreAcceptedOrIsAuthor
  :: Kontrakcja m => SignatoryLinkID -> [FileID] -> Document -> m ()
guardThatAllAttachmentsAreAcceptedOrIsAuthor slid acceptedAttachments doc = do
  unless (allRequiredAttachmentsAreOnList acceptedAttachments doc)
    $ unless (isAuthor $ fromJust $ getSigLinkFor slid doc)
    $ -- Author does not need to accept attachments
      apiError
    $ (signatoryStateError "Some mandatory author attachments aren't accepted")

guardThatAllSignatoryAttachmentsAreUploadedOrMarked
  :: Kontrakcja m => SignatoryLinkID -> [Text] -> Document -> m ()
guardThatAllSignatoryAttachmentsAreUploadedOrMarked slid notUploadedSignatoryAttachments doc
  = do
    let
      sigAttachments = signatoryattachments $ fromJust $ getSigLinkFor slid doc
      requiredSigAttachments = filter signatoryattachmentrequired sigAttachments
      optionalSigAttachments = filter (not . signatoryattachmentrequired) sigAttachments
      optionalSigAttachmentsNames = map signatoryattachmentname optionalSigAttachments
    -- all not uploaded signatory attachment names must exist
    when (any (`notElem` optionalSigAttachmentsNames) notUploadedSignatoryAttachments)
      $ apiError
      $ signatoryStateError "Optional signatory attachment name does not exist"
    -- all required signatory attachments must be uploaded
    when (any (isNothing . signatoryattachmentfile) requiredSigAttachments)
      $ apiError
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
      $ apiError
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
      $ apiError
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
guardThatAuthorIsNotApprover doc =
  if any (isAuthor && isApprover) (documentsignatorylinks doc)
    then apiError $ requestParameterInvalid "document" "Author can't be an approver"
    else return ()

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
    unless (all (`elem` responseIDs) questionIDs) $ apiError $ requestParameterInvalid
      "consent_responses"
      "Some consent questions have not been answered"
    unless (all (`elem` questionIDs) responseIDs) $ apiError $ requestParameterInvalid
      "consent_responses"
      "Consent responses are corrupted"

-- | For the given DocumentID:
--
-- 1. Try to get a valid session for the given `Maybe SignatoryLinkID`
--
-- if that fails or no SignatoryLinkID is given, then:
--
-- Get permissions using `getAPIUser` with given privileges.
-- If the user account is not linked to the document then also guard extra
-- permissions using guardThatUserIsAuthorOrCompanyAdminOrDocumentIsShared
--
-- SYSTEM ADMINS WILL ALWAYS SUCCEED
--
-- This is useful in all situations where a signatory or other users could use
-- the API call (e.g. document GET call), but not that this function is focused only
-- on ability to read document

guardDocumentReadAccess
  :: Kontrakcja m => Maybe SignatoryLinkID -> Document -> m DocumentAccess
guardDocumentReadAccess mslid doc = do
  mSessionSignatory <- case mslid of
    Nothing   -> return Nothing
    Just slid -> do
      sid          <- view #sessionID <$> getContext
      validSession <- dbQuery $ CheckDocumentSession sid slid
      if validSession
        then do
          fmap Just . apiGuardJust (documentNotFound (documentid doc)) $ getSigLinkFor
            slid
            doc
        else return Nothing

  case mSessionSignatory of
    Just sl -> do
      unless (signatoryisauthor sl) $ guardThatDocumentIsReadableBySignatories doc
      return $ documentAccessForSlid (signatorylinkid sl) doc
    Nothing -> do
      (user, _) <- getAPIUser APIDocCheck
      case getSigLinkFor user doc of
        Just sl -> do
          unless (signatoryisauthor sl) $ guardThatDocumentIsReadableBySignatories doc
          return $ documentAccessForUser user doc
        Nothing -> do
          admin  <- isUserAdmin user <$> getContext
          author <- getAuthor doc
          case () of
            _
              | userIsAuthorOrCompanyAdminOrDocumentIsShared user doc author
              -> return $ documentAccessForUser user doc
              | admin && False -- disable temporarily
              -> do
                logInfo "GOD DOCUMENT ACCESS" $ object ["god" .= show (user ^. #id)]
                return $ documentAccessForAdminonly doc
              | otherwise
              -> apiError documentActionForbidden

guardThatDocumentIsReadableBySignatories :: Kontrakcja m => Document -> m ()
guardThatDocumentIsReadableBySignatories doc = do
  unless (isAccessibleBySignatories doc)
    $  apiError
    $  documentStateErrorWithCode 410
    $  "The document has expired or has been withdrawn. (status: "
    <> showt (documentstatus doc)
    <> ")"

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
