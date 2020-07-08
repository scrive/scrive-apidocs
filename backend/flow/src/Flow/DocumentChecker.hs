module Flow.DocumentChecker
  ( FlowUser(..)
  , FlowUserDocRole
  , MatchingResult(..)
  , Signatory(..)
  , SignatoryConsistencyError(..)
  , SignatoryDocRole
  , UserMatch(..)
  , aggregateSignatories
  , documentSignatories
  , docRolesMatch
  , flowUserMatchesSignatory
  , groupFlowUsers
  , matchUsers
  )
where

import Data.Bifunctor
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

import Doc.DocumentID
import Doc.SignatoryLinkID
import Doc.Types.Document
import Doc.Types.SignatoryField
import Doc.Types.SignatoryLink
import Flow.VariableCollector
import User.UserID

-- Every user can only play one role in a document.
-- We should forbid Flow processes that require multiple roles.

-- | This information should be specified in the template parameters.
-- Since documents allow considerable freedom in identifying
-- signing parties, we should support that as well.
data FlowUser
  = Email Text
  | PhoneNumber Text
  | UserID UserID
  deriving (Eq, Ord, Show)
type FlowUserDocRole = DocRoleFor FlowUser DocumentID

-- | This is a simplified version of the `SignatoryLink`.
-- We only keep the fields we need in Flow.
data Signatory = Signatory
  { id          :: SignatoryLinkID
  , email       :: Maybe Text
  , phoneNumber :: Maybe Text
  , userId      :: Maybe UserID
  } deriving (Eq, Ord, Show)
type SignatoryDocRole = DocRoleFor Signatory DocumentID

-- | Association between a Flow user, as specified
-- by a Flow process, and a signatory, as specified
-- in a document.
data UserMatch = UserMatch
  { flow      :: FlowUser
  , signatory :: Signatory
  } deriving (Eq, Ord, Show)

-- | Result of matching Flow users with the signatories
-- in the documents associated with that Flow.
data MatchingResult = MatchingResult
  { matched              :: Set UserMatch
  , unmatchedFlowUsers   :: Set FlowUserDocRole
  , unmatchedSignatories :: Set SignatoryDocRole
  } deriving (Eq, Show)

-- | Extract user document roles from a Flow process.
-- signatoryRequirements :: HighTongue -> Set FlowUserDocRole

-- Forwarded roles only occur in a running process,
-- so do not concern us here when checking documents for starting.
signatoryLinkRoleToSignatoryRole :: SignatoryRole -> Role
signatoryLinkRoleToSignatoryRole = \case
  SignatoryRoleViewer                -> Viewer
  SignatoryRoleSigningParty          -> SigningParty
  SignatoryRoleApprover              -> Approver
  SignatoryRoleForwardedSigningParty -> SigningParty
  SignatoryRoleForwardedApprover     -> Approver

-- | Extract signatory document roles from a document.
documentSignatories :: Document -> Set SignatoryDocRole
documentSignatories doc =
  Set.fromList . map (sigDocRole . signatoryInfo) $ documentsignatorylinks doc
  where sigDocRole (sig, role) = DocRoleFor role sig (documentid doc)

-- | Convert document signatory to a signatory document role.
signatoryInfo :: SignatoryLink -> (Signatory, Role)
signatoryInfo sigLink = (Signatory { .. }, role)
  where
    id          = signatorylinkid sigLink
    fields      = signatoryfields sigLink
    -- Text.strip is here because it's in SignatoryField.getEmail,
    -- suggesting that emails might be unsanitized in the DB.
    email       = getFieldByIdentity EmailFI fields >>= fieldTextValue <&> Text.strip
    phoneNumber = getFieldByIdentity MobileFI fields >>= fieldTextValue
    userId      = maybesignatory sigLink
    role        = signatoryLinkRoleToSignatoryRole $ signatoryrole sigLink

-- | Tries to match the flow users described in a Flow process
-- with the signatories in the associated documents.
-- It returns both the matching pairs, as well as all the unmatched
-- users on both the Flow side and the Document side.
--
-- Note: the implementation uses a naive quadratic algorithm that should be fine
-- until we have hundreds of users signing dozens of documents in a single process.
matchUsers :: Set FlowUserDocRole -> Set SignatoryDocRole -> MatchingResult
matchUsers fudrs sdrs = MatchingResult { .. }
  where
    matched              = Set.fromList $ map toUserMatch matchedDocRoles
    toUserMatch          = uncurry UserMatch . bimap user user
    unmatchedFlowUsers   = Set.difference fudrs . Set.fromList $ map fst matchedDocRoles
    unmatchedSignatories = Set.difference sdrs . Set.fromList $ map snd matchedDocRoles
    matchedDocRoles      = mapMaybe matchUser $ Set.toList fudrs

    matchUser :: FlowUserDocRole -> Maybe (FlowUserDocRole, SignatoryDocRole)
    matchUser fudr = (fudr, ) <$> find (docRolesMatch fudr) (Set.toList sdrs)

docRolesMatch :: FlowUserDocRole -> SignatoryDocRole -> Bool
docRolesMatch fudr sdr =
  flowUserMatchesSignatory (user fudr) (user sdr)
    && (document fudr == document sdr)
    && (role fudr == role sdr)

flowUserMatchesSignatory :: FlowUser -> Signatory -> Bool
flowUserMatchesSignatory flowUser Signatory {..} = case flowUser of
  Email       email'       -> Just email' == email
  PhoneNumber phoneNumber' -> Just phoneNumber' == phoneNumber
  UserID      userId'      -> Just userId' == userId

data SignatoryConsistencyError
  = InconsistentEmail (Set Text)
  | InconsistentPhoneNumber (Set Text)
  | InconsistentUserId (Set UserID)
  | NoSignatories

groupFlowUsers :: Set UserMatch -> [(FlowUser, Set Signatory)]
groupFlowUsers =
  map toOutput . groupBy (\x y -> flow x == flow y) . sortOn flow . Set.toList
  where toOutput l = (flow $ head l, Set.fromList $ map signatory l)

-- | This function verifies that the signatory contact/user data
-- is consistent across several records, i.e. no kind of contact/user datum
-- (email, say) has multiple values. If this is the case, it creates
-- an aggregate signatory record.
--
-- The purpose is to check that signatory records coming from several
-- documents (such as those associated with a Flow) are consistent
-- with each other.
aggregateSignatories :: Set Signatory -> Either SignatoryConsistencyError Signatory
aggregateSignatories sigs
  | Set.null sigs = Left NoSignatories
  | otherwise = do
    e   <- tryUnique email InconsistentEmail
    pn  <- tryUnique phoneNumber InconsistentPhoneNumber
    uid <- tryUnique userId InconsistentUserId
    pure $ Signatory (id . head $ Set.toList sigs) e pn uid
  where
    unique :: Ord a => (Signatory -> Maybe a) -> Set a
    unique f = Set.fromList . mapMaybe f $ Set.toList sigs
    tryUnique f mkErr = case Set.toList (unique f) of
      []  -> pure Nothing
      [x] -> pure $ Just x
      _   -> Left . mkErr $ unique f
