module Doc.Types.SignatoryAccessToken
  ( SignatoryAccessTokenReason(..)
  , SignatoryAccessToken(..)
  , signatoryAccessTokenSelectors
  , isValidSignatoryAccessToken
  ) where

import Control.Monad.Catch
import Data.Int
import Data.Time

import DB
import Doc.Tables
import Doc.Types.DocumentStatus
import MagicHash

-- | Reason why the access token was generated. This gives context
-- so we can invalidate it when necessary.
data SignatoryAccessTokenReason
  = SignatoryAccessTokenLegacy
  -- ^ The legacy tokens were originally called @TemporarySignatoryMagicHash@
  -- which were time-restricted magic hashes.
  -- It was an initial approach leading up to the current solution, where the
  -- magic hashes have a well defined "reason" for existing.
  | SignatoryAccessTokenForMailBeforeClosing
  | SignatoryAccessTokenForMailAfterClosing
  | SignatoryAccessTokenForSMSBeforeClosing
  | SignatoryAccessTokenForSMSAfterClosing
  | SignatoryAccessTokenForQRCode
  | SignatoryAccessTokenForAPI
  deriving (Eq, Show)

instance PQFormat SignatoryAccessTokenReason where
  pqFormat = pqFormat @Int16

instance ToSQL SignatoryAccessTokenReason where
  type PQDest SignatoryAccessTokenReason = PQDest Int16
  toSQL = toSQL @Int16 . \case
    SignatoryAccessTokenLegacy               -> 1
    SignatoryAccessTokenForMailBeforeClosing -> 2
    SignatoryAccessTokenForMailAfterClosing  -> 3
    SignatoryAccessTokenForSMSBeforeClosing  -> 4
    SignatoryAccessTokenForSMSAfterClosing   -> 5
    SignatoryAccessTokenForQRCode            -> 6
    SignatoryAccessTokenForAPI               -> 7

instance FromSQL SignatoryAccessTokenReason where
  type PQBase SignatoryAccessTokenReason = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return SignatoryAccessTokenLegacy
      2 -> return SignatoryAccessTokenForMailBeforeClosing
      3 -> return SignatoryAccessTokenForMailAfterClosing
      4 -> return SignatoryAccessTokenForSMSBeforeClosing
      5 -> return SignatoryAccessTokenForSMSAfterClosing
      6 -> return SignatoryAccessTokenForQRCode
      7 -> return SignatoryAccessTokenForAPI
      _ -> throwM RangeError { reRange = [(1, 7)], reValue = n }

---------------------------------

signatoryAccessTokenSelectors :: [SQL]
signatoryAccessTokenSelectors =
  [ "signatory_access_tokens.hash"
  , "signatory_access_tokens.reason"
  , "signatory_access_tokens.expiration_time"
  ]

data SignatoryAccessToken = SignatoryAccessToken
  { signatoryAccessTokenHash           :: MagicHash
  , signatoryAccessTokenReason         :: SignatoryAccessTokenReason
  , signatoryAccessTokenExpirationTime :: Maybe UTCTime
  } deriving (Eq, Show)

instance PQFormat SignatoryAccessToken where
  pqFormat = compositeTypePqFormat ctSignatoryAccessToken

type instance CompositeRow SignatoryAccessToken
  = (MagicHash, SignatoryAccessTokenReason, Maybe UTCTime)

instance CompositeFromSQL SignatoryAccessToken where
  toComposite (hash, reason, time) = SignatoryAccessToken hash reason time

isValidSignatoryAccessToken :: UTCTime -> DocumentStatus -> SignatoryAccessToken -> Bool
isValidSignatoryAccessToken now status SignatoryAccessToken {..} =
  let expired = case signatoryAccessTokenExpirationTime of
        Just v -> v <= now
        _      -> False
      closedOrPending       = status == Pending || status == Closed
      reasonIsStillRelevant = case signatoryAccessTokenReason of
        SignatoryAccessTokenForMailBeforeClosing -> closedOrPending
        SignatoryAccessTokenForSMSBeforeClosing  -> closedOrPending
        SignatoryAccessTokenForMailAfterClosing  -> status == Closed
        SignatoryAccessTokenForSMSAfterClosing   -> status == Closed
        SignatoryAccessTokenLegacy               -> True
        SignatoryAccessTokenForQRCode            -> True
        SignatoryAccessTokenForAPI               -> True
  in  reasonIsStillRelevant && not expired
