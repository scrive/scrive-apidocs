module Auth.OAuth where

import Data.Int
import Misc

-- | AccessToken for OAuth2 API calls
newtype AccessToken = AccessToken { unAccessToken :: Int64 }
                    deriving (Eq, Ord)

-- | APIToken for API calls
newtype APIToken = APIToken { unAPIToken :: Int64 }
                    deriving (Eq, Ord)

-- | APISecret (like the password) for API calls
newtype APISecret = APISecret { unAPISecret :: Int64 }
                    deriving (Eq, Ord)

-- | APITokens have a status
data APITokenStatus = APITokenActive   -- | The API Token can be used to get an AccessToken
                    | APITokenDisabled -- | The API Token cannot be used to get an AccessToken

instance SafeEnum APITokenStatus where
  fromSafeEnum APITokenActive  = 1
  fromSafeEnum APITokenDisabled = 0
  
  toSafeEnum 1 = Just APITokenActive
  toSafeEnum 0 = Just APITokenDisabled
  toSafeEnum _ = Nothing
