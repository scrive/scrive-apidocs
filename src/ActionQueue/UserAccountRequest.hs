module ActionQueue.UserAccountRequest where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Typeable

import ActionQueue.Core
import ActionQueue.Tables
import Company.CompanyID
import Crypto.RNG
import DB
import KontraLink
import MagicHash
import MinutesTime
import Misc
import User.Model

data UserAccountRequest = UserAccountRequest {
    uarEmail :: Email
  , uarExpires :: MinutesTime
  , uarCompanyID :: Maybe CompanyID
  , uarFirstName :: String
  , uarLastName :: String
  , uarToken :: MagicHash
  } deriving (Show, Typeable)

userAccountRequest :: QueueAction Email UserAccountRequest (Email, Maybe CompanyID, String, String, MagicHash) (DBT IO)
userAccountRequest = QueueAction {
    qaTable = tableUserAccountRequests
  , qaFields = \(email, mcid, first_name, last_name, token) -> [
      sql "email" email
    , sql "company_id" mcid
    , sql "first_name" first_name
    , sql "last_name" last_name
    , sql "token" token
    ]
  , qaSelectFields = ["email", "expires", "company_id", "first_name", "last_name", "token"]
  , qaIndexField = "email"
  , qaExpirationDelay = "14 days"
  , qaDecode = foldDB decoder []
  , qaUpdateSQL = \UserAccountRequest{..} -> mkSQL UPDATE tableUserAccountRequests [
      sql "expires" uarExpires
    , sql "company_id" uarCompanyID
    , sql "first_name" uarFirstName
    , sql "last_name" uarLastName
    , sql "token" uarToken
    ] <++> SQL ("WHERE " ++ qaIndexField userAccountRequest ++ " = ?") [toSql uarEmail]
  , qaEvaluateExpired = \UserAccountRequest{uarEmail} -> do
    _ <- dbUpdate $ DeleteAction userAccountRequest uarEmail
    return ()
  }
  where
    decoder acc email expires mcid first_name last_name token = UserAccountRequest {
        uarEmail = email
      , uarExpires = expires
      , uarCompanyID = mcid
      , uarFirstName = first_name
      , uarLastName = last_name
      , uarToken = token
      } : acc

getUserAccountRequest :: MonadDB m => Email -> MagicHash -> m (Maybe UserAccountRequest)
getUserAccountRequest email token = runMaybeT $ do
  Just uar@UserAccountRequest{..} <- dbQuery $ GetAction userAccountRequest email
  guard $ uarToken == token
  return uar

newUserAccountRequest :: (MonadDB m, CryptoRNG m) => Email -> Maybe CompanyID -> String -> String -> m (Maybe UserAccountRequest)
newUserAccountRequest email mcid first_name last_name = runMaybeT $ do
  token <- random
  expires <- minutesAfter (24*60) `liftM` getMinutesTime
  -- FIXME: highly unlikely, but possible race condition
  Nothing <- dbQuery $ GetUserByEmail Nothing email
  dbUpdate $ NewAction userAccountRequest expires (email, mcid, first_name, last_name, token)

newUserAccountRequestLink :: (MonadDB m, CryptoRNG m) => Email -> Maybe CompanyID -> String -> String -> m (Maybe KontraLink)
newUserAccountRequestLink email mcid first_name last_name = runMaybeT $ do
  Just uar <- newUserAccountRequest email mcid first_name last_name
  return $ LinkAccountCreated (uarEmail uar) (uarToken uar)
