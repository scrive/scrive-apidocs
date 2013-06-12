module SMS.SMS (
    SMS(..)
  , scheduleSMS
  , mkSMS
  ) where

import DB
import SMS.Model
import qualified Log
import MessageData
import Doc.DocStateData
import User.Model
import Company.Model
import Util.HasSomeUserInfo
import Data.Char
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad

data SMS = SMS {
    smsMSISDN     :: String -- ^ Number of recipient in international form (+NNXXYYYYYYY)
  , smsData       :: MessageData -- ^ Message body
  , smsBody       :: String -- ^ Message body
  , smsOriginator :: String -- ^ SMS originator/sender name
  } deriving (Eq, Ord, Show)

mkSMS :: MonadDB m => SignatoryLink -> MessageData -> String -> m SMS
mkSMS sl msgData msgBody = do
  mmsgOriginator <- runMaybeT $ do
    uid <- MaybeT $ return $ maybesignatory sl
    user <- MaybeT $ dbQuery $ GetUserByID uid
    cid <- MaybeT $ return $ usercompany user
    company <- MaybeT $ dbQuery $ GetCompany cid
    let name = companyname $ companyinfo company
    guard $ not $ all isSpace name
    return name
  return $ SMS (getMobile sl) msgData msgBody $ fromMaybe "Scrive" mmsgOriginator

-- | Schedule SMS sendout. Note that body/originator needs
-- to be converted to latin1 as sms provider supports latin1 messages only.
-- Transliterate is used since eg. polish characters are not supported by
-- latin1, but we still want messages containing such characters to be sent
-- successfully.
scheduleSMS :: MonadDB m => SMS -> m ()
scheduleSMS msg@SMS{..} = do
    now <- getMinutesTime
    sid <- dbUpdate $ CreateSMS smsOriginator (filter goodChars smsMSISDN) smsBody (show smsData) now
    Log.debug $ "SMS " ++ show msg ++ " with id #" ++ show sid ++ " scheduled for sendout"
    return ()
  where
    goodChars ' ' = False
    goodChars '-' = False
    goodChars _   = True
