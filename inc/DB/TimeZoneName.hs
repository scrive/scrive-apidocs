{-# LANGUAGE OverloadedStrings #-}
module DB.TimeZoneName
  ( TimeZoneName
  , mkTimeZoneName
  , withTimeZone
  , toString
  ) where

import Data.Char (isAlpha, isAlphaNum)
import qualified Control.Exception.Lifted as E
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Convertible (Convertible (..))
import Data.String (fromString)
import Data.Typeable (Typeable)
import Database.HDBC (SqlValue)

import DB.Core (MonadDB)
import DB.Env (DBEnv)
import DB.Functions (kRun)
import DB.SQL ((<?>), (<+>))
import DB.Utils (getOne, exactlyOneObjectReturnedGuard)


-- | Time zone names that the database backend accepts.  See also
-- http://en.wikipedia.org/wiki/List_of_tz_database_time_zones
newtype TimeZoneName = TimeZoneName String
  deriving (Eq, Show, Ord, Typeable)

mkTimeZoneName :: (MonadBaseControl IO m, MonadDB m) => String -> DBEnv m TimeZoneName
mkTimeZoneName s | not (sanityCheck s) = fail $ "mkTimeZoneName: illegal time zone string: " ++ show s
                 | otherwise = do
  -- Check if we can use the string to form a valid SQL 'timestamp with time zone' value.
  _ <- (kRun $
        "SELECT cast(" <?> ("2000-01-01 " ++ s) <+> "as timestamp with time zone)")
       `E.catch` \(E.SomeException e) -> do
         fail $ "mkTimeZoneName: time zone not recognized by database: " ++ show (s,e)
  return $ TimeZoneName s

sanityCheck :: String -> Bool
sanityCheck s = case break (=='/') s of
          (as@(_:_),'/':bs) -> all isAlpha as
                          && all (\b -> isAlphaNum b || b `elem` "+-") bs
          _           -> False

withTimeZone :: (MonadDB m, MonadBaseControl IO m)
             => TimeZoneName -> DBEnv m a -> DBEnv m a
withTimeZone (TimeZoneName tz) m = E.bracket
  (do stz :: String <- getOne "SHOW timezone" >>= exactlyOneObjectReturnedGuard
      setTz tz
      return stz)
  setTz
  (const m)
  where setTz tz' = kRun ("SET timezone =" <+> tzq) >> return ()
           where tzq = fromString ("'" ++ tz' ++ "'") -- tz' is sanity checked

toString :: TimeZoneName -> String
toString (TimeZoneName s) = s

instance Convertible TimeZoneName SqlValue where
  safeConvert = safeConvert . toString

instance Convertible SqlValue TimeZoneName where
  safeConvert s = do
    st <- safeConvert s
    if sanityCheck st then return (TimeZoneName st)
                     else fail "TimeZoneName.safeConvert"
