module DB.TimeZoneName
  ( TimeZoneName
  , defaultTimeZoneName
  , unsafeTimeZoneName
  , mkTimeZoneName
  , withTimeZone
  , toString
  ) where

import Control.Monad.Catch
import Data.Char
import Data.Functor.Invariant
import Data.Typeable
import Data.Unjson
import Database.PostgreSQL.PQTypes
import qualified Control.Exception.Lifted as E
import qualified Data.Text as T

-- | Time zone names that the database backend accepts.  See also
-- http://en.wikipedia.org/wiki/List_of_tz_database_time_zones
newtype TimeZoneName = TimeZoneName Text
  deriving (Eq, Show, Ord, Typeable)

instance Unjson TimeZoneName where
  unjsonDef = invmap unsafeTimeZoneName toString unjsonDef

defaultTimeZoneName :: TimeZoneName
defaultTimeZoneName = TimeZoneName "Europe/Stockholm"

unsafeTimeZoneName :: Text -> TimeZoneName
unsafeTimeZoneName = TimeZoneName

mkTimeZoneName :: (MonadDB m, MonadCatch m) => Text -> m TimeZoneName
mkTimeZoneName s
  | not (sanityCheck s) = fail $ "mkTimeZoneName: illegal time zone string: " <> show s
  | otherwise = do
    -- Check if we can use the string to form a valid SQL 'timestamp with time zone' value.
    runSQL_ ("SELECT cast(" <?> ("2000-01-01 " <> s) <+> "as timestamp with time zone)")
      `catch` \(E.SomeException e) -> do
        fail $ "mkTimeZoneName: time zone not recognized by database: " <> show (s, e)
    return $ TimeZoneName s

sanityCheck :: Text -> Bool
sanityCheck s = case break (=='/') (T.unpack s) of
  (as@(_:_),'/':bs) -> all isAlpha as && all (\b -> isAlphaNum b || isAllowedChar b) bs
  _                 -> all (\b -> isAlphaNum b || isAllowedChar b) (T.unpack s)
  where
    isAllowedChar :: Char -> Bool
    isAllowedChar = (`elem` ("/+-_"::String))

withTimeZone :: forall m a. (MonadDB m, MonadMask m)
             => TimeZoneName -> m a -> m a
withTimeZone (TimeZoneName tz) = bracket setNewTz setTz . const
  where
    setNewTz = do
      runSQL_ "SHOW timezone"
      oldtz <- fetchOne runIdentity
      setTz tz
      return oldtz

    -- tz' was checked before in mkTimeZoneName/fromSQL, so unsafeSQL is safe.
    setTz :: Text -> m ()
    setTz tz' = runSQL_ $ "SET timezone =" <+> unsafeSQL (T.unpack $ "'" <> tz' <> "'")

toString :: TimeZoneName -> Text
toString (TimeZoneName s) = s

----------------------------------------

instance PQFormat TimeZoneName where
  pqFormat = pqFormat @String

instance FromSQL TimeZoneName where
  type PQBase TimeZoneName = PQBase String
  fromSQL mbase = do
    s <- fromSQL mbase
    if sanityCheck s
      then return $ TimeZoneName s
      else hpqTypesError $ T.unpack $ "fromSQL (TimeZoneName): sanity check on value '" <> s <> "' failed"

instance ToSQL TimeZoneName where
  type PQDest TimeZoneName = PQDest String
  toSQL (TimeZoneName s) = toSQL s
