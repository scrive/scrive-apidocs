module DB.TimeZoneName
  ( TimeZoneName
  , mkTimeZoneName
  , withTimeZone
  , toString
  ) where


import Data.Char
import Data.Typeable
import Control.Monad.Trans.Control
import Data.Monoid.Space
import Database.PostgreSQL.PQTypes
import qualified Control.Exception.Lifted as E

-- | Time zone names that the database backend accepts.  See also
-- http://en.wikipedia.org/wiki/List_of_tz_database_time_zones
newtype TimeZoneName = TimeZoneName String
  deriving (Eq, Show, Ord, Typeable)

mkTimeZoneName :: (MonadBaseControl IO m, MonadDB m) => String -> m TimeZoneName
mkTimeZoneName s
  | not (sanityCheck s) = fail $ "mkTimeZoneName: illegal time zone string: " ++ show s
  | otherwise = do
    -- Check if we can use the string to form a valid SQL 'timestamp with time zone' value.
    runSQL_ ("SELECT cast(" <?> ("2000-01-01 " ++ s) <+> "as timestamp with time zone)")
      `E.catch` \(E.SomeException e) -> do
        fail $ "mkTimeZoneName: time zone not recognized by database: " ++ show (s, e)
    return $ TimeZoneName s

sanityCheck :: String -> Bool
sanityCheck s = case break (=='/') s of
  (as@(_:_),'/':bs) -> all isAlpha as && all (\b -> isAlphaNum b || b `elem` "/+-_") bs
  _                 -> all (\b -> isAlphaNum b || b `elem` "/+-_") s

withTimeZone :: forall m a. (MonadDB m, MonadBaseControl IO m)
             => TimeZoneName -> m a -> m a
withTimeZone (TimeZoneName tz) = E.bracket setNewTz setTz . const
  where
    setNewTz = do
      runSQL_ "SHOW timezone"
      oldtz <- fetchOne unSingle
      setTz tz
      return oldtz

    -- tz' was checked before in mkTimeZoneName/fromSQL, so unsafeSQL is safe.
    setTz :: String -> m ()
    setTz tz' = runSQL_ $ "SET timezone =" <+> unsafeSQL ("'" ++ tz' ++ "'")

toString :: TimeZoneName -> String
toString (TimeZoneName s) = s

----------------------------------------

instance PQFormat TimeZoneName where
  pqFormat _ = pqFormat (undefined::String)

instance FromSQL TimeZoneName where
  type PQBase TimeZoneName = PQBase String
  fromSQL mbase = do
    s <- fromSQL mbase
    if sanityCheck s
      then return $ TimeZoneName s
      else hpqTypesError $ "fromSQL (TimeZoneName): sanity check on value '" ++ s ++ "' failed"

instance ToSQL TimeZoneName where
  type PQDest TimeZoneName = PQDest String
  toSQL (TimeZoneName s) = toSQL s
