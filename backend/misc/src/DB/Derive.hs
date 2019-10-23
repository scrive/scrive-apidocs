module DB.Derive (
    jsonFromSQL
  , jsonToSQL
  , jsonFromSQL'
  , jsonToSQL'
  , nothingToResult
  ) where

import Database.PostgreSQL.PQTypes
import Foreign.Ptr
import Text.JSON.Generic
import Text.JSON.String
import Type.Reflection

jsonFromSQL :: (Data a, Typeable a) => Maybe (PQBase String) -> IO a
jsonFromSQL = jsonFromSQL' fromJSON

jsonToSQL :: Data a => a -> ParamAllocator -> (Ptr (PQDest String) -> IO r) -> IO r
jsonToSQL = jsonToSQL' toJSON

----------------------------------------

jsonFromSQL'
  :: forall a
   . (Data a, Typeable a)
  => (JSValue -> Result a)
  -> Maybe (PQBase String)
  -> IO a
jsonFromSQL' f mbase = do
  s <- fromSQL mbase
  case runGetJSON readJSValue s of
    Right js -> case f js of
      Error msg -> err msg
      Ok    x   -> return x
    Left msg -> err msg
  where
    err msg = hpqTypesError $ "jsonFromSQL (" ++ typerep ++ "): " ++ msg
    typerep = show $ typeRep @a

jsonToSQL'
  :: Data a
  => (a -> JSValue)
  -> a
  -> ParamAllocator
  -> (Ptr (PQDest String) -> IO r)
  -> IO r
jsonToSQL' f v = toSQL (showJSValue (f v) "")

----------------------------------------

nothingToResult :: Maybe a -> Result a
nothingToResult Nothing  = Error "Nothing found when Just expected"
nothingToResult (Just a) = Ok a
