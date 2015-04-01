module DB.Derive (
    newtypeDeriveUnderlyingReadShow
  , jsonFromSQL
  , jsonToSQL
  , jsonFromSQL'
  , jsonToSQL'
  , nothingToResult
  ) where

import Control.Arrow
import Data.Typeable
import Database.PostgreSQL.PQTypes
import Foreign.Ptr
import Language.Haskell.TH
import Text.JSON.Generic
import Text.JSON.String

import KontraPrelude

-- | Derives Read/Show instances for a given newtype
-- that behave like the ones of underlying type.
-- Given newtype T, it expands to the following code:
--
-- instance Show T where
--   showsPrec p (T v) = showsPrec p v
-- instance Read T where
--   readsPrec p s = first T `fmap` readsPrec p s
--
newtypeDeriveUnderlyingReadShow :: Name -> Q [Dec]
newtypeDeriveUnderlyingReadShow t = do
  info <- reify t
  case info of
    TyConI (NewtypeD _ name _ tcon _) -> do
      let con = case tcon of
            RecC c _    -> c
            NormalC c _ -> c
            _ -> error $ "Wrong constructor: " ++ show tcon
      p' <- newName "p"
      s' <- newName "s"
      v' <- newName "v"
      return [
        InstanceD [] (AppT (ConT ''Read) (ConT name)) [
            FunD 'readsPrec [
              Clause [VarP p', VarP s']
                (NormalB (InfixE (Just (AppE (VarE 'first) (ConE con))) (VarE 'fmap) (Just (AppE (AppE (VarE 'readsPrec) (VarE p')) (VarE s'))))) []
              ]
            ]
        , InstanceD [] (AppT (ConT ''Show) (ConT name)) [
            FunD 'showsPrec [
              Clause [VarP p', ConP con [VarP v']]
                (NormalB (AppE (AppE (VarE 'showsPrec) (VarE p')) (VarE v'))) []
              ]
            ]
        ]
    _ -> error $ "Not a newtype declaration: " ++ show info

jsonFromSQL :: (Data a, Typeable a) => Maybe (PQBase String) -> IO a
jsonFromSQL = jsonFromSQL' fromJSON

jsonToSQL :: Data a => a -> ParamAllocator -> (Ptr (PQDest String) -> IO r) -> IO r
jsonToSQL = jsonToSQL' toJSON

----------------------------------------

jsonFromSQL' :: forall a. (Data a, Typeable a)
             => (JSValue -> Result a) -> Maybe (PQBase String) -> IO a
jsonFromSQL' f mbase = do
  s <- fromSQL mbase
  case runGetJSON readJSValue s of
    Right js -> case f js of
      Error msg -> err msg
      Ok x -> return x
    Left msg -> err msg
  where
    err msg = hpqTypesError $ "jsonFromSQL (" ++ typerep ++ "): " ++ msg
    typerep = show $ typeOf (undefined::a)

jsonToSQL' :: Data a => (a -> JSValue) -> a -> ParamAllocator -> (Ptr (PQDest String) -> IO r) -> IO r
jsonToSQL' f v = toSQL (showJSValue (f v) "")

----------------------------------------

nothingToResult :: Maybe a -> Result a
nothingToResult Nothing  = Error "Nothing found when Just expected"
nothingToResult (Just a) = Ok a
