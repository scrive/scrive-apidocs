{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module DB.Derive (
    Convertible(..)
  , ConvertError(..)
  , newtypeDeriveUnderlyingReadShow
  , newtypeDeriveConvertible
  , enumDeriveConvertible
  , bitfieldDeriveConvertible
  , jsonableDeriveConvertible
  ) where

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.List
import Data.Typeable(typeOf)
import Database.HDBC
import Data.Convertible
import Language.Haskell.TH
import Text.JSON.Generic
import Text.JSON.String

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

-- | Derives Convertible instances from/to SqlValue.
-- Given newtype T, it expands to the following code:
--
--  instance Convertible T SqlValue where
--    safeConvert (T v) = safeConvert v
--  instance Convertible SqlValue T where
--    safeConvert = fmap T . safeConvert
--
newtypeDeriveConvertible :: Name -> Q [Dec]
newtypeDeriveConvertible t = do
  info <- reify t
  case info of
    TyConI (NewtypeD _ name _ tcon _) -> do
      let con = case tcon of
            RecC c _    -> c
            NormalC c _ -> c
            _ -> error $ "Wrong constructor: " ++ show tcon
      v' <- newName "v"
      return [
          InstanceD [] (AppT (AppT (ConT ''Convertible) (ConT name)) (ConT ''SqlValue)) [
            FunD 'safeConvert [
              Clause [ConP con [VarP v']]
                (NormalB (AppE (VarE 'safeConvert) (VarE v'))) []
              ]
            ]
        , InstanceD [] (AppT (AppT (ConT ''Convertible) (ConT ''SqlValue)) (ConT name)) [
            ValD (VarP 'safeConvert)
              (NormalB (InfixE (Just (AppE (VarE 'fmap) (ConE con))) (VarE '(.)) (Just (VarE 'safeConvert)))) []
            ]
        ]
    _ -> error $ "Not a newtype declaration: " ++ show info

-- | Derives Convertible instances from/to SqlValue for enums, i.e. data
-- types providing only trivial constructors that don't take any values.
-- Given data T = C1 | C2 |  ... | CN, it expands to the following code:
--
-- instance Convertible T SqlValue where
--   safeConvert C1 = Right (SqlInteger 1)
--   safeConvert C2 = Right (SqlInteger 2)
--   ...
--   safeConvert CN = Right (SqlInteger N)
-- instance Convertible SqlValue T where
--  safeConvert v =
--    case safeConvert v :: ConvertResult Integer of
--      Right 1 -> Right C1
--      Right 2 -> Right C2
--      ...
--      Right N -> Right CN
--      Right n -> Left $ ConvertError {
--          convSourceValue = show n
--        , convSourceType = "Integer"
--        , convDestType = "T"
--        , convErrorMessage = "Value is out of bounds ([1;" ++ show N ++ "] expected, " ++ show n ++ " given)"
--      }
--      Left e  -> Left e
--
enumDeriveConvertible :: Name -> Q [Dec]
enumDeriveConvertible t = do
  info <- reify t
  case info of
    TyConI (DataD _ name _ cons _) -> do
      ncons <- forM cons $ \c -> do
        case c of
          NormalC con [] -> return con
          _ -> error $ "Data constructor '" ++ show c ++ "' is not trivial"
      v' <- newName "v"
      n' <- newName "n"
      e' <- newName "e"
      return [
          InstanceD [] (AppT (AppT (ConT ''Convertible) (ConT name)) (ConT ''SqlValue)) [
            FunD 'safeConvert $ map (\(n, con) ->
              Clause [ConP con []]
                (NormalB (AppE (ConE 'Right) (AppE (ConE 'SqlInteger) (LitE (IntegerL n))))) [])
              $ zip [1..] ncons
            ]
        , InstanceD [] (AppT (AppT (ConT ''Convertible) (ConT ''SqlValue)) (ConT name)) [
            FunD 'safeConvert [Clause [VarP v']
              (NormalB (CaseE (SigE (AppE (VarE 'safeConvert) (VarE v')) (AppT (ConT ''ConvertResult) (ConT ''Integer))) $
                  map (\(n, con) ->
                    Match (ConP 'Right [LitP (IntegerL n)]) (NormalB (AppE (ConE 'Right) (ConE con))) [])
                    (zip [1..] ncons)
                ++ [
                  Match (ConP 'Right [VarP n'])
                    (NormalB (AppE (ConE 'Left) (RecConE 'ConvertError [
                        ('convSourceValue, AppE (VarE 'show) (VarE n'))
                      , ('convSourceType, LitE (StringL "Integer"))
                      , ('convDestType, LitE (StringL $ show name))
                      , ('convErrorMessage, InfixE (Just (LitE (StringL "Value is out of bounds ([1;"))) (VarE '(++)) (Just (InfixE (Just (AppE (VarE 'show) (SigE (LitE (IntegerL $ fromIntegral $ length cons)) (ConT ''Int)))) (VarE '(++)) (Just (InfixE (Just (LitE (StringL "] expected, "))) (VarE '(++)) (Just (InfixE (Just (AppE (VarE 'show) (VarE n'))) (VarE '(++)) (Just (LitE (StringL " given)"))))))))))
                      ]))) []
                , Match (ConP 'Left [VarP e'])
                    (NormalB (AppE (ConE 'Left) (VarE e'))) []
                ])) []
              ]
            ]
        ]
    _ -> error $ "Not a data type declaration: " ++ show info

-- | Derives Convertible instances from/to SqlValue for bitfields, i.e.
-- data types providing only trivial constructors that don't take any values.
-- Given data T = C1 | C2 |  ... | CN, it expands to the following code:
--
-- instance Convertible [T] SqlValue where
--   safeConvert = Right . SqlInteger . foldl' (\acc n -> acc + mkInt n) 0
--     where
--       mkInt C1 = 2^0
--       mkInt C2 = 2^1
--       ...
--       mkInt CN = 2^n
--
-- instance Convertible SqlValue [T] where
--   safeConvert v = case safeConvert v :: ConvertResult Int of
--     Right n -> Right [ x | (p, x) <- values, p .&. n /= 0]
--     Left e -> Left e
--     where
--       values = zip (map (shiftL 1) [0..]) [C1, C2, ..., CN]
--

bitFieldToSqlValue :: Enum a => [a] -> ConvertResult SqlValue
bitFieldToSqlValue = Right . SqlInteger . foldl' (\acc n -> acc + 2^fromEnum n) 0
 
bitFieldFromSqlValue :: (Bounded a, Enum a) => SqlValue -> ConvertResult [a]
bitFieldFromSqlValue = fmap conv . (safeConvert :: SqlValue -> ConvertResult Integer)
  where conv n = [x | (p, x) <- values, p.&. n /= 0]
        values = [(shiftL 1 (fromEnum c),c) | c <- [minBound..maxBound]]

bitfieldDeriveConvertible :: Name -> Q [Dec]
bitfieldDeriveConvertible t = do
  info <- reify t
  case info of
    TyConI (DataD _ name _ cons _) -> do
      let _ = name -- workaround for bug in ghc 6.12.3 for unused variable warning
      forM_ cons $ \c -> do
        case c of
          NormalC _ [] -> return ()
          _ -> error $ "Data constructor '" ++ show c ++ "' is not trivial"
      [d|instance Convertible [$(conT name)] SqlValue where safeConvert = bitFieldToSqlValue
         instance Convertible SqlValue [$(conT name)] where safeConvert = bitFieldFromSqlValue|]
    _ -> error $ "Not a data type declaration: " ++ show info

-- | Derives Convertible instances from/to SqlValue for types that will
-- be stored in DB as JSON (Data/Typeable instances are required)

jsonToSqlValue :: Data a => a -> ConvertResult SqlValue
jsonToSqlValue = Right . SqlString . encodeJSON

jsonFromSqlValue :: forall a. (Data a, Typeable a) => SqlValue -> ConvertResult a
jsonFromSqlValue v = safeDecodeJSON (show (typeOf (undefined :: a))) =<< safeConvert v

jsonableDeriveConvertible :: TypeQ -> Q [Dec]
jsonableDeriveConvertible tq = do
  let _ = tq -- workaround for bug in ghc 6.12.3 for unused variable warning
  [d|instance Convertible $(tq) SqlValue where safeConvert = jsonToSqlValue
     instance Convertible SqlValue $(tq) where safeConvert = jsonFromSqlValue|]

safeDecodeJSON :: Data a => String -> String -> ConvertResult a
safeDecodeJSON t s = case runGetJSON readJSValue s of
  Right j -> case fromJSON j of
    Error msg -> err msg
    Ok x -> Right x
  Left msg -> err msg
  where
    err msg = Left ConvertError {
        convSourceValue = s
      , convSourceType = "String"
      , convDestType = t
      , convErrorMessage = msg
    }
