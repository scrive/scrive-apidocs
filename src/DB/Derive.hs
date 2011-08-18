module DB.Derive (
    Convertible(..)
  , ConvertError(..)
  , newtypeDeriveUnderlyingReadShow
  , newtypeDeriveConvertible
  , enumDeriveConvertible
  ) where

import Control.Arrow
import Control.Monad
import Database.HDBC
import Data.Convertible
import Language.Haskell.TH

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
      forM_ cons $ \c -> do
        case c of
          NormalC _ [] -> return ()
          _ -> error $ "Data constructor '" ++ show c ++ "' is not trivial"
      v' <- newName "v"
      n' <- newName "n"
      e' <- newName "e"
      return [
          InstanceD [] (AppT (AppT (ConT ''Convertible) (ConT name)) (ConT ''SqlValue)) [
            FunD 'safeConvert $ map (\(n, NormalC con _) ->
              Clause [ConP con []]
                (NormalB (AppE (ConE 'Right) (AppE (ConE 'SqlInteger) (LitE (IntegerL n))))) [])
              $ zip [1..] cons
            ]
        , InstanceD [] (AppT (AppT (ConT ''Convertible) (ConT ''SqlValue)) (ConT name)) [
            FunD 'safeConvert [Clause [VarP v']
              (NormalB (CaseE (SigE (AppE (VarE 'safeConvert) (VarE v')) (AppT (ConT ''ConvertResult) (ConT ''Integer))) $
                  map (\(n, NormalC con _) ->
                    Match (ConP 'Right [LitP (IntegerL n)]) (NormalB (AppE (ConE 'Right) (ConE con))) [])
                    (zip [1..] cons)
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
