{-# LANGUAGE TemplateHaskell #-}
module Utils.TH where

import Language.Haskell.TH
import Prelude

-- | Evaluates to a function
-- @
--   \t -> case t of
--     Con{} -> True
--     _     -> False
-- @
isConstr :: Name -> Q Exp
isConstr con = do
  t <- newName "t"
  return $ LamE [VarP t] $ CaseE
    (VarE t)
    [ Match (RecP con []) (NormalB $ ConE 'True)  []
    , Match WildP         (NormalB $ ConE 'False) []
    ]
