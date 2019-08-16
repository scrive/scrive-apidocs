{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  InspectXML
--
-- Class for viewing data. Useful for debugging etc.
-- Contains a derivator.
-----------------------------------------------------------------------------
module InspectXML where

import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Utils.String

class InspectXML a where
  inspectXML :: (TextShow a) => a -> Text
  inspectXML = escapeString . showt

table :: Text -> Text -> Text
table a b = "<table><tbody><tr><td valign='top' style='padding-right:5px'> "<> a <>" </td><td> </td><td> "<> b <>" </td></tr></tbody></table>"

deriveInspectXML :: TH.Name -> TH.Q [TH.Dec]
deriveInspectXML name = do
  info <- TH.reify name
  let namesOfNormal fields = map (\_x -> "") fields
  let namesOfRec fields = map (\(x, _, _) -> TH.nameBase x) fields
  let u :: TH.Name -> [String] -> TH.MatchQ
      u fname fields = do
        n <- mapM (\f -> TH.newName f) fields
        let s = TH.nameBase fname
        TH.match (TH.conP fname (map TH.varP n))
                   (TH.normalB [|  table s (T.concat (zipWith table fields $(TH.listE (map (\x -> TH.varE 'inspectXML `TH.appE` TH.varE x) n))))

                               |]) []
  let pcon (TH.NormalC fname fields) = do
                          let k = namesOfNormal fields
                          u fname k
      pcon (TH.RecC fname fields) = do
                          let k = namesOfRec fields
                          u fname k
      d fname cons =
        [d| instance InspectXML $(TH.conT fname) where
              inspectXML x = $( TH.caseE (TH.varE 'x)
                              (map pcon cons) ) |]

  case info of
    TH.TyConI (TH.DataD _ _ _ _ cons _fields) -> d name cons
    TH.TyConI (TH.NewtypeD _ _ _ _ con _fields) -> d name [con]
    _ -> unexpectedError $ "deriveInspectXML cannot handle: " <> (showt info)
