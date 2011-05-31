{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  InspectXML
--
-- Class and basic instances for viewing data. Usefull for debugging etc.
-- Contains a derivator. But it forces big types to be moved to InspectXMLUtil.
-- Otherwise we would like to have them here.
-----------------------------------------------------------------------------
module InspectXML where

import HSX.XMLGenerator
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Language.Haskell.TH as TH

class InspectXML a where
    inspectXML :: (XMLGenerator m) => a -> GenChildList m

instance InspectXML Bool where
    inspectXML = asChild . show

instance InspectXML Int where
    inspectXML = asChild . show

instance InspectXML BS.ByteString where
    inspectXML = asChild . BS.toString

instance InspectXML a => InspectXML [a] where
    inspectXML l = asChild $ genElement (Nothing,"ol") []
                     (map (asChild . genElement (Nothing,"li") [] . return . inspectXML) l)

instance InspectXML a => InspectXML (Maybe a) where
    inspectXML Nothing = asChild "Nothing"
    inspectXML (Just x) = inspectXML x

instance (InspectXML a, InspectXML b, InspectXML c) => InspectXML (a, b, c) where
    inspectXML (a, b, c) = asChild $ genElement (Nothing, "triple") []
                            [inspectXML a, inspectXML b, inspectXML c]

m :: forall (m :: * -> *). EmbedAsChild m (XML m) => [([Char], XMLGenT m [Child m])] -> [GenChildList m]
m g = map y g
      where y ("",cont) = asChild $ genElement (Nothing,"li") [] [cont]
            y (z,cont) = asChild $ genElement (Nothing,"li") [] [asChild z, asChild ": ", cont]

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
                   (TH.normalB [| 
                                  asChild [ asChild s
                                  , asChild ":"
                                  , asChild $ genElement (Nothing,"ul") [] $
                                       m (zip fields $(TH.listE (map (\x -> TH.varE 'inspectXML `TH.appE` TH.varE x) n)))
                                  ]
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
    TH.TyConI (TH.DataD _ _ _ cons _fields) -> d name cons
    TH.TyConI (TH.NewtypeD _ _ _ con _fields) -> d name [con]
    _ -> error ("deriveInspectXML cannot handle: " ++ show info)

