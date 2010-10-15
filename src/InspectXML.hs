{-# LANGUAGE ForeignFunctionInterface, CPP, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances  #-}


module InspectXML where
import Control.Monad(msum,liftM,mzero,guard,MonadPlus(..))
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import Data.Maybe
import Data.Either
import HSX.XMLGenerator
import HSP
import qualified Control.Exception as C
import System.Log.Logger (errorM)
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
                     (map inspectXML l)

instance InspectXML a => InspectXML (Maybe a) where
    inspectXML Nothing = asChild "Nothing"
    inspectXML (Just x) = inspectXML x

-- m :: (XMLGenerator m) => [(String, GenChildList m)] -> GenChildList m
m g = map y g
      where y ("",cont) = asChild $ genElement (Nothing,"li") [] [cont]
            y (z,cont) = asChild $ genElement (Nothing,"li") [] [asChild z, asChild ": ", cont]

deriveInspectXML :: TH.Name -> TH.Q [TH.Dec]
deriveInspectXML name = do
  info <- TH.reify name
  let namesOfNormal fields = map (\x -> "") fields
  let namesOfRec fields = map (\(x,_,_) -> TH.nameBase x) fields
  let u :: TH.Name -> [String] -> TH.MatchQ
      u name fields = do
        n <- mapM (\f -> TH.newName f) fields
        let s = TH.nameBase name
        TH.match (TH.conP name (map TH.varP n)) 
                   (TH.normalB [| 
                                  asChild [ asChild s
                                  , asChild ":"
                                  , asChild $ genElement (Nothing,"ul") [] $
                                       m (zip fields $(TH.listE (map (\x -> TH.varE 'inspectXML `TH.appE` TH.varE x) n)))
                                  ]
                               |]) []

  let pcon (TH.NormalC name fields) = do
                          let k = namesOfNormal fields
                          u name k
      pcon (TH.RecC name fields) = do
                          let k = namesOfRec fields
                          u name k
      d name cons = 
        [d| instance InspectXML $(TH.conT name) where
              inspectXML x = $( TH.caseE (TH.varE 'x) 
                              (map pcon cons) ) |]

  case info of
    TH.TyConI (TH.DataD _ _ _ cons fields) -> d name cons
    TH.TyConI (TH.NewtypeD _ _ _ con fields) -> d name [con]
    _ -> error ("deriveInspectXML cannot handle: " ++ show info)

