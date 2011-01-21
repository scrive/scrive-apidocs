{-# LANGUAGE CPP, OverloadedStrings, TupleSections , OverlappingInstances , ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  PayEx.PayExInterface
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
--  Wrapper around PeyEx webservices
-----------------------------------------------------------------------------
module PayEx.PayExInterface(payexTest) where
import "mtl" Control.Monad.State
import PayEx.PayExState
import PayEx.PayExRequest
import PayEx.PayExResponse
import SOAP.SOAP
import User
import Happstack.Server
import AppView
import qualified HSP.XML as HSP



payexTest::Kontra Response    
payexTest = do
             ctx <- get
             request <- askRq
             compleateRq <- toPC $ PayExInit request $ emptyPayment {orderRef = "6773ad5fb29740cfa46bcb3519f01ea8"}
             res <- liftIO $ (makeSoapCallINSECURE "https://test-external.payex.com/PxOrder/Pxorder.asmx"  "http://external.payex.com/PxOrder/Initialize7" compleateRq:: IO (Either String (PX InitResponse)))
             renderFromBody ctx TopNone kontrakcja $ HSP.cdata $ "Anser "++ (show res) 
