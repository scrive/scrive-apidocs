{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Payments.PaymentsUtils
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
-- Some usefull utils, now only for parsing money 
--
-----------------------------------------------------------------------------
module Payments.PaymentsUtils(showMoney,readMoney) where

import KontraLink
import Templates
import Payments.PaymentsState
import Data.Typeable
import Data.Data
import Control.Monad
import Misc

{-| Money printer. Since show instance is derived by storage module, we provided nicer version -}
showMoney::Money->String  
showMoney (Money i) = if (i>=0)
                       then (show $ i `div` 100)++"."++(show $ (i `div` 10) `mod` 10) ++ (show $ i `mod` 10)
                       else "-" ++ (showMoney (Money $ -1*i))
                       
{-| Money parser -}
readMoney::String->Maybe Money  
readMoney s = do
               let (m,r) = break (== '.') s 
               main<-maybeRead m
               rest<-(maybeRead $ drop 1 r) `mplus` (return 0)
               let rest' = rest * (if (main>=0) then 1 else -1)
               return $ Money (main * 100 + rest')
