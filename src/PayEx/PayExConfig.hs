{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  PayEx.PayExConfig
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
--  Configuration of PayEx service 
-----------------------------------------------------------------------------
module PayEx.PayExConfig(PayExConfig(..),getPayExConfig) where
import Control.Monad.Reader
import Payments.PaymentsState
import PayEx.PayExState

{- | View of payment models (not editable) -}
getPayExConfig:: IO PayExConfig
getPayExConfig = return $ PayExConfig {
                            accountNumber  ="50251179"
                          , encryptionKey ="ArxLm7bk3craUnNU2K9r"
                          , vat = 0 
                          , returnUrl = "http://localhost"
                         }

data PayExConfig = PayExConfig {
                     accountNumber::String,
                     encryptionKey::String,
                     vat::Integer,
                     returnUrl::String
                     } deriving Show
                     