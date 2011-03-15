{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.MailsUtil
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Mail delivery status for now. We need this module because it is under State files and SendMail is above
-----------------------------------------------------------------------------
module Mails.MailsUtil(
             MailsDeliveryStatus(..)) where

import Data.Data
import Happstack.Data


data MailsDeliveryStatus = Delivered | Undelivered | Unknown
                           deriving (Eq, Ord, Typeable, Show, Read, Data)

   
$(deriveSerialize ''MailsDeliveryStatus)
instance Version MailsDeliveryStatus  