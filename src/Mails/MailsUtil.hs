{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
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


data MailsDeliveryStatus = Delivered | Undelivered | Unknown | Deferred
                           deriving (Eq, Ord, Typeable, Show, Read, Data)

   
$(deriveSerialize ''MailsDeliveryStatus)
instance Version MailsDeliveryStatus  
