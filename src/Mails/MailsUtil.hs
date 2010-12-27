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

import Happstack.Data


$(deriveAll [''Eq, ''Ord, ''Default, ''Show]
  [d|
     data MailsDeliveryStatus = Delivered | Undelivered | Unknown
   |])
   
$(deriveSerialize ''MailsDeliveryStatus)
instance Version MailsDeliveryStatus  