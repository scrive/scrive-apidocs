-----------------------------------------------------------------------------
-- |
-- Module      :  Util.ServiceUtils 
-- Author      :  Mariusz Rak
-- Stability   :  development
-- Portability :  portable
--
-- Checking if objects are owned by same service
-----------------------------------------------------------------------------
module Util.ServiceUtils (
              sameService
            , MayHaveService(..)
       ) where

import API.Service.Model
import User.Model
import Doc.DocStateData
import Company.Model
import Control.Monad
import Utils.Prelude

class MayHaveService a where
  getServiceID :: a -> Maybe ServiceID

instance MayHaveService Document where
  getServiceID = documentservice

instance MayHaveService User where
  getServiceID = userservice
  
instance MayHaveService Company where
  getServiceID = companyservice

instance MayHaveService ServiceID where
  getServiceID  = Just

instance MayHaveService Service where
  getServiceID  = getServiceID . serviceid

instance (MayHaveService a) => MayHaveService (Maybe a) where
  getServiceID = join . fmap getServiceID

sameService :: (MayHaveService a ,MayHaveService b) => a -> b -> Bool
sameService a b = joinB $ liftM2 (==) (getServiceID a) (getServiceID b)
