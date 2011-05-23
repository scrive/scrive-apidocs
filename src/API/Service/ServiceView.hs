{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  API.Service.ServiceView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  develpment
-- Portability :  portable
--
-- Almoust all the stuff that is visible under /adminsonly path 
--
-----------------------------------------------------------------------------
module API.Service.ServiceView( 
              serviceAdminPage
            , servicesListPage
) where

import KontraLink
import Templates.Templates 
import Templates.TemplatesUtils 
import Text.StringTemplate.GenericStandard()
import Control.Applicative
import Data.ByteString.UTF8 (toString)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char
import Data.Typeable
import Data.Data
import Payments.PaymentsView
import Payments.PaymentsState
import Payments.PaymentsUtils
import Misc
import MinutesTime
import User.UserView
import User.UserState
import Doc.DocState
import API.Service.ServiceState
import Happstack.State (query)
import Control.Monad
import API.Service.ServiceState

serviceAdminPage :: KontrakcjaTemplates -> Service -> IO String
serviceAdminPage templates service = 
    renderTemplate templates "serviceAdminPage" $ do
       field "name" $ show $ serviceid service
       field "nameforurl" $ encodeForURL $ serviceid service
       field "nicename"  $  servicename service
       field "documentinvitationmail" $ servicedocumentinvitationmail service

servicesListPage :: KontrakcjaTemplates ->  [Service] -> IO String
servicesListPage templates services = 
    renderTemplate templates "serviceList" $ do
       field "services" $ 
            for services $ \srvs -> do
                field "name" $ show $ serviceid srvs
                field "nameforurl" $ encodeForURL $ serviceid srvs
                                    
