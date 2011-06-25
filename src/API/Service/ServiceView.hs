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

import Templates.Templates 
import Text.StringTemplate.GenericStandard()
import Misc
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
                                    
