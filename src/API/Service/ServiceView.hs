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
import Util.HasSomeUserInfo
import Data.Maybe
import User.UserState
import Happstack.State (query)
import Data.Functor
import KontraLink

serviceAdminPage :: KontrakcjaTemplates -> Bool -> Service -> IO String
serviceAdminPage templates superuser service = 
    renderTemplate templates "serviceAdminPage" $ do
       field "name" $ show $ serviceid service
       field "nameforurl" $ encodeForURL $ serviceid service
       field "mailfooter"  $ servicemailfooter $ serviceui service
       field "mailfromaddress"  $ servicemailfromaddress $ servicesettings service
       field "buttons" $ isJust $ servicebuttons $ serviceui service
       field "buttonBodyLink"  $ show $ LinkServiceButtonsBody $ serviceid service
       field "buttonRestLink"  $ show $ LinkServiceButtonsRest $  serviceid service
       field "background"  $ servicebackground $ serviceui service   
       field "overlaybackground"  $ serviceoverlaybackground $ serviceui service   
       field "barsbackground"  $ servicebarsbackground $ serviceui service
       field "logo" $ isJust $ servicelogo $ serviceui service
       field "logoLink"  $ show $ LinkServiceLogo $ serviceid service
       fieldIO "admin" $ fmap getSmartName <$> (query $ GetUserByUserID $ UserID $ unServiceAdmin $ serviceadmin $ servicesettings service)
       field "location" $ fmap show $ servicelocation $ servicesettings service 
       field "allowToChangeSettings" $ superuser

servicesListPage :: KontrakcjaTemplates ->  [Service] -> IO String
servicesListPage templates services = 
    renderTemplate templates "serviceList" $ do
       field "services" $ 
            for services $ \srvs -> do
                field "name" $ show $ serviceid srvs
                field "nameforurl" $ encodeForURL $ serviceid srvs
                                    
