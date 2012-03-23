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
import API.Service.Model
import Util.HasSomeUserInfo
import Data.Maybe
import User.Model
import Data.Functor
import KontraLink
import DB.Classes

serviceAdminPage :: TemplatesMonad m => DBEnv -> Bool -> Service -> m String
serviceAdminPage dbenv superuser service =
    renderTemplateFM "serviceAdminPage" $ do
       field "name" $ show $ serviceid service
       field "nameforurl" $ encodeForURL $ serviceid service
       field "mailfooter"  $ servicemailfooter $ serviceui service
       field "mailfromaddress"  $ servicemailfromaddress $ servicesettings service
       field "buttons" $ isJust $ servicebuttons $ serviceui service
       field "buttonBodyLink"  $ show $ LinkServiceButtonsBody $ serviceid service
       field "buttonRestLink"  $ show $ LinkServiceButtonsRest $  serviceid service
       field "buttonstextcolor"  $ servicebuttonstextcolor $ serviceui service
       field "background"  $ servicebackground $ serviceui service
       field "overlaybackground"  $ serviceoverlaybackground $ serviceui service
       field "barsbackground"  $ servicebarsbackground $ serviceui service
       field "logo" $ isJust $ servicelogo $ serviceui service
       field "logoLink"  $ show $ LinkServiceLogo $ serviceid service
       fieldM "admin" $ fmap getSmartName <$> (ioRunDB dbenv $ dbQuery $ GetUserByID $ serviceadmin $ servicesettings service)
       field "location" $ fmap unServiceLocation $ servicelocation $ servicesettings service
       field "allowToChangeSettings" $ superuser

servicesListPage :: TemplatesMonad m => [Service] -> m String
servicesListPage services =
    renderTemplateFM "serviceList" $ do
       fieldFL "services" $
            for services $ \srvs -> do
                field "name" $ show $ serviceid srvs
                field "nameforurl" $ encodeForURL $ serviceid srvs
