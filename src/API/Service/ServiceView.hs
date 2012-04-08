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
import Misc
import API.Service.Model
import Util.HasSomeUserInfo
import Data.Maybe
import User.Model
import Data.Functor
import KontraLink
import DB.Classes
import qualified Templates.Fields as F

serviceAdminPage :: (MonadDB m, TemplatesMonad m) => Bool -> Service -> m String
serviceAdminPage superuser service = renderTemplate "serviceAdminPage" $ do
  F.value "name" $ show $ serviceid service
  F.value "nameforurl" $ encodeForURL $ serviceid service
  F.value "mailfooter" $ servicemailfooter $ serviceui service
  F.value "mailfromaddress" $ servicemailfromaddress $ servicesettings service
  F.value "buttons" $ isJust $ servicebuttons $ serviceui service
  F.value "buttonBodyLink" $ show $ LinkServiceButtonsBody $ serviceid service
  F.value "buttonRestLink" $ show $ LinkServiceButtonsRest $  serviceid service
  F.value "buttonstextcolor" $ servicebuttonstextcolor $ serviceui service
  F.value "background" $ servicebackground $ serviceui service
  F.value "overlaybackground" $ serviceoverlaybackground $ serviceui service
  F.value "barsbackground" $ servicebarsbackground $ serviceui service
  F.value "logo" $ isJust $ servicelogo $ serviceui service
  F.value "logoLink" $ show $ LinkServiceLogo $ serviceid service
  F.valueM "admin" $ fmap getSmartName <$> (runDBQuery $ GetUserByID $ serviceadmin $ servicesettings service)
  F.value "location" $ fmap unServiceLocation $ servicelocation $ servicesettings service
  F.value "allowToChangeSettings" $ superuser

servicesListPage :: TemplatesMonad m => [Service] -> m String
servicesListPage services = renderTemplate "serviceList" $ do
  F.objects "services" $ for services $ \srvs -> do
    F.value "name" $ show $ serviceid srvs
    F.value "nameforurl" $ encodeForURL $ serviceid srvs
