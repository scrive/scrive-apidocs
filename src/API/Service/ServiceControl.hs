-----------------------------------------------------------------------------
-- |
-- Module      :  API.Service.ServiceControl
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Handlers for all administrations tasks
--
-----------------------------------------------------------------------------
module  API.Service.ServiceControl(
              handleShowService
            , handleShowServiceList
            , handleChangeServiceUI
            , handleChangeServicePassword
            , handleChangeServiceSettings
            , handleServiceLogo
            , handleServiceButtonsBody
            , handleServiceButtonsRest
            , handleChangeServicePasswordAdminOnly
          ) where
import Control.Monad.State
import Data.Functor
import Misc
import Kontra
import KontraLink
import Data.Maybe
import API.Service.Model
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import API.Service.ServiceView
import Doc.DocUtils
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Happstack.Server.SimpleHTTP
import DB.Classes
import DB.Types
import User.Model
import Util.FlashUtil
import Util.KontraLinkUtils
import Util.MonadUtils

handleChangeServiceUI :: Kontrakcja m => ServiceID -> m KontraLink
handleChangeServiceUI sid = do
    ctx <- getContext
    mservice <- runDBQuery $ GetService sid
    clear <- isFieldSet "clear"
    case (mservice,sameUser (ctxmaybeuser ctx) (serviceadmin . servicesettings <$> mservice)
                   || isAdmin ctx,clear) of
        (Just service,True,False) -> do
           mailfooter <- getFieldUTF "mailfooter"
           buttonBody <- fmap Binary <$> getFileField "button-body"
           buttonRest <- fmap Binary <$> getFileField "button-rest"
           buttonstextcolor <- getFieldUTF "buttonstextcolor"
           background <- getFieldUTF "background"
           overlaybackground <- getFieldUTF "overlaybackground"
           barsbackground <- getFieldUTF "barsbackground"
           logo <- fmap Binary <$> getFileField "logo"
           let ui = serviceui service
           _ <- runDBUpdate $ UpdateServiceUI sid $ ui {
              servicemailfooter = joinEmpty $ mailfooter `mplus` (servicemailfooter ui)
            , servicebuttons = (pairMaybe buttonBody buttonRest) `mplus`
                               (mapFst (`fromMaybe` buttonBody) $ mapSnd (`fromMaybe` buttonRest) $ (servicebuttons  ui))
            , servicebuttonstextcolor = joinEmpty $ buttonstextcolor `mplus` (servicebuttonstextcolor ui)                               
            , servicebackground = joinEmpty $ background `mplus` (servicebackground ui)
            , serviceoverlaybackground = joinEmpty $ overlaybackground `mplus` (serviceoverlaybackground ui)
            , servicebarsbackground =  joinEmpty $ barsbackground  `mplus` (servicebarsbackground  ui)
            , servicelogo = logo `mplus` (servicelogo ui)
            }
           addFlash $ (OperationDone, "UI setting changes")
           return LoopBack
        (Just service,True,True) -> do
           _ <- runDBUpdate $ UpdateServiceUI sid $ (serviceui service) {
              servicemailfooter = Nothing
            , servicebuttons = Nothing
            , servicebuttonstextcolor = Nothing
            , servicebackground = Nothing
            , serviceoverlaybackground = Nothing
            , servicebarsbackground =  Nothing
            , servicelogo = Nothing
            }
           addFlash (OperationDone, "UI setting cleared")
           return LoopBack
        _ -> do
           addFlash (OperationFailed, "UI setting not saved")
           return LoopBack

handleChangeServicePassword :: Kontrakcja m => ServiceID -> m KontraLink
handleChangeServicePassword sid = do
    ctx <- getContext
    mservice <- runDBQuery $ GetService sid
    case (mservice,sameUser (ctxmaybeuser ctx) (serviceadmin . servicesettings <$> mservice)
                   || isAdmin ctx) of
     (Just service,True) -> do
            password <- getFieldUTFWithDefault BS.empty "oldpassword"
            newpassword1 <- getFieldUTFWithDefault BS.empty "newpassword1"
            newpassword2 <- getFieldUTFWithDefault BS.empty "newpassword2"
            if (verifyPassword (servicepassword $ servicesettings service) password) && (newpassword1 == newpassword2)
                then do
                    pwd <- createPassword newpassword1
                    _ <- runDBUpdate $ UpdateServiceSettings sid $ (servicesettings service) {servicepassword = Just pwd}
                    addFlash (OperationDone, "Password changed")
                    return LoopBack
                else do
                    addFlash (OperationFailed, "Not changed")
                    return LoopBack
     _ -> do
         addFlash (OperationFailed, "Not changed")
         return LoopBack

handleChangeServicePasswordAdminOnly :: Kontrakcja m => ServiceID -> String -> m KontraLink
handleChangeServicePasswordAdminOnly sid passwordString = do
    let password = BS.fromString passwordString
    ctx <- getContext
    mservice <- runDBQuery $ GetService sid
    case (mservice, isAdmin ctx) of
     (Just service,True) -> do
       pwd <- createPassword password
       _ <- runDBUpdate $ UpdateServiceSettings sid $ (servicesettings service) {servicepassword = Just pwd}
       addFlash (OperationDone, "Password changed")
       getHomeOrUploadLink
     _ -> mzero

handleChangeServiceSettings :: Kontrakcja m => ServiceID -> m KontraLink
handleChangeServiceSettings sid = do
    ctx <- getContext
    mservice <- runDBQuery $ GetService sid
    case (mservice, isAdmin ctx) of
     (Just service,True) -> do
            location <- getFieldUTF "location"
            admin <- liftMM (runDBQuery . GetUserByEmail Nothing) (fmap Email <$> getFieldUTF "admin")
            mailfromaddress <- getFieldUTF "mailfromaddress"
            _ <- runDBUpdate $ UpdateServiceSettings sid $ (servicesettings service)
                        {   servicelocation = (ServiceLocation <$> location) `mplus` (servicelocation $ servicesettings  service)
                          , servicemailfromaddress  = mailfromaddress `mplus` (servicemailfromaddress $ servicesettings  service)
                          , serviceadmin =  fromMaybe (serviceadmin $ servicesettings service) (userid <$> admin)
                        }
            addFlash (OperationDone, "Settings changed")
            return LoopBack

     _ -> do
         addFlash (OperationFailed, "Not changed")
         return LoopBack


handleShowService :: Kontrakcja m => ServiceID -> m (Either KontraLink String)
handleShowService sid = do
    ctx <- getContext
    conn <- getConnection
    mservice <- runDBQuery $ GetService sid
    if ((isJust mservice)
        && (sameUser (ctxmaybeuser ctx) (serviceadmin . servicesettings <$> mservice)
            || isAdmin ctx))
       then Right <$> serviceAdminPage conn (isAdmin ctx) (fromJust mservice)
       else do
              linkmain <- getHomeOrUploadLink
              return $ Left linkmain

handleShowServiceList :: Kontrakcja m => m String
handleShowServiceList = do
    ctx <- getContext
    case (ctxmaybeuser ctx, isAdmin ctx) of
         (Just user, False) -> do
             srvs <- runDBQuery $ GetServicesForAdmin $ userid user
             servicesListPage srvs
         (_, True) -> do
             srvs <- runDBQuery GetServices
             servicesListPage srvs
         _ -> mzero

handleServiceLogo :: Kontrakcja m => ServiceID -> m Response
handleServiceLogo = handleServiceBinary (servicelogo . serviceui)

handleServiceButtonsBody :: Kontrakcja m =>ServiceID -> m Response
handleServiceButtonsBody = handleServiceBinary (fmap fst . servicebuttons . serviceui)

handleServiceButtonsRest :: Kontrakcja m => ServiceID -> m Response
handleServiceButtonsRest = handleServiceBinary (fmap snd . servicebuttons . serviceui)

handleServiceBinary :: Kontrakcja m => (Service -> Maybe Binary) -> ServiceID -> m Response
handleServiceBinary f sid = do
  mimg <- join <$> fmap f <$> (runDBQuery $ GetService sid)
  return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $
    Response 200 Map.empty nullRsFlags (BSL.fromChunks $ map unBinary $ maybeToList mimg) Nothing
