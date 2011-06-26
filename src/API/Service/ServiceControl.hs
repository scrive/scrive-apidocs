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
          ) where
import Control.Monad.State
import Data.Functor
import Happstack.State (update,query)
import Misc
import Kontra
import KontraLink
import Data.Maybe
import API.Service.ServiceState
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import API.Service.ServiceView
import Doc.DocUtils
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import FlashMessage
import Happstack.Server.SimpleHTTP

handleChangeServiceUI :: ServiceID -> Kontra KontraLink
handleChangeServiceUI sid = do
    ctx <- get
    mservice <- query $ GetService sid
    clear <- isFieldSet "clear"
    case (mservice,sameUser (ctxmaybeuser ctx) (serviceadmin . servicesettings <$> mservice)
                   || isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx),clear) of
        (Just service,True,False) -> do
           mailfooter <- getFieldUTF "mailfooter"
           buttonBody <-  getFileField "button-body"
           buttonRest <-  getFileField "button-rest"
           background <-  getFieldUTF "background"
           overlaybackground <-   getFieldUTF "overlaybackground"
           barsbackground <- getFieldUTF "barsbackground"
           logo <-  getFileField "logo"
           let ui = serviceui service
           update $ UpdateServiceUI sid $ ui {
              servicemailfooter = joinEmpty $ mailfooter `mplus` (servicemailfooter ui)
            , servicebuttons = (pairMaybe buttonBody buttonRest) `mplus`  
                               (mapFst (`fromMaybe` buttonBody) $ mapSnd (`fromMaybe` buttonRest) $ (servicebuttons  ui))
            , servicebackground = joinEmpty $ background `mplus` (servicebackground ui)
            , serviceoverlaybackground = joinEmpty $ overlaybackground `mplus` (serviceoverlaybackground ui)
            , servicebarsbackground =  joinEmpty $ barsbackground  `mplus` (servicebarsbackground  ui)
            , servicelogo = logo `mplus` (servicelogo ui)
            }
           addFlashMsg $ toFlashMsg OperationDone "UI setting changes"
           return LoopBack
        (Just service,True,True) -> do
           update $ UpdateServiceUI sid $ (serviceui service) {
              servicemailfooter = Nothing
            , servicebuttons = Nothing
            , servicebackground = Nothing
            , serviceoverlaybackground = Nothing
            , servicebarsbackground =  Nothing
            , servicelogo = Nothing
            }
           addFlashMsg $ toFlashMsg OperationDone "UI setting cleared"
           return LoopBack
        _ -> do
           addFlashMsg $ toFlashMsg OperationFailed "UI setting not saved"
           return LoopBack
       
handleChangeServicePassword :: ServiceID -> Kontra KontraLink
handleChangeServicePassword sid = do
    ctx <- get
    mservice <- query $ GetService sid
    case (mservice,sameUser (ctxmaybeuser ctx) (serviceadmin . servicesettings <$> mservice) 
                   || isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx)) of
     (Just service,True) -> do
            password <- getFieldUTFWithDefault BS.empty "oldpassword"
            newpassword1 <- getFieldUTFWithDefault BS.empty "newpassword1"
            newpassword2 <- getFieldUTFWithDefault BS.empty "newpassword2"
            if (verifyPassword (servicepassword $ servicesettings service) password) && (newpassword1 == newpassword2)
                then do
                    pwd <- liftIO $ createPassword password
                    update $ UpdateServiceSettings sid $ (servicesettings service) {servicepassword = pwd}
                    addFlashMsg $ toFlashMsg OperationDone "Password changed"
                    return LoopBack
                else do 
                    addFlashMsg $ toFlashMsg OperationFailed "Not changed"
                    return LoopBack
     _ -> do
         addFlashMsg $ toFlashMsg OperationFailed "Not changed"
         return LoopBack


       
handleChangeServiceSettings :: ServiceID -> Kontra KontraLink
handleChangeServiceSettings sid = do
    ctx <- get
    mservice <- query $ GetService sid
    case (mservice, isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx)) of
     (Just service,True) -> do
            location <- getFieldUTF "location"
            admin <- liftMM (query . GetUserByEmail Nothing) (fmap Email <$> getFieldUTF "admin")
            mailfromaddress <- getFieldUTF "mailfromaddress"
            update $ UpdateServiceSettings sid $ (servicesettings service)  
                        {   servicelocation = (ServiceLocation <$> location) `mplus` (servicelocation $ servicesettings  service)
                          , servicemailfromaddress  = mailfromaddress `mplus` (servicemailfromaddress $ servicesettings  service) 
                          , serviceadmin =  fromMaybe (serviceadmin $ servicesettings service) (ServiceAdmin <$> unUserID <$> userid <$> admin)
                        }
            addFlashMsg $ toFlashMsg OperationDone "Settings changed"
            return LoopBack
            
     _ -> do
         addFlashMsg $ toFlashMsg OperationFailed "Not changed"
         return LoopBack
    
       
handleShowService :: ServiceID -> Kontra (Either KontraLink String)
handleShowService sid = do
    ctx <- get
    mservice <- query $ GetService sid
    if ((isJust mservice) 
        && (sameUser (ctxmaybeuser ctx) (serviceadmin . servicesettings <$> mservice)
            || isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx)))
       then liftIO $  Right <$> serviceAdminPage (ctxtemplates ctx)  (isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx)) (fromJust mservice)
       else return $ Left LinkMain
       
handleShowServiceList  :: Kontra String
handleShowServiceList = do
    ctx <- get
    case (ctxmaybeuser ctx, isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx)) of
         (Just user, False) -> do
             srvs <- query $ GetServicesForAdmin $ ServiceAdmin $ unUserID $ userid user
             liftIO $ servicesListPage (ctxtemplates ctx) srvs
         (_, True) -> do             
             srvs <- query $ GetServices
             liftIO $ servicesListPage (ctxtemplates ctx) srvs
         _ -> mzero       

handleServiceLogo :: ServiceID -> Kontra Response
handleServiceLogo sid = do
    mlogo <- join <$> fmap (servicelogo . serviceui) <$> (query $ GetService sid)
    return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $ 
                      Response 200 Map.empty nullRsFlags (BSL.fromChunks $ maybeToList mlogo) Nothing
    
     
handleServiceButtonsBody:: ServiceID -> Kontra Response
handleServiceButtonsBody sid = do
    mimg <- join <$> fmap (fmap fst . servicebuttons . serviceui) <$> (query $ GetService sid)
    return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $ 
                      Response 200 Map.empty nullRsFlags (BSL.fromChunks $ maybeToList mimg) Nothing

handleServiceButtonsRest:: ServiceID -> Kontra Response
handleServiceButtonsRest sid = do
    mimg <- join <$> fmap (fmap snd . servicebuttons . serviceui) <$> (query $ GetService sid)
    return $ setHeaderBS (BS.fromString "Content-Type") (BS.fromString "image/png") $ 
                      Response 200 Map.empty nullRsFlags (BSL.fromChunks $ maybeToList mimg) Nothing
