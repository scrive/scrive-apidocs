{-# LANGUAGE CPP #-}

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
            , handleChangeService
          ) where
import Control.Monad.State
import Data.Functor
import Happstack.State (update,query)
import Misc
import Kontra
import qualified Data.ByteString as BS
import KontraLink
import Data.Maybe
import API.Service.ServiceState
import API.Service.ServiceView
import Doc.DocUtils


handleChangeService :: ServiceID -> Kontra KontraLink
handleChangeService sid = do
    ctx <- get
    mservice <- query $ GetService sid
    if ((isJust mservice) && sameUser (ctxmaybeuser ctx) (serviceadmin <$> mservice))
       then do
           --let service = fromJust mservice
           nicename <- getFieldUTFWithDefault BS.empty "nicename"
           invitationmain <- getFieldUTFWithDefault BS.empty "documentinvitationmail"
           update $ UpdateService sid nicename invitationmain
           return LoopBack
       else return $ LinkMain
       
       
       
handleShowService :: ServiceID -> Kontra (Either KontraLink String)
handleShowService sid = do
    ctx <- get
    mservice <- query $ GetService sid
    if ((isJust mservice) && sameUser (ctxmaybeuser ctx) (serviceadmin <$> mservice))
       then liftIO $  Right <$> serviceAdminPage (ctxtemplates ctx) (fromJust mservice)
       else return $ Left LinkMain
       
handleShowServiceList  :: Kontra String
handleShowServiceList = do
    ctx <- get
    case (ctxmaybeuser ctx) of
         Nothing -> mzero
         Just user -> do
             srvs <- query $ GetServicesForAdmin $ ServiceAdmin $ unUserID $ userid user
             liftIO $ servicesListPage (ctxtemplates ctx) srvs
