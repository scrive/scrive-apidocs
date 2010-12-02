{-# LANGUAGE CPP, OverloadedStrings #-}

module Payments.PaymentsControl where
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Trans (liftIO,MonadIO,lift)
import "base" Control.Monad
import AppView
import Data.Maybe
import Data.Object
import DocState
import DocView
import Happstack.Data.IxSet 
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (Update,update,query)
import Happstack.Util.Common (readM)
import KontraLink
import Misc
import SendMail(Mail,sendMail,fullnameemails)
import Session
import System.Log.Logger
import System.Process
import System.Random
import User
import UserState
import UserView
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Lazy.UTF8 as LS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Object.Json as Json
import qualified Data.Set as Set
import HSP
import Payments.PaymentsState
import Payments.PaymentsView

{- adminView provides view for admin-}
handleAdminView::Maybe String -> Kontra Response
handleAdminView Nothing = do
                    ctx<- get
                    content <- liftIO $ adminView (ctxtemplates ctx)
                    renderFromBody ctx TopEmpty kontrakcja $ cdata content

handleAdminView a = do
                    ctx<- get
                    content <- liftIO $ adminView (ctxtemplates ctx)
                    renderFromBody ctx TopEmpty kontrakcja $ cdata content
                    
{- View of payments for sales guy-}
handleSalesView::Maybe String -> Kontra Response
handleSalesView Nothing = do
                           ctx<- get
                           content <- liftIO $ salesView (ctxtemplates ctx)
                           renderFromBody ctx TopEmpty kontrakcja $ cdata content
                           
handleSalesView a = do
                           ctx<- get
                           content <- liftIO $ salesView (ctxtemplates ctx)
                           renderFromBody ctx TopEmpty kontrakcja $ cdata content                           
 