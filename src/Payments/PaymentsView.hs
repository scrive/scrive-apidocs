{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Payments.PaymentsView(adminView,salesView) where

import HSP hiding (Request)
import Happstack.Server.SimpleHTTP
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import UserState
import AppView
import User
import KontraLink
import SendMail(Mail,emptyMail,title,content)
import qualified HSX.XMLGenerator
import Templates
import Payments.PaymentsState

{- adminView provides view for admin-}
adminView templates = do
             renderTemplate templates "paymentsadminpagenotselected" $   [("action", show LinkPaymentsAdmin)]++
                                                    map (\x->("list",show x)) accountTypes

{- View of payments for sales guy-}
salesView templates =do  renderTemplate templates "paymentsadminpagenotselected" $   [("action", show LinkPaymentsAdmin)]++
                                                                   map (\x->("list",show x)) accountTypes
