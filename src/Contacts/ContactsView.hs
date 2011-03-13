-----------------------------------------------------------------------------
-- |                MOCKUP!!!!!!!
-- Module      :  Administration.AdministrationView
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  develpment
-- Portability :  portable
--
-- Almoust all the stuff that is visible under /adminsonly path 
--
-----------------------------------------------------------------------------
module Contacts.ContactsView( contactsView
         ) where

import KontraLink
import Templates.Templates 
import Templates.TemplatesUtils 
import Text.StringTemplate.GenericStandard()
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

contactsView templates = renderTemplate templates "contactsView" ()