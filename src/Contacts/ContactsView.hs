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

import Templates.Templates
import Text.StringTemplate.GenericStandard()

contactsView :: KontrakcjaTemplates -> IO String
contactsView templates = renderTemplate templates "contactsView" ()
