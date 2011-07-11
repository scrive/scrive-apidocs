-----------------------------------------------------------------------------
-- |           MOCKUP!!!!!!!
-- Module      :  Administration.AdministrationControl
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Handlers for all administrations tasks
--
-----------------------------------------------------------------------------
module Contacts.ContactsControl(
            showContacts
          , handleContactsChange
          ) where
import Control.Monad.State
import AppView
import Happstack.Server hiding (simpleHTTP)
import Kontra
import Contacts.ContactsView
import KontraLink
import FlashMessage


showContacts ::Kontra Response
showContacts =  do
  ctx <- getContext
  content <- liftIO $ contactsView (ctxtemplates ctx)
  renderFromBody TopEmpty kontrakcja content

{- | Process view for advanced user administration -}
handleContactsChange :: Kontra KontraLink
handleContactsChange =  do
  addFlashMsg $ toFlashMsg OperationDone "POST"
  return LoopBack
