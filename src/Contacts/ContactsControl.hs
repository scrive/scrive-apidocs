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
import HSP (cdata)
import Contacts.ContactsView
import KontraLink


showContacts ::Kontra Response
showContacts =  do
  ctx <- lift get
  content <- liftIO $ contactsView (ctxtemplates ctx)
  renderFromBody ctx TopEmpty kontrakcja $ cdata content 

{- | Process view for advanced user administration -}                    
handleContactsChange :: Kontra KontraLink
handleContactsChange =  do
  addFlashMsg $ FlashMessage (OperationDone,"POST")
  return LoopBack


