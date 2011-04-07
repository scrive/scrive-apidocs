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
import Happstack.State (update,query)
import Misc
import Kontra
import HSP (cdata)
import Contacts.ContactsView
import Payments.PaymentsState
import Doc.DocState
import Data.ByteString.UTF8 (fromString,toString)
import Data.ByteString (ByteString,empty, hGetContents)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as L
import KontraLink
import Payments.PaymentsControl(readMoneyField,getPaymentChangeChange)
import MinutesTime
import System.Directory
import Data.List (isPrefixOf,sort)
import User.UserControl
import User.UserView
import Data.Maybe
import Redirect
import System.Process
import System.IO (hClose)
import qualified TrustWeaver as TW
import Data.Char
import Happstack.Util.Common


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


