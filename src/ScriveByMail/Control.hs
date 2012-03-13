module ScriveByMail.Control 
       (

         handleMailAPI,
         handleConfirmDelay
       )
       
       where

import Company.Model
import CompanyAccounts.CompanyAccountsControl
import DB.Classes
import Doc.DocStateData
import FlashMessage
import Kontra
import KontraLink
import MagicHash
import Misc
import ScriveByMail.Action
import ScriveByMail.Model
import ScriveByMail.View
import User.Model
import User.UserControl
import Util.FlashUtil
import Util.MonadUtils
import qualified Doc.DocControl as DocControl

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Int
import Happstack.Server hiding (simpleHTTP, host)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS

{- |

   The handler for the mail api. This is where emails get posted to.

-}
handleMailAPI :: Kontrakcja m => m String
handleMailAPI = do
  Input contentspec _ _ <- getDataFnM (lookInput "mail")
  content <- concatChunks <$> case contentspec of
    Left filepath -> liftIO $ BSL.readFile filepath
    Right content -> return content
  mresult <- doMailAPI content
  case mresult of
    Just (doc2, doc, msiglinkid) -> do
        _ <- DocControl.postDocumentChangeAction doc2 doc msiglinkid
        return $ show $ documentid doc
    Nothing -> return ""

handleConfirmDelay :: Kontrakcja m => String -> Int64 -> MagicHash -> m KontraLink
handleConfirmDelay adminemail delayid key = do
  ctx <- getContext
  mdelay <- runDBQuery $ GetMailAPIUserRequest delayid key (ctxtime ctx)
  case mdelay of
    Nothing -> addFlashM $ modalDenyDelay
    Just (email, companyid) -> do
      company   <- guardJustM $ runDBQuery $ GetCompany companyid
      adminuser <- guardJustM $ runDBQuery $ GetUserByEmail Nothing (Email $ BS.fromString adminemail)
      guard (Just companyid == usercompany adminuser && useriscompanyadmin adminuser)
      newuser   <- guardJustM $ createUser (Email $ BS.fromString email) BS.empty BS.empty (Just company)
      _ <- runDBUpdate $ ConfirmBossDelay delayid (ctxtime ctx)
      _ <- sendNewCompanyUserMail adminuser company newuser
      addFlashM $ modalConfirmDelay email
  return $ LinkHome (ctxlocale ctx)
      