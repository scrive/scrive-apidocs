module ScriveByMail.Control (
    handleMailAPI
  , handleConfirmDelay
  ) where

import Company.Model
import CompanyAccounts.CompanyAccountsControl
import DB
import Doc.DocStateData
import Kontra
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as BSL

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
import Doc.Action

import Control.Applicative
import Control.Monad
import Data.Int
import Happstack.Server hiding (simpleHTTP, host)

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
    Just doc -> do
        _ <- postDocumentPreparationChange doc
        return $ show $ documentid doc
    Nothing -> return ""

handleConfirmDelay :: Kontrakcja m => String -> Int64 -> MagicHash -> m KontraLink
handleConfirmDelay adminemail delayid key = do
  ctx <- getContext
  mdelay <- dbQuery $ GetMailAPIUserRequest delayid key (ctxtime ctx)
  case mdelay of
    Nothing -> addFlashM $ modalDenyDelay
    Just (email, companyid) -> do
      company   <- guardJustM $ dbQuery $ GetCompany companyid
      adminuser <- guardJustM $ dbQuery $ GetUserByEmail Nothing (Email adminemail)
      unless (Just companyid == usercompany adminuser && useriscompanyadmin adminuser)
             internalError
      newuser   <- guardJustM $ createUser (Email email) "" "" (Just company)
      _ <- dbUpdate $ ConfirmBossDelay delayid (ctxtime ctx)
      _ <- sendNewCompanyUserMail adminuser company newuser
      addFlashM $ modalConfirmDelay email
  return $ LinkHome (ctxlocale ctx)
