module ScriveByMail.Control 
       (

         sendMailAPIConfirmEmail,
         sendMailAPIErrorEmail,
         sendMailAPIDelayAdminEmail,
         sendMailAPIDelayUserEmail,
         markDocumentAuthorReadAndSeen,
         handleMailAPI,
         parseEmailMessageToParts,
         charset,
         handleConfirmDelay,
         doMailAPI
       )
       
       where

import Company.Model
import DB.Classes
import Doc.DocStateData
import Doc.DocStorage
import Doc.DocUtils
import Doc.JSON
import Doc.Model
import EvidenceLog.Model
import File.Model
import Kontra
import KontraLink
import MagicHash
import Mails.SendMail
import MinutesTime
import Misc
import Util.MonadUtils
import Redirect
import ScriveByMail.Model
import ScriveByMail.Parse
import ScriveByMail.View
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Doc.DocControl as DocControl
import qualified Log (scrivebymail, scrivebymailfailure, mailAPI, jsonMailAPI)

import Codec.MIME.Decode
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Either
import Data.Int
import Data.List
import Data.Maybe
import Data.String.Utils
import Happstack.Server hiding (simpleHTTP, host)
import Text.JSON
import Text.JSON.String
import qualified Codec.MIME.Parse as MIME
import qualified Codec.MIME.Type as MIME
import qualified Codec.Text.IConv as IConv
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS

checkThat :: String -> Bool -> Maybe String
checkThat s b = Nothing <| b |> Just s

charset :: MIME.Type -> String
charset mimetype = fromMaybe "us-ascii" $ lookup "charset" (MIME.mimeParams mimetype)        

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
  return $ show (documentid doc)

handleConfirmDelay :: Kontrakcja m => String -> Int64 -> MagicHash -> m String
handleConfirmDelay adminemail delayid key = do
  ctx <- getContext
  mdelay <- runDBQuery $ GetMailAPIDelay delayid key (ctxtime ctx)
  case mdelay of
    Nothing -> return "This confirmation link has expired."
    Just (email, companyid, _) -> do
      company   <- guardJustM $ runDBQuery $ GetCompany companyid
      adminuser <- guardJustM $ runDBQuery $ GetUserByEmail Nothing (Email $ BS.fromString adminemail)
      guard (Just companyid == usercompany adminuser && useriscompanyadmin adminuser)
      newuser   <- guardJustM $ createUser (Email $ BS.fromString email) BS.empty BS.empty (Just company)
      _ <- runDBUpdate $ ConfirmBossDelay delayid (ctxtime ctx)
      _ <- sendNewCompanyUserMail adminuser company newuser
      return $ "An account for your company has been created for " ++ email ++ ". Thank you."
      