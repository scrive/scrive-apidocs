module Payments.View where

--import Data.Int
--import Text.JSON
--import Text.JSON.Gen hiding (value)
--import qualified Text.JSON.Gen as J

--import MagicHash
--import Misc
--import OAuth.Model
import Control.Monad
import Text.StringTemplates.Templates
import qualified Recurly as Recurly
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import Company.Model
import Mails.MailsConfig
import Mails.SendMail(Mail, kontramail)
import MinutesTime
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

mailSignup :: (TemplatesMonad m) => MailsConfig -> Maybe BrandedDomain -> String -> User -> Company -> Recurly.Subscription -> m Mail
mailSignup mc mbd hp user company subscription = do
  kontramail mc mbd "paymentsSignupEmail" $ do
    F.value "ctxhostpart" hp
    F.value "fullname" $ getFullName user
    F.value "startdate" $ showDate $ Recurly.subCurrentBillingStarted subscription
    F.value "enddate" $ showDate $ Recurly.subCurrentBillingEnds subscription
    F.value "planname" $ Recurly.subName subscription
    F.value "quantity" $ show $ Recurly.subQuantity subscription
    F.value "total" $ showTotal (Recurly.subQuantity subscription)
            (Recurly.subUnitAmountInCents subscription)
    F.value "currency" $ Recurly.subCurrency subscription
    when (not $ null $ getCompanyName company) $ do
      F.value "companyname" $ getCompanyName company
    F.value "email" $ getEmail user

mailFailed :: (TemplatesMonad m) => MailsConfig -> Maybe BrandedDomain ->  String -> User -> Company -> Recurly.Invoice -> m Mail
mailFailed mc mbd hp user company invoice = do
  kontramail mc mbd "paymentsDeclinedEmail" $ do
    F.value "ctxhostpart" hp
    F.value "fullname" $ getFullName user
    when (not $ null $ getCompanyName company) $ do
      F.value "companyname" $ getCompanyName company
    F.value "email" $ getEmail user
    F.value "date" $ showDate $ Recurly.inDate invoice
    F.value "total" $ showTotal 1 $ Recurly.inTotalInCents invoice
    F.value "currency" $ Recurly.inCurrency invoice

mailExpired :: TemplatesMonad m => MailsConfig -> Maybe BrandedDomain ->  String -> m Mail
mailExpired mc mbd hp = do
  kontramail mc mbd "paymentsExpiredEmail" $ do
    F.value "ctxhostpart" hp

showTotal :: Int -> Int -> String
showTotal q tc = insertDecimal $ show (q * tc)
  where insertDecimal [] = undefined
        insertDecimal [x,y] = [',', x, y]
        insertDecimal (x:xs) = x : insertDecimal xs

showDate :: String -> String
showDate s = case parseTime' "%Y-%m-%dT%H:%M:%S%Z" s of
  Nothing -> "unknown date"
  Just d -> formatTimeYMD d
