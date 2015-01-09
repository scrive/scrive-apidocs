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
import Mails.SendMail(Mail, kontramail)
import MinutesTime
import Theme.Model
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

mailSignup :: (TemplatesMonad m) => BrandedDomain -> Theme -> String -> User -> Company -> Recurly.Subscription -> m Mail
mailSignup bd theme hp user company subscription = do
  kontramail bd theme "paymentsSignupEmail" $ do
    F.value "ctxhostpart" hp
    F.value "fullname" $ getFullName user
    F.value "startdate" $ showDate $ Recurly.subCurrentBillingStarted subscription
    F.value "enddate" $ showDate $ Recurly.subCurrentBillingEnds subscription
    F.value "planname" $ Recurly.subName subscription
    F.value "quantity" $ show $ Recurly.subQuantity subscription
    F.value "total" $ showTotal (Recurly.subQuantity subscription)
            (Recurly.subUnitAmountInCents subscription)
    F.value "totalwithvat" $ showTotal (Recurly.subQuantity subscription)
         amountInCentsWithVat
    F.value "currency" $ Recurly.subCurrency subscription
    when (not $ null $ getCompanyName company) $ do
      F.value "companyname" $ getCompanyName company
    F.value "email" $ getEmail user
  where amountInCentsWithVat = round $ (fromIntegral $ Recurly.subUnitAmountInCents subscription) * 1.25

mailFailed :: (TemplatesMonad m) => BrandedDomain -> Theme ->  String -> User -> Company -> Recurly.Invoice -> m Mail
mailFailed bd theme hp user company invoice = do
  kontramail bd theme "paymentsDeclinedEmail" $ do
    F.value "ctxhostpart" hp
    F.value "fullname" $ getFullName user
    when (not $ null $ getCompanyName company) $ do
      F.value "companyname" $ getCompanyName company
    F.value "email" $ getEmail user
    F.value "date" $ showDate $ Recurly.inDate invoice
    F.value "total" $ showTotal 1 $ Recurly.inTotalInCents invoice
    F.value "currency" $ Recurly.inCurrency invoice

mailExpired :: TemplatesMonad m => BrandedDomain -> Theme ->  String -> m Mail
mailExpired bd theme hp = do
  kontramail bd theme "paymentsExpiredEmail" $ do
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
