module Payments.View where

--import Data.Int
--import Text.JSON
--import Text.JSON.Gen hiding (value)
--import qualified Text.JSON.Gen as J

--import MagicHash
--import Misc
--import OAuth.Model
import MinutesTime
import Mails.SendMail(Mail, kontramail)
import Company.Model
import Text.StringTemplates.Templates
import User.Model
import qualified Recurly as Recurly
import qualified Text.StringTemplates.Fields as F
import Util.HasSomeUserInfo
import Util.HasSomeCompanyInfo

import Control.Applicative

mailSignup :: (TemplatesMonad m) => String -> User -> Maybe Company -> Recurly.Subscription -> m Mail
mailSignup hp user mcompany subscription = do
  kontramail "paymentsSignupEmail" $ do
    F.value "ctxhostpart" hp
    F.value "fullname" $ getFullName user
    F.value "startdate" $ showDate $ Recurly.subCurrentBillingStarted subscription
    F.value "enddate" $ showDate $ Recurly.subCurrentBillingEnds subscription
    F.value "planname" $ Recurly.subName subscription
    F.value "quantity" $ show $ Recurly.subQuantity subscription
    F.value "total" $ showTotal (Recurly.subQuantity subscription) 
            (Recurly.subUnitAmountInCents subscription)
    F.value "currency" $ Recurly.subCurrency subscription
    if null $ getCompanyName (user, mcompany)
      then return ()
      else  F.value "companyname" $ getCompanyName (user, mcompany)
    F.value "email" $ getEmail user
    
mailFailed :: (TemplatesMonad m) => String -> User -> Maybe Company -> Recurly.Invoice -> m Mail
mailFailed hp user mcompany invoice = do
  kontramail "paymentsDeclinedEmail" $ do
    F.value "ctxhostpart" hp
    F.value "fullname" $ getFullName user
    if null $ getCompanyName (user, mcompany)
      then return ()
      else  F.value "companyname" $ getCompanyName (user, mcompany)
    F.value "email" $ getEmail user
    F.value "date" $ showDate $ Recurly.inDate invoice
    F.value "total" $ showTotal 1 $ Recurly.inTotalInCents invoice
    F.value "currency" $ Recurly.inCurrency invoice
    
mailExpired :: TemplatesMonad m => String -> m Mail
mailExpired hp = do
  kontramail "paymentsExpiredEmail" $ do
    F.value "ctxhostpart" hp

showTotal :: Int -> Int -> String
showTotal q tc = insertDecimal $ show (q * tc)
  where insertDecimal [] = undefined
        insertDecimal [x,y] = [',', x, y]
        insertDecimal (x:xs) = x : insertDecimal xs

showDate :: String -> String
showDate s = case showDateDMYYYY <$> parseMinutesTime "%Y-%m-%dT%H:%M:%S%Z" s of
  Nothing -> "unknown date"
  Just d -> d