module Stats.View
       (
         statisticsCompanyFields,
         statisticsFields
       )
       where

import MinutesTime
import qualified Text.StringTemplates.Fields as F
import User.Model
import Control.Applicative

statisticsFields :: Monad m => (MinutesTime -> String) -> [UserUsageStats] -> [F.Fields m ()]
statisticsFields formatTime = map f
  where f uus = do
                F.value "date" $ formatTime (fst $ uusTimeSpan uus)
                F.value "closed" (uusDocumentsClosed uus)
                F.value "signatures" (uusSignaturesClosed uus)
                F.value "sent" (uusDocumentsSent uus)

statisticsCompanyFields :: Monad m => (MinutesTime -> String) -> [UserUsageStats] -> [F.Fields m ()]
statisticsCompanyFields formatTime = map f
  where f uus = do
                F.value "date" $ formatTime (fst $ uusTimeSpan uus)
                F.value "user" $ ((\(_,_,n) -> n) <$> uusUser uus)
                F.value "istotal" False -- FIMXE: need totals...
                F.value "closed" $ uusDocumentsClosed uus
                F.value "signatures" $ uusSignaturesClosed uus
                F.value "sent" $ uusDocumentsSent uus
