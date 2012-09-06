module Stats.View
       (
         statisticsCompanyFieldsByDay,
         statisticsFieldsByDay,
         statisticsFieldsByMonth
       )
       where

import MinutesTime
import Templates.Templates
import Utils.Prelude
import qualified Templates.Fields as F

statisticsFieldsByDay :: Monad m => [(Int, [Int])] -> [Fields m ()]
statisticsFieldsByDay stats = for stats f
  where f (ct, c:s:i:u:_) = do
                F.value "date" $ showAsDate ct
                F.value "closed" c
                F.value "signatures" s
                F.value "sent" i
                F.value "users" u
                F.value "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double))
        f _ = error $ "statisticsFieldsByDay: bad stats"

statisticsFieldsByMonth :: Monad m => [(Int, [Int])] -> [Fields m ()]
statisticsFieldsByMonth stats = for stats f
  where f (ct, c:s:i:u:_) = do
                F.value "date" $ showAsMonth ct
                F.value "closed" c
                F.value "signatures" s
                F.value "sent" i
                F.value "users" u
                F.value "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double))
        f _ = error $ "statisticsFieldsByMonth: bad stats"


statisticsCompanyFieldsByDay :: Monad m => [(Int, String, [Int])] -> [Fields m ()]
statisticsCompanyFieldsByDay stats = for stats f
  where f (ct, u, c:s:i:_) = do
                F.value "date" $ showAsDate ct
                F.value "user" u
                F.value "istotal" (u == "Total")
                F.value "closed" c
                F.value "signatures" s
                F.value "sent" i
                F.value "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double))
        f _ = error $ "statisticsCompanyFieldsByDay: bad stats"
