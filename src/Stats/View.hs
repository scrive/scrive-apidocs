module Stats.View 
       (
         statisticsCompanyFieldsForASingleUser,
         statisticsFieldsForASingleUser
       )
       where

import Control.Monad.Trans
import Templates.Templates
import Text.Printf
import Misc

statisticsFieldsForASingleUser :: (Functor m, MonadIO m) => [(Int, Int, Int, Int)] -> Fields m
statisticsFieldsForASingleUser stats = 
  fieldFL "statistics" $ for stats (\(ct, c, s, i) -> do
                                       field "date" $ showAsDate ct
                                       field "closed" c
                                       field "signatures" s
                                       field "sent" i
                                       field "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double)))

statisticsCompanyFieldsForASingleUser :: (Functor m, MonadIO m) => [(Int, String, Int, Int, Int)] -> Fields m
statisticsCompanyFieldsForASingleUser stats = 
  fieldFL "statistics" $ for stats (\(ct, u, c, s, i) -> do
                                       field "date" $ showAsDate ct
                                       field "user" u
                                       field "istotal" (u == "Total")
                                       field "closed" c
                                       field "signatures" s
                                       field "sent" i
                                       field "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double)))

showAsDate :: Int -> String
showAsDate int = printf "%04d-%02d-%02d" (int `div` 10000) (int `div` 100 `mod` 100) (int `mod` 100)
