module Stats.View 
       (
         statisticsCompanyFieldsForASingleUser,
         statisticsCSV,
         statisticsFieldsForASingleUser
       )
       where

import Control.Monad.Trans
import Templates.Templates
import Text.Printf
import Misc
import Stats.Model
import MinutesTime
import API.Service.Model

import Data.List
import qualified Data.ByteString.UTF8 as BS hiding (length)


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

statisticsCSV :: [DocStatEvent] -> String
statisticsCSV events = 
  "\"" ++ intercalate "\",\""  
  ["userid", "date", "event", "count", "docid", "serviceid", "companyid", "doctype"]
  ++ "\"\n" ++
  (concat $ map csvline events)
    where csvline event = "\"" ++ intercalate "\",\""
                          [ show $ seUserID event
                          , showDateYMD $ seTime event
                          , show $ seQuantity event
                          , show $ seAmount event
                          , show $ seDocumentID event
                          , maybe "" (BS.toString . unServiceID) $ seServiceID event
                          , maybe "" show $ seCompanyID event
                          , show $ seDocumentType event
                          ]
                          ++ "\"\n"

showAsDate :: Int -> String
showAsDate int = printf "%04d-%02d-%02d" (int `div` 10000) (int `div` 100 `mod` 100) (int `mod` 100)
