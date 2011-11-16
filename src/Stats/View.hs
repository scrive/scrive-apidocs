module Stats.View 
       (
         statisticsCompanyFieldsByDay,
         statisticsCSV,
         userStatisticsCSV,
         statisticsFieldsByDay,
         statisticsFieldsByMonth         
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


statisticsFieldsByDay :: (Functor m, MonadIO m) => [(Int, [Int])] -> [Fields m]
statisticsFieldsByDay stats = for stats f
  where f (ct, c:s:i:u:_) = do
                field "date" $ showAsDate ct
                field "closed" c
                field "signatures" s
                field "sent" i
                field "users" u
                field "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double))
        f _ = error $ "statisticsFieldsByDay: bad stats: "++show stats

statisticsFieldsByMonth :: (Functor m, MonadIO m) => [(Int, [Int])] -> [Fields m]
statisticsFieldsByMonth stats = for stats f
  where f (ct, c:s:i:u:_) = do
                field "date" $ showAsMonth ct
                field "closed" c
                field "signatures" s
                field "sent" i
                field "users" u
                field "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double))
        f _ = error $ "statisticsFieldsByMonth: bad stats: "++show stats


statisticsCompanyFieldsByDay :: (Functor m, MonadIO m) => [(Int, String, [Int])] -> [Fields m]
statisticsCompanyFieldsByDay stats = for stats f
  where f (ct, u, c:s:i:_) = do
                field "date" $ showAsDate ct
                field "user" u
                field "istotal" (u == "Total")
                field "closed" c
                field "signatures" s
                field "sent" i
                field "avg" (if c == 0 then 0 else ((fromIntegral s / fromIntegral c) :: Double))
        f _ = error $ "statisticsCompanyFieldsByDay: bad stats: "++show stats

statisticsCSV :: [DocStatEvent] -> String
statisticsCSV events = 
  "\"" ++ intercalate "\";\""  
  ["userid", "date", "event", "count", "docid", "serviceid", "companyid", "doctype"]
  ++ "\"\n" ++
  (concat $ map csvline events)
    where csvline event = "\"" ++ intercalate "\";\""
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

userStatisticsCSV :: [UserStatEvent] -> String
userStatisticsCSV events = 
  "\"" ++ intercalate "\";\""  
  ["userid", "date", "event", "count", "serviceid", "companyid"]
  ++ "\"\n" ++
  (concat $ map csvline events)
    where csvline event = "\"" ++ intercalate "\";\""
                          [ show                                 $ usUserID    event                                    
                          , showDateYMD                          $ usTime      event                               
                          , show                                 $ usQuantity  event                                  
                          , show                                 $ usAmount    event                                    
                          , maybe "" (BS.toString . unServiceID) $ usServiceID event 
                          , maybe "" show                        $ usCompanyID event                        
                          ]
                          ++ "\"\n"


showAsDate :: Int -> String
showAsDate int = printf "%04d-%02d-%02d" (int `div` 10000) (int `div` 100 `mod` 100) (int `mod` 100)

showAsMonth :: Int -> String
showAsMonth int = printf "%04d-%02d" (int `div` 10000) (int `div` 100 `mod` 100)
