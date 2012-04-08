module Stats.View 
       (
         statisticsCompanyFieldsByDay,
         statisticsCSV,
         userStatisticsCSV,
         statisticsFieldsByDay,
         statisticsFieldsByMonth,
         
         signStatsCSV,
         
         docHistCSV,
         signHistCSV
         
       )
       where

import Misc
import Stats.Model
import MinutesTime
import API.Service.Model
import Templates.Templates
import qualified Templates.Fields as F

import Data.List
import qualified Data.ByteString.UTF8 as BS hiding (length)


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

signStatsCSV :: [SignStatEvent] -> String
signStatsCSV events = 
  "\"" ++ intercalate "\";\""  
  ["documentid", "signatorylinkid", "date", "event", "doctype", "service (author)", "company (author)"]
  ++ "\"\n" ++
  (concat $ map csvline events)
    where csvline event = "\"" ++ intercalate "\";\""
                          [ show        $ ssDocumentID      event 
                          , show        $ ssSignatoryLinkID event                               
                          , showDateYMD $ ssTime            event     
                          , show        $ ssQuantity        event
                          , show        $ ssDocumentProcess event
                          , show        $ ssServiceID       event
                          , show        $ ssCompanyID       event
                          ]
                          ++ "\"\n"

docHistCSV :: [[String]] -> String
docHistCSV rows = 
  "\"" ++ intercalate "\";\""  
  ["documentid", "serviceid", "companyid", "doctype", "create", "send", "close", "reject", "cancel", "timeout"]
  ++ "\"\n" ++ 
  (concat $ map csvline rows)
    where csvline row = "\"" ++ intercalate "\";\"" row ++ "\"\n"

signHistCSV :: [[String]] -> String
signHistCSV rows = 
  "\"" ++ intercalate "\";\""  
  ["documentid", "signatoryid", "serviceid", "companyid", "doctype", "invite", "receive", "open", "link", "sign", "reject", "delete", "purge"]
  ++ "\"\n" ++ 
  (concat $ map csvline rows)
    where csvline row = "\"" ++ intercalate "\";\"" row ++ "\"\n"


