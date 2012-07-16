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

import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Util.CSVUtil

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

statisticsCSV :: [DocStatEvent] -> BSL.ByteString
statisticsCSV events = renderCSV $
  ["userid", "date", "event", "count", "docid", "serviceid", "companyid", "doctype"] :
  (map csvline events)
    where csvline event = [ show $ seUserID event
                          , showDateYMD $ seTime event
                          , show $ seQuantity event
                          , show $ seAmount event
                          , show $ seDocumentID event
                          , maybe "" (BS.toString . unServiceID) $ seServiceID event
                          , maybe "" show $ seCompanyID event
                          , show $ seDocumentType event
                          ]

userStatisticsCSV :: [UserStatEvent] -> BSL.ByteString
userStatisticsCSV events = renderCSV $
  ["userid", "date", "event", "count", "serviceid", "companyid"] :
  (map csvline events)
    where csvline event = [ show                                 $ usUserID    event
                          , showDateYMD                          $ usTime      event
                          , show                                 $ usQuantity  event
                          , show                                 $ usAmount    event
                          , maybe "" (BS.toString . unServiceID) $ usServiceID event
                          , maybe "" show                        $ usCompanyID event
                          ]

signStatsCSV :: [SignStatEvent] -> BSL.ByteString
signStatsCSV events = renderCSV $
  ["documentid", "signatorylinkid", "date", "event", "doctype", "service (author)", "company (author)"] :
  (map csvline events)
    where csvline event = [ show        $ ssDocumentID      event
                          , show        $ ssSignatoryLinkID event
                          , showDateYMD $ ssTime            event
                          , show        $ ssQuantity        event
                          , show        $ ssDocumentProcess event
                          , show        $ ssServiceID       event
                          , show        $ ssCompanyID       event
                          ]

docHistCSV :: [[String]] -> BSL.ByteString
docHistCSV rows = renderCSV $
  ["documentid", "serviceid", "companyid", "doctype", "create", "send", "close", "reject", "cancel", "timeout"] :
  (rows)

signHistCSV :: [[String]] -> BSL.ByteString
signHistCSV rows = renderCSV $
  ["documentid", "signatoryid", "serviceid", "companyid", "doctype", "invite", "receive", "open", "link", "sign", "reject", "delete", "purge"] : rows
