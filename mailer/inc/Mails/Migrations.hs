module Mails.Migrations (
    mailerMigrations
  ) where

import DB
import DB.SQL2
import Mails.Tables
import Control.Monad
import Text.JSON
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS
import Control.Applicative
import Data.Int
import qualified Log as Log

-- Note: ALWAYS append new migrations TO THE END of this list.
mailerMigrations :: MonadDB m => [Migration m]
mailerMigrations = [
    addTestServiceToMails
   , moveAtachmentsToSeparateTable
  ]

addTestServiceToMails :: MonadDB m => Migration m
addTestServiceToMails =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 1
  , mgrDo = do
      kRunRaw "ALTER TABLE mails ADD COLUMN service_test BOOL"
      _ <- kRun $ SQL "UPDATE mails SET service_test = ?" [toSql False]
      kRunRaw "ALTER TABLE mails ALTER COLUMN service_test SET NOT NULL"
  }

moveAtachmentsToSeparateTable :: MonadDB m => Migration m
moveAtachmentsToSeparateTable =
  Migration {
    mgrTable = tableMails
  , mgrFrom = 2
  , mgrDo = do

      -- we have to do this one by one because emails are big and we
      -- might run out of memory
      let loop n = do
            kRun_ $ sqlSelect "mails" $ do
                 sqlResult "mails.id"
                 sqlResult "attachments"
                 sqlWhere "attachments IS NOT NULL"
                 sqlWhere "attachments <> '[]'"
                 sqlLimit 10
            let decoder acc mid json = (mid,json) : acc
            movable <- foldDB decoder []
            kRun_ $ sqlUpdate "mails" $ do
                 sqlSet "attachments" SqlNull
                 sqlWhereIn "id" (map fst movable)
            Log.debug $ "Moving attachments: " ++ show (n + length movable)
            forM_ movable $ \(mid,jsontext) -> do
                   --Log.debug $ "JSON: " ++ jsontext
                   let Ok (JSArray values) = decode jsontext
                   let lookup' name (JSObject obj) =
                         case lookup name (fromJSObject obj) of
                           Just (JSString val) -> fromJSString val
                           _ -> error $ "Cannot convert"
                       lookup' _name _ = error "Cannot convert"
                   let (names :: [String]) = map (lookup' "attName") values
                   let (contents :: [String]) = map (lookup' "attContent") values
                   when (not (null values)) $ do
                     kRun_ $ sqlInsert "mail_attachments" $ do
                       sqlSet "mail_id" (mid :: Int64)
                       sqlSetList "name" names
                       sqlSetList "content" ((Binary . B64.decodeLenient . BS.pack) <$> contents)
            if not (null movable)
              then loop (n + length movable)
              else return ()
      loop 0

      kRunRaw "ALTER TABLE mails DROP COLUMN attachments"
      error "For now retry!"
  }
