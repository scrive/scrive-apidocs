{-# LANGUAGE CPP #-}
#ifndef DOCUMENTS_IN_POSTGRES
{-# OPTIONS_GHC -w #-}
#endif

module Doc.Import where

import Doc.Model
import qualified Doc.DocState as Old
import qualified Data.ByteString.UTF8 as BS
import Happstack.State
import qualified AppLogger as Log
import Database.HDBC
import Database.HDBC.PostgreSQL
import DB.Classes
import Doc.DocStateData

populateDBWithDocumentsIfEmpty :: Connection -> IO ()
populateDBWithDocumentsIfEmpty conn = do
#ifdef DOCUMENTS_IN_POSTGRES
  ioRunDB conn $ do
    mrow <- wrapDB $ \conn' -> do
              st <- prepare conn' $ "SELECT * FROM documents LIMIT 1"
              _ <- execute st []
              fetchRow st
    case mrow of
      Just _ -> Log.server "Not importing any documents as database contains some already"
      Nothing -> do
                -- get documents from Happstack state
                alldocuments <- query $ Old.GetAllDocuments
                let l = length alldocuments
                Log.server $ "Moving " ++ show l ++ " documents from Happstack.State into PostgreSQL"
                let imp :: (Int,Document) -> DB ()
                    imp (ind,doc) = do
                      Log.server $ "Moving document " ++ show ind ++ "/" ++ show l ++ " " ++ BS.toString (documenttitle doc)
                      _ <- insertDocumentAsIs doc
                      return ()
                mapM_ imp (zip [1..] alldocuments)
                Log.server $ "All " ++ show l ++ " documents imported into PostgreSQL"
#else
  return ()
#endif
