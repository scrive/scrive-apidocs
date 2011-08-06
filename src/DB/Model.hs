module DB.Model where

import Database.HDBC

import DB.Classes

data TableValidationResult = TVRvalid | TVRcreated | TVRinvalid

data Table = Table {
    tblName             :: String
  , tblVersion          :: Int
  , tblCreateOrValidate :: [(String, SqlColDesc)] -> DB TableValidationResult
  , tblPutProperties    :: DB ()
  }

data Migration = Migration {
    mgrTable :: Table
  , mgrFrom  :: Int
  , mgrDo    :: DB ()
  }
