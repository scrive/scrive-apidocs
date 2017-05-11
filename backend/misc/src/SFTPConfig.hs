module SFTPConfig
    ( SFTPConfig(..) ) where

import Data.Unjson

import KontraPrelude

data SFTPConfig =
    SFTPConfig {
      sftpUser      :: String
    , sftpRemoteDir :: String
    , sftpHost      :: String
    , sftpKeyPath   :: String
    } deriving (Eq, Show)

instance Unjson SFTPConfig where
  unjsonDef = objectOf $ SFTPConfig
    <$> field "user"
        sftpUser
        "SFTP user for the remote system"
    <*> field "dir"
        sftpRemoteDir
        "Remote directory to use"
    <*> field "host"
        sftpHost
        "The SFTP host"
    <*> field "key"
        sftpKeyPath
        "The key for the above specified user"
