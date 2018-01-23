module SFTPConfig
    ( SFTPConfig(..) ) where

import Data.Unjson

data SFTPConfig =
    SFTPConfig {
      sftpUser      :: String
    , sftpPassword  :: String
    , sftpRemoteDir :: String
    , sftpHost      :: String
    } deriving (Eq, Show)

instance Unjson SFTPConfig where
  unjsonDef = objectOf $ SFTPConfig
    <$> field "user"
        sftpUser
        "SFTP user for the remote system"
    <*> field "password"
        sftpPassword
        "The password for the above specified user"
    <*> field "dir"
        sftpRemoteDir
        "Remote directory to use"
    <*> field "host"
        sftpHost
        "The SFTP host"
