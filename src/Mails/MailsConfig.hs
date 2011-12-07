{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.MailsConfig
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Basic email sending configuration with SendGrid. Conf file in main dir with name mail.conf
-- We use Show/Read instances for parsing. We may have some problems with unicode on win machines,
-- but don't care about this for now
-----------------------------------------------------------------------------
module Mails.MailsConfig
    ( MailsConfig(..)
    , defaultMailConfig
    , isBackdoorOpen
    ) where

import Misc

-- | Configuration of mails
data MailsConfig
    = MailsSendgrid
      { mailbackdooropen         :: Bool
      , ourInfoEmail         :: String
      , ourInfoEmailNiceName :: String
      , sendgridSMTP         :: String
      , sendgridRestAPI      :: String
      , sendgridUser         :: String
      , sendgridPassword     :: String
      }
    | MailsSendmail
      { mailbackdooropen         :: Bool
      , ourInfoEmail         :: String
      , ourInfoEmailNiceName :: String
      }
    | MailsLocalOpen
      { mailbackdooropen         :: Bool
      , ourInfoEmail         :: String
      , ourInfoEmailNiceName :: String
      }
      deriving (Read, Eq, Ord)

instance Show MailsConfig where
    show (MailsSendgrid{..}) = indentLinesMore 2 $ unlines
      [ "MailsSendgrid"
      , "{ mailbackdooropen = " ++ show mailbackdooropen
      , ", ourInfoEmail = " ++ show ourInfoEmail
      , ", ourInfoEmailNiceName = " ++ show ourInfoEmailNiceName
      , ", sendgridSMTP = " ++ show sendgridSMTP
      , ", sendgridRestAPI = " ++ show sendgridRestAPI
      , ", sendgridUser = " ++ show sendgridUser
      , ", sendgridPassword = " ++ show sendgridPassword
      , "}"
      ]
    show (MailsSendmail{..}) = indentLinesMore 2 $ unlines
      [ "MailsSendmail"
      , "{ mailbackdooropen = " ++ show mailbackdooropen
      , ", ourInfoEmail = " ++ show ourInfoEmail
      , ", ourInfoEmailNiceName = " ++ show ourInfoEmailNiceName
      , "}"
      ]
    show (MailsLocalOpen{..}) = indentLinesMore 2 $ unlines
      [ "MailsLocalOpen"
      , "{ mailbackdooropen = " ++ show mailbackdooropen
      , ", ourInfoEmail = " ++ show ourInfoEmail
      , ", ourInfoEmailNiceName = " ++ show ourInfoEmailNiceName
      , "}"
      ]

isBackdoorOpen :: MailsConfig -> Bool
isBackdoorOpen MailsSendgrid{mailbackdooropen} = mailbackdooropen
isBackdoorOpen MailsSendmail{mailbackdooropen} = mailbackdooropen
isBackdoorOpen MailsLocalOpen{mailbackdooropen} = mailbackdooropen

defaultMailConfig :: MailsConfig
defaultMailConfig = MailsLocalOpen
      { mailbackdooropen          = False
      , ourInfoEmail          = "development-system@skrivapa.se"
      , ourInfoEmailNiceName  = "Development"
      }


{-
    MailsConfig { ourInfoEmail = "mariusz@skrivapa.se"
                , ourInfoEmailNiceName = "SkrivaPå"
                , sendgridSMTP = "smtp://smtp.sendgrid.net"
                , sendgridRestAPI = "https://sendgrid.com/api"
                , sendgridUser= "duzyrak@gmail.com"
                , sendgridPassword = "zimowisko"
                , sendMails = False
                }
-}
