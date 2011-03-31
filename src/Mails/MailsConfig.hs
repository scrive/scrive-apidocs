{-# OPTIONS_GHC -Wall #-}
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
    ) where
             
-- | Configuration of mails
data MailsConfig 
    = MailsSendgrid
      { ourInfoEmail         :: String
      , ourInfoEmailNiceName :: String
      , sendgridSMTP         :: String
      , sendgridRestAPI      :: String
      , sendgridUser         :: String
      , sendgridPassword     :: String       
      } 
    | MailsSendmail
      { ourInfoEmail         :: String
      , ourInfoEmailNiceName :: String
      } 
    | MailsLocalOpen
      { ourInfoEmail         :: String
      , ourInfoEmailNiceName :: String
      } 
      deriving (Show, Read, Eq, Ord)
                   


defaultMailConfig :: MailsConfig                  
defaultMailConfig = MailsLocalOpen
      { ourInfoEmail          = "development-system@skrivapa.se"
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