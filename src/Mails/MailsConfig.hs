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
module Mails.MailsConfig(
             MailsConfig(..)
           , getMailsConfig
           , defaultMailConfig) where
             
import System.Log.Logger (Priority(..), logM)
import System.IO
-- | Configuration of mails
data MailsConfig = MailsConfig {
                      ourInfoEmail::String
                    , ourInfoEmailNiceName::String
                    , sendgridSMTP::String
                    , sendgridRestAPI::String
                    , sendgridUser::String
                    , sendgridPassword::String       
                    , sendMails::Bool
                   } deriving (Show, Read)
                   
{- | Getting configuration - in future wilk work with files.
     Setting production param can change default setting (not to send mails)
-}
getMailsConfig :: IO MailsConfig                  
getMailsConfig = do 
                  catch (  do
                            h <- openFile "mail.conf" ReadMode
                            hSetEncoding h utf8
                            c <- hGetContents h
                            conf <- readIO c
                            hClose h
                            logM "Happstack.Server" NOTICE "Mail config file read and parsed"  
                            return conf
                         )   (\_ ->  
                           do 
                             logM "Happstack.Server" NOTICE "No mail config provided. Falling back do default"  
                             return defaultMailConfig )
         


defaultMailConfig::MailsConfig                  
defaultMailConfig = MailsConfig {
                      ourInfoEmail = "mariusz@skrivapa.se"
                    , ourInfoEmailNiceName = "SkrivaPå"
                    , sendgridSMTP = "smtp://smtp.sendgrid.net"
                    , sendgridRestAPI = "https://sendgrid.com/api"
                    , sendgridUser= "duzyrak@gmail.com"
                    , sendgridPassword = "zimowisko"       
                    , sendMails = False 
                   } 