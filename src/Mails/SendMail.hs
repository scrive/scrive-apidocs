{-# LANGUAGE ForeignFunctionInterface, CPP #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.SendMail
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Main module for sending mails .
-- Structure for holding mails + sending function
-----------------------------------------------------------------------------

module Mails.SendMail(Mail(..),emptyMail,sendMail,MailInfo(..)) where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL  hiding (length)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.UTF8 as BS
import System.Exit
import System.Directory
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.QuotedPrintable as QuotedPrintable
import Data.List
import System.Log.Logger
import Happstack.Util.LogFormat
import Data.Time.Clock
import Control.Concurrent (forkIO)
import Control.Monad
import UserState 
import DocState 
import Mails.MailsConfig
import Misc

-- from simple utf-8 to =?UTF-8?Q?zzzzzzz?=
-- FIXME: should do better job at checking if encoding should be applied or not
mailEncode :: BS.ByteString -> String
mailEncode source = 
    "=?UTF-8?Q?" ++ QuotedPrintable.encode (BS.unpack source) ++ "?="

mailEncode1 :: String -> String
mailEncode1 source = mailEncode (BS.fromString source)

-- | Structure for holding mails. If from is not set mail will be send as SkrivaPa admin (fromMails Config)
data Mail =  Mail {
               fullnameemails::[(BS.ByteString, BS.ByteString)], --Fullname, Email
               title::BS.ByteString,
               content::BS.ByteString,      
               attachments::[(BS.ByteString,BS.ByteString)], -- list of attachments (name,content). 
               from::Maybe User,
               mailInfo ::MailInfo 
             }
             
data MailInfo = Invitation SignatoryLinkID | None deriving (Show,Read)

emptyMail::Mail
emptyMail = Mail { fullnameemails=[], title= BS.empty, content = BS.empty, attachments = [], from = Nothing, mailInfo=None}    

{- | Sending mails using curl. 
     Important stuff:
       1) We log every mail in sendouts and mail.log
       2) If sendMails in MailsConfig is not set we only create local email file and try to open it.
 -}
sendMail::MailsConfig -> Mail->IO ()         
sendMail config (Mail {fullnameemails,title,content,attachments,from,mailInfo}) = do
  tm <- getCurrentTime
  let mailtos = map fmt fullnameemails 
      -- ("Gracjan Polak","gracjan@skrivapa.se") => "Gracjan Polak <gracjan@skrivapa.se>"
      fmt (fullname,email) = mailEncode fullname ++ " <" ++ BS.toString email ++ ">"
  logM "Kontrakcja.Mail" NOTICE $ formatTimeCombined tm ++ " " ++ concat (intersperse ", " mailtos)


  -- FIXME: add =?UTF8?B= everywhere it is needed here
  let boundary = "skrivapa-mail-12-337331046" 
  let fromHeader =  case from of 
                Nothing -> "From: " ++ mailEncode1 (ourInfoEmailNiceName config) ++ " <"++(ourInfoEmail config)++">\r\n"
                Just user -> "From: " ++ (mailEncode1 $ BS.toString $ userfullname user) ++ " <"++(BS.toString $ unEmail $ useremail $ userinfo user )++ ">\r\n"
  let header = 
          -- FIXME: encoded word should not be longer than 75 bytes including everything
          "Subject: " ++ mailEncode title ++ "\r\n" ++
          "To: " ++ concat (intersperse ", " mailtos) ++ "\r\n" ++
          fromHeader ++
          "X-SMTPAPI:  {\"unique_args\": {\"mailinfo\": \""++(show mailInfo)++"\"} }\r\n" ++
          "MIME-Version: 1.0\r\n" ++
          "Content-Type: multipart/mixed; boundary=" ++ boundary ++ "\r\n" ++
          "\r\n"
  let header1 = "--" ++ boundary ++ "\r\n" ++
          "Content-type: text/html; charset=utf-8\r\n" ++
          "\r\n"
  let footer = "\r\n--" ++ boundary ++ "--\r\n.\r\n"
  let aheader fname = "\r\n--" ++ boundary ++ "\r\n" ++
          "Content-Disposition: inline; filename=\"" ++ (BS.toString fname) ++".pdf\"\r\n" ++
          "Content-Type: application/pdf; name=\"" ++ (BS.toString fname) ++ ".pdf\"\r\n" ++
          "Content-Transfer-Encoding: base64\r\n" ++
          "\r\n"
  let attach (fname,fcontent) = BSL.fromString (aheader fname) `BSL.append` 
                                (BSLC.pack $ concat $ intersperse "\r\n" $ Base64.chop 72 $ Base64.encode $ BS.unpack fcontent)
  let wholeContent = BSL.concat $ 
                     [ BSL.fromString header
                     , BSL.fromString header1
                     , BSL.fromChunks [content]
                     ] ++ map attach attachments ++
                     [ BSL.fromString footer
                     ]
                     
  if (sendMails config)                   
   then do
         _ <- forkIO $ 
          do
            let rcpt = concatMap (\(_,x) -> ["--mail-rcpt", "<" ++ BS.toString x ++ ">"]) fullnameemails
            (code,_,_) <- readProcessWithExitCode' "./curl" ([ "--user"
                                                            , (sendgridUser config) ++":"++(sendgridPassword config)
                                                            , (sendgridSMTP config)
                                                            , "-k", "--ssl", "--mail-from"
                                                            , "<"++(ourInfoEmail config)++">"]++ rcpt) wholeContent
            when (code /= ExitSuccess) $
               logM "Kontrakcja.Mail" ERROR $ "Cannot execute ./curl to send emails"
         return ()   
   else
        do
           tmp <- getTemporaryDirectory
           let filename = tmp ++ "/Email-" ++ BS.toString (snd (head fullnameemails)) ++ ".eml"
           BSL.writeFile filename wholeContent
           openDocument filename
           return ()