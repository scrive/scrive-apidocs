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

module Mails.SendMail(Mail(..),emptyMail,Mailer(..),createRealMailer,createDevMailer,MailInfo(..)) where
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
import User.UserState 
import Doc.DocState 
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
             } deriving Show
             
data MailInfo = Invitation SignatoryLinkID | None deriving (Show,Read)

emptyMail::Mail
emptyMail = Mail { fullnameemails=[], title= BS.empty, content = BS.empty, attachments = [], from = Nothing, mailInfo=None}    

newtype Mailer = Mailer {sendMail :: Mail -> IO ()}

createRealMailer :: MailsConfig -> Mailer
createRealMailer config = Mailer {sendMail = reallySend}
  where 
    reallySend mail@(Mail {fullnameemails,title,content,attachments,from,mailInfo}) = do
    let mailtos = createMailTos mail
        wholeContent = createWholeContent (ourInfoEmail config) (ourInfoEmailNiceName config) mail
    tm <- getCurrentTime
    logM "Kontrakcja.Mail" NOTICE $ formatTimeCombined tm ++ " " ++ concat (intersperse ", " mailtos)
    _ <- forkIO $ 
          do
            let rcpt = concatMap (\(_,x) -> ["--mail-rcpt", "<" ++ BS.toString x ++ ">"]) fullnameemails
            (code,_,_) <- readProcessWithExitCode' "./curl" ([ "--user"
                                    , (sendgridUser config) ++":"++(sendgridPassword config)
                                    , (sendgridSMTP config)
                                    , "-k", "--ssl", "--mail-from"
                                    , "<"++(ourInfoEmail config)++">"]++ rcpt) wholeContent
            if (code /= ExitSuccess) 
             then  logM "Kontrakcja.Mail" ERROR $ "Cannot execute ./curl to send emails"
             else  logM "Kontrakcja.Mail" ERROR $ "Curl executed with mail: " ++ (show $ mail {attachments = map (\(x,_)->(x,BS.empty) )attachments})
    return ()   

createDevMailer :: String -> String -> Mailer
createDevMailer ourInfoEmail ourInfoEmailNiceName = Mailer {sendMail = sendToTempFile}
  where 
    sendToTempFile mail@(Mail {fullnameemails}) = do
    tmp <- getTemporaryDirectory
    let wholeContent = createWholeContent ourInfoEmail ourInfoEmailNiceName mail
        mailtos = createMailTos mail
        filename = tmp ++ "/Email-" ++ BS.toString (snd (head fullnameemails)) ++ ".eml"
    tm <- getCurrentTime
    logM "Kontrakcja.Mail" NOTICE $ formatTimeCombined tm ++ " " ++ concat (intersperse ", " mailtos) ++ "  [staging: saved to file " ++ filename ++ "]"
    BSL.writeFile filename wholeContent
    openDocument filename
    return ()

createMailTos mail@(Mail {fullnameemails}) =
  map fmt fullnameemails 
  where fmt (fullname,email) = mailEncode fullname ++ " <" ++ BS.toString email ++ ">"

createWholeContent ourInfoEmail ourInfoEmailNiceName mail@(Mail {title,content,attachments,from,mailInfo}) =
  let mailtos = createMailTos mail 
      -- FIXME: add =?UTF8?B= everywhere it is needed here
      boundary = "skrivapa-mail-12-337331046" 
      fromHeader =  case from of 
                Nothing -> "From: " ++ mailEncode1 ourInfoEmailNiceName ++ " <" ++ ourInfoEmail ++ ">\r\n"
                Just user -> "From: " ++ (mailEncode1 $ BS.toString $ userfullname user) ++ " <"++(BS.toString $ unEmail $ useremail $ userinfo user )++ ">\r\n"
      header = 
          -- FIXME: encoded word should not be longer than 75 bytes including everything
          "Subject: " ++ mailEncode title ++ "\r\n" ++
          "To: " ++ concat (intersperse ", " mailtos) ++ "\r\n" ++
          fromHeader ++
          "X-SMTPAPI:  {\"unique_args\": {\"mailinfo\": \""++(show mailInfo)++"\"} }\r\n" ++
          "MIME-Version: 1.0\r\n" ++
          "Content-Type: multipart/mixed; boundary=" ++ boundary ++ "\r\n" ++
          "\r\n"
      header1 = "--" ++ boundary ++ "\r\n" ++
          "Content-type: text/html; charset=utf-8\r\n" ++
          "\r\n"
      footer = "\r\n--" ++ boundary ++ "--\r\n.\r\n"
      aheader fname = "\r\n--" ++ boundary ++ "\r\n" ++
          "Content-Disposition: inline; filename=\"" ++ (BS.toString fname) ++".pdf\"\r\n" ++
          "Content-Type: application/pdf; name=\"" ++ (BS.toString fname) ++ ".pdf\"\r\n" ++
          "Content-Transfer-Encoding: base64\r\n" ++
          "\r\n"
      attach (fname,fcontent) = BSL.fromString (aheader fname) `BSL.append` 
                                (BSLC.pack $ concat $ intersperse "\r\n" $ Base64.chop 72 $ Base64.encode $ BS.unpack fcontent)
      wholeContent = BSL.concat $ 
                     [ BSL.fromString header
                     , BSL.fromString header1
                     , BSL.fromChunks [content]
                     ] ++ map attach attachments ++
                     [ BSL.fromString footer
                     ]
  in wholeContent
