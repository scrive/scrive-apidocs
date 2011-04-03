{-# OPTIONS_GHC -Wall -Werror #-}
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

module Mails.SendMail (
      Mail(..)
    , emptyMail
    , Mailer(..)
    , createSendgridMailer
    , createLocalOpenMailer
    , createSendmailMailer
    , MailInfo(..)
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL  hiding (length)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.UTF8 as BS hiding (length)
import System.Exit
import System.Directory
import System.Random
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.QuotedPrintable as QuotedPrintable
import Data.List
import Data.Typeable
import User.UserState 
import Doc.DocState
import Happstack.State
import Mails.MailsConfig
import Misc
import qualified AppLogger as Log

-- from simple utf-8 to =?UTF-8?Q?zzzzzzz?=
-- FIXME: should do better job at checking if encoding should be applied or not
mailEncode :: BS.ByteString -> String
mailEncode source = 
    "=?UTF-8?Q?" ++ QuotedPrintable.encode (BS.unpack source) ++ "?="

-- FIXME: use words/unword here
mailEncode1 :: String -> String
mailEncode1 source = mailEncode (BS.fromString source)

-- | Structure for holding mails. If from is not set mail will be send
-- as SkrivaPa admin (fromMails Config).
data Mail = Mail {
      fullnameemails :: [(BS.ByteString, BS.ByteString)] -- Fullname, Email
    , title          :: BS.ByteString
    , content        :: BS.ByteString
    , attachments    :: [(BS.ByteString, BS.ByteString)] -- list of attachments (name,content)
    , from           :: Maybe User
    , mailInfo       :: MailInfo 
    } deriving (Eq, Ord, Show, Typeable)

data MailInfo = Invitation DocumentID SignatoryLinkID
              | None
                deriving (Eq, Ord, Show, Read, Typeable)

$(deriveSerialize ''Mail)
instance Version Mail

$(deriveSerialize ''MailInfo)
instance Version MailInfo

emptyMail :: Mail
emptyMail = Mail {
      fullnameemails = []
    , title          = BS.empty
    , content        = BS.empty
    , attachments    = []
    , from           = Nothing
    , mailInfo       = None
}
-- 
newtype Mailer = Mailer { sendMail :: Mail -> IO Bool }

createSendgridMailer :: MailsConfig -> Mailer
createSendgridMailer config = Mailer{sendMail = reallySend}
    where
        reallySend mail@(Mail {fullnameemails, title, attachments}) = do
            mailId <- createMailId
            let mailtos = createMailTos mail
                wholeContent = createWholeContent (ourInfoEmail config) (ourInfoEmailNiceName config) mailId mail
                rcpt = concatMap (\(_,x) -> ["--mail-rcpt", "<" ++ BS.toString x ++ ">"]) fullnameemails
                curlargs =
                    [ "--user"
                    , (sendgridUser config) ++ ":" ++ (sendgridPassword config)
                    , (sendgridSMTP config)
                    , "-k", "--ssl", "--mail-from"
                    , "<" ++ (ourInfoEmail config) ++ ">"
                    ] ++ rcpt
            Log.mail $ "sending mail '" ++ BS.toString title ++ "' to " ++ show mailId ++ " " ++ concat (intersperse ", " mailtos)
            (code, _, bsstderr) <- readProcessWithExitCode' "./curl" curlargs wholeContent
            case code of
                 ExitFailure retcode -> do
                     Log.mail $ "Cannot execute ./curl to send emails, code " ++ show retcode ++ " stderr: " ++ BSL.toString bsstderr
                     return False
                 ExitSuccess -> do
                     Log.mail $ "Curl successfully executed with mail: " ++ (show $ mail {attachments = map (\(x,a) -> (x, BS.fromString ("length " ++ show (BS.length a)))) attachments})
                     return True

createSendmailMailer :: MailsConfig -> Mailer
createSendmailMailer config = Mailer{sendMail = reallySend}
    where
        reallySend mail@(Mail {title,attachments}) = do
            mailId <- createMailId
            let mailtos = createMailTos mail
                wholeContent = createWholeContent (ourInfoEmail config) (ourInfoEmailNiceName config) mailId mail
                sendmailargs =
                    [ "-t" -- get the addresses from the content
                    , "-i" -- ignore single dots in input
                    ]
            Log.mail $ "sending mail '" ++ BS.toString title ++ "' to " ++ show mailId ++ " " ++ concat (intersperse ", " mailtos)
            (code, _, bsstderr) <- readProcessWithExitCode' "sendmail" sendmailargs wholeContent
            case code of
                 ExitFailure retcode -> do
                     Log.mail $ "Cannot execute sendmail to send emails, code " ++ show retcode ++ " stderr: " ++ BSL.toString bsstderr
                     return False
                 ExitSuccess -> do
                     Log.mail $ "Successfully executed sendmail with mail: " ++ (show $ mail {attachments = map (\(x,a) -> (x,BS.fromString ("length " ++ show (BS.length a)))) attachments})
                     return True

createLocalOpenMailer :: String -> String -> Mailer
createLocalOpenMailer ourInfoEmail ourInfoEmailNiceName = Mailer{sendMail = sendToTempFile}
    where
        sendToTempFile mail@(Mail {fullnameemails}) = do
            tmp <- getTemporaryDirectory
            mailId <- createMailId
            uid <- randomRIO (1, 100000) :: IO Int
            let wholeContent = createWholeContent ourInfoEmail ourInfoEmailNiceName mailId mail
                mailtos = createMailTos mail
                filename = tmp ++ "/Email-" ++ BS.toString (snd $ head fullnameemails) ++ "-" ++ show uid ++ ".eml"
            Log.mail $ show mailId ++ " " ++ concat (intersperse ", " mailtos) ++ "  [staging: saved to file " ++ filename ++ "]"
            BSL.writeFile filename wholeContent
            openDocument filename
            return True

createMailTos :: Mail -> [String]
createMailTos (Mail {fullnameemails}) =
  map fmt fullnameemails 
  where fmt (fullname,email) = mailEncode fullname ++ " <" ++ BS.toString email ++ ">"

createMailId :: IO Integer
createMailId = randomIO 

createWholeContent :: String -> String -> Integer -> Mail -> BSLC.ByteString
createWholeContent ourInfoEmail ourInfoEmailNiceName mailId mail@(Mail {title,content,attachments,from,mailInfo}) =
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
          "X-SMTPAPI:  {\"unique_args\": " ++ 
                       "{\"mailinfo\": \""++(show mailInfo)++"\", "++
                       "\"id\": \""++(show mailId)++"\"} "++
                       
                      "}\r\n" ++
          "MIME-Version: 1.0\r\n" ++
          "Content-Type: multipart/mixed; boundary=" ++ boundary ++ "\r\n" ++
          "\r\n"
      header1 = "--" ++ boundary ++ "\r\n" ++
          "Content-type: text/html; charset=utf-8\r\n" ++
          "\r\n"
      footer = "\r\n--" ++ boundary ++ "--\r\n"
      aheader fname = "\r\n--" ++ boundary ++ "\r\n" ++
          "Content-Disposition: inline; filename=\"" ++ mailEncode fname ++".pdf\"\r\n" ++
          "Content-Type: application/pdf; name=\"" ++ mailEncode fname ++ ".pdf\"\r\n" ++
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
