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
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSL  hiding (length)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.UTF8 as BS hiding (length)
import System.Exit
import System.Directory
import System.Random
import qualified Data.ByteString.Base64 as Base64
import qualified Codec.Binary.QuotedPrintable as QuotedPrintable
import Control.Applicative
import Control.Arrow (first)
import Data.Char (isSpace, toLower)
import Data.List
import Data.Typeable
import User.UserState 
import Doc.DocState
import Happstack.State
import Mails.MailsConfig
import Text.HTML.TagSoup
import Misc
import qualified AppLogger as Log
import Data.ByteString.Internal (c2w,w2c)

-- from simple utf-8 to =?UTF-8?Q?zzzzzzz?=
mailEncode :: BS.ByteString -> String
mailEncode source = unwords (map encodeWord w)
    where
        -- here we really want to use latin1 encoding to treat chars as bytes
        -- the QuotedPrintable encoder treats chars as bytes
        -- overall this is UTF-8 encoded
        --
        -- also: this removes spaces at front, spaces at the end, double spaces
        -- and (important!) converts \r and \n to space.
        --
        -- please keep these conversions as the are security measure
        w = words (map w2c $ BS.unpack source)
        wordNeedsEncoding word = any charNeedsEncoding word
        -- FIXME: needs to check if this is full list of exceptions
        charNeedsEncoding char = char <= ' ' || char >= '\x7f' || char == '='
        encodeWord wa | wordNeedsEncoding wa = "=?UTF-8?Q?" ++ QuotedPrintable.encode (map c2w wa) ++ "?="
                      | otherwise = wa

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
            boundaries <- createBoundaries
            let mailtos = createMailTos mail
                wholeContent = createWholeContent boundaries (ourInfoEmail config) (ourInfoEmailNiceName config) mailId mail
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
            boundaries <- createBoundaries
            let mailtos = createMailTos mail
                wholeContent = createWholeContent boundaries (ourInfoEmail config) (ourInfoEmailNiceName config) mailId mail
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
            boundaries <- createBoundaries
            uid <- randomRIO (1, 100000) :: IO Int
            let wholeContent = createWholeContent boundaries ourInfoEmail ourInfoEmailNiceName mailId mail
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

createBoundaries :: IO (String, String)
createBoundaries = (,) <$> f <*> f
    where
        f = randomString 32 $ ['0'..'9'] ++ ['a'..'z']

createWholeContent :: (String, String) -> String -> String -> Integer -> Mail -> BSLC.ByteString
createWholeContent (boundaryMixed, boundaryAlternative) ourInfoEmail ourInfoEmailNiceName mailId mail@(Mail {title,content,attachments,from,mailInfo}) =
  let mailtos = createMailTos mail 
      -- FIXME: add =?UTF8?B= everywhere it is needed here
      fromHeader =  case from of 
                Nothing -> "From: " ++ mailEncode1 ourInfoEmailNiceName ++ " <" ++ ourInfoEmail ++ ">\r\n"
                Just user -> "From: " ++ (mailEncode1 $ BS.toString $ userfullname user) ++ " <"++(BS.toString $ unEmail $ useremail $ userinfo user )++ ">\r\n"
      headerEmail = 
          -- FIXME: encoded word should not be longer than 75 bytes including everything
          "Subject: " ++ mailEncode title ++ "\r\n" ++
          "To: " ++ concat (intersperse ", " mailtos) ++ "\r\n" ++
          fromHeader ++
          "X-SMTPAPI:  {\"unique_args\": " ++ 
                       "{\"mailinfo\": \""++(show mailInfo)++"\", "++
                       "\"id\": \""++(show mailId)++"\"} "++
                       
                      "}\r\n" ++
          "MIME-Version: 1.0\r\n" ++
          "Content-Type: multipart/mixed; boundary=" ++ boundaryMixed ++ "\r\n" ++
          "\r\n"
      headerContent = "--" ++ boundaryMixed ++ "\r\n" ++
          "Content-Type: multipart/alternative; boundary=" ++ boundaryAlternative ++ "\r\n" ++ "\r\n"
      headerContentText = "--" ++ boundaryAlternative ++ "\r\n" ++
          "Content-type: text/plain; charset=utf-8\r\n" ++
          "\r\n"
      headerContentHtml = "--" ++ boundaryAlternative ++ "\r\n" ++
          "Content-type: text/html; charset=utf-8\r\n" ++
          "\r\n"
      footerContent = "\r\n--" ++ boundaryAlternative ++ "--\r\n"
      footerEmail = "\r\n--" ++ boundaryMixed ++ "--\r\n"
      headerAttach fname = "\r\n--" ++ boundaryMixed ++ "\r\n" ++
          "Content-Disposition: inline; filename=\"" ++ mailEncode fname ++".pdf\"\r\n" ++
          "Content-Type: application/pdf; name=\"" ++ mailEncode fname ++ ".pdf\"\r\n" ++
          "Content-Transfer-Encoding: base64\r\n" ++
          "\r\n"
      attach (fname,fcontent) = BSL.fromString (headerAttach fname) `BSL.append` 
                                BSL.fromChunks [Base64.joinWith (BSC.pack "\r\n") 72 $ Base64.encode fcontent]
      wholeContent = BSL.concat $ 
                     [ BSL.fromString headerEmail
                     , BSL.fromString headerContent
                     , BSL.fromString headerContentText
                     , BSL.fromString $ htmlToTxt $ BS.toString content
                     , BSL.fromString headerContentHtml
                     , BSL.fromChunks [content]
                     , BSL.fromString footerContent
                     ] ++ map attach attachments ++
                     [ BSL.fromString footerEmail
                     ]
  in wholeContent

-- | Convert e-mail from html to txt
htmlToTxt :: String -> String
htmlToTxt = dropWhile isSpace . toText . removeWSAfterNL . reduceWS .
    replaceLinks . concatTexts . filterUnneeded . lowerTags . parseTags
    where
        toText = concat . foldr (\t ts -> f t : ts) []
            where
                f (TagText s)   = s
                f (TagOpen t _) = fromTag t
                f (TagClose t)  = fromTag t
                f _             = ""

                fromTag t =
                    case t of
                        "b"      -> "*"
                        "br"     -> "\r\n"
                        "i"      -> "_"
                        "p"      -> "\r\n"
                        "strong" -> "*"
                        _        -> ""

        replaceLinks [] = []
        replaceLinks (t@(TagOpen "a" attrs):ts) =
            case "href" `lookup` attrs of
                 Just link -> TagText link : replaceLinks (drop 1 $ dropWhile (/= (TagClose "a")) ts)
                 Nothing   -> t : replaceLinks ts
        replaceLinks (t:ts) = t : replaceLinks ts

        removeWSAfterNL = foldr f []
            where
                f t ts@((TagText s):ts') =
                    case t of
                         TagOpen el _ -> g el
                         TagClose el  -> g el
                         _            -> t : ts
                    where
                        g el =
                            if el `elem` ["p", "br"]
                               then t : TagText (dropWhile isSpace s) : ts'
                               else t : ts
                f t ts = t : ts

        reduceWS = map f
            where
                f (TagText s) = TagText $ omitWS s
                f t = t

                omitWS [] = []
                omitWS (x:xs) =
                    if isSpace x
                       then ' ' : omitWS (dropWhile isSpace xs)
                       else x   : omitWS xs

        concatTexts = foldr f []
            where
                f (TagText s) ((TagText s'):ts) = TagText (s ++ s') : ts
                f t ts = t : ts

        filterUnneeded = filter f
            where
                f (TagText s)   = not $ all isSpace s
                f (TagOpen t _) = t `elem` ["a", "b", "i", "p", "strong"]
                f (TagClose t)  = t `elem` ["a", "b", "br", "i", "p", "strong"]
                f _             = False

        lowerTags = map f
            where
                f (TagOpen t attrs) = TagOpen (lowerCase t) $ map (first lowerCase) attrs
                f (TagClose t) = TagClose (lowerCase t)
                f t = t

                lowerCase = map toLower
