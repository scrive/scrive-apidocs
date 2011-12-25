{-# LANGUAGE CPP #-}
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

module Mails.SendMail
    ( Mail(..)
    , emptyMail
    , unsendable 
    , MailAddress(..)
    , MailInfo(..)
    , scheduleEmailSendout'
    , scheduleEmailSendout
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSL  hiding (length)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.UTF8 as BS hiding (length)
import qualified Data.ByteString.Base64 as Base64
import qualified Codec.Binary.QuotedPrintable as QuotedPrintable
import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isSpace, toLower)
import Mails.MailsConfig
import Mails.MailsData
import Text.HTML.TagSoup
import Misc
import Data.ByteString.Internal (c2w,w2c)
import Data.List
import API.Service.Model
--import Context
--import KontraMonad
import DB.Classes
import Util.MonadUtils
import InputValidation

import Mails.Public

-- Needed only for FROM address
import User.Region
import Util.SignatoryLinkUtils
import Doc.Transitory
import Doc.DocStateData

scheduleEmailSendout :: DBMonad m => MailsConfig -> Mail -> m ()
scheduleEmailSendout mc = runDB . scheduleEmailSendout' mc

scheduleEmailSendout' :: MailsConfig -> Mail -> DB ()
scheduleEmailSendout' mc mail@Mail{to} = do
  boundaries <- createBoundaries
  mailId <- dbUpdate $ CreateEmail $ map (BS.toString . email) to
  content <- createWholeContent boundaries (ourInfoEmail mc) (ourInfoEmailNiceName mc) mailId mail
  _ <- dbUpdate $ AddContentToEmail mailId content
  return ()

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
        w = words $ unescapeEntities (map w2c $ BS.unpack source)
        wordNeedsEncoding word = any charNeedsEncoding word
        -- FIXME: needs to check if this is full list of exceptions
        charNeedsEncoding char = char <= ' ' || char >= '\x7f' || char == '='
        encodeWord wa | wordNeedsEncoding wa = "=?UTF-8?Q?" ++ QuotedPrintable.encode (map c2w wa) ++ "?="
                      | otherwise = wa

mailEncode1 :: String -> String
mailEncode1 source = mailEncode (BS.fromString source)

emptyMail :: Mail
emptyMail = Mail
    { to             = []
    , title          = BS.empty
    , content        = BS.empty
    , attachments    = []
    , from           = Nothing
    , mailInfo       = None
}

-- Mail is unsendable if there is no to adress provided
unsendable :: Mail -> Bool
unsendable mail = any (not . valid) (email <$> to mail)
    where
        valid x = case asValidEmail (BS.toString x) of
                        Good _ -> True
                        _ -> False

createMailTos :: Mail -> String
createMailTos (Mail {to}) =
    concat $ intersperse ", " $ map createMailTos' to

createMailTos' :: MailAddress -> String
createMailTos' (MailAddress {fullname, email}) =
    mailEncode fullname ++ " <" ++ BS.toString email ++ ">"

createBoundaries :: MonadIO m => m (String, String)
createBoundaries = liftIO $ (,) <$> f <*> f
    where
        f = randomString 32 $ ['0'..'9'] ++ ['a'..'z']

createWholeContent :: Show a => (String, String) -> String -> String -> a -> Mail -> DB BSLC.ByteString
createWholeContent (boundaryMixed, boundaryAlternative) ourInfoEmail ourInfoEmailNiceName mailId mail@(Mail {title,content,attachments,from,mailInfo}) = do
  fromHeader <- do
      otheraddres <- (fmap (servicemailfromaddress . servicesettings))
        <$> liftMM (dbQuery . GetService) (return from)
      case join otheraddres of
         Nothing -> do
             niceAddress <- fromNiceAddress mailInfo ourInfoEmailNiceName
             return $ "From: " ++ mailEncode1 niceAddress ++ " <" ++ ourInfoEmail ++ ">\r\n"
         Just address ->  return $ "From: <"++ BS.toString address ++ ">\r\n"
  let mailtos = createMailTos mail
      -- FIXME: add =?UTF8?B= everywhere it is needed here
      headerEmail =
          -- FIXME: encoded word should not be longer than 75 bytes including everything
          "Subject: " ++ mailEncode title ++ "\r\n" ++
          "To: " ++ mailtos ++ "\r\n" ++
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
      headerContentText = "\r\n--" ++ boundaryAlternative ++ "\r\n" ++
          "Content-type: text/plain; charset=utf-8\r\n" ++
          "\r\n"
      headerContentHtml = "\r\n--" ++ boundaryAlternative ++ "\r\n" ++
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
  return $ BSL.concat $
                     [ BSL.fromString headerEmail
                     , BSL.fromString headerContent
                     , BSL.fromString headerContentText
                     , BSL.fromString $ htmlToTxt $ BS.toString $ content
                     , BSL.fromString headerContentHtml
                     , BSL.fromChunks [BS.fromString $ wrapMail $ BS.toString content]
                     , BSL.fromString footerContent
                     ] ++ map attach attachments ++
                     [ BSL.fromString footerEmail
                     ]


-- | Convert e-mail from html to txt
htmlToTxt :: String -> String
htmlToTxt = dropWhile isSpace . unescapeEntities . toText . removeWSAfterNL . reduceWS .
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
                f (TagOpen t _) = t `elem` ["a", "b", "br", "i", "p", "strong"]
                f (TagClose t)  = t `elem` ["a", "b", "i", "p", "strong"]
                f _             = False

        lowerTags = map f
            where
                f (TagOpen t attrs) = TagOpen (lowerCase t) $ map (first lowerCase) attrs
                f (TagClose t) = TagClose (lowerCase t)
                f t = t

                lowerCase = map toLower


wrapMail :: String -> String
wrapMail body = "<html>"++
                    "<head>" ++
                      "<meta http-equiv='content-type' content='text/html; charset=utf-8'/>" ++
                            -- "<meta http-equiv='content-language' content='$ctxlang$'/>" ++
                    "</head>" ++
                    "<body style='background-color: #f5f5f5;'>" ++
                       body ++ 
                    "</body>" ++ 
                "</html>"          
  
  
-- Prototyped. This is why texts are here. But the propper way to do that is not to add some extra info in Mail data structure
-- Propper way is to hold as abstract data there.
fromNiceAddress ::  MailInfo -> String -> DB String
fromNiceAddress (None) servicename = return servicename
fromNiceAddress (Invitation did _) servicename = do
    mdoc <- doc_query' $ GetDocumentByDocumentID did
    case mdoc of
         Nothing -> return $ servicename
         Just doc -> case (documentregion doc, BS.toString $ getAuthorName doc) of 
                          (_,[]) -> return $  servicename 
                          (REGION_SE, an) -> return $ an ++ " genom " ++ servicename
                          (REGION_GB, an) -> return $ an ++ " through " ++ servicename
