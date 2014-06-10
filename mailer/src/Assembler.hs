{-# LANGUAGE RecordWildCards #-}
module Assembler (
    assembleContent
  , htmlToTxt
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Internal (c2w, w2c)
import Data.Char
import Data.List
import Data.Maybe
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity
import Text.JSON.Gen as J
import qualified Codec.Binary.QuotedPrintable as QP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.Base64 as Base64
import qualified Text.JSON as J

import Crypto.RNG
import Crypto.RNG.Utils
import Mails.Model
import File.Storage
import DB
import qualified Amazon as AWS
import qualified Log

assembleContent :: (CryptoRNG m, MonadDB m, Log.MonadLog m, AWS.AmazonMonad m) => Mail -> m BSL.ByteString
assembleContent Mail{..} = do
  (boundaryMixed, boundaryAlternative) <- createBoundaries
  let datafields = do
        J.value "email_id" $ show mailID
        J.value "email_token" $ show mailToken
        forM_ (fromXSMTPAttrs mailXSMTPAttrs) $ uncurry J.value
      mailgundata = runJSONGen datafields
      xsmtpapi = runJSONGen $ J.object "unique_args" datafields
  let -- FIXME: add =?UTF8?B= everywhere it is needed here
      headerEmail =
        -- FIXME: encoded word should not be longer than 75 bytes including everything
        "Subject: " ++ mailEncode mailTitle ++ "\r\n" ++
        "To: " ++ createMailTos mailTo ++ "\r\n" ++
        "From: " ++ mailEncode (addrName mailFrom) ++ " <" ++ addrEmail mailFrom ++ ">\r\n" ++
        "X-SMTPAPI: " ++ J.encode xsmtpapi ++ "\r\n" ++
        "X-Mailgun-Variables: " ++ J.encode mailgundata ++ "\r\n" ++
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
      attachmentType fname =
                if (".pdf" `isSuffixOf` (map toLower fname))
                    then  "application/pdf"
                    else if (".png" `isSuffixOf` (map toLower fname))
                      then "image/png"
                      else if (".jpg" `isSuffixOf` (map toLower fname))
                        then "image/jpeg"
                        else "application/octet-stream"

      headerAttach fname = "\r\n--" ++ boundaryMixed ++ "\r\n" ++
        "Content-Disposition: inline; filename=\"" ++ mailEncode fname ++"\"\r\n" ++
        "Content-Type: "++attachmentType fname++"; name=\"" ++ mailEncode fname ++ "\"\r\n" ++
        "Content-Transfer-Encoding: base64\r\n" ++
        "\r\n"
      attach Attachment{..} = do
        content <- case attContent of
                  Left c -> return c
                  Right file_id -> do
                    getFileIDContents file_id
        return $ BSLU.fromString (headerAttach attName) `BSL.append`
               BSL.fromChunks [Base64.joinWith (BSC.pack "\r\n") 72 $ Base64.encode content]
  atts <- mapM attach mailAttachments
  return $ BSL.concat $ [
      BSLU.fromString headerEmail
    , BSLU.fromString headerContent
    , BSLU.fromString headerContentText
    , BSLU.fromString $ htmlToTxt mailContent
    , BSLU.fromString headerContentHtml
    , BSL.fromChunks [BSU.fromString mailContent]
    , BSLU.fromString footerContent
    ] ++ atts
      ++ [BSLU.fromString footerEmail]

-- from simple utf-8 to =?UTF-8?Q?zzzzzzz?=
mailEncode :: String -> String
mailEncode source = unwords $ map encodeWord $ map convW $ words source
  where
    -- here we really want to use latin1 encoding to treat chars as bytes
    -- the QuotedPrintable encoder treats chars as bytes
    -- overall this is UTF-8 encoded
    --
    -- also: this removes spaces at front, spaces at the end, double spaces
    -- and (important!) converts \r and \n to space.
    --
    -- please keep these conversions as the are security measure
    wordNeedsEncoding word = any charNeedsEncoding word
    convW t = unescapeHTML $ map w2c $ BS.unpack $ BSU.fromString t
    -- The character à resulted in a \160 char, which is a non-breaking space, and thus words removed that char. Which broke the subject of a few french emails.
    -- FIXME: needs to check if this is full list of exceptions
    charNeedsEncoding char = char <= ' ' || char >= '\x7f' || char == '='
    encodeWord wa | wordNeedsEncoding wa = "=?UTF-8?Q?" ++ QP.encode (map c2w wa) ++ "?="
                  | otherwise = wa

createMailTos :: [Address] -> String
createMailTos = intercalate ", "
  . map (\Address{..} -> mailEncode addrName ++ " <" ++ addrEmail ++ ">")

createBoundaries :: (MonadIO m, CryptoRNG m) => m (String, String)
createBoundaries = return (,) `ap` f `ap` f
  where
    f = randomString 32 $ ['0'..'9'] ++ ['a'..'z']

unescapeHTML :: String -> String
unescapeHTML [] = []
unescapeHTML ('&':xs) =
  let (b, a) = break (== ';') xs in
  case lookupEntity b of
    Just c | listToMaybe a == Just ';' ->  c ++ unescapeHTML (tail a)
    _                                  -> '&' : unescapeHTML xs
unescapeHTML (x:xs) = x : unescapeHTML xs

-- | Convert e-mail from html to txt
htmlToTxt :: String -> String
htmlToTxt = dropWhile isSpace . unescapeHTML . removeTripleNL . toText . removeWSAfterNL . reduceWS .
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
            "tr"     -> "\r\n"
            "strong" -> "*"
            _        -> ""

    replaceLinks [] = []
    replaceLinks (t@(TagOpen "a" attrs):ts) =
      case "href" `lookup` attrs of
        Just link -> TagText "[ " : linktext ++ [TagText (": " ++ link ++ " ]")] ++ replaceLinks (drop 1 rest)
          where (linktext,rest) = break (==(TagClose "a")) ts
        Nothing   -> t : replaceLinks ts
    replaceLinks (t:ts) = t : replaceLinks ts

    removeTripleNL [] = []
    removeTripleNL ('\r':'\n':'\r':'\n':'\r':'\n':l) = removeTripleNL ('\r':'\n':'\r':'\n':l)
    removeTripleNL (a:l) = a : removeTripleNL l

    removeWSAfterNL = foldr f []
      where
        f t ts@((TagText s):ts') =
          case t of
            TagOpen el _ -> g el
            TagClose el  -> g el
            _            -> t : ts
          where
            g el =
              if el `elem` ["p", "br", "tr"]
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
        f (TagOpen t _) = t `elem` ["a", "b", "br", "i", "p", "strong", "tr"]
        f (TagClose t)  = t `elem` ["a", "b", "i", "p", "strong", "tr"]
        f _             = False

    lowerTags = map f
      where
        f (TagOpen t attrs) = TagOpen (lowerCase t) $ map (first lowerCase) attrs
        f (TagClose t) = TagClose (lowerCase t)
        f t = t

    lowerCase = map toLower
