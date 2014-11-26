{-# LANGUAGE RecordWildCards #-}
module Assembler (
    assembleContent
  , htmlToTxt
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity
import Text.JSON.Gen as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8 as BSU
import qualified Text.JSON as J

import Crypto.RNG
import Crypto.RNG.Utils
import Data.ByteString.Utils (splitEvery)
import DB
import File.Storage
import Mails.Model
import qualified Amazon as AWS
import qualified Log

assembleContent :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadIO m, Log.MonadLog m, AWS.AmazonMonad m) => Mail -> m BSL.ByteString
assembleContent Mail{..} = do
  (boundaryMixed, boundaryAlternative,boundaryRelated) <- createBoundaries
  let datafields = do
        J.value "email_id" $ show mailID
        J.value "email_token" $ show mailToken
        forM_ (fromXSMTPAttrs mailXSMTPAttrs) $ uncurry J.value
      mailgundata = runJSONGen datafields
      xsmtpapi = runJSONGen $ J.object "unique_args" datafields
      lineReplyTo = case mailReplyTo of
                     Just addr -> "Reply-To: " ++ createAddrString addr ++ "\r\n"
                     Nothing   -> ""
  let -- FIXME: add =?UTF8?B= everywhere it is needed here
      headerEmail =
        -- FIXME: encoded word should not be longer than 75 bytes including everything
        mailHeader "Subject" mailTitle ++
        "To: " ++ createMailTos mailTo ++ "\r\n" ++
        "From: " ++ createAddrString mailFrom ++ "\r\n" ++
        lineReplyTo ++
        "X-SMTPAPI: " ++ J.encode xsmtpapi ++ "\r\n" ++
        "X-Mailgun-Variables: " ++ J.encode mailgundata ++ "\r\n" ++
        "MIME-Version: 1.0\r\n" ++
        "Content-Type: multipart/mixed;\r\n boundary=" ++ boundaryMixed ++ "\r\n" ++
        "\r\n"
      headerContent = "--" ++ boundaryMixed ++ "\r\n" ++
        "Content-Type: multipart/alternative; boundary=" ++ boundaryAlternative ++ "\r\n" ++ "\r\n"
      headerContentText = "\r\n--" ++ boundaryAlternative ++ "\r\n" ++
        "Content-type: text/plain; charset=utf-8\r\n" ++
        "\r\n"
      headerContentRelated = "\r\n--" ++ boundaryAlternative ++ "\r\n" ++
        "Content-Type: multipart/related; boundary=" ++ boundaryRelated ++ "\r\n" ++ "\r\n"

      headerContentHtml = "\r\n--" ++ boundaryRelated ++ "\r\n" ++
        "Content-type: text/html; charset=utf-8\r\n" ++
        "\r\n"
      footerRelated = "\r\n--" ++ boundaryRelated ++ "--\r\n"
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

      headerRelated Attachment{..} =
        let filename = mailEncode Nothing attName
        in "\r\n--" ++ boundaryRelated ++ "\r\n" ++
        "Content-Disposition: inline; filename=\"" ++ filename ++"\"\r\n" ++
        "Content-Type: " ++ attachmentType attName ++ "; name=\"" ++ filename ++ "\"\r\n" ++
        "Content-ID: <" ++ attName ++ ">\r\n" ++
        "Content-Transfer-Encoding: base64\r\n" ++
        "\r\n"
      headerMixed Attachment{..} =
        let filename = mailEncode Nothing attName
        in "\r\n--" ++ boundaryMixed ++ "\r\n" ++
        "Content-Disposition: attachment; filename=\"" ++ filename ++"\"\r\n" ++
        "Content-Type: " ++ attachmentType attName ++ "; name=\"" ++ filename ++ "\"\r\n" ++
        "Content-Transfer-Encoding: base64\r\n" ++
        "\r\n"
      (relatedAtt,mixedAtt) = partition (\att -> ".png" `isSuffixOf` attName att || ".jpg" `isSuffixOf` attName att) mailAttachments

  relatedAttContent  <- forM relatedAtt $ \att -> do
        content <- either return getFileIDContents $ attContent att
        return $ BSLU.fromString (headerRelated att) `BSL.append`
               BSL.fromChunks [Base64.joinWith (BSC.pack "\r\n") 72 $ Base64.encode content]
  mixedAttContent  <- forM mixedAtt $ \att -> do
        content <- either return getFileIDContents $ attContent att
        return $ BSLU.fromString (headerMixed att) `BSL.append`
               BSL.fromChunks [Base64.joinWith (BSC.pack "\r\n") 72 $ Base64.encode content]

  return $ BSL.concat $ [
      BSLU.fromString headerEmail
    , BSLU.fromString headerContent
    , BSLU.fromString headerContentText
    , BSLU.fromString $ htmlToTxt mailContent
    , BSLU.fromString headerContentRelated
    , BSLU.fromString headerContentHtml
    , BSL.fromChunks [BSU.fromString mailContent]
    ] ++ relatedAttContent ++
    [
      BSLU.fromString footerRelated
    , BSLU.fromString footerContent
    ] ++ mixedAttContent
      ++ [BSLU.fromString footerEmail]


mailHeader :: String -> String -> String
mailHeader headerName headerValue = prefix ++ mailEncode (Just $ 75 - length prefix) headerValue ++ "\r\n"
    where prefix = headerName ++ ": "

-- from simple utf-8 to =?UTF-8?B?zzzzzzz?=
mailEncode :: Maybe Int -> String -> String
mailEncode mFirstLineLength source = concat $ intersperse "\r\n\t" $ map encodeWord chunksBeforeEncoding
  where
    -- Use encoded-words from rfc 1342 (using utf-8 and base64)
    -- Every encoded-word cannot be longer than 75 chars
    -- (including delimiters and encoding/charset specifiers),
    -- so we split text into 45 char chunks (45 bytes after base64 encoding
    -- is 60 characters, and after adding 12 characters of encoded-word syntax
    -- is almost at the limit. 46 bytes would end up using 76 chars)
    -- Last chunk could be shorter than 45 bytes.
    -- If mFirstLineLength is Just number, we use that as the first chunk size,
    -- because first encoded-word may end up in the same line as header name.

    -- encode utf-8 encoded byte string with base64 and add encoded-word syntax
    encodeWord :: BS.ByteString -> String
    encodeWord chunk = "=?UTF-8?B?" ++ (BSU.toString $ Base64.encode chunk) ++ "?="

    -- calculate number of bytes that will fit in desired encoded chunk size
    -- 12 chars for encoded-word syntax, (`div` 4) . (*3) for base64 overhead
    preEncodeChunkSize desiredEncodedSize = 3 * ((desiredEncodedSize - 12) `div` 4)

    cleanedUpSource = unwords $ words source
    (firstLineBeforeEncoding, restBeforeEncoding) = BS.splitAt (preEncodeChunkSize $ fromMaybe 75 mFirstLineLength) $ BSU.fromString cleanedUpSource
    chunksBeforeEncoding = if (BS.null restBeforeEncoding)
                              then [firstLineBeforeEncoding] -- Lets not encode empty lines
                              else firstLineBeforeEncoding:splitEvery (preEncodeChunkSize 75) restBeforeEncoding

createMailTos :: [Address] -> String
createMailTos = intercalate ", " . map (\a -> createAddrString a)

createAddrString :: Address -> String
createAddrString Address{..} = mailEncode Nothing addrName ++ " <" ++ addrEmail ++ ">"

createBoundaries :: CryptoRNG m => m (String, String,String)
createBoundaries = return (,,) `ap` f `ap` f `ap` f
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
