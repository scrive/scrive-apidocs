{-# LANGUAGE RecordWildCards #-}

module Assembler (
    assembleContent
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Time
import Control.Monad.Trans.Control
import Crypto.RNG
import Crypto.RNG.Utils
import Data.Char
import Log
import Text.JSON.Gen as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.JSON as J

import AddressUtils (punyEncode)
import Data.ByteString.Utils (splitEvery)
import DB
import File.Storage
import HtmlToTxt
import Mails.Types
import MinutesTime

assembleContent :: (CryptoRNG m, MonadDB m, MonadIO m, MonadMask m, MonadBaseControl IO m, MonadLog m, MonadFileStorage m, MonadTime m) => Mail -> m BSL.ByteString
assembleContent Mail{..} = do
  time <- currentTime
  (boundaryMixed, boundaryAlternative,boundaryRelated) <- createBoundaries
  let datafields = do
        J.value "email_id" $ show mailID
        J.value "email_token" $ show mailToken
      mailgundata = runJSONGen datafields
      xsmtpapi = runJSONGen $ J.object "unique_args" datafields
      lineReplyTo = case mailReplyTo of
                     -- temporarily make reply-to equal to from header
                     -- to workaround spam issues
                     Just _addr -> "Reply-To: " <> createAddrString mailFrom <> "\r\n"
                     Nothing   -> ""
  let -- FIXME: add =?UTF8?B= everywhere it is needed here
      headerEmail :: Text =
        -- FIXME: encoded word should not be longer than 75 bytes including everything
        mailHeader "Subject" mailTitle <>
        "To: " <> createMailTos mailTo <> "\r\n" <>
        "From: " <> createAddrString mailFrom <> "\r\n" <>
        "Date: " <> (T.pack $ formatTimeForMail time) <>"\r\n" <>
        lineReplyTo <>
        "X-SMTPAPI: " <> (T.pack $ J.encode xsmtpapi) <> "\r\n" <>
        "X-Mailgun-Variables: " <> (T.pack $ J.encode mailgundata) <> "\r\n" <>
        "X-xsMessageId: " <> (showt mailID) <> "-" <> (showt mailToken) <> "\r\n" <>
        "X-MJ-CustomID: " <> (showt mailID) <> "-" <> (showt mailToken) <> "\r\n" <>
        "MIME-Version: 1.0\r\n" <>
        "Content-Type: multipart/mixed;\r\n boundary=" <> boundaryMixed <> "\r\n" <>
        "\r\n"
      headerContent = "--" <> boundaryMixed <> "\r\n" <>
        "Content-Type: multipart/alternative; boundary=" <> boundaryAlternative <> "\r\n" <> "\r\n"
      headerContentText = "\r\n--" <> boundaryAlternative <> "\r\n" <>
        "Content-type: text/plain; charset=utf-8\r\n" <>
        "Content-Transfer-Encoding: 8bit\r\n" <>
        "\r\n"
      headerContentRelated = "\r\n--" <> boundaryAlternative <> "\r\n" <>
        "Content-Type: multipart/related; boundary=" <> boundaryRelated <> "\r\n" <> "\r\n"

      headerContentHtml = "\r\n--" <> boundaryRelated <> "\r\n" <>
        "Content-type: text/html; charset=utf-8\r\n" <>
        "Content-Transfer-Encoding: 8bit\r\n" <>
        "\r\n"
      footerRelated = "\r\n--" <> boundaryRelated <> "--\r\n"
      footerContent = "\r\n--" <> boundaryAlternative <> "--\r\n"
      footerEmail = "\r\n--" <> boundaryMixed <> "--\r\n"
      -- mail content lines should not exceed 500chars for some mail servers
      -- replace <br/> with <br/>\r\n
      fixLineLengths = T.replace "<br/>\r\n\r\n" "<br/>\r\n" . T.replace "<br/>" "<br/>\r\n" . T.replace "<BR/>" "<br/>"
      attachmentType fname =
                if (".pdf" `T.isSuffixOf` (T.toLower fname))
                    then  "application/pdf"
                    else if (".png" `T.isSuffixOf` (T.toLower fname))
                      then "image/png"
                      else if (".jpg" `T.isSuffixOf` (T.toLower fname))
                        then "image/jpeg"
                        else "application/octet-stream"

      headerRelated Attachment{..} =
        let filename = mailEncode Nothing attName
        in "\r\n--" <> boundaryRelated <> "\r\n" <>
        "Content-Disposition: inline; filename=\"" <> filename <>"\"\r\n" <>
        "Content-Type: " <> attachmentType attName <> "; name=\"" <> filename <> "\"\r\n" <>
        "Content-ID: <" <> attName <> ">\r\n" <>
        "Content-Transfer-Encoding: base64\r\n" <>
        "\r\n"
      headerMixed Attachment{..} =
        let filename = mailEncode Nothing attName
        in "\r\n--" <> boundaryMixed <> "\r\n" <>
        "Content-Disposition: attachment; filename=\"" <> filename <>"\"\r\n" <>
        "Content-Type: " <> attachmentType attName <> "; name=\"" <> filename <> "\"\r\n" <>
        "Content-Transfer-Encoding: base64\r\n" <>
        "\r\n"
      (relatedAtt,mixedAtt) = partition (\att -> ".png" `T.isSuffixOf` attName att || ".jpg" `T.isSuffixOf` attName att) mailAttachments

  relatedAttContent  <- forM relatedAtt $ \att -> do
        content <- either return getFileIDContents $ attContent att
        return $ (BSL.fromStrict $ TE.encodeUtf8  (headerRelated att)) `BSL.append`
               BSL.fromChunks [Base64.joinWith (BSC.pack "\r\n") 72 $ Base64.encode content]
  mixedAttContent  <- forM mixedAtt $ \att -> do
        content <- either return getFileIDContents $ attContent att
        return $ (BSL.fromStrict $ TE.encodeUtf8 $ (headerMixed att)) `BSL.append`
               BSL.fromChunks [Base64.joinWith (BSC.pack "\r\n") 72 $ Base64.encode content]

  return $ BSL.concat $ [
      BSL.fromStrict $ TE.encodeUtf8 headerEmail
    , BSL.fromStrict $ TE.encodeUtf8 headerContent
    , BSL.fromStrict $ TE.encodeUtf8 headerContentText
    , BSLU.fromString $ htmlToTxt $ T.unpack mailContent
    , BSL.fromStrict $ TE.encodeUtf8 headerContentRelated
    , BSL.fromStrict $ TE.encodeUtf8 headerContentHtml
    , BSL.fromChunks [TE.encodeUtf8 $ fixLineLengths mailContent]
    ] <> relatedAttContent <>
    [
      BSL.fromStrict $ TE.encodeUtf8 footerRelated
    , BSL.fromStrict $ TE.encodeUtf8 footerContent
    ] <> mixedAttContent
      <> [BSL.fromStrict $ TE.encodeUtf8 footerEmail]


mailHeader :: Text -> Text -> Text
mailHeader headerName headerValue = prefix <> mailEncode (Just $ 75 - T.length prefix) headerValue <> "\r\n"
    where prefix = headerName <> ": "

asciiPrintable :: Text -> Bool
asciiPrintable = T.all $ \c -> ord c >= 32 && ord c <= 126 && ord c /= ord ';'

mailEncode :: Maybe Int -> Text -> Text
mailEncode mFirstLineLength source | asciiPrintable source = asciiMailEncode mFirstLineLength source
                                   | otherwise = unicodeMailEncode mFirstLineLength source

-- only line breaking
asciiMailEncode :: Maybe Int -> Text -> Text
asciiMailEncode mFirstLineLength source = T.concat $ intersperse "\r\n " $ map T.unwords linesWithWords
  where
    lineLength = 10
    linesWithWords = aux [] [] (fromMaybe lineLength mFirstLineLength) $ T.words source

    -- this function tries to rearrenge long text into lines
    -- where every line takes at most 78 characters (including \r\n and leading space)
    -- but if there is a single word longer than 78 it will occupy whole line.
    -- we cannot split words
    -- w=word, ws=words, l=line, ls=lines, rsil=remaining space in line
    aux :: [[Text]] -- list of lines generated so far (line is a list of words)
        -> [Text]   -- new line we are generating (list of words)
        -> Int        -- remaining number of characters in the current line
        -> [Text]   -- list of words left to arrange in lines
        -> [[Text]] -- list of lines
    aux ls [] _ [] = ls -- if current line is empty and no more words to arrange
    aux ls l _ [] = ls <> [l] -- no more words to arrange -> return all the lines and current line
    aux ls l rsil (w:ws) | T.length w >= lineLength = -- this word will never fit with anything else
                             case l of
                               -- current line is empty, so append new line consisting of only this new mega-word
                               [] -> aux (ls <> [[w]]) [] lineLength ws
                               -- finish current line by adding it to all the lines, and also append new line consisting of only this new mega-word
                               _ -> aux (ls <> [l, [w]]) [] lineLength ws
                         -- in current empty line there is enough space for the new word
                         -- append this new word to the current line, and adjust remaining space counter
                         | null l && T.length w <= rsil = aux ls [w] (rsil - T.length w) ws
                         -- in current non-empty line there is enough space for the new word and a space char
                         -- append this new word to the current line, and adjust remaining space counter
                         | T.length w + 1 <= rsil = aux ls (l <> [w]) (rsil - T.length w - 1) ws
                         -- this new word will not fit in current line,
                         -- move current line to other lines (so this line is done)
                         -- and start a new line with only this new word
                         | otherwise = aux (ls <> [l]) [w] (lineLength - T.length w) ws

-- from simple utf-8 to =?UTF-8?B?zzzzzzz?=
unicodeMailEncode :: Maybe Int -> Text -> Text
unicodeMailEncode mFirstLineLength source = T.concat $ intersperse "\r\n\t" $ map encodeWord chunksBeforeEncoding
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
    encodeWord :: BS.ByteString -> Text
    encodeWord chunk = "=?UTF-8?B?" <> (TE.decodeUtf8 $ Base64.encode chunk) <> "?="

    -- calculate number of bytes that will fit in desired encoded chunk size
    -- 12 chars for encoded-word syntax, (`div` 4) . (*3) for base64 overhead
    preEncodeChunkSize desiredEncodedSize = 3 * ((desiredEncodedSize - 12) `div` 4)

    cleanedUpSource = T.unwords $ T.words source
    (firstLineBeforeEncoding, restBeforeEncoding) =
      BS.splitAt (preEncodeChunkSize $ fromMaybe 75 mFirstLineLength) $
        BSU.fromString $ T.unpack cleanedUpSource
    chunksBeforeEncoding = if (BS.null restBeforeEncoding)
                              then [firstLineBeforeEncoding] -- Lets not encode empty lines
                              else firstLineBeforeEncoding:splitEvery (preEncodeChunkSize 75) restBeforeEncoding

createMailTos :: [Address] -> Text
createMailTos = T.intercalate ", " . map (\a -> createAddrString a)

createAddrString :: Address -> Text
createAddrString Address{..}
    | asciiPrintable addrName = mailEncode Nothing escapedName <> " <" <> punyEncode addrEmail <> ">"
    | otherwise = mailEncode Nothing addrName <> " <" <> punyEncode addrEmail <> ">"
  -- wrap in quotes to handle commas correctly in names
  -- discard quotes from the inside, to block any injections
  -- most clients do this anyway
  -- Only do this for ascii-only names - unicode names will get encoded anyway
  where escapedName = "\"" <> T.filter (/='"') addrName <> "\""

createBoundaries :: forall m . CryptoRNG m => m (Text, Text, Text)
createBoundaries = return (,,) `ap` f `ap` f `ap` f
  where
    f :: m Text
    f = fmap T.pack $ randomString 32 $ ['0'..'9'] <> ['a'..'z']
