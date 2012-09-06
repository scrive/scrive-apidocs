-----------------------------------------------------------------------------
-- |
-- Module      :  SOAP
-- Maintainer  :
-- Stability   :  development
-- Portability :  portable
--
-- Common SOAP stuff (for Webservices)
--
-----------------------------------------------------------------------------

module SOAP.SOAP
  ( elementNS
  , inElementNS
  , makeSoapCallWithCert
  , makeSoapCallWithCA
  , makeSoapCallWithCookies ) where

import Utils.Either
import Utils.IO
import System.Exit
import Text.XML.HaXml.XmlContent.Parser
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types (QName(N))
import Text.XML.HaXml.Posn
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length, drop, break)
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length, drop)
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Exception as E
import qualified Log

import Prelude hiding (print, putStrLn, putStr)

data SOAP a = SOAP a
              | SOAPFault String String String
  deriving (Eq,Ord,Show,Read)

instance HTypeable (SOAP a) where
  toHType _ = Defined "soap" [] []
instance (XmlContent a) => XmlContent (SOAP a) where
  toContents (SOAP a) =
    [CElem (Elem (N "Envelope") [mkAttr "xmlns" "http://schemas.xmlsoap.org/soap/envelope/"]
      [CElem (Elem (N "Body") []
      (toContents a)) ()]) ()]
  toContents _ = error "Please do not serialize SOAPFault"
  parseContents = do
    { inElementNS "Envelope" $ do
      { _ <- optional $ elementNS "Header"
      ; inElementNS "Body" $ choice SOAP
                                    (inElementNS "Fault" $ do
                                      { faultcode <- inElementNS "faultcode" text
                                      ; faultstring <- inElementNS "faultstring" text
                                      -- ; faultactor <- optional $ inElementNS "faultactor" text
                                      ; _ <- optional $ elementNS "detail"
                                      -- ; return (SOAPFault faultcode faultstring (maybe "" id faultactor))
                                      ; return (SOAPFault faultcode faultstring "")
                                      }
                                    )
      }
    } `adjustErr` ("in <Envelope/Body>, "++)

skipNamespacePrefix :: String -> String -> Bool
skipNamespacePrefix fqname tomatch =
  case break (==':') fqname of
    (name,"") -> name == tomatch
    (_,':':name) -> name == tomatch
    _ -> False

elementNS :: String -> XMLParser (Element Text.XML.HaXml.Posn.Posn)
elementNS name = elementWith skipNamespacePrefix [name]

inElementNS :: String -> XMLParser b-> Parser (Content Text.XML.HaXml.Posn.Posn) b
inElementNS name action = do
  e <- elementNS name
  interior e action

tryAndJoinEither :: IO (Either String a) -> IO (Either String a)
tryAndJoinEither action = do
  result <- E.try action
  case result of
    Right value -> return value
    Left (excpt :: E.SomeException) -> return (Left (show excpt))

-- other makeSoapCallXXX functions use this as their base
makeSoapCall :: (XmlContent request, XmlContent result)
                => String
                -> String
                -> [String]
                -> request
                -> IO (Either String result)
makeSoapCall url action extraargs request = tryAndJoinEither $ do
  let input = showXml False (SOAP request)
  -- BSL.appendFile "soap.xml" input
  Log.debug $ input
  let args = [ "-X", "POST",
               "-k", "--show-error" ] ++ extraargs ++
             [ "--data-binary", "@-",
               "-H", "Content-Type: text/xml; charset=UTF-8",
               "-H", "Expect: 100-continue",
               "-H", "SOAPAction: " ++ action,
               url
             ]

  (code,stdout,stderr) <- readCurl args $ BSL.fromString input
  -- BSL.appendFile "soap.xml" stdout

  {-
   stdout here might be a multipart stream
   lets do it the painful way

   --uuid:1616144b-0f50-40a5-9587-92b5b989e330+id=48
   Content-ID: <http://tempuri.org/0>
   Content-Transfer-Encoding: 8bit
   Content-Type: application/xop+xml;charset=utf-8;type="text/xml"

   <s:Envelope ...>,,,</s:Envelope>
   --uuid:1616144b-0f50-40a5-9587-92b5b989e330+id=48--

   so:
   1. See if it starts with --
   2. Cut off to first \n and remember
   3. Cut-off to first \n\n
   4. Cut up to the remembered part
   5. Use middle as XML to parse
   -}
  let (boundary,rest) = BS.breakSubstring (BS.fromString "\r\n") (BS.concat (BSL.toChunks (BSL.drop 2 stdout)))
      (_,rest2) = BS.breakSubstring (BS.fromString "\r\n\r\n") rest
      (xml1,_) = BS.breakSubstring boundary rest2
  let xml = if BSL.fromString "\r\n--" `BSL.isPrefixOf` stdout
            then BSL.fromChunks [xml1]
            else stdout
  case code of
    ExitFailure _ ->
      return (Left $ "Cannot execute 'curl' for soap: " ++ show args ++ BSL.toString stderr)
    ExitSuccess -> do
      BSL.appendFile "soap.xml" xml
      let s = BSL.toString xml
      Log.debug $ "length of xml string from soap call: " ++ show (length s)
      let rx = readXml s
      Log.debug $ "readXml returned left or right: " ++ if isLeft rx then "Left" else "Right"
      case rx of
        Right (SOAP result) -> return (Right result)
        Right (SOAPFault soapcode string actor) -> return (Left (soapcode ++":" ++ string ++":" ++ actor))
        Left errmsg -> return (Left (errmsg ++ ": " ++ BSL.toString stdout))

makeSoapCallWithCert :: (XmlContent request, XmlContent result)
                        => String
                        -> String
                        -> String
                        -> String
                        -> request
                        -> IO (Either String result)
makeSoapCallWithCert url action cert certpwd request =
  let args = [ "--silent",
               "--cert", cert ++ ":" ++ certpwd,
               "--cacert", cert
             ] in
  makeSoapCall url action args request

makeSoapCallWithCA :: (XmlContent request, XmlContent response)
                      => String
                      -> String
                      -> String
                      -> request
                      -> IO (Either String response)
makeSoapCallWithCA url cert action request =
  let args = [ "--verbose",
               "--cacert", cert
             ] in
  makeSoapCall url action args request

makeSoapCallWithCookies :: (XmlContent request, XmlContent result)
                           => String
                           -> FilePath
                           -> String
                           -> request
                           -> IO (Either String result)
makeSoapCallWithCookies url cookiefile action request =
  let args = [ "--silent",
               "--cookie", cookiefile,
               "--cookie-jar", cookiefile
             ] in
  makeSoapCall url action args request
