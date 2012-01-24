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
    -- (signDocument)
    where
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length, drop, break)
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length, drop)
import qualified Data.ByteString.Lazy as BSL
import Misc
import System.Exit
import Text.XML.HaXml.XmlContent.Parser
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Posn
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
        [CElem (Elem "Envelope" [mkAttr "xmlns" "http://schemas.xmlsoap.org/soap/envelope/"]
                         [CElem (Elem "Body" []
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

makeSoapCall :: (XmlContent request, XmlContent result)
                => String
             -> String
             -> String
             -> String
             -> request
             -> IO (Either String result)
makeSoapCall url action cert certpwd request = tryAndJoinEither $ do
  Log.debug $ show request
  let input = fpsShowXml False (SOAP request)
  Log.debug $ show input
  -- BSL.appendFile "soap.xml" input

  let args = [ "-X", "POST",
               "-k", "--silent", "--show-error",
               "--cert", cert ++ ":" ++ certpwd,
               "--cacert", cert,
               "--data-binary", "@-",
               "-H", "Content-Type: text/xml; charset=UTF-8",
               "-H", "Expect: 100-continue",
               "-H", "SOAPAction: " ++ action,
               url
             ]

  (code,stdout,stderr) <- readCurl args input
  Log.debug $ show code
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
  Log.debug $ show xml
  case code of
    ExitFailure _ -> do
       return (Left $ "Cannot execute 'curl' for TrustWeaver: " ++ show args ++ BSL.toString stderr)
    ExitSuccess -> do
      case readXml (BSL.toString xml) of
        Right (SOAP result) -> return (Right result)
        Right (SOAPFault soapcode string actor) -> return (Left (soapcode ++":" ++ string ++":" ++ actor))
        Left errmsg -> return (Left (errmsg ++ ": " ++ BSL.toString stdout))

makeSoapCallCA :: (XmlContent request, XmlContent response) =>
                        String -> String -> String -> request -> IO (Either String response)
makeSoapCallCA url cert action request = do
  let input = fpsShowXml False (SOAP request)
  -- BSL.appendFile "soap.xml" input
  let args = [               "-k", "--verbose", "--show-error",
               -- "--cert", cert ++ ":" ++ certpwd,
               "--cacert", cert,
               "--data-binary", "@-",
               "-H", "Content-Type: text/xml; charset=UTF-8",
               "-H", "Expect: 100-continue",
               "-H", "SOAPAction: " ++ action,
               url
             ]

  (code,stdout,stderr) <- readProcessWithExitCode' "curl" args input

  let (boundary,rest) = BS.breakSubstring (BS.fromString "\r\n") (BS.concat (BSL.toChunks (BSL.drop 2 stdout)))
      (_,rest2) = BS.breakSubstring (BS.fromString "\r\n\r\n") rest
      (xml1,_) = BS.breakSubstring boundary rest2

  xml <- if BSL.fromString "\r\n--" `BSL.isPrefixOf` stdout
            then do
              return (BSL.fromChunks [xml1])
            else return stdout
  if (code /= ExitSuccess)
       then return (Left $ "Cannot execute ./curl for BankID: " ++ show args ++ BSL.toString stderr)
     else case readXml (BSL.toString xml) of
            Right (SOAP result) -> return (Right result)
            Right (SOAPFault soapcode string actor) -> return (Left (soapcode ++":" ++ string ++":" ++ actor))
            Left errmsg -> return (Left (errmsg ++ ": " ++ BSL.toString stdout))

makeSoapCallINSECURE :: (XmlContent request, XmlContent response) =>
                        String -> String -> request -> IO (Either String response)
makeSoapCallINSECURE url action request = do
  let input = fpsShowXml False (SOAP request)
  -- BSL.appendFile "soap.xml" input

  let args = [               "-k", "--verbose", "--show-error",
               -- "--cert", cert ++ ":" ++ certpwd,
               --"--cacert", cert,
               "--data-binary", "@-",
               "-H", "Content-Type: text/xml; charset=UTF-8",
               "-H", "Expect: 100-continue",
               "-H", "SOAPAction: " ++ action,
               url
             ]

  (code,stdout,stderr) <- readProcessWithExitCode' "curl" args input

  let (boundary,rest) = BS.breakSubstring (BS.fromString "\r\n") (BS.concat (BSL.toChunks (BSL.drop 2 stdout)))
      (_,rest2) = BS.breakSubstring (BS.fromString "\r\n\r\n") rest
      (xml1,_) = BS.breakSubstring boundary rest2

  xml <- if BSL.fromString "\r\n--" `BSL.isPrefixOf` stdout
            then do
              return (BSL.fromChunks [xml1])
            else return stdout
  if (code /= ExitSuccess)
       then return (Left $ "Cannot execute ./curl for BankID: " ++ show args ++ BSL.toString stderr)
     else case readXml (BSL.toString xml) of
            Right (SOAP result) -> return (Right result)
            Right (SOAPFault soapcode string actor) -> return (Left (soapcode ++":" ++ string ++":" ++ actor))
            Left errmsg -> return (Left (errmsg ++ ": " ++ BSL.toString stdout))
