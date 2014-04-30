module LiveDocx (
  LiveDocxConf(..)
  , FileFormat(..)
  , LiveDocxError(..)
  , getFileFormatForConversion
  , convertToPDF
) where

import qualified Control.Exception.Lifted as E (catch)

import Control.Monad()
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Base
import Data.Char
import Text.XML.HaXml.XmlContent.Parser
import Text.XML.HaXml.Types (QName(N))
import System.CPUTime
import System.FilePath
import System.IO.Temp
import System.IO
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS

import Utils.Read
import SOAP.SOAP
import qualified Log

import LiveDocxConf
import LiveDocxTypes

data LiveDocxContext = LiveDocxContext {
    ctxurl :: String
  , ctxcookiefile :: FilePath
}

type LiveDocx m a = ReaderT LiveDocxContext m (Either LiveDocxError a)

mkLiveDocxContext :: LiveDocxConf -> FilePath -> LiveDocxContext
mkLiveDocxContext conf cookiefile = LiveDocxContext {
    ctxurl = url conf
  , ctxcookiefile = cookiefile
}

makeLiveDocxCall :: (XmlContent request, HasXmlNamespace request, XmlContent result, Log.MonadLog m, MonadBaseControl IO m)
                      => request
                      -> LiveDocx m result
makeLiveDocxCall request = do
  ctx <- ask
  eresult <- makeSoapCallWithCookies
    (ctxurl ctx)
    (ctxcookiefile ctx)
    (liveDocxNamespace ++ xmlNamespace request)
    request
  return $ case eresult of
    Left msg -> Left $ LiveDocxSoapError msg
    Right result -> Right result

ignoreIfRight :: (Monad m) => Either LiveDocxError a -> LiveDocx m ()
ignoreIfRight (Left x) = return $ Left x
ignoreIfRight (Right _) = return $ Right ()

logIn :: forall m. (Log.MonadLog m, MonadBaseControl IO m) => String -> String -> LiveDocx m ()
logIn username password = ignoreIfRight =<<
  (makeLiveDocxCall
    (LogIn username password) :: LiveDocx m LogInResponse)

logOut :: forall m. (Log.MonadLog m, MonadBaseControl IO m) => LiveDocx m ()
logOut = ignoreIfRight =<<
  (makeLiveDocxCall
    LogOut :: LiveDocx m LogOutResponse)

setLocalTemplate :: forall m. (Log.MonadLog m, MonadBaseControl IO m) => BS.ByteString -> FileFormat ->  LiveDocx m ()
setLocalTemplate filecontents format = ignoreIfRight =<<
  (makeLiveDocxCall
    (SetLocalTemplate filecontents format) :: LiveDocx m SetLocalTemplateResponse)

createDocument :: forall m. (Log.MonadLog m, MonadBaseControl IO m) => LiveDocx m ()
createDocument = ignoreIfRight =<<
  (makeLiveDocxCall
    CreateDocument :: LiveDocx m CreateDocumentResponse)

retrieveDocument :: forall m. (Log.MonadLog m, MonadBaseControl IO m) => String ->  LiveDocx m BS.ByteString
retrieveDocument format = do
  result <- makeLiveDocxCall
              (RetrieveDocument format)
  case result of
    Right (RetrieveDocumentResponse pdfcontents) -> return $ Right pdfcontents
    Left msg -> return $ Left msg

getFileFormatForConversion :: FilePath -> Maybe FileFormat
getFileFormatForConversion = maybeRead . map toUpper . dropWhile (== '.') . takeExtension


withSystemTempFile :: (MonadBaseControl IO m) => String -> (String -> Handle -> m a) -> m a
withSystemTempFile initname = liftBaseOp (System.IO.Temp.withSystemTempFile initname . curry) . uncurry

{- | Calls the LiveDocx Soap API to convert the given document contents to a pdf.
     Errors are put in the docconverter.log.
 -}
convertToPDF :: forall m. (Log.MonadLog m, MonadBaseControl IO m)
             => LiveDocxConf -> BS.ByteString -> FileFormat -> m (Either LiveDocxError BS.ByteString)
convertToPDF conf filecontents format = do
  start <- liftBase $ getCPUTime
  res <- E.catch
           --withSystemTempFile gives us uniquely named temp file for storing LiveDocx session cookies in for curl
           (LiveDocx.withSystemTempFile "livedocx-cookies.txt" $ \cookiefile _cookiefilehandle ->
              runReaderT convertDocument (mkLiveDocxContext conf cookiefile))
           (return . Left . LiveDocxIOError)
  end <- liftBase $ getCPUTime
  case res of
    Left err ->
      Log.mixlog_ $ "failed conversion from " ++ show format ++ " to PDF: " ++ show err
    Right _ ->
      let diff = ((fromIntegral (end - start) * 0.00000000001) :: Double) in
      Log.mixlog_ $ "successful conversion from " ++ show format ++ " to PDF (took " ++ show diff ++ "s)"
  return res
  where
    {- | API calls are as follows:
         * logIn: login with username and password, generates session cookies which we store in a temp file for curl
         * setLocalTemplate: set a template with the original file contents and file format
         * createDocument: creates a document out of the template that we uploaded in the previous call
         * retrieveDocument: retrieves the document we created in the previous call from the LiveDocx server, we retrieve it in PDF format
         * logOut: finally logOut of the server
    -}
    convertDocument :: LiveDocx m BS.ByteString
    convertDocument = do
      _ <- logIn (username conf) (password conf)
      _ <- setLocalTemplate filecontents format
      _ <- createDocument
      pdfcontents <- retrieveDocument "PDF"
      _ <- logOut
      return pdfcontents

{- |
    Definitions of all the calls and responses we want to use.
    The documentation for these is here: https://api.livedocx.com/1.2/mailmerge.asmx?wsdl
-}


class HasXmlNamespace a where
  xmlNamespace :: a -> String

liveDocxNamespace :: String
liveDocxNamespace = "http://api.livedocx.com/2.1/mailmerge/"

data LogIn = LogIn String String
  deriving (Eq,Ord,Show,Read)

instance HTypeable LogIn where
    toHType _ = Defined "LogIn" [] []
instance XmlContent LogIn where
  toContents (LogIn username password) =
    [CElem (Elem (N "LogIn") [mkAttr "xmlns" liveDocxNamespace]
      [ mkElemC "username" (toText username)
      , mkElemC "password" (toText password)
      ]) ()]
  parseContents = error "Please do not parse a LogIn"
instance HasXmlNamespace LogIn where
  xmlNamespace = const "LogIn"

data LogInResponse = LogInResponse
  deriving (Eq,Ord,Show,Read)

instance HTypeable LogInResponse where
    toHType _ = Defined "LogInResponse" [] []
instance XmlContent LogInResponse where
  toContents LogInResponse = error "Please do not serialize LogInResponse"
  parseContents = do
    { _e <- elementNS "LogInResponse"
    ; return LogInResponse
    } `adjustErr` ("in <LogInResponse>, " ++)

data LogOut = LogOut
  deriving (Eq,Ord,Show,Read)

instance HTypeable LogOut where
    toHType _ = Defined "LogOut" [] []
instance XmlContent LogOut where
  toContents LogOut =
    [CElem (Elem (N "LogOut") [mkAttr "xmlns" liveDocxNamespace] []) ()]
  parseContents = error "Please do not parse a LogOut"
instance HasXmlNamespace LogOut where
  xmlNamespace = const "LogOut"

data LogOutResponse = LogOutResponse
  deriving (Eq,Ord,Show,Read)

instance HTypeable LogOutResponse where
    toHType _ = Defined "LogOutResponse" [] []
instance XmlContent LogOutResponse where
  toContents LogOutResponse = error "Please do not serialize LogOutResponse"
  parseContents = do
    { _e <- elementNS "LogOutResponse"
    ; return LogOutResponse
    } `adjustErr` ("in <LogOutResponse>, " ++)

data SetLocalTemplate = SetLocalTemplate BS.ByteString FileFormat
  deriving (Eq,Ord,Show,Read)

instance HTypeable SetLocalTemplate where
    toHType _ = Defined "SetLocalTemplate" [] []
instance XmlContent SetLocalTemplate where
  toContents (SetLocalTemplate template format) =
    let base64data = BSC.unpack (Base64.encode template) in
    [CElem (Elem (N "SetLocalTemplate") [mkAttr "xmlns" liveDocxNamespace]
      [ mkElemC "template" (toText base64data)
      , mkElemC "format" (toText $ show format)
      ]) ()]
  parseContents = error "Please do not parse an SetLocalTemplate"
instance HasXmlNamespace SetLocalTemplate where
  xmlNamespace = const "SetLocalTemplate"

data SetLocalTemplateResponse = SetLocalTemplateResponse
  deriving (Eq,Ord,Show,Read)

instance HTypeable SetLocalTemplateResponse where
    toHType _ = Defined "SetLocalTemplateResponse" [] []
instance XmlContent SetLocalTemplateResponse where
  toContents SetLocalTemplateResponse = error "Please do not serialize SetLocalTemplateResponse"
  parseContents = do
    { _e <- elementNS "SetLocalTemplateResponse"
    ; return SetLocalTemplateResponse
    } `adjustErr` ("in <SetLocalTemplateResponse>, " ++)

data CreateDocument = CreateDocument
  deriving (Eq,Ord,Show,Read)

instance HTypeable CreateDocument where
    toHType _ = Defined "CreateDocument" [] []
instance XmlContent CreateDocument where
  toContents CreateDocument =
    [CElem (Elem (N "CreateDocument") [mkAttr "xmlns" liveDocxNamespace] []) ()]
  parseContents = error "Please do not parse a CreateDocument"
instance HasXmlNamespace CreateDocument where
  xmlNamespace = const "CreateDocument"

data CreateDocumentResponse = CreateDocumentResponse
  deriving (Eq,Ord,Show,Read)

instance HTypeable CreateDocumentResponse where
    toHType _ = Defined "CreateDocumentResponse" [] []
instance XmlContent CreateDocumentResponse where
  toContents CreateDocumentResponse = error "Please do not serialize CreateDocumentResponse"
  parseContents = do
    { _e <- elementNS "CreateDocumentResponse"
    ; return CreateDocumentResponse
    } `adjustErr` ("in <CreateDocumentResponse>, " ++)

data RetrieveDocument = RetrieveDocument String
  deriving (Eq,Ord,Show,Read)

instance HTypeable RetrieveDocument where
    toHType _ = Defined "RetrieveDocument" [] []
instance XmlContent RetrieveDocument where
  toContents (RetrieveDocument format) =
    [CElem (Elem (N "RetrieveDocument") [mkAttr "xmlns" liveDocxNamespace]
      [ mkElemC "format" (toText format)
      ]) ()]
  parseContents = error "Please do not parse a RetrieveDocument"
instance HasXmlNamespace RetrieveDocument where
  xmlNamespace = const "RetrieveDocument"

data RetrieveDocumentResponse = RetrieveDocumentResponse BS.ByteString
  deriving (Eq,Ord,Show,Read)

instance HTypeable RetrieveDocumentResponse where
    toHType _ = Defined "RetrieveDocumentResponse" [] []
instance XmlContent (RetrieveDocumentResponse) where
  toContents (RetrieveDocumentResponse _result) = error "Please do not serialize RetrieveDocumentResponse"
  parseContents =  do
      { e <- elementNS "RetrieveDocumentResponse"
      ; base64 <- interior e $ do
          { result <- elementNS "RetrieveDocumentResult"
          ; interior result $ text
          }
      ; case Base64.decode (BSC.pack base64) of
          Left errmsg -> fail errmsg
          Right value -> return (RetrieveDocumentResponse value)
      } `adjustErr` ("in <RetrieveDocumentResponse>, " ++)
