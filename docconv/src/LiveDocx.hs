module LiveDocx (
  LiveDocxConf(..)
  , FileFormat(..)
  , convertToPDF
) where

import Control.Monad()
import Control.Monad.Reader
import Text.XML.HaXml.XmlContent.Parser
import System.IO.Temp
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS

import SOAP.SOAP

import LiveDocxConf

data FileFormat = DOC | DOCX | RTF | TXD
  deriving (Eq,Ord,Show,Read)

data LiveDocxContext = LiveDocxContext {
    ctxurl :: String
  , ctxcookiefile :: FilePath
}

type LiveDocx a = ReaderT LiveDocxContext IO (Either String a)

mkLiveDocxContext :: LiveDocxConf -> FilePath -> LiveDocxContext
mkLiveDocxContext conf cookiefile = LiveDocxContext {
    ctxurl = url conf
  , ctxcookiefile = cookiefile
}

makeLiveDocxCall :: (XmlContent request, XmlContent result)
                      => String
                      -> request
                      -> LiveDocx result
makeLiveDocxCall action request = do
  ctx <- ask
  liftIO $ makeSoapCallWithCookies
    (ctxurl ctx)
    (ctxcookiefile ctx)
    (liveDocxNamespace ++ action)
    request

ignoreIfRight :: Either String a -> LiveDocx ()
ignoreIfRight (Left x) = return $ Left x
ignoreIfRight (Right _) = return $ Right ()

logIn :: String -> String -> LiveDocx ()
logIn username password = ignoreIfRight =<<
  (makeLiveDocxCall
    "LogIn"
    (LogIn username password) :: LiveDocx LogInResponse)

logOut :: LiveDocx ()
logOut = ignoreIfRight =<<
  (makeLiveDocxCall
    "LogOut"
    LogOut :: LiveDocx LogOutResponse)

setLocalTemplate :: BS.ByteString -> FileFormat ->  LiveDocx ()
setLocalTemplate filecontents format = ignoreIfRight =<<
  (makeLiveDocxCall
    "SetLocalTemplate"
    (SetLocalTemplate filecontents format) :: LiveDocx SetLocalTemplateResponse)

createDocument ::  LiveDocx ()
createDocument = ignoreIfRight =<<
  (makeLiveDocxCall
    "CreateDocument"
    CreateDocument :: LiveDocx CreateDocumentResponse)

retrieveDocument :: String ->  LiveDocx BS.ByteString
retrieveDocument format = do
  result <- makeLiveDocxCall
              "RetrieveDocument"
              (RetrieveDocument format)
  case result of
    Right (RetrieveDocumentResponse pdfcontents) -> return $ Right pdfcontents
    Left msg -> return $ Left msg

convertToPDF :: LiveDocxConf -> BS.ByteString -> FileFormat -> IO (Either String BS.ByteString)
convertToPDF conf filecontents format =
  withSystemTempFile "livedocx-cookies.txt" $ \cookiefile _cookiefilehandle ->
    liftIO $ runReaderT convertDocument (mkLiveDocxContext conf cookiefile)
  where
    convertDocument :: LiveDocx BS.ByteString
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

liveDocxNamespace :: String
liveDocxNamespace = "http://api.livedocx.com/1.2/mailmerge/"

data LogIn = LogIn String String
  deriving (Eq,Ord,Show,Read)

instance HTypeable LogIn where
    toHType _ = Defined "LogIn" [] []
instance XmlContent LogIn where
  toContents (LogIn username password) =
    [CElem (Elem "LogIn" [mkAttr "xmlns" liveDocxNamespace]
      [ mkElemC "username" (toText username)
      , mkElemC "password" (toText password)
      ]) ()]
  parseContents = error "Please do not parse a LogIn"

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
    [CElem (Elem "LogOut" [mkAttr "xmlns" liveDocxNamespace] []) ()]
  parseContents = error "Please do not parse a LogOut"

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
    [CElem (Elem "SetLocalTemplate" [mkAttr "xmlns" liveDocxNamespace]
      [ mkElemC "template" (toText base64data)
      , mkElemC "format" (toText $ show format)
      ]) ()]
  parseContents = error "Please do not parse an SetLocalTemplate"

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
    [CElem (Elem "CreateDocument" [mkAttr "xmlns" liveDocxNamespace] []) ()]
  parseContents = error "Please do not parse a CreateDocument"

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
    [CElem (Elem "RetrieveDocument" [mkAttr "xmlns" liveDocxNamespace]
      [ mkElemC "format" (toText format)
      ]) ()]
  parseContents = error "Please do not parse a RetrieveDocument"

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
