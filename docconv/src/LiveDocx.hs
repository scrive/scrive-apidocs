module LiveDocx (
  LiveDocxConf(..)
  , FileFormat(..)
  , convertToPDF
) where

import Control.Exception.Base (bracket_)
import Control.Monad()
import Text.XML.HaXml.XmlContent.Parser
import System.IO.Temp
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS

import SOAP.SOAP

import LiveDocxConf

data FileFormat = DOC | DOCX | RTF | TXD
  deriving (Eq,Ord,Show,Read)

ignoreIfRight :: Either String a -> IO (Either String ())
ignoreIfRight (Left x) = return $ Left x
ignoreIfRight (Right _) = return $ Right ()

logIn :: LiveDocxConf -> FilePath -> IO (Either String ())
logIn conf cookiefile = ignoreIfRight =<<
  (makeSoapCallWithCookies
    (url conf)
    cookiefile
    (liveDocxNamespace ++ "LogIn")
    (LogIn (username conf) (password conf)) :: IO (Either String LogInResponse))

logOut :: LiveDocxConf -> FilePath -> IO (Either String ())
logOut conf cookiefile = ignoreIfRight =<<
  (makeSoapCallWithCookies
    (url conf)
    cookiefile
    (liveDocxNamespace ++ "LogOut")
    LogOut :: IO (Either String LogOutResponse))

setLocalTemplate :: LiveDocxConf -> FilePath -> BS.ByteString -> FileFormat -> IO (Either String ())
setLocalTemplate conf cookiefile filecontents format = ignoreIfRight =<<
  (makeSoapCallWithCookies
    (url conf)
    cookiefile
    (liveDocxNamespace ++ "SetLocalTemplate")
    (SetLocalTemplate filecontents format) :: IO (Either String SetLocalTemplateResponse))

createDocument :: LiveDocxConf -> FilePath -> IO (Either String ())
createDocument conf cookiefile = ignoreIfRight =<<
  (makeSoapCallWithCookies
    (url conf)
    cookiefile
    (liveDocxNamespace ++ "CreateDocument")
    CreateDocument :: IO (Either String CreateDocumentResponse))

retrieveDocument :: LiveDocxConf -> FilePath -> String -> IO (Either String BS.ByteString)
retrieveDocument conf cookiefile format = do
  result <- makeSoapCallWithCookies
              (url conf)
              cookiefile
              (liveDocxNamespace ++ "RetrieveDocument")
              (RetrieveDocument format)
  case result of
    Right (RetrieveDocumentResponse pdfcontents) -> return $ Right pdfcontents
    Left msg -> return $ Left msg

convertToPDF :: LiveDocxConf -> BS.ByteString -> FileFormat -> IO (Either String BS.ByteString)
convertToPDF conf filecontents format =
  withSystemTempFile "livedocx-cookies.txt" $ \cookiefile _cookiefilehandle ->
    bracket_ (logIn conf cookiefile) (logOut conf cookiefile) $ do
      _ <- setLocalTemplate conf cookiefile filecontents format
      _ <- createDocument conf cookiefile
      retrieveDocument conf cookiefile "PDF"

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
