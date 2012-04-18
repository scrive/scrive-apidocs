module LiveDocx (
    LiveDocxConf(..)
  , convertToPDF
) where

import Control.Exception.Base (bracket_)
import Control.Monad()
import Text.XML.HaXml.XmlContent.Parser
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS

import SOAP.SOAP

import LiveDocxConf

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

uploadTemplate :: LiveDocxConf -> FilePath -> BS.ByteString -> String -> IO (Either String ())
uploadTemplate conf cookiefile filecontents filename = ignoreIfRight =<<
  (makeSoapCallWithCookies
    (url conf)
    cookiefile
    (liveDocxNamespace ++ "UploadTemplate")
    (UploadTemplate filecontents filename) :: IO (Either String UploadTemplateResponse))

setRemoteTemplate :: LiveDocxConf -> FilePath -> String -> IO (Either String ())
setRemoteTemplate conf cookiefile filename = ignoreIfRight =<<
  (makeSoapCallWithCookies
    (url conf)
    cookiefile
    (liveDocxNamespace ++ "SetRemoteTemplate")
    (SetRemoteTemplate filename) :: IO (Either String SetRemoteTemplateResponse))

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

convertToPDF :: LiveDocxConf -> String -> BS.ByteString -> IO (Either String BS.ByteString)
convertToPDF conf uniquefilename filecontents =
  let cookiefile = uniquefilename ++ "_cookies.txt" in
  bracket_ (logIn conf cookiefile) (logOut conf cookiefile) $ do
    _ <- uploadTemplate conf cookiefile filecontents uniquefilename
    _ <- setRemoteTemplate conf cookiefile uniquefilename
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

data UploadTemplate = UploadTemplate BS.ByteString String
  deriving (Eq,Ord,Show,Read)

instance HTypeable UploadTemplate where
    toHType _ = Defined "UploadTemplate" [] []
instance XmlContent UploadTemplate where
  toContents (UploadTemplate template filename) =
    let base64data = BSC.unpack (Base64.encode template) in
    [CElem (Elem "UploadTemplate" [mkAttr "xmlns" liveDocxNamespace]
      [ mkElemC "template" (toText base64data)
      , mkElemC "filename" (toText filename)
      ]) ()]
  parseContents = error "Please do not parse an UploadTemplate"

data UploadTemplateResponse = UploadTemplateResponse
  deriving (Eq,Ord,Show,Read)

instance HTypeable UploadTemplateResponse where
    toHType _ = Defined "UploadTemplateResponse" [] []
instance XmlContent UploadTemplateResponse where
  toContents UploadTemplateResponse = error "Please do not serialize UploadTemplateResponse"
  parseContents = do
  { _e <- elementNS "UploadTemplateResponse"
  ; return UploadTemplateResponse
  } `adjustErr` ("in <UploadTemplateResponse>, " ++)

data SetRemoteTemplate = SetRemoteTemplate String
  deriving (Eq,Ord,Show,Read)

instance HTypeable SetRemoteTemplate where
    toHType _ = Defined "SetRemoteTemplate" [] []
instance XmlContent SetRemoteTemplate where
  toContents (SetRemoteTemplate filename) =
    [CElem (Elem "SetRemoteTemplate" [mkAttr "xmlns" liveDocxNamespace]
      [ mkElemC "filename" (toText filename)
      ]) ()]
  parseContents = error "Please do not parse an SetRemoteTemplate"

data SetRemoteTemplateResponse = SetRemoteTemplateResponse
  deriving (Eq,Ord,Show,Read)

instance HTypeable SetRemoteTemplateResponse where
    toHType _ = Defined "SetRemoteTemplateResponse" [] []
instance XmlContent SetRemoteTemplateResponse where
  toContents SetRemoteTemplateResponse = error "Please do not serialize UploadTemplateResponse"
  parseContents = do
  { _e <- elementNS "SetRemoteTemplateResponse"
  ; return SetRemoteTemplateResponse
  } `adjustErr` ("in <SetRemoteTemplateResponse>, " ++)

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
