module LiveDocx (
    LiveDocxConf(..)
  , convertToPDF
) where

import Control.Concurrent
import Control.Monad()
import Data.List
import Text.XML.HaXml.XmlContent.Parser
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS

import SOAP.SOAP
import qualified Log

data LiveDocxConf = LiveDocxConf {
    url :: String
  , username :: String
  , password :: String
}

data LiveDocx a = LDXSuccess a | LDXProblem String

instance Monad LiveDocx where
  return = LDXSuccess

  LDXSuccess x >>= f = f x
  LDXProblem msg >>= _ = LDXProblem msg

liveDocxToEither :: LiveDocx a -> Either String a
liveDocxToEither (LDXSuccess x) = Right x
liveDocxToEither (LDXProblem msg) = Left msg

logIn :: LiveDocxConf -> LiveDocx ()
logIn = undefined

logOut :: LiveDocx ()
logOut = undefined

uploadTemplate :: BS.ByteString -> String -> LiveDocx ()
uploadTemplate = undefined

setRemoteTemplate :: String -> LiveDocx ()
setRemoteTemplate = undefined

createDocument :: LiveDocx ()
createDocument = undefined

retrieveDocument :: String -> LiveDocx BS.ByteString
retrieveDocument = undefined

--TODO: make like a try finally
withLiveDocxAccount :: LiveDocxConf -> LiveDocx a -> LiveDocx a
withLiveDocxAccount conf action = do
  _ <- logIn conf
  result <- action
  _ <- logOut
  return result

convertToPDF :: LiveDocxConf -> String -> BS.ByteString -> Either String BS.ByteString
convertToPDF conf uniquefilename filecontents = liveDocxToEither . withLiveDocxAccount conf $ do
    _ <- uploadTemplate filecontents uniquefilename
    _ <- setRemoteTemplate uniquefilename
    _ <- createDocument
    retrieveDocument "PDF"

{- |
    Definitions of all the calls and responses we want to use.
    The documentation for these is here: https://api.livedocx.com/1.2/mailmerge.asmx?wsdl
-}

liveDocxNamespace = "http://api.livedocx.com/1.2/mailmerge/"

data LogIn = LogIn String String
  deriving (Eq,Ord,Show,Read)

instance HTypeable LogIn where
    toHType _ = Defined "LogIn" [] []
instance XMLContent LogIn where
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
instance XMLContent LogOut where
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
instance XMLContent UploadTemplate where
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
  ; return LogOutResponse
  } `adjustErr` ("in <UploadTemplateResponse>, " ++)

data SetRemoteTemplate = SetRemoteTemplate String
  deriving (Eq,Ord,Show,Read)

instance HTypeable SetRemoteTemplate where
    toHType _ = Defined "SetRemoteTemplate" [] []
instance XMLContent SetRemoteTemplate where
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
  ; return LogOutResponse
  } `adjustErr` ("in <SetRemoteTemplateResponse>, " ++)

data CreateDocument = CreateDocument
  deriving (Eq,Ord,Show,Read)

instance HTypeable CreateDocument where
    toHType _ = Defined "CreateDocument" [] []
instance XMLContent CreateDocument where
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
  ; return LogOutResponse
  } `adjustErr` ("in <CreateDocumentResponse>, " ++)

data RetrieveDocument = RetrieveDocument String
  deriving (Eq,Ord,Show,Read)

instance HTypeable RetrieveDocument where
    toHType _ = Defined "RetrieveDocument" [] []
instance XMLContent RetrieveDocument where
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
