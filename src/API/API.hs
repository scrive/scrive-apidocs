{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  API.API
-- Maintainer  :  all
-- Stability   :  development
-- Portability :  portable
--
-- Schema for all api calls
-----------------------------------------------------------------------------
module API.API where

--import ELegitimation.BankID as BankID
import ActionSchedulerState
import Control.Monad (msum, mzero)
import Control.Monad.State
import Control.Monad.Trans (liftIO,lift)
import Control.Concurrent
import Data.Functor
import AppView as V
import Control.Concurrent
import Crypto
import Data.List
import Data.Maybe
import Doc.DocState
import HSP.XML (cdata)
import Happstack.Server hiding (simpleHTTP,host,body)
import Happstack.State (query)
import InspectXML
import InspectXMLInstances
import KontraLink
import MinutesTime
import Misc
import Network.Socket
import Session
import System.IO.Unsafe
import Kontra
import qualified User.UserControl as UserControl
import User.UserView as UserView
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.UTF8 as BS
import qualified Doc.DocControl as DocControl
import qualified HSP as HSP
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import qualified Network.AWS.AWSConnection as AWS
import qualified TrustWeaver as TW
import qualified Payments.PaymentsControl as Payments
import qualified Contacts.ContactsControl as Contacts
import qualified ELegitimation.BankID as BankID
import Templates.Templates (readTemplates, renderTemplate, KontrakcjaTemplates, getTemplatesModTime)
import qualified Administration.AdministrationControl as Administration
import Control.Concurrent.MVar
import Mails.MailsConfig
import Mails.SendGridEvents
import Mails.SendMail
import System.Log.Logger (Priority(..), logM)
import qualified FlashMessage as F
import qualified AppLogger as Log (error, security, debug)
import qualified MemCache
import Happstack.State (update)
import Redirect
import PayEx.PayExInterface -- Import so at least we check if it compiles
import InputValidation
import System.Directory
import ListUtil
import Data.Word
import System.Time
import Text.JSON
import Text.JSON.String
import Control.Monad.Reader
import Control.Monad.Error
{- | 
  Defines the application's configuration.  This includes amongst other things
  the http port number, amazon, trust weaver and email configuraton,
  as well as a handy boolean indicating whether this is a production or
  development instance.
-}

type APIResponse = JSObject JSValue
type APIRequestBody = JSValue

type APIFunction c a = ReaderT c (ErrorT (API_ERROR,String) Kontra') a

apiResponse ::  Kontra APIResponse ->  Kontra Response   
apiResponse action = action >>= simpleResponse . encode                         

class APICall a where
    apiCall :: String -> a -> Kontra Response
    
instance APICall (Kontra APIResponse) where
    apiCall s action = dir "api" $ dir s $ do
                    methodM POST 
                    apiResponse action
                    
    
instance (APIContext c) => APICall (APIFunction c APIResponse) where
    apiCall s action = apiCall s $ do
        mcontext <- apiContext
        case mcontext  of
             Right apicontext -> fmap (either (uncurry apiError) id) $ runErrorT $ runReaderT action apicontext
             Left emsg -> return $ uncurry apiError emsg


class APIContext a where
    apiContext::Kontra (Either (API_ERROR,String) a)       
    body:: a -> APIRequestBody
    newBody:: APIRequestBody -> a -> a
        
apiUnknownCall ::  Kontra Response        
apiUnknownCall = dir "api" $ apiResponse $  return $ apiError API_ERROR_UNNOWN_CALL "Bad request"
   
-- Digging into request params   
apiAskBS::(APIContext c) => String -> APIFunction c (Maybe BS.ByteString)
apiAskBS =  fmap (fmap BS.fromString) . apiAskString

apiAskString::(APIContext c) => String -> APIFunction c (Maybe String)
apiAskString s = apiLocal s (fromString <$> askBody)
    where
        fromString (JSString string) = Just $ fromJSString string
        fromString _ = Nothing 

apiAskStringMap::(APIContext c) => APIFunction c (Maybe [(String,String)])
apiAskStringMap = join <$> fmap (foldl getStr (Just [])) <$> apiAskMap
    where
        getStr (Just l) (key,JSString string) = Just $ (key,fromJSString string):l
        getStr _ _ = Nothing
        
apiAskMap::(APIContext c) => APIFunction c (Maybe [(String,JSValue)])
apiAskMap = fromObject <$> askBody
    where
        fromObject (JSObject object) = Just $ fromJSObject object
        fromObject _ =Nothing 
  
apiAskList:: (APIContext c) => APIFunction c (Maybe [JSValue])
apiAskList = fromList <$> askBody
    where
        fromList (JSArray list) = Just $ list
        fromList _ = Nothing 

apiLocal ::(APIContext c) => String -> APIFunction c (Maybe a) -> APIFunction c (Maybe a)
apiLocal s digger= do
    mobj <- apiAskField s
    case mobj of
         Just obj ->  withReaderT (newBody obj) digger
         Nothing -> return Nothing
         
apiMapLocal :: (APIContext c) => APIFunction c (Maybe a) -> APIFunction c (Maybe [a])
apiMapLocal digger = do
    mdiggers <- (fmap $ map (\nbody -> withReaderT (newBody nbody) digger)) <$> apiAskList
    case mdiggers of
         Just diggers -> runDiggers $ diggers
         Nothing -> return Nothing
     where
         runDiggers (d:ds) = do 
             mres <- d
             case mres of 
                 Just res -> do
                     mress <- runDiggers ds
                     case mress of
                         Just ress -> return $ Just (res:ress)
                         _ -> return Nothing 
                 _ -> return Nothing        
         runDiggers _ = return $ Just []
         
apiAskField:: (APIContext c) => String -> APIFunction c (Maybe JSValue)
apiAskField s = fromObject <$> askBody
    where 
      fromObject (JSObject object) = lookup s $ fromJSObject object
      fromObject _ = Nothing 

askBody :: (APIContext c) => APIFunction c APIRequestBody
askBody = body <$> ask

askKontraContext :: (APIContext c) => APIFunction c Context
askKontraContext = lift $ lift $ get


--- AUTORIZE AND BUILDING CONTEXT
apiBody ::  Kontra (Either String JSValue)
apiBody = do
      body <- getFieldWithDefault "" "body"
      return $ runGetJSON readJSObject body
             
--- ERROR Response

-- This is how we represent errors to the user
apiError::API_ERROR -> String ->  APIResponse
apiError code message= toJSObject [
      ("error" , showJSON $ fromEnum code)
    , ("error_message", showJSON message)
    ]

instance Error (API_ERROR,String) where
    strMsg s = (API_ERROR_OTHER,s)
    
--This will break the execution and send error message to he user
throwApiError:: (APIContext c) => API_ERROR -> String -> APIFunction c a
throwApiError = curry throwError 



data API_ERROR =   API_ERROR_LOGIN 
                 | API_ERROR_UNNOWN_CALL 
                 | API_ERROR_PARSING
                 | API_ERROR_NO_USER
                 | API_ERROR_NO_DOCUMENT
                 | API_ERROR_PERMISSION_ACCESS
                 | API_ERROR_PERMISSION_ACTION
                 | API_ERROR_ILLEGAL_VALUE
                 | API_ERROR_MISSING_VALUE
                 | API_ERROR_OTHER
                 
                 
instance  Enum API_ERROR where
    fromEnum API_ERROR_LOGIN = 101
    fromEnum API_ERROR_UNNOWN_CALL = 102    
    fromEnum API_ERROR_PARSING = 103
    fromEnum API_ERROR_NO_USER = 104
    fromEnum API_ERROR_NO_DOCUMENT = 105
    fromEnum API_ERROR_PERMISSION_ACCESS = 106
    fromEnum API_ERROR_PERMISSION_ACTION = 107
    fromEnum API_ERROR_ILLEGAL_VALUE = 107
    fromEnum API_ERROR_MISSING_VALUE = 109
    fromEnum API_ERROR_OTHER = 500
