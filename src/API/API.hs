-----------------------------------------------------------------------------
-- |
-- Module      :  API.API
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Schema for all api calls. API calls are working as reader some APIContext. 
-- API context contains (at least) JSON Object. Look at IntegrationAPI or WebShopAPI 
-- for some examples.
-----------------------------------------------------------------------------
module API.API(
     -- Main stuff to build new API instance  
       APIFunction
     , APIRequestBody  
     , APIResponse  
     , APIContext(..)
     , APICall(..)
     , apiUnknownCall
     -- Diggers for JSON embedded params 
     , apiAskBS
     , apiAskString
     , apiAskInteger
     , apiAskBase64
     , apiAskStringMap
     , apiAskMap
     , apiMapLocal
     , apiLocal
     -- Asking about Kontra context
     , askKontraContext
     -- Standard way of getting JSON request body
     , apiBody
     , apiError
     , apiResponse
     -- Errors 
     , throwApiError
     , API_ERROR(..)
      ) where


import Control.Monad.State
import Data.Functor
import AppView as V
import Happstack.Server hiding (simpleHTTP,host,body)
import Misc
import Kontra
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Text.JSON
import Text.JSON.String
import Control.Monad.Reader
import Control.Monad.Error
import Data.Ratio
import qualified Data.ByteString.Base64 as BASE64
import qualified AppLogger as Log (debug)

{- | API calls user JSPO object as a response and work within json value as a context-}
type APIResponse = JSObject JSValue
type APIRequestBody = JSValue

{- | API functions are build over Kontra with a ability to exit, and with some context -}
type APIFunction c a = ReaderT c (ErrorT (API_ERROR,String) Kontra') a

{- |  Used to convert json object to HTTP response-} 
apiResponse ::  Kontra APIResponse ->  Kontra Response   
apiResponse action = action >>= simpleResponse . encode                         

{- | Used to build api routing tables
     Routing table could look like
          dir "myapi" msum [
                apiCall "apicall1" action1
              , apiCall "apicall2" action2
              , apiCall "apicall3" action3
              , apiUnknownCall
          ]
    APICall can be build from Kontra or from APIFunction over some APIContext,
    as long as they hold response JSON inside.
    
-}
class APICall a where
    apiCall :: String -> a -> Kontra Response
    
instance APICall (Kontra APIResponse) where
    apiCall s action = dir "api" $ dir s $ do
                    methodM POST 
                    Log.debug $ "API call " ++ s ++ " matched"
                    apiResponse action


instance (APIContext c) => APICall (APIFunction c APIResponse) where
    apiCall s action = apiCall s $ do
        mcontext <- apiContext
        case mcontext  of
             Right apicontext -> do
                 res <- fmap (either (uncurry apiError) id) $ runErrorT $ runReaderT action apicontext
                 Log.debug $ "API call result: " ++ show res
                 return res
             Left emsg -> return $ uncurry apiError emsg

{- | Also for routing tables, to mark that api calls did not match and not to fall to mzero-}
apiUnknownCall ::  Kontra Response        
apiUnknownCall = dir "api" $ apiResponse $  return $ apiError API_ERROR_UNNOWN_CALL "Bad request"




{- Each API can have its own context. For example WebShop has a context containing user, 
   while integration API has a context containing service. 
   But each context has to be able get read from HTTP params and should have JSON object inside.
-}

class APIContext a where
    apiContext::Kontra (Either (API_ERROR,String) a)       
    body:: a -> APIRequestBody
    newBody:: APIRequestBody -> a -> a
        
  
-- When building API calls you wan't to dig into JSON object with params
-- Here are some helper functions for that. Some can be localized (like in ReaderMonad) to subcontext.

apiAskBS::(APIContext c) => String -> APIFunction c (Maybe BS.ByteString)
apiAskBS =  fmap (fmap BS.fromString) . apiAskString

apiAskString::(APIContext c) => String -> APIFunction c (Maybe String)
apiAskString s = apiLocal s (fromString <$> askBody)
    where
        fromString (JSString string) = Just $ fromJSString string
        fromString _ = Nothing 

apiAskInteger::(APIContext c) => String -> APIFunction c (Maybe Integer)
apiAskInteger s = apiLocal s (fromNumerator <$> askBody)
    where
        fromNumerator (JSRational _ r) = Just $ numerator r
        fromNumerator _ = Nothing 
        
apiAskBase64::(APIContext c) => String -> APIFunction c (Maybe BS.ByteString)        
apiAskBase64 s =  do
    coded <- (fmap $ BASE64.decode)  <$> apiAskBS s 
    case coded of 
         Just (Right r) -> return $ Just r
         _ -> return Nothing 


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


--- This will read JSON object from some HTTP request body parameter
apiBody ::  Kontra (Either String JSValue)
apiBody = runGetJSON readJSObject <$> getFieldWithDefault "" "body"
             
--- ERROR Response

-- Building pure api error.
apiError::API_ERROR -> String ->  APIResponse
apiError code message= toJSObject [
      ("error" , showJSON $ fromSafeEnum code)
    , ("error_message", showJSON message)
    ]

instance Error (API_ERROR,String) where
    strMsg s = (API_ERROR_OTHER,s)
    
--This will break the execution and send error message to the user
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
                 
                 
instance SafeEnum API_ERROR where
    fromSafeEnum API_ERROR_LOGIN = 101
    fromSafeEnum API_ERROR_UNNOWN_CALL = 102    
    fromSafeEnum API_ERROR_PARSING = 103
    fromSafeEnum API_ERROR_NO_USER = 104
    fromSafeEnum API_ERROR_NO_DOCUMENT = 105
    fromSafeEnum API_ERROR_PERMISSION_ACCESS = 106
    fromSafeEnum API_ERROR_PERMISSION_ACTION = 107
    fromSafeEnum API_ERROR_ILLEGAL_VALUE = 107
    fromSafeEnum API_ERROR_MISSING_VALUE = 109
    fromSafeEnum API_ERROR_OTHER = 500
    toSafeEnum _ = Nothing

-- Other constants used for api calls

