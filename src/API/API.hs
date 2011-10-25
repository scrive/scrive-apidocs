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
       APIFunction(unAF)
     , APIRequestBody
     , APIResponse
     , APIContext(..)
     --, APICall(..)
     , apiCall
     , liftKontra
     , runApiFunction
     , apiUnknownCall
     , apiError
     , apiResponse
     -- Errors
     , throwApiError
     , API_ERROR(..)
      ) where


import Control.Monad.State
import Data.Functor
import AppView as V
import Happstack.Server (Response, Method(POST))
import Happstack.StaticRouting (Route, Path, dir, path, remainingPath)
import Misc
import KontraMonad
import Text.JSON
import Control.Monad.Reader
import Control.Monad.Error
import DB.Classes
import Templates.Templates
import qualified AppLogger as Log
import Util.JSON

{- | API calls user JSPO object as a response and work within json value as a context-}
type APIResponse = JSObject JSValue
type APIRequestBody = JSValue

{- | API functions are build over Kontra with an ability to exit, and with some context -}
newtype APIFunction m c a = AF { unAF :: ReaderT c (ErrorT (API_ERROR, String) m) a }
    deriving (Functor, Monad, MonadError (API_ERROR, String), MonadIO, MonadReader c)

instance (APIContext c, Kontrakcja m) => DBMonad (APIFunction m c) where
    getConnection = liftKontra getConnection
    handleDBError e = do
      Log.error $ show e
      throwApiError API_ERROR_OTHER "Database problem"

instance Kontrakcja m => TemplatesMonad (APIFunction m c) where
    getTemplates = liftKontra getTemplates

instance Kontrakcja m => KontraMonad (APIFunction m c) where
    getContext    = liftKontra getContext
    modifyContext = liftKontra . modifyContext

runApiFunction :: Kontrakcja m => APIFunction m c a -> c -> m (Either (API_ERROR, String) a)
runApiFunction f ctx = runErrorT $ runReaderT (unAF f) ctx

liftKontra :: Kontrakcja m => m a -> APIFunction m c a
liftKontra = AF . lift . lift

{- |  Used to convert json object to HTTP response-}
apiResponse :: Kontrakcja m => m APIResponse -> m Response
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

apiCall :: (APIContext c, Kontrakcja m, Path m (m Response) Response Response) => 
           String -> APIFunction m c APIResponse -> Route (m Response)
apiCall s f = dir s $ path POST id $ do
    Log.debug $ "API call " ++ s ++ " matched"
    apiResponse $ do
        mcontext <- apiContext
        case mcontext  of
             Right apictx -> do
                 res <- either (uncurry apiError) id <$> runApiFunction f apictx
                 Log.debug $ "API call result: " ++ encode res
                 return res
             Left emsg -> return $ uncurry apiError emsg

{- | Also for routing tables, to mark that api calls did not match and not to fall to mzero-}
apiUnknownCall :: (Kontrakcja m, Path m (m Response) Response Response) => Route (m Response)
apiUnknownCall = remainingPath POST $ apiResponse $ return $ apiError API_ERROR_UNNOWN_CALL "Bad request"



{- Each API can have its own context. For example WebShop has a context containing user,
   while integration API has a context containing service.
   But each context has to be able get read from HTTP params and should have JSON object inside.
-}

class (JSONContainer a) =>  APIContext a where
    apiContext :: Kontrakcja m => m (Either (API_ERROR,String) a)

--- ERROR Response

-- Building pure api error.
apiError :: API_ERROR -> String -> APIResponse
apiError code message= toJSObject [
      ("error" , showJSON $ fromSafeEnum code)
    , ("status" , showJSON $ "error")
    , ("error_message", showJSON message)
    ]

instance Error (API_ERROR,String) where
    strMsg s = (API_ERROR_OTHER,s)

--This will break the execution and send error message to the user
throwApiError :: (APIContext c, Kontrakcja m) => API_ERROR -> String -> APIFunction m c a
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

