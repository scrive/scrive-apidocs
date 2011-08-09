{-# LANGUAGE OverlappingInstances #-}
module TestKontra (
      TestKontra
    , runTestKontra
    , inText
    , inFile
    , mkHeaders
    , mkCookies
    , getHeader
    , getCookie
    , mkRequest
    , mkContext
    , testAPI
    , isFlashOfType
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.List
import Happstack.Server hiding (mkHeaders, getHeader, method, path)
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Map as M
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP

import API.API
import Context
import DB.Classes
import FlashMessage
import KontraMonad
import MinutesTime
import Templates.Templates
import qualified MemCache

-- | Monad that emulates the server
newtype TestKontra a = TK { unTK :: ErrorT Response (ReaderT Request (StateT (Context, Response -> Response) IO)) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadPlus)

instance Kontrakcja TestKontra

instance DBMonad TestKontra where
  getConnection = ctxdbconn <$> getContext
  handleDBError e = finishWith =<< (internalServerError $ toResponse $ show e)

instance TemplatesMonad TestKontra where
    getTemplates = ctxtemplates <$> getContext

instance KontraMonad TestKontra where
    getContext    = TK $ fst <$> get
    modifyContext = TK . modify . first

instance ServerMonad TestKontra where
    askRq     = TK $ ask
    localRq f = TK . local f . unTK

instance HasRqData TestKontra where
    askRqEnv = do
        rq <- askRq
        mbi <- liftIO $ if rqMethod rq == POST || rqMethod rq == PUT
                           then readInputsBody rq
                           else return $ Just []
        return (rqInputsQuery rq, mbi, rqCookies rq)
    rqDataError (Errors msgs) = fail $ intercalate " || " msgs
    localRqEnv f m = do
        rq <- askRq
        b <- liftIO $ readInputsBody rq
        let (q', b', c') = f (rqInputsQuery rq, b, rqCookies rq)
        bv <- liftIO $ newMVar $ fromMaybe [] b'
        let rq' = rq {
              rqInputsQuery = q'
            , rqInputsBody = bv
            , rqCookies = c'
        }
        localRq (const rq') m

instance FilterMonad Response TestKontra where
    setFilter f     = TK $ modify $ \(ctx, _) -> (ctx, f)
    composeFilter f = TK $ modify $ \(ctx, g) -> (ctx, f . g)
    getFilter m     = TK $ do
        f <- snd <$> get
        unTK m >>= \x -> return (x, f)

instance WebMonad Response TestKontra where
    finishWith = TK . throwError

-- | Typeclass for running handlers within TestKontra monad
class RunnableTestKontra a where
    runTestKontra :: Request -> Context -> TestKontra a -> IO (a, Context)

instance RunnableTestKontra a where
    runTestKontra rq ctx tk = do
        (mres, (ctx', _)) <- runStateT (runReaderT (runErrorT $ unTK tk) rq) (ctx, id)
        case mres of
             Right res -> return (res, ctx')
             Left  _   -> error "finishWith called in function that doesn't return Response"

instance RunnableTestKontra Response where
    runTestKontra rq ctx tk = do
        (mres, (ctx', f)) <- runStateT (runReaderT (runErrorT $ unTK tk) rq) (ctx, id)
        case mres of
             Right res -> return (f res, ctx')
             Left  res -> return (f res, ctx')

-- Various helpers for constructing appropriate Context/Request

-- | Creates GET/POST input text variable
inText :: String -> Input
inText value = Input {
      inputValue = Right $ BSLU.fromString value
    , inputFilename = Nothing
    , inputContentType = ContentType {
          ctType = "text"
        , ctSubtype = "plain"
        , ctParameters = []
        }
    }

-- | Creates GET/POST input file variable
inFile :: FilePath -> Input
inFile path = Input {
      inputValue = Left path
    , inputFilename = Just $ takeFileName path
    , inputContentType = ContentType {
          ctType = "application"
        , ctSubtype = "octet-stream"
        , ctParameters = []
        }
    }

-- | Constructs headers from list of string pairs
mkHeaders :: [(String, [String])] -> Headers
mkHeaders = M.fromList . map (f . g)
    where
        g = BSU.fromString *** map BSU.fromString
        f (name, values) = (name, HeaderPair {
              hName = name
            , hValue = values
        })

-- | Constructs cookies from list of string pairs
mkCookies :: [(String, String)] -> [(String, Cookie)]
mkCookies = map (\(n, v) -> (n, mkCookie n v))

-- | Retrieves specific header value
getHeader :: String -> Headers -> Maybe String
getHeader name hdrs = BSU.toString <$> join
    (listToMaybe . hValue <$> M.lookup (BSU.fromString name) hdrs)

-- | Retrieves specific cookie value
getCookie :: String -> [(String, Cookie)] -> Maybe String
getCookie name cookies = cookieValue <$> lookup name cookies

-- | Constructs initial request with given data (POST or GET)
mkRequest :: Method -> [(String, Input)] -> IO Request
mkRequest method vars = do
    rqbody <- newEmptyMVar
    ib <- if isReqPost
             then newMVar vars
             else newEmptyMVar
    let iq = if isReqPost
                then []
                else vars
    return Request {
          rqMethod = POST
        , rqPaths = []
        , rqUri = ""
        , rqQuery = ""
        , rqInputsQuery = iq
        , rqInputsBody = ib
        , rqCookies = []
        , rqVersion = HttpVersion 1 1
        , rqHeaders = M.empty
        , rqBody = rqbody
        , rqPeer = ("", 0)
    }
    where
        isReqPost = method == POST || method == PUT

-- | Constructs initial context with given templates
mkContext :: KontrakcjaTemplates -> IO Context
mkContext templates = do
    docs <- newMVar M.empty
    enforcer <- newEmptyMVar
    memcache <- MemCache.new BS.length 52428800
    time <- getMinutesTime
    return Context {
          ctxmaybeuser = Nothing
        , ctxhostpart = "http://testkontra.fake"
        , ctxflashmessages = []
        , ctxtime = time
        , ctxnormalizeddocuments = docs
        , ctxipnumber = 0
        , ctxdbconn = error "db connection is not defined"
        , ctxdocstore = error "docstore is not defined"
        , ctxs3action = AWS.S3Action {
              AWS.s3conn = AWS.amazonS3Connection "" ""
            , AWS.s3bucket = ""
            , AWS.s3object = ""
            , AWS.s3query = ""
            , AWS.s3metadata = []
            , AWS.s3body = BSL.empty
            , AWS.s3operation = HTTP.GET
        }
        , ctxgscmd = "gs"
        , ctxproduction = False
        , ctxtemplates = templates
        , ctxesenforcer = enforcer
        , ctxtwconf = error "twconf is not defined"
        , ctxelegtransactions = []
        , ctxfilecache = memcache
        , ctxxtoken = error "xtoken is not defined"
        , ctxcompany = Nothing
        , ctxservice = Nothing
        , ctxlocation = error "location is not defined"
        , ctxadminaccounts = []
    }

-- | Runs API function and returns its json response
testAPI :: (APIContext c, Kontrakcja m) => APIFunction m c APIResponse -> m APIResponse
testAPI f = do
    methodM POST
    mcontext <- apiContext
    case mcontext of
         Right apictx -> either (uncurry apiError) id <$> runApiFunction f apictx
         Left emsg -> return $ uncurry apiError emsg

-- Various helpers

-- | Checks type of flash message
isFlashOfType :: FlashMessage -> FlashType -> Bool
isFlashOfType (FlashMessage ft _) t = ft == t
isFlashOfType (FlashTemplate ft _ _) t = ft == t
