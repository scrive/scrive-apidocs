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

import Context
import Crypto.RNG (CryptoRNG, getCryptoRNGState)
import DB.Classes
import KontraMonad
import Mails.MailsConfig
import MinutesTime
import Misc (unknownIPAddress)
import Templates.Templates
import qualified MemCache
import User.Locale
import qualified Data.Map as Map

-- | Monad that emulates the server
newtype TestKontra a = TK { unTK :: ErrorT Response (ReaderT Request (StateT (Context, Response -> Response) IO)) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadPlus)

instance Kontrakcja TestKontra

instance CryptoRNG TestKontra where
  getCryptoRNGState = TK $ gets (rngstate . ctxdbenv . fst)

instance DBMonad TestKontra where
  getDBEnv = ctxdbenv <$> getContext
  handleDBError e = finishWith =<< (internalServerError $ toResponse $ show e)

instance TemplatesMonad TestKontra where
    getTemplates = ctxtemplates <$> getContext
    getLocalTemplates locale = do
      Context{ctxglobaltemplates} <- getContext
      return $ localizedVersion locale ctxglobaltemplates

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
    runTestKontra :: MonadIO m => Request -> Context -> TestKontra a -> m (a, Context)

instance RunnableTestKontra a where
    runTestKontra rq ctx tk = do
        let noflashctx = ctx{ctxflashmessages=[]}
        (mres, (ctx', _)) <- liftIO $ runStateT (runReaderT (runErrorT $ unTK tk) rq) (noflashctx, id)
        case mres of
             Right res -> return (res, ctx')
             Left  _   -> error "finishWith called in function that doesn't return Response"

instance RunnableTestKontra Response where
    runTestKontra rq ctx tk = do
        let noflashctx = ctx{ctxflashmessages=[]}
        (mres, (ctx', f)) <- liftIO $ runStateT (runReaderT (runErrorT $ unTK tk) rq) (noflashctx, id)
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
mkRequest :: MonadIO m => Method -> [(String, Input)] -> m Request
mkRequest method vars = liftIO $ do
    rqbody <- newEmptyMVar
    ib <- if isReqPost
             then newMVar vars
             else newEmptyMVar
    let iq = if isReqPost
                then []
                else vars
    return Request {
          rqSecure = False
        , rqMethod = POST
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
mkContext :: MonadIO m => Locale -> KontrakcjaGlobalTemplates -> m Context
mkContext locale globaltemplates = liftIO $ do
    docs <- newMVar M.empty
    memcache <- MemCache.new BS.length 52428800
    time <- getMinutesTime
    return Context {
          ctxmaybeuser = Nothing
        , ctxhostpart = "http://testkontra.fake"
        , ctxflashmessages = []
        , ctxtime = time
        , ctxnormalizeddocuments = docs
        , ctxipnumber = unknownIPAddress
        , ctxdbenv = error "dbenv is not defined"
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
        , ctxtemplates = localizedVersion locale globaltemplates
        , ctxglobaltemplates = globaltemplates
        , ctxlocale = locale
        , ctxlocaleswitch = False
        , ctxmailsconfig = defaultMailsConfig
        , ctxtwconf = error "twconf is not defined"
        , ctxelegtransactions = []
        , ctxfilecache = memcache
        , ctxxtoken = error "xtoken is not defined"
        , ctxcompany = Nothing
        , ctxservice = Nothing
        , ctxlocation = error "location is not defined"
        , ctxadminaccounts = []
        , ctxsalesaccounts = []
        , ctxmagichashes = Map.empty
        , ctxmaybepaduser = Nothing
    }
