{-# LANGUAGE OverlappingInstances #-}
module TestKontra (
      TestEnv
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
import Control.Monad.State
import Data.Maybe
import Happstack.Server hiding (mkHeaders, getHeader, method, path)
import Happstack.Server.Internal.Monads
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Map as M
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP

import Crypto.RNG
import DB.Core
import Kontra
import Mails.MailsConfig
import MinutesTime
import IPAddress
import Templates.TemplatesLoader
import qualified MemCache
import User.Locale
import Util.FinishWith
import qualified Data.Map as Map
import qualified Control.Exception.Lifted as E

type TestEnv = CryptoRNGT (DBT IO)

runTestKontraHelper :: Request -> Context -> Kontra a -> TestEnv (a, Context, FilterFun Response)
runTestKontraHelper rq ctx tk = do
  let noflashctx = ctx { ctxflashmessages = [] }
  mres <- mapCryptoRNGT (mapDBT $ \m -> ununWebT $ runServerPartT m rq) $
    runStateT (unKontraPlus $ unKontra tk) noflashctx
  case mres of
    Nothing -> fail "runTestKontraHelper mzero"
    Just (Left _res, _fs) -> fail "This should never happen since we don't use finishWith"
    Just (Right (res, ctx'), fs) -> return (res, ctx', fs)

-- | Typeclass for running handlers within TestKontra monad
class RunnableTestKontra a where
  runTestKontra :: Request -> Context -> Kontra a -> TestEnv (a, Context)

instance RunnableTestKontra a where
  runTestKontra rq ctx tk = do
    (res, ctx', _) <- runTestKontraHelper rq ctx tk
      `E.catch` (\(FinishWith _ _) -> error "finishWith called in function that doesn't return Response")
    return (res, ctx')

instance RunnableTestKontra Response where
  runTestKontra rq ctx tk = do
    (res, ctx', f) <- runTestKontraHelper rq ctx tk
      `E.catch` (\(FinishWith res ctx') -> return (res, ctx', filterFun id))
    return (unFilterFun f res, ctx')

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
        , ctxresourcehostpart = "http://testkontra.fake"
        , ctxflashmessages = []
        , ctxtime = time
        , ctxnormalizeddocuments = docs
        , ctxipnumber = noIP
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
