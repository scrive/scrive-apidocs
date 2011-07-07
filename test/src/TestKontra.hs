{-# LANGUAGE OverlappingInstances #-}
module TestKontra {-(
      TestKontra(unTK)
    , RunnableTestKontra(..)
    , inText
    , inFile
    , headers
    , cookies
    , request
    , context
    )-} where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.List
import Happstack.Server hiding (method, path)
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
import KontraMonad
import MinutesTime
import Templates.Templates
import qualified MemCache

import System.IO.Temp
import User.UserState
import User.Password
import StateHelper
import Templates.TemplatesLoader
import Happstack.State hiding (Method)
import API.MailAPI
import API.API
import AppControl
import Redirect

-- | Monad that emulates the server
newtype TestKontra a = TK { unTK :: ReaderT Request (StateT (Context, Response -> Response) IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadPlus)

instance Kontrakcja TestKontra

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
        return (rqInputsQuery rq, fromMaybe [] mbi, rqCookies rq)
    rqDataError (Errors msgs) = fail $ intercalate " || " msgs
    localRqEnv f m = do
        rq <- askRq
        b <- fromMaybe [] <$> (liftIO $ readInputsBody rq)
        let (q', b', c') = f (rqInputsQuery rq, b, rqCookies rq)
        bv <- liftIO $ newMVar b'
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

-- | Typeclass for running handlers within TestKontra monad
class RunnableTestKontra a where
    runTestKontra :: Request -> Context -> TestKontra a -> IO (a, Context)

instance RunnableTestKontra a where
    runTestKontra rq ctx tk = do
        (res, (ctx', _)) <- runStateT (runReaderT (unTK tk) rq) (ctx, id)
        return (res, ctx')

instance RunnableTestKontra Response where
    runTestKontra rq ctx tk = do
        (res, (ctx', f)) <- runStateT (runReaderT (unTK tk) rq) (ctx, id)
        return (f res, ctx')

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
headers :: [(String, [String])] -> Headers
headers = M.fromList . map (f . g)
    where
        g = BSU.fromString *** map BSU.fromString
        f (name, values) = (name, HeaderPair {
              hName = name
            , hValue = values
        })

-- | Constructs cookies from list of string pairs
cookies :: [(String, String)] -> [(String, Cookie)]
cookies = map f
    where
        f (name, value) = (name, Cookie {
              cookieVersion = ""
            , cookiePath = ""
            , cookieDomain = ""
            , cookieName = name
            , cookieValue = value
            , secure = False
            , httpOnly = False
        })

-- | Constructs initial request with given data (POST or GET)
request :: Method -> [(String, Input)] -> IO Request
request method vars = do
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
context :: KontrakcjaTemplates -> IO Context
context templates = do
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
testAPI :: (Kontrakcja m, APIContext m c) => APIFunction m c APIResponse -> m APIResponse
testAPI f = do
    methodM POST
    mcontext <- apiContext
    case mcontext of
         Right apictx -> either (uncurry apiError) id <$> runApiFunction f apictx
         Left emsg -> return $ uncurry apiError emsg

-- Two examples (will be moved into separate files later)

loginTest :: IO ()
loginTest = withTestState $ do
    req <- request POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "admin")]
    ctx <- context =<< readTemplates LANG_EN
    pwd <- createPassword $ BS.pack "admin"
    _ <- update $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") pwd Nothing Nothing Nothing
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    print res
    print $ unEmail . useremail . userinfo <$> ctxmaybeuser ctx'
    print $ ctxflashmessages ctx'

mailapiTest :: IO ()
mailapiTest = withTestState $ withSystemTempDirectory "mailapi-test" $ \tmpdir -> do
    req <- request POST [("mail", inFile "test/mailapitest.eml")]
    ctx <- (\c -> c { ctxdocstore = tmpdir }) <$> (context =<< readTemplates LANG_EN)
    Just User{userid} <- update $ AddUser (BS.empty, BS.empty) (BS.pack "andrzej@skrivapa.se") NoPassword Nothing Nothing Nothing
    _ <- update $ SetUserMailAPI userid $ Just UserMailAPI {
          umapiKey = read "ef545848bcd3f7d8"
        , umapiDailyLimit = 1
        , umapiSentToday = 0
        , umapiLastSentDate = asInt $ ctxtime ctx
    }
    (res, _) <- runTestKontra req ctx $ testAPI handleMailCommand
    print res
