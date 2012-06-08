{-# LANGUAGE OverlappingInstances #-}
module TestKontra (
      TestEnv
    , TestEnvSt(..)
    , runTestEnv
    , ununTestEnv
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
import Control.Monad.Base
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Maybe
import Happstack.Data (Proxy(..))
import Happstack.Server hiding (mkHeaders, dir, getHeader, method, path)
import Happstack.Server.Internal.Monads
import Happstack.State (runTxSystem, TxControl, shutdownSystem, Saver(..))
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Map as M
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP

import GuardTime (GuardTimeConf(..))
import AppState
import Control.Monad.Trans.Control.Util
import Configuration
import Crypto.RNG
import DB
import Kontra
import Mails.MailsConfig
import MinutesTime
import Misc
import IPAddress
import Templates.Templates
import Templates.TemplatesLoader
import qualified MemCache
import User.Locale
import Util.FinishWith
import ELegitimation.BankIDRequests (LogicaConfig(..))
import qualified Data.Map as Map
import qualified Control.Exception.Lifted as E
import qualified Static.Resources as SR
import qualified Doc.JpegPages as JpegPages

data TestEnvSt = TestEnvSt {
    teNexus           :: Nexus
  , teRNGState        :: CryptoRNGState
  , teGlobalTemplates :: KontrakcjaGlobalTemplates
  }

type InnerTestEnv = ReaderT TestEnvSt IO

newtype TestEnv a = TestEnv { unTestEnv :: InnerTestEnv a }
  deriving (Applicative, Functor, Monad, MonadBase IO, MonadIO, MonadReader TestEnvSt)

runTestEnv :: TestEnvSt -> TestEnv () -> IO ()
runTestEnv st = ununTestEnv st . withTestState . withTestDB

ununTestEnv :: TestEnvSt -> TestEnv a -> IO a
ununTestEnv st m = runReaderT (unTestEnv m) st

instance CryptoRNG TestEnv where
  getCryptoRNGState = teRNGState <$> ask

instance MonadDB TestEnv where
  getNexus     = teNexus <$> ask
  localNexus f = local (\st -> st { teNexus = f (teNexus st) })


instance TemplatesMonad TestEnv where
  getTemplates = getLocalTemplates defaultValue
  getLocalTemplates locale = do
    globaltemplates <- teGlobalTemplates <$> ask
    return $ localizedVersion locale globaltemplates

  
instance MonadBaseControl IO TestEnv where
  newtype StM TestEnv a = StTestEnv { unStTestEnv :: StM InnerTestEnv a }
  liftBaseWith = newtypeLiftBaseWith TestEnv unTestEnv StTestEnv
  restoreM     = newtypeRestoreM TestEnv unStTestEnv
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runTestKontraHelper :: Request -> Context -> Kontra a -> TestEnv (a, Context, FilterFun Response)
runTestKontraHelper rq ctx tk = do
  let noflashctx = ctx { ctxflashmessages = [] }
  nex <- getNexus
  rng <- getCryptoRNGState
  mres <- liftIO $ ununWebT $ runServerPartT (runDBT nex $ runCryptoRNGT rng $
    runStateT (unKontraPlus $ unKontra tk) noflashctx) rq
  case mres of
    Nothing -> fail "runTestKontraHelper mzero"
    Just (Left _, _) -> fail "This should never happen since we don't use Happstack's finishWith"
    Just (Right (res, ctx'), fs) -> return (res, ctx', fs)

-- | Typeclass for running handlers within TestKontra monad
class RunnableTestKontra a where
  runTestKontra :: Request -> Context -> Kontra a -> TestEnv (a, Context)

instance RunnableTestKontra a where
  runTestKontra rq ctx tk = do
    (res, ctx', _) <- runTestKontraHelper rq ctx tk
      `E.catch` (\(FinishWith _ _) -> error "FinishWith thrown in function that doesn't return Response")
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
mkRequest :: Method -> [(String, Input)] -> TestEnv Request
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
mkContext :: Locale -> TestEnv Context
mkContext locale = do
  globaltemplates <- teGlobalTemplates <$> ask
  liftIO $ do
    docs <- MemCache.new JpegPages.pagesCount 500
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
        , ctxmailsconfig = defaultMailsConfig
        , ctxlivedocxconf = confDefault
        , ctxlogicaconf = LogicaConfig { logicaEndpoint = "https://eidt.funktionstjanster.se:18898/osif"
                                       , logicaCertFile = "certs/steria3.pem"
                                       , logicaServiceID = "logtest004" 
                                       , logicaMBIEndpoint = "https://eidt.funktionstjanster.se:18898/mbi/service"
                                       , logicaMBIDisplayName = "Test av Mobilt BankID"
                                       }
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
        , ctxstaticresources = SR.ResourceSetsForImport []
        , ctxusehttps = False
        , ctxgtconf = GuardTimeConf { guardTimeURL = "http://stamper.guardtime.net/gt-signingservice" }
    }

-- pgsql database --

-- | Runs set of sql queries within one transaction and clears all tables in the end
withTestDB :: TestEnv () -> TestEnv ()
withTestDB m = E.finally m $ do
  clearTables
  dbCommit

clearTables :: TestEnv ()
clearTables = runDBEnv $ do
  kRunRaw "UPDATE users SET service_id = NULL, company_id = NULL"
  kRunRaw "DELETE FROM evidence_log"
  kRunRaw "DELETE FROM doc_stat_events"
  kRunRaw "DELETE FROM user_stat_events"
  kRunRaw "DELETE FROM sign_stat_events"
  kRunRaw "DELETE FROM companyinvites"

  kRunRaw "DELETE FROM author_attachments"
  kRunRaw "DELETE FROM signatory_attachments"
  kRunRaw "DELETE FROM signatory_links"
  kRunRaw "DELETE FROM documents"

  kRunRaw "DELETE FROM companies"
  kRunRaw "DELETE FROM services"
  kRunRaw "DELETE FROM users"
  kRunRaw "DELETE FROM files"

  kRunRaw "DELETE FROM mails"

-- happstack-state --

startUp :: Saver -> TestEnv (MVar TxControl)
startUp saver = liftIO $ runTxSystem saver (Proxy :: Proxy AppState)

withSaver :: Saver -> TestEnv () -> TestEnv ()
withSaver saver m = E.bracket (startUp saver) (liftIO . shutdownSystem) (const m)

withFileSaver :: FilePath -> TestEnv () ->TestEnv ()
withFileSaver dir m = withSaver (Queue (FileSaver dir)) m

withTemporaryDirectory :: (FilePath -> TestEnv a) -> TestEnv a
withTemporaryDirectory = withSystemTempDirectory' "kontrakcja-test-"

withTestState :: TestEnv () -> TestEnv ()
withTestState m = withTemporaryDirectory (\tmpDir -> withFileSaver tmpDir m)
