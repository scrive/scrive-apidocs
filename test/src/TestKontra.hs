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
    , defaultUri
    , mkRequest
    , mkRequestWithHeaders
    , mkContext
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Maybe
import Database.PostgreSQL.PQTypes.Internal.Monad
import Database.PostgreSQL.PQTypes.Internal.State
import Happstack.Server hiding (mkHeaders, dir, getHeader, method, path)
import Happstack.Server.Internal.Monads
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Map as M

import qualified Amazon as AWS
import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB
import GuardTime (GuardTimeConf(..))
import Kontra
import MinutesTime
import Mails.MailsConfig
import Utils.Default
import Payments.Config (RecurlyConfig(..))
import IPAddress
import Session.SessionID
import qualified Text.StringTemplates.TemplatesLoader as TL
import Text.StringTemplates.Templates
import Templates
import qualified MemCache
import User.Lang
import Util.FinishWith
import ELegitimation.Config (LogicaConfig(..))
import qualified Control.Exception.Lifted as E
import qualified Doc.RenderedPages as RenderedPages
import qualified Log
import Salesforce.Conf

data TestEnvSt = TestEnvSt {
    teConnSource        :: ConnectionSource
  , teStaticConnSource  :: ConnectionSource
  , teTransSettings     :: TransactionSettings
  , teRNGState          :: CryptoRNGState
  , teActiveTests       :: TVar (Bool, Int)
  , teGlobalTemplates   :: KontrakcjaGlobalTemplates
  , teRejectedDocuments :: TVar Int
  , teOutputDirectory   :: Maybe String -- ^ Put test artefact output in this directory if given
  }

type InnerTestEnv = ReaderT TestEnvSt (DBT IO)

newtype TestEnv a = TestEnv { unTestEnv :: InnerTestEnv a }
  deriving (Applicative, Functor, Monad, MonadCatch, MonadThrow, MonadMask, MonadIO, MonadReader TestEnvSt, MonadBase IO)

instance Log.MonadLog TestEnv where
  mixlogjs title js = liftBase (Log.mixlogjsIO title js)

runTestEnv :: TestEnvSt -> TestEnv () -> IO ()
runTestEnv st m = do
  can_be_run <- fst <$> atomically (readTVar $ teActiveTests st)
  when can_be_run $ do
    atomically . modifyTVar' (teActiveTests st) $ second (succ $!)
    E.finally (runDBT (teStaticConnSource st) (teTransSettings st) $ ununTestEnv st $ withTestDB m) $ do
      atomically . modifyTVar' (teActiveTests st) $ second (pred $!)

ununTestEnv :: TestEnvSt -> TestEnv a -> DBT IO a
ununTestEnv st m = runReaderT (unTestEnv m) st

instance CryptoRNG TestEnv where
  randomBytes n = asks teRNGState >>= liftIO . randomBytesIO n

instance MonadDB TestEnv where
  runQuery = TestEnv . runQuery
  getLastQuery = TestEnv getLastQuery
  getConnectionStats = TestEnv getConnectionStats
  getQueryResult = TestEnv getQueryResult
  clearQueryResult = TestEnv clearQueryResult
  getTransactionSettings = TestEnv getTransactionSettings
  setTransactionSettings = TestEnv . setTransactionSettings
  foldlM = foldLeftM
  foldrM = foldRightM
  withNewConnection (TestEnv m) = do
    -- we run TestEnv with static connection source that uses
    -- the same connection over and over again. however, when
    -- withNewConnection is called, we actually want to spawn
    -- a different one, thus we can't use current (static)
    -- connection source, but the one that actually creates
    -- new connection.
    cs <- asks teConnSource
    TestEnv . ReaderT $ \r -> DBT . StateT $ \st -> do
      res <- runDBT cs (dbTransactionSettings st) (runReaderT m r)
      return (res, st)

instance TemplatesMonad TestEnv where
  getTemplates = getTextTemplatesByLanguage $ codeFromLang defaultValue
  getTextTemplatesByLanguage langStr = do
    globaltemplates <- teGlobalTemplates <$> ask
    return $ TL.localizedVersion langStr globaltemplates

instance MonadBaseControl IO TestEnv where
  newtype StM TestEnv a = StTestEnv { unStTestEnv :: StM InnerTestEnv a }
  liftBaseWith = newtypeLiftBaseWith TestEnv unTestEnv StTestEnv
  restoreM     = newtypeRestoreM TestEnv unStTestEnv
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runTestKontraHelper :: ConnectionSource -> Request -> Context -> Kontra a -> TestEnv (a, Context, FilterFun Response)
runTestKontraHelper cs rq ctx tk = do
  filecache <- MemCache.new BS.length 52428800
  let noflashctx = ctx { ctxflashmessages = [] }
      amazoncfg = AWS.AmazonConfig Nothing filecache
  rng <- asks teRNGState
  ts <- getTransactionSettings
  -- commit previous changes and do not begin new transaction as runDBT
  -- does it and we don't want these pesky warnings about transaction
  -- being already in progress
  commit' ts { tsAutoTransaction = False }
  mres <- E.finally (liftIO . ununWebT $ runServerPartT
    (runDBT cs ts . runCryptoRNGT rng $
      AWS.runAmazonMonadT amazoncfg $ runStateT (unKontraPlus $ unKontra tk) noflashctx) rq)
      -- runDBT commits and doesn't run another transaction, so begin new one
      begin
  case mres of
    Nothing -> fail "runTestKontraHelper mzero"
    Just (Left _, _) -> fail "This should never happen since we don't use Happstack's finishWith"
    Just (Right (res, ctx'), fs) -> return (res, ctx', fs)

-- | Typeclass for running handlers within TestKontra monad
class RunnableTestKontra a where
  runTestKontra :: Request -> Context -> Kontra a -> TestEnv (a, Context)

instance RunnableTestKontra a where
  runTestKontra rq ctx tk = do
    cs <- asks teConnSource
    (res, ctx', _) <- runTestKontraHelper cs rq ctx tk
      `E.catch` (\(FinishWith _ _) -> error "FinishWith thrown in function that doesn't return Response")
    return (res, ctx')

instance RunnableTestKontra Response where
  runTestKontra rq ctx tk = do
    cs <- asks teConnSource
    (res, ctx', f) <- runTestKontraHelper cs rq ctx tk
      `E.catch` (\(FinishWith res ctx') -> return (res, ctx', filterFun id))
    return (unFilterFun f res, ctx')

-- Various helpers for constructing appropriate Context/Request

-- | Creates GET/POST input text variable
inText :: String -> Input
inText val = Input {
      inputValue = Right $ BSLU.fromString val
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

-- | Default uri used for tests
defaultUri :: String
defaultUri = "http://testkontra.fake"

-- | Constructs initial request with given data (POST or GET)
mkRequest :: MonadIO m => Method -> [(String, Input)] -> m Request
mkRequest method vars = mkRequestWithHeaders method vars [("host",["testkontra.fake"])]

mkRequestWithHeaders :: MonadIO m => Method -> [(String, Input)] -> [(String, [String])]-> m Request
mkRequestWithHeaders method vars headers = liftIO $ do
    rqbody <- newEmptyMVar
    ib <- newMVar vars
    let iq = if isReqPost
                then []
                else vars
    return Request {
          rqSecure = False
        , rqMethod = POST
        , rqPaths = []
        , rqUri = defaultUri
        , rqQuery = ""
        , rqInputsQuery = iq
        , rqInputsBody = ib
        , rqCookies = []
        , rqVersion = HttpVersion 1 1
        , rqHeaders = mkHeaders $ headers
        , rqBody = rqbody
        , rqPeer = ("", 0)
    }
    where
        isReqPost = method == POST || method == PUT

-- | Constructs initial context with given templates
mkContext :: Lang -> TestEnv Context
mkContext lang = do
  globaltemplates <- teGlobalTemplates <$> ask
  time <- currentTime
  liftIO $ do
    docs <- MemCache.new RenderedPages.pagesCount 500
    memcache <- MemCache.new BS.length 52428800
    return Context {
          ctxmaybeuser = Nothing
        , ctxhostpart = defaultUri
        , ctxresourcehostpart = defaultUri
        , ctxflashmessages = []
        , ctxtime = time
        , ctxclientname = Nothing
        , ctxclienttime = Nothing
        , ctxnormalizeddocuments = docs
        , ctxipnumber = noIP
        , ctxproduction = False
        , ctxtemplates = localizedVersion lang globaltemplates
        , ctxglobaltemplates = globaltemplates
        , ctxlang = lang
        , ctxmailsconfig = defaultMailsConfig
        , ctxlivedocxconf = defaultValue
        , ctxlogicaconf = LogicaConfig { logicaEndpoint = "https://grpt.funktionstjanster.se:18898/osif"
                                       , logicaCertFile = "certs/steria3.pem"
                                       , logicaServiceID = "logtest004"
                                       , logicaMBIEndpoint = "https://grpt.funktionstjanster.se:18898/mbi/service"
                                       , logicaMBIDisplayName = "Test av Mobilt BankID"
                                       }
        , ctxfilecache = memcache
        , ctxxtoken = error "xtoken is not defined"
        , ctxadminaccounts = []
        , ctxsalesaccounts = []
        , ctxmaybepaduser = Nothing
        , ctxusehttps = False
        , ctxgtconf = GuardTimeConf { guardTimeURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-signingservice"
                                    , guardTimeExtendingServiceURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-extendingservice"
                                    , guardTimeControlPublicationsURL = "http://internal-guardtime-load-balancer-256298782.eu-west-1.elb.amazonaws.com:8080/gt-controlpublications.bin"
                                    }
        , ctxrecurlyconfig = RecurlyConfig { recurlySubdomain  = "scrive-test"
                                           , recurlyAPIKey     = "c31afaf14af3457895ee93e7e08e4451"
                                           , recurlyPrivateKey = "49c1b30592fa475b8535a0ca04f88e65"
                                           }
        , ctxsessionid = tempSessionID
        , ctxmixpaneltoken = "5b04329b972851feac0e9b853738e742"
        , ctxgoogleanalyticstoken = "5b04329b972851feac0e9b853738e741"
        , ctxhomebase = "https://staging.scrive.com"
        , ctxbrandeddomain = Nothing
        , ctxsalesforceconf = SalesforceConf "" "" "" "" "" "" ""
    }

-- pgsql database --

-- | Runs set of sql queries within one transaction and clears all tables in the end
withTestDB :: TestEnv () -> TestEnv ()
withTestDB m = E.finally m $ do
  -- if there was db error, fix transaction state
  rollback
  clearTables
  commit

clearTables :: TestEnv ()
clearTables = do
  runSQL_ "DELETE FROM evidence_log"
  runSQL_ "DELETE FROM companyinvites"
  runSQL_ "DELETE FROM payment_plans"
  runSQL_ "DELETE FROM payment_stats"

  runSQL_ "DELETE FROM email_change_requests"
  runSQL_ "DELETE FROM password_reminders"
  runSQL_ "DELETE FROM user_account_requests"

  runSQL_ "DELETE FROM mail_attachments"
  runSQL_ "DELETE FROM author_attachments"
  runSQL_ "DELETE FROM signatory_attachments"
  runSQL_ "DELETE FROM signatory_links"
  runSQL_ "DELETE FROM documents"

  runSQL_ "DELETE FROM users"
  runSQL_ "DELETE FROM companies"
  runSQL_ "DELETE FROM files"

  runSQL_ "DELETE FROM sessions"

  runSQL_ "DELETE FROM mails"
