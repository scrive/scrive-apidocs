module TestKontra (
      inTestDir
    , TestEnv
    , TestEnvSt(..)
    , runTestEnv
    , ununTestEnv
    , runTestKontra
    , inText
    , inTextBS
    , inFile
    , mkHeaders
    , mkCookies
    , getHeader
    , getCookie
    , mkRequest
    , mkRequestWithHeaders
    , mkContext
    , modifyTestTime
    , setTestTime
    , setRequestURI
    , testGTConf
    , testLogConfig
    ) where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Time
import Database.PostgreSQL.PQTypes.Internal.Monad
import Database.PostgreSQL.PQTypes.Internal.State
import Happstack.Server hiding (dir, getHeader, method, mkHeaders, path)
import Log
import System.FilePath
import Text.StringTemplates.Templates
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map as M
import qualified Text.StringTemplates.TemplatesLoader as TL

import BrandedDomain.Model
import Context.Internal
import DB
import DB.PostgreSQL
import GuardTime.Class
import Happstack.Server.ReqHandler
import IPAddress
import Kontra
import Log.Configuration
import MinutesTime
import PdfToolsLambda.Conf
import Session.SessionID
import Templates
import User.Lang
import qualified Amazon as AWS
import qualified MemCache

inTestDir :: FilePath -> FilePath
inTestDir = ("backend/test" </>)

data TestEnvSt = TestEnvSt {
    teConnSource        :: !BasicConnectionSource
  , teStaticConnSource  :: !BasicConnectionSource
  , teTransSettings     :: !TransactionSettings
  , teRNGState          :: !CryptoRNGState
  , teRunLogger         :: !(forall m r . LogT m r -> m r)
  , teActiveTests       :: !(TVar (Bool, Int))
  , teGlobalTemplates   :: !KontrakcjaGlobalTemplates
  , teRejectedDocuments :: !(TVar Int)
  , teOutputDirectory   :: !(Maybe String) -- ^ Put test artefact output in this directory if given
  , teStagingTests      :: !Bool
  , tePdfToolsLambdaConf :: PdfToolsLambdaConf
  }

data TestEnvStRW = TestEnvStRW {
    terwTimeDelay   :: !NominalDiffTime -- Modifies currentTime, when taken from IO
  , terwCurrentTime :: !(Maybe UTCTime) -- When 'Nothing', currentTime is taken from IO
  , terwRequestURI  :: !String
  }

type InnerTestEnv = StateT TestEnvStRW (ReaderT TestEnvSt (LogT (DBT IO)))

newtype TestEnv a = TestEnv { unTestEnv :: InnerTestEnv a }
  deriving (Applicative, Functor, Monad, MonadLog, MonadCatch, MonadThrow, MonadMask, MonadIO, MonadReader TestEnvSt, MonadBase IO, MonadState TestEnvStRW)

runTestEnv :: TestEnvSt -> TestEnv () -> IO ()
runTestEnv st m = do
  can_be_run <- fst <$> atomically (readTVar $ teActiveTests st)
  when can_be_run $ do
    atomically . modifyTVar' (teActiveTests st) $ second (succ $!)
    E.finally (runDBT (unConnectionSource $ teStaticConnSource st) (teTransSettings st) $ ununTestEnv st $ withTestDB m) $ do
      atomically . modifyTVar' (teActiveTests st) $ second (pred $!)

ununTestEnv :: TestEnvSt -> TestEnv a -> DBT IO a
ununTestEnv st m = teRunLogger st
  . (\m' -> runReaderT m' st)
  -- for each test start with no time delay
  . (\m' -> fst <$> (runStateT m' $ TestEnvStRW
      { terwTimeDelay = 0
      , terwCurrentTime = Nothing
      , terwRequestURI = "http://testkontra.fake"
      }
    ))
  . unTestEnv $ m

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
  withNewConnection (TestEnv m) = do
    -- we run TestEnv with static connection source that uses
    -- the same connection over and over again. however, when
    -- withNewConnection is called, we actually want to spawn
    -- a different one, thus we can't use current (static)
    -- connection source, but the one that actually creates
    -- new connection.
    ConnectionSource pool <- asks teConnSource
    runLogger <- asks teRunLogger
    TestEnv . StateT $ \terw -> ReaderT $ \te -> LogT . ReaderT $ \_ -> DBT . StateT $ \st -> do
      res <- runDBT pool (dbTransactionSettings st) . runLogger $ runReaderT (runStateT m terw) te
      return (res, st)
  getNotification = TestEnv . getNotification

instance MonadTime TestEnv where
  currentTime = do
    mtesttime <- gets terwCurrentTime
    case mtesttime of
      -- we use static time
      Just testtime -> return testtime
      -- we use IO time, but with a configurable delay
      Nothing -> do
        delay <- gets terwTimeDelay
        now   <- liftIO getCurrentTime
        return $ addUTCTime delay now

instance TemplatesMonad TestEnv where
  getTemplates = getTextTemplatesByLanguage $ codeFromLang def
  getTextTemplatesByLanguage langStr = do
    globaltemplates <- teGlobalTemplates <$> ask
    return $ TL.localizedVersion langStr globaltemplates

instance MonadBaseControl IO TestEnv where
  type StM TestEnv a = StM InnerTestEnv a
  liftBaseWith f = TestEnv $ liftBaseWith $ \run -> f $ run . unTestEnv
  restoreM       = TestEnv . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runTestKontraHelper :: BasicConnectionSource -> Request -> Context -> Kontra a -> TestEnv (a, Context, Response -> Response)
runTestKontraHelper (ConnectionSource pool) rq ctx tk = do
  filecache <- MemCache.new BS.length 52428800
  let amazoncfg = AWS.AmazonConfig Nothing filecache Nothing
  now <- currentTime
  rng <- asks teRNGState
  runLogger <- asks teRunLogger
  ts <- getTransactionSettings
  -- commit previous changes and do not begin new transaction as runDBT
  -- does it and we don't want these pesky warnings about transaction
  -- being already in progress
  commit' ts { tsAutoTransaction = False }
  ((res, ctx'), st) <- E.finally
    (liftBase $ runStateT (unReqHandlerT . runLogger . runCryptoRNGT rng . AWS.runAmazonMonadT amazoncfg . runDBT pool ts $ runStateT (unKontra tk) ctx) $ ReqHandlerSt rq id now)
    -- runDBT commits and doesn't run another transaction, so begin new one
    begin
  return (res, ctx', hsFilter st)

-- | Typeclass for running handlers within TestKontra monad
class RunnableTestKontra a where
  runTestKontra :: Request -> Context -> Kontra a -> TestEnv (a, Context)

instance RunnableTestKontra a where
  runTestKontra rq ctx tk = do
    cs <- asks teConnSource
    (res, ctx', _) <- runTestKontraHelper cs rq ctx tk
    return (res, ctx')

instance {-# OVERLAPPING #-} RunnableTestKontra Response where
  runTestKontra rq ctx tk = do
    cs <- asks teConnSource
    (res, ctx', f) <- runTestKontraHelper cs rq ctx tk
    return (f res, ctx')

-- | Modifies time, but does not change, whether the time is static or from IO.
modifyTestTime :: (MonadState TestEnvStRW m) => (UTCTime -> UTCTime) -> m ()
modifyTestTime modtime = do
  mtesttime <- gets terwCurrentTime
  case mtesttime of
    Just testtime -> modify (\terw -> terw { terwCurrentTime = Just $ modtime testtime })
    Nothing       -> modify (\terw -> terw { terwTimeDelay = diffUTCTime (modtime unixEpoch) unixEpoch })

-- | Sets time and also stops time flow
setTestTime :: (MonadState TestEnvStRW m) => UTCTime -> m ()
setTestTime currtime = modify (\terw -> terw { terwCurrentTime = Just currtime })

-- | Sets current uri of all test requests
setRequestURI :: (MonadState TestEnvStRW m) => String -> m ()
setRequestURI uri = modify (\terw -> terw { terwRequestURI = uri })

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

-- | Creates GET/POST input text variable
inTextBS :: BSLU.ByteString -> Input
inTextBS val = Input {
      inputValue = Right $ val
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
mkRequest :: (MonadState TestEnvStRW m, MonadIO m) => Method -> [(String, Input)] -> m Request
mkRequest method vars = mkRequestWithHeaders method vars [("host",["testkontra.fake"])]

mkRequestWithHeaders ::(MonadState TestEnvStRW m, MonadIO m) =>  Method -> [(String, Input)] -> [(String, [String])]-> m Request
mkRequestWithHeaders method vars headers = do
    uri <- gets terwRequestURI
    liftIO $ do
      rqbody <- newEmptyMVar
      ib <- newMVar vars
      let iq = if isReqPost
                  then []
                  else vars
      return Request {
            rqSecure = False
          , rqMethod = POST
          , rqPaths = []
          , rqUri = uri
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
  pdfSealLambdaConf <- tePdfToolsLambdaConf <$> ask
  globaltemplates <- teGlobalTemplates <$> ask
  time <- currentTime
  bd <- dbQuery $ GetMainBrandedDomain
  liftIO $ do
    filecache <- MemCache.new BS.length 52428800
    return Context {
          _ctxmaybeuser = Nothing
        , _ctxtime = time
        , _ctxclientname = Nothing
        , _ctxclienttime = Nothing
        , _ctxipnumber = noIP
        , _ctxproduction = False
        , _ctxcdnbaseurl = Nothing
        , _ctxtemplates = localizedVersion lang globaltemplates
        , _ctxglobaltemplates = globaltemplates
        , _ctxlang = lang
        , _ctxismailbackdooropen = False
        , _ctxmailnoreplyaddress = "noreply@scrive.com"
        , _ctxcgigrpconfig = Nothing
        , _ctxmrediscache = Nothing
        , _ctxfilecache = filecache
        , _ctxxtoken = unexpectedError "xtoken is not defined"
        , _ctxadminaccounts = []
        , _ctxsalesaccounts = []
        , _ctxmaybepaduser = Nothing
        , _ctxusehttps = False
        , _ctxgtconf = testGTConf
        , _ctxsessionid = tempSessionID
        , _ctxtrackjstoken = Nothing
        , _ctxmixpaneltoken = Nothing
        , _ctxgatoken = Nothing
        , _ctxhubspotconf = Nothing
        , _ctxbrandeddomain = bd
        , _ctxsalesforceconf = Nothing
        , _ctxnetsconfig = Nothing
        , _ctxisapilogenabled = True
        , _ctxnetssignconfig = Nothing
        -- We use real lambda config here because we want our tests to check it
        -- This lambda and s3 bucket are dedicated for tests and development
        , _ctxpdftoolslambdaconf = pdfSealLambdaConf
    }

testGTConf :: GuardTimeConf
testGTConf = GuardTimeConf {
    guardTimeSigningServiceURL = "http://internal-gt-signer-848430379.eu-west-1.elb.amazonaws.com:8080/gt-signingservice"
  , guardTimeExtendingServiceURL ="http://internal-gt-extender-2081608339.eu-west-1.elb.amazonaws.com:8081/gt-extendingservice"
  , guardTimeControlPublicationsURL = "http://verify.guardtime.com/ksi-publications.bin"
  , guardTimeSigningLoginUser ="anon"
  , guardTimeSigningLoginKey = "anon"
  , guardTimeExtendingLoginUser ="anon"
  , guardTimeExtendingLoginKey = "1234"
  }

testLogConfig :: LogConfig
testLogConfig = LogConfig {
    lcSuffix  = "dev"
  , lcLoggers = [StandardOutput]
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
  runSQL_ "DELETE FROM chargeable_items"

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

  runSQL_ "DELETE FROM async_event_queue"
  runSQL_ "DELETE FROM signatory_link_fields"
  runSQL_ "DELETE FROM kontra_info_for_mails"
  runSQL_ "DELETE FROM main_files"
  runSQL_ "DELETE FROM document_sealing_jobs"
  runSQL_ "DELETE FROM amazon_upload_jobs"
