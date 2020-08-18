{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestKontra (
      inTestDir
    , readTestFile
    , readTestFileAsBS
    , readTestFileAsStr
    , TestEnv(..)
    , module TestEnvSt
    , runTestEnv
    , ununTestEnv
    , TestKontra
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
    , mkContext'
    , mkContextWithUser
    , modifyTestTime
    , setTestTime
    , setRequestURI
    , testGtConf
    , dummyGtConf
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
import Optics (assign, gview, to, use)
import System.FilePath
import Text.StringTemplates.Templates
import qualified Control.Exception.Lifted as E
import qualified Control.Monad.State.Strict as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.StringTemplates.TemplatesLoader as TL

import BrandedDomain.Model
import DB
import DB.PostgreSQL
import EventStream.Class
import FileStorage
import GuardTime.Class
import Happstack.Server.ReqHandler
import IPAddress
import Kontra
import Log.Configuration
import MailContext (MailContextMonad(..))
import MinutesTime
import PasswordService.Conf
import PdfToolsLambda.Class
import PdfToolsLambda.Conf.Internal
import PdfToolsLambda.Control
import Session.SessionID as SessionID
import Templates
import TestEnvSt
import TestFileStorage
import User.Lang
import User.Types.User
import qualified Context.Internal
import qualified TestEnvSt.Internal

inTestDir :: FilePath -> FilePath
inTestDir = ("backend/test" </>)

readTestFile :: MonadIO m => FilePath -> m BSL.ByteString
readTestFile = liftIO . BSL.readFile . inTestDir

readTestFileAsBS :: MonadIO m => FilePath -> m BS.ByteString
readTestFileAsBS = liftIO . BS.readFile . inTestDir

readTestFileAsStr :: MonadIO m => FilePath -> m Text
readTestFileAsStr = fmap T.pack . liftIO . readFile . inTestDir

----------------------------------------

type InnerTestEnv
  = TestFileStorageT (StateT TestEnvStRW (ReaderT TestEnvSt (LogT (DBT IO))))

newtype TestEnv a = TestEnv { unTestEnv :: InnerTestEnv a }
  deriving
    ( Applicative, Functor, Monad, MonadBase IO
    , MonadCatch, MonadIO, MonadFail, MonadLog, MonadMask
    , MonadReader TestEnvSt, MonadState TestEnvStRW
    , MonadThrow )

runTestEnv :: TestEnvSt -> TestEnv () -> IO ()
runTestEnv st m = do
  can_be_run <- fst <$> readTVarIO (st ^. #activeTests)
  when can_be_run $ do
    atomically . modifyTVar' (st ^. #activeTests) $ second (succ $!)
    E.finally
      (runDBT (unConnectionSource $ st ^. #staticConnSource)
              (st ^. #transSettings)
              (ununTestEnv st $ withTestDB m)
      )
      (atomically . modifyTVar' (st ^. #activeTests) $ second (pred $!))

ununTestEnv :: TestEnvSt -> TestEnv a -> DBT IO a
ununTestEnv st =
  unRunLogger (st ^. #runLogger)
    . flip runReaderT st
  -- for each test start with no time delay
    . flip
        evalStateT
        TestEnvStRW { timeDelay   = 0
                    , currentTime = Nothing
                    , requestUri  = "http://testkontra.fake"
                    }
    . runFileStorage
    . unTestEnv
  where
    var = st ^. #memoryStorage
    runFileStorage m = do
      liftIO . atomically $ writeTVar var HM.empty
      evalTestFileStorageT
        var
        ((, st ^. #redisConn, st ^. #fileMemCache) <$> st ^. #amazonS3Env)
        m


instance CryptoRNG TestEnv where
  randomBytes n = gview #rngState >>= liftIO . randomBytesIO n

instance MonadDB TestEnv where
  runQuery               = TestEnv . runQuery
  getLastQuery           = TestEnv getLastQuery
  getConnectionStats     = TestEnv getConnectionStats
  getQueryResult         = TestEnv getQueryResult
  clearQueryResult       = TestEnv clearQueryResult
  getTransactionSettings = TestEnv getTransactionSettings
  setTransactionSettings = TestEnv . setTransactionSettings

  -- We run 'TestEnv' with a static connection source that uses the
  -- same connection over and over again. However, when
  -- 'withNewConnection' is called, we actually want to spawn a
  -- different one, thus we can't use current (static) connection
  -- source, so we need one that actually creates new connections.
  withNewConnection (TestEnv m) = do
    ConnectionSource pool <- gview #connSource
    runLogger             <- gview (#runLogger % to unRunLogger)
    TestEnv . TestFileStorageT . ReaderT $ \fsVar -> do
      StateT $ \terw -> ReaderT $ \te -> do
        LogT . ReaderT $ \_ -> DBT . StateT $ \st -> do
          res <- runDBT pool (dbTransactionSettings st) . runLogger $ runReaderT
            (runStateT (runTestFileStorageT m fsVar) terw)
            te
          return (res, st)
  getNotification = TestEnv . getNotification

instance MonadTime TestEnv where
  currentTime = do
    mtesttime <- use #currentTime
    case mtesttime of
      -- we use static time
      Just testtime -> return testtime
      -- we use IO time, but with a configurable delay
      Nothing       -> do
        delay <- use #timeDelay
        now   <- liftIO getCurrentTime
        return $ addUTCTime delay now

instance TemplatesMonad TestEnv where
  getTemplates = getTextTemplatesByLanguage . T.unpack $ codeFromLang defaultLang
  getTextTemplatesByLanguage langStr = do
    globaltemplates <- gview #globalTemplates
    return $ TL.localizedVersion langStr globaltemplates

instance MonadBaseControl IO TestEnv where
  type StM TestEnv a = StM InnerTestEnv a
  liftBaseWith f = TestEnv $ liftBaseWith (\run -> f $ run . unTestEnv)
  restoreM = TestEnv . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadFileStorage TestEnv where
  saveNewContents url contents = TestEnv $ saveNewContents url contents
  getSavedContents    = TestEnv . getSavedContents
  deleteSavedContents = TestEnv . deleteSavedContents

instance MonadEventStream TestEnv where
  pushEvent _ _ _ = pure ()

----------------------------------------
type InnerTestKontra
  = S.StateT Context (DBT (TestFileStorageT (CryptoRNGT (LogT (ReqHandlerT IO)))))

-- | TestKontra is the equivalent of 'Kontra' but with file storage and stream
-- event transformers suitable for running request handlers during testing.
newtype TestKontra a = TestKontra { unTestKontra :: InnerTestKontra a }
  deriving ( Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad
           , MonadBase IO, MonadCatch, MonadDB, MonadIO, MonadMask, MonadThrow
           , ServerMonad, MonadFileStorage, MonadLog, MonadFail)

instance MonadBaseControl IO TestKontra where
  type StM TestKontra a = StM InnerTestKontra a
  liftBaseWith f = TestKontra $ liftBaseWith (\run -> f $ run . unTestKontra)
  restoreM = TestKontra . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTime TestKontra where
  currentTime = view #time <$> getContext

instance KontraMonad TestKontra where
  getContext    = TestKontra S.get
  modifyContext = TestKontra . S.modify

instance TemplatesMonad TestKontra where
  getTemplates = view #templates <$> getContext
  getTextTemplatesByLanguage langStr = do
    globaltemplates <- view #globalTemplates <$> getContext
    return $ TL.localizedVersion langStr globaltemplates

instance GuardTimeConfMonad TestKontra where
  getGuardTimeConf = view #gtConf <$> getContext

instance PdfToolsLambdaMonad TestKontra where
  callPdfToolsSealing spec =
    TestKontra $ callPdfToolsSealingPrim spec =<< use #pdfToolsLambdaEnv
  callPdfToolsPresealing spec =
    TestKontra $ callPdfToolsPresealingPrim spec =<< use #pdfToolsLambdaEnv
  callPdfToolsAddImage spec =
    TestKontra $ callPdfToolsAddImagePrim spec =<< use #pdfToolsLambdaEnv
  callPdfToolsPadesSign spec =
    TestKontra $ callPdfToolsPadesSignPrim spec =<< use #pdfToolsLambdaEnv
  -- Don't run pdf cleaning in tests
  callPdfToolsCleaning = pure . Just . BSL.toStrict
  lambdaEnv            = TestKontra $ use #pdfToolsLambdaEnv

instance MailContextMonad TestKontra where
  getMailContext = contextToMailContext <$> getContext

instance MonadEventStream TestKontra where
  pushEvent _ _ _ = pure ()

runTestKontraHelper
  :: BasicConnectionSource
  -> Request
  -> Context
  -> TestKontra a
  -> TestEnv (a, Context, Response -> Response)
runTestKontraHelper (ConnectionSource pool) rq ctx tk = do
  now       <- currentTime
  rng       <- gview #rngState
  runLogger <- gview (#runLogger % to unRunLogger)
  ts        <- getTransactionSettings
  -- commit previous changes and do not begin new transaction as runDBT
  -- does it and we don't want these pesky warnings about transaction
  -- being already in progress
  fsEnv     <- TestEnv getTestFSEnv
  commit' ts { tsAutoTransaction = False }
  ((res, ctx'), st) <- E.finally
    (liftBase $ runStateT
      ( unReqHandlerT
      . runLogger
      . runCryptoRNGT rng
      . flip runTestFileStorageT fsEnv
      . runDBT pool ts
      $ runStateT (unTestKontra tk) ctx
      )
      (ReqHandlerSt rq identity now)
    )
    -- runDBT commits and doesn't run another transaction, so begin new one
    begin
  return (res, ctx', hsFilter st)

-- | Typeclass for running handlers within TestKontra monad
class RunnableTestKontra a where
  runTestKontra :: Request -> Context -> TestKontra a
                -> TestEnv (a, Context)

instance RunnableTestKontra a where
  runTestKontra rq ctx tk = do
    cs             <- gview #connSource
    (res, ctx', _) <- runTestKontraHelper cs rq ctx tk
    return (res, ctx')

instance {-# OVERLAPPING #-} RunnableTestKontra Response where
  runTestKontra rq ctx tk = do
    cs             <- gview #connSource
    (res, ctx', f) <- runTestKontraHelper cs rq ctx tk
    return (f res, ctx')

----------------------------------------

-- | Modifies time, but does not change, whether the time is static or from IO.
modifyTestTime :: (MonadState TestEnvStRW m) => (UTCTime -> UTCTime) -> m ()
modifyTestTime modtime = do
  mtesttime <- use #currentTime
  case mtesttime of
    Just testtime -> assign #currentTime (Just $ modtime testtime)
    Nothing       -> assign #timeDelay (diffUTCTime (modtime unixEpoch) unixEpoch)

-- | Sets time and also stops time flow
setTestTime :: (MonadState TestEnvStRW m) => UTCTime -> m ()
setTestTime currtime = assign #currentTime (Just currtime)

-- | Sets current uri of all test requests
setRequestURI :: (MonadState TestEnvStRW m) => Text -> m ()
setRequestURI uri = assign #requestUri $ T.unpack uri

-- Various helpers for constructing appropriate Context/Request.

-- | Creates GET/POST input text variable
inText :: Text -> Input
inText val = Input
  { inputValue       = Right . BSL.fromStrict $ TE.encodeUtf8 val
  , inputFilename    = Nothing
  , inputContentType = ContentType { ctType       = "text"
                                   , ctSubtype    = "plain"
                                   , ctParameters = []
                                   }
  }

-- | Creates GET/POST input text variable
inTextBS :: BSLU.ByteString -> Input
inTextBS val = Input
  { inputValue       = Right val
  , inputFilename    = Nothing
  , inputContentType = ContentType { ctType       = "text"
                                   , ctSubtype    = "plain"
                                   , ctParameters = []
                                   }
  }

-- | Creates GET/POST input file variable
inFile :: FilePath -> Input
inFile path = Input
  { inputValue       = Left path
  , inputFilename    = Just $ takeFileName path
  , inputContentType = ContentType { ctType       = "application"
                                   , ctSubtype    = "octet-stream"
                                   , ctParameters = []
                                   }
  }

-- | Constructs headers from list of string pairs
mkHeaders :: [(Text, [Text])] -> Headers
mkHeaders = M.fromList . map (f . g)
  where
    g :: (Text, [Text]) -> (BSU.ByteString, [BSU.ByteString])
    g = TE.encodeUtf8 *** map TE.encodeUtf8

    f :: (BSU.ByteString, [BSU.ByteString]) -> (BSU.ByteString, HeaderPair)
    f (name, values) = (name, HeaderPair { hName = name, hValue = values })

-- | Constructs cookies from list of string pairs
mkCookies :: [(Text, Text)] -> [(Text, Cookie)]
mkCookies = map $ \(n, v) -> (n, mkCookie (T.unpack n) (T.unpack v))

-- | Retrieves specific header value
getHeader :: Text -> Headers -> Maybe Text
getHeader name hdrs =
  TE.decodeUtf8 <$> (listToMaybe . hValue =<< M.lookup (TE.encodeUtf8 name) hdrs)

-- | Retrieves specific cookie value
getCookie :: Text -> [(Text, Cookie)] -> Maybe Text
getCookie name cookies = T.pack . cookieValue <$> lookup name cookies

-- | Constructs initial request with given data (POST or GET)
mkRequest
  :: (MonadState TestEnvStRW m, MonadIO m) => Method -> [(Text, Input)] -> m Request
mkRequest method vars = mkRequestWithHeaders method vars [("host", ["testkontra.fake"])]

mkRequestWithHeaders
  :: (MonadState TestEnvStRW m, MonadIO m)
  => Method
  -> [(Text, Input)]
  -> [(Text, [Text])]
  -> m Request
mkRequestWithHeaders method vars headers = do
  let vars' :: [(String, Input)] = fmap (first T.unpack) vars
  uri <- use #requestUri
  liftIO $ do
    rqbody <- newEmptyMVar
    ib :: MVar [(String, Input)] <- newMVar vars'
    let iq = if isReqPost then [] else vars'
    return Request { rqSecure      = False
                   , rqMethod      = POST
                   , rqPaths       = []
                   , rqUri         = uri
                   , rqQuery       = ""
                   , rqInputsQuery = iq
                   , rqInputsBody  = ib
                   , rqCookies     = []
                   , rqVersion     = HttpVersion 1 1
                   , rqHeaders     = mkHeaders headers
                   , rqBody        = rqbody
                   , rqPeer        = ("", 0)
                   }
  where isReqPost = method == POST || method == PUT

-- | Constructs initial context with given templates
mkContext :: Lang -> TestEnv Context
mkContext lang = do
  pdfToolsLambdaEnv <- gview #pdfToolsLambdaEnv
  globalTemplates   <- gview #globalTemplates
  time              <- currentTime
  bd                <- dbQuery GetMainBrandedDomain
  fileCache         <- newFileMemCache 52428800
  pure $ mkContext' pdfToolsLambdaEnv globalTemplates time bd fileCache lang

mkContext'
  :: PdfToolsLambdaEnv
  -> KontrakcjaGlobalTemplates
  -> UTCTime
  -> BrandedDomain
  -> FileMemCache
  -> Lang
  -> Context
mkContext' pdfToolsLambdaEnv globalTemplates time bd fileCache lang = Context
  { maybeUser               = Nothing
  , time                    = time
  , clientName              = Nothing
  , clientTime              = Nothing
  , ipAddr                  = noIP
  , production              = False
  , cdnBaseUrl              = Nothing
  , templates               = localizedVersion lang globalTemplates
  , globalTemplates         = globalTemplates
  , lang                    = lang
  , isMailBackdoorOpen      = False
  , mailNoreplyAddress      = "noreply@scrive.com"
  , cgiGrpConfig            = Nothing
  , redisCache              = Nothing
  , fileCache               = fileCache
  , xToken                  = unexpectedError "xtoken is not defined"
  , adminAccounts           = []
  , salesAccounts           = []
  , maybePadUser            = Nothing
  , useHttps                = False
  -- We're using a dummy GT conf because the proper one is not needed
  -- and causes timeouts when used outside of VPN.
  , gtConf                  = dummyGtConf
  , sessionID               = SessionID.tempSessionID
  , trackJsToken            = Nothing
  , zendeskKey              = Nothing
  , mixpanelToken           = Nothing
  , gaToken                 = Nothing
  , hubspotConf             = Nothing
  , brandedDomain           = bd
  , salesforceConf          = Nothing
  , netsConfig              = Nothing
  , isApiLogEnabled         = True
  , netsSignConfig          = Nothing
  -- We use real Lambda config here because we want our tests
  -- to check it.  This Lambda and S3 bucket are dedicated
  -- for tests and development.
  , pdfToolsLambdaEnv       = pdfToolsLambdaEnv
  , passwordServiceConf     = defaultPasswordService
  , maybeApiUser            = Nothing
  , eidServiceConf          = Nothing
  , ssoConf                 = Nothing
  , postSignViewRedirectURL = ""
  }

mkContextWithUser :: Lang -> User -> TestEnv Context
mkContextWithUser lang user = set #maybeUser (Just user) <$> mkContext lang

testGtConf :: GuardTimeConf
testGtConf = GuardTimeConf
  { guardTimeSigningServiceURL      =
    "http://internal-gt-signer-848430379.eu-west-1.elb.amazonaws.com:8080"
      <> "/gt-signingservice"
  , guardTimeExtendingServiceURL    =
    "http://internal-gt-extender-2081608339.eu-west-1.elb.amazonaws.com:8081"
      <> "/gt-extendingservice"
  , guardTimeControlPublicationsURL = "http://verify.guardtime.com/ksi-publications.bin"
  , guardTimeSigningLoginUser       = "anon"
  , guardTimeSigningLoginKey        = "anon"
  , guardTimeExtendingLoginUser     = "anon"
  , guardTimeExtendingLoginKey      = "1234"
  , guardTimeGatewayIdentity        = "GT :: GT :: Scrive :: public"
  }

dummyGtConf :: GuardTimeConf
dummyGtConf =
  testGtConf { guardTimeSigningServiceURL = "", guardTimeExtendingServiceURL = "" }

testLogConfig :: LogConfig
testLogConfig = LogConfig { lcSuffix = "dev", lcLoggers = [StandardOutput] }

-- pgsql database --

-- | Runs set of SQL queries within one transaction and clears all
-- tables in the end.
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

  runSQL_ "DELETE FROM flow_instances"
  runSQL_ "DELETE FROM flow_templates"

  runSQL_ "DELETE FROM mail_attachments"
  runSQL_ "DELETE FROM author_attachments"
  runSQL_ "DELETE FROM signatory_attachments"
  runSQL_ "DELETE FROM signatory_links"

  runSQL_ "DELETE FROM documents"
  runSQL_ "DELETE FROM users"
  runSQL_ "DELETE FROM files"

  runSQL_ "DELETE FROM sessions"

  runSQL_ "DELETE FROM mails"

  runSQL_ "DELETE FROM async_event_queue"
  runSQL_ "DELETE FROM signatory_link_fields"
  runSQL_ "DELETE FROM kontra_info_for_mails"
  runSQL_ "DELETE FROM main_files"
  runSQL_ "DELETE FROM document_sealing_jobs"
  runSQL_ "DELETE FROM user_groups"
  runSQL_ "DELETE FROM user_group_invoicings"
  runSQL_ "DELETE FROM user_group_uis"
  runSQL_ "DELETE FROM user_group_addresses"
  runSQL_ "DELETE FROM user_group_settings"
  runSQL_ "DELETE FROM access_control"
  runSQL_ "DELETE FROM folders"
