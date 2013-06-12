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
    , mkRequestWithHeaders
    , mkContext
    ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Maybe
import Happstack.Server hiding (mkHeaders, dir, getHeader, method, path)
import Happstack.Server.Internal.Monads
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Map as M

import qualified Amazon as AWS
import Control.Monad.Trans.Control.Util
import Configuration
import Crypto.RNG
import DB
import GuardTime (GuardTimeConf(..))
import Kontra
import Mails.MailsConfig
import Utils.Default
import Payments.Config (RecurlyConfig(..))
import IPAddress
import OurServerPart
import Session.SessionID
import qualified Text.StringTemplates.TemplatesLoader as TL
import Text.StringTemplates.Templates
import Templates
import qualified MemCache
import User.Lang
import Util.FinishWith
import ELegitimation.Config (LogicaConfig(..))
import qualified Control.Exception.Lifted as E
import qualified Static.Resources as SR
import qualified Doc.JpegPages as JpegPages
import System.Time
import qualified Log

data TestEnvSt = TestEnvSt {
    teNexus           :: Nexus
  , teRNGState        :: CryptoRNGState
  , teActiveTests     :: TVar (Bool, Int)
  , teGlobalTemplates :: KontrakcjaGlobalTemplates
  , teRejectedDocuments :: TVar Int
  }

type InnerTestEnv = ReaderT TestEnvSt (DBT IO)

newtype TestEnv a = TestEnv { unTestEnv :: InnerTestEnv a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader TestEnvSt, Log.MonadLog, MonadBase IO)

runTestEnv :: TestEnvSt -> TestEnv () -> IO ()
runTestEnv st m = do
  can_be_run <- fst <$> atomically (readTVar $ teActiveTests st)
  when can_be_run $ do
    atomically . modifyTVar' (teActiveTests st) $ second (succ $!)
    E.finally (runDBT (teNexus st) (DBEnvSt Nothing [] Nothing) $ ununTestEnv st $ withTestDB m) $ do
      atomically . modifyTVar' (teActiveTests st) $ second (pred $!)

ununTestEnv :: TestEnvSt -> TestEnv a -> DBT IO a
ununTestEnv st m = runReaderT (unTestEnv m) st

instance CryptoRNG TestEnv where
  getCryptoRNGState = teRNGState <$> ask

instance MonadDB TestEnv where
  getNexus     = teNexus <$> ask
  localNexus f = local (\st -> st { teNexus = f (teNexus st) })
  kCommit      = TestEnv $ kCommit
  kRollback    = TestEnv $ kRollback
  kClone       = TestEnv $ kClone
  kRunSQL      = TestEnv . kRunSQL
  kFinish      = TestEnv $ kFinish
  kGetTables   = TestEnv $ kGetTables
  kDescribeTable   = TestEnv . kDescribeTable
  kFold2 decoder init_acc = TestEnv (kFold2 decoder init_acc)
  kThrow       = TestEnv . kThrow
  getMinutesTime = TestEnv $ getMinutesTime

instance TemplatesMonad TestEnv where
  getTemplates = getTextTemplatesByColumn $ show (defaultValue :: Lang)
  getTextTemplatesByColumn langStr = do
    globaltemplates <- teGlobalTemplates <$> ask
    return $ TL.localizedVersion langStr globaltemplates

instance MonadBaseControl IO TestEnv where
  newtype StM TestEnv a = StTestEnv { unStTestEnv :: StM InnerTestEnv a }
  liftBaseWith = newtypeLiftBaseWith TestEnv unTestEnv StTestEnv
  restoreM     = newtypeRestoreM TestEnv unStTestEnv
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runTestKontraHelper :: Request -> Context -> Kontra a -> TestEnv (a, Context, FilterFun Response)
runTestKontraHelper rq ctx tk = do
  filecache <- MemCache.new BS.length 52428800
  let noflashctx = ctx { ctxflashmessages = [] }
      amazoncfg = AWS.AmazonConfig Nothing filecache
  nex <- getNexus
  rng <- getCryptoRNGState
  mres <- liftIO . ununWebT $ runServerPartT
    (runOurServerPartT . runDBT nex (DBEnvSt Nothing [] Nothing) . runCryptoRNGT rng $
      AWS.runAmazonMonadT amazoncfg $ runStateT (unKontraPlus $ unKontra tk) noflashctx) rq
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
mkRequest method vars = mkRequestWithHeaders method vars []

mkRequestWithHeaders :: Method -> [(String, Input)] -> [(String, [String])]-> TestEnv Request
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
        , rqUri = ""
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
  time <- getMinutesTime
  liftIO $ do
    docs <- MemCache.new JpegPages.pagesCount 500
    memcache <- MemCache.new BS.length 52428800
    return Context {
          ctxmaybeuser = Nothing
        , ctxhostpart = "http://testkontra.fake"
        , ctxresourcehostpart = "http://testkontra.fake"
        , ctxflashmessages = []
        , ctxtime = time
        , ctxnormalizeddocuments = docs
        , ctxipnumber = noIP
        , ctxproduction = False
        , ctxtemplates = localizedVersion lang globaltemplates
        , ctxglobaltemplates = globaltemplates
        , ctxlang = lang
        , ctxmailsconfig = defaultMailsConfig
        , ctxlivedocxconf = confDefault
        , ctxlogicaconf = LogicaConfig { logicaEndpoint = "https://eidt.funktionstjanster.se:18898/osif"
                                       , logicaCertFile = "certs/steria3.pem"
                                       , logicaServiceID = "logtest004"
                                       , logicaMBIEndpoint = "https://eidt.funktionstjanster.se:18898/mbi/service"
                                       , logicaMBIDisplayName = "Test av Mobilt BankID"
                                       }
        , ctxfilecache = memcache
        , ctxxtoken = error "xtoken is not defined"
        , ctxadminaccounts = []
        , ctxsalesaccounts = []
        , ctxmaybepaduser = Nothing
        , ctxstaticresources = SR.ResourceSetsForImport [] (TOD 0 0)
        , ctxusehttps = False
        , ctxgtconf = GuardTimeConf { guardTimeURL = "http://stamper.guardtime.net/gt-signingservice"
                                    , guardTimeExtendingServiceURL = "http://verifier.guardtime.net/gt-extendingservice"
                                    , guardTimeControlPublicationsURL = "http://verify.guardtime.com/gt-controlpublications.bin"
                                    }
        , ctxrecurlyconfig = RecurlyConfig { recurlySubdomain  = "scrive-test"
                                           , recurlyAPIKey     = "c31afaf14af3457895ee93e7e08e4451"
                                           , recurlyPrivateKey = "49c1b30592fa475b8535a0ca04f88e65"
                                           }
        , ctxsessionid = tempSessionID
        , ctxmixpaneltoken = "5b04329b972851feac0e9b853738e742"
        , ctxhomebase = "https://staging.scrive.com"
        , ctxbrandeddomains = []
    }

-- pgsql database --

-- | Runs set of sql queries within one transaction and clears all tables in the end
withTestDB :: TestEnv () -> TestEnv ()
withTestDB m = E.finally m $ do
  clearTables
  kCommit

clearTables :: TestEnv ()
clearTables = do
  kRunRaw "UPDATE users SET company_id = NULL"
  kRunRaw "DELETE FROM evidence_log"
  kRunRaw "DELETE FROM doc_stat_events"
  kRunRaw "DELETE FROM user_stat_events"
  kRunRaw "DELETE FROM sign_stat_events"
  kRunRaw "DELETE FROM companyinvites"
  kRunRaw "DELETE FROM payment_plans"
  kRunRaw "DELETE FROM payment_stats"

  kRunRaw "DELETE FROM email_change_requests"
  kRunRaw "DELETE FROM password_reminders"
  kRunRaw "DELETE FROM user_account_requests"

  kRunRaw "DELETE FROM mail_attachments"
  kRunRaw "DELETE FROM author_attachments"
  kRunRaw "DELETE FROM signatory_attachments"
  kRunRaw "DELETE FROM signatory_links"
  kRunRaw "DELETE FROM documents"

  kRunRaw "DELETE FROM companies"
  kRunRaw "DELETE FROM users"
  kRunRaw "DELETE FROM files"

  kRunRaw "DELETE FROM sessions"

  kRunRaw "DELETE FROM mails"
