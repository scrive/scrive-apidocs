module Main (main) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Crypto.RNG
import Database.PostgreSQL.PQTypes.Checks
import Happstack.Server hiding (waitForTermination)
import Happstack.StaticRouting
import Log
import Network.Curl
import Network.HostName
import System.Console.CmdArgs hiding (def)
import System.Environment
import qualified Control.Exception.Lifted as E
import qualified Data.Text.IO as T
import qualified Data.Traversable as F

import AppConf
import AppControl
import AppDBTables
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Configuration
import Database.Redis.Configuration
import DB
import DB.PostgreSQL
import FeatureFlags.Model
import FileStorage
import FileStorage.Amazon.S3Env
import Folder.Model
import Happstack.Server.ReqHandler
import Log.Configuration
import Monitoring
import PdfToolsLambda.Conf
import RoutingTable
import Templates
import User.Email
import User.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import Utils.IO
import Utils.Network
import qualified HostClock.Model as HC
import qualified VersionTH

data CmdConf = CmdConf {
  config :: String
} deriving (Data, Typeable)

cmdConf :: String -> CmdConf
cmdConf progName = CmdConf {
  config = configFile
        &= help ("Configuration file (default: " ++ configFile ++ ")")
        &= typ "FILE"
} &= program progName
  where
    configFile = "kontrakcja.conf"

----------------------------------------

type MainM = LogT IO

main :: IO ()
main = withCurlDo $ do
  CmdConf{..} <- cmdArgs . cmdConf =<< getProgName
  appConf <- readConfig putStrLn config
  case monitoringConfig appConf of
    Just conf -> void $ startMonitoringServer conf
    Nothing   -> return ()
  let connSettings  = pgConnSettings $ dbConfig appConf
      extrasOptions = defaultExtrasOptions { eoEnforcePKs = True }
  pool <- liftBase $ createPoolSource
          (connSettings kontraComposites) (maxDBConnections appConf)
  rng <- newCryptoRNGState
  (errs, lr) <- mkLogRunner "kontrakcja" (logConfig appConf) rng
  mapM_ T.putStrLn errs

  withLogger lr $ \runLogger -> runLogger $ do
    logInfo "Starting kontrakcja-server" $ object [
        "version" .= VersionTH.versionID
      ]
    checkExecutables

    withPostgreSQL (unConnectionSource . simpleSource $ connSettings []) $ do
      checkDatabase extrasOptions kontraComposites kontraDomains kontraTables
      unless (readOnlyDatabase appConf) $ do
        dbUpdate $ SetMainDomainURL $ mainDomainUrl appConf

    appGlobals <- do
      templates <- liftBase $
                   newMVar =<< (,) <$> getTemplatesModTime <*> readGlobalTemplates
      mrediscache <- F.forM (redisCacheConfig appConf) mkRedisConnection
      filecache   <- newFileMemCache $ localFileCacheSize appConf
      hostname    <- liftBase getHostName
      amazonEnv   <- s3envFromConfig $ amazonConfig appConf
      lambdaEnv   <- pdfToolsLambdaEnvFromConf $ pdfToolsLambdaConf appConf

      return AppGlobals {
          templates          = templates
        , mrediscache        = mrediscache
        , filecache          = filecache
        , cryptorng          = rng
        , connsource         = pool
        , runlogger          = runLogger
        , hostname           = hostname
        , amazons3env        = amazonEnv
        , pdftoolslambdaenv  = lambdaEnv
        }

    startSystem appGlobals appConf

startSystem :: AppGlobals -> AppConf -> MainM ()
startSystem appGlobals appConf = E.bracket startServer stopServer waitForTerm
  where
    startServer :: MainM ThreadId
    startServer = do
      let (iface,port) = httpBindAddress appConf
      listensocket <- liftBase $ listenOn (htonl iface) (fromIntegral port)
      routes <- case compile $ staticRoutes (production appConf) of
        Left e -> do
          logInfo "Error while compiling routes" $ object [
              "error" .= e
            ]
          unexpectedError "static routing"
        Right r -> return r
      let conf = nullConf {
            port = fromIntegral port
          , timeout = 120
          , logAccess = Nothing
          }

      fork . liftBase . runReqHandlerT listensocket conf $ do
        (runlogger appGlobals) $ appHandler routes appConf appGlobals
    stopServer = killThread
    waitForTerm _ = do
      let trackedConnSource =
            unConnectionSource $
            connsource appGlobals (maxConnectionTracker $ maxDBConnections appConf)
      withPostgreSQL trackedConnSource . runCryptoRNGT (cryptorng appGlobals) $
        initDatabaseEntries appConf
      liftBase $ waitForTermination
      logInfo_ "Termination request received"

initDatabaseEntries
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => AppConf -> m ()
initDatabaseEntries appConf = do
  unless (production appConf) $ do
    -- Add some host_clock entries in "dev" mode if there are no valid samples
    clockErrors <- dbQuery $ HC.GetNClockErrorEstimates 10
    unless (HC.enoughClockErrorOffsetSamples clockErrors) $ do
      void $ dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.001) 0.5
      void $ dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.0015) 0.5
      return ()
  flip mapM_ (initialUsers appConf) $ \(email, passwordstring) -> do
    -- create initial database entries
    passwd    <- createPassword passwordstring
    maybeuser <- dbQuery $ GetUserByEmail email
    case maybeuser of
      Nothing -> do
        bd <- dbQuery $ GetMainBrandedDomain
        ugFolder <- dbUpdate . FolderCreate $ defaultFolder
        ug <- dbUpdate . UserGroupCreate
          . set ugHomeFolderID (Just $ get folderID ugFolder) $ defaultUserGroup
        userFolder <- dbUpdate . FolderCreate
          . set folderParentID (Just $ get folderID ugFolder) $ defaultFolder
        void $ dbUpdate $ AddUser ("", "")
          (unEmail email) (Just passwd) (get ugID ug, Just $ get folderID userFolder, True)
          LANG_EN (get bdid bd) ByAdmin
        let features = fromJust $ get ugFeatures ug
        -- enable everything for initial admins
        let adminFeatures = (fromJust $ get ugFeatures ug)
              { fAdminUsers = (fAdminUsers features)
                  { ffCanUseDKAuthenticationToView = True
                  , ffCanUseDKAuthenticationToSign = True
                  , ffCanUseFIAuthenticationToView = True
                  , ffCanUseNOAuthenticationToView = True
                  , ffCanUseNOAuthenticationToSign = True
                  , ffCanUseSEAuthenticationToView = True
                  , ffCanUseSEAuthenticationToSign = True
                  }
              }
        dbUpdate . UserGroupUpdate
          . set ugInvoicing (Invoice EnterprisePlan)
          . set ugFeatures (Just adminFeatures)
          $ ug
      Just _ -> return () -- user exist, do not add it
