module Main (main) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Catch
import Crypto.RNG
import Database.PostgreSQL.PQTypes.Checks
import Database.PostgreSQL.PQTypes.Internal.Connection
import Happstack.Server hiding (waitForTermination)
import Happstack.StaticRouting
import Log
import Network.Curl
import Network.HostName
import System.Console.CmdArgs hiding (def)
import System.Environment
import System.FilePath ((</>))
import qualified Control.Exception.Lifted as E
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as F

import AppConf
import AppControl
import AppDBTables
import AppDir (AppPaths(..), setupAppPaths)
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
import UserGroup.FolderListCallsTransition
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import Utils.IO
import Utils.Network
import qualified HostClock.Model as HC
import Flow.Server
import qualified VersionTH

{-# ANN type CmdConf ("HLint: ignore Use newtype instead of data" :: String) #-}
-- newtype doesn't work, see https://github.com/ndmitchell/cmdargs/issues/44
data CmdConf = CmdConf
  { config :: String
  } deriving Data

cmdConf :: FilePath -> String -> CmdConf
cmdConf workspaceRoot progName =
  CmdConf
      { config =
        configFile &= help ("Configuration file (default: " ++ configFile ++ ")") &= typ
          "FILE"
      }
    &= program progName
  where configFile = workspaceRoot </> "kontrakcja.conf"

----------------------------------------

type MainM = LogT IO

main :: IO ()
main = withCurlDo $ do
  (AppPaths _ workspaceRoot) <- setupAppPaths

  CmdConf {..}               <- cmdArgs . cmdConf workspaceRoot =<< getProgName
  appConf                    <- readConfig putStrLn config

  -- Transition to folder list calls
  when (useFolderListCallsByDefault appConf) $ do
    enableFolderListCallsForDefaultUserGroupSettings

  case monitoringConfig appConf of
    Just conf -> void $ startMonitoringServer conf
    Nothing   -> return ()
  let connSettings  = pgConnSettings $ dbConfig appConf
      extrasOptions = defaultExtrasOptions { eoEnforcePKs = True }
  pool <- liftBase
    $ createPoolSource (connSettings kontraComposites) (maxDBConnections appConf)
  rng        <- newCryptoRNGState
  (errs, lr) <- mkLogRunner "kontrakcja" (logConfig appConf) rng
  mapM_ T.putStrLn errs

  hostname <- getHostName
  let globalLogContext = ["server_hostname" .= hostname]

  runWithLogRunner lr . localData globalLogContext $ do
    logInfo "Starting kontrakcja-server" $ object ["version" .= VersionTH.versionID]
    checkExecutables

    withPostgreSQL (unConnectionSource . simpleSource $ connSettings []) $ do
      checkDatabase extrasOptions kontraComposites kontraDomains kontraTables
      unless (readOnlyDatabase appConf) $ do
        dbUpdate . SetMainDomainURL $ T.unpack (mainDomainUrl appConf)

    appGlobals <- do
      templates <-
        liftBase $ newMVar =<< (,) <$> getTemplatesModTime <*> readGlobalTemplates
      mrediscache <- F.forM (redisCacheConfig appConf) mkRedisConnection
      filecache   <- newFileMemCache $ localFileCacheSize appConf
      amazonEnv   <- s3envFromConfig $ amazonConfig appConf
      lambdaEnv   <- pdfToolsLambdaEnvFromConf $ pdfToolsLambdaConf appConf

      return AppGlobals { templates         = templates
                        , mrediscache       = mrediscache
                        , filecache         = filecache
                        , cryptorng         = rng
                        , connsource        = pool
                        , hostname          = T.pack hostname
                        , amazons3env       = amazonEnv
                        , pdftoolslambdaenv = lambdaEnv
                        }
    liftIO . void . fork . runFlow $ FlowConfiguration (unConnectionSource . simpleSource $ connSettings [])
    startSystem appGlobals appConf

startSystem :: AppGlobals -> AppConf -> MainM ()
startSystem appGlobals appConf = E.bracket startServer stopServer waitForTerm
  where
    startServer :: MainM ThreadId
    startServer = do
      let (iface, port) = httpBindAddress appConf
      listensocket <- liftBase $ listenOn (htonl iface) (fromIntegral port)
      routes       <- case compile $ staticRoutes (production appConf) of
        Left e -> do
          logInfo "Error while compiling routes" $ object ["error" .= e]
          unexpectedError "static routing"
        Right r -> return r
      let conf =
            nullConf { port = fromIntegral port, timeout = 120, logAccess = Nothing }

      fork . mapLogT (runReqHandlerT listensocket conf) $ appHandler routes
                                                                     appConf
                                                                     appGlobals
    stopServer = killThread
    waitForTerm _ = do
      let trackedConnSource = unConnectionSource
            $ connsource appGlobals (maxConnectionTracker $ maxDBConnections appConf)
      withPostgreSQL trackedConnSource
        . runCryptoRNGT (cryptorng appGlobals)
        $ initDatabaseEntries appConf
      liftBase waitForTermination
      logInfo_ "Termination request received"

initDatabaseEntries
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m) => AppConf -> m ()
initDatabaseEntries appConf = do
  unless (production appConf) $ do
    -- Add some host_clock entries in "dev" mode if there are no valid samples
    clockErrors <- dbQuery $ HC.GetNClockErrorEstimates 10
    unless (HC.enoughClockErrorOffsetSamples clockErrors) $ do
      void . dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.001) 0.5
      void . dbUpdate $ HC.InsertClockOffsetFrequency (Just 0.0015) 0.5
  forM_ (initialUsers appConf) $ \(email, passwordstring) -> do
    -- create initial database entries
    passwd    <- createPassword passwordstring
    maybeuser <- dbQuery $ GetUserByEmail email
    case maybeuser of
      Nothing -> do
        bd       <- dbQuery GetMainBrandedDomain
        ugFolder <- dbUpdate . FolderCreate $ defaultFolder
        ug       <-
          dbUpdate
          . UserGroupCreate
          . set #homeFolderID (Just $ ugFolder ^. #id)
          $ defaultUserGroup
        userFolder <-
          dbUpdate . FolderCreate . set #parentID (Just $ ugFolder ^. #id) $ defaultFolder
        void . dbUpdate $ AddUser ("", "")
                                  (unEmail email)
                                  (Just passwd)
                                  (ug ^. #id, Just $ userFolder ^. #id, True)
                                  LANG_EN
                                  (bd ^. #id)
                                  ByAdmin
                                  S.empty
                                  S.empty
        let features = fromJust $ ug ^. #features
        -- enable everything for initial admins
        let adminFeatures = (fromJust $ ug ^. #features)
              { fAdminUsers = (fAdminUsers features) { ffCanUseDKAuthenticationToView = True
                                                     , ffCanUseDKAuthenticationToSign = True
                                                     , ffCanUseFIAuthenticationToView = True
                                                     , ffCanUseNOAuthenticationToView = True
                                                     , ffCanUseNOAuthenticationToSign = True
                                                     , ffCanUseSEAuthenticationToView = True
                                                     , ffCanUseSEAuthenticationToSign = True
                                                     }
              }
        dbUpdate
          . UserGroupUpdate
          . set #invoicing (Invoice EnterprisePlan)
          . set #features  (Just adminFeatures)
          $ ug
      Just _ -> return () -- user exist, do not add it
