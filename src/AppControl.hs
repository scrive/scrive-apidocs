{-# LANGUAGE ExtendedDefaultRules #-}
{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module AppControl
    ( appHandler
    , AppGlobals(..)
    , maybeReadTemplates
    -- exported for the sake of unit tests
    , getStandardLang
    ) where

import Control.Concurrent.Lifted (MVar, modifyMVar, threadDelay, readMVar)
import Control.Monad.Base
import Control.Monad.Error
import Control.Monad.Trans.Control
import Data.Functor
import Data.Maybe
import Data.Time.Clock
import Data.Typeable
import GHC.Stack
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Happstack.Server.Internal.Cookie
import Network.Socket
import System.Directory
import Text.JSON.Gen
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map

import AppConf
import AppView as V
import BrandedDomain.Model
import Control.Concurrent.MVar.Util (tryReadMVar)
import Crypto.RNG
import DB hiding (ErrorCode(..))
import DB.PostgreSQL
import Doc.RenderedPages
import File.FileID
import Happstack.MonadPlus (runMPlusT)
import IPAddress
import Kontra
import MinutesTime
import Salesforce.Conf
import Session.Data hiding (session)
import Session.Model
import Templates
import User.Model
import Util.FinishWith
import Util.FlashUtil
import Utils.HTTP
import qualified Amazon as AWS
import qualified FlashMessage as F
import qualified Log
import qualified MemCache

{- |
  Global application data
-}
data AppGlobals
    = AppGlobals { templates       :: MVar (UTCTime, KontrakcjaGlobalTemplates)
                 , filecache       :: MemCache.MemCache FileID BS.ByteString
                 , docscache       :: RenderedPagesCache
                 , cryptorng       :: CryptoRNGState
                 , connsource      :: ConnectionSource
                 }


{- |
    Determines the lang of the current user (whether they are logged in or not), by checking
    their settings, the request, and cookies.
-}
getStandardLang :: (HasLang a, HasRqData m, ServerMonad m, FilterMonad Response m, MonadIO m, Functor m) => Maybe a -> m Lang

getStandardLang muser = do
  rq <- askRq
  langcookie <- runMPlusT $ lookCookie "lang" -- MPLusT captures evil use of rqDataError
  let mcookielang = join $ langFromCode <$> cookieValue <$> langcookie
  let browserlang = langFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
      newlang = fromMaybe browserlang $ msum [(getLang <$> muser), mcookielang]
      newlangcookie = mkCookie "lang" (codeFromLang newlang)
  addCookie (MaxAge (60*60*24*366)) newlangcookie
  return newlang

maybeReadTemplates :: (MonadBaseControl IO m, Log.MonadLog m)
                   => Bool
                   -> MVar (UTCTime, KontrakcjaGlobalTemplates) -> m KontrakcjaGlobalTemplates
maybeReadTemplates production mvar | production = snd <$> readMVar mvar
maybeReadTemplates _ mvar = modifyMVar mvar $ \(modtime, templates) -> do
  modtime' <- liftBase getTemplatesModTime
  if modtime /= modtime'
    then do
      Log.mixlog_ $ "Reloading templates"
      templates' <- liftBase readGlobalTemplates
      return ((modtime', templates'), templates')
    else return ((modtime, templates), templates)

-- | Show nicely formated headers. Same header lines can appear
-- multiple times in HTTP so we need to beautifully show them.  We
-- also skip 'cookies' header as we show it later in a nicer form.
showNamedHeader :: (a, HeaderPair) -> [String]
showNamedHeader (_nm,hd) | hName hd == BS.fromString "cookie" = []
showNamedHeader (_nm,hd) = map showHeaderLine (hValue hd)
  where
    showHeaderLine value' = BS.toString (hName hd) ++ ": " ++ BS.toString value'

showNamedCookie :: (String, Cookie) -> String
showNamedCookie (_name,cookie) = mkCookieHeader Nothing cookie

showNamedInput :: (String, Input) -> String
showNamedInput (name,input) = name ++ ": " ++
    case inputFilename input of
      Just filename -> filename
      _ -> case inputValue input of
             Left _tmpfilename -> "<<content in /tmp>>"
             Right value' -> show (BSL.toString value')

logRequest :: (Monad m) => Request -> Maybe [(String, Input)] -> JSONGenT m ()
logRequest rq maybeInputsBody = do
    value "request" (show (rqMethod rq) ++ " " ++ rqUri rq ++ rqQuery rq)
    value "post variables" $ map showNamedInput (fromMaybe [] maybeInputsBody)
    value "http headers" $ concatMap showNamedHeader (Map.toList $ rqHeaders rq)
    value "http cookies" $ map showNamedCookie (rqCookies rq)

-- | Long polling implementation.
--
-- The 'enhanceYourCalm' function checks for 420 Enhance Your Calm
-- status code and if detected it retries to invoke a handler. This is
-- done for at most 10s, then gives up and returns result as given.
--
-- It has to be done outside of database connection, because database
-- connection needs to be dropped between retries to allow for commits
-- to take place.
enhanceYourCalm :: (MonadIO m) => m Response -> m Response
enhanceYourCalm action = enhanceYourCalmWorker 100
  where
    enhanceYourCalmWorker 0 = action
    enhanceYourCalmWorker n = do
      result' <- action
      case rsCode result' of
        420 -> do
          liftIO $ threadDelay 100000
          enhanceYourCalmWorker (n-1)
        _ -> return result'

{- |
   Creates a context, routes the request, and handles the session.
-}
appHandler :: KontraPlus (Maybe Response) -> AppConf -> AppGlobals -> ServerPartT IO Response
appHandler handleRoutes appConf appGlobals = catchEverything . enhanceYourCalm $
  withPostgreSQL (connsource appGlobals) . runCryptoRNGT (cryptorng appGlobals) $
    AWS.runAmazonMonadT amazoncfg $ do
    startTime <- liftIO getCurrentTime
    let quota = 10000000
    temp <- liftIO getTemporaryDirectory
    decodeBody (defaultBodyPolicy temp quota quota quota)
    session <- getCurrentSession
    ctx <- createContext session
    -- commit is needed after getting session from the database
    -- since session expiration date is updated while getting it,
    -- which results in pgsql locking the row. then, if request
    -- handler somehow gets stuck, transaction is left open for
    -- some time, row remains locked and subsequent attempts to
    -- refresh the page will fail, because they will try to
    -- access/update session from a row that was previously locked.
    commit
    rq <- askRq
    Log.mixlog_ $ "Handling routes for : " ++ rqUri rq ++ rqQuery rq

    (res, ctx') <- routeHandlers ctx

    let newsession = session {
          sesID        = ctxsessionid ctx'
        , sesUserID    = userid <$> ctxmaybeuser ctx'
        , sesPadUserID = userid <$> ctxmaybepaduser ctx'
        }
        newflashmessages = ctxflashmessages ctx'
    F.updateFlashCookie (ctxflashmessages ctx) newflashmessages
    issecure <- isSecure
    let usehttps = useHttps appConf
    when (issecure || not usehttps) $
      updateSession session newsession

    -- Here we show in debug log some statistics that should help
    -- optimize code and instantly see if there is something
    -- wrong. Measurements are not perfect, for example time is not
    -- full response time, it is just the part that is under
    -- application control. That is good because we want to stress
    -- places that can be fixed.

    stats <- getConnectionStats
    finishTime <- liftIO getCurrentTime
    Log.mixlog_ $ concat [
        "Statistics for " ++ rqUri rq ++ rqQuery rq ++ ":\n"
      , "* " ++ show stats ++ "\n"
      , "* Time: " ++ show (diffUTCTime finishTime startTime)
      ]

    case res of
      Right response -> return response
      Left response -> do
        rollback -- if exception was thrown, rollback everything
        return response
  where
    amazoncfg = AWS.AmazonConfig (amazonConfig appConf) (filecache appGlobals)
    catchEverything m = m `E.catch` \(e::E.SomeException) -> do
      uri <- rqUri <$> askRq
      Log.attention "appHandler: exception caught at top level" $ do
         value "exception" (show e)
         value "url" uri
      internalServerError $ toResponse ""

    routeHandlers ctx = runKontraPlus ctx $ do
      res <- (Right <$> (handleRoutes >>= maybe (E.throwIO Respond404) return)) `E.catches` [
          E.Handler $ \e -> Left <$> case e of
            InternalError stack -> do
              rq <- askRq
              mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
              Log.attention "InternalError" $ do
                value "stacktrace" (reverse stack)
                logRequest rq mbody
              internalServerErrorPage >>= internalServerError
            Respond404 -> do
              -- there is no way to get stacktrace here as Respond404 is a CAF, fix this later
              rq <- askRq
              mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
              Log.attention "Respond404" $
                logRequest rq mbody
              notFoundPage >>= notFound
        , E.Handler $ \(FinishWith res ctx') -> do
            modifyContext $ const ctx'
            return $ Right res
        , E.Handler $ \e@DBException{..} -> Left <$> do
            rq <- askRq
            mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
            stack <- liftIO $ whoCreated e
            Log.attention "DBException" $ do
              value "dbeQueryContext" $ show dbeQueryContext
              case cast dbeError of
                Nothing -> do
                  value "dbeError" $ show dbeError
                Just (SomeKontraException ee) -> do
                  value "exception" $ toJSValue ee
              value "stacktrace" (reverse stack)
              logRequest rq mbody
            internalServerErrorPage >>= internalServerError
        , E.Handler $ \e@(SomeKontraException ee) -> Left <$> do
            rq <- askRq
            mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
            stack <- liftIO $ whoCreated e
            Log.attention "SomeKontraException" $ do
              value "exception" $ toJSValue ee
              value "stacktrace" (reverse stack)
              logRequest rq mbody
            internalServerErrorPage >>= internalServerError
        , E.Handler $ \(e :: E.SomeException) -> Left <$> do
            rq <- askRq
            mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
            stack <- liftIO $ whoCreated e
            Log.attention "Exception caught in routeHandlers" $ do
              value "exception" (show e)
              value "stacktrace" (reverse stack)
              logRequest rq mbody
            internalServerErrorPage >>= internalServerError
        ]
      ctx' <- getContext
      return (res, ctx')

    createContext session = do
      -- rqPeer hostname comes always from showHostAddress
      -- so it is a bunch of numbers, just read them out
      rq <- askRq
      peerip <- do
        -- First, we look for x-forwarded-for, which a proxy might insert
        -- Then, we look for x-real-ip, which nginx might insert
        let peerhost :: HostName
            peerhost = head $ catMaybes $
                         [ BS.toString <$> getHeader h rq
                         |  h <- ["x-forwarded-for", "x-real-ip"]
                         ] ++ [Just (fst (rqPeer rq))]
            hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST] }
        (do addrs <- liftIO $ getAddrInfo (Just hints) (Just peerhost) Nothing
            return $ case addrAddress $ head addrs of
              SockAddrInet _ hostip -> unsafeIPAddress hostip
              _                     -> noIP)
           `E.catch` \ (_ :: E.SomeException) -> return noIP

      currhostpart <- getHostpart
      reshostpart <- getResourceHostpart
      minutestime <- currentTime
      let clientName = BS.toString <$> getHeader "client-name" rq
          clientTime = parseTimeISO =<< (BS.toString <$> getHeader "client-time" rq)
          userAgent  = BS.toString <$> getHeader "user-agent" rq
      muser <- getUserFromSession session
      mpaduser <- getPadUserFromSession session
      mbrandeddomain <- dbQuery $ GetBrandedDomainByURL currhostpart

      flashmessages <- withDataFn F.flashDataFromCookie $ maybe (return []) $ \fval -> do
        flashes <- liftIO $ (E.try (E.evaluate $ F.fromCookieValue fval) :: IO (Either  E.SomeException (Maybe [FlashMessage])))
        case flashes of
          Right (Just fs) -> return fs
          _ -> do
            Log.mixlog_ $ "Couldn't read flash messages from value: " ++ fval
            F.removeFlashCookie
            return []

      -- do reload templates in non-production code
      templates2 <- maybeReadTemplates (production appConf) (templates appGlobals)

      -- work out the language
      userlang <- getStandardLang muser

      return Context {
          ctxmaybeuser = muser
        , ctxhostpart = currhostpart
        , ctxresourcehostpart = reshostpart
        , ctxflashmessages = flashmessages
        , ctxtime = minutestime
        , ctxclientname = clientName `mplus` userAgent
        , ctxclienttime = clientTime
        , ctxnormalizeddocuments = docscache appGlobals
        , ctxipnumber = peerip
        , ctxproduction = production appConf
        , ctxtemplates = localizedVersion userlang templates2
        , ctxglobaltemplates = templates2
        , ctxlang = userlang
        , ctxmailsconfig = mailsConfig appConf
        , ctxgtconf = guardTimeConf appConf
        , ctxlivedocxconf = liveDocxConfig appConf
        , ctxlogicaconf   = logicaConfig appConf
        , ctxfilecache = filecache appGlobals
        , ctxxtoken = sesCSRFToken session
        , ctxadminaccounts = admins appConf
        , ctxsalesaccounts = sales appConf
        , ctxmaybepaduser = mpaduser
        , ctxusehttps = useHttps appConf
        , ctxrecurlyconfig = recurlyConfig appConf
        , ctxsessionid = sesID session
        , ctxmixpaneltoken = mixpanelToken appConf
        , ctxgoogleanalyticstoken = googleanalyticsToken appConf
        , ctxhomebase = homebase appConf
        , ctxbrandeddomain = mbrandeddomain
        , ctxsalesforceconf = getSalesforceConf appConf
        }
