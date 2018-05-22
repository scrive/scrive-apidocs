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

import Control.Concurrent.Lifted
import Control.DeepSeq
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Aeson.Types
import Data.Time.Clock
import Data.Typeable
import GHC.Stack
import Happstack.Server hiding (dir, host, lookCookieValue, path, simpleHTTP)
import Happstack.Server.Internal.Cookie
import Happstack.Server.Internal.Multipart (defaultFileSaver, defaultInputIter)
import Log
import Network.Socket
import System.Directory
import Text.JSON.ToJSValue
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Database.Redis as R

import AppConf
import AppView as V
import BrandedDomain.Model
import Context.Internal
import Cookies (lookCookieValue)
import DB hiding (ErrorCode(..))
import DB.PostgreSQL
import FileStorage
import FileStorage.MemCache
import Happstack.Server.ReqHandler
import IPAddress
import Kontra
import Log.Identifier
import Log.Utils
import MinutesTime
import Session.Data
import Session.Model
import Templates
import Text.JSON.Convert
import User.Model
import Utils.HTTP

-- | Global application data
data AppGlobals = AppGlobals {
    templates          :: !(MVar (UTCTime, KontrakcjaGlobalTemplates))
  , mrediscache        :: !(Maybe R.Connection)
  , filecache          :: !FileMemCache
  , cryptorng          :: !CryptoRNGState
  , connsource         :: !(ConnectionTracker -> TrackedConnectionSource)
  , runlogger          :: !(forall m r . LogT m r -> m r)
  }

{- |
    Determines the lang of the current user (whether they are logged in or not), by checking
    their settings, the request, and cookies.
-}
getStandardLang :: (HasLang a, ServerMonad m, FilterMonad Response m, MonadIO m, Functor m, MonadLog m, MonadCatch m) => Maybe a -> m Lang
getStandardLang muser = do
  rq <- askRq
  let mlangcookie = lookCookieValue "lang" $ rqHeaders rq
      mcookielang = join $ langFromCode <$> mlangcookie
      browserlang = langFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
      newlang = fromMaybe browserlang $ msum [(getLang <$> muser), mcookielang]
      newlangcookie = mkCookie "lang" (codeFromLang newlang)
  addCookie (MaxAge (60*60*24*366)) newlangcookie
  return newlang

maybeReadTemplates :: (MonadBaseControl IO m, MonadLog m)
                   => Bool
                   -> MVar (UTCTime, KontrakcjaGlobalTemplates) -> m KontrakcjaGlobalTemplates
maybeReadTemplates production mvar | production = snd <$> readMVar mvar
maybeReadTemplates _ mvar = modifyMVar mvar $ \(modtime, templates) -> do
  modtime' <- liftBase getTemplatesModTime
  if modtime /= modtime'
    then do
      logInfo_ "Reloading templates"
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

logRequest :: Request -> Maybe [(String, Input)] -> [Pair]
logRequest rq maybeInputsBody = [
    "request" .= (show (rqMethod rq) ++ " " ++ rqUri rq ++ rqQuery rq)
  , "post variables" .= (map showNamedInput $ fromMaybe [] maybeInputsBody)
  , "http headers" .= (concatMap showNamedHeader . Map.toList $ rqHeaders rq)
  , "http cookies" .= (map showNamedCookie $ rqCookies rq)
  ]

-- | Outer handler monad
type HandlerM = LogT (ReqHandlerT IO)
-- | Inner handler monad.
type InnerHandlerM = DBT (FileStorageT (CryptoRNGT HandlerM))

-- | Creates a context, routes the request, and handles the session.
appHandler :: Kontra (Maybe Response) -> AppConf -> AppGlobals -> HandlerM Response
appHandler handleRoutes appConf appGlobals = runHandler . localRandomID "handler_id" $ do
  realStartTime <- liftBase getCurrentTime
  temp <- liftIO getTemporaryDirectory
  let quota = 30000000
      stripFilename = reverse . takeWhile (\c -> c /= '/' && c /= '\\') . reverse
      -- just like defaultFileSaver, but accepts filenames containing full paths (they are stripped though)
      fileSaver tmpDir diskQuota filename b = defaultFileSaver tmpDir diskQuota (stripFilename filename) b
      bodyPolicy = (defaultBodyPolicy temp quota quota quota) { inputWorker = defaultInputIter fileSaver temp 0 0 0 }
  logInfo_ "Incoming request, decoding body"
  withDecodedBody bodyPolicy $ do
    rq <- askRq
    let routeLogData = ["uri" .= rqUri rq, "query" .= rqQuery rq]
    (res, ConnectionStats{..}, handlerTime) <- withPostgreSQL (unConnectionSource $ connsource appGlobals detailedConnectionTracker) $ do
      logInfo_ "Retrieving session"
      session <- getCurrentSession
      logInfo_ "Initializing context"
      ctx <- createContext session
      -- commit is needed after getting session from the database
      -- since session expiration date is updated while getting it,
      -- which results in pgsql locking the row. then, if request
      -- handler somehow gets stuck, transaction is left open for
      -- some time, row remains locked and subsequent attempts to
      -- refresh the page will fail, because they will try to
      -- access/update session from a row that was previously locked.
      commit

      logInfo "Handler started" $ object routeLogData

      (res, handlerTime) <- localData [identifier_ $ sesID session] $ do
        startTime <- liftBase getCurrentTime
        -- Make Response filters local to the main request handler so
        -- that they do not interfere with delayed response.
        ((eres, ctx'), resFilter) <- getFilter $ routeHandlers ctx
        finishTime <- liftBase getCurrentTime

        issecure <- isSecure
        let usehttps = useHttps appConf
        when (issecure || not usehttps) $ do
          logInfo_ "Updating session"
          updateSession session (get ctxsessionid ctx') (userid <$> get ctxmaybeuser ctx') (userid <$> get ctxmaybepaduser ctx')

        logInfo_ "Evaluating response"
        -- Make sure response is well defined before passing it further.
        res <- E.evaluate . force . resFilter =<< case eres of
          Right response -> return response
          Left response -> do
            rollback -- if exception was thrown, rollback everything
            return response

        return (res, timeDiff finishTime startTime)

      stats <- getConnectionStats
      return (res, stats, handlerTime)

    realFinishTime <- liftBase getCurrentTime
    logInfo "Handler finished" . object $ [
        "statistics" .= object [
            "queries" .= statsQueries
          , "rows"    .= statsRows
          , "values"  .= statsValues
          , "params"  .= statsParams
          ]
      , "elapsed_time" .= handlerTime
      , "full_time" .= timeDiff realFinishTime realStartTime
      ] ++ routeLogData

    return $ contentLength res
  where
    timeDiff :: UTCTime -> UTCTime -> Double
    timeDiff t = realToFrac . diffUTCTime t

    runHandler :: FileStorageT (CryptoRNGT HandlerM) Response
               -> HandlerM Response
    runHandler = catchEverything
      . runCryptoRNGT (cryptorng appGlobals)
      . runFileStorageT ( amazonConfig appConf, mrediscache appGlobals
                        , Just (filecache appGlobals) )

    catchEverything :: HandlerM Response -> HandlerM Response
    catchEverything m = m `E.catch` \(e::E.SomeException) -> do
      uri <- rqUri <$> askRq
      logAttention "appHandler: exception caught at top level" $ object [
          "exception" .= show e
        , "url" .= uri
        ]
      internalServerError $ toResponse ""

    routeHandlers :: Context -> InnerHandlerM (Either Response Response, Context)
    routeHandlers ctx = runKontra ctx $ do
      res <- (Right <$> (handleRoutes >>= maybe (E.throwIO Respond404) return)) `E.catches` [
          E.Handler $ \e -> Left <$> case e of
            InternalError stack -> do
              rq <- askRq
              mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
              logAttention "InternalError" . object $ [
                  "stacktrace" .= reverse stack
                ] ++ logRequest rq mbody
              internalServerErrorPage >>= internalServerError
            Respond404 -> do
              -- there is no way to get stacktrace here as Respond404 is a CAF, fix this later
              rq <- askRq
              mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
              logInfo "Respond404" . object $ logRequest rq mbody
              notFoundPage >>= notFound
            LinkInvalid -> do
              rq <- askRq
              mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
              logAttention "LinkInvalid" . object $ logRequest rq mbody
              -- We reply with 422, because the request is in correct form (so not 400)
              linkInvalidPage >>= resp 422
        , E.Handler $ \e@DBException{..} -> Left <$> do
            rq <- askRq
            mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
            stack <- liftIO $ whoCreated e
            logAttention "DBException" . object $ [
                "dbe_query_context" .= show dbeQueryContext
              , case cast dbeError of
                  Nothing -> "dbe_error" .= show dbeError
                  Just (SomeDBExtraException ee) -> "extra_exception" .= jsonToAeson (toJSValue ee)
              , "stacktrace" .= reverse stack
              ] ++ logRequest rq mbody
            internalServerErrorPage >>= internalServerError
        , E.Handler $ \e@(SomeDBExtraException ee) -> Left <$> do
            rq <- askRq
            mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
            stack <- liftIO $ whoCreated e
            logAttention "SomeDBExtraException" . object $ [
                "extra_exception" .= jsonToAeson (toJSValue ee)
              , "stacktrace" .= reverse stack
              ] ++ logRequest rq mbody
            internalServerErrorPage >>= internalServerError
        , E.Handler $ \(e :: E.SomeException) -> Left <$> do
            rq <- askRq
            mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
            stack <- liftIO $ whoCreated e
            logAttention "Exception caught in routeHandlers" . object $ [
                "exception" .= show e
              , "stacktrace" .= reverse stack
              ] ++ logRequest rq mbody
            internalServerErrorPage >>= internalServerError
        ]
      ctx' <- getContext
      return (res, ctx')

    createContext :: Session -> InnerHandlerM Context
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
      minutestime <- currentTime
      let clientName = BS.toString <$> getHeader "client-name" rq
          clientTime = parseTimeISO =<< (BS.toString <$> getHeader "client-time" rq)
          userAgent  = BS.toString <$> getHeader "user-agent" rq
      muser <- getUserFromSession session
      mpaduser <- getPadUserFromSession session
      brandeddomain <- dbQuery $ GetBrandedDomainByURL currhostpart

      -- do reload templates in non-production code
      templates2 <- maybeReadTemplates (production appConf) (templates appGlobals)

      -- work out the language
      userlang <- getStandardLang muser

      return Context {
          _ctxmaybeuser          = muser
        , _ctxtime               = minutestime
        , _ctxclientname         = clientName `mplus` userAgent
        , _ctxclienttime         = clientTime
        , _ctxipnumber           = peerip
        , _ctxproduction         = production appConf
        , _ctxcdnbaseurl         = cdnBaseUrl appConf
        , _ctxtemplates          = localizedVersion userlang templates2
        , _ctxglobaltemplates    = templates2
        , _ctxlang               = userlang
        , _ctxismailbackdooropen = isMailBackdoorOpen appConf
        , _ctxmailnoreplyaddress = mailNoreplyAddress appConf
        , _ctxgtconf             = guardTimeConf appConf
        , _ctxcgigrpconfig       = cgiGrpConfig appConf
        , _ctxmrediscache        = mrediscache appGlobals
        , _ctxfilecache          = filecache appGlobals
        , _ctxxtoken             = sesCSRFToken session
        , _ctxadminaccounts      = admins appConf
        , _ctxsalesaccounts      = sales appConf
        , _ctxmaybepaduser       = mpaduser
        , _ctxusehttps           = useHttps appConf
        , _ctxsessionid          = sesID session
        , _ctxtrackjstoken       = trackjsToken appConf
        , _ctxgatoken            = gaToken appConf
        , _ctxmixpaneltoken      = mixpanelToken appConf
        , _ctxhubspotconf        = hubspotConf appConf
        , _ctxbrandeddomain      = brandeddomain
        , _ctxsalesforceconf     = salesforceConf appConf
        , _ctxnetsconfig         = netsConfig appConf
        , _ctxisapilogenabled    = isAPILogEnabled appConf
        , _ctxnetssignconfig     = netsSignConfig appConf
        , _ctxpdftoolslambdaconf = pdfToolsLambdaConf appConf
        }
