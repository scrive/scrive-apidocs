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
import Data.Aeson.Types
import Data.Functor
import Data.Time.Clock
import Data.Typeable
import GHC.Stack
import Happstack.Server hiding (dir, host, lookCookieValue, path, simpleHTTP)
import Happstack.Server.Internal.Cookie
import Log
import Network.Socket
import System.Directory
import Text.JSON.ToJSValue
import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Database.Redis as R

import AppConf
import AppView as V
import BrandedDomain.Model
import Cookies (lookCookieValue)
import Crypto.RNG
import DB hiding (ErrorCode(..))
import DB.PostgreSQL
import File.FileID
import Happstack.Server.ReqHandler
import IPAddress
import Kontra
import KontraPrelude
import Log.Identifier
import Log.Utils
import MinutesTime
import Session.Data hiding (session)
import Session.Model
import Templates
import Text.JSON.Convert
import User.Model
import Util.FinishWith
import Utils.HTTP
import qualified Amazon as AWS
import qualified FlashMessage as F
import qualified MemCache

-- | Global application data
data AppGlobals = AppGlobals {
    templates          :: !(MVar (UTCTime, KontrakcjaGlobalTemplates))
  , mrediscache        :: !(Maybe R.Connection)
  , filecache          :: !(MemCache.MemCache FileID BS.ByteString)
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
  langcookie <- lookCookieValue "lang"
  let mcookielang = join $ langFromCode <$> langcookie
  let browserlang = langFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
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
type InnerHandlerM = DBT (AWS.AmazonMonadT (CryptoRNGT HandlerM))

-- | Creates a context, routes the request, and handles the session.
appHandler :: Kontra (Maybe Response) -> AppConf -> AppGlobals -> HandlerM Response
appHandler handleRoutes appConf appGlobals = runHandler . localRandomID "handler_id" $ do
  realStartTime <- liftBase getCurrentTime
  temp <- liftIO getTemporaryDirectory
  let quota = 10000000
      bodyPolicy = defaultBodyPolicy temp quota quota quota
  logInfo_ "Incoming request, decoding body"
  withDecodedBody bodyPolicy $ do
    rq <- askRq
    let routeLogData = ["uri" .= rqUri rq, "query" .= rqQuery rq]
    (res, mdres, ConnectionStats{..}, handlerTime) <- withPostgreSQL (unConnectionSource $ connsource appGlobals detailedConnectionTracker) $ do
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

      (res, mdres, handlerTime) <- localData [identifier_ $ sesID session] $ do
        startTime <- liftBase getCurrentTime
        -- Make Response filters local to the main request handler so
        -- that they do not interfere with delayed response.
        ((eres, ctx'), resFilter) <- getFilter $ routeHandlers ctx
        finishTime <- liftBase getCurrentTime

        F.updateFlashCookie (ctxflashmessages ctx) (ctxflashmessages ctx')
        issecure <- isSecure
        let usehttps = useHttps appConf
        when (issecure || not usehttps) $ do
          logInfo_ "Updating session"
          updateSession session (ctxsessionid ctx') (userid <$> ctxmaybeuser ctx') (userid <$> ctxmaybepaduser ctx')

        logInfo_ "Evaluating response"
        -- Make sure response is well defined before passing it further.
        res <- E.evaluate . force . resFilter =<< case eres of
          Right response -> return response
          Left response -> do
            rollback -- if exception was thrown, rollback everything
            return response

        return (res, ctxdelayedresponse ctx', timeDiff finishTime startTime)

      stats <- getConnectionStats
      return (res, mdres, stats, handlerTime)

    -- Override response with delayed one if appropriate.
    finalResponse <- contentLength <$> case mdres of
      Nothing -> return res
      Just dr -> do
        logInfo_ "Waiting for delayed response"
        maybe (return res) (E.evaluate . force) =<< unDelayedResponse dr

    realFinishTime <- liftBase getCurrentTime
    logInfo "Handler finished" . object $ [
        "statistics" .= object [
            "queries" .= statsQueries
          , "rows"    .= statsRows
          , "values"  .= statsValues
          , "params"  .= statsParams
          ]
      , "time" .= handlerTime
      , "full_time" .= timeDiff realFinishTime realStartTime
      ] ++ routeLogData

    return finalResponse
  where
    timeDiff :: UTCTime -> UTCTime -> Double
    timeDiff t = realToFrac . diffUTCTime t

    runHandler :: AWS.AmazonMonadT (CryptoRNGT HandlerM) Response
               -> HandlerM Response
    runHandler = catchEverything
      . runCryptoRNGT (cryptorng appGlobals)
      . AWS.runAmazonMonadT (AWS.AmazonConfig (amazonConfig appConf) (filecache appGlobals) $ mrediscache appGlobals)

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
              logAttention "Respond404" . object $ logRequest rq mbody
              notFoundPage >>= notFound
        , E.Handler $ \(FinishWith res ctx') -> do
            modifyContext $ const ctx'
            return $ Right res
        , E.Handler $ \e@DBException{..} -> Left <$> do
            rq <- askRq
            mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
            stack <- liftIO $ whoCreated e
            logAttention "DBException" . object $ [
                "dbeQueryContext" .= show dbeQueryContext
              , case cast dbeError of
                  Nothing -> "dbeError" .= show dbeError
                  Just (SomeDBExtraException ee) -> "exception" .= jsonToAeson (toJSValue ee)
              , "stacktrace" .= reverse stack
              ] ++ logRequest rq mbody
            internalServerErrorPage >>= internalServerError
        , E.Handler $ \e@(SomeDBExtraException ee) -> Left <$> do
            rq <- askRq
            mbody <- liftIO (tryReadMVar $ rqInputsBody rq)
            stack <- liftIO $ whoCreated e
            logAttention "SomeDBExtraException" . object $ [
                "exception" .= jsonToAeson (toJSValue ee)
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
            peerhost = $head $ catMaybes $
                         [ BS.toString <$> getHeader h rq
                         |  h <- ["x-forwarded-for", "x-real-ip"]
                         ] ++ [Just (fst (rqPeer rq))]
            hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST] }
        (do addrs <- liftIO $ getAddrInfo (Just hints) (Just peerhost) Nothing
            return $ case addrAddress $ $head addrs of
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

      mflashmessages <- F.flashDataFromCookie
      flashmessages <- case mflashmessages of
                        Nothing -> return []
                        Just fs -> do
                          flashes <- liftIO $ F.fromCookieValue fs
                          case flashes of
                            Just fs' -> return fs'
                            _ -> do
                              logInfo_ "Couldn't read flash messages"
                              F.removeFlashCookie
                              return []

      -- do reload templates in non-production code
      templates2 <- maybeReadTemplates (production appConf) (templates appGlobals)

      -- work out the language
      userlang <- getStandardLang muser

      return Context {
          ctxmaybeuser = muser
        , ctxflashmessages = flashmessages
        , ctxtime = minutestime
        , ctxclientname = clientName `mplus` userAgent
        , ctxclienttime = clientTime
        , ctxipnumber = peerip
        , ctxproduction = production appConf
        , ctxcdnbaseurl = cdnBaseUrl appConf
        , ctxtemplates = localizedVersion userlang templates2
        , ctxglobaltemplates = templates2
        , ctxlang = userlang
        , ctxismailbackdooropen = isMailBackdoorOpen appConf
        , ctxgtconf = guardTimeConf appConf
        , ctxcgigrpconfig = cgiGrpConfig appConf
        , ctxmrediscache = mrediscache appGlobals
        , ctxfilecache = filecache appGlobals
        , ctxxtoken = sesCSRFToken session
        , ctxadminaccounts = admins appConf
        , ctxsalesaccounts = sales appConf
        , ctxmaybepaduser = mpaduser
        , ctxusehttps = useHttps appConf
        , ctxrecurlyconfig = recurlyConfig appConf
        , ctxsessionid = sesID session
        , ctxtrackjstoken = trackjsToken appConf
        , ctxmixpaneltoken = mixpanelToken appConf
        , ctxhubspotconf = hubspotConf appConf
        , ctxbrandeddomain = brandeddomain
        , ctxsalesforceconf = salesforceConf appConf
        , ctxnetsconfig = netsConfig appConf
        , ctxdelayedresponse = Nothing
        , ctxthreadjoins = []
        }
