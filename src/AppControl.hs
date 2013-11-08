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

import AppConf

import qualified Amazon as AWS
import AppView as V
import Crypto.RNG
import DB
import DB.PostgreSQL
import IPAddress
import Kontra
import MinutesTime (parseMinutesTimeRealISO)
import Utils.Either
import Utils.HTTP
import Utils.Monoid
import OurServerPart
import Session.Data hiding (session)
import Session.Model
import Templates
import User.Model
import qualified Log (error, debug)
import qualified FlashMessage as F
import qualified MemCache
import Util.FinishWith
import Util.FlashUtil
import File.FileID

import Control.Concurrent
import Control.Concurrent.MVar.Util (tryReadMVar)
import Control.Monad.Error
import Data.Functor
import Data.Maybe
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Happstack.Server.Internal.Cookie
import Network.Socket

import System.Directory
import System.Time
import Data.Time.Clock

import qualified Control.Exception.Lifted as E
import Doc.JpegPages
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Static.Resources as SR
import Salesforce.Conf

{- |
  Global application data
-}
data AppGlobals
    = AppGlobals { templates       :: MVar (UTCTime, KontrakcjaGlobalTemplates)
                 , filecache       :: MemCache.MemCache FileID BS.ByteString
                 , docscache       :: MemCache.MemCache FileID JpegPages
                 , cryptorng       :: CryptoRNGState
                 , staticResources :: MVar SR.ResourceSetsForImport
                 }


{- |
    Determines the lang of the current user (whether they are logged in or not), by checking
    their settings, the request, and cookies.
-}
getStandardLang :: (HasLang a, HasRqData m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m, Functor m) => Maybe a -> m Lang

getStandardLang muser = do
  rq <- askRq
  currentcookielang <- optional $ readCookieValue "lang"
  let browserlang = langFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
      newlang = fromMaybe browserlang $ msum [(getLang <$> muser), currentcookielang]
      newlangcookie = mkCookie "lang" (show newlang)
  addCookie (MaxAge (60*60*24*366)) newlangcookie
  return newlang

maybeReadTemplates :: Bool -> MVar (UTCTime, KontrakcjaGlobalTemplates) -> IO KontrakcjaGlobalTemplates
maybeReadTemplates production mvar = modifyMVar mvar $ \(modtime, templates) -> do
        if (production)
         then return ((modtime, templates), templates)
         else do
              modtime' <- getTemplatesModTime
              if modtime /= modtime'
               then do
                   Log.debug $ "Reloading templates"
                   templates' <- readGlobalTemplates
                   return ((modtime', templates'), templates')
               else return ((modtime, templates), templates)

maybeReadStaticResources :: Bool -> MVar SR.ResourceSetsForImport -> String -> IO SR.ResourceSetsForImport
maybeReadStaticResources production mvar srConfigForAppConf = modifyMVar mvar $ \sr ->
        if (production)
         then return $  (sr,sr)
         else do
              mtime <- SR.resourcesMTime srConfigForAppConf
              if (SR.generationTime sr < mtime)
               then do
                   sr' <- fromRight <$> SR.getResourceSetsForImport SR.Development srConfigForAppConf ""
                   return (sr',sr')
               else return $  (sr,sr)


-- | Show nicely formated headers. Same header lines can appear
-- multiple times in HTTP so we need to beautifully show them.  We
-- also skip 'cookies' header as we show it later in a nicer form.
showNamedHeader :: (a, HeaderPair) -> [String]
showNamedHeader (_nm,hd) | hName hd == BS.fromString "cookie" = []
showNamedHeader (_nm,hd) = map showHeaderLine (hValue hd)
  where
    showHeaderLine value = BS.toString (hName hd) ++ ": " ++ BS.toString value

showNamedCookie :: (String, Cookie) -> String
showNamedCookie (_name,cookie) = mkCookieHeader Nothing cookie

showNamedInput :: (String, Input) -> String
showNamedInput (name,input) = name ++ ": " ++
    case inputFilename input of
      Just filename -> filename
      _ -> case inputValue input of
             Left _tmpfilename -> "<<content in /tmp>>"
             Right value -> show (BSL.toString value)

showRequest :: Request -> Maybe [(String, Input)] -> String
showRequest rq maybeInputsBody = unlines $
    [show (rqMethod rq) ++ " " ++ rqUri rq ++ rqQuery rq] ++
    (indented "post variables" . map showNamedInput) (fromMaybe [] maybeInputsBody) ++
    (indented "http headers" . concatMap showNamedHeader) (Map.toList $ rqHeaders rq) ++
    (indented "http cookies" . map showNamedCookie) (rqCookies rq)
  where
    indented _title [] = []
    indented title list =
        ["  " ++ title ++ ":"] ++
        map (\line -> "    " ++ line) list


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
appHandler :: KontraPlus Response -> AppConf -> AppGlobals -> ServerPartT IO Response
appHandler handleRoutes appConf appGlobals = catchEverything . runOurServerPartT . enhanceYourCalm $
  withPostgreSQL (dbConfig appConf) . runCryptoRNGT (cryptorng appGlobals) $
    AWS.runAmazonMonadT amazoncfg $ do
    startTime <- liftIO getClockTime
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
    kCommit
    rq <- askRq
    Log.debug $ "Handling routes for : " ++ rqUri rq ++ rqQuery rq

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

    stats <- getNexusStats =<< getNexus
    finishTime <- liftIO getClockTime
    let TOD ss sp = startTime
        TOD fs fp = finishTime
        diff = (fs - ss) * 1000000000000 + fp - sp

    Log.debug $ "SQL stats: " ++ rqUri rq ++ rqQuery rq ++
                "\n    " ++ show stats ++ ", time: " ++ show (diff `div` 1000000000) ++ "ms"

    case res of
      Right response -> return response
      Left response -> do
        kRollback -- if exception was thrown, rollback everything
        return response
  where
    amazoncfg = AWS.AmazonConfig (amazonConfig appConf) (filecache appGlobals)
    catchEverything m = m `E.catch` \(e::E.SomeException) -> do
      lift $ Log.error $ "appHandler: exception caught at top level: " ++ show e ++ " (this shouldn't happen)"
      internalServerError $ toResponse ""

    routeHandlers ctx = runKontraPlus ctx $ do
      res <- (Right <$> handleRoutes `mplus` E.throwIO Respond404) `E.catches` [
          E.Handler handleKontraError
        , E.Handler $ \(FinishWith res ctx') -> do
            modifyContext $ const ctx'
            return $ Right res
        , E.Handler $ \(e::E.SomeException) -> do
          Log.error $ "Exception caught in routeHandlers: " ++ show e
          handleKontraError InternalError
        ]
      ctx' <- getContext
      return (res, ctx')

    handleKontraError e = Left <$> do
      rq <- askRq
      liftIO (tryReadMVar $ rqInputsBody rq)
        >>= Log.error . shows e . (++) ": " . showRequest rq
      case e of
        InternalError -> internalServerErrorPage >>= internalServerError
        Respond404 -> notFoundPage >>= notFound

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
      minutestime <- getMinutesTime
      let clientName = BS.toString <$> getHeader "client-name" rq
          clientTime = parseMinutesTimeRealISO =<< (BS.toString <$> getHeader "client-time" rq)
          userAgent  = BS.toString <$> getHeader "user-agent" rq
      muser <- getUserFromSession session
      mpaduser <- getPadUserFromSession session

      flashmessages <- withDataFn F.flashDataFromCookie $ maybe (return []) $ \fval -> do
        flashes <- liftIO $ (E.try (E.evaluate $ F.fromCookieValue fval) :: IO (Either  E.SomeException (Maybe [FlashMessage])))
        case flashes of
          Right (Just fs) -> return fs
          _ -> do
            Log.error $ "Couldn't read flash messages from value: " ++ fval
            F.removeFlashCookie
            return []

      -- do reload templates in non-production code
      templates2 <- liftIO $ maybeReadTemplates (production appConf) (templates appGlobals)

      -- regenerate static files that need to be regenerated in development mode
      staticRes <-  liftIO $ maybeReadStaticResources (production appConf) (staticResources appGlobals) (srConfig appConf)

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
        , ctxstaticresources = staticRes
        , ctxusehttps = useHttps appConf
        , ctxrecurlyconfig = recurlyConfig appConf
        , ctxsessionid = sesID session
        , ctxmixpaneltoken = mixpanelToken appConf
        , ctxhomebase = homebase appConf
        , ctxbrandeddomains = brandedDomains appConf
        , ctxsalesforceconf = getSalesforceConf appConf
        }
