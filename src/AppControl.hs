{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module AppControl
    ( appHandler
    , AppGlobals(..)
    , maybeReadTemplates
    -- exported for the sake of unit tests
    , getStandardLocale
    ) where

import AppConf

import Amazon
import AppView as V
import Crypto.RNG
import DB
import DB.PostgreSQL
import Doc.DocStateData
import IPAddress
import Kontra
import MinutesTime
import Utils.Either
import Utils.HTTP
import Utils.Monoid
import OurServerPart
import Redirect
import Session.Data hiding (session)
import Session.Model
import Templates.TemplatesLoader
import User.Model
import qualified Log (error, debug)
import qualified FlashMessage as F
import qualified MemCache
import Util.FinishWith
import Util.FlashUtil
import KontraLink
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

import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Static.Resources as SR

{- |
  Global application data
-}
data AppGlobals
    = AppGlobals { templates       :: MVar (ClockTime, KontrakcjaGlobalTemplates)
                 , filecache       :: MemCache.MemCache FileID BS.ByteString
                 , docscache       :: MemCache.MemCache FileID JpegPages
                 , cryptorng       :: CryptoRNGState
                 , staticResources :: MVar SR.ResourceSetsForImport
                 }


{- |
    Determines the locale of the current user (whether they are logged in or not), by checking
    their settings, the request, and cookies.
-}
getStandardLocale :: (HasLocale a, HasRqData m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m, Functor m) => Maybe a -> m Locale

getStandardLocale muser = do
  rq <- askRq
  currentcookielocale <- optional $ readCookieValue "locale"
  let urlregion = (listToMaybe $ rqPaths rq) >>= regionFromCode
      urllang = (listToMaybe . drop 1 $ rqPaths rq) >>= langFromCode
      urllocale = case (urlregion, urllang) of
                    (Just region, Just lang) -> Just $ mkLocale region lang
                    _ -> Nothing
      browserlocale =  mkLocaleFromRegion $ regionFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
      newlocale = fromMaybe browserlocale $ msum [urllocale, (getLocale <$> muser), currentcookielocale]
      newlocalecookie = mkCookie "locale" (show newlocale)
  addCookie (MaxAge (60*60*24*366)) newlocalecookie
  return newlocale

maybeReadTemplates :: Bool -> MVar (ClockTime, KontrakcjaGlobalTemplates) -> IO KontrakcjaGlobalTemplates
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
showRequest rq maybeInputsBody =
    show (rqMethod rq) ++ " " ++ rqUri rq ++ rqQuery rq ++ "\n" ++
    (unlinesIndented "post variables" . map showNamedInput) (fromMaybe [] maybeInputsBody) ++
    (unlinesIndented "http headers" . concatMap showNamedHeader) (Map.toList $ rqHeaders rq) ++
    (unlinesIndented "http cookies" . map showNamedCookie) (rqCookies rq)
  where
    unlinesIndented _title [] = ""
    unlinesIndented title list = "  " ++ title ++ ":\n" ++
        concatMap (\line -> "    " ++ line ++ "\n") list

{- |
   Creates a context, routes the request, and handles the session.
-}
appHandler :: KontraPlus Response -> AppConf -> AppGlobals -> ServerPartT IO Response
appHandler handleRoutes appConf appGlobals = catchEverything . runOurServerPartT $
  withPostgreSQL (dbConfig appConf) . runCryptoRNGT (cryptorng appGlobals) $ do
    startTime <- liftIO getClockTime
    let quota = 10000000
    temp <- liftIO getTemporaryDirectory
    decodeBody (defaultBodyPolicy temp quota quota quota)
    session <- getCurrentSession
    ctx <- createContext session

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

    rq <- askRq
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
        dbRollback -- if exception was thrown, rollback everything
        return response
  where
    catchEverything m = m `E.catch` \(e::E.SomeException) -> do
      Log.error $ "appHandler: exception caught at top level: " ++ show e ++ " (this shouldn't happen)"
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
        InternalError -> do
          addFlashM V.modalError
          linkmain <- getHomeOrArchiveLink
          sendRedirect linkmain
        Respond404 -> notFoundPage >>= notFound

    createContext session = do
      -- rqPeer hostname comes always from showHostAddress
      -- so it is a bunch of numbers, just read them out
      -- getAddrInfo is strange that it can throw exceptions
      -- if exception is thrown, whole page load fails with
      -- error notification
      peerip <- do
        rq <- askRq
        -- FIXME: we should read some headers from upstream proxy, if any
        let peerhost = case getHeader "x-real-ip" rq of
              Just name -> BS.toString name
              Nothing -> fst (rqPeer rq)
            hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST] }
        addrs <- liftIO $ getAddrInfo (Just hints) (Just peerhost) Nothing
        return $ case addrAddress $ head addrs of
          SockAddrInet _ hostip -> unsafeIPAddress hostip
          _                     -> noIP

      currhostpart <- getHostpart
      reshostpart <- getResourceHostpart
      minutestime <- getMinutesTime
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

      -- work out the region and language
      userlocale <- getStandardLocale muser

      return Context {
          ctxmaybeuser = muser
        , ctxhostpart = currhostpart
        , ctxresourcehostpart = reshostpart
        , ctxflashmessages = flashmessages
        , ctxtime = minutestime
        , ctxnormalizeddocuments = docscache appGlobals
        , ctxipnumber = peerip
        , ctxs3action = mkAWSAction appConf
        , ctxproduction = production appConf
        , ctxtemplates = localizedVersion userlocale templates2
        , ctxglobaltemplates = templates2
        , ctxlocale = userlocale
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
        }
