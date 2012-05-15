{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module AppControl
    ( module AppConf
    , appHandler
    , AppGlobals(..)
    , defaultAWSAction

    -- exported for the sake of unit tests
    , getStandardLocale
    ) where

import AppConf
import API.Service.Model

import AppView as V
import Crypto.RNG
import DB
import DB.PostgreSQL
import Doc.DocStateData
import IPAddress
import Kontra
import MinutesTime
import Misc
import Redirect
import Session
import Templates.TemplatesLoader
import User.Model
import qualified Log (error, debug)
import qualified FlashMessage as F
import qualified MemCache
import qualified TrustWeaver as TW
import Util.FinishWith
import Util.FlashUtil
import Util.KontraLinkUtils
import File.FileID

import Control.Concurrent
import Control.Concurrent.MVar.Util (tryReadMVar)
import Control.Monad.Error
import Data.Functor
import Data.List
import Data.Maybe
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Happstack.Server.Internal.Cookie
import Network.Socket
import System.Directory
import System.Time

import qualified Control.Exception.Lifted as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import qualified Static.Resources as SR

{- |
  Global application data
-}
data AppGlobals
    = AppGlobals { templates       :: MVar (ClockTime, KontrakcjaGlobalTemplates)
                 , filecache       :: MemCache.MemCache FileID BS.ByteString
                 , docscache       :: MVar (Map.Map FileID JpegPages)
                 , cryptorng       :: CryptoRNGState
                 , staticResources :: SR.ResourceSetsForImport
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

{- |
   Creates a default amazon configuration based on the
   given AppConf
-}
defaultAWSAction :: AppConf -> AWS.S3Action
defaultAWSAction appConf =
    let (bucket,accessKey,secretKey) = maybe ("","","") id (amazonConfig appConf)
    in
    AWS.S3Action
           { AWS.s3conn = AWS.amazonS3Connection accessKey secretKey
           , AWS.s3bucket = bucket
           , AWS.s3object = ""
           , AWS.s3query = ""
           , AWS.s3metadata = []
           , AWS.s3body = BSL.empty
           , AWS.s3operation = HTTP.GET
           }


maybeReadTemplates :: MVar (ClockTime, KontrakcjaGlobalTemplates)
                      -> IO KontrakcjaGlobalTemplates
maybeReadTemplates mvar = modifyMVar mvar $ \(modtime, templates) -> do
        modtime' <- getTemplatesModTime
        if modtime /= modtime'
            then do
                Log.debug $ "Reloading templates"
                templates' <- readGlobalTemplates
                return ((modtime', templates'), templates')
            else return ((modtime, templates), templates)

showNamedHeader :: forall t . (t, HeaderPair) -> [Char]
showNamedHeader (_nm,hd) = BS.toString (hName hd) ++ ": [" ++
                      concat (intersperse ", " (map (show . BS.toString) (hValue hd))) ++ "]"

showNamedCookie :: ([Char], Cookie) -> [Char]
showNamedCookie (name,cookie) = name ++ ": " ++ mkCookieHeader Nothing cookie

showNamedInput :: ([Char], Input) -> [Char]
showNamedInput (name,input) = name ++ ": " ++ case inputFilename input of
                                                  Just filename -> filename
                                                  _ -> case inputValue input of
                                                           Left _tmpfilename -> "<<content in /tmp>>"
                                                           Right value -> show (BSL.toString value)

showRequest :: Request -> Maybe [([Char], Input)] -> [Char]
showRequest rq maybeInputsBody =
    show (rqMethod rq) ++ " " ++ rqUri rq ++ rqQuery rq ++ "\n" ++
    "post variables:\n" ++
    maybe "" (unlines . map showNamedInput) maybeInputsBody ++
    "http headers:\n" ++
    (unlines $ map showNamedHeader (Map.toList $ rqHeaders rq)) ++
    "http cookies:\n" ++
    (unlines $ map showNamedCookie (rqCookies rq))

{- |
   Creates a context, routes the request, and handles the session.
-}
appHandler :: KontraPlus Response -> AppConf -> AppGlobals -> ServerPartT IO Response
appHandler handleRoutes appConf appGlobals = measureResponseTime $
  withPostgreSQL (dbConfig appConf) . runCryptoRNGT (cryptorng appGlobals) $ do
    let quota = 10000000
    temp <- liftIO getTemporaryDirectory
    decodeBody (defaultBodyPolicy temp quota quota quota)
    session <- handleSession
    ctx <- createContext session

    (res, ctx') <- routeHandlers ctx

    let newsessionuser = userid <$> ctxmaybeuser ctx'
        newsessionpaduser = userid <$> ctxmaybepaduser ctx'
        newflashmessages = ctxflashmessages ctx'
        newelegtrans = ctxelegtransactions ctx'
        newmagichashes = ctxmagichashes ctx'
    F.updateFlashCookie (aesConfig appConf) (ctxflashmessages ctx) newflashmessages
    updateSessionWithContextData session newsessionuser newelegtrans newmagichashes newsessionpaduser

    rq <- askRq
    stats <- getNexusStats =<< getNexus
    Log.debug $ "SQL for " ++ rqUri rq ++ ": " ++ show stats

    case res of
      Right response -> return response
      Left response -> do
        dbRollback -- if exception was thrown, rollback everything
        return response
  where
    measureResponseTime action = do
      startTime <- liftIO getClockTime
      res <- action
      finishTime <- liftIO getClockTime
      let TOD ss sp = startTime
          TOD fs fp = finishTime
          diff = (fs - ss) * 1000000000000 + fp - sp
      Log.debug $ "Response time " ++ show (diff `div` 1000000000) ++ "ms"
      return res

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
      Log.error $ show e
      liftIO (tryReadMVar $ rqInputsBody rq)
        >>= Log.error . showRequest rq
      service <- ctxservice <$> getContext
      case e of
        InternalError -> case service of
          Nothing -> do
            addFlashM V.modalError
            linkmain <- getHomeOrUploadLink
            sendRedirect linkmain
          _ -> embeddedErrorPage
        Respond404 -> case service of
          Nothing -> notFoundPage >>= notFound
          _ -> embeddedErrorPage

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
      mcompany <- getCompanyFromSession session
      mpaduser <- getPadUserFromSession session
      mservice <- do
        clink <- currentLink
        if hostpart appConf `isPrefixOf` clink
          then return Nothing
          else dbQuery $ GetServiceByLocation $ toServiceLocation clink

      flashmessages <- withDataFn F.flashDataFromCookie $ maybe (return []) $ \fval ->
        case F.fromCookieValue (aesConfig appConf) fval of
          Just flashes -> return flashes
          Nothing -> do
            Log.error $ "Couldn't read flash messages from value: " ++ fval
            F.removeFlashCookie
            return []

      -- do reload templates in non-production code
      templates2 <- liftIO $ maybeReadTemplates (templates appGlobals)

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
        , ctxdocstore = docstore appConf
        , ctxs3action = defaultAWSAction appConf
        , ctxgscmd = gsCmd appConf
        , ctxproduction = production appConf
        , ctxtemplates = localizedVersion userlocale templates2
        , ctxglobaltemplates = templates2
        , ctxlocale = userlocale
        , ctxlocaleswitch = True
        , ctxmailsconfig = mailsConfig appConf
        , ctxtwconf = TW.TrustWeaverConf {
            TW.signConf = trustWeaverSign appConf
          , TW.adminConf = trustWeaverAdmin appConf
          , TW.storageConf = trustWeaverStorage appConf
          , TW.retries = 3
          , TW.timeout = 60000
          }
        , ctxlivedocxconf = liveDocxConfig appConf
        , ctxelegtransactions = getELegTransactions session
        , ctxfilecache = filecache appGlobals
        , ctxxtoken = getSessionXToken session
        , ctxcompany = mcompany
        , ctxservice = mservice
        , ctxlocation = getLocationFromSession session
        , ctxadminaccounts = admins appConf
        , ctxsalesaccounts = sales appConf
        , ctxmagichashes = getMagicHashes session
        , ctxmaybepaduser = mpaduser
        , ctxstaticresources = staticResources appGlobals
        }
