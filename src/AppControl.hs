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
import Crypto.RNG (CryptoRNGState, random, inIO)
import DB.Classes
import Doc.DocStateData
import IPAddress
import Kontra
import KontraError (KontraError(..))
import MinutesTime
import Misc
import Redirect
import Session
import Templates.Templates
import User.Model
import qualified Log (error, debug)
import qualified FlashMessage as F
import qualified MemCache
import qualified TrustWeaver as TW
import Util.FlashUtil
import Util.KontraLinkUtils
import File.FileID

import Control.Concurrent
import Control.Concurrent.MVar.Util (tryReadMVar)
import Control.Monad.Error
import Data.Functor
import Data.List
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import DB.Nexus
import GHC.Int (Int64(..))
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Happstack.Server.Internal.Cookie
import Network.Socket
import System.Directory
import System.Time


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP


{- |
  Global application data
-}
data AppGlobals
    = AppGlobals { templates       :: MVar (ClockTime, KontrakcjaGlobalTemplates)
                 , filecache       :: MemCache.MemCache FileID BS.ByteString
                 , docscache       :: MVar (Map.Map FileID JpegPages)
                 , cryptorng       :: CryptoRNGState
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
    Handles an error by displaying the home page with a modal error dialog.
-}
handleError :: (MonadIO m, Kontrakcja m) => KontraError -> m Response
handleError e = do
     rq <- askRq
     Log.error (show e)
     liftIO (tryReadMVar (rqInputsBody rq)) >>= Log.error . showRequest rq
     handleError' e
  where
  handleError' :: Kontrakcja m => KontraError -> m Response
  handleError' InternalError = do
    ctx <- getContext
    case ctxservice ctx of
         Nothing -> do
            addFlashM V.modalError
            linkmain <- getHomeOrUploadLink
            sendRedirect linkmain
         _ -> embeddedErrorPage
  handleError' Respond404 = do
    ctx <- getContext
    case ctxservice ctx of
         Nothing -> notFoundPage >>= notFound
         _ -> embeddedErrorPage

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
appHandler :: Kontra' Response -> AppConf -> AppGlobals -> ServerPartT IO Response
appHandler handleRoutes appConf appGlobals = do
  startTime <- liftIO getClockTime

  let quota :: GHC.Int.Int64 = 10000000

  temp <- liftIO $ getTemporaryDirectory
  decodeBody (defaultBodyPolicy temp quota quota quota)

  session <- handleSession (cryptorng appGlobals)
  ctx <- createContext session
  response <- handle session ctx
  finishTime <- liftIO getClockTime
  let TOD ss sp = startTime
      TOD fs fp = finishTime
      diff = (fs - ss) * 1000000000000 + fp - sp
  Log.debug $ "Response time " ++ show (diff `div` 1000000000) ++ "ms"
  return response
  where
    handle :: Session -> Context -> ServerPartT IO Response
    handle session ctx = do
      Right (res, ctx') <- runKontra' ctx $ do
          res <- (handleRoutes `mplus` throwError Respond404) `catchError` handleError
          ctx' <- getContext
          return (res,ctx')

      let newsessionuser = fmap userid $ ctxmaybeuser ctx'
      let newsessionpaduser = fmap userid $ ctxmaybepaduser ctx'
      let newflashmessages = ctxflashmessages ctx'
      let newelegtrans = ctxelegtransactions ctx'
      let newmagichashes = ctxmagichashes ctx'
      F.updateFlashCookie (aesConfig appConf) (ctxflashmessages ctx) newflashmessages
      rng <- inIO (cryptorng appGlobals) random
      updateSessionWithContextData rng session newsessionuser newelegtrans newmagichashes newsessionpaduser
      stats <- liftIO $ getNexusStats (nexus $ ctxdbenv ctx')
      rq <- askRq
      Log.debug $ "SQL for " ++ rqUri rq ++ ": " ++ show stats

      liftIO $ disconnect $ nexus $ ctxdbenv ctx'
      return res

    createContext session = do
      rq <- askRq
      currhostpart <- getHostpart
      -- FIXME: we should read some headers from upstream proxy, if any
      let peerhost = case getHeader "x-real-ip" rq of
                       Just name -> BS.toString name
                       Nothing -> fst (rqPeer rq)

      -- rqPeer hostname comes always from showHostAddress
      -- so it is a bunch of numbers, just read them out
      -- getAddrInfo is strange that it can throw exceptions
      -- if exception is thrown, whole page load fails with
      -- error notification
      let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_NUMERICHOST] }
      addrs <- liftIO $ getAddrInfo (Just hints) (Just peerhost) Nothing
      let addr = head addrs
      let peerip = case addrAddress addr of
            SockAddrInet _ hostip -> unsafeIPAddress hostip
            _                     -> noIP

      psqlconn <- liftIO $ connectPostgreSQL $ dbConfig appConf
      dbenv <- mkDBEnv psqlconn (cryptorng appGlobals)
      minutestime <- liftIO getMinutesTime
      muser <- getUserFromSession dbenv session
      mcompany <- getCompanyFromSession dbenv session
      mpaduser <- getPadUserFromSession dbenv session
      location <- getLocationFromSession session
      mservice <- currentLink >>= \clink -> if (hostpart appConf `isPrefixOf` clink)
                                             then return Nothing
                                             else ioRunDB dbenv $ dbQuery $ GetServiceByLocation $ toServiceLocation clink

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

      let elegtrans = getELegTransactions session
          ctx = Context
                { ctxmaybeuser = muser
                , ctxhostpart = currhostpart
                , ctxflashmessages = flashmessages
                , ctxtime = minutestime
                , ctxnormalizeddocuments = docscache appGlobals
                , ctxipnumber = peerip
                , ctxdbenv = dbenv
                , ctxdocstore = docstore appConf
                , ctxs3action = defaultAWSAction appConf
                , ctxgscmd = gsCmd appConf
                , ctxproduction = production appConf
                , ctxtemplates = localizedVersion userlocale templates2
                , ctxglobaltemplates = templates2
                , ctxlocale = userlocale
                , ctxlocaleswitch = True
                , ctxmailsconfig = mailsConfig appConf
                , ctxtwconf = TW.TrustWeaverConf
                              { TW.signConf = trustWeaverSign appConf
                              , TW.adminConf = trustWeaverAdmin appConf
                              , TW.storageConf = trustWeaverStorage appConf
                              , TW.retries = 3
                              , TW.timeout = 60000
                              }
                , ctxelegtransactions = elegtrans
                , ctxfilecache = filecache appGlobals
                , ctxxtoken = getSessionXToken session
                , ctxcompany = mcompany
                , ctxservice = mservice
                , ctxlocation = location
                , ctxadminaccounts = admins appConf
                , ctxsalesaccounts = sales appConf
                , ctxmagichashes = getMagicHashes session
                , ctxmaybepaduser = mpaduser
                }
      return ctx

