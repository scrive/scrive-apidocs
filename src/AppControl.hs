{- |
   Initialises contexts and sessions, and farms requests out to the appropriate handlers.
 -}
module AppControl
    ( module AppConf
    , appHandler
    , AppGlobals(..)
    , defaultAWSAction

    -- exported for the sake of unit tests
    , handleLoginPost
    , getDocumentLocale
    , getUserLocale
    , signupPagePost
    ) where

import AppConf
import API.Service.Model

import ActionSchedulerState
import AppView as V
import DB.Classes
import Doc.DocState
import InputValidation
import Kontra
import KontraLink
import Mails.MailsConfig
import Mails.SendMail
import Numeric
import MinutesTime
import Misc
--import PayEx.PayExInterface ()-- Import so at least we check if it compiles
import Redirect
import Session
import Templates.Templates
import User.Model
import User.UserView as UserView
import qualified AppLogger as Log (error, debug)
import qualified Doc.DocControl as DocControl
import qualified FlashMessage as F
import qualified MemCache
import qualified TrustWeaver as TW
import qualified User.UserControl as UserControl
import Util.FlashUtil
import Util.HasSomeUserInfo
import Util.KontraLinkUtils
import File.FileID

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.Reader
import Data.Functor
import Data.List
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import GHC.Int (Int64(..))
import Happstack.Server hiding (simpleHTTP, host, dir, path)
import Happstack.Server.Internal.Cookie
import Happstack.State (query)
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
                 , mailer          :: Mailer
                 , appbackdooropen    :: Bool --whether a backdoor used to get email content is open or not
                 , docscache       :: MVar (Map.Map FileID JpegPages)
                 , esenforcer      :: MVar ()
                 }

{- |
    If the current request is referring to a document then this will
    return the locale of that document.
-}
getDocumentLocale :: Connection -> (ServerMonad m, Functor m, MonadIO m) => m (Maybe Locale)
getDocumentLocale conn = do
  rq <- askRq
  let docids = catMaybes . map (fmap fst . listToMaybe . readDec) $ rqPaths rq
  mdoclocales <- runReaderT (mapM (DocControl.getDocumentLocale . DocumentID) docids) conn
  return . listToMaybe $ catMaybes mdoclocales

{- |
    Determines the locale of the current user (whether they are logged in or not), by checking
    their settings, the request, and cookies.
-}
getUserLocale :: (MonadPlus m, MonadIO m, ServerMonad m, FilterMonad Response m, Functor m, HasRqData m) =>
                   Connection -> Maybe User -> m Locale
getUserLocale conn muser = do
  rq <- askRq
  currentcookielocale <- optional (readCookieValue "locale")
  activationlocale <- getActivationLocale rq
  let userlocale = locale <$> usersettings <$> muser
      urlregion = (listToMaybe $ rqPaths rq) >>= regionFromCode
      urllang = (listToMaybe . drop 1 $ rqPaths rq) >>= langFromCode
      urllocale = case (urlregion, urllang) of
                    (Just region, Just lang) -> Just $ mkLocale region lang
                    _ -> Nothing
  doclocale <- getDocumentLocale conn
  let browserlocale = getBrowserLocale rq
  let newlocale = firstOf [ activationlocale
                          , userlocale
                          , doclocale
                          , urllocale
                          , currentcookielocale
                          , Just browserlocale
                          ]
  let newlocalecookie = mkCookie "locale" (show newlocale)
  addCookie (MaxAge (60*60*24*366)) newlocalecookie
  return newlocale
  where
    getBrowserLocale rq =
      mkLocaleFromRegion $ regionFromHTTPHeader (fromMaybe "" $ BS.toString <$> getHeader "Accept-Language" rq)
    -- try and get the locale from the current activation user by checking the path for action ids, and giving them a go
    getActivationLocale rq = do
      let actionids = catMaybes . map (fmap fst . listToMaybe . readDec) $ rqPaths rq
      mactionlocales <- mapM (getActivationLocaleFromAction . ActionID) actionids
      return . listToMaybe $ catMaybes mactionlocales
    getActivationLocaleFromAction aid = do
      maction <- query $ GetAction aid
      mactionuser <- case fmap actionType maction of
                       Just (AccountCreatedBySigning _ uid _ _) -> ioRunDB conn . dbQuery $ GetUserByID uid
                       Just (AccountCreated uid _) -> ioRunDB conn . dbQuery $ GetUserByID uid
                       _ -> return Nothing
      return $ fmap (locale . usersettings) mactionuser
    optional c = (liftM Just c) `mplus` (return Nothing)
    firstOf :: Bounded a => [Maybe a] -> a
    firstOf opts =
      case find isJust opts of
        Just val -> fromJust val
        Nothing -> defaultValue


{- |
    Handles an error by displaying the home page with a modal error dialog.
-}
handleError :: Kontra Response
handleError = do
    ctx <- getContext
    case (ctxservice ctx) of
         Nothing -> do
            addFlashM V.modalError
            linkmain <- getHomeOrUploadLink
            sendRedirect linkmain
         Just _ -> embeddedErrorPage

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
appHandler :: Kontra Response -> AppConf -> AppGlobals -> ServerPartT IO Response
appHandler handleRoutes appConf appGlobals = do
  startTime <- liftIO getClockTime

  let quota :: GHC.Int.Int64 = 10000000

  temp <- liftIO $ getTemporaryDirectory
  decodeBody (defaultBodyPolicy temp quota quota quota)

  rq <- askRq

  session <- handleSession
  ctx <- createContext rq session
  response <- handle rq session ctx
  finishTime <- liftIO getClockTime
  let TOD ss sp = startTime
      TOD fs fp = finishTime
      _diff = (fs - ss) * 1000000000000 + fp - sp
  --Log.debug $ "Response time " ++ show (diff `div` 1000000000) ++ "ms"
  return response
  where
    handle :: Request -> Session -> Context -> ServerPartT IO Response
    handle rq session ctx = do
      (res,ctx') <- toIO ctx . runKontra $
         do
          res <- handleRoutes  `mplus` do
             rqcontent <- liftIO $ tryTakeMVar (rqInputsBody rq)
             when (isJust rqcontent) $
                 liftIO $ putMVar (rqInputsBody rq) (fromJust rqcontent)
             Log.error $ showRequest rq rqcontent
             response <- handleError
             setRsCode 404 response
          ctx' <- getContext
          return (res,ctx')

      let newsessionuser = fmap userid $ ctxmaybeuser ctx'
      let newflashmessages = ctxflashmessages ctx'
      let newelegtrans = ctxelegtransactions ctx'
      F.updateFlashCookie (aesConfig appConf) (ctxflashmessages ctx) newflashmessages
      updateSessionWithContextData session newsessionuser newelegtrans
      liftIO $ disconnect $ ctxdbconn ctx'
      return res

    createContext rq session = do
      hostpart <- getHostpart
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
                     SockAddrInet _ hostip -> hostip
                     _ -> 0

      conn <- liftIO $ connectPostgreSQL $ dbConfig appConf
      minutestime <- liftIO getMinutesTime
      muser <- getUserFromSession conn session
      mcompany <- getCompanyFromSession conn session
      location <- getLocationFromSession session
      mservice <- ioRunDB conn . dbQuery . GetServiceByLocation . toServiceLocation =<< currentLink
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
      doclocale <- getDocumentLocale conn
      userlocale <- getUserLocale conn muser

      let elegtrans = getELegTransactions session
          ctx = Context
                { ctxmaybeuser = muser
                , ctxhostpart = hostpart
                , ctxflashmessages = flashmessages
                , ctxtime = minutestime
                , ctxnormalizeddocuments = docscache appGlobals
                , ctxipnumber = peerip
                , ctxdbconn = conn
                , ctxdocstore = docstore appConf
                , ctxs3action = defaultAWSAction appConf
                , ctxgscmd = gsCmd appConf
                , ctxproduction = production appConf
                , ctxbackdooropen = isBackdoorOpen $ mailsConfig appConf
                , ctxtemplates = localizedVersion userlocale templates2
                , ctxglobaltemplates = templates2
                , ctxlocale = userlocale
                , ctxlocaleswitch = isNothing $ doclocale
                , ctxesenforcer = esenforcer appGlobals
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
                }
      return ctx

{- |
   Handles viewing of the signup page
-}
_signupPageGet :: Kontra Response
_signupPageGet = do
    ctx <- getContext
    content <- liftIO (signupPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja  content


_signupVipPageGet :: Kontra Response
_signupVipPageGet = do
    ctx <- getContext
    content <- liftIO (signupVipPageView $ ctxtemplates ctx)
    V.renderFromBody V.TopNone V.kontrakcja content
{- |
   Handles submission of the signup form.
   Normally this would create the user, (in the process mailing them an activation link),
   but if the user already exists, we check to see if they have accepted the tos.  If they haven't,
   then we send them a new activation link because probably the old one expired or was lost.
   If they have then we stop the signup.
-}
signupPagePost :: Kontrakcja m => m KontraLink
signupPagePost = do
    Context { ctxtime } <- getContext
    signup False $ Just ((60 * 24 * 31) `minutesAfter` ctxtime)

{-
    A comment next to LoopBack says never to use it. Is this function broken?
-}
signup :: Kontrakcja m => Bool -> Maybe MinutesTime -> m KontraLink
signup vip _freetill =  do
  memail <- getOptionalField asValidEmail "email"
  case memail of
    Nothing -> return LoopBack
    Just email -> do
      muser <- runDBQuery $ GetUserByEmail Nothing $ Email $ email
      case (muser, muser >>= userhasacceptedtermsofservice) of
        (Just user, Nothing) -> do
          -- there is an existing user that hasn't been activated
          -- send them another invite
          UserControl.sendNewUserMail vip user
        (Nothing, Nothing) -> do
          -- this email address is new to the system, so create the user
          -- and send an invite
          mnewuser <- UserControl.createUser (Email email) BS.empty BS.empty Nothing
          maybe (return ()) (UserControl.sendNewUserMail vip) mnewuser
        (_, _) -> return ()
      -- whatever happens we want the same outcome, we just claim we sent the activation link,
      -- because we don't want any security problems with user information leaks
      addFlashM $ modalUserSignupDone (Email email)
      return LoopBack

{- |
   Sends a new activation link mail, which is really just a new user mail.
-}
_sendNewActivationLinkMail:: Context -> User -> Kontra ()
_sendNewActivationLinkMail Context{ctxhostpart, ctxesenforcer} user = do
    let email = getEmail user
    al <- newAccountCreatedLink user
    mail <- newUserMail ctxhostpart email email al False
    scheduleEmailSendout ctxesenforcer $ mail { to = [MailAddress {fullname = email, email = email}] }

{- |
   Handles submission of a login form.  On failure will redirect back to referer, if there is one.
-}
handleLoginPost :: Kontrakcja m => m KontraLink
handleLoginPost = do
    ctx <- getContext
    memail  <- getOptionalField asDirtyEmail    "email"
    mpasswd <- getOptionalField asDirtyPassword "password"
    let linkemail = maybe "" BS.toString memail
    case (memail, mpasswd) of
        (Just email, Just passwd) -> do
            -- check the user things here
            maybeuser <- runDBQuery $ GetUserByEmail Nothing (Email email)
            case maybeuser of
                Just user@User{userpassword}
                    | verifyPassword userpassword passwd -> do
                        Log.debug $ "User " ++ show email ++ " logged in"
                        _ <- runDBUpdate $ SetUserSettings (userid user) $ (usersettings user) {
                          locale = ctxlocale ctx
                        }
                        muuser <- runDBQuery $ GetUserByID (userid user)
                        logUserToContext muuser
                        return BackToReferer
                Just _ -> do
                        Log.debug $ "User " ++ show email ++ " login failed (invalid password)"
                        return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail
                Nothing -> do
                    Log.debug $ "User " ++ show email ++ " login failed (user not found)"
                    return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail
        _ -> return $ LinkLogin (ctxlocale ctx) $ InvalidLoginInfo linkemail

