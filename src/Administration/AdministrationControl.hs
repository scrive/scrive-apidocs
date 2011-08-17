{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Administration.AdministrationControl
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Handlers for all administrations tasks
--
-----------------------------------------------------------------------------
module Administration.AdministrationControl(
            showAdminMainPage
          , showAdminUserAdvanced
          , showAdminUsers
          , showAdminUsersForSales
          , showAdminUsersForPayments
          , showAdminUserUsageStats
          , showAllUsersTable
          , showStats
          , showServicesPage
          , showAdminTranslations
          , indexDB
          , getUsersDetailsToCSV
          , handleUserChange
          , handleDatabaseCleanup
          , handleCreateUser
          , handleCreateService
          , handleStatistics
          , handleFixForBug510
          , resealFile
          ) where
import Control.Monad.State
import Data.Functor
import AppView
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (query)
import Misc
import Kontra
import Administration.AdministrationView
import Doc.DocControl (postDocumentChangeAction) -- required for 510 bug fix migration
import Happstack.State (update) -- required for 510 bug fix migration
import Doc.DocState
import Doc.DocProcess
import Data.ByteString.UTF8 (fromString,toString)
import Data.ByteString (ByteString, hGetContents)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as L
import Company.Model
import KontraLink
import MinutesTime
import System.Directory
import DB.Classes
import User.UserControl
import User.UserView
import User.Model
import Data.Maybe
import System.Process
import System.IO (hClose)
import Data.Char
import Happstack.Util.Common
import API.Service.Model
import Data.Monoid
import qualified Data.IntMap as IntMap
import Templates.Templates
import Text.Printf
import Util.FlashUtil
import Data.List
import Templates.TextTemplates
import Util.MonadUtils
import qualified AppLogger as Log
import Doc.DocSeal (sealDocument)
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Redirect

{- | Main page. Redirects users to other admin panels -}
showAdminMainPage :: Kontrakcja m => m Response
showAdminMainPage = onlySuperUser $ do
  content <- adminMainPage
  renderFromBody TopEmpty kontrakcja content

{- | Process view for advanced user administration -}
showAdminUserAdvanced :: Kontrakcja m => m Response
showAdminUserAdvanced = onlySuperUser $ do
  users <- runDBQuery GetUsers
  mcompanies <- mapM getCompanyForUser users
  params <- getAdminUsersPageParams
  content <- adminUsersAdvancedPage (zip users mcompanies) params
  renderFromBody TopEmpty kontrakcja content

{- | Process view for finding a user in basic administration. If provided with userId string as param
it allows to edit user details -}
showAdminUsers :: Kontrakcja m => Maybe UserID -> m Response
showAdminUsers Nothing = onlySuperUser $ do
  users <- getUsersAndStats
  params <- getAdminUsersPageParams
  content <- adminUsersPage users params
  renderFromBody TopEmpty kontrakcja content

showAdminUsers (Just userId) = onlySuperUser $ do
  muser <- runDBQuery $ GetUserByID userId
  case muser of
    Nothing -> mzero
    Just user -> do
      mcompany <- getCompanyForUser user
      content <- adminUserPage user mcompany
      renderFromBody TopEmpty kontrakcja content

showAdminUsersForSales :: Kontrakcja m => m Response
showAdminUsersForSales = onlySuperUser $ do
  users <- getUsersAndStats
  params <- getAdminUsersPageParams
  content <- adminUsersPageForSales users params
  renderFromBody TopEmpty kontrakcja content

showAdminUsersForPayments :: Kontrakcja m => m Response
showAdminUsersForPayments = onlySuperUser $ do
  users <- getUsersAndStats
  params <- getAdminUsersPageParams
  content <- adminUsersPageForPayments users params
  renderFromBody TopEmpty kontrakcja content

getUsersAndStats :: Kontrakcja m => m [(User,Maybe Company, DocStats)]
getUsersAndStats = do
    Context{ctxtime} <- getContext
    users <- runDBQuery GetUsers
    let queryStats user = do
          mcompany <- getCompanyForUser user
          docstats <- query $ GetDocumentStatsByUser user ctxtime
          return (user, mcompany, docstats)
    users2 <- mapM queryStats users
    return users2

showAdminUserUsageStats :: Kontrakcja m => UserID -> m Response
showAdminUserUsageStats userid = onlySuperUser $ do
  documents <- query $ GetDocumentsByAuthor userid
  Just user <- runDBQuery $ GetUserByID userid
  mcompany <- getCompanyForUser user
  content <- adminUserUsageStatsPage user mcompany $ do
    fieldsFromStats [user] documents
  renderFromBody TopEmpty kontrakcja content


{- Shows table of all users-}
showAllUsersTable :: Kontrakcja m => m Response
showAllUsersTable = onlySuperUser $ do
    users <- getUsersAndStats
    content <- allUsersTable users
    renderFromBody TopEmpty kontrakcja content


#ifndef WINDOWS
read_df :: IO ByteString
read_df = do
  (_,Just handle_out,_,handle_process) <-
      createProcess (proc "df" []) { std_out = CreatePipe, env = Just [("LANG","C")] }
  s <- hGetContents handle_out
  hClose handle_out
  _ <- waitForProcess handle_process
  return s
#endif


showStats :: Kontrakcja m => m Response
showStats = onlySuperUser $ do
    docstats <- query GetDocumentStats
#ifndef WINDOWS
    df <- liftIO read_df
#else
    let df = empty
#endif
    let stats = StatsView { svDoccount = doccount docstats,
                            svSignaturecount = signaturecount docstats }
    content <- statsPage stats $ toString df
    renderFromBody TopEmpty kontrakcja content

indexDB :: Kontrakcja m => m Response
indexDB = onlySuperUser $ do
    files <- liftIO $ getDirectoryContents "_local/kontrakcja_state"
    content <- databaseContent $ sort files
    renderFromBody TopEmpty kontrakcja content

getUsersDetailsToCSV :: Kontrakcja m => m Response
getUsersDetailsToCSV = onlySuperUser $ do
      x <- runDBQuery ExportUsersDetailsToCSV
      let response = toResponseBS (fromString "text/csv") (L.fromChunks [x])
      return response

{- | Handling user details change. It reads user info change -}
handleUserChange :: Kontrakcja m => UserID -> m KontraLink
handleUserChange uid = onlySuperUser $ do
  _ <- getAsStrictBS "change"
  mupgradetocompany  <- getFieldUTF "upgradetocompany"
  olduser <- runDBOrFail $ dbQuery $ GetUserByID uid
  -- if the "upgrade to company" box was checked, and this is
  -- a single user then create a new company, and make this person
  -- the admin of it
  user <- case (mupgradetocompany, usercompany olduser) of
    (Just upgradeval, Nothing) | (toString upgradeval) == "on" -> do
      runDBOrFail $ do
        company <- dbUpdate $ CreateCompany Nothing Nothing
        _ <- dbUpdate $ SetUserCompany uid (companyid company)
        _ <- dbUpdate $ MakeUserCompanyAdmin uid
        dbQuery $ GetUserByID uid
    _ -> return olduser
  infoChange <- getUserInfoChange
  _ <- runDBUpdate $ SetUserInfo uid $ infoChange $ userinfo user
  companyInfoChange <- getCompanyInfoChange
  getCompanyForUser user >>= maybe (return ()) (\c -> do
    _ <- runDBUpdate $ SetCompanyInfo (companyid c) (companyInfoChange $ companyinfo c)
    return ())
  return $ LinkUserAdmin $ Just uid

{-| Cleaning the database -}
handleDatabaseCleanup :: Kontrakcja m => m KontraLink
handleDatabaseCleanup = onlySuperUser $  do
    -- dangerous, cleanup all old files, where old means chechpoints but the last one
    -- and all events that have numbers less than last checkpoint
    _ <- liftIO databaseCleanupWorker
    return LinkAdminOnlyIndexDB

databaseCleanupWorker :: IO [FilePath]
databaseCleanupWorker = do
  contents <- getDirectoryContents "_local/kontrakcja_state"
  let checkpoints = filter ("checkpoints-" `isPrefixOf`) contents
  let events = filter ("events-" `isPrefixOf`) contents
  let lastcheckpoint = last (sort checkpoints)
  let cutoffevent = "events-" ++ drop 12 lastcheckpoint
  let eventsToRemove = filter (< cutoffevent) events
  let checkpointsToRemove = filter (< lastcheckpoint) checkpoints
  mapM_ (\x -> removeFile ("_local/kontrakcja_state/" ++ x)) (eventsToRemove ++ checkpointsToRemove)
  getDirectoryContents "_local/kontrakcja_state" --This can be dropped


handleCreateUser :: Kontrakcja m => m KontraLink
handleCreateUser = onlySuperUser $ do
    ctx <- getContext
    email' <- getAsStrictBS "email"
    let email = BSC.map toLower email'
    fstname <- getAsStrictBS "fstname"
    sndname <- getAsStrictBS "sndname"
    custommessage <- getField "custommessage"
    freetill <- fmap (join . (fmap parseMinutesTimeDMY)) $ getField "freetill"
    muser <- createNewUserByAdmin ctx (fstname, sndname) email freetill custommessage
    when (isNothing muser) $
        addFlashM flashMessageUserWithSameEmailExists

    -- FIXME: where to redirect?
    return LinkStats
    
{- | Reads params and returns function for conversion of company info.  With no param leaves fields unchanged -}
getCompanyInfoChange :: Kontrakcja m => m (CompanyInfo -> CompanyInfo)
getCompanyInfoChange = do
  mcompanyname    <- getFieldUTF "companyname"
  mcompanynumber  <- getFieldUTF "companynumber"
  mcompanyaddress <- getFieldUTF "companyaddress"
  mcompanyzip     <- getFieldUTF "companyzip"
  mcompanycity    <- getFieldUTF "companycity"
  mcompanycountry <- getFieldUTF "companycountry"
  return $ \CompanyInfo {
      companyname
    , companynumber
    , companyaddress
    , companyzip
    , companycity
    , companycountry
    } ->  CompanyInfo {
        companyname =  fromMaybe companyname mcompanyname
      , companynumber  =  fromMaybe companynumber mcompanynumber
      , companyaddress =  fromMaybe companyaddress mcompanyaddress
      , companyzip = fromMaybe companyzip mcompanyzip
      , companycity  = fromMaybe companycity mcompanycity
      , companycountry = fromMaybe companycountry mcompanycountry
    }

{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}
getUserInfoChange :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoChange = do
  muserfstname         <- getFieldUTF "userfstname"
  musersndname         <- getFieldUTF "usersndname"
  muserpersonalnumber  <- getFieldUTF "userpersonalnumber"
  musercompanyposition <- getFieldUTF "usercompanyposition"
  muserphone           <- getFieldUTF "userphone"
  musermobile          <- getFieldUTF "usermobile"
  museremail           <- fmap Email <$> getFieldUTF "useremail"
  return $ \UserInfo {
      userfstname
    , usersndname
    , userpersonalnumber
    , usercompanyposition
    , userphone
    , usermobile
    , useremail
    } ->  UserInfo {
      userfstname = fromMaybe userfstname muserfstname
      , usersndname = fromMaybe usersndname musersndname
      , userpersonalnumber = fromMaybe userpersonalnumber muserpersonalnumber
      , usercompanyposition = fromMaybe usercompanyposition musercompanyposition
      , userphone = fromMaybe userphone muserphone
      , usermobile = fromMaybe usermobile musermobile
      , useremail =  fromMaybe useremail museremail
    }

{- | Reads params and returns structured params for user managment pages. -}
getAdminUsersPageParams :: Kontrakcja m => m AdminUsersPageParams
getAdminUsersPageParams = do
  search <- getDataFn' (look "search")
  startletter <-  getDataFn' (look "startletter")
  mpage <-  getDataFn' (look "page")
  let (mpage'::Maybe Int) = join $ fmap readM mpage
  return $ AdminUsersPageParams {search = search, startletter=startletter, page = maybe 0 id mpage'}


{- Create service-}
handleCreateService :: Kontrakcja m => m KontraLink
handleCreateService = onlySuperUser $ do
    mname<- getFieldUTF "name"
    madmin <- liftMM  (runDBQuery . GetUserByEmail Nothing . Email) (getFieldUTF "admin")
    case (mname,madmin) of
         (Just name,Just admin) -> do
            pwdBS <- getFieldUTFWithDefault mempty "password"
            pwd <- liftIO $ createPassword pwdBS
            mservice <- runDBUpdate $ CreateService (ServiceID name) (Just pwd) (userid admin)
            _ <- case mservice of
                Just srvs -> do
                    location <- getFieldUTF "location"
                    runDBUpdate $ UpdateServiceSettings (serviceid srvs) (servicesettings srvs)
                                    {servicelocation = ServiceLocation <$> location}
                _ -> mzero
            return LoopBack
         _ -> mzero

{- Services page-}
showServicesPage :: Kontrakcja m => m Response
showServicesPage = onlySuperUser $ do
  conn <- getConnection
  services <- runDBQuery GetServices
  content <- servicesAdminPage conn services
  renderFromBody TopEmpty kontrakcja content


{-
Sales leads stats:

User name
User email
Total finalized docs (total signatures)
Sales rep (editable, free text)
Status (1-5) (editable, free text)
Subaccounts
User company
User title
User phone
Date TOS accepted
Subacc (y/n)/Superaccount
-}

{-
Billing stats:

Superuser Company name
Superuser email
Payment plan
Next billing date
Last billing date
Last billing total fee
Current plan price
Current per signature price
Current TW storage price
-}

{-

Nr of Users
Total nr of signatures	*
Total nr of signatures of finished docs (these are the ones we charge for)	*
Nr of docs with cross status	*
Nr of docs with blue status	*
Nr of docs with green status	*
Nr of docs yellow status	*
Nr of docs with orange status	*
Nr of docs with red status	*
Nr of docs with red exclamation mark status	*
Nr of friend invites	*
Nr of SkrivaPå staff invites *
Nr of Signups after finalized offer	TODO
Nr of Signups after finalized contract  TODO
-}

{-
Total nr of signatures
Total nr of signatures of finished docs (these are the ones we charge for)
Nr of docs with cross status
Nr of docs with blue status
Nr of docs with green status
Nr of docs with yellow status
Nr of docs with orange status
Nr of docs with red status
Nr of docs with red exclamation mark status
Nr of friend invites
-}


{-
User list:

Email
Name
Title
Company
Phone
Sales rep
Used signatures total
Used signatures last 1 month
Used signatures last 2 months
Used signatures last 3 months
Used signatures last 6 months
Used signatures last 12 months
-}

data DocStatsL = DocStatsL
                { dsAllDocuments :: !Int
                , dsPreparationDocuments :: !Int
                , dsPendingDocuments :: !Int
                , dsCanceledDocuments :: !Int
                , dsTimedOutDocuments :: !Int
                , dsClosedDocuments :: !Int
                , dsRejectedDocuments :: !Int
                , dsAwaitingAuthorDocuments :: !Int
                , dsErrorDocuments :: !Int
                , dsAllSignatures :: !Int
                , dsSignaturesInClosed :: !Int

                , dsAllUsers :: !Int
                , dsViralInvites :: !Int
                , dsAdminInvites :: !Int
                }

docStatsZero :: DocStatsL
docStatsZero = DocStatsL 0 0 0 0 0 0 0 0 0 0 0 0 0 0

addStats :: DocStatsL -> DocStatsL -> DocStatsL
addStats (DocStatsL a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14) (DocStatsL b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14) =
      DocStatsL (a1+b1) (a2+b2) (a3+b3) (a4+b4) (a5+b5) (a6+b6) (a7+b7) (a8+b8) (a9+b9) (a10+b10) (a11+b11) (a12+b12) (a13+b13) (a14+b14)

countSignatures :: Document -> Int
countSignatures = length . filter (isJust . maybesigninfo) . documentsignatorylinks

calculateStatsFromDocuments :: [Document] -> IntMap.IntMap DocStatsL
calculateStatsFromDocuments documents =
  foldl' ins IntMap.empty documents
  where
    ins mapfunc doc = foldl' (\m (k,v) -> IntMap.insertWith addStats k v m) mapfunc (stuff doc)
    stuff doc = [ (asInt $ documentctime doc, docStatsZero { dsAllDocuments = 1
                                                           , dsAllSignatures = countSignatures doc
                                                           , dsSignaturesInClosed = if documentstatus doc == Closed
                                                                                    then countSignatures doc
                                                                                    else 0
                                                           })
                , (asInt $ documentmtime doc, case documentstatus doc of
                      Preparation -> docStatsZero { dsPreparationDocuments = 1}
                      Pending     -> docStatsZero { dsPendingDocuments = 1}
                      Rejected    -> docStatsZero { dsRejectedDocuments = 1}
                      Canceled    -> docStatsZero { dsCanceledDocuments = 1}
                      DocumentError {}    -> docStatsZero { dsErrorDocuments = 1}
                      Closed      -> docStatsZero { dsClosedDocuments = 1}
                      Timedout    -> docStatsZero { dsTimedOutDocuments = 1}
                      AwaitingAuthor -> docStatsZero {dsAwaitingAuthorDocuments = 1}
                      --_ -> docStatsZero  -- catch all to make it run in case somebody adds new status
                      )
                ]

fieldsFromStats :: (Functor m, MonadIO m) => [User] -> [Document] -> Fields m
fieldsFromStats _users documents = do
    let userStats = IntMap.empty -- calculateStatsFromUsers users
        documentStats = calculateStatsFromDocuments documents
        showAsDate :: Int -> String
        showAsDate int = printf "%04d-%02d-%02d" (int `div` 10000) (int `div` 100 `mod` 100) (int `mod` 100)
        showAsMonth :: Int -> String
        showAsMonth int = printf "%04d-%02d" (int `div` 10000) (int `div` 100 `mod` 100)
        stats' = IntMap.toList (IntMap.unionWith addStats userStats documentStats)
        lastMonthStats = take 30 (reverse stats')
        allMonthsStats = reverse $ IntMap.toList $ IntMap.fromListWith addStats (map ( \(k,v) -> (k `div` 100 * 100, v)) stats')
    let fieldify showDate (date,stat) = do
          field "date" $ showDate date
          fieldF "documents" $ do
            field "all" $ dsAllDocuments stat
            field "preparation" $ dsPreparationDocuments stat
            field "pending" $ dsPendingDocuments stat
            field "error" $ dsErrorDocuments stat
            field "timeout" $ dsTimedOutDocuments stat
            field "awaitingauthor" $ dsAwaitingAuthorDocuments stat
            field "closed" $ dsClosedDocuments stat
            field "rejected" $ dsRejectedDocuments stat
            field "canceled" $ dsCanceledDocuments stat
            field "signatures" $ dsAllSignatures stat
            field "signaturesInClosed" $ dsSignaturesInClosed stat
          fieldF "users" $ do
            field "all" $ dsAllUsers stat
            field "viralInvites" $ dsViralInvites stat
            field "adminInvites" $ dsAdminInvites stat

    fieldFL "lastMonthStats" $ map (fieldify showAsDate) lastMonthStats
    fieldFL "allMonthsStats" $ map (fieldify showAsMonth) allMonthsStats

handleStatistics :: Kontrakcja m => m Response
handleStatistics =
  onlySuperUser $ do
    ctx <- getContext
    documents <- query $ GetDocuments $ currentServiceID ctx
    users <- runDBQuery GetUsers
    content <- renderTemplateFM "statisticsPage" $ do
      fieldsFromStats users documents
    renderFromBody TopEmpty kontrakcja content

showAdminTranslations :: Kontrakcja m => m String
showAdminTranslations = do
    liftIO $ updateCSV
    adminTranslationsPage
    
{- |
    This handles fixing of documents broken by bug 510, which means
    that the authors were mistakenly made signatories of offers or orders.
-}
handleFixForBug510 :: Kontrakcja m => m Response
handleFixForBug510 = onlySuperUser $ do
  services <- runDBQuery $ GetServices
  mapM_ fixForService $ Nothing : map (Just . serviceid) services
  sendRedirect LinkMain
  where
    fixForService :: Kontrakcja m => Maybe ServiceID -> m ()
    fixForService service = do
      docs <- query $ GetDocuments service
      mapM_ maybeFixForDocument docs
      return ()
    maybeFixForDocument :: Kontrakcja m => Document -> m ()
    maybeFixForDocument doc =
      let isauthorsendonly = Just True == getValueForProcess doc processauthorsend
          isauthorsigning = maybe False (elem SignatoryPartner . signatoryroles) (getAuthorSigLink doc)
          nonauthorpartisgood = nonAuthorPartLooksGood doc
          hasauthorsigned = maybe False (isJust . maybesigninfo) (getAuthorSigLink doc) in
      case (isauthorsendonly && isauthorsigning,
            nonauthorpartisgood,
            hasauthorsigned) of
        (True, False, _) -> Log.debug $ mkMsg doc
          "broken, but because of counterparts looks like it was broken by something else, leaving for now"
        (True, _, True) -> Log.debug $ mkMsg doc
          "broken, but the author has already signed, so unsure how to fix, leaving for now"
        (True, True, False) -> do
          Log.debug $ mkMsg doc "fixing"
          udoc <- guardRightM . update $ FixBug510ForDocument (documentid doc)
          postDocumentChangeAction udoc doc Nothing
        _ -> return ()
    nonAuthorPartLooksGood :: Document -> Bool
    nonAuthorPartLooksGood doc =
      let nonauthorparts = filter (not . isAuthor) $ documentsignatorylinks doc
      in case nonauthorparts of
        (nonauthorpart:[]) | SignatoryPartner `elem` (signatoryroles nonauthorpart) -> True
        _ -> False
    mkMsg :: Document -> String -> String
    mkMsg doc msg = "Handling 510 bug fix for " ++
                    " doc " ++ (show $ documentid doc) ++
                    " with type " ++ (show $ documenttype doc) ++
                    " and author " ++ (show . getEmail . fromJust $ getAuthorSigLink doc) ++ " : " ++ msg

-- This method can be used do reseal a document 
resealFile :: Kontrakcja m => DocumentID -> m KontraLink
resealFile docid = onlySuperUser $ do
  Log.debug $ "Trying to reseal document "++ show docid ++" | Only superadmin can do that"
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing -> mzero
    Just doc -> case (documentfiles doc,documentsealedfiles doc, documentstatus doc) of  
                     ((_:_),[], Closed) -> do
                         ctx <- getContext
                         Log.debug "Document is valid for resealing sealing"
                         res <- sealDocument ctx doc
                         case res of
                           Left  _ -> Log.debug "We failed to reseal the document"
                           Right _ -> Log.debug "Ok, so the document has been resealed"
                         return LoopBack
                     _ -> do
                         Log.debug "Document is not valid for resealing sealing"
                         mzero

