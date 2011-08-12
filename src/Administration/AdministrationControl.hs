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
          --, handleUserEnableTrustWeaverStorage
          , handleCreateService
          , handleStatistics
          --, migrateSigAccounts
          --, migrateCompanies
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
import Doc.DocState
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

{-eitherFlash :: Kontrakcja m => m (Either String b) -> m b
eitherFlash action = do
  x <- action
  case x of
    Left errmsg -> do
           addFlash (OperationFailed, errmsg)
           mzero
    Right value -> return value
-}

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
      --paymentmodel <- query $ GetPaymentModel $ paymentaccounttype $ userpaymentpolicy user
      mcompany <- getCompanyForUser user
      content <- adminUserPage user mcompany -- paymentmodel
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



{- | Handling user details change. It reads user info change, user settings change , paymentpolicy and payment account change -}
handleUserChange :: Kontrakcja m => String -> m KontraLink
handleUserChange a = onlySuperUser $
                     do
                     let (muserId::Maybe UserID) = readM a
                     _ <- getAsStrictBS "change"
                     case muserId of
                       Nothing -> mzero
                       Just userId ->
                        do
                          muser <- runDBQuery $ GetUserByID userId
                          case muser of
                             Nothing -> mzero
                             Just user -> do
                                           --Reading changes from params using dedicated functions for each user part
                                           --freetrialexpirationdate <- join . (fmap parseMinutesTimeDMY) <$> getField "freetrialexpirationdate"
                                           infoChange <- getUserInfoChange
                                           companyInfoChange <- getCompanyInfoChange
                                           --settingsChange <- getUserSettingsChange
                                           --paymentAccountChange <- getUserPaymentAccountChange
                                           --paymentPaymentPolicy <- getUserPaymentPolicyChange
                                           --Updating DB , ignoring fails
                                           runDB $ do
                                             --_ <- dbUpdate $ SetFreeTrialExpirationDate userId freetrialexpirationdate
                                             _ <- dbUpdate $ SetUserInfo userId $ infoChange $ userinfo user
                                             --_ <- dbUpdate $ SetUserSettings userId $ settingsChange $ usersettings user
                                             return ()
                                           --_ <- update $ SetUserPaymentAccount userId $ paymentAccountChange $ userpaymentaccount user
                                           --_ <- update $ SetUserPaymentPolicyChange userId $ paymentPaymentPolicy $ userpaymentpolicy user
                                           mcompany <- getCompanyForUser user
                                           case mcompany of
                                             Just company -> do
                                               _ <- runDBUpdate $ SetCompanyInfo (companyid company) (companyInfoChange $ companyinfo company)
                                               return ()
                                             Nothing -> do
                                               return ()
                                           return $ LinkUserAdmin $ Just userId

{-
handleUserEnableTrustWeaverStorage :: Kontrakcja m => String -> m KontraLink
handleUserEnableTrustWeaverStorage a =
    onlySuperUser $
                  do
                    let (muserId::Maybe UserID) = readM a
                    _ <- getAsStrictBS "enabletrustweaver"
                    case muserId of
                       Nothing -> mzero
                       Just userId ->
                        do
                          muser <- query $ GetUserByID userId
                          case muser of
                             Nothing -> mzero
                             Just user ->
                                     case signeddocstorage (usersettings user) of
                                       Just _ -> do
                                         -- FIXME: add text: was already enabled
                                         return $ LinkUserAdmin $ Just userId
                                       Nothing -> (do
                                         let name = show userId
                                         Context{ctxtwconf} <- getContext
                                         -- FIXME: error handling here
                                         (superAdminUsername, superAdminPwd, sectionPath) <-
                                             eitherFlash $ liftIO $ TW.registerAndEnableSection ctxtwconf name
                                         let newsettings = (usersettings user)
                                                           { signeddocstorage =
                                                                 Just (TrustWeaverStorage
                                                                       { storagetwenabled = True
                                                                       , storagetwname = fromString name
                                                                       , storagetwsuperadmin = fromString superAdminUsername
                                                                       , storagetwsuperadminpwd = fromString superAdminPwd
                                                                       , storagetwsectionpath = fromString sectionPath
                                                                       })
                                                           }

                                         _ <- update $ SetUserSettings userId newsettings

                                         return $ LinkUserAdmin $ Just userId)
                                            `mplus` (return $ LinkUserAdmin $ Just userId)
-}
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
                     mcompanyaddress        <- getFieldUTF "companyaddress"
                     mcompanyzip            <- getFieldUTF "companyzip"
                     mcompanycity           <- getFieldUTF "companycity"
                     mcompanycountry        <- getFieldUTF "companycountry"
                     return (\CompanyInfo {
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
                                        })

{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}
getUserInfoChange :: Kontrakcja m => m (UserInfo -> UserInfo)
getUserInfoChange = do
                     muserfstname        <- getFieldUTF "userfstname"
                     musersndname        <- getFieldUTF "usersndname"
                     muserpersonalnumber <- getFieldUTF "userpersonalnumber"
                     musercompanyposition    <- getFieldUTF "usercompanyposition"
                     muserphone          <- getFieldUTF "userphone"
                     musermobile         <- getFieldUTF "usermobile"
                     museremail          <- fmap (fmap Email) $ getFieldUTF "useremail"
                     return (\UserInfo {
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
                                        })

{- | Reads params and returns function for conversion of user settings. With no param leaves fields unchanged -}
{-getUserSettingsChange :: Kontrakcja m => m (UserSettings -> UserSettings)
getUserSettingsChange =  do
                          return (\UserSettings {
                                   preferreddesignmode
                                 , lang
                                 , systemserver }
                                       -> UserSettings {
                                            preferreddesignmode = preferreddesignmode
                                          , lang = lang
                                          , systemserver = systemserver
                                          })
-}

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

-- FIXME: make it work
{-
calculateStatsFromUsers :: [User] -> IntMap.IntMap DocStatsL
calculateStatsFromUsers users =
  foldl' ins IntMap.empty users
  where
    ins mapfunc user = foldl' (\m (k,v) -> IntMap.insertWith addStats k v m) mapfunc (stuff user)
    stuff user = catMaybes [ do -- Maybe monad
                                time <- userhasacceptedtermsofservice user
                                return (asInt time, docStatsZero { dsAllUsers = 1})
                           , do
                                info <- userinviteinfo user
                                time <- invitetime info
                                typex <- invitetype info
                                return (asInt time, case typex of
                                                         Viral -> docStatsZero { dsViralInvites = 1 }
                                                         Admin -> docStatsZero { dsAdminInvites = 1 })
                           ]
-}
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
    Migrate companies.  This takes the following steps
    
    * setup things for the companies which are already stored on the users' usercompany field,
      by making the user an admin if they're not a subaccount, and by copying the relevant company info from the user
      onto the company
    * creating new companies for individual users with company name or company number set, or for supervisors.
      the user is made a company admin and the relevant company info is copied from them onto the company
    * link each subaccount to their supervisor's company, by making their companyuser field the same
    * link each signatory link to the relevant company by finding a user which is their maybesignatory or maybesupervisor
      and copying the usercompany information onto the new siglink field maybecompany.
      
    After migration there will be a whole load of fields that are currently deprecated, but we will be able to remove from
    the codebase.
    
    Don't run this more than once!  Because of the setupInfoForExistingCompanies step it'll make all the subaccounts
    into admins if you do.
-}
{-
migrateCompanies :: Kontrakcja m => m Response
migrateCompanies = onlySuperUser $ do
  setupInfoForExistingCompanies --sets things up for companies already stored on usercompany
  createNewCompaniesThatAreRequired --sets up new companies where needed, for example for supervisors
  linkSubaccountsToCompanies --links subaccounts to their supervisor's company
  linkSignatoryLinksToCompanies --links signatorylinks to their supervisor's company
  Log.debug "company migration complete"
  sendRedirect LinkMain
  where-}
    {- |
        Where we have users with a usercompany already
        we want to copy over the company info onto that company.
    -}
    {-setupInfoForExistingCompanies :: Kontrakcja m => m ()
    setupInfoForExistingCompanies = do
      users <- query $ GetAllUsers
      mapM_ maybeSetupInfoForUsersCompany users
      return ()-}
    
    {- |
        If a user has a company and is either a supervisor or a single user
        then we make them an admin, and copy their company info over onto the company.
    -}
    {-maybeSetupInfoForUsersCompany :: Kontrakcja m => User -> m ()
    maybeSetupInfoForUsersCompany user'@User{userid} = do
      mcompany <- getCompanyForUser user'
      case (mcompany, userShouldBeAdmin user') of
        (Just company, True) -> do
--          _ <- guardRightM . update $ MakeUserACompanyAdmin userid  --  don't need this line, user doesn't need to be an admin
          user <- queryOrFail $ GetUserByID userid
          let newcompanyinfo = makeCompanyInfoFromUserInfo user
          newcompany <- guardRightM . update $ SetCompanyInfo company newcompanyinfo
          Log.debug $ "Setup existing company with admin user " ++ (show $ getEmail user) ++ " :"
                        ++ " id " ++ (show $ companyid newcompany)
                        ++ " name " ++ (show $ getCompanyName newcompany)
          return ()  
        _ -> return ()
        -}
    {- |
        Checks to see if the user should be made a company admin.
        To qualify for this they need:
          * to be live
          * have a company
          * be either a single user or a supervisor (same as not being a subaccount)
    -}
    {-userShouldBeAdmin :: User -> Bool
    userShouldBeAdmin user =
      let islive = not $ userdeleted user
          hascompany = isJust $ usercompany user
          issubaccount = isJust $ usersupervisor user
      in islive && hascompany && (not issubaccount)
      -}
    {- |
        This creates any new companies that are required.
    -}
    {-createNewCompaniesThatAreRequired :: Kontrakcja m => m ()
    createNewCompaniesThatAreRequired = do
      users <- query $ GetAllUsers
      mapM_ maybeSetupCompanyForUser users
      return ()
      -}
    {- |
        If the user is a single user with company info, or a supervisor
        then we should setup a company for them, copy their data over,
        and make them the admin of it.
    -}
    {-maybeSetupCompanyForUser :: Kontrakcja m => User -> m ()
    maybeSetupCompanyForUser user@User{userid} = do
      companyrequired <- query $ RequiresCompanyForMigration userid
      when companyrequired (setupCompanyForUser user)
      -}
    {- |
        This sets up a brand new company based on the given
        user, including copying their user info over to be company info.
    -}
    {-setupCompanyForUser :: Kontrakcja m => User -> m ()
    setupCompanyForUser User{userid} = do
      company@Company{companyid} <- update $ CreateNewCompany
      _ <- guardRightM . update $ SetUserCompany userid companyid
      _ <- guardRightM . update $ MakeUserACompanyAdmin userid
      user <- queryOrFail $ GetUserByID userid
      let newcompanyinfo = makeCompanyInfoFromUserInfo user
      newcompany <- guardRightM . update $ SetCompanyInfo company newcompanyinfo
      Log.debug $ "Created new company for " ++ (show $ getEmail user) ++ " :"
                    ++ " id " ++ (show companyid)
                    ++ " name " ++ (show $ getCompanyName newcompany)
      return ()
      
    makeCompanyInfoFromUserInfo user =
      CompanyInfo {
          companyname = usercompanyname $ userinfo user
        , companynumber = usercompanynumber $ userinfo user
        , companyaddress = useraddress $ userinfo user
        , companyzip = userzip $ userinfo user
        , companycity = usercity $ userinfo user
        , companycountry = usercountry $ userinfo user 
      }
      -}
    {- |
        This will go through and set the company on each subaccount
        by looking it up from their supervisor.
    -}
    {-linkSubaccountsToCompanies :: Kontrakcja m => m ()
    linkSubaccountsToCompanies = do
      users <- query $ GetAllUsers
      mapM_ maybeLinkSubaccountToCompany users
      return ()
      -}
    {- |
        If the user is a subaccount then this will link the user
        to their supervisor's company.
    -}
    {-maybeLinkSubaccountToCompany :: Kontrakcja m => User -> m ()
    maybeLinkSubaccountToCompany user@User{userid} =
      case usersupervisor user of
        Just supervisorid -> do
          supervisor <- queryOrFail . GetUserByID . UserID $ unSupervisorID supervisorid
          case usercompany supervisor of
            Nothing -> do
              Log.debug $ "the supervisor " ++ (show supervisorid) ++ " doesn't have a company - something went wrong with the migration!\nsupervisor=" ++ (show supervisor) ++ "\nsubaccount=" ++ (show user)
              mzero
            Just companyid -> do
              _ <- guardRightM . update $ SetUserCompany userid companyid
              Log.debug $ "linked subaccount " ++ (toString $ getEmail user) ++ " to their supervisor " ++ (toString $ getEmail supervisor) ++ "'s company (" ++ (show companyid) ++ ")"
              return ()
        Nothing -> return ()
        -}
    {- |
        This hooks up all the signatory links to the user's companies.
    -}    
    {-linkSignatoryLinksToCompanies :: Kontrakcja m => m ()
    linkSignatoryLinksToCompanies = do
      Log.debug $ "populating companies on the document sig links"
      services <- query $ GetServices
      mapM_ migrateSigLinksForService $ Nothing : map (Just . serviceid) services
      return ()
      where
        migrateSigLinksForService :: Kontrakcja m => Maybe ServiceID -> m ()
        migrateSigLinksForService service = do
          docs <- query $ GetDocuments service
          mapM_ migrateSigLinksForDocument docs
        migrateSigLinksForDocument :: Kontrakcja m => Document -> m ()
        migrateSigLinksForDocument Document{documentid,documentsignatorylinks} = do
          let siguserids = (catMaybes . map maybesignatory $ documentsignatorylinks)
                             ++ (catMaybes . map maybesupervisor $ documentsignatorylinks)
          msigusers <- mapM (query . GetUserByID) siguserids
          _ <- update $ MigrateDocumentSigLinkCompanies documentid (catMaybes msigusers)
          return ()
          -}
{- |
    Piece of migration in response to SKRIVAPADEV-380.  The idea is to populate
    maybesignatory and maybesupervisor on the siglinks.  Want it so that:
    
    * they are correctly populated for every author siglink 
      - this should already be the case, unless an earlier migration went really wrong!
    * they are correctly populated for every non-author siglink
      - they should be populated where a doc is signable or an attachment and not in preparation mode
      - they shouldn't be populated for templates or template attachments or docs in preparation mode
      
    I'm scared that previous migrations may have put this data in the wrong place (so for those in preparation mode).
    Also, we have some documents that certainly need this populating.
    
    From now on we're populate these values whenever a doc is signed or sent, or when a user signs up
    for an account after signing a document.
    
    Some things that'll happen which I think make good sense:
      * people who sign up in the future, and have previous not saved a document won't be able to see that document
      * people who haved signed up in the past can currently see documents that they may have refused to save.  they
        will still be able to see them after migration.
      * At the moment we don't offer an account creation, document saving thing, for those viewing a document rather than
    signing it.  This means that if they subsequently sign up they won't see those documents they viewed in the past.
    
    Because of the niggles above, ideally this migration should just be ran once.  Otherwise people may see documents they
    asked not to save appearing in their archive (although thankfully it's perfectly possible there is no-one like this
    using our service).
-}
{-
migrateSigAccounts :: Kontrakcja m => m Response
migrateSigAccounts = onlySuperUser $ do
  services <- query $ GetServices
  mapM_ migrateSigAccountsForService $ Nothing : map (Just . serviceid) services
  sendRedirect LinkMain
  where
    migrateSigAccountsForService :: Kontrakcja m => Maybe ServiceID -> m ()
    migrateSigAccountsForService service = do
      docs <- query $ GetDocuments service
      mapM_ migrateSigAccountsForDocument docs
      return ()
    migrateSigAccountsForDocument :: Kontrakcja m => Document -> m (Either String Document)
    migrateSigAccountsForDocument Document{documentid,documentservice,documentsignatorylinks} = do
      musers <- mapM (query . GetUserByEmail documentservice . Email . signatoryemail . signatorydetails) documentsignatorylinks
      update $ MigrateDocumentSigAccounts documentid (catMaybes musers) 
-}

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

