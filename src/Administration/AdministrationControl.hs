{-# OPTIONS_GHC -Wall #-}
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
          , showAdminUserUsageStats
          , showAllUsersTable
          , showStats
          , showServicesPage
          , indexDB
          , getUsersDetailsToCSV
          , handleUserChange
          , handleDatabaseCleanup
          , handleCreateUser
          , handleUserEnableTrustWeaverStorage
          , handleMigrate0
          , handleCreateService
          , handleStatistics
          ) where
import Control.Monad.State
import Data.Functor
import AppView
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update,query)
import Misc
import Kontra
import HSP (cdata)
import Administration.AdministrationView
import Payments.PaymentsState
import Doc.DocState
import Data.ByteString.UTF8 (fromString,toString)
import Data.ByteString (ByteString,empty, hGetContents)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as L
import KontraLink
import Payments.PaymentsControl(readMoneyField,getPaymentChangeChange)
import MinutesTime
import FlashMessage
import System.Directory
import Data.List (isPrefixOf,sort, foldl')
import User.UserControl
import User.UserView
import Data.Maybe
import Redirect
import System.Process
import System.IO (hClose)
import qualified TrustWeaver as TW
import Data.Char
import Happstack.Util.Common
import API.Service.ServiceState
import Data.Monoid
import qualified Data.IntMap as IntMap
import Templates.Templates

eitherFlash :: ServerPartT (StateT Context IO) (Either String b)
            -> ServerPartT (StateT Context IO) b
eitherFlash action = do
  x <- action
  case x of
    Left errmsg -> do
           addFlashMsg $ toFlashMsg OperationFailed errmsg
           mzero
    Right value -> return value


{- | Main page. Redirects users to other admin panels -} 
showAdminMainPage :: Kontra Response
showAdminMainPage = onlySuperUser $ do
  ctx@Context {ctxtemplates} <- lift get
  content <- liftIO $ adminMainPage ctxtemplates 
  renderFromBody TopEmpty kontrakcja $ cdata content 

{- | Process view for advanced user administration -}                    
showAdminUserAdvanced :: Kontra Response
showAdminUserAdvanced = onlySuperUser $ do
  ctx@Context {ctxtemplates} <- lift get
  users <- query $ GetAllUsers
  params <- getAdminUsersPageParams
  content <- liftIO $ adminUsersAdvancedPage ctxtemplates users params
  renderFromBody TopEmpty kontrakcja $ cdata content 

{- | Process view for finding a user in basic administration. If provided with userId string as param 
it allows to edit user details -}     
showAdminUsers :: Maybe UserID -> Kontra Response 
showAdminUsers Nothing = onlySuperUser $ do
  ctx@Context {ctxtemplates} <- lift get
  users <- getUsersAndStats
  params <- getAdminUsersPageParams
  content <- liftIO $ adminUsersPage ctxtemplates users params
  renderFromBody TopEmpty kontrakcja $ cdata content 

showAdminUsers (Just userId) = onlySuperUser $ do 
  ctx@Context {ctxtemplates} <- lift get
  muser <- query $ GetUserByUserID userId
  case muser of 
    Nothing -> mzero     
    Just user -> do   
      paymentmodel <- update $ GetPaymentModel $ paymentaccounttype $ userpaymentpolicy user
      content <- liftIO $ adminUserPage ctxtemplates user paymentmodel
      renderFromBody TopEmpty kontrakcja $ cdata content 

getUsersAndStats :: Kontra [(User,DocStats,UserStats)]
getUsersAndStats = do
    Context{ctxtime} <- get
    users <- query $ GetAllUsers
    let queryStats user = do
          docstats <- query $ GetDocumentStatsByUser user ctxtime
          userstats <- query $ GetUserStatsByUser user
          return (user, docstats, userstats)
    users2 <- mapM queryStats users
    return users2

showAdminUserUsageStats :: UserID -> Kontra Response
showAdminUserUsageStats userid = onlySuperUser $ do
  ctx@Context {ctxtemplates} <- get
  documents <- query $ GetDocumentsByAuthor userid
  Just user <- query $ GetUserByUserID userid
  content <- liftIO $ adminUserUsageStatsPage ctxtemplates user $ do
    fieldsFromStats [user] documents
  renderFromBody TopEmpty kontrakcja $ cdata content 
  
  
{- Shows table of all users-}
showAllUsersTable :: Kontra Response
showAllUsersTable = onlySuperUser $ do
    ctx@Context {ctxtemplates} <- lift get
    users <- getUsersAndStats
    content <- liftIO $ allUsersTable ctxtemplates users
    renderFromBody TopEmpty kontrakcja $ cdata  content



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


showStats :: Kontra Response
showStats = onlySuperUser $ do
    docstats <- query $ GetDocumentStats
    userstats <- query $ GetUserStats
#ifndef WINDOWS
    df <- liftIO read_df
#else
    let df = empty
#endif
    ctx@Context {ctxtemplates} <- lift get
    let stats = StatsView { svDoccount = doccount docstats,
                            svSignaturecount = signaturecount docstats,
                            svUsercount = usercount userstats,
                            svViralinvitecount = viralinvitecount userstats,
                            svAdmininvitecount = admininvitecount userstats }
    content <- liftIO $ statsPage ctxtemplates stats (toString df)
    renderFromBody TopEmpty kontrakcja $ cdata content

indexDB :: Kontra Response
indexDB = onlySuperUser $ do
    ctx@Context {ctxtemplates} <- lift get
    files <- liftIO $ getDirectoryContents "_local/kontrakcja_state"
    content <- liftIO $ databaseContent ctxtemplates (sort files) 
    renderFromBody TopEmpty kontrakcja $ cdata  content
    
getUsersDetailsToCSV :: Kontra Response
getUsersDetailsToCSV = onlySuperUser $ do
      x <- query $ ExportUsersDetailsToCSV
      let response = toResponseBS (fromString "text/csv")   (L.fromChunks [x])
      return response    
        
    
    
{- | Handling user details change. It reads user info change, user settings change , paymentpolicy and payment account change -}     
handleUserChange :: String -> Kontra KontraLink
handleUserChange a = onlySuperUser $
                     do
                     let muserId = readM a
                     _ <- getAsStrictBS "change"
                     case muserId of 
                       Nothing -> mzero   
                       Just userId ->    
                        do 
                          muser <- query $ GetUserByUserID userId
                          case muser of 
                             Nothing -> mzero     
                             Just user -> do   
                                           --Reading changes from params using dedicated functions for each user part
                                           freetrialexpirationdate <- join . (fmap parseMinutesTimeMDY) <$> getField "freetrialexpirationdate"
                                           infoChange <- getUserInfoChange
                                           settingsChange <- getUserSettingsChange
                                           paymentAccountChange <- getUserPaymentAccountChange
                                           paymentPaymentPolicy <- getUserPaymentPolicyChange
                                           --Updating DB , ignoring fails
                                           _ <- update $ SetFreeTrialExpirationDate userId freetrialexpirationdate
                                           _ <- update $ SetUserInfo userId $ infoChange $ userinfo user
                                           _ <- update $ SetUserSettings userId $ settingsChange $ usersettings user
                                           _ <- update $ SetUserPaymentAccount userId $ paymentAccountChange $ userpaymentaccount user
                                           _ <- update $ SetUserPaymentPolicyChange userId $ paymentPaymentPolicy $ userpaymentpolicy user
                                           return $ LinkUserAdmin $ Just userId

handleUserEnableTrustWeaverStorage :: String -> Kontra KontraLink
handleUserEnableTrustWeaverStorage a =
    onlySuperUser $
                  do
                    let muserId = readM a
                    _ <- getAsStrictBS "enabletrustweaver"
                    case muserId of 
                       Nothing -> mzero   
                       Just userId ->    
                        do 
                          muser <- query $ GetUserByUserID userId
                          case muser of 
                             Nothing -> mzero     
                             Just user -> 
                                     case signeddocstorage (usersettings user) of
                                       Just _ -> do
                                         -- FIXME: add text: was already enabled
                                         return $ LinkUserAdmin $ Just userId
                                       Nothing -> (do
                                         let name = show userId
                                         Context{ctxtwconf} <- get
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

{-| Cleaning the database -}
handleDatabaseCleanup :: Kontra KontraLink
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


handleCreateUser :: Kontra KontraLink
handleCreateUser = onlySuperUser $ do
    ctx <- get
    email' <- getAsStrictBS "email"
    let email = BSC.map toLower email'
    fstname <- getAsStrictBS "fstname"
    sndname <- getAsStrictBS "sndname"
    custommessage <- getField "custommessage"
    freetill <- fmap (join . (fmap parseMinutesTimeMDY)) $ getField "freetill"
    muser <- liftIO $ createNewUserByAdmin ctx (fstname, sndname) email freetill custommessage
    when (isNothing muser) $ addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists $ ctxtemplates ctx)

    -- FIXME: where to redirect?
    return LinkStats
          
{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}  
getUserInfoChange :: Kontra (UserInfo -> UserInfo)
getUserInfoChange = do      
                     muserfstname        <- getFieldUTF "userfstname" 
                     musersndname        <- getFieldUTF "usersndname" 
                     muserpersonalnumber <- getFieldUTF "userpersonalnumber" 
                     musercompanyname    <- getFieldUTF "usercompanyname" 
                     musercompanyposition    <- getFieldUTF "usercompanyposition" 
                     musercompanynumber  <- getFieldUTF "usercompanynumber" 
                     museraddress        <- getFieldUTF "useraddress" 
                     muserzip            <- getFieldUTF "userzip" 
                     musercity           <- getFieldUTF "usercity" 
                     musercountry        <- getFieldUTF "usercountry" 
                     muserphone          <- getFieldUTF "userphone"
                     musermobile         <- getFieldUTF "usermobile" 
                     museremail          <- fmap (fmap Email) $ getFieldUTF "useremail" 
                     return (\UserInfo {
                                    userfstname      
                                  , usersndname 
                                  , userpersonalnumber
                                  , usercompanyname
                                  , usercompanyposition
                                  , usercompanynumber 
                                  , useraddress
                                  , userzip
                                  , usercity
                                  , usercountry
                                  , userphone
                                  , usermobile
                                  , useremail
                                  } ->  UserInfo {
                                            userfstname = maybe' userfstname muserfstname
                                          , usersndname = maybe' usersndname musersndname
                                          , userpersonalnumber = maybe' userpersonalnumber muserpersonalnumber
                                          , usercompanyname =  maybe' usercompanyname musercompanyname
                                          , usercompanynumber  =  maybe' usercompanynumber musercompanynumber
                                          , usercompanyposition = maybe' usercompanyposition musercompanyposition
                                          , useraddress =  maybe' useraddress museraddress
                                          , userzip = maybe' userzip muserzip
                                          , usercity  = maybe' usercity musercity
                                          , usercountry = maybe' usercountry musercountry
                                          , userphone = maybe' userphone muserphone
                                          , usermobile = maybe' usermobile musermobile
                                          , useremail =  maybe' useremail museremail
                                        })
                                        
{- | Reads params and returns function for conversion of user settings. With no param leaves fields unchanged -}
getUserSettingsChange :: Kontra (UserSettings -> UserSettings)
getUserSettingsChange =  do 
                          maccounttype          <- readField "accounttype" 
                          maccountplan          <- readField "accountplan" 
                          msigneddocstorage     <- readField "signeddocstorage" 
                          muserpaymentmethod    <- readField "userpaymentmethod" 
                          return (\UserSettings {
                                   accounttype 
                                 , accountplan 
                                 , signeddocstorage 
                                 , userpaymentmethod
                                 , preferreddesignmode }
                                       -> UserSettings {
                                            accounttype  = maybe' accounttype  maccounttype 
                                          , accountplan = maybe' accountplan maccountplan
                                          , signeddocstorage  = maybe' signeddocstorage  msigneddocstorage 
                                          , userpaymentmethod =  maybe' userpaymentmethod muserpaymentmethod
                                          , preferreddesignmode = preferreddesignmode
                                          })
                                          
{- | Reads params and returns function for conversion of user payment account. With no param leaves fields unchanged -}
getUserPaymentAccountChange :: Kontra (UserPaymentAccount -> UserPaymentAccount)
getUserPaymentAccountChange =  do 
                          mpaymentaccountfreesignatures        <- readField "paymentaccountfreesignatures" 
                          return (\UserPaymentAccount {
                                   paymentAgreementRef
                                 , paymentaccountfreesignatures
                                  }
                                    -> UserPaymentAccount  {
                                            paymentAgreementRef  = paymentAgreementRef
                                          , paymentaccountfreesignatures = maybe' paymentaccountfreesignatures mpaymentaccountfreesignatures
                                        })        

replace0SignatoryLinkID :: SignatoryLink -> Kontra SignatoryLink
replace0SignatoryLinkID l 
    | (signatorylinkid l == SignatoryLinkID 0) = do
                                                  liftIO $ print "changed 0 linkid"
                                                  linkid <- update $ GetUniqueSignatoryLinkID
                                                  return l { signatorylinkid = linkid}
    | otherwise = return l

replace0MagicHash :: SignatoryLink -> Kontra SignatoryLink
replace0MagicHash l
    | (signatorymagichash l == MagicHash 0) = do
                                               liftIO $ print "changed 0 magichash"
                                               magichash <- update $ GetMagicHash
                                               return l { signatorymagichash = magichash }
    | otherwise = return l
                          
migrate0SignatoryLinks :: [SignatoryLink] -> Kontra [SignatoryLink]
migrate0SignatoryLinks links = do
    l1 <- sequence $ map replace0SignatoryLinkID links
    l2 <- sequence $ map replace0MagicHash l1
    return l2

{- |
   A temporary service to fix the migration; 0 MagicHash and 0 SignatoryLinkID is replaced with new
   random one.

  I'm removing this! -EN
 -}
handleMigrate0 :: Kontra Response
handleMigrate0 = onlySuperUser $ do
 documents <- query $ GetDocuments
 d2 <- sequence $ map (\d -> do
                               links <- migrate0SignatoryLinks $ documentsignatorylinks d
                               d2 <- update $ SetSignatoryLinks (documentid d) links
                               return d2)
                      documents
 liftIO $ print d2 -- force the value
 sendRedirect LinkAdminOnly

{- | Reads params and returns function for conversion of user payment policy. With no param clears custom and temporary fields !!!! -}
getUserPaymentPolicyChange :: Kontra (UserPaymentPolicy -> UserPaymentPolicy)
getUserPaymentPolicyChange =  do 
                          mtmppaymentchangeenddate   <- fmap (join . (fmap parseMinutesTimeMDY)) $ getField "tmppaymentchangeenddate"
                          mpaymentaccounttype        <- readField "paymentaccounttype" 
                          customPaymentChange        <- getPaymentChangeChange "custom"
                          tempPaymentChange          <- getPaymentChangeChange "temp"
                          return (\UserPaymentPolicy {
                                    paymentaccounttype 
                                  , custompaymentchange
                                  , temppaymentchange
                                  }
                                    -> UserPaymentPolicy  {
                                            paymentaccounttype   = maybe' paymentaccounttype   mpaymentaccounttype 
                                          , custompaymentchange = customPaymentChange custompaymentchange
                                          , temppaymentchange = case  mtmppaymentchangeenddate of
                                                                 Nothing ->  Nothing 
                                                                 Just enddate -> case temppaymentchange of
                                                                                   Nothing -> Just (enddate,tempPaymentChange emptyChange) 
                                                                                   Just (_,change) -> Just (enddate, tempPaymentChange change) 
                                                 
                                        })      
                                        
{- | Reads params and returns structured params for user managment pages. -}                                        
getAdminUsersPageParams :: Kontra AdminUsersPageParams
getAdminUsersPageParams = do
  search <- getDataFn' (look "search")         
  startletter <-  getDataFn' (look "startletter")         
  mpage <-  getDataFn' (look "page")         
  let mpage' = join $ fmap readM mpage
  return $ AdminUsersPageParams {search = search, startletter=startletter, page = maybe 0 id mpage'}
                                                                          

{- Create service-}
handleCreateService :: Kontra KontraLink
handleCreateService = onlySuperUser $ do
    mname<- getFieldUTF "name"
    madmin <- liftMM  (query . GetUserByEmail Nothing . Email) (getFieldUTF "admin")
    case (mname,madmin) of
         (Just name,Just admin) -> do 
            pwdBS <- getFieldUTFWithDefault mempty "password"
            pwd <- liftIO $ createPassword pwdBS
            update $ CreateService (ServiceID name) pwd (ServiceAdmin $ unUserID $ userid admin)
            return LoopBack
         _ -> return LinkMain
          
{- Services page-}
showServicesPage :: Kontra Response
showServicesPage = onlySuperUser $ do
  ctx@Context {ctxtemplates} <- lift get
  services <- query GetServices
  content <- liftIO $ servicesAdminPage ctxtemplates services
  renderFromBody TopEmpty kontrakcja $ cdata content 
    
                     
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
docStatsZero = DocStatsL 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                     
addStats (DocStatsL a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14) (DocStatsL b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14) = 
      DocStatsL (a1+b1) (a2+b2) (a3+b3) (a4+b4) (a5+b5) (a6+b6) (a7+b7) (a8+b8) (a9+b9) (a10+b10) (a11+b11) (a12+b12) (a13+b13) (a14+b14)

countSignatures = length . filter (isJust . maybesigninfo) . documentsignatorylinks 

-- calculateStats :: [Documents] -> ??
calculateStatsFromDocuments documents = 
  foldl' ins IntMap.empty documents
  where
    ins map doc = foldl' (\m (k,v) -> IntMap.insertWith addStats k v m) map (stuff doc)
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
                      _ -> docStatsZero  -- catch all to make it run in case somebody adds new status
                      )
                ]
                
calculateStatsFromUsers users =                     
  foldl' ins IntMap.empty users
  where
    ins map user = foldl' (\m (k,v) -> IntMap.insertWith addStats k v m) map (stuff user)
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
                
                
fieldsFromStats users documents = do
    let userStats = calculateStatsFromUsers users
        documentStats = calculateStatsFromDocuments documents
        showAsDate int = show (int `div` 10000) ++ "-" ++ show (int `div` 100 `mod` 100) ++ "-" ++ show (int `mod` 100)
        stats = IntMap.unionWith addStats userStats documentStats
    let fieldify (date,stat) = do 
          field "date" $ showAsDate date
          field "documents" $ do
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
          field "users" $ do
            field "all" $ dsAllUsers stat
            field "viralInvites" $ dsViralInvites stat
            field "adminInvites" $ dsAdminInvites stat
          
    field "stats" $ map fieldify (IntMap.toList stats) 
                    
handleStatistics :: Kontra Response
handleStatistics = 
  onlySuperUser $ do
    ctx@Context{ctxtemplates} <- get
    documents <- query $ GetDocuments
    users <- query $ GetAllUsers
    content <- renderTemplateM "statisticsPage" $ do
      fieldsFromStats users documents
    renderFromBody TopEmpty kontrakcja $ cdata content
    
  