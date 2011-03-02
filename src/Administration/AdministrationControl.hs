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
          , showAllUsersTable
          , showStats
          , indexDB
          , getUsersDetailsToCSV
          , handleUserChange
          , handleDatabaseCleanup
          , handleCreateUser
          , handleUserEnableTrustWeaverStorage
          , handleMigrate0
          ) where
import Control.Monad.State
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
import System.Directory
import Data.List (isPrefixOf,sort)
import User.UserControl
import User.UserView
import Data.Maybe
import Redirect
import System.Process
import System.IO (hClose)
import qualified TrustWeaver as TW
import Data.Char

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
showAdminMainPage ::Kontra Response
showAdminMainPage = onlySuperUser $
                     do
                      ctx@Context {ctxtemplates} <- lift get
                      content <- liftIO $ adminMainPage ctxtemplates 
                      renderFromBody ctx TopEmpty kontrakcja $ cdata content 

{- | Process view for advanced user administration -}                    
showAdminUserAdvanced :: Kontra Response
showAdminUserAdvanced = onlySuperUser $
                          do
                           ctx@Context {ctxtemplates} <- lift get
                           users <- query $ GetAllUsers
                           params <- getAdminUsersPageParams
                           content <- liftIO $ adminUsersAdvancedPage ctxtemplates users params
                           renderFromBody ctx TopEmpty kontrakcja $ cdata content 

{- | Process view for finding a user in basic administration. If provided with userId string as param 
it allows to edit user details -}     
showAdminUsers:: Maybe String -> Kontra Response 
showAdminUsers Nothing= onlySuperUser $
                          do
                           ctx@Context {ctxtemplates} <- lift get
                           users <- getUsersAndStats
                           params <- getAdminUsersPageParams
                           content <- liftIO $ adminUsersPage ctxtemplates users params
                           renderFromBody ctx TopEmpty kontrakcja $ cdata content 

showAdminUsers (Just a)= onlySuperUser $
                         do 
                         ctx@Context {ctxtemplates} <- lift get
                         let muserId = maybeRead a
                         case muserId of 
                           Nothing -> mzero   
                           Just userId ->    
                            do 
                             muser <- query $ GetUserByUserID userId
                             case muser of 
                              Nothing -> mzero     
                              Just user -> do   
                                     paymentmodel <- update $ GetPaymentModel $ paymentaccounttype $ userpaymentpolicy user
                                     content <- liftIO $ adminUserPage ctxtemplates user paymentmodel
                                     renderFromBody ctx TopEmpty kontrakcja $ cdata content 

getUsersAndStats :: Kontra [(User,DocStats)]
getUsersAndStats = do
    users <- query $ GetAllUsers
    let queryStats user = do
          docstats <- query $ GetDocumentStatsByUser user
          return (user, docstats)
    users2 <- mapM queryStats users
    return users2

{- Shows table of all users-}
showAllUsersTable :: Kontra Response
showAllUsersTable = onlySuperUser $ do
    ctx@Context {ctxtemplates} <- lift get
    users <- getUsersAndStats
    content <- liftIO $ allUsersTable ctxtemplates users
    renderFromBody ctx TopEmpty kontrakcja $ cdata  content



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
    allusers <- query $ GetAllUsers
#ifndef WINDOWS
    df <- liftIO read_df
#else
    let df = empty
#endif
    ctx@Context {ctxtemplates} <- lift get
    let stats = StatsView { svDoccount = doccount docstats,
                            svSignaturecount = signaturecount docstats,
                            svUsercount = (length allusers) }
    content <- liftIO $ statsPage ctxtemplates stats (toString df)
    renderFromBody ctx TopEmpty kontrakcja $ cdata content

indexDB :: Kontra Response
indexDB = onlySuperUser $ do
    ctx@Context {ctxtemplates} <- lift get
    files <- liftIO $ getDirectoryContents "_local/kontrakcja_state"
    content <- liftIO $ databaseContent ctxtemplates (sort files) 
    renderFromBody ctx TopEmpty kontrakcja $ cdata  content
    
getUsersDetailsToCSV :: Kontra Response
getUsersDetailsToCSV = onlySuperUser $ do
      x <- query $ ExportUsersDetailsToCSV
      let response = toResponseBS (fromString "text/csv")   (L.fromChunks [x])
      return response    
        
    
    
{- | Handling user details change. It reads user info change, user settings change , paymentpolicy and payment account change -}     
handleUserChange :: String -> Kontra KontraLink
handleUserChange a = onlySuperUser $
                     do
                     let muserId = maybeRead a
                     _ <- g "change"
                     case muserId of 
                       Nothing -> mzero   
                       Just userId ->    
                        do 
                          muser <- query $ GetUserByUserID userId
                          case muser of 
                             Nothing -> mzero     
                             Just user -> do   
                                           --Reading changes from params using dedicated functions for each user part
                                           infoChange <- getUserInfoChange
                                           settingsChange <- getUserSettingsChange
                                           paymentAccountChange <- getUserPaymentAccountChange
                                           paymentPaymentPolicy <- getUserPaymentPolicyChange
                                           --Updating DB , ignoring fails
                                           _ <- update $ SetUserInfo userId $ infoChange $ userinfo user
                                           _ <- update $ SetUserSettings userId $ settingsChange $ usersettings user
                                           _ <- update $ SetUserPaymentAccount userId $ paymentAccountChange $ userpaymentaccount user
                                           _ <- update $ SetUserPaymentPolicyChange userId $ paymentPaymentPolicy $ userpaymentpolicy user
                                           return $ LinkUserAdmin $ Just userId

handleUserEnableTrustWeaverStorage :: String -> Kontra KontraLink
handleUserEnableTrustWeaverStorage a =
    onlySuperUser $
                  do
                    let muserId = maybeRead a
                    _ <- g "enabletrustweaver"
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
    email' <- g "email"
    let email = BSC.map toLower email'
    fullname <- g "fullname"
    custommessage <- getField "custommessage"
    freetill <- fmap (join . (fmap parseMinutesTimeMDY)) $ getField "freetill"
    muser <- liftIO $ createNewUserByAdmin ctx fullname email freetill custommessage
    when (isNothing muser) $ addFlashMsg =<< (liftIO $ flashMessageUserWithSameEmailExists $ ctxtemplates ctx)

    -- FIXME: where to redirect?
    return LinkStats
          
{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}  
getUserInfoChange::Kontra (UserInfo -> UserInfo)
getUserInfoChange = do      
                     muserfstname        <- getField' fromString "userfstname" 
                     musersndname        <- getField' fromString "usersndname" 
                     muserpersonalnumber <- getField' fromString "userpersonalnumber" 
                     musercompanyname    <- getField' fromString "usercompanyname" 
                     musercompanyposition    <- getField' fromString "usercompanyposition" 
                     musercompanynumber  <- getField' fromString "usercompanynumber" 
                     museraddress        <- getField' fromString "useraddress" 
                     muserzip            <- getField' fromString "userzip" 
                     musercity           <- getField' fromString "usercity" 
                     musercountry        <- getField' fromString "usercountry" 
                     muserphone          <- getField' fromString "userphone"
                     musermobile         <- getField' fromString "usermobile" 
                     museremail          <- getField' (Email . fromString)  "useremail" 
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
getUserSettingsChange::Kontra (UserSettings -> UserSettings)
getUserSettingsChange =  do 
                          maccounttype          <- readField "accounttype" 
                          maccountplan          <- readField "accountplan" 
                          msigneddocstorage     <- readField "signeddocstorage" 
                          muserpaymentmethod    <- readField "userpaymentmethod" 
                          return (\UserSettings {
                                   accounttype 
                                 , accountplan 
                                 , signeddocstorage 
                                 , userpaymentmethod }
                                       -> UserSettings {
                                            accounttype  = maybe' accounttype  maccounttype 
                                          , accountplan = maybe' accountplan maccountplan
                                          , signeddocstorage  = maybe' signeddocstorage  msigneddocstorage 
                                          , userpaymentmethod =  maybe' userpaymentmethod muserpaymentmethod
                                          })
                                          
{- | Reads params and returns function for conversion of user payment account. With no param leaves fields unchanged -}
getUserPaymentAccountChange::Kontra (UserPaymentAccount -> UserPaymentAccount)
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

{- | Reads params and returns function for conversion of user payment policy. With no param clears custom and temporary fields !!!!-}
getUserPaymentPolicyChange::Kontra (UserPaymentPolicy -> UserPaymentPolicy)
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
getAdminUsersPageParams::Kontra AdminUsersPageParams
getAdminUsersPageParams = do
                          search <- getDataFn' (look "search")         
                          startletter <-  getDataFn' (look "startletter")         
                          mpage <-  getDataFn' (look "page")         
                          let mpage' = join $ fmap maybeRead mpage
                          return $ AdminUsersPageParams {search = search, startletter=startletter, page = maybe 0 id mpage'}
                                                                          


