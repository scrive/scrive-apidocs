{-# LANGUAGE CPP, OverloadedStrings, TupleSections#-}
{-# OPTIONS_GHC -Wall #-}
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
          , handleBecome
          , handleTakeOverDocuments
          , handleDeleteAccount
          , handleCreateUser
          , handleUserEnableTrustWeaverStorage
          ) where
import "mtl" Control.Monad.State
import AppView
import Happstack.Server hiding (simpleHTTP)
import Happstack.State (update,query)
import Misc
import User
import HSP (cdata)
import Administration.AdministrationView
import Payments.PaymentsState
import DocState
import Data.ByteString.UTF8 (fromString,toString)
import Data.ByteString (ByteString,empty, hGetContents)
import qualified Data.ByteString.Lazy  as L
import KontraLink
import Payments.PaymentsControl(readMoneyField,getPaymentChangeChange)
import MinutesTime
import System.Directory
import Data.List (isPrefixOf,sort)
import UserControl
import UserView
import Data.Maybe
import System.Process
import System.IO (hClose)
import qualified TrustWeaver as TW

eitherFlash :: ServerPartT (StateT Context IO) (Either String b)
            -> ServerPartT (StateT Context IO) b
eitherFlash action = do
  x <- action
  case x of
    Left errmsg -> do
           addFlashMsgText errmsg
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
                           users <- query $ GetAllUsers
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
{- Shows table of all users-}
showAllUsersTable :: Kontra Response
showAllUsersTable = onlySuperUser $ do
    ctx@Context {ctxtemplates} <- lift get
    users <- query $ GetAllUsers
    let queryNumberOfDocuments user = do
          documents <- query $ GetDocumentsByAuthor (userid user)
          return (user,length documents)
    users2 <- mapM queryNumberOfDocuments users
    content <- liftIO $ allUsersTable ctxtemplates users2
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
    ndocuments <- query $ GetDocumentStats
    allusers <- query $ GetAllUsers
#ifndef WINDOWS
    df <- liftIO read_df
#else
    let df = empty
#endif
    ctx@Context {ctxtemplates} <- lift get
    content <- liftIO $ statsPage ctxtemplates (length allusers) ndocuments (toString df)
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

                                         update $ SetUserSettings userId newsettings
                                         
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

{- Administrator can became any user he want's -}
handleBecome :: Kontra KontraLink
handleBecome = onlySuperUser $ do
    muserid  <- readField "user"
    case muserid of
        Just userid ->  do
                         user <- liftM query GetUserByUserID userid
                         logUserToContext user
                         return LinkMain
        _ -> mzero                 



{- Assinging all selected user docs to current user -}
handleTakeOverDocuments :: Kontra KontraLink
handleTakeOverDocuments = onlySuperUser $ do
     Context{ctxmaybeuser = Just ctxuser} <- lift $ get
     msrcuserid  <- readField "user"
     case (msrcuserid) of
      Just srcuserid ->  do
                         msrcuser <- query $ GetUserByUserID srcuserid
                         case msrcuser of
                          Just srcuser -> do     
                                         update $ FragileTakeOverDocuments (userid ctxuser) srcuserid
                                         addFlashMsgText $ "Took over all documents of '" ++ toString (userfullname srcuser) ++ "'. His account is now empty and can be deleted if you wish so. Show some mercy, though."
                                         return LoopBack
                          Nothing -> mzero                
      Nothing -> mzero                   

{- Deleting user account, Fails if user still has any documents -}
handleDeleteAccount :: Kontra KontraLink
handleDeleteAccount = onlySuperUser $ do
    muserid <- readField "user"
    case (muserid) of
      Just userid -> do
                muser <- query $ GetUserByUserID userid
                case (muser) of
                  Just user ->  do  
                      documents <- query $ GetDocumentsByAuthor userid
                      if null documents
                       then do
                           _ <- update $ FragileDeleteUser userid
                           addFlashMsgText ("User deleted. You will not see '" ++ toString (userfullname user) ++ "' here anymore")
                       else
                           addFlashMsgText ("I cannot delete user. '" ++ toString (userfullname user) ++ "' still has " ++ show (length documents) ++ " documents as author. Take over his documents, then try to delete the account again.")
                      return LoopBack
                  Nothing -> mzero
      Nothing -> mzero          

handleCreateUser :: Kontra KontraLink
handleCreateUser = onlySuperUser $ do
    ctx <- get
    email <- g "email"
    fullname <- g "fullname"
    muser <- liftIO $ createNewUserByAdmin ctx fullname email 
    when (isNothing muser) $ addFlashMsgText =<< (liftIO $ flashMessageUserWithSameEmailExists $ ctxtemplates ctx)

    -- FIXME: where to redirect?
    return LinkStats
          
{- | Reads params and returns function for conversion of user info. With no param leaves fields unchanged -}  
getUserInfoChange::Kontra (UserInfo -> UserInfo)
getUserInfoChange = do      
                     muserfstname        <- getField' fromString "userfstname" 
                     musersndname        <- getField' fromString "usersndname" 
                     muserpersonalnumber <- getField' fromString "userpersonalnumber" 
                     musercompanyname    <- getField' fromString "usercompanyname" 
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
                          mpaymentaccountmoney                 <- readMoneyField "paymentaccountmoney" 
                          mpaymentaccountfreesignatures        <- readField "paymentaccountfreesignatures" 
                          return (\UserPaymentAccount {
                                   paymentaccountmoney
                                 , paymentaccountfreesignatures
                                  }
                                    -> UserPaymentAccount  {
                                            paymentaccountmoney  = maybe' paymentaccountmoney  mpaymentaccountmoney
                                          , paymentaccountfreesignatures = maybe' paymentaccountfreesignatures mpaymentaccountfreesignatures
                                        })                                        

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
                                                                          


