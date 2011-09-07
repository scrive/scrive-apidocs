{-# LANGUAGE OverloadedStrings, OverlappingInstances, IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestingUtil where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)

import Control.Applicative
import System.Random
import Test.QuickCheck
import Happstack.Server
import Happstack.State
import Test.QuickCheck.Gen
import Control.Monad.Trans
import Data.Maybe
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Test.HUnit as T

import API.API
import DB.Classes
import DB.Types
import Company.Model
import FlashMessage
import qualified AppLogger as Log
import StateHelper
import Mails.MailsUtil
import Doc.DocState
import KontraMonad
import MinutesTime
import User.Model
import Misc
import API.Service.Model
import Data.Typeable
import Doc.Invariants
import Doc.DocInfo
import Doc.DocProcess

instance Arbitrary UserID where
  arbitrary = do
    a <- arbitrary
    return $ UserID a

instance Arbitrary Company where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Company { companyid  = a
                     , companyexternalid = b
                     , companyservice = c
                     , companyinfo = d
                     }
      
instance Arbitrary CompanyID where
  arbitrary = do
    a <- arbitrary
    return $ CompanyID a
  
instance Arbitrary ExternalCompanyID where
  arbitrary = do
    a <- arbitrary
    return $ ExternalCompanyID a

instance Arbitrary CompanyInfo where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    return $ CompanyInfo { companyname       = a
                         , companynumber     = b
                         , companyaddress    = c
                         , companyzip        = d
                         , companycity       = e
                         , companycountry    = f
                         }

instance Arbitrary ServiceID where
  arbitrary = do
    a <- arbitrary
    return $ ServiceID a
  
instance Arbitrary MagicHash where
  arbitrary = do
    a <- arbitrary
    return $ MagicHash a

instance Arbitrary MailsDeliveryStatus where
  arbitrary = elements [ Delivered 
                       , Undelivered
                       , Unknown
                       , Deferred]

instance Arbitrary TimeoutTime where
  arbitrary = do
    a <- arbitrary
    return $ TimeoutTime a

instance Arbitrary MinutesTime where
  arbitrary = do
    a <- arbitrary
    return $ fromSeconds a

instance Arbitrary DocumentTag where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ DocumentTag a b

instance Arbitrary DocumentUI where
  arbitrary = do
    a <- arbitrary
    return $ DocumentUI a


{- | Sometimes we get and object that is not as random as we would expect (from some reason)
     Like author signatorylink that by default does not have any fields attached
     This is a class to make it more random - so to attach this fields for example.
-}
class ExtendWithRandomnes a where
    moreRandom :: a -> Gen a
    extendRandomness :: MonadIO m => a -> m a
    extendRandomness a = do
          stdgen <- liftIO newStdGen
          return $ unGen (moreRandom a) stdgen 10

instance ExtendWithRandomnes SignatoryDetails where
    moreRandom sl = return sl

instance Arbitrary SignatoryLinkID where
  arbitrary = do
    si <- arbitrary
    return $ SignatoryLinkID si

instance Arbitrary SignatoryLink where
  arbitrary = do
    (slid, sd, mh) <- arbitrary
    (signinfo) <- arbitrary
    seeninfo <- arbitrary
    return $  SignatoryLink { signatorylinkid            = slid
                            , signatorydetails           = sd
                            , signatorymagichash         = mh
                            , maybesignatory             = Nothing
                            , maybesupervisor            = Nothing
                            , maybecompany               = Nothing
                            , maybesigninfo              = signinfo
                                                           -- we don't want seeninfo if we don't have signinfo
                            , maybeseeninfo              = maybe Nothing (\_ -> Just seeninfo) signinfo
                            , maybereadinvite            = Nothing
                            , invitationdeliverystatus   = Unknown
                            , signatorysignatureinfo     = Nothing
                            , signatoryroles             = []
                            , signatorylinkdeleted       = False
                            , signatorylinkreallydeleted = False
                            }

instance Arbitrary SignatureProvider where
  arbitrary = elements [ BankIDProvider
                       , TeliaProvider
                       , NordeaProvider
                       ]

instance Arbitrary SignatureInfo where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    g <- arbitrary
    return $ SignatureInfo { signatureinfotext        = a
                           , signatureinfosignature   = b
                           , signatureinfocertificate = c
                           , signatureinfoprovider    = d
                           , signaturefstnameverified = e
                           , signaturelstnameverified = f
                           , signaturepersnumverified = g
                           }

instance Arbitrary CSVUpload where
  arbitrary = do
    a <- arbitrary
    cols <- arbitrary
    rows <- arbitrary
    b <- vectorOf rows (vectorOf cols arbitrary)
    c <- arbitrary
    return $ CSVUpload { csvtitle = a
                       , csvcontents = b
                       , csvsignatoryindex = c
                       }

instance Arbitrary DocumentID where
  arbitrary = do
    ds <- arbitrary
    return $ DocumentID ds

instance Arbitrary DocumentType where
  arbitrary = elements [ Signable Contract
                       , Signable Order
                       , Signable Offer
                       , Template Contract
                       , Template Order
                       , Template Offer
                       , Attachment
                       , AttachmentTemplate
                       ]
         

instance Arbitrary Document where
  arbitrary = do
    ds <- arbitrary
    dt <- arbitrary
    sls <- arbitrary
    f <- arbitrary
    return $ blankDocument { documentstatus = ds 
                           , documenttype = dt
                           , documentsignatorylinks = sls
                           , documentfiles = [f]
                           }

instance Arbitrary DocumentStatus where
  arbitrary = elements [ Preparation
                       , Pending
                       , Closed
                       , Canceled
                       , Timedout
                       , Rejected
                       , AwaitingAuthor
                       , DocumentError "Bad document."
                       ]
    
nonemptybs :: Gen BS.ByteString              
nonemptybs = do
  s <- arbString 1 10
  return $ BS.fromString s
  
-- | Remove fields from duplicate types
filterSingleFieldType :: [SignatoryField] -> [SignatoryField]
filterSingleFieldType [] = []
filterSingleFieldType (f:fs) = f : filterSingleFieldType (filter (\h-> sfType f /= sfType h) fs)

instance Arbitrary SignatoryDetails where
  arbitrary = do
    fn <- nonemptybs
    ln <- nonemptybs
    em <- arbEmail
    fields <- filterSingleFieldType <$> arbitrary
    return $ SignatoryDetails { signatorysignorder = SignOrder 1
                              , signatoryfields = filter (\f->notElem (sfType f) [FirstNameFT, LastNameFT, EmailFT]) fields 
                                                  ++ [ SignatoryField FirstNameFT fn []
                                                     , SignatoryField LastNameFT  ln []
                                                     , SignatoryField EmailFT     em []]}
      
instance Arbitrary FieldPlacement where
  arbitrary = do
    (a,b,c,d,e) <- arbitrary
    return $ FieldPlacement { placementx = a
                            , placementy = b
                            , placementpage = c
                            , placementpagewidth = d
                            , placementpageheight = e      
                            }
    
instance Arbitrary FieldType where
  arbitrary = do
    fieldlabel <- arbitrary
    filled <- arbitrary
    elements [FirstNameFT, LastNameFT, EmailFT, CompanyFT, CompanyNumberFT, PersonalNumberFT, CustomFT fieldlabel filled]
  
instance Arbitrary SignatoryField where
  arbitrary = do
    t <- arbitrary
    v <- arbitrary
    p <- arbitrary
    return $ SignatoryField { sfType = t
                            , sfValue = v
                            , sfPlacements = p
                            }

instance Arbitrary FieldDefinition where
   arbitrary = do
    name <- arbitrary
    value <- arbitrary
    filledByAuthor <- arbitrary
    return $ FieldDefinition { fieldlabel = name,
                               fieldvalue = value,
                               fieldplacements = [],
                               fieldfilledbyauthor = filledByAuthor
                             }

instance Arbitrary SignatoryRole where
  arbitrary = return SignatoryPartner

instance Arbitrary DocumentFunctionality where
  arbitrary = elements [BasicFunctionality, AdvancedFunctionality]

instance Arbitrary IdentificationType where
  arbitrary = elements [EmailIdentification, ELegitimationIdentification]

instance Arbitrary UserInfo where
  arbitrary = do
    fn <- arbitrary
    ln <- arbitrary
    pn <- arbitrary
    em <- arbEmail

    return $ UserInfo { userfstname     = fn
                      , usersndname     = ln
                      , userpersonalnumber  = pn
                      , usercompanyposition = ""
                      , userphone           = ""
                      , usermobile          = ""
                      , useremail           = Email em
                      }

    
instance Arbitrary BS.ByteString where
  arbitrary = fmap BS.fromString arbitrary

arbString :: Int -> Int -> Gen String
arbString minl maxl = do
  l <- choose (minl, maxl)
  vectorOf l $ elements ['a'..'z']
  
arbEmail :: Gen BS.ByteString
arbEmail = do
  n <- arbString 1 34
  d <- arbString 3 7
  return $ BS.fromString (n ++ "@" ++ d ++ ".com")



blankUser :: User
blankUser = User {  
                   userid                  =  UserID 0
                 , userpassword            =  Nothing
                 , useriscompanyadmin = False
                 , useraccountsuspended    =  False  
                 , userhasacceptedtermsofservice = Nothing
                 , usersignupmethod = AccountRequest
                 , userinfo = UserInfo {
                                    userfstname = BS.empty
                                  , usersndname = BS.empty
                                  , userpersonalnumber = BS.empty
                                  , usercompanyposition =  BS.empty
                                  , userphone = BS.empty
                                  , usermobile = BS.empty
                                  , useremail =  Email BS.empty 
                                   }
                , usersettings  = UserSettings {
                                    preferreddesignmode = Nothing
                                  , lang = Misc.defaultValue
                                  , region = Misc.defaultValue
                                  , systemserver = Misc.defaultValue
                                  }
              , userservice = Nothing
              , usercompany = Nothing
              }

blankDocument :: Document 
blankDocument =
          Document
          { documentid                   = DocumentID 0
          , documenttitle                = BS.empty
          , documentsignatorylinks       = []
          , documentfiles                = []
          , documentstatus               = Preparation
          , documenttype                 = Signable Contract
          , documentfunctionality        = AdvancedFunctionality
          , documentctime                = fromSeconds 0
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = Nothing
          , documenttimeouttime          = Nothing
          , documentlog                  = []
          , documentinvitetext           = BS.empty
          , documentsealedfiles          = []
          , documenttrustweaverreference = Nothing
          , documentallowedidtypes       = []
          , documentcsvupload            = Nothing
          , documentcancelationreason    = Nothing
          , documentinvitetime           = Nothing
          , documentsharing              = Doc.DocState.Private
          , documentrejectioninfo        = Nothing
          , documenttags                 = []
          , documentui                   = emptyDocumentUI
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentdeleted              = False
          , documentsignatoryattachments = []
          , documentattachments          = []
          }


testThat :: String -> Connection -> DB () -> Test
testThat s conn a = testCase s (withTestEnvironment conn a)

addNewCompany ::  DB Company
addNewCompany = do
    eid <- rand 10 arbitrary 
    dbUpdate $ CreateCompany Nothing eid

addNewUser :: String -> String -> String -> DB (Maybe User)
addNewUser firstname secondname email =
  dbUpdate $ AddUser (BS.fromString firstname, BS.fromString secondname) (BS.fromString email) Nothing False Nothing Nothing defaultValue defaultValue defaultValue

addNewCompanyUser :: String -> String -> String -> CompanyID -> DB (Maybe User)
addNewCompanyUser firstname secondname email cid =
  dbUpdate $ AddUser (BS.fromString firstname, BS.fromString secondname) (BS.fromString email) Nothing False Nothing (Just cid) defaultValue defaultValue defaultValue

addNewRandomUser :: DB User
addNewRandomUser = do
  fn <- rand 10 $ arbString 3 30
  ln <- rand 10 $ arbString 3 30
  em <- rand 10 arbEmail
  muser <- addNewUser fn ln (BS.toString em)
  case muser of
    Just user -> return user
    Nothing -> do
      Log.debug "Could not create user, trying again."
      addNewRandomUser

addNewRandomAdvancedUser :: DB User
addNewRandomAdvancedUser = do
  User{userid,usersettings} <- addNewRandomUser
  True <- dbUpdate $ SetUserSettings userid (usersettings{ preferreddesignmode = Just AdvancedMode })
  Just user <- dbQuery $ GetUserByID userid
  return user

addService :: String -> UserID -> DB (Maybe Service)
addService name uid =
  dbUpdate $ CreateService (ServiceID $ BS.fromString name) Nothing uid

emptySignatoryDetails :: SignatoryDetails
emptySignatoryDetails = SignatoryDetails
    { signatoryfields = []
    , signatorysignorder = SignOrder 1
    }

addRandomDocumentWithAuthor :: User -> DB DocumentID
addRandomDocumentWithAuthor user = do
  rs <- rand 10 arbitrary
  let roles = SignatoryAuthor : rs
  doc <- rand 10 arbitrary
  slsab <- rand 10 arbitrary
  let sls = 1 + abs slsab
  sldets <- rand 10 (vectorOf sls arbitrary)
  slr <- rand 1000 (vectorOf sls $ elements [[], [SignatoryPartner]])
  slinks <- sequence $ zipWith (\a r -> update $ (SignLinkFromDetailsForTest a r)) sldets slr
  
  mcompany <- case usercompany user of  
    Nothing -> return Nothing
    Just cid -> dbQuery $ GetCompany cid
  
  asd <- extendRandomness $ signatoryDetailsFromUser user mcompany
  asl <- update $ SignLinkFromDetailsForTest asd roles
  let adoc = doc { documentsignatorylinks = slinks ++ 
                                            [asl { maybesignatory = Just (userid user) }]
                 }
  update $ StoreDocumentForTesting adoc

getRandomAuthorRoles :: MonadIO m => Document -> m [SignatoryRole]
getRandomAuthorRoles doc =
  rand 10000 (elements $ getPossibleAuthorRoles doc)

getPossibleAuthorRoles :: Document -> [[SignatoryRole]]
getPossibleAuthorRoles doc = [SignatoryAuthor] :
  case getValueForProcess doc processauthorsend of
    Just True -> []
    _ ->  [[SignatoryAuthor, SignatoryPartner], [SignatoryPartner, SignatoryAuthor]]

addRandomDocumentWithAuthorAndCondition :: User -> (Document -> Bool) -> DB Document
addRandomDocumentWithAuthorAndCondition user p =  do
  doc <- rand 10 arbitrary
  roles <- getRandomAuthorRoles doc
  
  mcompany <- case usercompany user of  
    Nothing -> return Nothing
    Just cid -> dbQuery $ GetCompany cid
    
  now <- liftIO getMinutesTime
  
  (signinfo, seeninfo) <- rand 10 arbitrary
  asd <- extendRandomness $ signatoryDetailsFromUser user mcompany
  asl <- update $ SignLinkFromDetailsForTest asd roles
  let asl' = asl { maybeseeninfo = seeninfo
                 , maybesigninfo = signinfo
                 }
             
  let siglinks = documentsignatorylinks doc ++ [asl' { maybesignatory = Just (userid user), maybecompany = usercompany user }]
  let unsignedsiglinks = map (\sl -> sl { maybesigninfo = Nothing,
                                          maybeseeninfo = Nothing }) siglinks
  let siglinksandauthor = (if isPreparation doc then unsignedsiglinks else siglinks)
  let adoc = doc { documentsignatorylinks = siglinksandauthor
                 }
  if p adoc
    then do
    let d = (invariantProblems now adoc)
    if isNothing d
      then do
        docid <- update $ StoreDocumentForTesting adoc
        mdoc <- query $ GetDocumentByDocumentID docid
        case mdoc of
          Nothing -> do
            assertFailure "Could not store document."
            return doc
          Just doc' -> do
            return doc'
      else do
        --uncomment this to find out why the doc was rejected
        --print adoc
        --print $ "rejecting doc: " ++ fromJust d
        addRandomDocumentWithAuthorAndCondition user p
    else do
      --print adoc   
      addRandomDocumentWithAuthorAndCondition user p

rand :: MonadIO m => Int -> Gen a -> m a
rand i a = do  
  stdgn <- liftIO newStdGen
  return $ unGen a stdgn i

untilCondition :: (Monad m) => (b -> Bool) -> m b -> m b
untilCondition cond gen = do
  v <- gen
  if cond v then return v else untilCondition cond gen

addRandomDocumentWithAuthor' :: User -> DB Document
addRandomDocumentWithAuthor' user = addRandomDocumentWithAuthorAndCondition user (\_ -> True)

doTimes :: Int -> DB (Maybe (DB ())) -> DB ()
doTimes i action
  | i == 0 = return ()
  | otherwise = do
    res <- action
    case res of
      Nothing -> doTimes i action
      Just ass -> do
        _ <- ass
        doTimes (i - 1) action

invalidateTest :: DB (Maybe (DB ()))
invalidateTest = return Nothing

validTest :: DB () -> DB (Maybe (DB ()))
validTest = return . Just

-- Random gen

--Random query
class RandomQuery a b where
  randomQuery :: MonadIO m => a -> m b
  
instance (QueryEvent ev res) => RandomQuery ev res where
  randomQuery = query

instance (Arbitrary a, RandomQuery c b) => RandomQuery (a -> c) b where
  randomQuery f = do
    a <- rand 10 arbitrary
    randomQuery $ f a

--Random update
class RandomUpdate a b where
  randomUpdate :: MonadIO m => a -> m b

instance (UpdateEvent ev res) => RandomUpdate ev res where
  randomUpdate = update
  
instance (Arbitrary a, RandomUpdate c b) => RandomUpdate (a -> c) b where
  randomUpdate f = do
    a <- rand 10 arbitrary
    randomUpdate $ f a

-- Other functions
class RandomCallable a b where
  randomCall :: MonadIO m => a -> m b

instance RandomCallable (IO res) res where
  randomCall = liftIO

instance (Typeable res) => RandomCallable res res where
  randomCall = return 


instance (Arbitrary a, RandomCallable c b) => RandomCallable (a -> c) b where
  randomCall f = do
    a <- rand 10 arbitrary
    randomCall $ f a


instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g)
         => Arbitrary (a, b, c, d, e, f, g) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    g <- arbitrary
    return (a, b, c, d, e, f, g)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g, Arbitrary h) 
         => Arbitrary (a, b, c, d, e, f, g, h) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    g <- arbitrary
    h <- arbitrary
    return (a, b, c, d, e, f, g, h)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i, Arbitrary j) 
         => Arbitrary (a, b, c, d, e, f, g, h, i, j) where
  arbitrary = do
    (a, b, c, d, e, f, g, h) <- arbitrary
    (i, j) <- arbitrary
    return (a, b, c, d, e, f, g, h, i, j)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i) 
         => Arbitrary (a, b, c, d, e, f, g, h, i) where
  arbitrary = do
    (a, b, c, d, e, f, g, h) <- arbitrary
    i <- arbitrary
    return (a, b, c, d, e, f, g, h, i)

instance Arbitrary FileID where
  arbitrary = fmap FileID arbitrary
  
instance Arbitrary File where
  arbitrary = do
    (a, b, c) <- arbitrary
    return $ File { fileid = a
                  , filename = b
                  , filestorage = FileStorageMemory c
                  }

instance Arbitrary SignInfo where
  arbitrary = do
    (a, b) <- arbitrary
    return $ SignInfo a b

-- our asserts

assertSuccess :: MonadIO m => m ()
assertSuccess = assertBool "not success?!" True

assertJust :: MonadIO m => Maybe a -> m ()
assertJust (Just _) = assertSuccess
assertJust Nothing = assertFailure "Should have returned Just but returned Nothing"

assertRight :: (Show a, MonadIO m) => Either a b -> m ()
assertRight (Right _) = assertSuccess
assertRight (Left a) = assertFailure $ "Should have return Right but returned Left " ++ show a

assertLeft :: MonadIO m => Either a b -> m ()
assertLeft (Left _) = assertSuccess
assertLeft _ = assertFailure "Should have returned Left but returned Right"

assertNothing :: MonadIO m => Maybe a -> m ()
assertNothing Nothing = assertSuccess
assertNothing (Just _) = assertFailure "Should have returned Nothing but returned Just"

-- versions of assert types from Test.HUnit with typeclass constraint for convenience

assert :: (T.Assertable t, MonadIO m) => t -> m ()
assert = liftIO . T.assert

assertBool :: MonadIO m => String -> Bool -> m ()
assertBool msg = liftIO . T.assertBool msg

assertEqual :: (Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
assertEqual msg a = liftIO . T.assertEqual msg a

assertFailure :: MonadIO m => String -> m ()
assertFailure = liftIO . T.assertFailure

assertString :: MonadIO m => String -> m ()
assertString = liftIO . T.assertString

assertionPredicate :: (T.AssertionPredicable t, MonadIO m) => t -> m Bool
assertionPredicate = liftIO . T.assertionPredicate

-- other helpers

-- | Runs API function and returns its json response
testAPI :: (APIContext c, Kontrakcja m) => APIFunction m c APIResponse -> m APIResponse
testAPI f = do
    methodM POST
    mcontext <- apiContext
    case mcontext of
         Right apictx -> either (uncurry apiError) id <$> runApiFunction f apictx
         Left emsg -> return $ uncurry apiError emsg

-- | Checks type of flash message
isFlashOfType :: FlashMessage -> FlashType -> Bool
isFlashOfType (FlashMessage ft _) t = ft == t
isFlashOfType (FlashTemplate ft _ _) t = ft == t
