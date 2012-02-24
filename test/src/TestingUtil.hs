{-# LANGUAGE OverlappingInstances, IncoherentInstances, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestingUtil where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)

import Control.Applicative
import Data.Char
import Data.Word
import System.Random (newStdGen)
import Test.QuickCheck
import Happstack.Server
import Doc.DocUtils
import Test.QuickCheck.Gen
import Control.Monad.Trans
import Data.Maybe
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Test.HUnit as T

import File.FileID
import API.API
import DB.Classes
import MagicHash (MagicHash, unsafeMagicHash)
import Company.Model
import FlashMessage
import qualified Log
import StateHelper
import Doc.Model
import Doc.DocStateData
import Doc.DocStateCommon
import KontraMonad
import MinutesTime
import User.Model
import Misc
import File.Model
import API.Service.Model
import Data.Typeable
import Doc.Invariants
--import Doc.DocInfo
import Doc.DocProcess
import ActionSchedulerState
import Text.JSON

--import Util.SignatoryLinkUtils

import EvidenceLog.Model

newtype NotNullWord8 = NotNullWord8 { fromNNW8 :: Word8 }
  deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Show NotNullWord8 where
  show = show . fromNNW8

instance Bounded NotNullWord8 where
  minBound = 1
  maxBound = NotNullWord8 maxBound

instance Arbitrary NotNullWord8 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink = shrinkIntegral

newtype StringWithoutNUL = StringWithoutNUL { fromSNN :: String }

instance Arbitrary SignOrder where
  arbitrary = SignOrder <$> arbitrary

instance Arbitrary DocumentTag where
  arbitrary = DocumentTag <$> arbitrary <*> arbitrary

instance Arbitrary UserID where
  arbitrary = unsafeUserID . abs <$> arbitrary

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
                     , companyui = emptyCompanyUI
                     }
    where
      emptyCompanyUI = CompanyUI {
        companybarsbackground = Nothing
      , companylogo = Nothing
      }

instance Arbitrary CompanyID where
  arbitrary = unsafeCompanyID . abs <$> arbitrary

instance Arbitrary ExternalCompanyID where
  arbitrary = ExternalCompanyID <$> arbitrary

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
  arbitrary = ServiceID <$> arbitrary

instance Arbitrary MagicHash where
  arbitrary = unsafeMagicHash <$> arbitrary

instance Arbitrary MailsDeliveryStatus where
  arbitrary = elements [ Delivered
                       , Undelivered
                       , Unknown
                       , Deferred
                       ]

instance Arbitrary TimeoutTime where
  arbitrary = TimeoutTime <$> arbitrary

instance Arbitrary MinutesTime where
  arbitrary = fromSeconds <$> arbitrary

instance Arbitrary DocumentUI where
  arbitrary = DocumentUI <$> arbitrary

instance Arbitrary ActionID where
  arbitrary = ActionID <$> arbitrary

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

instance Arbitrary AuthorActor where
  arbitrary = do
    (time, ip, uid, eml) <- arbitrary
    return $ AuthorActor time ip uid eml

instance Arbitrary SystemActor where
  arbitrary = do
    time <- arbitrary
    return $ SystemActor time

instance Arbitrary SignatoryActor where
  arbitrary = do
    (time, ip, uid, eml, slid) <- arbitrary
    return $ SignatoryActor time ip uid eml slid

instance Arbitrary MailAPIActor where
  arbitrary = do
    (time, uid, eml) <- arbitrary
    return $ MailAPIActor time uid eml

instance Arbitrary SignatoryLinkID where
  arbitrary = unsafeSignatoryLinkID . abs <$> arbitrary

instance Arbitrary SignatoryLink where
  arbitrary = do
    (slid, sd, mh) <- arbitrary
    seeninfo <- arbitrary
    signinfo <- if isJust seeninfo
                then arbitrary
                else return Nothing
    return $ SignatoryLink { signatorylinkid            = slid
                           , signatorydetails           = sd
                           , signatorymagichash         = mh
                           , maybesignatory             = Nothing
                           , maybesupervisor            = Nothing
                           , maybecompany               = Nothing
                           , maybesigninfo              = signinfo
                           , maybeseeninfo              = seeninfo
                           , maybereadinvite            = Nothing
                           , invitationdeliverystatus   = Unknown
                           , signatorysignatureinfo     = Nothing
                           , signatoryroles             = []
                           , signatorylinkdeleted       = False
                           , signatorylinkreallydeleted = False
                           , signatorylinkcsvupload     = Nothing
                           , signatoryattachments       = []
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
  arbitrary = unsafeDocumentID . abs <$> arbitrary

documentAllTypes :: [DocumentType]
documentAllTypes = [ Signable Contract
                   , Signable Order
                   , Signable Offer
                   , Template Contract
                   , Template Order
                   , Template Offer
                   , Attachment
                   , AttachmentTemplate
                   ]

documentSignableTypes :: [DocumentType]
documentSignableTypes = [ Signable Contract
                        , Signable Order
                        , Signable Offer
                        ]

documentTemplateTypes :: [DocumentType]
documentTemplateTypes = [ Template Contract
                        , Template Order
                        , Template Offer
                        ]

instance Arbitrary DocumentType where
  arbitrary = elements documentAllTypes


instance Arbitrary Document where
  arbitrary = do
    -- we can have any document type here
    dtype <- arbitrary
    -- status has meaning only for signables
    dstatus <- if isSignable dtype
               then arbitrary
               else return Preparation
    sls <- arbitrary
    ids <- arbitrary
    fnc <- arbitrary
    -- we can have any days to sign. almost
    ddaystosign <- elements [Nothing, Just 1, Just 10, Just 99]
    -- document timeout time makes sense only when days to sign was set for this document
    dtimeouttime <- if isJust ddaystosign
                    then arbitrary
                    else return Nothing
    return $ blankDocument { documentstatus = dstatus
                           , documenttype = dtype
                           , documentsignatorylinks = sls
                           , documentallowedidtypes = [ids]
                           , documentfunctionality = fnc
                           , documenttimeouttime = TimeoutTime <$> dtimeouttime
                           , documentdaystosign = ddaystosign
                           }

documentAllStatuses :: [DocumentStatus]
documentAllStatuses = [ Preparation
                      , Pending
                      , Closed
                      , Canceled
                      , Timedout
                      , Rejected
                      , AwaitingAuthor
                      , DocumentError "Bad document."
                      ]

instance Arbitrary DocumentStatus where
  arbitrary = elements documentAllStatuses

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
                      , usercompanyposition = BS.pack []
                      , userphone           = BS.pack []
                      , usermobile          = BS.pack []
                      , useremail           = Email em
                      }

-- generate (byte)strings without \NUL in them since
-- hdbc-postgresql plays around with these chars and
-- fucks them up
instance Arbitrary BS.ByteString where
  arbitrary = BS.pack . map fromNNW8 <$> arbitrary

instance Arbitrary StringWithoutNUL where
  arbitrary = StringWithoutNUL . map (chr . fromIntegral . fromNNW8) <$> arbitrary

arbString :: Int -> Int -> Gen String
arbString minl maxl = do
  l <- choose (minl, maxl)
  vectorOf l $ elements ['a'..'z']

arbEmail :: Gen BS.ByteString
arbEmail = do
  n <- arbString 1 34
  d <- arbString 3 7
  return $ BS.fromString (n ++ "@" ++ d ++ ".com")

signatoryLinkExample1 :: SignatoryLink
signatoryLinkExample1 = SignatoryLink { signatorylinkid = unsafeSignatoryLinkID 0
                                      , signatorymagichash = unsafeMagicHash 0
                                      , maybesignatory = Nothing
                                      , maybesupervisor = Nothing
                                      , maybecompany = Nothing
                                      , maybesigninfo = Just $ SignInfo (fromSeconds 0) unknownIPAddress
                                      , maybeseeninfo = Just $ SignInfo (fromSeconds 0) unknownIPAddress
                                      , maybereadinvite = Nothing
                                      , invitationdeliverystatus = Delivered
                                      , signatorysignatureinfo = Nothing
                                      , signatoryroles = [SignatoryPartner]
                                      , signatorylinkdeleted = False
                                      , signatorylinkreallydeleted = False
                                      , signatorydetails = SignatoryDetails { signatorysignorder = SignOrder 1,
                                                                              signatoryfields = [SignatoryField FirstNameFT (BS.fromString "Eric") [],
                                                                                                 SignatoryField LastNameFT (BS.fromString "Normand") [],
                                                                                                 SignatoryField EmailFT (BS.fromString "eric@scrive.com") [],
                                                                                                 SignatoryField CompanyFT (BS.fromString "Scrive") [],
                                                                                                 SignatoryField CompanyNumberFT (BS.fromString "1234") [],
                                                                                                 SignatoryField PersonalNumberFT (BS.fromString "9101112") [],
                                                                                                 SignatoryField (CustomFT (BS.fromString "phone") True) (BS.fromString "504-302-3742") []

                                                                                                ]

                                                                            }
                                      , signatorylinkcsvupload = Nothing
                                      , signatoryattachments   = []
                                      }

blankUser :: User
blankUser = User { userid                        = unsafeUserID 0
                 , userpassword                  = Nothing
                 , useriscompanyadmin            = False
                 , useraccountsuspended          = False
                 , userhasacceptedtermsofservice = Nothing
                 , usersignupmethod              = AccountRequest
                 , userinfo = UserInfo { userfstname = BS.empty
                                       , usersndname = BS.empty
                                       , userpersonalnumber = BS.empty
                                       , usercompanyposition =  BS.empty
                                       , userphone = BS.empty
                                       , usermobile = BS.empty
                                       , useremail = Email BS.empty
                                       }
                 , usersettings  = UserSettings { preferreddesignmode = Nothing
                                                , locale = mkLocaleFromRegion Misc.defaultValue
                                                , customfooter = Nothing
                                                }
                 , userservice = Nothing
                 , usercompany = Nothing
                 }

{-
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
          -- , documenttrustweaverreference = Nothing
          , documentallowedidtypes       = [EmailIdentification]
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
          -- , documentattachments          = []
          , documentregion               = REGION_SE
          }

-}

testThat :: String -> DBEnv -> DB () -> Test
testThat s env a = testCase s (withTestEnvironment env a)

addNewCompany ::  DB Company
addNewCompany = do
    eid <- rand 10 arbitrary
    dbUpdate $ CreateCompany Nothing eid

addNewFile :: String -> BS.ByteString -> DB File
addNewFile filename content =
  dbUpdate $ NewFile (BS.fromString filename) content

addNewRandomFile :: DB File
addNewRandomFile = do
  fn <- rand 10 $ arbString 3 30
  cnt <- rand 10 $ arbString 3 30
  addNewFile fn (BS.fromString cnt)

addNewUser :: String -> String -> String -> DB (Maybe User)
addNewUser firstname secondname email =
  dbUpdate $ AddUser (BS.fromString firstname, BS.fromString secondname) (BS.fromString email) Nothing False Nothing Nothing (mkLocaleFromRegion defaultValue)

addNewCompanyUser :: String -> String -> String -> CompanyID -> DB (Maybe User)
addNewCompanyUser firstname secondname email cid =
  dbUpdate $ AddUser (BS.fromString firstname, BS.fromString secondname) (BS.fromString email) Nothing False Nothing (Just cid) (mkLocaleFromRegion defaultValue)

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

addNewRandomCompanyUser :: CompanyID -> Bool -> DB User
addNewRandomCompanyUser cid isadmin = do
  User{userid} <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany userid (Just cid)
  _ <- dbUpdate $ SetUserCompanyAdmin userid isadmin
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

data RandomDocumentAllows = RandomDocumentAllows
                          { randomDocumentAllowedTypes :: [DocumentType]
                          , randomDocumentAllowedStatuses :: [DocumentStatus]
                          , randomDocumentAuthor :: User
                          , randomDocumentCondition :: Document -> Bool
                          }

randomDocumentAllowsDefault :: User -> RandomDocumentAllows
randomDocumentAllowsDefault user = RandomDocumentAllows
                              { randomDocumentAllowedTypes = [ Signable Contract
                                                             , Signable Order
                                                             , Signable Offer
                                                             , Template Contract
                                                             , Template Order
                                                             , Template Offer
                                                             , Attachment
                                                             ]
                              , randomDocumentAllowedStatuses = [ Preparation
                                                                , Pending
                                                                , Closed
                                                                , Canceled
                                                                , Timedout
                                                                , Rejected
                                                                , AwaitingAuthor
                                                                , DocumentError "Bad document."
                                                                ]
                              , randomDocumentAuthor = user
                              , randomDocumentCondition = const True
                              }

addRandomDocumentWithAuthor :: User -> DB DocumentID
addRandomDocumentWithAuthor user = documentid <$> addRandomDocument (randomDocumentAllowsDefault user)


randomSigLinkByStatus :: DocumentStatus -> Gen SignatoryLink
randomSigLinkByStatus Closed = do
  (sl, sign, seen) <- arbitrary
  return $ sl{maybesigninfo = Just sign, maybeseeninfo = Just seen, signatoryroles=[SignatoryPartner]}
randomSigLinkByStatus Preparation = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatoryroles=[SignatoryPartner]}
randomSigLinkByStatus Pending = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatoryroles=[SignatoryPartner]}
randomSigLinkByStatus AwaitingAuthor = do
  (sl,sign,seen) <- arbitrary
  return $ sl{maybesigninfo = Just sign, maybeseeninfo = Just seen, signatoryroles=[SignatoryPartner]}
randomSigLinkByStatus _ = arbitrary

randomAuthorLinkByStatus :: DocumentStatus -> Gen SignatoryLink
randomAuthorLinkByStatus Closed = do
  (sl, sign, seen) <- arbitrary
  return $ sl{maybesigninfo = Just sign, maybeseeninfo = Just seen, signatoryroles=[SignatoryAuthor]}
randomAuthorLinkByStatus Preparation = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatoryroles=[SignatoryAuthor]}
randomAuthorLinkByStatus Pending = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatoryroles=[SignatoryAuthor]}
randomAuthorLinkByStatus AwaitingAuthor = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatoryroles=[SignatoryAuthor]}
randomAuthorLinkByStatus _ = arbitrary


getRandomAuthorRoles :: MonadIO m => Document -> m [SignatoryRole]
getRandomAuthorRoles doc =
  rand 10000 (elements $ getPossibleAuthorRoles doc)

getPossibleAuthorRoles :: Document -> [[SignatoryRole]]
getPossibleAuthorRoles doc = [SignatoryAuthor] :
  case getValueForProcess doc processauthorsend of
    Just True -> [[SignatoryAuthor]]
    _ ->  [[SignatoryAuthor, SignatoryPartner], [SignatoryPartner, SignatoryAuthor]]

addRandomDocumentWithAuthorAndCondition :: User -> (Document -> Bool) -> DB Document
addRandomDocumentWithAuthorAndCondition user p =
  addRandomDocument ((randomDocumentAllowsDefault user) { randomDocumentCondition = p})

addRandomDocument :: RandomDocumentAllows -> DB Document
addRandomDocument rda = do
  file <- addNewRandomFile
  now <- liftIO getMinutesTime
  let user = randomDocumentAuthor rda
      p = randomDocumentCondition rda
  mcompany <- case usercompany user of
    Nothing  -> return Nothing
    Just cid -> dbQuery $ GetCompany cid
  --liftIO $ print $ "about to generate document"
  document <- liftIO $ worker file now user p mcompany
  docid <- dbUpdate $ StoreDocumentForTesting document
  mdoc  <- dbQuery  $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing -> do
      assertFailure "Could not store document."
      return document
    Just doc' -> do
              return doc'
  where
    worker :: File -> MinutesTime -> User -> (Document -> Bool) -> (Maybe Company) -> IO Document
    worker file now user p _mcompany = do
      doc' <- rand 10 arbitrary
      xtype <- rand 10 (elements $ randomDocumentAllowedTypes rda)
      status <- rand 10 (elements $ randomDocumentAllowedStatuses rda)

      siglinks <- rand 10 (listOf $ randomSigLinkByStatus status)

      let doc = doc' { documenttype = xtype, documentstatus = status }

      roles <- getRandomAuthorRoles doc
      asl' <- rand 10 $ randomAuthorLinkByStatus status
      let asl = asl' { maybesignatory = Just (userid user)
                     , maybecompany = usercompany user
                     , signatoryroles = roles
                     }

      let alllinks = asl : siglinks


      let adoc = doc { documentsignatorylinks = alllinks
                     , documentregion = getRegion user
                     , documentfiles = [fileid file]
                     }
      case (p adoc, invariantProblems now adoc) of
        (True, Nothing) -> return adoc
        (False, _)  -> do
          --liftIO $ print $ "did not pass condition; doc: " ++ show adoc
          worker file now user p _mcompany

        (_, Just _problems) -> do
               -- am I right that random document should not have invariantProblems?
               --uncomment this to find out why the doc was rejected
               --print adoc
               --liftIO $ print $ "rejecting doc: " ++ _problems
               worker file now user p _mcompany
      --asl <- dbUpdate $ SignLinkFromDetailsForTest asd roles


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

doNTimes :: (Monad m) => Int -> m () -> m ()
doNTimes 0 _ = return ()
doNTimes n a = do
  _ <- a
  doNTimes (n - 1) a

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
  randomQuery :: a -> DB b

instance (DBQuery ev res) => RandomQuery ev res where
  randomQuery = dbQuery

instance (Arbitrary a, RandomQuery c b) => RandomQuery (a -> c) b where
  randomQuery f = do
    a <- rand 10 arbitrary
    randomQuery $ f a

--Random update
class RandomUpdate a b where
  randomUpdate :: a -> DB b

instance (DBUpdate ev res) => RandomUpdate ev res where
  randomUpdate = dbUpdate

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
  arbitrary = unsafeFileID . abs <$> arbitrary

instance Arbitrary File where
  arbitrary = do
    (a, b, c) <- arbitrary
    return $ File { fileid = a
                  , filename = b
                  , filestorage = FileStorageMemory c
                  }

instance Arbitrary IPAddress where
  arbitrary = fmap IPAddress arbitrary

instance Arbitrary SignInfo where
  arbitrary = SignInfo <$> arbitrary <*> arbitrary


instance Arbitrary JSValue where
  arbitrary =
    oneof [ JSObject <$> toJSObject <$> (\(f,s) -> (maybeToList f) ++ (maybeToList s)) <$> arbitrary
          , JSArray <$> (\(f,s) -> (maybeToList f) ++ (maybeToList s)) <$> arbitrary
          , JSRational True <$> toRational <$> (arbitrary :: Gen Integer)
          , JSBool <$> arbitrary
          , JSString <$> toJSString <$> arbitrary
          ]


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

getFlashType :: FlashMessage -> FlashType
getFlashType (FlashMessage ft _) = ft
getFlashType (FlashTemplate ft _ _) = ft

instance Arbitrary Locale where
  arbitrary = do
    (a, b) <- arbitrary
    return $ mkLocale a b

instance Arbitrary Region where
  arbitrary = elements [REGION_SE, REGION_GB]

instance Arbitrary Lang where
  arbitrary = elements [LANG_SE, LANG_EN]
