{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestingUtil where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad (unless)
import Control.Monad.Catch
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Data.Char
import Data.Maybe
import Data.Text (pack)
import Data.Time.Clock.POSIX
import Data.Typeable
import Data.Word
import Happstack.Server
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Text.JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Test.HUnit as T

import BrandedDomain.BrandedDomainID
import BrandedDomain.Model
import Company.Model
import Crypto.RNG
import DB
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad (withDocumentID)
import Doc.DocUtils
import Doc.Model
import Doc.SealStatus (SealStatus(..))
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import Doc.TestInvariants
import EID.CGI.GRP.Transaction.Model
import EID.Signature.Model
import File.FileID
import File.Model
import FlashMessage
import IPAddress
import qualified  KontraError as KontraError
import KontraMonad
import MagicHash (MagicHash, unsafeMagicHash)
import MinutesTime
import System.Random.CryptoRNG ()
import TestKontra
import User.Email
import User.Model
import Util.Actor
import Utils.Default
import qualified Log
import qualified Text.XML.Content as C
import qualified Text.XML.DirtyContent as D

newtype XMLChar = XMLChar { unXMLChar :: Char }
  deriving (Enum, Eq, Ord)

instance Show XMLChar where
  show = show . unXMLChar

instance Arbitrary XMLChar where
  arbitrary = elements (map XMLChar ("\n\r\t" ++ [' '..'~'] ++ ['\160'..'\255']))

instance Arbitrary C.XMLContent where
  arbitrary = C.cdata . pack . map unXMLChar <$> arbitrary

instance Arbitrary D.XMLContent where
  arbitrary = oneof [ D.CleanXMLContent <$> arbitrary
                    , D.DirtyXMLContent . pack <$> arbitrary
                    ]

newtype NotNullWord8 = NotNullWord8 { fromNNW8 :: Word8 }
  deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Show NotNullWord8 where
  show = show . fromNNW8

instance Bounded NotNullWord8 where
  minBound = 1
  maxBound = NotNullWord8 maxBound

newtype StringNoNUL = StringNoNUL { fromSNN :: String }

instance Arbitrary NotNullWord8 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink = shrinkIntegral

instance Arbitrary SignOrder where
  arbitrary = SignOrder <$> arbitrary

instance Arbitrary DocumentTag where
  arbitrary = DocumentTag <$> (fromSNN <$> arbitrary) <*> (fromSNN <$> arbitrary)

instance Arbitrary UserID where
  arbitrary = unsafeUserID . abs <$> arbitrary

instance Arbitrary Company where
  arbitrary = do
    a <- arbitrary
    d <- arbitrary
    return $ Company { companyid  = a
                     , companyinfo = d
                     }

instance Arbitrary CompanyID where
  arbitrary = unsafeCompanyID . abs <$> arbitrary

instance Arbitrary CompanyInfo where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    e <- arbitrary
    f <- arbitrary
    h <- arbitrary
    i <- arbitrary
    return $ CompanyInfo { companyname       = a
                         , companynumber     = b
                         , companyaddress    = c
                         , companyzip        = d
                         , companycity       = e
                         , companycountry    = f
                         , companyipaddressmasklist = []
                         , companyallowsavesafetycopy = True
                         , companyidledoctimeout = h
                         , companycgidisplayname = i
                         }

instance Arbitrary MagicHash where
  arbitrary = unsafeMagicHash <$> arbitrary

instance Arbitrary DeliveryStatus where
  arbitrary = elements [ Delivered
                       , Undelivered
                       , Unknown
                       , Deferred
                       ]

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . fromInteger <$> arbitrary

{- | Sometimes we get and object that is not as random as we would expect (from some reason)
     Like author signatorylink that by default does not have any fields attached
     This is a class to make it more random - so to attach this fields for example.
-}
class ExtendWithRandomnes a where
    moreRandom :: a -> Gen a
    extendRandomness :: a -> TestEnv a
    extendRandomness a = do
          stdgen <- random
          return $ unGen (moreRandom a) stdgen 10

arbitraryAuthorActor :: TestEnv Actor
arbitraryAuthorActor = do
  ctx <- mkContext defaultValue
  authorActor ctx <$> rand 10 arbitrary

arbitrarySystemActor :: (Functor m, CryptoRNG m) => m Actor
arbitrarySystemActor = systemActor <$> rand 10 arbitrary

arbitrarySignatoryActor :: TestEnv Actor
arbitrarySignatoryActor = do
  ctx <- mkContext defaultValue
  sl <- rand 10 arbitrary
  withDocumentID (unsafeDocumentID 0) $ signatoryActor ctx sl

instance Arbitrary SignatoryLinkID where
  arbitrary = unsafeSignatoryLinkID . abs <$> arbitrary

instance Arbitrary SignatoryLink where
  arbitrary = do
    mh <- arbitrary
    fields <- arbitrary
    seeninfo <- arbitrary
    signinfo <- if isJust seeninfo
                then arbitrary
                else return Nothing

    delivery <- arbitrary
    authentication <- arbitrary
    return $ defaultValue { signatorylinkid = unsafeSignatoryLinkID 0
                          , signatoryfields = fields
                          , signatoryisauthor = False
                          , signatoryispartner = True
                          , signatorysignorder = SignOrder 1
                          , signatorymagichash         = mh
                          , maybesigninfo              = signinfo
                          , maybeseeninfo              = seeninfo
                          , signatorylinkdeliverymethod = delivery
                          , signatorylinkauthenticationmethod = authentication
                          }

instance Arbitrary CSVUpload where
  arbitrary = do
    a <- arbitrary
    cols <- arbitrary
    rows <- arbitrary
    b <- vectorOf rows (vectorOf cols arbitrary)
    return $ CSVUpload { csvtitle = a
                       , csvcontents = b
                       }

instance Arbitrary DocumentID where
  arbitrary = unsafeDocumentID . abs <$> arbitrary

documentAllTypes :: [DocumentType]
documentAllTypes = [ Signable
                   , Template
                   ]

documentSignableTypes :: [DocumentType]
documentSignableTypes = [ Signable
                        ]

documentTemplateTypes :: [DocumentType]
documentTemplateTypes = [ Template
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
    -- we can have any days to sign. almost
    ddaystosign <- elements [1, 10, 99]
    dtimeouttime <- arbitrary
    return $ defaultValue  { documentstatus = dstatus
                           , documenttype = dtype
                           , documentsignatorylinks = sls
                           , documenttimeouttime = Just dtimeouttime
                           , documentdaystosign = ddaystosign
                           }

documentAllStatuses :: [DocumentStatus]
documentAllStatuses = [ Preparation
                      , Pending
                      , Closed
                      , Canceled
                      , Timedout
                      , Rejected
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

instance Arbitrary [SignatoryField] where
  arbitrary = do
    fn <- arbString 1 20
    ln <- arbString 1 20
    em <- arbEmail
    (f1,f2,f3,f4,f5) <-  arbitrary
    return $ filter (\f->notElem (sfType f) [FirstNameFT, LastNameFT, EmailFT]) (filterSingleFieldType [f1,f2,f3,f4,f5])
                                                  ++ [ SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT (TextField fn) True False []
                                                     , SignatoryField (unsafeSignatoryFieldID 0) LastNameFT  (TextField ln) True False []
                                                     , SignatoryField (unsafeSignatoryFieldID 0) EmailFT     (TextField em) True False []]

instance Arbitrary FieldPlacement where
  arbitrary = do  -- We loose precision with conversion, so please watch out for this
    (a :: Int) <- choose (1,1000)
    (b :: Int) <- choose (1,1000)
    (c :: Int) <- choose (1,1000)
    (d :: Int) <- choose (1,1000)
    (e :: Int) <- choose (1,1000)
    (x :: Int) <- choose (1, 10)
    f <- arbitrary
    return $ FieldPlacement { placementxrel       = fromIntegral a / fromIntegral x
                            , placementyrel       = fromIntegral b / fromIntegral x
                            , placementwrel       = fromIntegral c / fromIntegral x
                            , placementhrel       = fromIntegral d / fromIntegral x
                            , placementfsrel      = fromIntegral e / fromIntegral x
                            , placementpage       = f
                            , placementtipside    = Nothing
                            , placementanchors    = []
                            }

instance Arbitrary FieldType where
  arbitrary = do
    fieldlabel <- arbString 1 20
    filled <- arbitrary
    elements [FirstNameFT, LastNameFT, EmailFT, CompanyFT, CompanyNumberFT, PersonalNumberFT, CustomFT fieldlabel filled]

instance Arbitrary SignatoryField where
  arbitrary = do
    t <- arbitrary
    v <- arbString 1 100
    p <- arbitrary
    return $ SignatoryField { sfID = unsafeSignatoryFieldID 0
                            , sfType = t
                            , sfValue = case t of
                              SignatureFT{} -> BinaryField $ BSC.pack v
                              _             -> TextField v
                            , sfPlacements = p
                            , sfObligatory = True
                            , sfShouldBeFilledBySender = False
                            }

instance Arbitrary AuthenticationMethod where
  arbitrary = elements [StandardAuthentication, ELegAuthentication]

instance Arbitrary DeliveryMethod where
  arbitrary = elements [EmailDelivery, PadDelivery]

instance Arbitrary UserInfo where
  arbitrary = do
    fn <- arbitrary
    ln <- arbitrary
    pn <- arbitrary
    em <- arbEmail

    return $ UserInfo { userfstname     = fn
                      , usersndname     = ln
                      , userpersonalnumber  = pn
                      , usercompanyposition = []
                      , userphone           = []
                      , useremail           = Email em
                      }

instance Arbitrary Password where
  arbitrary = Password <$> arbitrary <*> arbitrary

instance Arbitrary (Binary BS.ByteString) where
  arbitrary = Binary <$> arbitrary

instance Arbitrary SignupMethod where
  arbitrary = elements [AccountRequest, ViralInvitation, BySigning, ByAdmin, CompanyInvitation]

instance Arbitrary UserSettings where
  arbitrary = UserSettings <$> arbitrary

instance Arbitrary User where
  arbitrary = User <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> pure (unsafeBrandedDomainID 0)

instance Arbitrary CgiGrpTransaction where
  arbitrary = CgiGrpTransaction
    <$> arbitrary
    <*> (T.pack . fromSNN <$> arbitrary)
    <*> (T.pack . fromSNN <$> arbitrary)
    <*> (T.pack . fromSNN <$> arbitrary)

instance Arbitrary BankIDSignature where
  arbitrary = BankIDSignature
    <$> (C.renderXMLContent <$> arbitrary)
    <*> (C.renderXMLContent <$> arbitrary)
    <*> (C.renderXMLContent <$> arbitrary)
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ESignature where
  arbitrary = BankIDSignature_ <$> arbitrary

-- generate (byte)strings without \NUL in them since
-- hdbc-postgresql plays around with these chars and
-- fucks them up
instance Arbitrary BS.ByteString where
  arbitrary = BS.pack . map fromNNW8 <$> arbitrary

instance Arbitrary StringNoNUL where
  arbitrary = StringNoNUL . map (chr . fromIntegral . fromNNW8) <$> arbitrary

arbString :: Int -> Int -> Gen String
arbString minl maxl = do
  l <- choose (minl, maxl)
  vectorOf l $ elements ['a'..'z']

arbEmail :: Gen String
arbEmail = do
  n <- arbString 1 34
  d <- arbString 3 7
  return $ n ++ "@" ++ d ++ ".com"

signatoryLinkExample1 :: SignatoryLink
signatoryLinkExample1 = defaultValue { signatorylinkid = unsafeSignatoryLinkID 0
                                      , signatorymagichash = unsafeMagicHash 0
                                      , maybesignatory = Nothing
                                      , maybesigninfo = Just $ SignInfo unixEpoch noIP
                                      , maybeseeninfo = Just $ SignInfo unixEpoch noIP
                                      , maybereadinvite = Nothing
                                      , mailinvitationdeliverystatus = Delivered
                                      , smsinvitationdeliverystatus = Delivered
                                      , signatorylinkdeleted = Nothing
                                      , signatorylinkreallydeleted = Nothing
                                      , signatoryisauthor = False
                                      , signatoryispartner = True
                                      , signatorysignorder = SignOrder 1
                                      , signatoryfields = [ SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT "Eric" True False []
                                                          , SignatoryField (unsafeSignatoryFieldID 0) LastNameFT "Normand" True False []
                                                          , SignatoryField (unsafeSignatoryFieldID 0) EmailFT "eric@scrive.com" True False []
                                                          , SignatoryField (unsafeSignatoryFieldID 0) CompanyFT "Scrive" True False []
                                                          , SignatoryField (unsafeSignatoryFieldID 0) CompanyNumberFT "1234" True False []
                                                          , SignatoryField (unsafeSignatoryFieldID 0) PersonalNumberFT "9101112" True False []
                                                          , SignatoryField (unsafeSignatoryFieldID 0) (CustomFT "phone" True) "504-302-3742" True False []
                                                          ]
                                      , signatorylinkcsvupload = Nothing
                                      , signatoryattachments   = []
                                      , signatorylinksignredirecturl = Nothing
                                      , signatorylinkrejectiontime = Nothing
                                      , signatorylinkrejectionreason = Nothing
                                      , signatorylinkauthenticationmethod = StandardAuthentication
                                      }

testThat :: String -> TestEnvSt -> TestEnv () -> Test
testThat s env = testCase s . runTestEnv env

addNewCompany :: TestEnv Company
addNewCompany = do
    Company{companyid = cid} <- dbUpdate $ CreateCompany
    companyname <- rand 10 $ arbString 3 30
    companynumber <- rand 10 $ arbString 3 30
    companyaddress <- rand 10 $ arbString 3 30
    companyzip <- rand 10 $ arbString 3 30
    companycity <- rand 10 $ arbString 3 30
    companycountry <- rand 10 $ arbString 3 30
    _ <- dbUpdate $ SetCompanyInfo cid $ CompanyInfo
         { companyname = companyname
         , companynumber = companynumber
         , companyaddress = companyaddress
         , companyzip = companyzip
         , companycity = companycity
         , companycountry = companycountry
         , companyipaddressmasklist = []
         , companyallowsavesafetycopy = True
         , companyidledoctimeout = Nothing
         , companycgidisplayname = Nothing
         }
    Just company <- dbQuery $ GetCompany cid
    return company

addNewFile :: (CryptoRNG m, MonadDB m, MonadThrow m) => String -> BS.ByteString -> m FileID
addNewFile filename content = dbUpdate $ NewFile filename $ Binary content

addNewRandomFile :: (CryptoRNG m, MonadDB m, MonadThrow m) => m FileID
addNewRandomFile = do
  fn <- rand 10 $ arbString 3 30
  cnt <- rand 10 $ arbString 3 30
  addNewFile fn (BS.fromString cnt)

addNewUser :: (MonadDB m, MonadThrow m, Log.MonadLog m) => String -> String -> String -> m (Maybe User)
addNewUser firstname secondname email = do
  bd <- dbQuery $ GetMainBrandedDomain
  company <- dbUpdate $ CreateCompany
  dbUpdate $ AddUser (firstname, secondname) email Nothing (companyid company,True) defaultValue (bdid bd)

addNewCompanyUser :: String -> String -> String -> CompanyID -> TestEnv (Maybe User)
addNewCompanyUser firstname secondname email cid = do
  bd <- dbQuery $ GetMainBrandedDomain
  dbUpdate $ AddUser (firstname, secondname) email Nothing (cid,True) defaultValue (bdid bd)

addNewRandomUser :: (CryptoRNG m, MonadDB m, MonadThrow m, Log.MonadLog m) => m User
addNewRandomUser = do
  fn <- rand 10 $ arbString 3 30
  ln <- rand 10 $ arbString 3 30
  em <- rand 10 arbEmail
  muser <- addNewUser fn ln em
  case muser of
    Just user -> do
      -- change the user to have some distinct personal information
      personal_number <- rand 10 $ arbString 3 30
      company_position <- rand 10 $ arbString 3 30
      phone <- rand 10 $ arbString 3 30
      let userinfo = UserInfo
                     { userfstname = fn
                     , usersndname = ln
                     , userpersonalnumber = personal_number
                     , usercompanyposition = company_position
                     , userphone = phone
                     , useremail = Email em
                     }
      _ <- dbUpdate $ SetUserInfo (userid user) userinfo
      return user
    Nothing -> do
      Log.mixlog_ "Could not create user, trying again."
      addNewRandomUser

addNewRandomCompanyUser :: CompanyID -> Bool -> TestEnv User
addNewRandomCompanyUser cid isadmin = do
  User{userid} <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany userid cid
  _ <- dbUpdate $ SetUserCompanyAdmin userid isadmin
  Just user <- dbQuery $ GetUserByID userid
  return user

data RandomDocumentAllows = RandomDocumentAllows
                          { randomDocumentAllowedTypes :: [DocumentType]
                          , randomDocumentAllowedStatuses :: [DocumentStatus]
                          , randomDocumentAuthor :: User
                          , randomDocumentCondition :: Document -> Bool
                          }

randomDocumentAllowsDefault :: User -> RandomDocumentAllows
randomDocumentAllowsDefault user = RandomDocumentAllows
                              { randomDocumentAllowedTypes = [ Signable , Template ]
                              , randomDocumentAllowedStatuses = [ Preparation
                                                                , Pending
                                                                , Closed
                                                                , Canceled
                                                                , Timedout
                                                                , Rejected
                                                                , DocumentError "Bad document."
                                                                ]
                              , randomDocumentAuthor = user
                              , randomDocumentCondition = const True
                              }

addRandomDocumentWithAuthor :: User -> TestEnv DocumentID
addRandomDocumentWithAuthor user = documentid <$> addRandomDocument (randomDocumentAllowsDefault user)


randomSigLinkByStatus :: DocumentStatus -> Gen SignatoryLink
randomSigLinkByStatus Closed = do
  (sl, sign, seen) <- arbitrary
  return $ sl{maybesigninfo = Just sign, maybeseeninfo = Just seen}
randomSigLinkByStatus Preparation = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing}
randomSigLinkByStatus Pending = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing}
randomSigLinkByStatus _ = arbitrary

randomAuthorLinkByStatus :: DocumentStatus -> Gen SignatoryLink
randomAuthorLinkByStatus Closed = do
  (sl, sign, seen) <- arbitrary
  return $ sl{maybesigninfo = Just sign, maybeseeninfo = Just seen, signatoryisauthor = True  }
randomAuthorLinkByStatus Preparation = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatoryisauthor = True  }
randomAuthorLinkByStatus Pending = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatoryisauthor = True  }
randomAuthorLinkByStatus _ = arbitrary

addRandomDocumentWithAuthorAndCondition :: User -> (Document -> Bool) -> TestEnv Document
addRandomDocumentWithAuthorAndCondition user p =
  addRandomDocument2 user (\x -> x { randomDocumentCondition = p})

addRandomDocument2 :: User -> (RandomDocumentAllows -> RandomDocumentAllows) -> TestEnv Document
addRandomDocument2 user refine =
  addRandomDocument (refine (randomDocumentAllowsDefault user))

addRandomDocumentWithAuthorAndConditionAndFile :: User -> (Document -> Bool) -> FileID -> TestEnv Document
addRandomDocumentWithAuthorAndConditionAndFile user p file =
  addRandomDocumentWithFile file ((randomDocumentAllowsDefault user) { randomDocumentCondition = p})

addRandomDocument :: RandomDocumentAllows -> TestEnv Document
addRandomDocument rda = do
  file <- addNewRandomFile
  addRandomDocumentWithFile file rda

addRandomDocumentWithFile :: FileID -> RandomDocumentAllows -> TestEnv Document
addRandomDocumentWithFile file rda = do
  now <- currentTime
  let user = randomDocumentAuthor rda
      p = randomDocumentCondition rda
  mcompany <-  dbQuery $ GetCompany $ usercompany user
  --liftIO $ print $ "about to generate document"
  document <- worker now user p mcompany
  docid <- dbUpdate $ StoreDocumentForTesting document
  dbQuery $ GetDocumentByDocumentID docid
  where
    worker now user p mcompany = do
      doc' <- rand 10 arbitrary
      xtype <- rand 10 (elements $ randomDocumentAllowedTypes rda)
      status <- rand 10 (elements $ randomDocumentAllowedStatuses rda)

      siglinks <- rand 10 (listOf $ randomSigLinkByStatus status)

      let doc = doc' { documenttype = xtype, documentstatus = status }

      partner <- rand 10 arbitrary
      asl' <- rand 10 $ randomAuthorLinkByStatus status
      userDetails <- signatoryFieldsFromUser user
      let asl = asl' {   maybesignatory = Just (userid user)
                       , signatoryfields = userDetails
                       , signatoryispartner = partner
                     }

      let alllinks = asl : siglinks


      let closedfile = if documentstatus doc == Closed
                       then [MainFile file Closed Missing]
                       else []
      let adoc = doc { documentsignatorylinks = alllinks
                     , documentlang = getLang user
                     , documentmainfiles = closedfile ++ [MainFile file Preparation Missing]
                     }
      case (p adoc, invariantProblems now adoc) of
        (True, Nothing) -> return adoc
        (False, _)  -> do
          rej <- asks teRejectedDocuments
          liftIO $ (atomically . modifyTVar' rej) (+1)
          --liftIO $ print $ "did not pass condition; doc: " ++ show adoc
          worker now user p mcompany

        (_, Just _problems) -> do
          rej <- asks teRejectedDocuments
          liftIO $ (atomically . modifyTVar' rej) (+1)
          -- am I right that random document should not have invariantProblems?
          --uncomment this to find out why the doc was rejected
          --print adoc
          --liftIO $ print $ "rejecting doc: " ++ _problems
          worker now user p mcompany


rand :: CryptoRNG m => Int -> Gen a -> m a
rand i a = do
  stdgn <- random
  return $ unGen a stdgn i

untilCondition :: (Monad m) => (b -> Bool) -> m b -> m b
untilCondition cond gen = do
  v <- gen
  if cond v then return v else untilCondition cond gen

addRandomDocumentWithAuthor' :: User -> TestEnv Document
addRandomDocumentWithAuthor' user = addRandomDocumentWithAuthorAndCondition user (\_ -> True)

doTimes :: (Monad m) => Int -> m () -> m ()
doTimes 0 _ = return ()
doTimes n a = do
  _ <- a
  doTimes (n - 1) a


-- Random gen

--Random query
class RandomQuery a b where
  randomQuery :: a -> TestEnv b

instance (DBQuery TestEnv ev res) => RandomQuery ev res where
  randomQuery = dbQuery

instance (Arbitrary a, RandomQuery c b) => RandomQuery (a -> c) b where
  randomQuery f = do
    a <- rand 10 arbitrary
    randomQuery $ f a

--Random update
class RandomUpdate a b m where
  randomUpdate :: a -> m b

instance (DBUpdate m ev res) => RandomUpdate ev res m where
  randomUpdate = dbUpdate

instance (CryptoRNG m, Arbitrary a, RandomUpdate c b m) => RandomUpdate (a -> c) b m where
  randomUpdate f = do
    a <- rand 10 arbitrary
    randomUpdate $ f a

-- Other functions
class RandomCallable a b where
  randomCall :: a -> TestEnv b

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

instance Arbitrary IPAddress where
  arbitrary = unsafeIPAddress <$> arbitrary

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

-- TODO: For some reason InternalError gets packed into DBException. And I have no way of unpacking it - cast dbeError doesn't work
assertRaisesInternalError :: (Show v, MonadIO m, MonadMask m) =>  m v -> m ()
assertRaisesInternalError a = assertRaisesDBException a

assertRaisesDBException :: (Show v, MonadIO m, MonadMask m) =>  m v -> m ()
assertRaisesDBException a = (a >>= (\v -> assertFailure $ "Expecting db exception but got " ++ show v))  `catches` [
          Handler $ \_e@DBException{..} -> return ()
        ]


assertRaisesKontra :: forall e v m. (KontraException e, Show v, MonadIO m, MonadMask m)
             => (e -> Bool) -> m v -> m ()
assertRaisesKontra correctException action =
  (action >>= \r -> assertString $ "Expected KontraException " ++ typeOfE ++ ", instead returned result " ++ show r) `catches` [
    Handler helper
  -- support also KontraException nested within DBException
  , Handler $ \e@DBException{..} -> case cast dbeError of
    Just e' -> helper e'
    Nothing -> invExc e
  ]
  where
    helper (SomeKontraException e) = case cast e of
      Just e' -> if correctException e'
        then return ()
        else assertString $ "KontraException " ++ typeOfE ++ " is not correct " ++ show e'
      Nothing -> invExc e

    invExc :: (Show a, Typeable a) => a -> m ()
    invExc e = assertString $ "Expected KontraException " ++ typeOfE ++ ", instead got exception " ++ show e

    typeOfE = show $ typeOf (undefined :: e)

-- other helpers

guardMethodM :: Kontrakcja m => Method -> m ()
guardMethodM m = do
  rq <- askRq
  unless (rqMethod rq == m) KontraError.internalError

-- | Checks type of flash message
isFlashOfType :: FlashMessage -> FlashType -> Bool
isFlashOfType (FlashMessage ft _) t = ft == t

getFlashType :: FlashMessage -> FlashType
getFlashType (FlashMessage ft _) = ft

instance Arbitrary Lang where
  arbitrary = elements [LANG_SV, LANG_EN]
