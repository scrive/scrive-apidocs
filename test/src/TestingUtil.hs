{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestingUtil where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)

import Control.Applicative
import Data.Char
import Data.Word
import Test.QuickCheck
import Happstack.Server
import Doc.DocUtils
import Test.QuickCheck.Gen
import Control.Monad (unless)
import Control.Monad.Trans
import Data.Maybe
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Test.HUnit as T

import File.FileID
import Crypto.RNG
import DB
import MagicHash (MagicHash, unsafeMagicHash)
import Company.Model
import FlashMessage
import qualified Log
import Doc.Model
import Doc.DocStateData
import Doc.DocStateCommon
import ELegitimation.BankIDRequests
import ELegitimation.ELegTransaction.Model
import KontraError (internalError)
import KontraMonad
import MinutesTime
import User.Model
import Utils.Default
import IPAddress
import File.Model
import Data.Typeable
import Doc.Invariants
import System.Random.CryptoRNG ()
import Text.JSON
import TestKontra

import Util.Actor

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
    b <- arbitrary
    d <- arbitrary
    return $ Company { companyid  = a
                     , companyexternalid = b
                     , companyinfo = d
                     , companyui = emptyCompanyUI
                     }
    where
      emptyCompanyUI = CompanyUI {
        companybarsbackground = Nothing
      , companybarstextcolour = Nothing
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
                         , companyemaildomain = Nothing
                         }

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

instance ExtendWithRandomnes SignatoryDetails where
    moreRandom sl = return sl

newtype AuthorActor    = AuthorActor    { unAuthorActor    :: Actor }
newtype SystemActor    = SystemActor    { unSystemActor    :: Actor }
newtype SignatoryActor = SignatoryActor { unSignatoryActor :: Actor }
newtype MailAPIActor   = MailAPIActor   { unMailAPIActor   :: Actor }

instance Arbitrary AuthorActor where
  arbitrary = do
    (time, ip, uid, eml) <- arbitrary
    return $ AuthorActor $ authorActor time ip uid eml

instance Arbitrary SystemActor where
  arbitrary = do
    time <- arbitrary
    return $ SystemActor $ systemActor time

instance Arbitrary SignatoryActor where
  arbitrary = do
    (time, ip, uid, eml, slid) <- arbitrary
    return $ SignatoryActor $ signatoryActor time ip uid eml slid

instance Arbitrary MailAPIActor where
  arbitrary = do
    (time, uid, eml) <- arbitrary
    return $ MailAPIActor $ mailAPIActor time uid eml

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
                           , maybecompany               = Nothing
                           , maybesigninfo              = signinfo
                           , maybeseeninfo              = seeninfo
                           , maybereadinvite            = Nothing
                           , invitationdeliverystatus   = Unknown
                           , signatorysignatureinfo     = Nothing
                           , signatorylinkdeleted       = False
                           , signatorylinkreallydeleted = False
                           , signatorylinkcsvupload     = Nothing
                           , signatoryattachments       = []
                           , signatorylinkstatusclass   = SCDraft
                           , signatorylinksignredirecturl = Nothing
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
    h <- arbitrary
    return $ SignatureInfo { signatureinfotext        = a
                           , signatureinfosignature   = b
                           , signatureinfocertificate = c
                           , signatureinfoprovider    = d
                           , signaturefstnameverified = e
                           , signaturelstnameverified = f
                           , signaturepersnumverified = g
                           , signatureinfoocspresponse = h
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

instance Arbitrary DocumentProcess where
  arbitrary = elements [Contract, Order, Offer]

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
    auth <- arbitrary
    delivery <- arbitrary
    -- we can have any days to sign. almost
    ddaystosign <- elements [1, 10, 99]
    dtimeouttime <- arbitrary
    return $ blankDocument { documentstatus = dstatus
                           , documenttype = dtype
                           , documentsignatorylinks = sls
                           , documentauthenticationmethod = auth
                           , documentdeliverymethod = delivery
                           , documenttimeouttime = Just (TimeoutTime dtimeouttime)
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

instance Arbitrary SignatoryDetails where
  arbitrary = do
    fn <- arbString 1 20
    ln <- arbString 1 20
    em <- arbEmail
    fields <- filterSingleFieldType <$> arbitrary
    return $ SignatoryDetails { signatorysignorder = SignOrder 1
                              , signatoryisauthor = False
                              , signatoryispartner = True
                              , signatoryfields = filter (\f->notElem (sfType f) [FirstNameFT, LastNameFT, EmailFT]) fields
                                                  ++ [ SignatoryField FirstNameFT fn []
                                                     , SignatoryField LastNameFT  ln []
                                                     , SignatoryField EmailFT     em []]}

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
    return $ SignatoryField { sfType = t
                            , sfValue = v
                            , sfPlacements = p
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
                      , usermobile          = []
                      , useremail           = Email em
                      , usercompanyname = []
                      , usercompanynumber = []
                      }

instance Arbitrary CollectResponse where
  arbitrary = oneof [outstanding, usersign, complete]
    where
      outstanding = do
        tid <- arbitrary
        return $ CROutstanding tid
      usersign = do
        tid <- arbitrary
        return $ CRUserSign tid
      complete = do
        tid <- arbitrary
        sig <- arbitrary
        attrs <- arbitrary
        return $ CRComplete tid sig attrs

instance Arbitrary ELegTransaction where
  arbitrary = do
    tid <- arbitrary
    nonce <- arbitrary
    tbs <- arbitrary
    encodedtbs <- arbitrary
    token <- arbitrary
    status <- arbitrary
    ref <- arbitrary
    return ELegTransaction {
        transactiontransactionid = tid
      , transactionnonce = nonce
      , transactiontbs = tbs
      , transactionencodedtbs = encodedtbs
      , transactionsignatorylinkid = Nothing
      , transactiondocumentid = unsafeDocumentID 0
      , transactionmagichash = token
      , transactionstatus = status
      , transactionoref = ref
    }

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
signatoryLinkExample1 = SignatoryLink { signatorylinkid = unsafeSignatoryLinkID 0
                                      , signatorymagichash = unsafeMagicHash 0
                                      , maybesignatory = Nothing
                                      , maybecompany = Nothing
                                      , maybesigninfo = Just $ SignInfo (fromSeconds 0) noIP
                                      , maybeseeninfo = Just $ SignInfo (fromSeconds 0) noIP
                                      , maybereadinvite = Nothing
                                      , invitationdeliverystatus = Delivered
                                      , signatorysignatureinfo = Nothing
                                      , signatorylinkdeleted = False
                                      , signatorylinkreallydeleted = False
                                      , signatorydetails = SignatoryDetails {
                                      signatoryisauthor = False,
                                      signatoryispartner = True,
                                      signatorysignorder = SignOrder 1,
                                                                              signatoryfields = [SignatoryField FirstNameFT "Eric" [],
                                                                                                 SignatoryField LastNameFT "Normand" [],
                                                                                                 SignatoryField EmailFT "eric@scrive.com" [],
                                                                                                 SignatoryField CompanyFT "Scrive" [],
                                                                                                 SignatoryField CompanyNumberFT "1234" [],
                                                                                                 SignatoryField PersonalNumberFT "9101112" [],
                                                                                                 SignatoryField (CustomFT "phone" True) "504-302-3742" []

                                                                                                ]

                                                                            }
                                      , signatorylinkcsvupload = Nothing
                                      , signatoryattachments   = []
                                      , signatorylinkstatusclass = SCDraft
                                      , signatorylinksignredirecturl = Nothing
                                      }

blankUser :: User
blankUser = User { userid                        = unsafeUserID 0
                 , userpassword                  = Nothing
                 , useriscompanyadmin            = False
                 , useraccountsuspended          = False
                 , userhasacceptedtermsofservice = Nothing
                 , usersignupmethod              = AccountRequest
                 , userinfo = UserInfo { userfstname = []
                                       , usersndname = []
                                       , userpersonalnumber = []
                                       , usercompanyposition =  []
                                       , userphone = []
                                       , usermobile = []
                                       , useremail = Email []
                                       , usercompanyname = []
                                       , usercompanynumber = []
                                       }
                 , usersettings  = UserSettings { lang = defaultValue
                                                , customfooter = Nothing
                                                }
                 , usercompany = Nothing
                 , userisfree  = False
                 }

testThat :: String -> TestEnvSt -> TestEnv () -> Test
testThat s env = testCase s . runTestEnv env

addNewCompany :: TestEnv Company
addNewCompany = do
    eid <- rand 10 arbitrary
    Company{companyid = cid} <- dbUpdate $ CreateCompany eid
    companyname <- rand 10 $ arbString 3 30
    companynumber <- rand 10 $ arbString 3 30
    companyaddress <- rand 10 $ arbString 3 30
    companyzip <- rand 10 $ arbString 3 30
    companycity <- rand 10 $ arbString 3 30
    companycountry <- rand 10 $ arbString 3 30
    companyemaildomain <- rand 10 $ arbString 3 30
    _ <- dbUpdate $ SetCompanyInfo cid $ CompanyInfo
         { companyname = companyname
         , companynumber = companynumber
         , companyaddress = companyaddress
         , companyzip = companyzip
         , companycity = companycity
         , companycountry = companycountry
         , companyemaildomain = Just companyemaildomain
         }
    Just company <- dbQuery $ GetCompany cid
    return company

addNewFile :: String -> BS.ByteString -> TestEnv File
addNewFile filename content = dbUpdate $ NewFile filename $ Binary content

addNewRandomFile :: TestEnv File
addNewRandomFile = do
  fn <- rand 10 $ arbString 3 30
  cnt <- rand 10 $ arbString 3 30
  addNewFile fn (BS.fromString cnt)

addNewUser :: String -> String -> String -> TestEnv (Maybe User)
addNewUser firstname secondname email =
  dbUpdate $ AddUser (firstname, secondname) email Nothing Nothing defaultValue

addNewCompanyUser :: String -> String -> String -> CompanyID -> TestEnv (Maybe User)
addNewCompanyUser firstname secondname email cid =
  dbUpdate $ AddUser (firstname, secondname) email Nothing (Just cid) defaultValue

addNewRandomUser :: TestEnv User
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
      mobile <- rand 10 $ arbString 3 30
      company_name <- rand 10 $ arbString 3 30
      company_number <- rand 10 $ arbString 3 30
      let userinfo = UserInfo
                     { userfstname = fn
                     , usersndname = ln
                     , userpersonalnumber = personal_number
                     , usercompanyposition = company_position
                     , userphone = phone
                     , usermobile = mobile
                     , useremail = Email em
                     , usercompanyname  = company_name
                     , usercompanynumber = company_number
                     }
      _ <- dbUpdate $ SetUserInfo (userid user) userinfo
      return user
    Nothing -> do
      Log.debug "Could not create user, trying again."
      addNewRandomUser

addNewRandomCompanyUser :: CompanyID -> Bool -> TestEnv User
addNewRandomCompanyUser cid isadmin = do
  User{userid} <- addNewRandomUser
  _ <- dbUpdate $ SetUserCompany userid (Just cid)
  _ <- dbUpdate $ SetUserCompanyAdmin userid isadmin
  Just user <- dbQuery $ GetUserByID userid
  return user

emptySignatoryDetails :: SignatoryDetails
emptySignatoryDetails = SignatoryDetails
    { signatoryfields = []
    , signatorysignorder = SignOrder 1
    , signatoryisauthor = False
    , signatoryispartner = False
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
                                                             ]
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
  return $ sl{maybesigninfo = Just sign, maybeseeninfo = Just seen, signatorydetails = (signatorydetails sl) { signatoryisauthor = True } }
randomAuthorLinkByStatus Preparation = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatorydetails = (signatorydetails sl) { signatoryisauthor = True } }
randomAuthorLinkByStatus Pending = do
  (sl) <- arbitrary
  return $ sl{maybesigninfo = Nothing, maybeseeninfo = Nothing, signatorydetails = (signatorydetails sl) { signatoryisauthor = True } }
randomAuthorLinkByStatus _ = arbitrary

addRandomDocumentWithAuthorAndCondition :: User -> (Document -> Bool) -> TestEnv Document
addRandomDocumentWithAuthorAndCondition user p =
  addRandomDocument ((randomDocumentAllowsDefault user) { randomDocumentCondition = p})

addRandomDocument :: RandomDocumentAllows -> TestEnv Document
addRandomDocument rda = do
  file <- addNewRandomFile
  now <- liftIO getMinutesTime
  let user = randomDocumentAuthor rda
      p = randomDocumentCondition rda
  mcompany <- case usercompany user of
    Nothing  -> return Nothing
    Just cid -> dbQuery $ GetCompany cid
  --liftIO $ print $ "about to generate document"
  document <- worker file now user p mcompany
  docid <- dbUpdate $ StoreDocumentForTesting document
  mdoc  <- dbQuery  $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing -> do
      assertFailure "Could not store document."
      return document
    Just doc' -> do
              return doc'
  where
    worker file now user p mcompany = do
      doc' <- rand 10 arbitrary
      xtype <- rand 10 (elements $ randomDocumentAllowedTypes rda)
      status <- rand 10 (elements $ randomDocumentAllowedStatuses rda)

      siglinks <- rand 10 (listOf $ randomSigLinkByStatus status)

      let doc = doc' { documenttype = xtype, documentstatus = status }

      roles <- rand 10 arbitrary
      asl' <- rand 10 $ randomAuthorLinkByStatus status
      userDetails <- signatoryDetailsFromUser user roles
      let asl = asl' { maybesignatory = Just (userid user)
                     , maybecompany = usercompany user
                     , signatorydetails = userDetails
                     }

      let alllinks = asl : siglinks


      let adoc = doc { documentsignatorylinks = alllinks
                     , documentlang = getLang user
                     , documentfile = Just (fileid file)
                     , documentsealedfile = if documentstatus doc == Closed
                                               then Just (fileid file)
                                               else Nothing
                     }
      case (p adoc, invariantProblems now adoc) of
        (True, Nothing) -> return adoc
        (False, _)  -> do
          --liftIO $ print $ "did not pass condition; doc: " ++ show adoc
          worker file now user p mcompany

        (_, Just _problems) -> do
               -- am I right that random document should not have invariantProblems?
               --uncomment this to find out why the doc was rejected
               --print adoc
               --liftIO $ print $ "rejecting doc: " ++ _problems
               worker file now user p mcompany
      --asl <- dbUpdate $ SignLinkFromDetailsForTest asd roles


rand :: Int -> Gen a -> TestEnv a
rand i a = do
  stdgn <- random
  return $ unGen a stdgn i

untilCondition :: (Monad m) => (b -> Bool) -> m b -> m b
untilCondition cond gen = do
  v <- gen
  if cond v then return v else untilCondition cond gen

addRandomDocumentWithAuthor' :: User -> TestEnv Document
addRandomDocumentWithAuthor' user = addRandomDocumentWithAuthorAndCondition user (\_ -> True)

doNTimes :: (Monad m) => Int -> m () -> m ()
doNTimes 0 _ = return ()
doNTimes n a = do
  _ <- a
  doNTimes (n - 1) a

doTimes :: Int -> TestEnv (Maybe (TestEnv ())) -> TestEnv ()
doTimes i action
  | i == 0 = return ()
  | otherwise = do
    res <- action
    case res of
      Nothing -> doTimes i action
      Just ass -> do
        _ <- ass
        doTimes (i - 1) action

invalidateTest :: TestEnv (Maybe (TestEnv ()))
invalidateTest = return Nothing

validTest :: TestEnv () -> TestEnv (Maybe (TestEnv ()))
validTest = return . Just

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
class RandomUpdate a b where
  randomUpdate :: a -> TestEnv b

instance (DBUpdate TestEnv ev res) => RandomUpdate ev res where
  randomUpdate = dbUpdate

instance (Arbitrary a, RandomUpdate c b) => RandomUpdate (a -> c) b where
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

-- other helpers

guardMethodM :: Kontrakcja m => Method -> m ()
guardMethodM m = do
  rq <- askRq
  unless (rqMethod rq == m) internalError

-- | Checks type of flash message
isFlashOfType :: FlashMessage -> FlashType -> Bool
isFlashOfType (FlashMessage ft _) t = ft == t
isFlashOfType (FlashTemplate ft _ _) t = ft == t

getFlashType :: FlashMessage -> FlashType
getFlashType (FlashMessage ft _) = ft
getFlashType (FlashTemplate ft _ _) = ft

instance Arbitrary Lang where
  arbitrary = elements [LANG_SV, LANG_EN]
