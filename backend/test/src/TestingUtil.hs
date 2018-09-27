{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestingUtil where

import Control.Concurrent.STM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Crypto.RNG
import Data.Char
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable (cast)
import Data.Word
import Happstack.Server
import Log
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Unicode as QCU
import Type.Reflection
import qualified Crypto.Scrypt as Scrypt
import qualified Data.Aeson as A
import qualified Data.Aeson.Diff as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Test.HUnit as T

import BrandedDomain.BrandedDomain
import BrandedDomain.BrandedDomainID
import BrandedDomain.Model
import Context
import DataRetentionPolicy
import DB
import Doc.Action
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Model
import Doc.SealStatus (SealStatus(..))
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import Doc.TestInvariants
import EID.CGI.GRP.Transaction.Model
import EID.Signature.Model
import FeatureFlags.Model
import File.File
import File.FileID
import File.Model
import File.Storage
import FlashMessage
import GuardTime
import IPAddress
import KontraMonad
import MagicHash (MagicHash, unsafeMagicHash)
import MailContext
import MinutesTime
import PadApplication.Data
import Partner.Model
import PdfToolsLambda.Conf
import Session.SessionID
import SMS.Data (SMSProvider(..))
import System.Random.CryptoRNG ()
import Templates
import TestFileStorage (liftTestFileStorageT, runTestFileStorageT)
import TestKontra
import User.Email
import User.Model
import User.Password.Internal (Password(..))
import UserGroup.Data
import UserGroup.Data.PaymentPlan
import UserGroup.Model
import Util.Actor
import qualified KontraError as KE
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

arbitraryName :: Gen T.Text
arbitraryName = (return . pack . concat) =<< replicateM 3 arbitrarySyllable

arbitrarySyllable :: Gen String
arbitrarySyllable = do
  consonant <- elements "bcdfghjklmnpqrstvwxz"
  vovel <- elements "aeiyou"
  return [consonant,vovel]

instance Arbitrary T.Text where
  arbitrary = arbitraryName

instance Arbitrary PartnerID where
  arbitrary = unsafePartnerID . abs <$> arbitrary

instance Arbitrary SMSProvider where
  arbitrary = elements [ SMSDefault
                       , SMSTeliaCallGuide
                       ]

instance Arbitrary PadAppMode where
  arbitrary = elements [ ListView
                       , PinCode
                       ]

instance Arbitrary PaymentPlan where
  arbitrary = elements [ FreePlan
                       , OnePlan
                       , TeamPlan
                       , EnterprisePlan
                       , TrialPlan
                       ]

-- PostgreSQL will not store \NUL in DB
genUnicodeString :: Gen String
genUnicodeString = filter (/= '\NUL') <$> QCU.string

genMaybeUnicodeString :: Gen (Maybe String)
genMaybeUnicodeString = oneof [ pure Nothing, Just <$> genUnicodeString ]

instance Arbitrary UserGroup where
  arbitrary = (\name minfo maddress mui invoicing -> UserGroup emptyUserGroupID Nothing name maddress minfo invoicing mui)
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary DataRetentionPolicy where
  arbitrary = DataRetentionPolicy
    <$> oneof [return Nothing, Just <$> choose (1,365)]
    <*> oneof [return Nothing, Just <$> choose (1,365)]
    <*> oneof [return Nothing, Just <$> choose (1,365)]
    <*> oneof [return Nothing, Just <$> choose (1,365)]
    <*> oneof [return Nothing, Just <$> choose (1,365)]
    <*> oneof [return Nothing, Just <$> choose (1,365)]
    <*> arbitrary

instance Arbitrary UserGroupSettings where
  arbitrary = UserGroupSettings
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary UserGroupAddress where
  arbitrary = UserGroupAddress
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary UserGroupUI where
  arbitrary = (\mbrowsertitle msmsoriginator mfavicon -> UserGroupUI Nothing Nothing Nothing mbrowsertitle msmsoriginator mfavicon)
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary UserGroupInvoicing where
  arbitrary = oneof [
      pure None
    , BillItem <$> arbitrary
    , Invoice <$> arbitrary
    ]

instance Arbitrary MagicHash where
  arbitrary = unsafeMagicHash <$> arbitrary

instance Arbitrary DeliveryStatus where
  arbitrary = elements [ Delivered
                       , Undelivered
                       , Unknown
                       , Deferred
                       ]


instance Arbitrary FeatureFlags where
  arbitrary = do
    (a, b, c, d, e, f, g, h, i, j) <- arbitrary
    (k, l, m, n, o, p, q, r, s, t) <- arbitrary
    v <- arbitrary
    return $ FeatureFlags {
        ffCanUseTemplates = a
      , ffCanUseBranding  = b
      , ffCanUseAuthorAttachments = c
      , ffCanUseSignatoryAttachments = d
      , ffCanUseMassSendout = e
      , ffCanUseSMSInvitations = f
      , ffCanUseSMSConfirmations = g
      , ffCanUseDKAuthenticationToView = h
      , ffCanUseDKAuthenticationToSign = i
      , ffCanUseNOAuthenticationToView = j
      , ffCanUseNOAuthenticationToSign = k
      , ffCanUseSEAuthenticationToView = l
      , ffCanUseSEAuthenticationToSign = m
      , ffCanUseSMSPinAuthenticationToView = n
      , ffCanUseSMSPinAuthenticationToSign = o
      , ffCanUseStandardAuthenticationToView = p
      , ffCanUseStandardAuthenticationToSign = q
      , ffCanUseEmailInvitations = r
      , ffCanUseEmailConfirmations = v
      , ffCanUseAPIInvitations = s
      , ffCanUsePadInvitations = t
      }

instance Arbitrary Features where
  arbitrary = do
    admin  <- arbitrary
    others <- arbitrary
    return $ Features { fAdminUsers = admin, fRegularUsers = others }

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . fromInteger <$> arbitrary

{- | Sometimes we get and object that is not as random as we would expect (from some reason)
     Like author signatorylink that by default does not have any fields attached
     This is a class to make it more random - so to attach this fields for example.
-}
class ExtendWithRandomness a where
    moreRandom :: a -> Gen a
    extendRandomness :: a -> TestEnv a
    extendRandomness a = do
      gen <- random
      return $ unGen (moreRandom a) gen 10

arbitraryAuthorActor :: TestEnv Actor
arbitraryAuthorActor = do
  ctx <- mkContext def
  authorActor ctx <$> rand 10 arbitrary

arbitrarySystemActor :: (Functor m, CryptoRNG m) => m Actor
arbitrarySystemActor = systemActor <$> rand 10 arbitrary

arbitrarySignatoryActor :: TestEnv Actor
arbitrarySignatoryActor = do
  ctx <- mkContext def
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
    authenticationToSign <- arbitrary
    authenticationToView <- arbitrary
    (cmTitle, cmQuestions) <- oneof
      [ (\t qs -> (Just t, qs)) <$> arbString 1 100 <*> listOf1 arbitrary
      , return (Nothing, [])
      ]
    hidePN <- arbitrary
    return $ def { signatorylinkid = unsafeSignatoryLinkID 0
                          , signatoryfields = fields
                          , signatoryisauthor = False
                          , signatoryispartner = True
                          , signatorysignorder = SignOrder 1
                          , signatorymagichash         = mh
                          , maybesigninfo              = signinfo
                          , maybeseeninfo              = seeninfo
                          , signatorylinkdeliverymethod = delivery
                          , signatorylinkauthenticationtosignmethod = authenticationToSign
                          , signatorylinkauthenticationtoviewmethod = authenticationToView
                          , signatorylinkconsenttitle     = cmTitle
                          , signatorylinkconsentquestions = cmQuestions
                          , signatorylinkhidepn = hidePN
                          }

instance Arbitrary SignatoryConsentQuestion where
  arbitrary = do
    title       <- arbString 1 100
    po          <- arbString 1 100
    no          <- arbString 1 100
    description <- oneof
      [ (\ti te -> Just (ti, te)) <$> arbString 1 100 <*> arbString 1 100
      , return Nothing
      ]

    return $ def
      { scqTitle          = title
      , scqPositiveOption = po
      , scqNegativeOption = no
      , scqDescription    = description
      }

instance Arbitrary CSVUpload where
  arbitrary = do
    cols <- arbitrary
    rows <- arbitrary
    b <- vectorOf rows (vectorOf cols arbitrary)
    return $ CSVUpload b

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

documentAllSharings :: [DocumentSharing]
documentAllSharings = [Private, Shared]

instance Arbitrary DocumentSharing where
  arbitrary = elements documentAllSharings

instance Arbitrary Document where
  arbitrary = do
    -- we can have any document type here
    dtype <- arbitrary
    -- sharing has meaning only for templates
    dsharing <- if dtype == Template
                then arbitrary
                else return Private
    -- status has meaning only for signables
    dstatus <- if dtype == Signable
               then arbitrary
               else return Preparation
    sls <- arbitrary
    -- we can have any days to sign. almost
    ddaystosign <- elements [1, 10, 99]
    dtimeouttime <- arbitrary
    return $ def
      { documentstatus = dstatus
      , documenttype = dtype
      , documentsharing = dsharing
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
                      , DocumentError
                      ]

instance Arbitrary DocumentStatus where
  arbitrary = elements documentAllStatuses

nonemptybs :: Gen BS.ByteString
nonemptybs = do
  s <- arbString 1 10
  return $ BS.fromString s

-- | Remove fields from duplicate types
filterSingleFieldIdentity :: [SignatoryField] -> [SignatoryField]
filterSingleFieldIdentity [] = []
filterSingleFieldIdentity (f:fs) = f : filterSingleFieldIdentity (filter (\h-> fieldIdentity f /= fieldIdentity h) fs)

instance {-# OVERLAPPING #-} Arbitrary [SignatoryField] where
  arbitrary = do
    fn <- arbString 1 20
    ln <- arbString 1 20
    em <- arbEmail
    (f1,f2,f3,f4,f5) <-  arbitrary
    return $ filter (\f->notElem (fieldIdentity f) [(NameFI (NameOrder 1)), (NameFI (NameOrder 2)), EmailFI]) (filterSingleFieldIdentity [f1,f2,f3,f4,f5])
               ++[
                  fieldForTests (NameFI $ NameOrder 1) fn
                , fieldForTests (NameFI $ NameOrder 2) ln
                , fieldForTests EmailFI em
                ]


instance Arbitrary FieldPlacement where
  arbitrary = do  -- We loose precision with conversion, so please watch out for this
    (a :: Int) <- choose (1,1000)
    (b :: Int) <- choose (1,1000)
    (c :: Int) <- choose (1,1000)
    (d :: Int) <- choose (1,1000)
    (e :: Int) <- choose (1,1000)
    (x :: Int) <- choose (1, 10)
    f <- arbitrary
    return $ FieldPlacement { placementid         = tempPlacementID
                            , placementxrel       = fromIntegral a / fromIntegral x
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
    elements [NameFT, EmailFT, CompanyFT, CompanyNumberFT, PersonalNumberFT, TextFT]

instance Arbitrary SignatoryField where
  arbitrary = do
    t <- arbitrary
    case t of
       NameFT  ->  SignatoryNameField <$> arbitrary
       EmailFT ->  SignatoryEmailField <$> arbitrary
       CompanyFT ->  SignatoryCompanyField <$> arbitrary
       PersonalNumberFT ->  SignatoryPersonalNumberField <$> arbitrary
       CompanyNumberFT ->  SignatoryCompanyNumberField <$> arbitrary
       _ -> SignatoryTextField <$> arbitrary


instance Arbitrary SignatoryNameField where
  arbitrary = do
    no <- elements [NameOrder 1, NameOrder 2]
    v <- arbString 1 100
    p <- arbitrary
    return $ NameField {
         snfID = unsafeSignatoryFieldID 0
       , snfNameOrder = no
       , snfValue = v
       , snfObligatory = True
       , snfShouldBeFilledBySender = False
       , snfPlacements = p
    }

instance Arbitrary SignatoryEmailField where
  arbitrary = do
    v <- arbString 1 100
    p <- arbitrary
    return $ EmailField {
         sefID = unsafeSignatoryFieldID 0
       , sefValue = v
       , sefObligatory = True
       , sefShouldBeFilledBySender = False
       , sefEditableBySignatory    = False
       , sefPlacements = p
    }
instance Arbitrary SignatoryCompanyField where
  arbitrary = do
    v <- arbString 1 100
    p <- arbitrary
    return $ CompanyField {
         scfID = unsafeSignatoryFieldID 0
       , scfValue = v
       , scfObligatory = True
       , scfShouldBeFilledBySender = False
       , scfPlacements = p
    }


instance Arbitrary SignatoryPersonalNumberField where
  arbitrary = do
    v <- arbString 1 100
    p <- arbitrary
    return $ PersonalNumberField {
         spnfID = unsafeSignatoryFieldID 0
       , spnfValue = v
       , spnfObligatory = True
       , spnfShouldBeFilledBySender = False
       , spnfPlacements = p
    }

instance Arbitrary SignatoryCompanyNumberField where
  arbitrary = do
    v <- arbString 1 100
    p <- arbitrary
    return $ CompanyNumberField {
         scnfID = unsafeSignatoryFieldID 0
       , scnfValue = v
       , scnfObligatory = True
       , scnfShouldBeFilledBySender = False
       , scnfPlacements = p
    }

instance Arbitrary SignatoryTextField where
  arbitrary = do
    l <- arbString 1 20
    v <- arbString 1 100
    filled <- arbitrary
    p <- arbitrary
    usecustomvalidation <- arbitrary
    valpattern <- arbString 1 20
    valexample <- arbString 1 20
    valtooltip <- arbString 1 200
    return $ TextField {
         stfID = unsafeSignatoryFieldID 0
       , stfName = l
       , stfFilledByAuthor = filled
       , stfValue = v
       , stfObligatory = True
       , stfShouldBeFilledBySender = False
       , stfPlacements = p
       , stfCustomValidation = case usecustomvalidation of
           False -> Nothing
           True  -> Just $ TextCustomValidation valpattern valexample valtooltip
    }

instance Arbitrary AuthenticationToSignMethod where
  arbitrary = elements [StandardAuthenticationToSign, SEBankIDAuthenticationToSign, NOBankIDAuthenticationToSign, DKNemIDAuthenticationToSign]

instance Arbitrary AuthenticationToViewMethod where
  arbitrary = elements [StandardAuthenticationToView, SEBankIDAuthenticationToView, NOBankIDAuthenticationToView, DKNemIDAuthenticationToView]

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

instance Arbitrary Scrypt.EncryptedPass where
  arbitrary = do salt <- vectorOf 32 arbitrary
                 k    <- choose (8, 32)
                 pass <- vectorOf k arbitrary
                 let salt' = Scrypt.Salt . BS8.pack $ salt
                     pass' = Scrypt.Pass . BS.fromString $ pass
                 return $ Scrypt.encryptPass' salt' pass'

instance Arbitrary Password where
  arbitrary = oneof [ LegacyPassword <$> arbitrary <*> arbitrary
                    , Password <$> arbitrary <*> arbitrary ]

instance Arbitrary SignupMethod where
  arbitrary = elements [AccountRequest, ViralInvitation, ByAdmin, CompanyInvitation]

instance Arbitrary UserSettings where
  arbitrary = UserSettings <$> arbitrary <*> arbitrary

instance Arbitrary User where
  arbitrary = User <$> arbitrary
                   <*> arbitrary
                   -- Messes with tests if these are set
                   <*> pure Nothing -- usertotp
                   <*> pure False   -- usertotpactive
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> pure (unsafeBrandedDomainID 0)
                   <*> pure (unsafeUserGroupID 0)

instance Arbitrary CgiGrpTransaction where
  arbitrary = do
    CgiGrpSignTransaction
    <$> arbitrary
    <*> (T.pack . fromSNN <$> arbitrary)
    <*> (T.pack . fromSNN <$> arbitrary)
    <*> (T.pack . fromSNN <$> arbitrary)
    <*> (pure tempSessionID)

instance Arbitrary CGISEBankIDSignature where
  arbitrary = CGISEBankIDSignature
    <$> (C.renderXMLContent <$> arbitrary)
    <*> (C.renderXMLContent <$> arbitrary)
    <*> (C.renderXMLContent <$> arbitrary)
    <*> (C.renderXMLContent <$> arbitrary)
    <*> arbitrary
    <*> arbitrary

instance Arbitrary NetsNOBankIDSignature where
  arbitrary = NetsNOBankIDSignature
    <$> (T.pack <$> arbString 20 30)
    <*> (T.pack <$> arbString 200 300)
    <*> (T.pack <$> arbString 10 20)
    <*> (T.pack <$> arbString 11 11)

instance Arbitrary NetsDKNemIDSignature where
  arbitrary = NetsDKNemIDSignature
    <$> (T.pack <$> arbString 20 30)
    <*> (T.pack <$> arbString 200 300)
    <*> (T.pack <$> arbString 10 20)
    <*> (T.pack <$> arbString 10 10)

instance Arbitrary ESignature where
  arbitrary = oneof [
      CGISEBankIDSignature_ <$> arbitrary
    , NetsNOBankIDSignature_ <$> arbitrary
    , NetsDKNemIDSignature_ <$> arbitrary
    ]

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
  t <- arbString 2 4
  return $ n ++ "@" ++ d ++ "." ++ t

signatoryLinkExample1 :: SignatoryLink
signatoryLinkExample1 = def { signatorylinkid = unsafeSignatoryLinkID 0
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
                                      , signatoryfields = [
                                            fieldForTests (NameFI $ NameOrder 1) "Eric"
                                          , fieldForTests (NameFI $ NameOrder 2) "Normand"
                                          , fieldForTests EmailFI "eric@scrive.com"
                                          , fieldForTests CompanyFI "Scrive"
                                          , fieldForTests CompanyNumberFI "1234"
                                          , fieldForTests PersonalNumberFI "9101112"
                                          , fieldForTests (TextFI "phone") "504-302-3742"
                                          ]
                                      , signatorylinkcsvupload = Nothing
                                      , signatoryattachments   = []
                                      , signatorylinksignredirecturl = Nothing
                                      , signatorylinkrejectiontime = Nothing
                                      , signatorylinkrejectionreason = Nothing
                                      , signatorylinkauthenticationtosignmethod = StandardAuthenticationToSign
                                      }

testThat :: String -> TestEnvSt -> TestEnv () -> Test
testThat s env = testCase s . runTestEnv env

compareTime :: UTCTime -> UTCTime -> Bool
compareTime (UTCTime da ta) (UTCTime db tb) = (da == db)
  && (((ta + picosecondsToDiffTime (10^9) >= tb) && (ta <= tb)) || ((tb + picosecondsToDiffTime (10^9)) >= ta && (ta >= tb)))

addNewUserGroup :: TestEnv UserGroup
addNewUserGroup = do
    ugname <- rand 10 (T.pack <$> arbString 3 30)
    ugacompanynumber <- rand 10 (T.pack <$> arbString 3 30)
    ugaaddress <- rand 10 (T.pack <$> arbString 3 30)
    ugazip <- rand 10 (T.pack <$> arbString 3 30)
    ugacity <- rand 10 (T.pack <$> arbString 3 30)
    ugacountry <- rand 10 (T.pack <$> arbString 3 30)
    let ug = set ugName          ugname
          . set ugAddress       uga
          $ def
        uga = UserGroupAddress
          { _ugaCompanyNumber = ugacompanynumber
          , _ugaAddress       = ugaaddress
          , _ugaZip           = ugazip
          , _ugaCity          = ugacity
          , _ugaCountry       = ugacountry
          }
    dbUpdate . UserGroupCreate $ ug

addNewRandomFile :: ( CryptoRNG m, MonadBase IO m, MonadCatch m, MonadDB m
                    , MonadFileStorage m, MonadLog m, MonadThrow m, MonadTime m
                    ) => m FileID
addNewRandomFile = do
  fn <- rand 1 $ elements [ inTestDir "pdfs/simple.pdf"
                          , inTestDir "pdfs/telia.pdf"
                          , inTestDir "pdfs/hp-designjet.pdf"
                          , inTestDir "pdfs/visa-application.pdf"
                          ]
  cnt <- liftBase $ BS.readFile fn
  saveNewFile fn cnt

addNewUser :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m) => String -> String -> String -> m (Maybe User)
addNewUser firstname secondname email = do
  bd <- dbQuery $ GetMainBrandedDomain
  ug <- dbUpdate $ UserGroupCreate def
  dbUpdate $ AddUser (firstname, secondname) email Nothing (get ugID ug,True) def (get bdid bd) AccountRequest

addNewUserWithCompany :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
                      => String
                      -> String
                      -> String
                      -> m (Maybe (User, UserGroupID))
addNewUserWithCompany firstname secondname email = do
  bd <- dbQuery $ GetMainBrandedDomain
  ug <- dbUpdate $ UserGroupCreate def
  mUser <- dbUpdate $ AddUser (firstname, secondname) email Nothing (get ugID ug,True) def (get bdid bd) AccountRequest
  case mUser of
    Nothing -> return Nothing
    Just user -> return $ Just (user, get ugID ug)

-- | Create user and add it to a company as non-admin user.
addNewCompanyUser :: String -> String -> String -> UserGroupID
                  -> TestEnv (Maybe User)
addNewCompanyUser firstname secondname email ugid = do
  bd <- dbQuery $ GetMainBrandedDomain
  dbQuery (UserGroupGet ugid) >>= \case
    Nothing -> return Nothing
    Just _ug ->
      dbUpdate $ AddUser (firstname, secondname) email Nothing (ugid, False) def
                         (get bdid bd) CompanyInvitation

-- | Create user and add it to a company as admin.
addNewCompanyAdminUser :: String -> String -> String -> UserGroupID
                       -> TestEnv (Maybe User)
addNewCompanyAdminUser firstname secondname email ugid = do
  bd <- dbQuery $ GetMainBrandedDomain
  dbQuery (UserGroupGet ugid) >>= \case
    Nothing -> return Nothing
    Just _ug ->
      dbUpdate $ AddUser (firstname, secondname) email Nothing (ugid, True) def
                         (get bdid bd) CompanyInvitation

-- | Create user and add it to a new user group as admin.
addNewAdminUserAndUserGroup :: String -> String -> String
                            -> TestEnv (User, UserGroup)
addNewAdminUserAndUserGroup firstname secondname email = do
  ug <- addNewUserGroup
  bd <- dbQuery $ GetMainBrandedDomain
  Just user <- dbUpdate $ AddUser
    (firstname, secondname) email Nothing (get ugID ug, True) def (get bdid bd)
    CompanyInvitation
  return (user, ug)

addNewUserToUserGroup :: String -> String -> String -> UserGroupID -> TestEnv (Maybe User)
addNewUserToUserGroup firstname secondname email ugid = do
  bd <- dbQuery $ GetMainBrandedDomain
  dbQuery (UserGroupGet ugid) >>= \case
    Nothing -> return Nothing
    Just _ug ->
      dbUpdate $ AddUser (firstname, secondname) email Nothing (ugid,True) def (get bdid bd) CompanyInvitation

addNewRandomUser :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m) => m User
addNewRandomUser = do
  fn <- rand 10 $ arbString 3 30
  ln <- rand 10 $ arbString 3 30
  em <- rand 10 arbEmail
  Just user <- addNewUser fn ln em
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

addNewRandomUserWithCompany :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
                            => m (User, UserGroupID)
addNewRandomUserWithCompany = do
  fn <- rand 10 $ arbString 3 30
  ln <- rand 10 $ arbString 3 30
  em <- rand 10 arbEmail
  Just (user, ugid) <- addNewUserWithCompany fn ln em
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
  return (user, ugid)

addNewRandomUserWithPassword :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m) => String -> m User
addNewRandomUserWithPassword password = do
  -- create random user
  randomUser <- addNewRandomUser
  -- set his password
  passwordhash <- createPassword password
  _ <- dbUpdate $ SetUserPassword (userid randomUser) passwordhash
  return randomUser

addNewRandomCompanyUser :: UserGroupID -> Bool -> TestEnv User
addNewRandomCompanyUser ugid isadmin = do
  User{userid} <- addNewRandomUser
  _ <- dbUpdate $ SetUserUserGroup userid ugid
  _ <- dbUpdate $ SetUserCompanyAdmin userid isadmin
  Just user <- dbQuery $ GetUserByID userid
  return user

addNewRandomUserGroupUser :: UserGroupID -> Bool -> TestEnv User
addNewRandomUserGroupUser ugid isadmin = do
  User{userid} <- addNewRandomUser
  _ <- dbUpdate $ SetUserUserGroup userid ugid
  _ <- dbUpdate $ SetUserCompanyAdmin userid isadmin
  Just user <- dbQuery $ GetUserByID userid
  return user

addNewRandomPartnerUser :: TestEnv (User, UserGroup)
addNewRandomPartnerUser = do
  -- To use UserGroups as if they are Partners, we need to generate a UserGroupID
  -- which is not a PartnerID.
  partners <- dbQuery GetPartners
  -- try to generate a userGroup and always check, whether the ugid is already a partnerID.
  mresult <- (\folder -> foldM folder Nothing [1..100]) $ \mres _counter -> do
    case mres of
      Just res -> return . Just $ res
      Nothing -> do
        partnerAdminUser <- addNewRandomUser
        partnerAdminUserGroup <- dbQuery $ UserGroupGetByUserID (userid partnerAdminUser)
        let partnerIDAlreadyExists = (unsafeUserGroupIDToPartnerID . get ugID $ partnerAdminUserGroup) `elem` map ptID partners
        case partnerIDAlreadyExists of
          True -> return Nothing
          False -> return $ Just (partnerAdminUser, partnerAdminUserGroup)
  case mresult of
    Nothing -> unexpectedError "UserGroupID - PartnerID collision"
    Just (partnerAdminUser, partnerAdminUserGroup) -> do
      -- insert new partner row with the same ID as the UserGroup
      True <- dbUpdate . InsertPartnerForTests $ Partner {
                ptID = unsafeUserGroupIDToPartnerID . get ugID $ partnerAdminUserGroup
              , ptName = T.unpack . get ugName $ partnerAdminUserGroup
              , ptDefaultPartner = False
              , ptUserGroupID = Just $ get ugID partnerAdminUserGroup
              }
      True <- dbUpdate $ MakeUserPartnerAdmin (userid partnerAdminUser) (get ugID partnerAdminUserGroup)
      return (partnerAdminUser, partnerAdminUserGroup)

data RandomDocumentAllows = RandomDocumentAllows
                          { randomDocumentAllowedTypes :: [DocumentType]
                          , randomDocumentAllowedStatuses :: [DocumentStatus]
                          , randomDocumentAllowedSharings :: [DocumentSharing]
                          , randomDocumentAuthor :: User
                          , randomDocumentCondition :: Document -> Bool
                          }

randomDocumentAllowsDefault :: User -> RandomDocumentAllows
randomDocumentAllowsDefault user = RandomDocumentAllows
                              { randomDocumentAllowedTypes = documentAllTypes
                              , randomDocumentAllowedStatuses = [ Preparation
                                                                , Pending
                                                                , Closed
                                                                , Canceled
                                                                , Timedout
                                                                , Rejected
                                                                , DocumentError
                                                                ]
                              , randomDocumentAllowedSharings = documentAllSharings
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
addRandomDocumentWithFile fileid rda = do
  now <- currentTime
  let user = randomDocumentAuthor rda
      p = randomDocumentCondition rda
  file <- dbQuery $ GetFileByFileID fileid
  --liftIO $ print $ "about to generate document"
  document <- worker now user p file
  docid <- dbUpdate $ StoreDocumentForTesting document
  dbQuery $ GetDocumentByDocumentID docid
  where
    worker now user p file = do
      doc' <- rand 10 arbitrary
      xtype <- rand 10 (elements $ randomDocumentAllowedTypes rda)
      status <- if xtype /= Signable
                     && Preparation `elem` randomDocumentAllowedStatuses rda
        then return Preparation
        else rand 10 (elements $ randomDocumentAllowedStatuses rda)
      sharing <- if xtype /= Template
                      && Private `elem` randomDocumentAllowedSharings rda
        then return Private
        else rand 10 (elements $ randomDocumentAllowedSharings rda)
      title <- rand 1 $ arbString 10 25
      siglinks <- rand 10 (listOf $ randomSigLinkByStatus status)

      let doc = doc' { documenttype = xtype
                     , documentstatus = status
                     , documentsharing = sharing
                     , documenttitle = title
                     }

      partner <- rand 10 arbitrary
      asl' <- rand 10 $ randomAuthorLinkByStatus status
      userDetails <- signatoryFieldsFromUser user
      let asl = asl' {   maybesignatory = Just (userid user)
                       , signatoryfields = userDetails
                       , signatoryispartner = partner
                     }

      let alllinks = asl : siglinks


      let closedfile = if documentstatus doc == Closed
                       then [MainFile fileid Closed Missing (filename file)]
                       else []
      let adoc = doc { documentsignatorylinks = alllinks
                     , documentlang = getLang user
                     , documentmainfiles = closedfile ++ [MainFile fileid Preparation Missing (filename file)]
                     }
      case (p adoc, invariantProblems now adoc) of
        (True, Nothing) -> return adoc
        (False, _)  -> do
          rej <- asks teRejectedDocuments
          liftIO $ (atomically . modifyTVar' rej) (+1)
          --liftIO $ print $ "did not pass condition; doc: " ++ show adoc
          worker now user p file

        (_, Just _problems) -> do
          rej <- asks teRejectedDocuments
          liftIO $ (atomically . modifyTVar' rej) (+1)
          -- am I right that random document should not have invariantProblems?
          --uncomment this to find out why the doc was rejected
          --print adoc
          --liftIO $ print $ "rejecting doc: " ++ _problems
          worker now user p file

-- | Synchronously seal a document.
sealTestDocument :: Context -> DocumentID -> TestEnv ()
sealTestDocument ctx did = void $ TestEnv $ liftTestFileStorageT $ \fsEnv -> do
  cryptoSt <- newCryptoRNGState
  withDocumentID did
    . runGuardTimeConfT (get ctxgtconf ctx)
    . runPdfToolsLambdaConfT (get ctxpdftoolslambdaconf ctx)
    . runTemplatesT ((get ctxlang ctx), (get ctxglobaltemplates ctx))
    . runMailContextT (contextToMailContext ctx)
    . runCryptoRNGT cryptoSt
    . flip runTestFileStorageT fsEnv
    $ do
        res <- postDocumentClosedActions True False
                 `catch` \(_::KE.KontraError) -> return False
        when res $ do
          extendingJobCount <- runSQL $
            "SELECT * FROM document_extending_jobs WHERE id =" <?> did
          assertEqual "postDocumentClosedActions should add task to\
                      \ document_extending_jobs" 1 extendingJobCount
        return res

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

-- Random gen

--Random query
class RandomQuery a b where
  randomQuery :: a -> TestEnv b

instance (DBQuery TestEnv ev res) => RandomQuery ev res where
  randomQuery = dbQuery

instance {-# OVERLAPPING #-} (Arbitrary a, RandomQuery c b) => RandomQuery (a -> c) b where
  randomQuery f = do
    a <- rand 10 arbitrary
    randomQuery $ f a

--Random update
class RandomUpdate a b m where
  randomUpdate :: a -> m b

instance (DBUpdate m ev res) => RandomUpdate ev res m where
  randomUpdate = dbUpdate

instance {-# OVERLAPPING #-} (CryptoRNG m, Arbitrary a, RandomUpdate c b m) => RandomUpdate (a -> c) b m where
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

instance Arbitrary FileID where
  arbitrary = unsafeFileID . abs <$> arbitrary

instance Arbitrary IPAddress where
  arbitrary = unsafeIPAddress <$> arbitrary

instance Arbitrary IPAddressWithMask where
  arbitrary = unsafeIPAddressWithMask <$> arbitrary <*> arbitrary

instance Arbitrary SignInfo where
  arbitrary = SignInfo <$> arbitrary <*> arbitrary

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

assertEqualJson :: MonadIO m => String -> A.Value -> A.Value -> m ()
assertEqualJson msg expected got = unless (expected == got) $ do
  assertFailure . unlines $
    [ msg
    , ""
    , "Expected: " ++ show expected
    , "Got: " ++ show got
    , ""
    , "Steps to go from the expected value to the one we got:"
    ] ++ map ((" * " ++) . show) (A.patchOperations $ A.diff expected got)
      ++
    [ ""
    , "Steps to go from the value we got to the expected one:"
    ] ++ map ((" * " ++) . show) (A.patchOperations $ A.diff got expected)

assertFailure :: MonadIO m => String -> m ()
assertFailure = liftIO . T.assertFailure

assertString :: MonadIO m => String -> m ()
assertString = liftIO . T.assertString

assertionPredicate :: (T.AssertionPredicable t, MonadIO m) => t -> m Bool
assertionPredicate = liftIO . T.assertionPredicate

assertRaisesInternalError :: (Show v, MonadIO m, MonadMask m) =>  m v -> m ()
assertRaisesInternalError a = catchJust (\case
  KE.Respond404      -> Nothing
  KE.InternalError _ -> Just ()
  KE.LinkInvalid     -> Nothing)
  (a >>= assertFailure . ("Expecting InternalError but got " ++) . show)
  return

assertRaisesDBException :: (Show v, MonadIO m, MonadMask m) =>  m v -> m ()
assertRaisesDBException a = (a >>= (\v -> assertFailure $ "Expecting db exception but got " ++ show v))  `catches` [
          Handler $ \_e@DBException{..} -> return ()
        ]


assertRaisesKontra :: forall e v m. (DBExtraException e, Show v, MonadIO m, MonadMask m)
             => (e -> Bool) -> m v -> m ()
assertRaisesKontra correctException action =
  (action >>= \r -> assertString $ "Expected DBExtraException " ++ typeOfE ++ ", instead returned result " ++ show r) `catches` [
    Handler helper
  -- support also DBExtraException nested within DBException
  , Handler $ \e@DBException{..} -> case cast dbeError of
    Just e' -> helper e'
    Nothing -> invExc e
  ]
  where
    helper (SomeDBExtraException e) = case cast e of
      Just e' -> if correctException e'
        then return ()
        else assertString $ "DBExtraException " ++ typeOfE ++ " is not correct " ++ show e'
      Nothing -> invExc e

    invExc :: (Show a, Typeable a) => a -> m ()
    invExc e = assertString $ "Expected DBExtraException " ++ typeOfE ++ ", instead got exception " ++ show e

    typeOfE = show $ typeRep @e

-- other helpers

guardMethodM :: Kontrakcja m => Method -> m ()
guardMethodM m = do
  rq <- askRq
  unless (rqMethod rq == m) KE.internalError

-- | Checks type of flash message
isFlashOfType :: FlashMessage -> FlashType -> Bool
isFlashOfType (FlashMessage ft _) t = ft == t

getFlashType :: FlashMessage -> FlashType
getFlashType (FlashMessage ft _) = ft

instance Arbitrary Lang where
  arbitrary = elements [LANG_SV, LANG_EN]

-- Simple way of creating signatory fields. Since datatype is big - we want to skip many details in many tests.
fieldForTests :: FieldIdentity -> String -> SignatoryField
fieldForTests (NameFI no) v = SignatoryNameField $ NameField {
      snfID                     = (unsafeSignatoryFieldID 0)
    , snfNameOrder              = no
    , snfValue                  = v
    , snfObligatory             = True
    , snfShouldBeFilledBySender = False
    , snfPlacements             = []
  }
fieldForTests CompanyFI v = SignatoryCompanyField $ CompanyField {
      scfID                     = (unsafeSignatoryFieldID 0)
    , scfValue                  = v
    , scfObligatory             = True
    , scfShouldBeFilledBySender = False
    , scfPlacements             = []
  }
fieldForTests PersonalNumberFI v =  SignatoryPersonalNumberField $ PersonalNumberField {
      spnfID                     = (unsafeSignatoryFieldID 0)
    , spnfValue                  = v
    , spnfObligatory             = True
    , spnfShouldBeFilledBySender = False
    , spnfPlacements             = []
  }
fieldForTests CompanyNumberFI v  = SignatoryCompanyNumberField $ CompanyNumberField {
      scnfID                     = (unsafeSignatoryFieldID 0)
    , scnfValue                  = v
    , scnfObligatory             = True
    , scnfShouldBeFilledBySender = False
    , scnfPlacements             = []
  }
fieldForTests EmailFI v =  SignatoryEmailField $ EmailField {
      sefID                     = (unsafeSignatoryFieldID 0)
    , sefValue                  = v
    , sefObligatory             = True
    , sefShouldBeFilledBySender = False
    , sefEditableBySignatory    = False
    , sefPlacements             = []
  }
fieldForTests MobileFI v =  SignatoryMobileField $ MobileField {
      smfID                     = (unsafeSignatoryFieldID 0)
    , smfValue                  = v
    , smfObligatory             = True
    , smfShouldBeFilledBySender = False
    , smfEditableBySignatory    = False
    , smfPlacements             = []
  }
fieldForTests (TextFI l) v = SignatoryTextField $ TextField {
      stfID                     = (unsafeSignatoryFieldID 0)
    , stfName                   = l
    , stfFilledByAuthor         = True
    , stfValue                  = v
    , stfObligatory             = True
    , stfShouldBeFilledBySender = False
    , stfPlacements             = []
    , stfCustomValidation       = Nothing
  }
fieldForTests _ _  = unexpectedError "cant use signature or checkbox fields with this function"
