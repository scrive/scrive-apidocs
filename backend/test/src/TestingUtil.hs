{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestingUtil where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Trans
import Crypto.RNG
import Data.Char
import Data.Foldable (foldrM)
import Data.Int
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable (cast)
import Data.Word
import Happstack.Server
import Log
import Optics (gview)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.Text ()
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

import AccessControl.Model
import AccessControl.Types
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
import Folder.Model
import GuardTime
import IPAddress
import KontraMonad
import Log.Utils
import MagicHash (MagicHash, unsafeMagicHash)
import MailContext
import MinutesTime
import PadApplication.Types
import Partner.Model
import PdfToolsLambda.Conf
import Session.SessionID
import SMS.Types (SMSProvider(..))
import System.Random.CryptoRNG ()
import Templates
import TestFileStorage (liftTestFileStorageT, runTestFileStorageT)
import TestKontra
import User.Email
import User.Model
import User.Password.Internal (Password(..))
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import Util.Actor
import Util.MonadUtils
import qualified KontraError as KE
import qualified UserGroup.Internal as I
import qualified Text.XML.Content as C
import qualified Text.XML.DirtyContent as D

newtype XMLChar = XMLChar { unXMLChar :: Char }
  deriving (Enum, Eq, Ord)

instance Show XMLChar where
  show = show . unXMLChar

instance Arbitrary XMLChar where
  arbitrary = elements (map XMLChar ("\n\r\t" <> [' ' .. '~'] <> ['\160' .. '\255']))

instance Arbitrary C.XMLContent where
  arbitrary = C.cdata . pack . map unXMLChar <$> arbitrary

instance Arbitrary D.XMLContent where
  arbitrary =
    oneof [D.CleanXMLContent <$> arbitrary, D.DirtyXMLContent . pack <$> arbitrary]

newtype NotNullWord8 = NotNullWord8 { fromNNW8 :: Word8 }
  deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Show NotNullWord8 where
  show = show . fromNNW8

instance Bounded NotNullWord8 where
  minBound = 1
  maxBound = NotNullWord8 maxBound

newtype StringNoNUL = StringNoNUL { fromSNN :: Text }

instance Arbitrary NotNullWord8 where
  arbitrary = arbitrarySizedBoundedIntegral
  shrink    = shrinkIntegral

instance Arbitrary SignOrder where
  arbitrary = SignOrder <$> arbitrary

instance Arbitrary SignatoryRole where
  -- This cuts down on the number of generated documents.
  arbitrary = frequency
    [ (2, return SignatoryRoleSigningParty)
    , (1, return SignatoryRoleApprover)
    , (1, return SignatoryRoleViewer)
    ]


instance Arbitrary DocumentTag where
  arbitrary = DocumentTag <$> (fromSNN <$> arbitrary) <*> (fromSNN <$> arbitrary)

instance Arbitrary Folder where
  arbitrary = (\name -> Folder emptyFolderID Nothing name) <$> arbitraryName

instance Arbitrary UserID where
  arbitrary = unsafeUserID . abs <$> arbitrary

arbitraryName :: Gen Text
arbitraryName = (return . pack . concat) =<< replicateM 3 arbitrarySyllable

arbitrarySyllable :: Gen String
arbitrarySyllable = do
  consonant <- elements "bcdfghjklmnpqrstvwxz"
  vovel     <- elements "aeiyou"
  return [consonant, vovel]

arbitraryUnicodeText :: Gen Text
arbitraryUnicodeText = pack <$> (listOf (arbitraryUnicodeChar `suchThat` (/= '\0')))

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

arbitraryMaybe :: forall a . Gen a -> Gen (Maybe a)
arbitraryMaybe = liftArbitrary

newtype ArbitraryUnicode = ArbitraryUnicode
  { withArbitraryUnicode :: Text
  } deriving (Show, Eq, Ord)

instance Arbitrary ArbitraryUnicode where
  arbitrary = ArbitraryUnicode <$> arbitraryUnicodeText

instance Arbitrary PartnerID where
  arbitrary = unsafePartnerID . abs <$> arbitrary

instance Arbitrary SMSProvider where
  arbitrary = elements [SMSDefault, SMSTeliaCallGuide]

instance Arbitrary PadAppMode where
  arbitrary = elements [ListView, PinCode]

instance Arbitrary PaymentPlan where
  arbitrary = elements [FreePlan, OnePlan, TeamPlan, EnterprisePlan, TrialPlan]

-- PostgreSQL will not store \NUL in DB
genUnicodeString :: Gen String
genUnicodeString = filter (/= '\NUL') <$> QCU.string

genMaybeUnicodeString :: Gen (Maybe String)
genMaybeUnicodeString = oneof [pure Nothing, Just <$> genUnicodeString]

instance Arbitrary UserGroup where
  arbitrary =
    (I.UserGroup emptyUserGroupID Nothing)
      <$> arbitraryName
      <*> pure Nothing
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary UserGroupRoot where
  arbitrary =
    I.UserGroupRoot emptyUserGroupID
      <$> arbitraryUnicodeText
      <*> pure Nothing
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary DataRetentionPolicy where
  arbitrary =
    DataRetentionPolicy
      <$> oneof [return Nothing, Just <$> choose (1, 365)]
      <*> oneof [return Nothing, Just <$> choose (1, 365)]
      <*> oneof [return Nothing, Just <$> choose (1, 365)]
      <*> oneof [return Nothing, Just <$> choose (1, 365)]
      <*> oneof [return Nothing, Just <$> choose (1, 365)]
      <*> oneof [return Nothing, Just <$> choose (1, 365)]
      <*> arbitrary

instance Arbitrary UserGroupSettings where
  arbitrary =
    I.UserGroupSettings
      <$> arbitrary
      <*> arbitrary
      <*> arbitraryMaybe arbitraryUnicodeText
      <*> arbitraryMaybe arbitraryUnicodeText
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure False -- do not enforce 2FA in tests
      <*> pure Nothing -- do not set custom session expiry
      <*> pure Nothing -- no portal url

instance Arbitrary UserGroupAddress where
  arbitrary =
    I.UserGroupAddress
      <$> arbitraryUnicodeText
      <*> arbitraryUnicodeText
      <*> arbitraryUnicodeText
      <*> arbitraryUnicodeText
      <*> arbitraryUnicodeText
      <*> arbitraryUnicodeText

instance Arbitrary UserGroupUI where
  arbitrary =
    (I.UserGroupUI Nothing Nothing Nothing)
      <$> arbitraryMaybe arbitraryUnicodeText
      <*> arbitraryMaybe arbitraryUnicodeText
      <*> arbitrary

instance Arbitrary UserGroupInvoicing where
  arbitrary = oneof [pure None, BillItem <$> arbitrary, Invoice <$> arbitrary]

instance Arbitrary MagicHash where
  arbitrary = unsafeMagicHash <$> arbitrary

instance Arbitrary DeliveryStatus where
  arbitrary = elements [Delivered, Undelivered, Unknown, Deferred]


instance Arbitrary FeatureFlags where
  arbitrary = do
    (a, b, c, d, e, f, g, h, i, j) <- arbitrary
    (k, l, m, n, o, p, q, r, s, t) <- arbitrary
    (u, v, w, x, z, y, aa, bb)     <- arbitrary
    return $ FeatureFlags { ffCanUseTemplates                  = a
                          , ffCanUseBranding                   = b
                          , ffCanUseAuthorAttachments          = c
                          , ffCanUseSignatoryAttachments       = d
                          , ffCanUseMassSendout                = e
                          , ffCanUseSMSInvitations             = f
                          , ffCanUseSMSConfirmations           = g
                          , ffCanUseDKAuthenticationToView     = h
                          , ffCanUseDKAuthenticationToSign     = i
                          , ffCanUseNOAuthenticationToView     = j
                          , ffCanUseNOAuthenticationToSign     = k
                          , ffCanUseSEAuthenticationToView     = l
                          , ffCanUseSEAuthenticationToSign     = m
                          , ffCanUseSMSPinAuthenticationToView = n
                          , ffCanUseSMSPinAuthenticationToSign = o
                          , ffCanUseStandardAuthenticationToView = p
                          , ffCanUseStandardAuthenticationToSign = q
                          , ffCanUseVerimiAuthenticationToView = z
                          , ffCanUseIDINAuthenticationToView   = aa
                          , ffCanUseEmailInvitations           = r
                          , ffCanUseEmailConfirmations         = v
                          , ffCanUseAPIInvitations             = s
                          , ffCanUsePadInvitations             = t
                          , ffCanUseFIAuthenticationToView     = u
                          , ffCanUseShareableLinks             = w
                          , ffCanUseForwarding                 = x
                          , ffCanUseDocumentPartyNotifications = y
                          , ffCanUsePortal                     = bb
                          }

instance Arbitrary Features where
  arbitrary = do
    admin  <- arbitrary
    others <- arbitrary
    return $ Features { fAdminUsers = admin, fRegularUsers = others }

instance Arbitrary UTCTime where
  arbitrary = posixSecondsToUTCTime . fromInteger <$> arbitrary

-- | Sometimes we get an object that is not as random as we would
-- expect for some reason, like an author signatorylink that by
-- default does not have any fields attached. This is a class to make
-- such objects more random - so to attach these fields for example.
class ExtendWithRandomness a where
    moreRandom :: a -> Gen a
    extendRandomness :: a -> TestEnv a
    extendRandomness a = do
      gen <- random
      return $ unGen (moreRandom a) gen 10

arbitraryAuthorActor :: TestEnv Actor
arbitraryAuthorActor = do
  ctx <- mkContext defaultLang
  authorActor ctx <$> rand 10 arbitrary

arbitrarySystemActor :: (Functor m, CryptoRNG m) => m Actor
arbitrarySystemActor = systemActor <$> rand 10 arbitrary

arbitrarySignatoryActor :: TestEnv Actor
arbitrarySignatoryActor = do
  ctx <- mkContext defaultLang
  sl  <- rand 10 arbitrary
  withDocumentID (unsafeDocumentID 0) $ signatoryActor ctx sl

instance Arbitrary SignatoryLinkID where
  arbitrary = unsafeSignatoryLinkID . abs <$> arbitrary

arbitraryConsentModule :: Gen (Maybe String, [SignatoryConsentQuestion])
arbitraryConsentModule =
  (\t qs -> (Just t, qs)) <$> arbString 1 100 <*> listOf1 arbitrary

instance Arbitrary SignatoryLink where
  arbitrary = do
    fields   <- arbitrary
    seeninfo <- arbitrary
    role     <- arbitrary
    signinfo <-
      if isJust seeninfo && role `elem` [SignatoryRoleApprover, SignatoryRoleSigningParty]
        then arbitrary
        else return Nothing

    delivery               <- arbitrary
    authenticationToSign   <- arbitrary
    authenticationToView   <- arbitrary
    (cmTitle, cmQuestions) <- oneof [arbitraryConsentModule, return (Nothing, [])]
    hidePN                 <- arbitrary
    return $ defaultSignatoryLink
      { signatorylinkid               = unsafeSignatoryLinkID 0
      , signatoryfields               = fields
      , signatoryisauthor             = False
      , signatoryrole                 = role
      , signatorysignorder            = SignOrder 1
      , maybesigninfo                 = signinfo
      , maybeseeninfo                 = seeninfo
      , signatorylinkdeliverymethod   = delivery
      , signatorylinkauthenticationtosignmethod = authenticationToSign
      , signatorylinkauthenticationtoviewmethod = authenticationToView
      , signatorylinkconsenttitle     = cmTitle
      , signatorylinkconsentquestions = cmQuestions
      , signatorylinkhidepn           = hidePN
      }

instance Arbitrary SignatoryConsentQuestion where
  arbitrary = do
    title       <- arbString 1 100
    po          <- arbString 1 100
    no          <- arbString 1 100
    description <- oneof
      [(\ti te -> Just (ti, te)) <$> arbString 1 100 <*> arbString 1 100, return Nothing]

    return $ defaultSignatoryConsentQuestion { scqTitle          = title
                                             , scqPositiveOption = po
                                             , scqNegativeOption = no
                                             , scqDescription    = description
                                             }

instance Arbitrary CSVUpload where
  arbitrary = do
    cols <- arbitrary
    rows <- arbitrary
    b    <- vectorOf rows (vectorOf cols arbitrary)
    return $ CSVUpload b

instance Arbitrary DocumentID where
  arbitrary = unsafeDocumentID . abs <$> arbitrary

documentAllTypes :: [DocumentType]
documentAllTypes = [Signable, Template]

documentSignableTypes :: [DocumentType]
documentSignableTypes = [Signable]

documentTemplateTypes :: [DocumentType]
documentTemplateTypes = [Template]

instance Arbitrary DocumentType where
  arbitrary = elements documentAllTypes

documentAllSharings :: [DocumentSharing]
documentAllSharings = [Private, Shared]

instance Arbitrary DocumentSharing where
  arbitrary = elements documentAllSharings

instance Arbitrary Document where
  arbitrary = do
    dtype        <- arbitrary  -- we can have any document type here
    -- sharing has meaning only for templates
    dsharing     <- if dtype == Template then arbitrary else return Private
    -- status has meaning only for signables
    dstatus      <- if dtype == Signable then arbitrary else return Preparation
    sls          <- arbitrary
    -- we can have any days to sign. almost
    ddaystosign  <- elements [1, 10, 99]
    dtimeouttime <- arbitrary
    documentshareablelinkhash <- arbitrary
    return $ defaultDocument { documentstatus            = dstatus
                             , documenttype              = dtype
                             , documentsharing           = dsharing
                             , documentsignatorylinks    = sls
                             , documenttimeouttime       = Just dtimeouttime
                             , documentdaystosign        = ddaystosign
                             , documentshareablelinkhash
                             }

documentAllStatuses :: [DocumentStatus]
documentAllStatuses =
  [Preparation, Pending, Closed, Canceled, Timedout, Rejected, DocumentError]

instance Arbitrary DocumentStatus where
  arbitrary = elements documentAllStatuses

nonemptybs :: Gen BS.ByteString
nonemptybs = do
  s <- arbString 1 10
  return $ BS.fromString s

-- | Remove fields from duplicate types
filterSingleFieldIdentity :: [SignatoryField] -> [SignatoryField]
filterSingleFieldIdentity [] = []
filterSingleFieldIdentity (f : fs) =
  f : filterSingleFieldIdentity (filter (\h -> fieldIdentity f /= fieldIdentity h) fs)

instance {-# OVERLAPPING #-} Arbitrary [SignatoryField] where
  arbitrary = do
    fn                   <- arbText 1 20
    ln                   <- arbText 1 20
    em                   <- arbEmail
    (f1, f2, f3, f4, f5) <- arbitrary
    return
      $  filter
           (\f -> notElem (fieldIdentity f)
                          [(NameFI (NameOrder 1)), (NameFI (NameOrder 2)), EmailFI]
           )
           (filterSingleFieldIdentity [f1, f2, f3, f4, f5])

      <> [ fieldForTests (NameFI $ NameOrder 1) fn
         , fieldForTests (NameFI $ NameOrder 2) ln
         , fieldForTests EmailFI                em
         ]


instance Arbitrary FieldPlacement where
  -- We lose precision with conversion, so please
  -- watch out for this
  arbitrary = do
    (a :: Int) <- choose (1, 1000)
    (b :: Int) <- choose (1, 1000)
    (c :: Int) <- choose (1, 1000)
    (d :: Int) <- choose (1, 1000)
    (e :: Int) <- choose (1, 1000)
    (x :: Int) <- choose (1, 10)
    f          <- arbitrary
    return $ FieldPlacement { placementid      = tempPlacementID
                            , placementxrel    = fromIntegral a / fromIntegral x
                            , placementyrel    = fromIntegral b / fromIntegral x
                            , placementwrel    = fromIntegral c / fromIntegral x
                            , placementhrel    = fromIntegral d / fromIntegral x
                            , placementfsrel   = fromIntegral e / fromIntegral x
                            , placementpage    = f
                            , placementtipside = Nothing
                            , placementanchors = []
                            }

instance Arbitrary FieldType where
  arbitrary = do
    elements [NameFT, EmailFT, CompanyFT, CompanyNumberFT, PersonalNumberFT, TextFT]

instance Arbitrary SignatoryField where
  arbitrary = do
    t <- arbitrary
    case t of
      NameFT           -> SignatoryNameField <$> arbitrary
      EmailFT          -> SignatoryEmailField <$> arbitrary
      CompanyFT        -> SignatoryCompanyField <$> arbitrary
      PersonalNumberFT -> SignatoryPersonalNumberField <$> arbitrary
      CompanyNumberFT  -> SignatoryCompanyNumberField <$> arbitrary
      _                -> SignatoryTextField <$> arbitrary

instance Arbitrary SignatoryNameField where
  arbitrary = do
    no <- elements [NameOrder 1, NameOrder 2]
    v  <- arbText 1 100
    p  <- arbitrary
    return $ NameField { snfID                     = unsafeSignatoryFieldID 0
                       , snfNameOrder              = no
                       , snfValue                  = v
                       , snfObligatory             = True
                       , snfShouldBeFilledBySender = False
                       , snfPlacements             = p
                       }

instance Arbitrary SignatoryEmailField where
  arbitrary = do
    v <- arbText 1 100
    p <- arbitrary
    return $ EmailField { sefID                     = unsafeSignatoryFieldID 0
                        , sefValue                  = v
                        , sefObligatory             = True
                        , sefShouldBeFilledBySender = False
                        , sefEditableBySignatory    = False
                        , sefPlacements             = p
                        }

instance Arbitrary SignatoryMobileField where
  arbitrary = do
    v <- arbText 1 100
    p <- arbitrary
    return $ MobileField { smfID                     = unsafeSignatoryFieldID 0
                         , smfValue                  = v
                         , smfObligatory             = True
                         , smfShouldBeFilledBySender = False
                         , smfEditableBySignatory    = False
                         , smfPlacements             = p
                         }

instance Arbitrary SignatoryCompanyField where
  arbitrary = do
    v <- arbText 1 100
    p <- arbitrary
    return $ CompanyField { scfID                     = unsafeSignatoryFieldID 0
                          , scfValue                  = v
                          , scfObligatory             = True
                          , scfShouldBeFilledBySender = False
                          , scfPlacements             = p
                          }

instance Arbitrary SignatoryPersonalNumberField where
  arbitrary = do
    v <- arbText 1 100
    p <- arbitrary
    return $ PersonalNumberField { spnfID                     = unsafeSignatoryFieldID 0
                                 , spnfValue                  = v
                                 , spnfObligatory             = True
                                 , spnfShouldBeFilledBySender = False
                                 , spnfPlacements             = p
                                 }

instance Arbitrary SignatoryCompanyNumberField where
  arbitrary = do
    v <- arbText 1 100
    p <- arbitrary
    return $ CompanyNumberField { scnfID                     = unsafeSignatoryFieldID 0
                                , scnfValue                  = v
                                , scnfObligatory             = True
                                , scnfShouldBeFilledBySender = False
                                , scnfPlacements             = p
                                }

instance Arbitrary SignatoryTextField where
  arbitrary = do
    l                   <- arbText 1 20
    v                   <- arbText 1 100
    filled              <- arbitrary
    p                   <- arbitrary
    usecustomvalidation <- arbitrary
    valpattern          <- arbText 1 20
    valexample          <- arbText 1 20
    valtooltip          <- arbText 1 200
    return $ TextField
      { stfID                     = unsafeSignatoryFieldID 0
      , stfName                   = l
      , stfFilledByAuthor         = filled
      , stfValue                  = v
      , stfObligatory             = True
      , stfShouldBeFilledBySender = False
      , stfPlacements             = p
      , stfCustomValidation       =
        case usecustomvalidation of
          False -> Nothing
          True  -> Just $ TextCustomValidation valpattern valexample valtooltip
      }

instance Arbitrary AuthenticationToSignMethod where
  arbitrary = elements [toEnum 0 ..]

instance Arbitrary AuthenticationToViewMethod where
  arbitrary = elements [toEnum 0 ..]

instance Arbitrary DeliveryMethod where
  arbitrary = elements [EmailDelivery, PadDelivery]

instance Arbitrary UserInfo where
  arbitrary = do
    fn <- arbitraryUnicodeText
    ln <- arbitraryUnicodeText
    pn <- arbitraryUnicodeText
    em <- arbEmail

    return $ UserInfo { userfstname         = fn
                      , usersndname         = ln
                      , userpersonalnumber  = pn
                      , usercompanyposition = ""
                      , userphone           = ""
                      , useremail           = Email em
                      }

instance Arbitrary Scrypt.EncryptedPass where
  arbitrary = do
    salt <- vectorOf 32 arbitrary
    k    <- choose (8, 32)
    pass <- vectorOf k arbitrary
    let salt' = Scrypt.Salt . BS8.pack $ salt
        pass' = Scrypt.Pass . BS.fromString $ pass
    return $ Scrypt.encryptPass' salt' pass'

instance Arbitrary Password where
  arbitrary = oneof [Password <$> arbitrary <*> arbitrary]

instance Arbitrary SignupMethod where
  arbitrary = elements [AccountRequest, ViralInvitation, ByAdmin, CompanyInvitation]

instance Arbitrary UserSettings where
  arbitrary = UserSettings <$> arbitrary <*> arbitrary

instance Arbitrary User where
  arbitrary =
    User
      <$> arbitrary
      <*> arbitrary    -- Messes with tests if these are set:
      <*> pure Nothing -- usertotp
      <*> pure False   -- usertotpactive
      <*> pure False   -- usertotpismandatory
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure (unsafeBrandedDomainID 0)
      <*> pure (unsafeUserGroupID 0)
      <*> pure Nothing

instance Arbitrary CgiGrpTransaction where
  arbitrary =
    CgiGrpSignTransaction
      <$> arbitrary
      <*> (fromSNN <$> arbitrary)
      <*> (fromSNN <$> arbitrary)
      <*> (fromSNN <$> arbitrary)
      <*> (pure tempSessionID)

instance Arbitrary CGISEBankIDSignature where
  arbitrary =
    CGISEBankIDSignature
      <$> (C.renderXMLContent <$> arbitrary)
      <*> (C.renderXMLContent <$> arbitrary)
      <*> (C.renderXMLContent <$> arbitrary)
      <*> (C.renderXMLContent <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

instance Arbitrary NetsNOBankIDSignature where
  arbitrary =
    NetsNOBankIDSignature
      <$> (arbText 20 30)
      <*> (arbText 200 300)
      <*> (arbText 10 20)
      <*> (arbText 11 11)

instance Arbitrary NetsDKNemIDSignature where
  arbitrary =
    NetsDKNemIDSignature
      <$> (arbText 20 30)
      <*> (arbText 200 300)
      <*> (arbText 10 20)
      <*> (arbText 10 10)
      <*> (arbText 10 10)

instance Arbitrary ESignature where
  arbitrary = oneof
    [ CGISEBankIDSignature_ <$> arbitrary
    , NetsNOBankIDSignature_ <$> arbitrary
    , NetsDKNemIDSignature_ <$> arbitrary
    ]

-- generate (byte)strings without \NUL in them since
-- hdbc-postgresql plays around with these chars and
-- fucks them up
instance Arbitrary BS.ByteString where
  arbitrary = BS.pack . map fromNNW8 <$> arbitrary

instance Arbitrary StringNoNUL where
  arbitrary = StringNoNUL . T.pack . map (chr . fromIntegral . fromNNW8) <$> arbitrary

arbString :: Int -> Int -> Gen String
arbString minl maxl = do
  l <- choose (minl, maxl)
  vectorOf l $ elements ['a' .. 'z']

arbText :: Int -> Int -> Gen Text
arbText = fmap (fmap T.pack) . arbString

arbEmail :: Gen Text
arbEmail = do
  n <- arbText 1 34
  d <- arbText 3 7
  t <- arbText 2 4
  return $ n <> "@" <> d <> "." <> t

signatoryLinkExample1 :: SignatoryLink
signatoryLinkExample1 = defaultSignatoryLink
  { signatorylinkid              = unsafeSignatoryLinkID 0
  , maybesignatory               = Nothing
  , maybesigninfo                = Just $ SignInfo unixEpoch noIP
  , maybeseeninfo                = Just $ SignInfo unixEpoch noIP
  , maybereadinvite              = Nothing
  , mailinvitationdeliverystatus = Delivered
  , smsinvitationdeliverystatus  = Delivered
  , signatorylinkdeleted         = Nothing
  , signatorylinkreallydeleted   = Nothing
  , signatoryisauthor            = False
  , signatoryrole                = SignatoryRoleSigningParty
  , signatorysignorder           = SignOrder 1
  , signatoryfields              = [ fieldForTests (NameFI $ NameOrder 1) "Eric"
                                   , fieldForTests (NameFI $ NameOrder 2) "Normand"
                                   , fieldForTests EmailFI "eric@scrive.com"
                                   , fieldForTests CompanyFI              "Scrive"
                                   , fieldForTests CompanyNumberFI        "1234"
                                   , fieldForTests PersonalNumberFI       "9101112"
                                   , fieldForTests (TextFI "phone")       "504-302-3742"
                                   ]
  , signatorylinkcsvupload       = Nothing
  , signatoryattachments         = []
  , signatorylinksignredirecturl = Nothing
  , signatorylinkrejectiontime   = Nothing
  , signatorylinkrejectionreason = Nothing
  , signatorylinkauthenticationtosignmethod = StandardAuthenticationToSign
  }

testThat :: String -> TestEnvSt -> TestEnv () -> Test
testThat s env test = testCase s $ do
  ((), diff) <- timed $ runTestEnv env test
  modifyMVar_ (env ^. #testDurations) $ \td -> return $ (diff, s) : td

compareTime :: UTCTime -> UTCTime -> Bool
compareTime (UTCTime da ta) (UTCTime db tb) =
  (da == db)
    && (  ((ta + picosecondsToDiffTime (10 ^ 9) >= tb) && (ta <= tb))
       || ((tb + picosecondsToDiffTime (10 ^ 9)) >= ta && (ta >= tb))
       )

addNewUserGroup' :: Bool -> TestEnv UserGroup
addNewUserGroup' createFolder = addNewUserGroupWithParent createFolder Nothing

addNewUserGroup :: TestEnv UserGroup
addNewUserGroup = addNewUserGroupWithParent False Nothing

addNewUserGroupWithParent :: Bool -> Maybe UserGroupID -> TestEnv UserGroup
addNewUserGroupWithParent createFolder mparent = do
  mUgFolderID <- case createFolder of
    False -> return Nothing
    True  -> fmap (Just . folderID) . dbUpdate $ FolderCreate defaultFolder
  ugname           <- rand 10 (arbText 3 30)
  ugacompanynumber <- rand 10 (arbText 3 30)
  ugaentityname    <- rand 10 (arbText 3 30)
  ugaaddress       <- rand 10 (arbText 3 30)
  ugazip           <- rand 10 (arbText 3 30)
  ugacity          <- rand 10 (arbText 3 30)
  ugacountry       <- rand 10 (arbText 3 30)
  let ug = defaultUserGroup
        & (#parentGroupID .~ mparent)
        & (#name          .~ ugname)
        & (#address       .~ uga)
        & (#homeFolderID  .~ mUgFolderID)
      uga = Just $ I.UserGroupAddress { ugaCompanyNumber = ugacompanynumber
                                      , ugaEntityName    = ugaentityname
                                      , ugaAddress       = ugaaddress
                                      , ugaZip           = ugazip
                                      , ugaCity          = ugacity
                                      , ugaCountry       = ugacountry
                                      }
  dbUpdate . UserGroupCreate $ ug

addNewUserGroupWithParents :: Bool -> TestEnv UserGroupWithParents
addNewUserGroupWithParents createFolder = do
  ug <- addNewUserGroup' createFolder
  guardJustM . dbQuery . UserGroupGetWithParents $ ug ^. #id

addNewRandomFile
  :: ( CryptoRNG m
     , MonadBase IO m
     , MonadCatch m
     , MonadDB m
     , MonadFileStorage m
     , MonadLog m
     , MonadThrow m
     , MonadTime m
     )
  => m FileID
addNewRandomFile = do
  fn <- rand 1 $ elements
    [ inTestDir "pdfs/simple.pdf"
    , inTestDir "pdfs/telia.pdf"
    , inTestDir "pdfs/hp-designjet.pdf"
    , inTestDir "pdfs/visa-application.pdf"
    ]
  cnt <- liftBase $ BS.readFile fn
  saveNewFile (T.pack fn) cnt

addNewUser
  :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => Text
  -> Text
  -> Text
  -> m (Maybe User)
addNewUser firstname secondname email = do
  fmap fst <$> addNewUserWithCompany firstname secondname email True

addNewUserWithCompany
  :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => Text
  -> Text
  -> Text
  -> Bool
  -> m (Maybe (User, UserGroupID))
addNewUserWithCompany firstname secondname email createFolders = do
  ug    <- addNewCompany createFolders
  mUser <- addNewCompanyAdminUser firstname secondname email (ug ^. #id)
  return $ (, ug ^. #id) <$> mUser

addNewCompany :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m) => Bool -> m UserGroup
addNewCompany createFolders = do
  mUgFolder <- case createFolders of
    False -> return Nothing
    True  -> fmap Just . dbUpdate $ FolderCreate defaultFolder
  dbUpdate
    . UserGroupCreate
    . set #homeFolderID (folderID <$> mUgFolder)
    $ defaultUserGroup

createNewUser
  :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => (Text, Text)
  -> Text
  -> Maybe Password
  -> (UserGroupID, Bool)
  -> Lang
  -> BrandedDomainID
  -> SignupMethod
  -> m (Maybe User)
createNewUser names email mPasswd (ugid, isCompanyAdmin) lang bdID sm = do
  -- create User home Folder, if the UserGroup has one
  mUserFolder <- dbQuery (FolderGetUserGroupHome ugid) >>= \case
    Nothing -> return Nothing
    Just ugFolder ->
      fmap Just
        . dbUpdate
        . FolderCreate
        . set #folderParentID (Just $ folderID ugFolder)
        $ defaultFolder
  dbUpdate $ AddUser names
                     email
                     mPasswd
                     (ugid, folderID <$> mUserFolder, isCompanyAdmin)
                     lang
                     bdID
                     sm

-- | Create a new user and add it to a company as a non-admin.
addNewCompanyUser
  :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => Text
  -> Text
  -> Text
  -> UserGroupID
  -> m (Maybe User)
addNewCompanyUser = addNewCompanyUser' DontMakeAdmin

-- | Create a new user and add it to a company as an admin.
addNewCompanyAdminUser
  :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => Text
  -> Text
  -> Text
  -> UserGroupID
  -> m (Maybe User)
addNewCompanyAdminUser = addNewCompanyUser' MakeAdmin

data MakeAdmin = DontMakeAdmin | MakeAdmin
  deriving Eq

-- | Create a new user and add it to a company as either an admin or a
-- non-admin.
addNewCompanyUser'
  :: (MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => MakeAdmin
  -> Text
  -> Text
  -> Text
  -> UserGroupID
  -> m (Maybe User)
addNewCompanyUser' makeAdmin firstname secondname email ugid = do
  bd <- dbQuery $ GetMainBrandedDomain
  dbQuery (UserGroupGet ugid) >>= \case
    Nothing  -> return Nothing
    Just _ug -> createNewUser (firstname, secondname)
                              email
                              Nothing
                              (ugid, makeAdmin == MakeAdmin)
                              defaultLang
                              (bd ^. #id)
                              CompanyInvitation

-- | Create user and add it to a new user group as admin.
addNewAdminUserAndUserGroup :: Text -> Text -> Text -> TestEnv (User, UserGroup)
addNewAdminUserAndUserGroup firstname secondname email = do
  ug        <- addNewUserGroup' True
  bd        <- dbQuery $ GetMainBrandedDomain
  Just user <- createNewUser (firstname, secondname)
                             email
                             Nothing
                             (ug ^. #id, True)
                             defaultLang
                             (bd ^. #id)
                             CompanyInvitation
  return (user, ug)

addNewUserToUserGroup :: Text -> Text -> Text -> UserGroupID -> TestEnv (Maybe User)
addNewUserToUserGroup firstname secondname email ugid = do
  bd <- dbQuery $ GetMainBrandedDomain
  dbQuery (UserGroupGet ugid) >>= \case
    Nothing  -> return Nothing
    Just _ug -> createNewUser (firstname, secondname)
                              email
                              Nothing
                              (ugid, True)
                              defaultLang
                              (bd ^. #id)
                              CompanyInvitation

addNewUserFromInfo
  :: UserInfo
  -> (CryptoRNG m, MonadDB m, MonadFail m, MonadThrow m, MonadLog m, MonadMask m)
  => m User
addNewUserFromInfo userInfo@(UserInfo { userfstname = firstName, usersndname = lastName, useremail = Email { unEmail = email } })
  = do
    Just user <- addNewUser firstName lastName email
    void $ dbUpdate $ SetUserInfo (userid user) userInfo
    return user

randomPersonalNumber :: CryptoRNG m => m Text
randomPersonalNumber = rand 10 $ arbText 3 30

randomUserInfo :: CryptoRNG m => m UserInfo
randomUserInfo = do
  fn               <- rand 10 $ arbText 3 30
  ln               <- rand 10 $ arbText 3 30
  em               <- rand 10 arbEmail
  -- change the user to have some distinct personal information
  personal_number  <- randomPersonalNumber
  company_position <- rand 10 $ arbText 3 30
  phone            <- rand 10 $ arbText 3 30
  return UserInfo { userfstname         = fn
                  , usersndname         = ln
                  , userpersonalnumber  = personal_number
                  , usercompanyposition = company_position
                  , userphone           = phone
                  , useremail           = Email em
                  }

addNewRandomUser
  :: (CryptoRNG m, MonadDB m, MonadFail m, MonadThrow m, MonadLog m, MonadMask m)
  => m User
addNewRandomUser = do
  randomUserInfo >>= addNewUserFromInfo

addNewRandomUserWithCompany
  :: (CryptoRNG m, MonadDB m, MonadFail m, MonadThrow m, MonadLog m, MonadMask m)
  => m (User, UserGroupID)
addNewRandomUserWithCompany = addNewRandomUserWithCompany' True

addNewRandomUserWithCompany'
  :: (CryptoRNG m, MonadDB m, MonadFail m, MonadThrow m, MonadLog m, MonadMask m)
  => Bool
  -> m (User, UserGroupID)
addNewRandomUserWithCompany' createFolders = do
  fn                <- rand 10 $ arbText 3 30
  ln                <- rand 10 $ arbText 3 30
  em                <- rand 10 arbEmail
  Just (user, ugid) <- addNewUserWithCompany fn ln em createFolders
  -- change the user to have some distinct personal information
  personal_number   <- rand 10 $ arbText 3 30
  company_position  <- rand 10 $ arbText 3 30
  phone             <- rand 10 $ arbText 3 30
  let userinfo = UserInfo { userfstname         = fn
                          , usersndname         = ln
                          , userpersonalnumber  = personal_number
                          , usercompanyposition = company_position
                          , userphone           = phone
                          , useremail           = Email em
                          }
  void $ dbUpdate $ SetUserInfo (userid user) userinfo
  return (user, ugid)

addNewRandomUserWithPassword
  :: (CryptoRNG m, MonadDB m, MonadFail m, MonadThrow m, MonadLog m, MonadMask m)
  => Text
  -> m User
addNewRandomUserWithPassword password = do
  -- create random user
  randomUser   <- addNewRandomUser
  -- set his password
  passwordhash <- createPassword password
  void $ dbUpdate $ SetUserPassword (userid randomUser) passwordhash
  return randomUser

addNewRandomCompanyUser :: UserGroupID -> Bool -> TestEnv User
addNewRandomCompanyUser ugid isadmin = do
  User { userid } <- addNewRandomUser
  void $ dbUpdate $ SetUserUserGroup userid ugid
  void $ dbUpdate $ SetUserCompanyAdmin userid isadmin
  Just user <- dbQuery $ GetUserByID userid
  return user

addNewRandomUserGroupUser :: UserGroupID -> Bool -> TestEnv User
addNewRandomUserGroupUser ugid isadmin = do
  User { userid } <- addNewRandomUser
  void $ dbUpdate $ SetUserUserGroup userid ugid
  void $ dbUpdate $ SetUserCompanyAdmin userid isadmin
  Just user <- dbQuery $ GetUserByID userid
  return user

addNewRandomPartnerUser :: TestEnv (User, UserGroup)
addNewRandomPartnerUser = do
  -- To use UserGroups as if they are Partners, we need to generate a
  -- UserGroupID which is not a PartnerID.
  partners <- dbQuery GetPartners
  -- Try to generate a userGroup and always check whether the ugid is
  -- already a partnerID.
  mresult  <- (\folder -> foldM folder Nothing [1 .. 100]) $ \mres _counter -> do
    case mres of
      Just res -> return . Just $ res
      Nothing  -> do
        partnerAdminUser      <- addNewRandomUser
        partnerAdminUserGroup <- dbQuery $ UserGroupGetByUserID (userid partnerAdminUser)
        let partnerIDAlreadyExists =
              (unsafeUserGroupIDToPartnerID $ partnerAdminUserGroup ^. #id)
                `elem` map ptID partners
        case partnerIDAlreadyExists of
          True  -> return Nothing
          False -> return $ Just (partnerAdminUser, partnerAdminUserGroup)
  case mresult of
    Nothing -> unexpectedError "UserGroupID - PartnerID collision"
    Just (partnerAdminUser, partnerAdminUserGroup) -> do
      -- insert new partner row with the same ID as the UserGroup
      True <- dbUpdate . InsertPartnerForTests $ Partner
        { ptID             = unsafeUserGroupIDToPartnerID $ partnerAdminUserGroup ^. #id
        , ptName           = T.unpack $ partnerAdminUserGroup ^. #name
        , ptDefaultPartner = False
        , ptUserGroupID    = Just $ partnerAdminUserGroup ^. #id
        }
      let uid  = userid partnerAdminUser
          ugid = partnerAdminUserGroup ^. #id
      void . dbUpdate . AccessControlCreateForUser uid $ UserGroupAdminAR ugid
      return (partnerAdminUser, partnerAdminUserGroup)

newtype OneOf a = OneOf { fromOneOf :: [a] }
  deriving (Eq, Show)

newtype AllOf a = AllOf { fromAllOf :: [a] }
  deriving (Eq, Show)

-- | Choose one of sets of signatory conditions at random.
type RandomSignatorySpec = OneOf (AllOf RandomSignatoryCondition)

-- | Generate list of alternatives for signatories where one of them has a
-- bounded spec and the rest are free.
anyRandomSignatoryCondition
  :: Int -> Int -> RandomSignatorySpec -> OneOf [RandomSignatorySpec]
anyRandomSignatoryCondition a b cond =
  let freeSig = OneOf [AllOf []]
  in  OneOf $ do
        n <- [a .. b]
        k <- [1 .. n]
        return $ replicate (k - 1) freeSig ++ [cond] ++ replicate (n - k) freeSig

data RandomSignatoryCondition
  = RSC_IsSignatory
  | RSC_IsSignatoryThatHasSigned
  | RSC_IsSignatoryThatHasntSigned
  | RSC_IsApprover
  | RSC_IsApproverThatHasApproved
  | RSC_IsApproverThatHasntApproved
  | RSC_IsViewer
  | RSC_AuthToSignIs AuthenticationToSignMethod
  | RSC_AuthToViewIs AuthenticationToViewMethod
  | RSC_DeliveryMethodIs DeliveryMethod
  | RSC_HasConsentModule Bool
  | RSC_HasReadInvite Bool
  deriving (Eq, Show)

data RandomDocumentAllows = RandomDocumentAllows
  { rdaTypes       :: OneOf DocumentType
  , rdaStatuses    :: OneOf DocumentStatus
  , rdaSharings    :: OneOf DocumentSharing
  , rdaSignatories :: OneOf [RandomSignatorySpec]
  , rdaAuthor      :: User
  , rdaSharedLink  :: Bool
  , rdaTimeoutTime :: Bool
  , rdaTemplateId  :: Maybe DocumentID
  }

rdaDefault :: User -> RandomDocumentAllows
rdaDefault user = RandomDocumentAllows
  { rdaTypes       = OneOf documentAllTypes
  , rdaStatuses    = OneOf documentAllStatuses
  , rdaSharings    = OneOf documentAllSharings
  , rdaSignatories = OneOf $ map (`replicate` freeSignatory) [1 .. 10]
  , rdaAuthor      = user
  , rdaSharedLink  = False
  , rdaTimeoutTime = True
  , rdaTemplateId  = Nothing
  }
  where
    freeSignatory :: OneOf (AllOf RandomSignatoryCondition)
    freeSignatory = OneOf [AllOf []]

randomSigLinkByStatus :: DocumentStatus -> Gen SignatoryLink
randomSigLinkByStatus Closed = do
  sl <- arbitrary
  if signatoryrole sl `elem` [SignatoryRoleApprover, SignatoryRoleSigningParty]
    then do
      (sign, seen) <- arbitrary
      return sl { maybesigninfo = Just sign, maybeseeninfo = Just seen }
    else return sl { maybesigninfo = Nothing }

randomSigLinkByStatus Preparation = do
  (sl) <- arbitrary
  return $ sl { maybesigninfo = Nothing, maybeseeninfo = Nothing }
randomSigLinkByStatus Pending = do
  (sl) <- arbitrary
  return $ sl { maybesigninfo = Nothing, maybeseeninfo = Nothing }
randomSigLinkByStatus _ = arbitrary

randomAuthorLinkByStatus :: DocumentStatus -> Gen SignatoryLink
randomAuthorLinkByStatus Closed = do
  sl <- arbitrary
  rl <- elements [SignatoryRoleViewer, SignatoryRoleSigningParty]
  if rl == SignatoryRoleSigningParty
    then do
      (sign, seen) <- arbitrary
      return sl { maybesigninfo     = Just sign
                , maybeseeninfo     = Just seen
                , signatoryisauthor = True
                , signatoryrole     = rl
                }
    else do
      seen <- arbitrary
      return sl { maybesigninfo     = Nothing
                , maybeseeninfo     = Just seen
                , signatoryisauthor = True
                , signatoryrole     = rl
                }
randomAuthorLinkByStatus Preparation = do
  rl <- elements [SignatoryRoleViewer, SignatoryRoleSigningParty]
  sl <- arbitrary
  return $ sl { maybesigninfo     = Nothing
              , maybeseeninfo     = Nothing
              , signatoryisauthor = True
              , signatoryrole     = rl
              }
randomAuthorLinkByStatus Pending = do
  rl <- elements [SignatoryRoleViewer, SignatoryRoleSigningParty]
  sl <- arbitrary
  return $ sl { maybesigninfo     = Nothing
              , maybeseeninfo     = Nothing
              , signatoryisauthor = True
              , signatoryrole     = rl
              }
randomAuthorLinkByStatus _ = do
  rl <- elements [SignatoryRoleViewer, SignatoryRoleSigningParty]
  sl <- arbitrary
  return $ if rl == SignatoryRoleSigningParty
    then sl { signatoryisauthor = True, signatoryrole = rl }
    else sl { maybesigninfo = Nothing, signatoryisauthor = True, signatoryrole = rl }

addRandomDocumentWithAuthor :: User -> TestEnv DocumentID
addRandomDocumentWithAuthor user = documentid <$> addRandomDocumentWithAuthor' user

addRandomDocumentWithAuthor' :: User -> TestEnv Document
addRandomDocumentWithAuthor' user = addRandomDocument (rdaDefault user)

addRandomDocumentFromShareableLinkWithTemplateId :: User -> DocumentID -> TestEnv Document
addRandomDocumentFromShareableLinkWithTemplateId user templateId =
  addRandomDocument
    $ (rdaDefault user) { rdaSharedLink = True, rdaTemplateId = Just templateId }

addRandomDocument :: RandomDocumentAllows -> TestEnv Document
addRandomDocument rda = do
  file <- addNewRandomFile
  addRandomDocumentWithFile file rda

addRandomDocumentWithFile :: FileID -> RandomDocumentAllows -> TestEnv Document
addRandomDocumentWithFile fileid rda = do
  now <- currentTime
  let user = rdaAuthor rda
  file     <- dbQuery $ GetFileByFileID fileid
  --liftIO $ print $ "about to generate document"
  document <- worker now user file
  docid    <- dbUpdate $ StoreDocumentForTesting document
  dbQuery $ GetDocumentByDocumentID docid
  where
    worker now user file = do
      doc'   <- rand 10 arbitrary
      xtype  <- rand 10 (elements . fromOneOf $ rdaTypes rda)
      status <- if xtype == Template
        then return Preparation
        else rand 10 (elements . fromOneOf $ rdaStatuses rda)
      sharing <- if xtype == Template
        then rand 10 (elements . fromOneOf $ rdaSharings rda)
        else return Private
      title    <- rand 1 $ arbText 10 25
      sigspecs <- rand 10 (elements . fromOneOf $ rdaSignatories rda)
      -- First signatory link is the author
      let genFuns = randomAuthorLinkByStatus : repeat randomSigLinkByStatus
      asl' : siglinks <- forM (zip sigspecs genFuns) $ \(OneOf sigconds, genSigLink) -> do
        sigcond <- rand 10 $ elements sigconds
        siglink <- rand 10 $ genSigLink status
        let
          updateSeenSignInfos Nothing role sig = return sig { signatoryrole = role }
          updateSeenSignInfos (Just hasSignInfo) role sig = if hasSignInfo
            then do
              signinfo <- rand 10 arbitrary
              seeninfo <- maybe (rand 10 arbitrary) return (maybeseeninfo sig)
              return sig { signatoryrole = role
                         , maybesigninfo = Just signinfo
                         , maybeseeninfo = Just seeninfo
                         }
            else return sig { signatoryrole = role, maybesigninfo = Nothing }
          applyCond = \case
            RSC_IsSignatory ->
              \sig -> updateSeenSignInfos Nothing SignatoryRoleSigningParty sig
            RSC_IsSignatoryThatHasSigned ->
              \sig -> updateSeenSignInfos (Just True) SignatoryRoleSigningParty sig
            RSC_IsSignatoryThatHasntSigned ->
              \sig -> updateSeenSignInfos (Just False) SignatoryRoleSigningParty sig
            RSC_IsApprover ->
              \sig -> updateSeenSignInfos Nothing SignatoryRoleApprover sig
            RSC_IsApproverThatHasApproved ->
              \sig -> updateSeenSignInfos (Just True) SignatoryRoleApprover sig
            RSC_IsApproverThatHasntApproved ->
              \sig -> updateSeenSignInfos (Just False) SignatoryRoleApprover sig
            RSC_IsViewer -> \sig -> updateSeenSignInfos Nothing SignatoryRoleViewer sig
            RSC_AuthToSignIs authToSign -> \sig -> do
              return $ sig { signatorylinkauthenticationtosignmethod = authToSign }
            RSC_AuthToViewIs authToView -> \sig -> do
              return $ sig { signatorylinkauthenticationtoviewmethod = authToView }
            RSC_DeliveryMethodIs deliveryMethod -> \sig -> do
              return $ sig { signatorylinkdeliverymethod = deliveryMethod }
            RSC_HasConsentModule hasModule -> \sig -> if hasModule
              then do
                (cmTitle, cmQuestions) <- rand 10 arbitraryConsentModule
                return sig { signatorylinkconsenttitle     = cmTitle
                           , signatorylinkconsentquestions = cmQuestions
                           }
              else return sig { signatorylinkconsenttitle     = Nothing
                              , signatorylinkconsentquestions = []
                              }
            RSC_HasReadInvite readInvite -> \sig -> if readInvite
              then do
                timeReadInvite <- rand 10 arbitrary
                return sig { maybereadinvite = Just timeReadInvite }
              else return sig { maybereadinvite = Nothing }

        foldrM applyCond siglink (fromAllOf sigcond)

      let
        doc = doc'
          { documenttype              = xtype
          , documentstatus            = status
          , documentsharing           = sharing
          , documenttitle             = title
          , documentfromshareablelink = rdaSharedLink rda
          , documenttemplateid        = rdaTemplateId rda
          , documentfolderid          = userhomefolderid user
          , documenttimeouttime       = if rdaTimeoutTime rda
                                          then documenttimeouttime doc'
                                          else Nothing
          }
      userDetails <- signatoryFieldsFromUser user
      let asl =
            asl' { maybesignatory = Just (userid user), signatoryfields = userDetails }

      let alllinks = asl : siglinks


      let closedfile = if documentstatus doc == Closed
            then [MainFile fileid Closed Missing (T.unpack $ filename file)]
            else []
      let adoc = doc
            { documentsignatorylinks = alllinks
            , documentlang           = getLang user
            , documentmainfiles      = closedfile
                                         <> [ MainFile fileid
                                                       Preparation
                                                       Missing
                                                       (T.unpack $ filename file)
                                            ]
            }

      case invariantProblems now adoc of
        Nothing        -> return adoc
        Just _problems -> do
          rej <- gview #rejectedDocuments
          modifyMVar_ rej $ \i -> return $! i + 1
          -- Invariant problems might happen e.g. when all signatories are
          -- picked to have signed, but the document was earlier picked to be in
          -- Pending state. It's quite rare, but does happen.
          --liftIO $ print adoc
          --liftIO $ print $ "rejecting doc: " <> _problems
          worker now user file

-- | Synchronously seal a document.
sealTestDocument :: Context -> DocumentID -> TestEnv ()
sealTestDocument ctx did = void $ TestEnv $ liftTestFileStorageT $ \fsEnv -> do
  cryptoSt <- newCryptoRNGState
  withDocumentID did
    . runGuardTimeConfT (ctx ^. #gtConf)
    . runPdfToolsLambdaT (ctx ^. #pdfToolsLambdaEnv)
    . runTemplatesT (ctx ^. #lang, ctx ^. #globalTemplates)
    . runMailContextT (contextToMailContext ctx)
    . runCryptoRNGT cryptoSt
    . flip runTestFileStorageT fsEnv
    $ do
        res <- postDocumentClosedActions True False
          `catch` \(_ :: KE.KontraError) -> return False
        when res $ do
          extendingJobCount <-
            runSQL $ "SELECT * FROM document_extending_jobs WHERE id =" <?> did
          assertEqual
            "postDocumentClosedActions should add task to\
                      \ document_extending_jobs"
            1
            extendingJobCount
        return res

rand :: CryptoRNG m => Int -> Gen a -> m a
rand i a = do
  stdgn <- random
  return $ unGen a stdgn i

untilCondition :: (Monad m) => (b -> Bool) -> m b -> m b
untilCondition cond gen = do
  v <- gen
  if cond v then return v else untilCondition cond gen

-- Random gen

--Random query
class RandomQuery a b where
  randomQuery :: a -> TestEnv b

instance (DBQuery TestEnv ev res) => RandomQuery ev res where
  randomQuery = dbQuery

instance {-# OVERLAPPING #-}
  (Arbitrary a, RandomQuery c b) =>
  RandomQuery (a -> c) b where

  randomQuery f = do
    a <- rand 10 arbitrary
    randomQuery $ f a

-- | Random update.
class RandomUpdate a b m where
  randomUpdate :: a -> m b

instance (DBUpdate m ev res) => RandomUpdate ev res m where
  randomUpdate = dbUpdate

instance {-# OVERLAPPING #-}
  (CryptoRNG m, Arbitrary a, RandomUpdate c b m) =>
  RandomUpdate (a -> c) b m where

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

-- Versions of assert types from Test.HUnit with a typeclass constraint
-- for convenience.

assert :: (T.Assertable t, MonadIO m) => t -> m ()
assert = liftIO . T.assert

assertBool :: MonadIO m => String -> Bool -> m ()
assertBool msg = liftIO . T.assertBool msg

assertEqual :: (Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
assertEqual msg a = liftIO . T.assertEqual msg a

assertApproxEqual :: (Num a, Ord a, MonadIO m) => String -> a -> a -> a -> m ()
assertApproxEqual message expected epsilon actual =
  assertBool message $ (actual <= expected + epsilon) && (actual > expected - epsilon)

assertFailure :: MonadIO m => String -> m ()
assertFailure = liftIO . T.assertFailure

assertString :: MonadIO m => String -> m ()
assertString = liftIO . T.assertString

assertionPredicate :: (T.AssertionPredicable t, MonadIO m) => t -> m Bool
assertionPredicate = liftIO . T.assertionPredicate

-- Our asserts.

assertSuccess :: MonadIO m => m ()
assertSuccess = assertBool "not success?!" True

assertJust :: MonadIO m => Maybe a -> m ()
assertJust (Just _) = assertSuccess
assertJust Nothing  = assertFailure "Should have returned Just but returned Nothing"

assertJustAndExtract :: MonadIO m => Maybe a -> m a
assertJustAndExtract (Just x) = do
  assertSuccess
  return x
assertJustAndExtract Nothing =
  liftIO $ T.assertFailure "Should have returned Just but returned Nothing"

assertRight :: (Show a, MonadIO m) => Either a b -> m ()
assertRight (Right _) = assertSuccess
assertRight (Left a) =
  assertFailure $ "Should have return Right but returned Left " <> show a

assertLeft :: MonadIO m => Either a b -> m ()
assertLeft (Left _) = assertSuccess
assertLeft _        = assertFailure "Should have returned Left but returned Right"

assertNothing :: MonadIO m => Maybe a -> m ()
assertNothing Nothing  = assertSuccess
assertNothing (Just _) = assertFailure "Should have returned Nothing but returned Just"

assertNotEqual :: (Eq a, Show a, MonadIO m) => String -> a -> a -> m ()
assertNotEqual msg expected got = unless (expected /= got) $ do
  assertFailure . unlines $ [msg, "", "Expected: " <> show expected, "Got: " <> show got]

assertEqualJson :: MonadIO m => String -> A.Value -> A.Value -> m ()
assertEqualJson msg expected got = unless (expected == got) $ do
  assertFailure
    .  unlines
    $  [ msg
       , ""
       , "Expected: " <> show expected
       , "Got: " <> show got
       , ""
       , "Steps to go from the expected value to the one we got:"
       ]
    <> map ((" * " <>) . show) (A.patchOperations $ A.diff expected got)
    <> ["", "Steps to go from the value we got to the expected one:"]
    <> map ((" * " <>) . show) (A.patchOperations $ A.diff got expected)

assertRaisesInternalError :: (Show v, MonadIO m, MonadMask m) => m v -> m ()
assertRaisesInternalError a = catchJust
  (\case
    KE.Respond404      -> Nothing
    KE.InternalError _ -> Just ()
    KE.LinkInvalid     -> Nothing
  )
  (a >>= assertFailure . ("Expecting InternalError but got " <>) . show)
  return

assertRaisesDBException :: (Show v, MonadIO m, MonadMask m) => m v -> m ()
assertRaisesDBException a =
  (a >>= (\v -> assertFailure $ "Expecting db exception but got " <> show v))
    `catches` [Handler $ \_e@DBException {..} -> return ()]


assertRaisesKontra
  :: forall e v m
   . (DBExtraException e, Show v, MonadIO m, MonadMask m)
  => (e -> Bool)
  -> m v
  -> m ()
assertRaisesKontra correctException action =
  (action >>= \r ->
      assertString
        $  "Expected DBExtraException "
        <> typeOfE
        <> ", instead returned result "
        <> show r
    )
    `catches` [ Handler helper
    -- Also support DBExtraException nested within DBException
              , Handler $ \e@DBException {..} -> case cast dbeError of
                Just e' -> helper e'
                Nothing -> invExc e
              ]
  where
    helper (SomeDBExtraException e) = case cast e of
      Just e' -> if correctException e'
        then return ()
        else
          assertString $ "DBExtraException " <> typeOfE <> " is not correct " <> show e'
      Nothing -> invExc e

    invExc :: (Show a, Typeable a) => a -> m ()
    invExc e =
      assertString
        $  "Expected DBExtraException "
        <> typeOfE
        <> ", instead got exception "
        <> show e

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

-- A simple way of creating signatory fields. Since the data type is
-- big, we want to skip many details in many tests.
fieldForTests :: FieldIdentity -> Text -> SignatoryField
fieldForTests (NameFI no) v = SignatoryNameField $ NameField
  { snfID                     = (unsafeSignatoryFieldID 0)
  , snfNameOrder              = no
  , snfValue                  = v
  , snfObligatory             = True
  , snfShouldBeFilledBySender = False
  , snfPlacements             = []
  }
fieldForTests CompanyFI v = SignatoryCompanyField $ CompanyField
  { scfID                     = (unsafeSignatoryFieldID 0)
  , scfValue                  = v
  , scfObligatory             = True
  , scfShouldBeFilledBySender = False
  , scfPlacements             = []
  }
fieldForTests PersonalNumberFI v = SignatoryPersonalNumberField $ PersonalNumberField
  { spnfID                     = (unsafeSignatoryFieldID 0)
  , spnfValue                  = v
  , spnfObligatory             = True
  , spnfShouldBeFilledBySender = False
  , spnfPlacements             = []
  }
fieldForTests CompanyNumberFI v = SignatoryCompanyNumberField $ CompanyNumberField
  { scnfID                     = (unsafeSignatoryFieldID 0)
  , scnfValue                  = v
  , scnfObligatory             = True
  , scnfShouldBeFilledBySender = False
  , scnfPlacements             = []
  }
fieldForTests EmailFI v = SignatoryEmailField $ EmailField
  { sefID                     = (unsafeSignatoryFieldID 0)
  , sefValue                  = v
  , sefObligatory             = True
  , sefShouldBeFilledBySender = False
  , sefEditableBySignatory    = False
  , sefPlacements             = []
  }
fieldForTests MobileFI v = SignatoryMobileField $ MobileField
  { smfID                     = (unsafeSignatoryFieldID 0)
  , smfValue                  = v
  , smfObligatory             = True
  , smfShouldBeFilledBySender = False
  , smfEditableBySignatory    = False
  , smfPlacements             = []
  }
fieldForTests (TextFI l) v = SignatoryTextField $ TextField
  { stfID                     = (unsafeSignatoryFieldID 0)
  , stfName                   = l
  , stfFilledByAuthor         = True
  , stfValue                  = v
  , stfObligatory             = True
  , stfShouldBeFilledBySender = False
  , stfPlacements             = []
  , stfCustomValidation       = Nothing
  }
fieldForTests _ _ =
  unexpectedError "Can't use signature or checkbox fields with this function"


assertSQLCount :: String -> Int64 -> SQL -> TestEnv ()
assertSQLCount msg expectedCount sql = do
  runSQL_ sql
  count <- fetchOne runIdentity
  assertEqual msg expectedCount count
