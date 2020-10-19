{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module TestingUtil
  ( arbitraryText
  , genUnicodeString
  , genMaybeUnicodeString
  , extendRandomness
  , arbitraryAuthorActor
  , arbitrarySystemActor
  , arbitrarySignatoryActor
  , documentSignableTypes
  , documentTemplateTypes
  , nonemptybs
  , signatoryLinkExample1
  , testThat
  , compareTime
  , addNewRandomFile
  , UserGroupTemplate(..)
  , randomUserGroupTemplate
  , instantiateUserGroup
  , instantiateRandomUserGroup
  , UserTemplate(..)
  , randomUserTemplate
  , instantiateRandomPadesUser
  , instantiateRandomUser
  , instantiateUser
  , tryInstantiateUser
  , randomPersonalNumber
  , anyRandomSignatoryCondition
  , RandomDocumentAllows(..)
  , rdaDefault
  , randomSigLinkByStatus
  , randomAuthorLinkByStatus
  , documentAllTypes
  , addRandomDocument
  , addRandomDocumentWithAuthor
  , addRandomDocumentWithAuthor'
  , addRandomDocumentWithFile
  , addRandomDocumentFromShareableLinkWithTemplateId
  , sealTestDocument
  , untilCondition
  , assert
  , assertBool
  , assertEqual
  , assertApproxEqual
  , assertFailure
  , assertString
  , assertionPredicate
  , assertSuccess
  , assertJust
  , assertJustAndExtract
  , assertRight
  , assertLeft
  , assertNothing
  , assertNotEqual
  , assertEqualJson
  , assertRaisesInternalError
  , assertRaises404
  , assertRaisesDBException
  , assertRaisesKontra
  , assertSQLCount
  , guardMethodM
  , isFlashOfType
  , getFlashType
  , OneOf(..)
  , SignatoryTemplate
  , randomSignatory
  , documentAllStatuses
  , rand
  , arbText
  , arbString
  , arbitraryName
  , arbitraryUnicodeText
  , randomUpdate
  , fieldForTests
  , StringNoNUL(..)
  , RandomSignatoryCondition(..)
  , randomQuery
  , ArbitraryUnicode(..)
  ) where

import Control.Concurrent.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans
import Crypto.RNG
import Data.Char
import Data.Foldable (foldrM)
import Data.Int
import Data.Set (Set)
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable (cast)
import Data.Word
import Happstack.Server
import Log
import Optics (gview)
import Optics.TH
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
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Test.HUnit as T

import BrandedDomain.BrandedDomain
import BrandedDomain.BrandedDomainID
import BrandedDomain.Model
import Context
import DataRetentionPolicy
import DB
import DigitalSignatureMethod
import Doc.Action
import Doc.DigitalSignatureStatus (DigitalSignatureStatus(Missing))
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Model
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import Doc.TestInvariants
import EID.CGI.GRP.Transaction.Model
import EID.EIDService.Types
import EID.Signature.Model
import FeatureFlags.Model
import File.FileID
import File.Model
import File.Storage
import File.Types (fileid)
import FlashMessage
import Folder.Model
import GuardTime
import IPAddress
import KontraMonad
import Log.Utils
import LoginAuth.LoginAuthMethod
import MagicHash (MagicHash, unsafeMagicHash)
import MailContext
import MinutesTime
import PadApplication.Types
import PdfToolsLambda.Monad
import Session.SessionID
import SMS.Types (SMSProvider(..))
import System.Random.CryptoRNG ()
import Tag
import Templates
import TestKontra
import User.Email
import User.Model
import User.Password.Internal (Password(..))
import UserGroup.FreeDocumentTokens.Model
import UserGroup.Model
import UserGroup.Types
import UserGroup.Types.PaymentPlan
import Util.Actor
import qualified KontraError as KE
import qualified Text.XML.Content as C
import qualified Text.XML.DirtyContent as D
import qualified User.Types.User.Internal
import qualified UserGroup.Internal

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
  arbitrary =
    Folder emptyFolderID Nothing <$> arbitraryName <*> pure Nothing <*> pure Nothing

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
arbitraryUnicodeText = pack <$> listOf (arbitraryUnicodeChar `suchThat` (/= '\0'))

arbitraryText :: Gen Text
arbitraryText = T.pack <$> arbitrary

arbitraryMaybe :: forall a . Gen a -> Gen (Maybe a)
arbitraryMaybe = liftArbitrary

newtype ArbitraryUnicode = ArbitraryUnicode
  { withArbitraryUnicode :: Text
  } deriving (Show, Eq, Ord)

instance Arbitrary ArbitraryUnicode where
  arbitrary = ArbitraryUnicode <$> arbitraryUnicodeText

instance Arbitrary SMSProvider where
  arbitrary = elements [SMSDefault, SMSTeliaCallGuide]

instance Arbitrary PadAppMode where
  arbitrary = elements [ListView, PinCode]

instance Arbitrary PaymentPlan where
  arbitrary = elements [FreePlan, OnePlan, TeamPlan, EnterprisePlan, TrialPlan]

instance Arbitrary DigitalSignatureMethod where
  arbitrary = elements documentAllDigitalSignatureMethods

-- PostgreSQL will not store \NUL in DB
genUnicodeString :: Gen String
genUnicodeString = filter (/= '\NUL') <$> QCU.string

genMaybeUnicodeString :: Gen (Maybe String)
genMaybeUnicodeString = oneof [pure Nothing, Just <$> genUnicodeString]

instance Arbitrary UserGroup where
  arbitrary =
    UserGroup emptyUserGroupID Nothing
      <$> arbitraryName
      <*> pure Nothing
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary UserGroupRoot where
  arbitrary =
    UserGroupRoot emptyUserGroupID
      <$> arbitraryUnicodeText
      <*> pure Nothing
      <*> arbitrary
      <*> arbitrary
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

instance Arbitrary Tag where
  arbitrary = Tag <$> arbitraryUnicodeText <*> arbitraryUnicodeText

instance Arbitrary UserGroupSettings where
  arbitrary =
    UserGroupSettings
      <$> arbitrary
      <*> arbitrary
      <*> arbitraryMaybe arbitraryUnicodeText
      <*> arbitraryMaybe arbitraryUnicodeText
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure False   -- we must explicitly use folder list calls
      <*> arbitrary
      <*> pure False   -- do not enforce 2FA in tests
      <*> pure Nothing -- do not set custom session expiry
      <*> pure Nothing -- no portal url
      <*> pure Nothing -- no custom eidService token
      <*> arbitrary
      <*> pure Nothing -- do not set a custom document session expiry
      <*> pure False   -- do not set force hide personal numbers
      <*> arbitrary
      <*> pure Nothing
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure Nothing
      <*> pure Nothing -- do not set GlobalSign credentials label

instance Arbitrary UserGroupAddress where
  arbitrary =
    UserGroupAddress
      <$> arbitraryUnicodeText
      <*> arbitraryUnicodeText
      <*> arbitraryUnicodeText
      <*> arbitraryUnicodeText
      <*> arbitraryUnicodeText
      <*> arbitraryUnicodeText

instance Arbitrary UserGroupUI where
  arbitrary =
    UserGroupUI Nothing Nothing Nothing
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
    (a , b , c , d , e , f , g , h , i , j ) <- arbitrary
    (k , l , m , n , o , p , q , r , s , t ) <- arbitrary
    (u , v , w , x , z , y , aa, bb, cc, dd) <- arbitrary
    (ee, ff, gg, hh, ii, jj, kk, ll, mm, nn) <- arbitrary
    (oo, pp) <- arbitrary
    return $ FeatureFlags { ffCanUseTemplates                = a
                          , ffCanUseBranding                 = b
                          , ffCanUseAuthorAttachments        = c
                          , ffCanUseSignatoryAttachments     = d
                          , ffCanUseMassSendout              = e
                          , ffCanUseSMSInvitations           = f
                          , ffCanUseSMSConfirmations         = g
                          , ffCanUseDKCPRAuthenticationToView = h
                          , ffCanUseDKPIDAuthenticationToView = ll
                          , ffCanUseDKCVRAuthenticationToView = mm
                          , ffCanUseDKCPRAuthenticationToSign = i
                          , ffCanUseDKPIDAuthenticationToSign = nn
                          , ffCanUseDKCVRAuthenticationToSign = oo
                          , ffCanUseNOAuthenticationToView   = j
                          , ffCanUseNOAuthenticationToSign   = k
                          , ffCanUseSEAuthenticationToView   = l
                          , ffCanUseSEAuthenticationToSign   = m
                          , ffCanUseSMSPinAuthenticationToView = n
                          , ffCanUseSMSPinAuthenticationToSign = o
                          , ffCanUseStandardAuthenticationToView = p
                          , ffCanUseStandardAuthenticationToSign = q
                          , ffCanUseVerimiAuthenticationToView = z
                          , ffCanUseVerimiQesAuthenticationToSign = nn
                          , ffCanUseIDINAuthenticationToView = aa
                          , ffCanUseIDINAuthenticationToSign = cc
                          , ffCanUseOnfidoAuthenticationToSign = ff
                          , ffCanUseEmailInvitations         = r
                          , ffCanUseEmailConfirmations       = v
                          , ffCanUseAPIInvitations           = s
                          , ffCanUsePadInvitations           = t
                          , ffCanUseFIAuthenticationToView   = u
                          , ffCanUseFIAuthenticationToSign   = ee
                          , ffCanUseShareableLinks           = w
                          , ffCanUseForwarding               = x
                          , ffCanUseDocumentPartyNotifications = y
                          , ffCanUsePortal                   = bb
                          , ffCanUseCustomSMSTexts           = dd
                          , ffCanUseArchiveToDropBox         = gg
                          , ffCanUseArchiveToGoogleDrive     = hh
                          , ffCanUseArchiveToOneDrive        = ii
                          , ffCanUseArchiveToSharePoint      = jj
                          , ffCanUseArchiveToSftp            = kk
                          , ffCanUseFlow                     = pp
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
      [curry Just <$> arbString 1 100 <*> arbString 1 100, return Nothing]

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

documentAllDigitalSignatureMethods :: [DigitalSignatureMethod]
documentAllDigitalSignatureMethods = [Guardtime, Pades]

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
           (\f ->
             fieldIdentity f
               `notElem` [NameFI (NameOrder 1), NameFI (NameOrder 2), EmailFI]
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
      , stfCustomValidation       = if usecustomvalidation
                                      then Just $ TextCustomValidation valpattern
                                                                       valexample
                                                                       valtooltip
                                      else Nothing
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

    return $ UserInfo { firstName       = fn
                      , lastName        = ln
                      , personalNumber  = pn
                      , companyPosition = ""
                      , phone           = ""
                      , email           = Email em
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
      <*> pure LoginAuthNative
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CgiGrpTransaction where
  arbitrary =
    CgiGrpSignTransaction
      <$> arbitrary
      <*> (fromSNN <$> arbitrary)
      <*> (fromSNN <$> arbitrary)
      <*> (fromSNN <$> arbitrary)
      <*> pure tempSessionID

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
      <$> arbText 20  30
      <*> arbText 200 300
      <*> arbText 10  20
      <*> arbText 11  11

instance Arbitrary NetsDKNemIDSignature where
  arbitrary =
    NetsDKNemIDSignature
      <$> arbText 20  30
      <*> arbText 200 300
      <*> arbText 10  20
      <*> arbText 10  10
      <*> arbText 10  10

instance Arbitrary EIDServiceNLIDINSignature where
  arbitrary = do
    a <- arbText 20 30
    c <- arbText 10 20
    d <- arbText 10 10
    return $ EIDServiceNLIDINSignature { unEIDServiceIDINSigSignatoryName = a
                                       , unEIDServiceIDINSigDateOfBirth   = c
                                       , unEIDServiceIDINSigCustomerID    = d
                                       }

instance Arbitrary EIDServiceFITupasSignature where
  arbitrary = do
    a <- arbText 20 30
    b <- arbText 10 20
    c <- arbText 10 10
    return $ EIDServiceFITupasSignature { eidServiceFITupasSigSignatoryName  = a
                                        , eidServiceFITupasSigPersonalNumber = Just b
                                        , eidServiceFITupasSigDateOfBirth    = Just c
                                        }

instance Arbitrary EIDServiceOnfidoSignature where
  arbitrary = do
    a <- arbText 20 30
    b <- arbText 10 10
    c <- elements [OnfidoDocumentCheck, OnfidoDocumentAndPhotoCheck]
    return $ EIDServiceOnfidoSignature { eidServiceOnfidoSigSignatoryName = a
                                       , eidServiceOnfidoSigDateOfBirth   = b
                                       , eidServiceOnfidoSigMethod        = c
                                       }


instance Arbitrary EIDServiceVerimiQesSignature where
  arbitrary =
    EIDServiceVerimiQesSignature
      <$> arbText 20 30
      <*> arbitraryMaybe (arbText 20 30)
      <*> arbitraryMaybe (arbText 10 10)

instance Arbitrary EIDServiceDKNemIDSignature where
  arbitrary =
    EIDServiceDKNemIDSignature
      <$> arbText 20  30
      <*> arbText 200 300
      <*> arbText 10  20
      <*> arbitraryMaybe (arbText 10 10)
      <*> arbitraryMaybe (arbText 10 10)

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
  fileid <$> saveNewFile (T.pack fn) cnt

data UserGroupTemplate m = UserGroupTemplate
  { parentGroupID :: Maybe UserGroupID
  , groupHomeFolderID :: m (Maybe FolderID)
  , name :: m Text
  , address :: Maybe UserGroupAddress
  , settings  :: Maybe UserGroupSettings
  , invoicing :: UserGroupInvoicing
  , ui :: Maybe UserGroupUI
  , features :: Maybe Features
  , internalTags :: m (Set Tag)
  , externalTags :: m (Set Tag)
  }

-- | `randomUserGroupTemplate` represents 'sane defaults' for use with
-- `instantiateUserGroup`. An instance of this template has random name and
-- tags, a home folder and no parent group.
--
-- To create a user group with specific features, update the corresponding
-- fields in the template; e.g. `instantiateUserGroup $ randomUserGroupTemplate
-- {parentGroupID = Just ugid}` makes the created user group a child of an
-- existing user group with id `ugid`.
randomUserGroupTemplate
  :: (CryptoRNG m, MonadFail m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => UserGroupTemplate m
randomUserGroupTemplate = UserGroupTemplate
  { parentGroupID     = Nothing
  , groupHomeFolderID = fmap (Just . view #id) . dbUpdate $ FolderCreate defaultFolder
  , name              = rand 10 (arbText 3 30)
  , address           = defaultUserGroup ^. #address
  , settings          = defaultUserGroup ^. #settings
  , invoicing         = defaultUserGroup ^. #invoicing
  , ui                = defaultUserGroup ^. #ui
  , features          = defaultUserGroup ^. #features
  , internalTags      = rand 10 arbitrary
  , externalTags      = rand 10 arbitrary
  }

instantiateRandomUserGroup
  :: (MonadFail m, CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => m UserGroup
instantiateRandomUserGroup = instantiateUserGroup randomUserGroupTemplate

-- | To be used together with `randomUserGroupTemplate`, as in
-- `instantiateUserGroup $ randomUserGroupTemplate {..}`. Generates an instance
-- of the given template; see the documentation of `randomUserGroupTemplate`.
instantiateUserGroup
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => UserGroupTemplate m
  -> m UserGroup
instantiateUserGroup UserGroupTemplate { groupHomeFolderID = generateHomeFolderID, name = generateName, internalTags = generateInternalTags, externalTags = generateExternalTags, ..}
  = do
    let id = emptyUserGroupID
    name         <- generateName
    homeFolderID <- generateHomeFolderID
    internalTags <- generateInternalTags
    externalTags <- generateExternalTags
    ug           <- dbUpdate . UserGroupCreate $ UserGroup { .. }
    now          <- currentTime
    let fdts = freeDocumentTokensFromValues 10 (10 `minutesAfter` now)
    dbUpdate $ UserGroupFreeDocumentTokensUpdate (ug ^. #id) fdts
    return ug

data UserTemplate m = UserTemplate
  { password :: Maybe Text
  , isCompanyAdmin :: Bool
  , signupMethod :: SignupMethod
  , firstName :: m Text
  , lastName :: m Text
  , email :: m Text
  , personalNumber :: Text
  , companyPosition :: Text
  , phone :: Text
  , lang :: Lang
  , associatedDomainID :: m BrandedDomainID
  , groupID :: m UserGroupID
  , homeFolderID :: Maybe FolderID -> m (Maybe FolderID)
  -- ^ takes the group's home folder as parameter
  , internalTags :: m (Set Tag)
  , externalTags :: m (Set Tag)
  }

-- | `randomUserTemplate` represents 'sane defaults' for use with
-- `instantiateUser`. An instance of this template has random name and email; no
-- password, phone or personal number set; is a non-admin member of a freshly
-- created (with `instantiateRandomUserGroup`) user group and has a home folder
-- that is a sub-folder of the user group home folder.
--
-- To create a user with specific features, update the corresponding fields in
-- the template; e.g. `instantiateUser $ randomUserTemplate {email = return
-- "test@user.com", password = Just "secret"}` allows us to use
-- `handleLoginPost` with email and password to log in as that user.
--
-- As a rule you should only explicitly specify fields that are relevant to your
-- test case; in particular, there are very few instances in which the name of
-- the test user matters!
randomUserTemplate
  :: (CryptoRNG m, MonadFail m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => UserTemplate m
randomUserTemplate = UserTemplate
  { password           = Nothing
  , isCompanyAdmin     = False
  , signupMethod       = ByAdmin
  , firstName          = rand 10 (arbText 3 30)
  , lastName           = rand 10 (arbText 3 30)
  , email              = let go = do
                               email <- rand 10 arbEmail
                               mUser <- dbQuery . GetUserByEmail $ Email email
                               case mUser of  -- ensure uniqueness
                                 Nothing -> return email
                                 Just _  -> go
                         in  go
  , personalNumber     = ""
  , companyPosition    = ""
  , phone              = ""
  , lang               = defaultLang
  , associatedDomainID = view #id <$> dbQuery GetMainBrandedDomain
  , groupID            = view #id <$> instantiateRandomUserGroup
  , homeFolderID       = \groupHomeFolderID ->
                           fmap (Just . view #id) . dbUpdate . FolderCreate $ set
                             #parentID
                             groupHomeFolderID
                             defaultFolder
  , internalTags       = rand 10 arbitrary
  , externalTags       = rand 10 arbitrary
  }

instantiateRandomUser
  :: (CryptoRNG m, MonadFail m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => m User
instantiateRandomUser = instantiateUser randomUserTemplate

-- | To be used together with `randomUserTemplate`, as in `instantiateUser $
-- randomUserTemplate {..}`. Generates an instance of the given template; see
-- the documentation of `randomUserTemplate`.
instantiateUser
  :: (CryptoRNG m, MonadFail m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => UserTemplate m
  -> m User
instantiateUser = fmap (fromMaybe $ unexpectedError hint) . tryInstantiateUser
  where
    hint
      = "Failed to instantiate user template; you probably tried to\
        \ create two users with the same email address."

tryInstantiateUser
  :: (CryptoRNG m, MonadFail m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => UserTemplate m
  -> m (Maybe User)
tryInstantiateUser UserTemplate { firstName = generateFirstName, lastName = generateLastName, email = generateEmail, groupID = generateGroupID, associatedDomainID = generateAssociatedDomainID, homeFolderID = generateHomeFolderID, password = generatePassword, internalTags = generateInternalTags, externalTags = generateExternalTags, ..}
  = do
    firstName          <- generateFirstName
    lastName           <- generateLastName
    email              <- generateEmail
    groupID            <- generateGroupID
    groupHomeFolderID  <- fmap (view #id) <$> dbQuery (FolderGetUserGroupHome groupID)
    homeFolderID       <- generateHomeFolderID groupHomeFolderID
    associatedDomainID <- generateAssociatedDomainID
    password           <- case generatePassword of
      Nothing           -> return Nothing
      Just passwordText -> Just <$> createPassword passwordText
    internalTags <- generateInternalTags
    externalTags <- generateExternalTags
    mUser        <- dbUpdate $ AddUser (firstName, lastName)
                                       email
                                       password
                                       (groupID, homeFolderID, isCompanyAdmin)
                                       lang
                                       associatedDomainID
                                       signupMethod
                                       internalTags
                                       externalTags
    case mUser of
      Nothing   -> return Nothing
      Just user -> do
        -- AddUser doesn't allow us to provide all of UserInfo so we need to use
        -- SetUserInfo as well
        let userID = user ^. #id
        void . dbUpdate $ SetUserInfo userID UserInfo { email = Email email, .. }
        dbQuery $ GetUserByID userID

randomPersonalNumber :: CryptoRNG m => m Text
randomPersonalNumber = rand 10 $ arbText 3 30

newtype OneOf a = OneOf { fromOneOf :: [a] }
  deriving (Eq, Show)

-- | Choose one of sets of signatory conditions at random.
type SignatoryTemplate = OneOf [RandomSignatoryCondition]

-- | Template for a random signatory without any constraints.
randomSignatory :: SignatoryTemplate
randomSignatory = OneOf [[]]

-- | Generate list of alternatives for signatories where one of them has a
-- bounded spec and the rest are completely random.
anyRandomSignatoryCondition
  :: Int -> Int -> SignatoryTemplate -> OneOf [SignatoryTemplate]
anyRandomSignatoryCondition a b cond = OneOf $ do
  n <- [a .. b]
  k <- [1 .. n]
  return
    $  replicate (k - 1) randomSignatory
    ++ [cond]
    ++ replicate (n - k) randomSignatory

{-# ANN type RandomSignatoryCondition ("HLint: ignore Use camelCase" :: String) #-}
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
  | RSC_HasSignOrder SignOrder
  deriving (Eq, Show)

data RandomDocumentAllows = RandomDocumentAllows
  { rdaTypes       :: OneOf DocumentType
  , rdaStatuses    :: OneOf DocumentStatus
  , rdaSharings    :: OneOf DocumentSharing
  , rdaSignatories :: OneOf [SignatoryTemplate]
  , rdaAuthor      :: User
  , rdaSharedLink  :: Bool
  , rdaTimeoutTime :: Bool
  , rdaDaysToSign  :: Maybe Int32
  , rdaFolderId    :: FolderID
  , rdaTemplateId  :: Maybe DocumentID
  , rdaDigitalSignatureMethods :: OneOf DigitalSignatureMethod
  }

rdaDefault :: User -> RandomDocumentAllows
rdaDefault user = RandomDocumentAllows
  { rdaTypes                   = OneOf documentAllTypes
  , rdaStatuses                = OneOf documentAllStatuses
  , rdaSharings                = OneOf documentAllSharings
  , rdaSignatories             = OneOf $ map (`replicate` randomSignatory) [1 .. 10]
  , rdaAuthor                  = user
  , rdaSharedLink              = False
  , rdaTimeoutTime             = True
  , rdaDaysToSign              = Nothing
  , rdaFolderId                = fromJust $ user ^. #homeFolderID
  , rdaTemplateId              = Nothing
  , rdaDigitalSignatureMethods = OneOf documentAllDigitalSignatureMethods
  }

randomSigLinkByStatus :: DocumentStatus -> Gen SignatoryLink
randomSigLinkByStatus Closed = do
  sl <- arbitrary
  if signatoryrole sl `elem` [SignatoryRoleApprover, SignatoryRoleSigningParty]
    then do
      (sign, seen) <- arbitrary
      return sl { maybesigninfo = Just sign, maybeseeninfo = Just seen }
    else return sl { maybesigninfo = Nothing }

randomSigLinkByStatus Preparation = do
  sl <- arbitrary
  return $ sl { maybesigninfo = Nothing, maybeseeninfo = Nothing }
randomSigLinkByStatus Pending = do
  sl <- arbitrary
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
      digitalSignatureMethod <- rand
        1
        (elements . fromOneOf $ rdaDigitalSignatureMethods rda)
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
            RSC_IsSignatory -> updateSeenSignInfos Nothing SignatoryRoleSigningParty
            RSC_IsSignatoryThatHasSigned ->
              updateSeenSignInfos (Just True) SignatoryRoleSigningParty
            RSC_IsSignatoryThatHasntSigned ->
              updateSeenSignInfos (Just False) SignatoryRoleSigningParty
            RSC_IsApprover -> updateSeenSignInfos Nothing SignatoryRoleApprover
            RSC_IsApproverThatHasApproved ->
              updateSeenSignInfos (Just True) SignatoryRoleApprover
            RSC_IsApproverThatHasntApproved ->
              updateSeenSignInfos (Just False) SignatoryRoleApprover
            RSC_IsViewer                -> updateSeenSignInfos Nothing SignatoryRoleViewer
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
            RSC_HasSignOrder signOrder ->
              \sig -> return sig { signatorysignorder = signOrder }

        foldrM applyCond siglink sigcond

      let
        doc = doc'
          { documenttype                   = xtype
          , documentstatus                 = status
          , documentsharing                = sharing
          , documenttitle                  = title
          , documentfromshareablelink      = rdaSharedLink rda
          , documenttemplateid             = rdaTemplateId rda
          , documentfolderid               = rdaFolderId rda
          , documenttimeouttime            = if rdaTimeoutTime rda
                                               then documenttimeouttime doc'
                                               else Nothing
          , documentdaystosign             = case rdaDaysToSign rda of
                                               Nothing   -> documentdaystosign doc'
                                               Just days -> days
          , documentdigitalsignaturemethod = digitalSignatureMethod
          }
      userDetails <- signatoryFieldsFromUser user
      let asl =
            asl' { maybesignatory = Just (user ^. #id), signatoryfields = userDetails }

      let alllinks = asl : siglinks

      let pendingfile = [(unsafeDocumentFileID 1, InputFile file)]
          closedfile  = if documentstatus doc == Closed
            then [(unsafeDocumentFileID 2, ClosedFile (DigitallySignedFile file Missing))]
            else []

      let adoc = doc { documentsignatorylinks = alllinks
                     , documentlang           = getLang user
                     , documentfilehistory    = Map.fromList $ pendingfile <> closedfile
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
sealTestDocument ctx did = do
  withDocumentID did
    . runGuardTimeConfT (ctx ^. #gtConf)
    . runPdfToolsLambdaT (ctx ^. #pdfToolsLambdaEnv)
    . runTemplatesT (ctx ^. #lang, ctx ^. #globalTemplates)
    . runMailContextT (contextToMailContext ctx)
    $ do
        res <- postDocumentClosedActions True False
          `catch` \(_ :: KE.KontraError) -> return False
        when res $ do
          digitalSignatureMethod <- documentdigitalsignaturemethod <$> theDocument
          let expectedJobCount = case digitalSignatureMethod of
                Guardtime -> 1
                Pades     -> 0
          actualJobCount <-
            runSQL $ "SELECT * FROM document_extending_jobs WHERE id =" <?> did
          assertEqual
            "postDocumentClosedActions should add task to\
                      \ document_extending_jobs"
            expectedJobCount
            actualJobCount

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

instance {-# OVERLAPPABLE #-} (DBQuery TestEnv ev res) => RandomQuery ev res where
  randomQuery = dbQuery

instance
  (Arbitrary a, RandomQuery c b) =>
  RandomQuery (a -> c) b where

  randomQuery f = do
    a <- rand 10 arbitrary
    randomQuery $ f a

-- | Random update.
class RandomUpdate a b m where
  randomUpdate :: a -> m b

instance {-# OVERLAPPABLE #-} (DBUpdate m ev res) => RandomUpdate ev res m where
  randomUpdate = dbUpdate

instance
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
    KE.InternalError _ -> Just ()
    _                  -> Nothing
  )
  (a >>= assertFailure . ("Expecting InternalError but got " <>) . show)
  return

assertRaises404 :: (Show v, MonadIO m, MonadMask m) => m v -> m ()
assertRaises404 a = catchJust
  (\case
    KE.Respond404 -> Just ()
    _             -> Nothing
  )
  (a >>= assertFailure . ("Expecting Respond404 but got " <>) . show)
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
  { snfID                     = unsafeSignatoryFieldID 0
  , snfNameOrder              = no
  , snfValue                  = v
  , snfObligatory             = True
  , snfShouldBeFilledBySender = False
  , snfPlacements             = []
  }
fieldForTests CompanyFI v = SignatoryCompanyField $ CompanyField
  { scfID                     = unsafeSignatoryFieldID 0
  , scfValue                  = v
  , scfObligatory             = True
  , scfShouldBeFilledBySender = False
  , scfPlacements             = []
  }
fieldForTests PersonalNumberFI v = SignatoryPersonalNumberField $ PersonalNumberField
  { spnfID                     = unsafeSignatoryFieldID 0
  , spnfValue                  = v
  , spnfObligatory             = True
  , spnfShouldBeFilledBySender = False
  , spnfPlacements             = []
  }
fieldForTests CompanyNumberFI v = SignatoryCompanyNumberField $ CompanyNumberField
  { scnfID                     = unsafeSignatoryFieldID 0
  , scnfValue                  = v
  , scnfObligatory             = True
  , scnfShouldBeFilledBySender = False
  , scnfPlacements             = []
  }
fieldForTests EmailFI v = SignatoryEmailField $ EmailField
  { sefID                     = unsafeSignatoryFieldID 0
  , sefValue                  = v
  , sefObligatory             = True
  , sefShouldBeFilledBySender = False
  , sefEditableBySignatory    = False
  , sefPlacements             = []
  }
fieldForTests MobileFI v = SignatoryMobileField $ MobileField
  { smfID                     = unsafeSignatoryFieldID 0
  , smfValue                  = v
  , smfObligatory             = True
  , smfShouldBeFilledBySender = False
  , smfEditableBySignatory    = False
  , smfPlacements             = []
  }
fieldForTests (TextFI l) v = SignatoryTextField $ TextField
  { stfID                     = unsafeSignatoryFieldID 0
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

instantiateRandomPadesUser
  :: (CryptoRNG m, MonadFail m, MonadDB m, MonadThrow m, MonadLog m, MonadMask m)
  => m User
instantiateRandomPadesUser = do
  let padesSettings =
        defaultUserGroupSettings & #digitalSignatureMethod .~ DigitalSignatureMethod.Pades
  userGroup <- instantiateUserGroup
    $ randomUserGroupTemplate { settings = Just padesSettings }
  instantiateUser $ randomUserTemplate { groupID = pure $ userGroup ^. #id }

makeFieldLabelsWith noPrefixFieldLabels ''UserTemplate
makeFieldLabelsWith noPrefixFieldLabels ''UserGroupTemplate
