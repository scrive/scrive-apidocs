{-# LANGUAGE ApplicativeDo #-}
module Generators.DocumentGenerators
  ( startableDocumentOC
  , startableDocumentGen
  ) where

import Test.QuickCheck
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Doc.DigitalSignatureStatus
import Doc.Types.Document
import Doc.Types.DocumentFile
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryField
import Doc.Types.SignatoryLink
import File.Types
import Generators.OccurenceControl
import TestingUtil
import User.UserID

---- | Generate random documents that can be started with a probability of >80%.
startableDocumentGen :: UserID -> File -> Gen Document
startableDocumentGen uid file = runOccurenceControl 0.8 $ startableDocumentOC uid file

startableDocumentOC :: UserID -> File -> OccurenceControl Document
startableDocumentOC uid file = do
  doc            <- liftGen arbitrary

  noMainfile     <- decide' pure
  mainfile       <- documentfileOC file

  sls            <- listOC 2 5 signatoryLinkOC

  documentstatus <- startableDocumentStatusOC
  documenttype   <- startableDocumentTypeOC

  authorRole     <- decide (pure SignatoryRoleApprover)
                           (arbitrary `suchThat` (/= SignatoryRoleApprover))

  noSigningParty <- decide' pure
  notSigningRole <- liftGen $ arbitrary `suchThat` (/= SignatoryRoleSigningParty)

  pure doc
    { documentfilehistory    = if noMainfile
                                 then Map.empty
                                 else Map.singleton (unsafeDocumentFileID 1) mainfile
    , documentsignatorylinks =
      let
        firstSL : secondSL : sls' = sls
        author                    = firstSL { signatoryisauthor = True
                                            , signatoryrole     = authorRole
                                            , maybesignatory    = Just uid
                                            }
        sls''
          | noSigningParty = map (overwriteSigningRole notSigningRole)
                                 (author : secondSL : sls')
          | any ((== SignatoryRoleSigningParty) . signatoryrole) sls' = author
          : secondSL
          : sls'
          | otherwise = author
          : secondSL { signatoryrole = SignatoryRoleSigningParty }
          : sls'
      in
        sls''
    , documentstatus
    , documenttype
    }

  where
    overwriteSigningRole :: SignatoryRole -> SignatoryLink -> SignatoryLink
    overwriteSigningRole role sl
      | signatoryrole sl == SignatoryRoleSigningParty = sl { signatoryrole = role }
      | otherwise = sl

startableDocumentStatusOC :: OccurenceControl DocumentStatus
startableDocumentStatusOC = do
  decide (elements [Pending, Closed, Canceled, Timedout, Rejected, DocumentError])
         (pure Preparation)

startableDocumentTypeOC :: OccurenceControl DocumentType
startableDocumentTypeOC = decide (pure Template) (pure Signable)

documentfileOC :: File -> OccurenceControl DocumentFile
documentfileOC file = do
  isClosed               <- decide (pure True) (pure False)
  digitalSignatureStatus <- liftGen sealStatusGen
  pure $ if isClosed
    then ClosedFile
      { mainfileWithEvidence = DigitallySignedFile { digitallySignedFile    = file
                                                   , digitalSignatureStatus
                                                   }
      }
    else InputFile { mainfile = file }

sealStatusGen :: Gen DigitalSignatureStatus
sealStatusGen = oneof
  [ pure UnknownDigitalSignatureStatus
  , pure Missing
  , pure TrustWeaver
  , Guardtime <$> arbitrary <*> arbitrary
  , pure Pades
  ]

signatoryLinkOC :: OccurenceControl SignatoryLink
signatoryLinkOC = do
  sl                  <- liftGen arbitrary
  emailField          <- emailFieldOC
  mobileField         <- mobileFieldOC
  personalNumberField <- personalNumberFieldOC
  missingFieldForDelivery <- decide (Just <$> arbitrary) (pure Nothing)

  -- These may happen to be right by chance even when it is decided there
  -- should be an error but it is kind of unlikely anyway.
  authToView          <- decide arbitrary (pure StandardAuthenticationToView)
  authToSign          <- decide arbitrary (pure StandardAuthenticationToSign)

  placedFieldsForApprover <- decide' pure

  pure
    $ let (mobileField', personalNumberField')
            | authToView
              == StandardAuthenticationToView
              && authToSign
              == StandardAuthenticationToSign
            = (mobileField, personalNumberField)
            | otherwise
            = let SignatoryPersonalNumberField pn = personalNumberField
                  spnf = SignatoryPersonalNumberField pn { spnfValue = "invalid" }
                  SignatoryMobileField m = mobileField
                  smf                    = SignatoryMobileField m { smfValue = "invalid" }
              in  (smf, spnf)

          allFields = [emailField, mobileField', personalNumberField']

          sl'       = case (missingFieldForDelivery, signatorylinkdeliverymethod sl) of
            (Just True, EmailDelivery) -> sl
              { signatoryfields = filter
                                    (\case
                                      SignatoryEmailField _ -> False
                                      _                     -> True
                                    )
                                    allFields
              }
            (Just False, EmailDelivery) -> sl
              { signatoryfields = map
                                    (\case
                                      SignatoryEmailField f ->
                                        SignatoryEmailField $ f { sefValue = "" }
                                      f -> f
                                    )
                                    allFields
              }
            (Just True, MobileDelivery) -> sl
              { signatoryfields = filter
                                    (\case
                                      SignatoryMobileField _ -> False
                                      _ -> True
                                    )
                                    allFields
              }
            (Just False, MobileDelivery) -> sl
              { signatoryfields = map
                                    (\case
                                      SignatoryMobileField f ->
                                        SignatoryMobileField $ f { smfValue = "" }
                                      f -> f
                                    )
                                    allFields
              }
            (Just _, _) -> sl { signatorylinkdeliverymethod = EmailDelivery
                              , signatoryfields = [mobileField', personalNumberField']
                              }
            _ -> sl { signatoryfields = allFields }

          sl''
            | placedFieldsForApprover =
                -- It is very likely that one field already has a placement.
                                        sl' { signatoryrole = SignatoryRoleApprover }
            | signatoryrole sl' == SignatoryRoleApprover = sl'
              { signatoryfields = map removePlacements (signatoryfields sl')
              }
            | otherwise = sl'
      in  sl'' { maybesignatory    = Nothing
               , signatoryisauthor = False
               , signatorylinkauthenticationtoviewmethod = authToView
               , signatorylinkauthenticationtosignmethod = authToSign
               }

  where
    removePlacements :: SignatoryField -> SignatoryField
    removePlacements = \case
      SignatoryNameField    f -> SignatoryNameField f { snfPlacements = [] }
      SignatoryCompanyField f -> SignatoryCompanyField f { scfPlacements = [] }
      SignatoryCompanyNumberField f ->
        SignatoryCompanyNumberField f { scnfPlacements = [] }
      SignatoryPersonalNumberField f ->
        SignatoryPersonalNumberField f { spnfPlacements = [] }
      SignatoryEmailField      f -> SignatoryEmailField f { sefPlacements = [] }
      SignatoryMobileField     f -> SignatoryMobileField f { smfPlacements = [] }
      SignatoryTextField       f -> SignatoryTextField f { stfPlacements = [] }
      SignatoryCheckboxField   f -> SignatoryCheckboxField f { schfPlacements = [] }
      SignatorySignatureField  f -> SignatorySignatureField f { ssfPlacements = [] }
      SignatoryRadioGroupField f -> SignatoryRadioGroupField f { srgfPlacements = [] }

emailFieldOC :: OccurenceControl SignatoryField
emailFieldOC = do
  f     <- liftGen arbitrary
  email <- decide
    arbitraryUnicodeText
    (   (\x y z -> T.pack $ x <> "@" <> y <> "." <> z)
    <$> listOf1 (elements "abcdefghijklmnopqrstuvwxyz0123456789")
    <*> listOf1 (elements "abcdefghijklmnopqrstuvwxyz0123456789")
    <*> elements ["com", "sv", "newgtld"]
    )
  pure $ SignatoryEmailField f { sefValue = email }

mobileFieldOC :: OccurenceControl SignatoryField
mobileFieldOC = do
  f      <- liftGen arbitrary
  mobile <- decide arbitraryUnicodeText
                   (T.pack . ('+' :) <$> listOf (elements "0123456789"))
  pure $ SignatoryMobileField f { smfValue = mobile }

personalNumberFieldOC :: OccurenceControl SignatoryField
personalNumberFieldOC = do
  f  <- liftGen arbitrary
  pn <- liftGen . vectorOf 11 $ elements "0123456789"
  pure $ SignatoryPersonalNumberField f { spnfValue = T.pack pn }
