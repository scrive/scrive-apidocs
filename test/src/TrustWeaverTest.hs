module TrustWeaverTest(
    trustWeaverTests
) where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import MinutesTime

import qualified Data.ByteString as BS
import qualified TrustWeaver as TW

{-

TrustWeaver test specification scenarios

Scenario 1: Sign Invoices
1.1 Correct Invoice [OK]
1.2.1 Could not find associated sign policy [OK]
1.2.2 Could not find associated certificate(s) for sender=<senderTag> and receiver=<receiverTag> [OK]
1.3.1 TrustWeaver-Signing™ is accessible however the service is unable to perform the request. [OK]
1.3.2 TrustWeaver-Signing™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Signing™. [OK]
Scenario 2: Validate Invoices
2.1 Correct Signed Invoice [OK]
2.2.1 SignatureInvalid [not tested]
2.3.1 SignerInvalid [not tested]
2.4.1 Could not find any validation policies for sender=XX and receiver=YY [OK]
2.5.1 TrustWeaver-Signing™ is accessible however the service is unable to perform the validation request. [OK]
2.5.2 Optional: TrustWeaver-Signing™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Signing™.  [OK]
Scenario 3: Archive Time-stamping Invoices (not tested)
- we aren't doing this
3.1 Correct hash file (not tested)
3.2.1 InputType HASHXML does not match input document type
3.3.1. TrustWeaver-Signing™ is accessible however the service is unable to perform the signing request. 
3.3.2 TrustWeaver-Signing™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Signing™.
Scenario 4: Auditing Invoices Using TrustWeaver Audit Page [OK]
Scenario 5: Auditing Invoices By Web Services Integration
 - we aren't doing this
Scenario 6: Registering and enabling a storage section
6.1 Correct Requests [OK]
6.1.1. 	RegisterSectionRequest for registering organization X with appropriate meta-data. [OK]
6.1.2. 	RegisterSectionRequest for registering organization Y with appropriate meta-data. [OK]
6.1.3. 	EnableSectionRequest for enabling organization X with appropriate meta-data. [OK]
6.2.1 Provide an incomplete RegisterSectionRequest, e.g. by not setting the parameter Name. [OK]
6.2.2 Provide an EnableSectionRequest with a Name that has not been registered. [OK]
6.2.3 Provide an EnableSectionRequest with the Name of Organization X. [OK]
6.3.1 TrustWeaver-Archiving™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Archiving™.  [OK]
Scenario 7: Storing Invoices in TrustWeaver-Archiving™
7.1 Correct Requests [OK]
7.1.1: 	ImplicitSignatureDocument PDF/PDF [OK]
7.2.1 Provide an incomplete StoreInvoiceRequest, e.g. by not setting the parameter DocumentFormat. [OK]
7.2.2 Provide an invalid StoreInvoiceRequest, e.g. by setting the parameter SignatureType=XYZ.  [OK]
7.3.1 TrustWeaver-Archiving™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Archiving™.  [OK]
Scenario 8: Retrieving Invoices from TrustWeaver-Archiving™
8.1 Correct Requests [OK]
8.2.1 Provide an incomplete GetInvoiceRequest, e.g. by not setting the parameter Reference. [OK]
8.2.2 Provide an invalid StoreInvoiceRequest, e.g. by setting the parameter Reference=XYZ. [OK]
8.3.1 TrustWeaver-Archiving™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Archiving™. [OK]

Formal tests

Sign Invoice (test case F1.1) [OK]
Validate Invoices (test case F1.2) [OK] 
Archive Time-stamping Invoices (test case F1.3) (not tested)
Auditing Invoices With TrustWeaver Audit Page (test case F1.4) [OK]
Auditing Invoices Through Web Services Integration (test case F1.5) (not tested)
Registering and enabling a storage section (test case F1.6) [OK]
Storing Invoices in TrustWeaver-Archiving™ (test case F1.7) [OK]
Retrieving Invoices from TrustWeaver-Archiving™ (test case F1.8) [OK]

-}

twconf = TW.TrustWeaverConf
    { TW.signConf = Just ("https://tseiod-dev.trustweaver.com/ts/svs.asmx",
                          "certs/dev.pem",
                          "abcd1234")
    , TW.adminConf = Just ("https://twa-test-db.trustweaver.com/ta_hubservices/Admin/AdminService.svc",
                           "certs/dev-admin.pem",
                           "abcd1234")
    , TW.storageConf = Just ("https://twa-test-db.trustweaver.com/ta_hubservices/Storage/StorageService.svc",
                             "",
                             "")
    , TW.retries = 1
    , TW.timeout = 10
    }

twconf_bad = TW.TrustWeaverConf
    { TW.signConf = Just ("https://tseiod-dev.trustweaver.com/ts/svs.asmx-",
                          "certs/dev.pem",
                          "abcd1234")
    , TW.adminConf = Just ("https://twa-test-db.trustweaver.com/ta_hubservices/Admin/AdminService.svc-",
                           "certs/dev-admin.pem",
                           "abcd1234")
    , TW.storageConf = Just ("https://twa-test-db.trustweaver.com/ta_hubservices/Storage/StorageService.svc-",
                             "",
                             "")
    , TW.retries = 1
    , TW.timeout = 10
    }

trustWeaverTests :: [Test]
trustWeaverTests = [ testGroup "TrustWeaver"
                     [ testCase "Signing and validation (F1.1 and F1.2) (1.1 and 2.1)" $ 
                                testSignAndValidate twconf
                     , testCase "Register and enable section (F1.6) (6.1.1, 6.1.2, 6.1.3)" $
                                testRegisterAndEnableSection twconf
                     , testCase "Store and retrieve invoice (F1.7 and F1.8) (7.1, 7.1.1, 8.1)" $
                                testStoreAndRetrieveInvoice twconf
                     ]
                   , testGroup "TrustWeaver_badurls" 
                     [ testCase "Signing and validation (F1.1 and F1.2) (retries and fails) (1.3.2 and 2.5.2)" $
                                testSignAndValidateFail twconf_bad
                     , testCase "Register and enable section (F1.6) (retries and fails) (6.3.1)" $
                                testRegisterAndEnableSectionFail twconf_bad
                     , testCase "Store and retrieve invoice (F1.7 and F1.8) (retries and fails) (7.3.1, 8.3.1)" $
                                testStoreAndRetrieveInvoiceFail twconf_bad
                     ]  
                   , testGroup "TrustWeaver user errors"
                               [ testCase "Could not find associated sign policy (1.2.1)" $
                                          testSignBadSignPolicy twconf
                               , testCase "Could not find associated certificate(s) (1.2.2)" $
                                          testSignNoSignCertificates twconf
                               , testCase "Provide an incomplete RegisterSectionRequest, e.g. by not setting the parameter Name. (6.2.1)" $
                                          testIncompleteRegisterSection twconf
                               , testCase "Provide an EnableSectionRequest with a Name that has not been registered. (6.2.2)" $
                                          testEnableSectionUnknownName twconf
                               , testCase "Provide an EnableSectionRequest with the Name of Organization X. (6.2.3)" $
                                          testEnableSectionSecondTime twconf
                               , testCase "Provide an incomplete StoreInvoiceRequest, e.g. by not setting the parameter DocumentFormat. (7.2.1)" $
                                          testIncompleteStoreInvoice twconf
                               , testCase "Provide an invalid StoreInvoiceRequest, e.g. by setting the parameter SignatureType=XYZ. (7.2.2)" $
                                          testInvalidStoreInvoice twconf
                               , testCase "Provide an incomplete GetInvoiceRequest, e.g. by not setting the parameter Reference. (8.2.1)" $
                                          testIncompleteGetInvoice twconf
                               , testCase "Provide an invalid StoreInvoiceRequest, e.g. by setting the parameter Reference=XYZ. (8.2.2)" $
                                          testInvalidGetInvoice twconf

                               ]
                   ] 

testSignAndValidate :: TW.TrustWeaverConf -> IO ()
testSignAndValidate twconf = do
  pdfdata <- BS.readFile "nda.pdf"
  result1 <- TW.signDocument twconf pdfdata
  case result1 of
    Left errmsg1 -> error $ "SignDocument: " ++ errmsg1
    Right signeddoc -> 
        do
          BS.writeFile "nda-signed.pdf" signeddoc
          result2 <- TW.validateDocument twconf signeddoc
          case result2 of
            Left errmsg2 -> error $ "ValidateDocument: " ++ errmsg2
            Right x -> do
                        return ()

testSignAndValidateFail :: TW.TrustWeaverConf -> IO ()
testSignAndValidateFail twconf = do
  pdfdata <- BS.readFile "nda.pdf"
  result1 <- TW.signDocument twconf pdfdata
  case result1 of
    Left errmsg1 -> return ()
    Right signeddoc -> do
                 -- this should not happen
                 error "Server should not be accessible"

testSignBadSignPolicy :: TW.TrustWeaverConf -> IO ()
testSignBadSignPolicy twconf = do
  pdfdata <- BS.readFile "nda.pdf"
  result1 <- TW.signDocumentEx twconf pdfdata "TV" "TV"
  case result1 of
    Left errmsg1 -> return ()
    Right signeddoc -> do
                 -- this should not happen
                 error "Cannot sign from TV to TV"

testSignNoSignCertificates :: TW.TrustWeaverConf -> IO ()
testSignNoSignCertificates twconf = do
  pdfdata <- BS.readFile "nda.pdf"
  result1 <- TW.signDocumentEx twconf pdfdata "BS" "BS"
  case result1 of
    Left errmsg1 -> return ()
    Right signeddoc -> do
                 -- this should not happen
                 error "Cannot sign from BS to BS"

testRegisterAndEnableSection :: TW.TrustWeaverConf -> IO ()
testRegisterAndEnableSection twconf = do
  MinutesTime m s <- getMinutesTime
  -- should be unique enough
  result <- TW.registerAndEnableSection twconf ("skrivapa-test-section-" ++ show m)
  case result of
    Left errmsg -> error $ "RegisterAndEnableSection: " ++ errmsg
    Right x -> 
        do
          return ()

testRegisterAndEnableSectionFail :: TW.TrustWeaverConf -> IO ()
testRegisterAndEnableSectionFail twconf = do
  MinutesTime m s <- getMinutesTime
  -- should be unique enough
  result <- TW.registerAndEnableSection twconf ("skrivapa-test-section-" ++ show m)
  case result of
    Left errmsg -> return ()
    Right x -> error "Should not happen"

testStoreAndRetrieveInvoice :: TW.TrustWeaverConf -> IO ()
testStoreAndRetrieveInvoice twconf = do
  pdfdata <- BS.readFile "nda-signed.pdf"
  result <- TW.storeInvoice twconf "11111111" "2010-12-12T00:00:00" "skrivapa-test-section" pdfdata
  case result of
    Left errmsg -> error $ "StoreInvoice: " ++ errmsg
    Right reference -> do
                result2 <- TW.getInvoice twconf reference
                case result2 of
                  Left errmsg -> error $ "GetInvoice: " ++ errmsg
                  Right _ -> do
                                return ()

testStoreAndRetrieveInvoiceFail :: TW.TrustWeaverConf -> IO ()
testStoreAndRetrieveInvoiceFail twconf = do
  pdfdata <- BS.readFile "nda-signed.pdf"
  result <- TW.storeInvoice twconf "11111111" "2010-12-12T00:00:00" "skrivapa-test-section" pdfdata
  case result of
    Left errmsg -> return ()
    Right reference -> error "Should not happen"
                       
testIncompleteRegisterSection twconf = do
  MinutesTime m s <- getMinutesTime
  -- should be unique enough
  result <- TW.registerSection twconf ""
  case result of
    Left errmsg -> return ()
    Right x -> error "Should not happen"

testEnableSectionUnknownName twconf = do
  MinutesTime m s <- getMinutesTime
  -- should be unique enough
  result <- TW.enableSection twconf ("a-section-name-that-is-not-there-2")
  case result of
    Left errmsg -> return ()
    Right x -> error "Should not happen"

testEnableSectionSecondTime twconf = do
  MinutesTime m s <- getMinutesTime
  -- should be unique enough
  result <- TW.enableSection twconf ("skrivapa-test-section-" ++ show m)
  case result of
    Left errmsg -> return ()
    Right x -> error "Should not happen"

               
testIncompleteStoreInvoice twconf = do
  pdfdata <- BS.readFile "nda-signed.pdf"
  result <- TW.storeInvoice twconf "" "2010-12-12T00:00:00" "skrivapa-test-section" pdfdata
  case result of
    Left errmsg -> return ()
    Right reference -> error "Should fail"

testInvalidStoreInvoice twconf = do
  pdfdata <- BS.readFile "nda-signed.pdf"
  result <- TW.storeInvoice twconf "11111111" "2010-12-12T00:00:00" "there should be no spaces" pdfdata
  case result of
    Left errmsg -> return ()
    Right reference -> error "Should fail"

testIncompleteGetInvoice twconf = do
  result2 <- TW.getInvoice twconf ""
  case result2 of
    Left errmsg -> return ()
    Right _ -> error "Should fail"

testInvalidGetInvoice twconf = do
  result2 <- TW.getInvoice twconf "there should be no spaces"
  case result2 of
    Left errmsg -> return ()
    Right _ -> error "Should fail"
