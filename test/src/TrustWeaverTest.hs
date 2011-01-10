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
1.1 Correct Invoice
1.2.1 Could not find associated sign policy
1.2.2 Could not find associated certificate(s) for sender=<senderTag> and receiver=<receiverTag>
1.3.1. TrustWeaver-Signing™ is accessible however the service is unable to perform the request.
1.3.2 TrustWeaver-Signing™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Signing™.
Scenario 2: Validate Invoices
2.1 Correct Signed Invoice
2.2.1 SignatureInvalid
2.3.1 SignerInvalid
2.4.1 Could not find any validation policies for sender=XX and receiver=YY
2.5.1. TrustWeaver-Signing™ is accessible however the service is unable to perform the validation request.
2.5.2 Optional: TrustWeaver-Signing™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Signing™.
Scenario 3: Archive Time-stamping Invoices
3.1 Correct hash file
3.2.1 InputType HASHXML does not match input document type
3.3.1. TrustWeaver-Signing™ is accessible however the service is unable to perform the signing request. 
3.3.2 TrustWeaver-Signing™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Signing™.
Scenario 4: Auditing Invoices Using TrustWeaver Audit Page
 - we aren't doing this
Scenario 5: Auditing Invoices By Web Services Integration
 - we aren't doing this
Scenario 6: Registering and enabling a storage section
6.1 Correct Requests
6.1.1. 	RegisterSectionRequest for registering organization X with appropriate meta-data.
6.1.2. 	RegisterSectionRequest for registering organization Y with appropriate meta-data.
6.1.3. 	EnableSectionRequest for enabling organization X with appropriate meta-data.
6.2.1 Provide an incomplete RegisterSectionRequest, e.g. by not setting the parameter Name.
6.2.2 Provide an EnableSectionRequest with a Name that has not been registered.
6.2.3 Provide an EnableSectionRequest with the Name of Organization X.
6.3.1 TrustWeaver-Archiving™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Archiving™. 
Scenario 7: Storing Invoices in TrustWeaver-Archiving™
7.1 Correct Requests
7.1.1: 	ImplicitSignatureDocument PDF/PDF
7.2.1 Provide an incomplete StoreInvoiceRequest, e.g. by not setting the parameter DocumentFormat.
7.2.2 Provide an invalid StoreInvoiceRequest, e.g. by setting the parameter SignatureType=XYZ.
7.3.1 TrustWeaver-Archiving™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Archiving™. 
Scenario 8: Retrieving Invoices from TrustWeaver-Archiving™
8.1 Correct Requests
8.2.1 Provide an incomplete GetInvoiceRequest, e.g. by not setting the parameter Reference.
8.2.2 Provide an invalid StoreInvoiceRequest, e.g. by setting the parameter Reference=XYZ.
8.3.1 TrustWeaver-Archiving™ is inaccessible i.e. it is not possible to establish an HTTPS session with TrustWeaver-Archiving™.

Formal tests

Sign Invoice (test case F1.1)
Validate Invoices (test case F1.2) 
Archive Time-stamping Invoices (test case F1.3)
Auditing Invoices With TrustWeaver Audit Page (test case F1.4)
Auditing Invoices Through Web Services Integration (test case F1.5)
Registering and enabling a storage section (test case F1.6)
Storing Invoices in TrustWeaver-Archiving™ (test case F1.7)
Retrieving Invoices from TrustWeaver-Archiving™ (test case F1.8)

-}


trustWeaverTests :: [Test]
trustWeaverTests = [ testGroup "TrustWeaver" 
                     [ testCase "Signing and validation (F1.1 and F1.2)" testSignAndValidate
                     , testCase "Register and enable section (F1.6)" testRegisterAndEnableSection
                     , testCase "Store and retrieve invoice (F1.7 and F1.8)" testStoreAndRetrieveInvoice
                    ]  
                 ] 

testSignAndValidate :: IO ()
testSignAndValidate = do
  twconf <- fmap read $ readFile "tw-test.conf" 
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
                        print x
                        return ()

testRegisterAndEnableSection :: IO ()
testRegisterAndEnableSection = do
  twconf <- fmap read $ readFile "tw-test.conf" 
  MinutesTime m <- getMinutesTime
  -- should be unique enough
  result <- TW.registerAndEnableSection twconf ("skrivapa-test-section-" ++ show m)
  case result of
    Left errmsg -> error $ "RegisterAndEnableSection: " ++ errmsg
    Right x -> 
        do
          print x
          return ()

testStoreAndRetrieveInvoice :: IO ()
testStoreAndRetrieveInvoice = do
  twconf <- fmap read $ readFile "tw-test.conf" 
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

