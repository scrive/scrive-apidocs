module SampleData where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import DocState
import MinutesTime
import Misc
import Templates.Templates
import Mails.SendMail
import User
import Mails.MailsUtil
import Mails.MailsConfig
import KontraLink 
import TrustWeaver
import Payments.PaymentsState

import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP as HTTP
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSConnection as AWS

aTestHost = "http://localhost:8080"
aTestName = (BS.fromString "Bob O'Brien")
anotherTestName = (BS.fromString "Annie Angus")
anotherTestEmail = (BS.fromString "annie@testcorp.com")
aTestEmail = (BS.fromString "bob@testcorp.com")
aTestCompany = (BS.fromString "Test Corp")
aTestLink = LoopBack --doesn't matter for tests
aTestPassword = (BS.fromString "pa$$w0rd")
someTestUserInfo = UserInfo {
                  userfstname = aTestName          
                , usersndname = BS.empty
                , userpersonalnumber = BS.empty
                , usercompanyname = aTestCompany
                , usercompanynumber = BS.empty
                , useraddress = BS.empty
                , userzip = BS.empty
                , usercity  = BS.empty
                , usercountry = BS.empty
                , userphone = BS.empty
                , usermobile = BS.empty
                , useremail = Email aTestEmail   
                }
someTestUserSettings = UserSettings {
                        accounttype = MainAccount
                      , accountplan = Basic
                      , userpaymentmethod = Undefined
                      }

aTestPaymentChange = PaymentChange {  
                                  changePaymentForAccounts = testChangePaymentForAccounts,
                                  changePaymentForSignature = testChangePaymentForSignature,  
                                  changePaymentForSignedStorage = testChangePaymentForSignedStorage,  
                                  changePaymentForOtherStorage = testChangePaymentForOtherStorage }
                      where testChangePaymentForAccounts = PaymentForAccounts {
                                                              forAccount = Nothing,
                                                              forSubaccount = Nothing}
                            testChangePaymentForSignature = PaymentForSignature {
                                                               forEmailSignature = Nothing,
                                                               forElegSignature = Nothing,  
                                                               forMobileSignature = Nothing,  
                                                               forCreditCardSignature = Nothing,  
                                                               forIPadSignature = Nothing}
                            testChangePaymentForSignedStorage = PaymentForSignedStorage {
                                                                   forAmazon = Nothing,
                                                                   forTrustWeaver = Nothing}
                            testChangePaymentForOtherStorage = PaymentForOtherStorage {
                                                                   forTemplate = Nothing,
                                                                   forDraft = Nothing}
aTestUserPaymentPolicy =  UserPaymentPolicy {
               paymentaccounttype = Private
             , custompaymentchange = aTestPaymentChange
             , temppaymentchange = Nothing } 
aTestUserPaymentAccount = UserPaymentAccount {
               paymentaccountmoney = Money 0
             , paymentaccountfreesignatures = 100  
      }
aTestUser = User { 
            userid = UserID 123
          , userpassword = unsafePerformIO $ createPassword aTestPassword
          , usersupervisor = Nothing
          , usercanhavesubaccounts = True
          , useraccountsuspended = False
          , userhasacceptedtermsofservice = Just $ MinutesTime 123
          , userinfo = someTestUserInfo
          , usersettings = someTestUserSettings
          , userpaymentpolicy = aTestUserPaymentPolicy
          , userpaymentaccount = aTestUserPaymentAccount
          , userfriends = []
          , userdefaultmainsignatory = DefaultMainSignatory 123
          }
aTestS3action = AWS.S3Action { AWS.s3conn = AWS.amazonS3Connection "accesskey" "secretkey",
                               AWS.s3bucket = "bucket",
                               AWS.s3object = "",
                               AWS.s3query = "",
                               AWS.s3metadata = [],
                               AWS.s3body = L.empty,
                               AWS.s3operation = HTTP.GET}
aTestTwConf = TrustWeaverConf 
                          { signcert = ""
                          , signcertpwd = ""
                          , admincert = ""
                          , admincertpwd = ""
                          }

aTestCtx = Context{ctxmaybeuser = Just aTestUser, 
                   ctxhostpart = aTestHost,
                   ctxflashmessages = [],
                   ctxtime = MinutesTime 123,
                   ctxnormalizeddocuments = unsafePerformIO $ newMVar Map.empty,
                   ctxipnumber = 123,
                   ctxs3action = aTestS3action,
                   ctxproduction = False,
                   ctxtemplates = unsafePerformIO $ readTemplates,
                   ctxmailsconfig = defaultMailConfig,
                   ctxtwconf = aTestTwConf,
                   ctxelegtransactions = []}

otherSignatoryDetails = SignatoryDetails { signatoryname = anotherTestName,
                                           signatorycompany = aTestCompany,
                                           signatorynumber = BS.fromString "12345",
                                           signatoryemail = anotherTestEmail,
                                           signatorynameplacements = [],
                                           signatorycompanyplacements = [],
                                           signatoryemailplacements = [],
                                           signatorynumberplacements = [],
                                           signatoryotherfields = [] }
anUnsignedSigLink = SignatoryLink { 
             signatorylinkid = SignatoryLinkID 123,
             signatorydetails = otherSignatoryDetails,
             signatorymagichash = MagicHash 123,
             maybesignatory = Nothing,
             maybesigninfo = Nothing,
             maybeseeninfo = Nothing, 
             invitationdeliverystatus = Unknown}
someSignInfo = SignInfo {signtime = MinutesTime 123, signipnumber = 123}
aSignedSigLink = SignatoryLink { 
             signatorylinkid = SignatoryLinkID 123,
             signatorydetails = otherSignatoryDetails,
             signatorymagichash = MagicHash 123,
             maybesignatory = Nothing,
             maybesigninfo = Just someSignInfo,
             maybeseeninfo = Nothing, 
             invitationdeliverystatus = Unknown}
aTestFile = File {fileid = FileID 123,
                  filename = BS.fromString "a_test_doc.pdf",
                  filestorage = FileStorageMemory $ BS.fromString ""}
anUnsignedDocument = Document { 
            documentid = DocumentID 123
          , documenttitle = BS.fromString "a_test_doc.pdf"
          , documentauthor = Author $ UserID 123
          , documentsignatorylinks = [anUnsignedSigLink] 
          , documentfiles = [aTestFile]
          , documentsealedfiles = []
          , documentstatus = Pending
          , documentctime = MinutesTime 123
          , documentmtime = MinutesTime 123
          , documentchargemode = ChargeNormal
          , documentdaystosign = Nothing
          , documenttimeouttime = Nothing
          , documentdeleted = False
          , documenthistory = []
          , documentinvitetext = BS.fromString "some test invite text"
          , documenttrustweaverreference = Nothing
          }
aSignedDocument = Document { 
            documentid = DocumentID 123
          , documenttitle = BS.fromString "a_test_doc.pdf"
          , documentauthor = Author $ UserID 123
          , documentsignatorylinks = [aSignedSigLink] 
          , documentfiles = [aTestFile]
          , documentsealedfiles = []
          , documentstatus = Pending
          , documentctime = MinutesTime 123
          , documentmtime = MinutesTime 123
          , documentchargemode = ChargeNormal
          , documentdaystosign = Nothing
          , documenttimeouttime = Nothing
          , documentdeleted = False
          , documenthistory = []
          , documentinvitetext = BS.fromString "some test invite text"
          , documenttrustweaverreference = Nothing
          }
aCustomMsg = (BS.fromString "blah blah, custom message blah")


