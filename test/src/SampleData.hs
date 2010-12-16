module SampleData where

import Test.HUnit (assert, assertEqual, assertFailure, Assertion(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import DocState
import MinutesTime
import Misc
import Templates.Templates
import SendMail
import User

import Control.Concurrent.MVar
import System.IO.Unsafe
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP as HTTP
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSConnection as AWS

aHost = "http://localhost:8080"
aTestName = (BS.fromString "Bob O'Brien")
aTestEmail = (BS.fromString "bob@testcorp.com")
aTestPassword = (BS.fromString "pa$$w0rd")
aTestCompany = (BS.fromString "Test Corp")
anotherTestName = (BS.fromString "Annie Angus")
anotherTestEmail = (BS.fromString "annie@testcorp.com")
aTestUserInfo = UserInfo {
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
aTestUser = User{ userid = UserID 123,
                  -- userfullname = aTestName, 
                  -- useremail = Email aTestEmail,
                  -- usercompanyname = aTestCompany,
                  -- usercompanynumber = BS.fromString "",
                  -- userinvoiceaddress = BS.fromString "",
                  -- userflashmessages = [],
                  userinfo = aTestUserInfo,
                  userpassword = unsafePerformIO $ createPassword aTestPassword,
                  usersupervisor = Nothing,
                  usercanhavesubaccounts = False,
                  useraccountsuspended = False,
                  userhasacceptedtermsofservice = Just $ MinutesTime 123 }
aTestS3action = AWS.S3Action { AWS.s3conn = AWS.amazonS3Connection "accesskey" "secretkey",
                               AWS.s3bucket = "bucket",
                               AWS.s3object = "",
                               AWS.s3query = "",
                               AWS.s3metadata = [],
                               AWS.s3body = L.empty,
                               AWS.s3operation = HTTP.GET}
aTestCtx = Context{ctxmaybeuser = Just aTestUser, 
                   ctxhostpart = aHost,
                   ctxflashmessages = [],
                   ctxtime = MinutesTime 123,
                   ctxnormalizeddocuments = unsafePerformIO $ newMVar Map.empty,
                   ctxipnumber = 123,
                   ctxs3action = aTestS3action,
                   ctxproduction = False,
                   ctxtemplates = unsafePerformIO $ readTemplates}
aCustomMsg = (BS.fromString "blah blah, custom message blah")
someSignatoryDetails = SignatoryDetails { signatoryname = aTestName,
                                          signatorycompany = aTestCompany,
                                          signatorynumber = BS.fromString "1234",
                                          signatoryemail = aTestEmail,
                                          signatorynameplacements = [],
                                          signatorycompanyplacements = [],
                                          signatoryemailplacements = [],
                                          signatorynumberplacements = [],
                                          signatoryotherfields = [] }
otherSignatoryDetails = SignatoryDetails { signatoryname = anotherTestName,
                                           signatorycompany = aTestCompany,
                                           signatorynumber = BS.fromString "12345",
                                           signatoryemail = anotherTestEmail,
                                           signatorynameplacements = [],
                                           signatorycompanyplacements = [],
                                           signatoryemailplacements = [],
                                           signatorynumberplacements = [],
                                           signatoryotherfields = [] }
someSignInfo = SignInfo {signtime = MinutesTime 123, signipnumber = 123}
anUnsignedSigLink = SignatoryLink { signatorylinkid = SignatoryLinkID 123,
             signatorydetails = otherSignatoryDetails,
             signatorymagichash = MagicHash 123,
             maybesignatory = Nothing,
             maybesigninfo = Nothing,
             maybeseeninfo = Nothing }
aSignedSigLink = SignatoryLink { signatorylinkid = SignatoryLinkID 123,
             signatorydetails = otherSignatoryDetails,
             signatorymagichash = MagicHash 123,
             maybesignatory = Nothing,
             maybesigninfo = Just someSignInfo,
             maybeseeninfo = Nothing }
aTestFile = File {fileid = FileID 123,
                  filename = BS.fromString "a_test_doc.pdf",
                  filestorage = FileStorageMemory $ BS.fromString ""}
anUnsignedDocument = Document {
               documentid = DocumentID 123,
               documenttitle = BS.fromString "a_test_doc.pdf",
               documentauthor = Author $ UserID 123,
               documentsignatorylinks = [anUnsignedSigLink],
               documentfiles = [aTestFile],
               documentsealedfiles = [],
               documentstatus = Pending,
               documentctime = MinutesTime 123,
               documentmtime = MinutesTime 123,
               documentchargemode = ChargeNormal,
               documentdaystosign = Nothing,
               documenttimeouttime = Nothing,
               documentdeleted = False,
               documentauthordetails = someSignatoryDetails,
               documentmaybesigninfo = Nothing,
               documenthistory = [],
               documentinvitetext = BS.fromString "" }
aSignedDocument = Document {
               documentid = DocumentID 123,
               documenttitle = BS.fromString "a_test_doc.pdf",
               documentauthor = Author $ UserID 123,
               documentsignatorylinks = [aSignedSigLink],
               documentfiles = [aTestFile],
               documentsealedfiles = [],
               documentstatus = Pending,
               documentctime = MinutesTime 123,
               documentmtime = MinutesTime 123,
               documentchargemode = ChargeNormal,
               documentdaystosign = Nothing,
               documenttimeouttime = Nothing,
               documentdeleted = False,
               documentauthordetails = someSignatoryDetails,
               documentmaybesigninfo = Just (SignInfo {signtime = MinutesTime 123, signipnumber = 123}),
               documenthistory = [],
               documentinvitetext = BS.fromString "" }

