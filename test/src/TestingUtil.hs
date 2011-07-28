{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestingUtil where

import Test.HUnit (assertFailure, Assertion, assertBool)
import Test.Framework
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import System.Random
import Test.QuickCheck
import Happstack.State
import Test.QuickCheck.Gen
import Control.Monad.Trans

import Company.CompanyState
import qualified AppLogger as Log
import StateHelper
import Mails.MailsUtil
import Doc.DocState
import MinutesTime
import User.UserState
import Misc
import Payments.PaymentsState as Payments
import API.Service.ServiceState

instance Arbitrary Company where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Company { companyid  = a
                     , companyexternalid = b
                     , companyservice = c
                     , companyinfo = d
                     }
      
instance Arbitrary CompanyID where
  arbitrary = do
    a <- arbitrary
    return $ CompanyID a
  
instance Arbitrary ExternalCompanyID where
  arbitrary = do
    a <- arbitrary
    return $ ExternalCompanyID a
  
instance Arbitrary CompanyUser where
  arbitrary = do
    a <- arbitrary
    return $ CompanyUser a
  
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
                         }

instance Arbitrary ServiceID where
  arbitrary = do
    a <- arbitrary
    return $ ServiceID a
  
instance Arbitrary MagicHash where
  arbitrary = do
    a <- arbitrary
    return $ MagicHash a

instance Arbitrary MailsDeliveryStatus where
  arbitrary = elements [ Delivered 
                       , Undelivered
                       , Unknown
                       , Deferred]

instance Arbitrary TimeoutTime where
  arbitrary = do
    a <- arbitrary
    return $ TimeoutTime a

instance Arbitrary MinutesTime where
  arbitrary = do
    a <- arbitrary
    return $ fromSeconds a

instance Arbitrary DocumentTag where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ DocumentTag a b

instance Arbitrary DocumentUI where
  arbitrary = do
    a <- arbitrary
    return $ DocumentUI a


{- | Sometimes we get and object that is not as random as we would expect (from some reason)
     Like author signatorylink that by default does not have any fields attached
     This is a class to make it more random - so to attach this fields for example.
-}
class ExtendWithRandomnes a where
    moreRandom :: a -> Gen a
    extendRandomness ::  a -> IO a
    extendRandomness a = do
          stdgen <- newStdGen
          return $ unGen (moreRandom a) stdgen 10

        
instance ExtendWithRandomnes SignatoryDetails where
    moreRandom sl = do
        ofields <- arbitrary
        return $ sl {signatoryotherfields = ofields}
    
instance Arbitrary SignatoryLinkID where
  arbitrary = do
    si <- arbitrary
    return $ SignatoryLinkID si

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
    return $ SignatureInfo { signatureinfotext        = a
                           , signatureinfosignature   = b
                           , signatureinfocertificate = c
                           , signatureinfoprovider    = d
                           , signaturefstnameverified = e
                           , signaturelstnameverified = f
                           , signaturepersnumverified = g
                           }

do100Times' :: IO (Maybe Assertion) -> IO ()
do100Times' action = doTimes 100 action
             
doTimes :: Int -> IO (Maybe Assertion) -> IO ()
doTimes i action 
  | i == 0 = return ()
  | otherwise = do
    res <- action
    case res of
      Nothing -> doTimes i action
      Just ass -> do
        _ <- ass
        doTimes (i - 1) action

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
  arbitrary = do
    ds <- arbitrary
    return $ DocumentID ds

instance Arbitrary DocumentType where
  arbitrary = elements [ Signable Contract
                       , Signable Order
                       , Signable Offer
                       , Template Contract
                       , Template Order
                       , Template Offer
                       , Attachment
                       , AttachmentTemplate
                       ]
         

instance Arbitrary Document where
  arbitrary = do
    ds <- arbitrary
    dt <- arbitrary
    return $ blankDocument { documentstatus = ds 
                           , documenttype = dt
                           }

instance Arbitrary DocumentStatus where
  arbitrary = elements [ Preparation
                       , Pending
                       , Closed
                       , Canceled
                       , Timedout
                       , Rejected
                       , AwaitingAuthor
                       , DocumentError "Bad document."
                       ]
    
instance Arbitrary SignatoryDetails where
  arbitrary = do
    fn <- arbitrary
    ln <- arbitrary
    cn <- arbitrary
    pn <- arbitrary
    cm <- arbitrary
    em <- arbEmail
    ofields <- arbitrary
    return $ SignatoryDetails { signatoryfstname        = fn
                              , signatorysndname        = ln
                              , signatorycompany        = cn
                              , signatorypersonalnumber = pn
                              , signatorycompanynumber  = cm
                              , signatoryemail          = em
                              , signatorysignorder = SignOrder 1
                              , signatoryfstnameplacements        = []
                              , signatorysndnameplacements        = []
                              , signatorycompanyplacements        = []
                              , signatoryemailplacements          = []
                              , signatorypersonalnumberplacements = []
                              , signatorycompanynumberplacements  = []
                              , signatoryotherfields              = ofields
                              }

instance Arbitrary FieldDefinition where
   arbitrary = do
    name <- arbitrary
    value <- arbitrary
    filledByAuthor <- arbitrary
    return $ FieldDefinition { fieldlabel = name,
                               fieldvalue = value,
                               fieldplacements = [],
                               fieldfilledbyauthor = filledByAuthor
                             }
                      
instance Arbitrary SignatoryRole where
  arbitrary = return SignatoryPartner
  
instance Arbitrary DocumentFunctionality where
  arbitrary = elements [BasicFunctionality, AdvancedFunctionality]
  
instance Arbitrary IdentificationType where
  arbitrary = elements [EmailIdentification, ELegitimationIdentification]

instance Arbitrary UserInfo where
  arbitrary = do
    fn <- arbitrary
    ln <- arbitrary
    cn <- arbitrary
    pn <- arbitrary
    cm <- arbitrary
    em <- arbEmail

    return $ UserInfo { userfstname     = fn
                      , usersndname     = ln
                      , userpersonalnumber  = pn
                      , usercompanyname     = cn
                      , usercompanyposition = ""
                      , usercompanynumber   = cm
                      , useraddress         = ""
                      , userzip             = ""
                      , usercity            = ""
                      , usercountry         = ""
                      , userphone           = ""
                      , usermobile          = ""
                      , useremail           = Email em
                      }

    
instance Arbitrary BS.ByteString where
  arbitrary = fmap BS.fromString arbitrary

arbString :: Int -> Int -> Gen String
arbString minl maxl = do
  l <- choose (minl, maxl)
  vectorOf l $ elements ['a'..'z']

arbEmail :: Gen BS.ByteString
arbEmail = do
  n <- arbString 1 7
  d <- arbString 3 7
  return $ BS.fromString (n ++ "@" ++ d ++ ".com")



blankUser :: User
blankUser = User {  
                   userid                  =  UserID 0
                 , userpassword            =  NoPassword
                 , usersupervisor          =  Nothing 
                 , useriscompanyadmin = False
                 , useraccountsuspended    =  False  
                 , userhasacceptedtermsofservice = Nothing
                 , userfreetrialexpirationdate = Nothing
                 , usersignupmethod = AccountRequest
                 , userinfo = UserInfo {
                                    userfstname = BS.empty
                                  , usersndname = BS.empty
                                  , userpersonalnumber = BS.empty
                                  , usercompanyname =  BS.empty
                                  , usercompanyposition =  BS.empty
                                  , usercompanynumber  =  BS.empty
                                  , useraddress =  BS.empty
                                  , userzip = BS.empty
                                  , usercity  = BS.empty
                                  , usercountry = BS.empty
                                  , userphone = BS.empty
                                  , usermobile = BS.empty
                                  , useremail =  Email BS.empty 
                                   }
                , usersettings  = UserSettings {
                                    accounttype = PrivateAccount
                                  , accountplan = Basic
                                  , signeddocstorage = Nothing
                                  , userpaymentmethod = Undefined
                                  , preferreddesignmode = Nothing
                                  , lang = Misc.defaultValue
                                  , systemserver = Misc.defaultValue
                                  }                   
                , userpaymentpolicy = Payments.initialPaymentPolicy
                , userpaymentaccount = Payments.emptyPaymentAccount
              , userfriends = []
              , userinviteinfo = Nothing
              , userlogininfo = LoginInfo
                                { lastsuccesstime = Nothing
                                , lastfailtime = Nothing
                                , consecutivefails = 0
                                }
              , userservice = Nothing
              , usercompany = Nothing
              , usermailapi = Nothing
              , userdeleted = False
              }

blankDocument :: Document 
blankDocument =
          Document
          { documentid                   = DocumentID 0
          , documenttitle                = BS.empty
          , documentsignatorylinks       = []
          , documentfiles                = []
          , documentstatus               = Preparation
          , documenttype                 = Signable Contract
          , documentfunctionality        = AdvancedFunctionality
          , documentctime                = fromSeconds 0
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = Nothing
          , documenttimeouttime          = Nothing
          , documentlog                  = []
          , documentinvitetext           = BS.empty
          , documentsealedfiles          = []
          , documenttrustweaverreference = Nothing
          , documentallowedidtypes       = []
          , documentcsvupload            = Nothing
          , documentcancelationreason    = Nothing
          , documentinvitetime           = Nothing
          , documentsharing              = Doc.DocState.Private
          , documentrejectioninfo        = Nothing
          , documenttags                 = []
          , documentui                   = emptyDocumentUI
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentdeleted              = False
          , documentsignatoryattachments = []
          , documentattachments          = []
          }


testThat :: String -> Assertion -> Test
testThat s a = testCase s (withTestState a)

assertSuccess :: Assertion
assertSuccess = assertBool "not success?!" True

addNewUser :: String -> String -> String -> IO (Maybe User)
addNewUser firstname secondname email = 
  update $ AddUser (BS.fromString firstname, BS.fromString secondname) (BS.fromString email) NoPassword False Nothing Nothing defaultValue

whatTimeIsIt :: IO (MinutesTime)
whatTimeIsIt = liftIO $ getMinutesTime

      
addNewRandomUser :: IO (User)
addNewRandomUser = do
  stdgn <- newStdGen
  let fn = unGen arbitrary stdgn 10
      ln = unGen arbitrary stdgn 10
      em = unGen arbEmail  stdgn 10
  muser <- addNewUser fn ln (BS.toString em)
  case muser of
    Just user -> return user
    Nothing -> do
      Log.debug "Could not create user, trying again."
      addNewRandomUser

emptySignatoryDetails :: SignatoryDetails
emptySignatoryDetails = SignatoryDetails
    { signatoryfstname        = ""
    , signatorysndname        = ""
    , signatorycompany        = ""
    , signatorypersonalnumber = ""
    , signatorycompanynumber  = ""
    , signatoryemail          = ""
    , signatorysignorder = SignOrder 1
    , signatoryfstnameplacements        = []
    , signatorysndnameplacements        = []
    , signatorycompanyplacements        = []
    , signatoryemailplacements          = []
    , signatorypersonalnumberplacements = []
    , signatorycompanynumberplacements  = []
    , signatoryotherfields              = []
    }

addRandomDocumentWithAuthor :: User -> IO DocumentID
addRandomDocumentWithAuthor user = do
  stdgen <- newStdGen
  let rs = unGen arbitrary stdgen 100
  let roles = SignatoryAuthor : rs
  let doc = unGen arbitrary stdgen 10
      sls = 1 + (abs $ unGen arbitrary stdgen 10)
      sldets = unGen (vectorOf sls arbitrary) stdgen 10
      slr = unGen (vectorOf sls $ elements [[], [SignatoryPartner]]) stdgen 10000
  slinks <- sequence $ zipWith (\a r -> update $ (SignLinkFromDetailsForTest a r)) sldets slr
  
  mcompany <- case usercompany user of  
    Nothing -> return Nothing
    Just cid -> query $ GetCompany cid
  
  asd <- extendRandomness $ signatoryDetailsFromUser user mcompany
  asl <- update $ SignLinkFromDetailsForTest asd roles
  let adoc = doc { documentsignatorylinks = slinks ++ 
                                            [asl { maybesignatory = Just (userid user) }]
                 }
  update $ StoreDocumentForTesting adoc

addRandomDocumentWithAuthor' :: User -> IO Document
addRandomDocumentWithAuthor' user = do
  stdgen <- newStdGen
  let roles = unGen (elements [[SignatoryAuthor], [SignatoryAuthor, SignatoryPartner], [SignatoryPartner, SignatoryAuthor]])
              stdgen 10000
  let doc = unGen arbitrary stdgen 10
      sls = 1 + (abs $ unGen arbitrary stdgen 10)
      sldets = unGen (vectorOf sls arbitrary) stdgen 10
      slr = unGen (vectorOf sls $ elements [[], [SignatoryPartner]]) stdgen 10000
  slinks <- sequence $ zipWith (\a r -> update $ (SignLinkFromDetailsForTest a r)) sldets slr
  
  mcompany <- case usercompany user of  
    Nothing -> return Nothing
    Just cid -> query $ GetCompany cid
  
  asd <- extendRandomness $ signatoryDetailsFromUser user mcompany
  asl <- update $ SignLinkFromDetailsForTest asd roles
  let adoc = doc { documentsignatorylinks = slinks ++ 
                                            [asl { maybesignatory = Just (userid user) }]
                 }
  docid <- update $ StoreDocumentForTesting adoc
  mdoc <- query $ GetDocumentByDocumentID docid
  case mdoc of
    Nothing -> do
      assertFailure "Could not store document."
      return doc
    Just doc' -> return doc'

invalidateTest :: IO (Maybe Assertion)
invalidateTest = return Nothing

validTest :: Assertion -> IO (Maybe Assertion)
validTest = return . Just

