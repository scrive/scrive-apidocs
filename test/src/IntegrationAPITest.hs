module IntegrationAPITest (integrationAPITests) where

import Control.Applicative
import Happstack.Server
import Test.HUnit (Assertion)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Char8 as BS
import StateHelper
import Templates.TemplatesLoader
import TestingUtil
import TestKontra as T
import User.Model
import Misc
import API.API
import API.Service.Model
import API.IntegrationAPI
import Context
import DB.Classes
import Text.JSON
import Data.Ratio
import Data.List
import Database.HDBC.PostgreSQL

integrationAPITests :: Connection -> Test
integrationAPITests conn = testGroup "Integration API" [
      testCase "Testing if we can create sample document" $ testDocumentCreation conn
    , testCase "Accessing documents created by the API" $ testDocumentsAccess conn
    , testCase "Accessing documents in embedded frame" $ testDocumentAccessEmbeddedPage conn
    ]

-- Main tests

testDocumentCreation :: Connection -> Assertion
testDocumentCreation conn = withTestEnvironment conn $ do
    createTestService
    apiReq1 <- createDocumentJSON "test_company1" "mariusz@skrivapa.se"
    apiRes1 <- makeAPIRequest createDocument $ apiReq1
    assertBool ("Failed to create first document :" ++ show apiRes1)  $ not (isError apiRes1)
    apiReq2 <- createDocumentJSON "test_company2" "mariusz@skrivapa.se"
    apiRes2 <- makeAPIRequest createDocument $ apiReq2
    assertBool ("Failed to create second document" ++ show apiRes2) $ not (isError apiRes2)
    apiReq3 <- createDocumentJSON "test_company1" "mariusz+1@skrivapa.se"
    apiRes3 <- makeAPIRequest createDocument $ apiReq3
    assertBool ("Failed to create third document" ++ show apiRes3) $ not (isError apiRes3)

testDocumentsAccess :: Connection -> Assertion
testDocumentsAccess conn = withTestEnvironment conn $ do
    createTestService
    _ <- makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se"
    _ <- makeAPIRequest createDocument =<< createDocumentJSON "test_company2" "mariusz@skrivapa.se"
    _ <- makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz+1@skrivapa.se"
    apiReqDocs1 <- getDocumentsJSON "test_company1" "mariusz@skrivapa.se"
    apiRespDocs1 <- makeAPIRequest getDocuments $ apiReqDocs1
    assertBool ("Two document was created by  this company but " ++ (show $ docsCount apiRespDocs1) ++ " were found") $ (docsCount apiRespDocs1) == 2
    apiReqDocs2 <- getDocumentsJSON "test_company2" "mariusz@skrivapa.se"
    apiRespDocs2 <- makeAPIRequest getDocuments $ apiReqDocs2
    assertBool ("One document was created by  this company but " ++ (show $ docsCount apiRespDocs2) ++ " were found") $ (docsCount apiRespDocs2) == 1
    apiReqDocs3 <- getDocumentsJSON "test_company3" "mariusz+1@skrivapa.se"
    apiRespDocs3 <- makeAPIRequest getDocuments $ apiReqDocs3
    assertBool ("No document was created for this company but " ++ (show $ docsCount apiRespDocs3) ++ " were found") $ (docsCount apiRespDocs3) == 0


testDocumentAccessEmbeddedPage :: Connection -> Assertion
testDocumentAccessEmbeddedPage conn = withTestEnvironment conn $ do
    createTestService
    docid1 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz@skrivapa.se")
    docid2 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company2" "mariusz@skrivapa.se")
    docid3 <- getJSONStringField "document_id" <$> (makeAPIRequest createDocument =<< createDocumentJSON "test_company1" "mariusz+1@skrivapa.se")
    apiRespDocs1 <- makeAPIRequest embeddDocumentFrame =<< getEmbedDocumentaJSON docid1 "test_company1" "mariusz@skrivapa.se"
    assertBool ("User from right company could not access his document") $ containsUserEmbedLink apiRespDocs1
    apiRespDocs2 <- makeAPIRequest embeddDocumentFrame =<< getEmbedDocumentaJSON docid2 "test_company1" "mariusz@skrivapa.se"
    assertBool ("Company could access other company documents") $ isError apiRespDocs2
    apiRespDocs3 <- makeAPIRequest embeddDocumentFrame =<< getEmbedDocumentaJSON docid3 "test_company1" "mariusz@skrivapa.se"
    assertBool ("Company could not access its documents, with nonauthor") $ containsCompanyEmbedLink apiRespDocs3


-- Requests body 
createDocumentJSON :: String -> String -> DB JSValue
createDocumentJSON company author = randomCall $ \title fname sname -> JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("title" , JSString $ toJSString  title)
         ,("type" , JSRational True (1%1))
         ,("involved" , JSArray [ JSObject $ toJSObject $
                                    [ ("fstname", JSString $ toJSString fname),
                                      ("sndname", JSString $ toJSString sname),
                                      ("email",   JSString $ toJSString author)
                                    ]
                                ]
        )]

getDocumentsJSON :: String -> String -> DB JSValue
getDocumentsJSON company email = randomCall $ JSObject $ toJSObject $
        [ ("company_id", JSString $ toJSString company)
         ,("email" , JSString $ toJSString  email)
        ]

getEmbedDocumentaJSON :: String -> String -> String -> DB JSValue
getEmbedDocumentaJSON  documentid company email = randomCall $ JSObject $ toJSObject $
        [ ("document_id", JSString $ toJSString documentid)
         ,("company_id", JSString $ toJSString company)
         ,("email" , JSString $ toJSString  email)
        ]

-- Making requests
makeAPIRequest :: IntegrationAPIFunction TestKontra APIResponse -> APIRequestBody -> DB APIResponse
makeAPIRequest handler req = wrapDB $ \conn -> do
    ctx <- (\c -> c { ctxdbconn = conn }) <$> (mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates)
    rq <- mkRequest POST [("service", inText "test_service"), ("password", inText "test_password") ,("body", inText $ encode req)]
    fmap fst $ runTestKontra rq ctx $ testAPI handler

-- A service to be used with API. We need one to use it.
createTestService :: DB ()
createTestService = do
    pwd <- createPassword $ BS.pack "test_password"
    Just User{userid} <- dbUpdate $ AddUser (BS.empty, BS.empty) (BS.pack "mariusz@skrivapa.se") (Just pwd) False Nothing Nothing defaultValue (mkLocaleFromRegion defaultValue)
    _ <- dbUpdate $ CreateService (ServiceID $ BS.pack "test_service") (Just pwd) userid
    return ()

-- Utils
isError :: JSObject JSValue -> Bool
isError = any (== "error") . map fst .fromJSObject

getJSONField :: String -> JSObject JSValue -> Maybe JSValue
getJSONField s = lookup s .fromJSObject

docsCount :: JSObject JSValue -> Int
docsCount obj = case getJSONField "documents" obj of
                   Just (JSArray docs) -> length docs
                   _ -> -1

containsUserEmbedLink :: JSObject JSValue -> Bool
containsUserEmbedLink obj =    "connectuser" `isInfixOf` (getJSONStringField "link" obj)

containsCompanyEmbedLink :: JSObject JSValue -> Bool
containsCompanyEmbedLink obj = "connectcompany" `isInfixOf` (getJSONStringField "link" obj)

getJSONStringField :: String -> JSObject JSValue ->  String
getJSONStringField name obj =
    case (getJSONField name obj) of
        Just (JSString s) -> fromJSString s
        _ -> ""
