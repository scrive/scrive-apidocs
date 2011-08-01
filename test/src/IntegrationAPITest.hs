module IntegrationAPITest (integrationAPITests) where

import Control.Applicative
import Happstack.Server
import Happstack.State
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Char8 as BS

import StateHelper
import Templates.TemplatesLoader
import TestKontra as T
import User.Password
import User.UserState
import Misc
import API.API
import API.Service.ServiceState
import API.IntegrationAPI 
import Text.JSON
import Kontra

integrationAPITests :: Test
integrationAPITests = testGroup "Integration API" [
      testCase "Testing if we can create sample document" testDocumentCreation
    , testCase "can't login with invalid user" testDocumentsAccess
    , testCase "can't login with invalid password" testDocumentAccessEmbeddedPage
    ]

testDocumentCreation :: Assertion
testDocumentCreation = withTestState $ do 
    createTestService
    _ <- makeAPIRequest createDocument $ JSObject $ toJSObject  []
    assertBool "Something" $ True


testDocumentsAccess :: Assertion
testDocumentsAccess = withTestState $ do undefined
    {- 
    _ <- createTestUser
    ctx <- mkContext =<<  localizedVersion defaultValue <$> readGlobalTemplates
    req <- mkRequest POST [("email", inText "emily@skrivapa.se"), ("password", inText "admin")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'
    -}
testDocumentAccessEmbeddedPage :: Assertion
testDocumentAccessEmbeddedPage = withTestState $ do undefined
    {- 
    _ <- createTestUser
    ctx <- mkContext =<< localizedVersion defaultValue <$> readGlobalTemplates
    req <- mkRequest POST [("email", inText "andrzej@skrivapa.se"), ("password", inText "invalid")]
    (res, ctx') <- runTestKontra req ctx $ handleLoginPost >>= sendRedirect
    loginFailureChecks res ctx'
        -}

makeAPIRequest:: IntegrationAPIFunction TestKontra APIResponse -> APIRequestBody -> IO APIResponse
makeAPIRequest handler req = do
    ctx <- mkContext =<<  localizedVersion defaultValue <$> readGlobalTemplates
    rq <- mkRequest POST [("service", inText "test_service"), ("password", inText "test_password") ,("body", inText $ encode req)]
    fmap fst $ runTestKontra rq ctx $ do
        mcontext <- apiContext
        case mcontext  of
             Right apictx -> do
                 res <- either (uncurry apiError) id <$> runApiFunction handler apictx
                 return res
             Left emsg -> return $ uncurry apiError emsg
    
    
        
createTestService :: IO ()
createTestService = do
    pwd <- createPassword $ BS.pack "test_password"
    Just User{userid} <- update $ AddUser (BS.empty, BS.empty) (BS.pack "mariusz@skrivapa.se") pwd False Nothing Nothing defaultValue
    _ <- update $ CreateService (ServiceID $ BS.pack "test_service") pwd (ServiceAdmin $ unUserID userid)
    return ()
    
