{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}

module HttpHelper (
  getForm,
  postForm,
  requestURL,
  assertURL,
  assertURLStartsWith,
  withTestServer,
  assertXPathExists,
  assertXPathDoesntExist) where

import Data.Maybe
import Data.List
import Data.Word
import Control.Concurrent
import Control.Exception
import Network.Browser
import Network.HTTP
import Network.URI
import Test.HUnit (assertBool, assertEqual, Assertion)
import Text.XML.HXT.Parser.HtmlParsec
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.XPath.XPathEval
import System.Directory
import System.FilePath

import AppControl
import KontrakcjaServer
import StateHelper

getForm :: (TestableURL a) => a -> [FormVar] -> IO (URI, Response String)
getForm url =
  makeBrowserRequest . formToRequest . Form GET (getURI url)

postForm :: (TestableURL a) => a -> [FormVar] -> IO (URI, Response String)
postForm url =
  makeBrowserRequest . formToRequest . Form POST (getURI url)

requestURL :: (TestableURL a) => a -> IO (URI, Response String)
requestURL = makeBrowserRequest . getRequest . show . getURI

makeBrowserRequest :: Request String -> IO (URI, Response String)
makeBrowserRequest req = Network.Browser.browse $ do
  setAllowRedirects True -- handle HTTP redirects
  setOutHandler $ const (return ()) -- make it quiet
  request req

type XPath = String
  
assertXPathExists :: (TestableHtml a) => XPath -> a -> Assertion
assertXPathExists xpath html = do
  let selected = selectWithXPath xpath html
  assertBool ("No elements were returned for " ++ xpath)
             (not $ null selected)
  
assertXPathDoesntExist :: (TestableHtml a) => XPath -> a -> Assertion
assertXPathDoesntExist xpath html = do
  let selected = selectWithXPath xpath html
  assertBool ("Elements were returned for " ++ xpath ++ "\n" ++ (show selected)) 
             (null selected) 

selectWithXPath :: (TestableHtml a) => XPath -> a -> XmlTrees
selectWithXPath xpath html =
  let parsed = parseHtmlDocument "testcontent" $ getContent html
  in concat $ map (getXPath xpath) parsed

class TestableHtml a where
  getContent :: a -> String
  
instance TestableHtml String where
  getContent = id
  
instance TestableHtml (Response String) where
  getContent = rspBody

instance TestableHtml (URI, Response String) where
  getContent (_, rsp) = getContent rsp

class TestableURL a where
  getURI :: a -> URI
             
assertURL :: (TestableURL a, TestableURL b) => a -> b -> Assertion
assertURL expectedurl actualurl =
  assertEqual "Wrong URL" (getURI expectedurl) (getURI actualurl)  

assertURLStartsWith :: (TestableURL a, TestableURL b) => a -> b -> Assertion
assertURLStartsWith expectedurl actualurl =
  let expectedstr = show $ getURI expectedurl
      actualstr = show $ getURI actualurl in
  assertBool ("Expected URL " ++ expectedstr ++ " but got " ++ actualstr)
             (expectedstr `isPrefixOf` actualstr)
  
instance TestableURL URI where
  getURI = id

instance TestableURL String where
  getURI url@('/':_) = fromJust . parseURI $ testHost ++ url
  getURI url = getURI $ '/':url
  
instance TestableURL (URI, Response String) where
  getURI (uri, _) = getURI uri

withTestServer :: IO () -> IO ()
withTestServer test = withTemporaryDirectory $ \dir ->
  bracket (startTestServer dir) stopTestServer (const test)

--TODO: i don't like this threadDelay
startTestServer :: FilePath -> IO ThreadId
startTestServer dir = do
  setupTestDir dir
  threadid <- forkIO $ withCurrentDirectory dir runKontrakcjaServer
  threadDelay 3000000
  return threadid

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = do
  originaldir <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir)
           (setCurrentDirectory originaldir)
           action
  
setupTestDir :: FilePath -> IO ()
setupTestDir dir = do
  createDirectory dir
  currentdir <- getCurrentDirectory
  recursiveCopy (currentdir </> "templates") $ dir </> "templates"
  recursiveCopy (currentdir </> "texts") $ dir </> "texts"
  recursiveCopy (currentdir </> "public") $ dir </> "public"
  writeFile (dir </> "kontrakcja.conf") (show makeTestConf)

--TODO: use either posix or windows function for doing this
recursiveCopy :: FilePath -> FilePath -> IO ()
recursiveCopy from to = do
  isfile <- doesFileExist from
  if isfile
    then copyFile from to
    else do
      children <- getDirectoryContents from
      createDirectory to
      mapM_ copyChild $ filter ((/=) '.' . head) children
  where
    copyChild :: FilePath -> IO ()
    copyChild child = recursiveCopy (from </> child) $ to </> (takeFileName child)
  
makeTestConf :: AppConf
makeTestConf =
  let conf = defaultConf "kontrakcjatest" in
  conf {
    httpBindAddress = (fst $ httpBindAddress conf, testPort)
  , hostpart = testHost
  }
  
testHost :: String
testHost = "http://localhost:" ++ (show testPort)

testPort :: Word16
testPort = 8001

--TODO: i don't like this threadDelay  
stopTestServer :: ThreadId -> IO ()
stopTestServer threadid = do
  killThread threadid
  threadDelay 3000000

