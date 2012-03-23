{-# LANGUAGE CPP, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| Dump bin for things that do not fit anywhere else

I do not mind people sticking stuff in here. From time to time just
please go over this file, reorganize, pull better parts to other
modules.

Keep this one as unorganized dump.
-}
module Misc where

import KontraError(KontraError, internalError)

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad.Error (MonadError)
import Control.Monad.State
import Data.Char
import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid
import Numeric (readDec)
import Happstack.Server hiding (simpleHTTP,dir)
import Happstack.Util.Common hiding  (mapFst,mapSnd)
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Crypto.RNG (CryptoRNG, randomR)
import System.Time
import qualified Log
import qualified Codec.Binary.Url as URL
import qualified Control.Exception as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS
import Network.HTTP (urlDecode)

-- | Infix version of mappend, provided for convenience.
(<++>) :: Monoid m => m -> m -> m
(<++>) = mappend
infixr 6 <++>

-- We want this operators to bind strongly but weeker then . to do cond1 &&^ not . cond2
infixl 8  &&^
infixl 8  ||^
infixr 9 $^ 
infixr 9 $^^

randomPassword :: (MonadIO m, CryptoRNG m) => m String
randomPassword = randomString 8 (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])

concatChunks :: BSL.ByteString -> BS.ByteString
concatChunks = BS.concat . BSL.toChunks

-- | Generate random string of specified length that contains allowed
-- chars.
randomString :: (MonadIO m, CryptoRNG m) => Int -> [Char] -> m String
randomString n allowed_chars =
  sequence $ replicate n $ ((!!) allowed_chars `liftM` randomR (0, len))
  where
    len = length allowed_chars - 1

-- | Extract data from GET or POST request. Fail with 'internalError' if param
-- variable not present or when it cannot be read.
getDataFnM :: (HasRqData m, MonadIO m, ServerMonad m, MonadError KontraError m) => RqData a -> m a
getDataFnM fun = either (const internalError) return =<< getDataFn fun

-- | Since we sometimes want to get 'Maybe' and also we wont work with
-- newer versions of happstack here is.  This should be droped when
-- new version is globaly established.
getDataFn' :: (HasRqData m, MonadIO m, ServerMonad m) => RqData a -> m (Maybe a)
getDataFn' fun = either (const Nothing) Just `liftM` getDataFn fun

getAsString :: (HasRqData m, MonadIO m, ServerMonad m, MonadError KontraError m) => String -> m String
getAsString = getDataFnM . look

-- | Useful inside the 'RqData' monad.  Gets the named input parameter
-- (either from a @POST@ or a @GET@)
lookInputList :: String -> RqData [BSL.ByteString]
lookInputList name = do
  inputs <- lookInputs name
  return [value | Input (Right value) _ _ <- inputs]

-- | Create an external process with arguments. Feed it input, collect
-- exit code, stdout and stderr.
--
-- Standard input is first written to a temporary file. GHC 6.12.1
-- seemed to have trouble doing multitasking when writing to a slow
-- process like curl upload.
readProcessWithExitCode'
    :: FilePath                                    -- ^ command to run
    -> [String]                                    -- ^ any arguments
    -> BSL.ByteString                              -- ^ standard input
    -> IO (ExitCode,BSL.ByteString,BSL.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input =
  withSystemTempFile "process" $ \_inputname inputhandle -> do
    BSL.hPutStr inputhandle input
    hFlush inputhandle
    hSeek inputhandle AbsoluteSeek 0

    (_, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = UseHandle inputhandle,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }
    outMVar <- newEmptyMVar

    outM <- newEmptyMVar
    errM <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    _ <- forkIO $ do
      out <- BSL.hGetContents outh
      _ <- C.evaluate (BSL.length out)
      putMVar outM out
      putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    _ <- forkIO $ do
      err  <- BSL.hGetContents errh
      _ <- C.evaluate (BSL.length err)
      putMVar errM err
      putMVar outMVar ()

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    C.handle ((\_e -> return ()) :: (C.IOException -> IO ())) $ hClose outh
    C.handle ((\_e -> return ()) :: (C.IOException -> IO ())) $ hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    out <- readMVar outM
    err <- readMVar errM

    return (ex, out, err)

curl_exe :: String
#ifdef WINDOWS
curl_exe = "curl.exe"
#else
curl_exe = "curl"
#endif

{-| This function executes curl as external program. Args are args.
-}
readCurl :: [String]                 -- ^ any arguments
         -> BSL.ByteString           -- ^ standard input
         -> IO (ExitCode,BSL.ByteString,BSL.ByteString) -- ^ exitcode, stdout, stderr
readCurl args input = readProcessWithExitCode' curl_exe args input

-- | Select first alternative from a list of options.
--
-- Remeber LISP and its cond!
caseOf :: [(Bool, t)] -> t -> t
caseOf ((True,a):_) _ = a
caseOf (_:r) d = caseOf r d
caseOf [] d = d

-- | Enumerate all values of a bounded type.
allValues::(Bounded a, Enum a) => [a]
allValues = enumFromTo minBound maxBound

class HasDefaultValue a where
  defaultValue :: a

instance (Bounded a) => HasDefaultValue a where
  defaultValue = minBound

-- | Extra classes for one way enums
class SafeEnum a where
  fromSafeEnum :: Integral b =>  a -> b
  toSafeEnum   :: Integral b =>  b -> Maybe a

fromSafeEnumInt :: (SafeEnum a) => a -> Int
fromSafeEnumInt = fromSafeEnum

toSafeEnumInt :: (SafeEnum a) => Int -> Maybe a
toSafeEnumInt = toSafeEnum

-- | Just @flip map@.
for :: [a] -> (a -> b) -> [b]
for = flip map

isFieldSet :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> f Bool
isFieldSet name = isJust <$> getField name

getFields :: (HasRqData m, MonadIO m, ServerMonad m) => String -> m [String]
getFields name = map BSL.toString `liftM` fromMaybe [] `liftM` getDataFn' (lookInputList name)

getField :: (HasRqData m, MonadIO m, ServerMonad m) => String -> m (Maybe String)
getField name = (listToMaybe . reverse) `liftM` getFields name

getField' :: (HasRqData m, MonadIO m, ServerMonad m) => String -> m String
getField' name = fromMaybe "" `liftM` getField name

readField :: (HasRqData f, MonadIO f, Read a, Functor f, ServerMonad f) => String -> f (Maybe a)
readField name =  (join . (fmap readM)) <$> getField name

whenMaybe :: Monad m => Bool -> m a -> m (Maybe a)
whenMaybe True  c = liftM Just c
whenMaybe False _ = return Nothing

getFileField :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> f (Maybe BS.ByteString)
getFileField name = do
    finput <- getDataFn (lookInput name)
    case finput of
        Right (Input contentspec _ _)->
            case contentspec of
                Left filepath -> joinEmpty <$> Just . concatChunks <$> (liftIO $ BSL.readFile filepath)
                Right content -> return $ joinEmpty . Just $ concatChunks content
        _ -> return Nothing

-- | Pack value to just unless we have 'mzero'.  Since we can not check
-- emptyness of string in templates we want to pack it in maybe.
nothingIfEmpty::(Eq a, Monoid a) => a -> Maybe a
nothingIfEmpty a = if mempty == a then Nothing else Just a

-- | Failing if inner value is empty
joinEmpty::(MonadPlus m, Monoid a, Ord a) => m a -> m a
joinEmpty m = do
                mv <- m
                if mv == mempty
                 then mzero
                 else return mv

{-| This function is useful when creating 'Typeable' instance when we
want a specific name for type.  Example of use:

  > instance Typeable Author where typeOf _ = mkTypeOf "XX_Author"

-}
mkTypeOf :: String -> TypeRep
mkTypeOf name = mkTyConApp (mkTyCon name) []

-- | Pad string with zeros at the beginning.
pad0 :: Int         -- ^ how long should be the number
     -> String      -- ^ the number as string
     -> String      -- ^ zero padded number
pad0 len str = take missing (repeat '0') ++ str
    where
        diff = len - length str
        missing = max 0 diff

-- | Logging left to error log
eitherLog :: IO (Either String b) -> IO b
eitherLog action = do
  value <- action
  case value of
    Left errmsg -> do
      Log.error errmsg
      error errmsg
    Right val -> return val

-- | Triples
fst3 :: (t1, t2, t3) -> t1
fst3 (a,_,_) = a

snd3 :: (t1, t2, t3) -> t2
snd3 (_,b,_) = b

thd3 :: (t1, t2, t3) -> t3
thd3 (_,_,c) = c

para :: String -> String
para s = "<p>" ++ s ++ "</p>"

-- HTTPS utils
isIphone::ServerMonad m => m Bool
isIphone =  do
    magent <- fmap BS.toString  `liftM` (getHeaderM "User-Agent")
    case magent of
         Nothing -> return False
         Just agent -> return $ "iphone" `isInfixOf` (map toLower agent)

isSecure::ServerMonad m => m Bool
isSecure = (Just (BS.fromString "http") /=) `liftM` getHeaderM "scheme"

isHTTPS :: (ServerMonad m) => m Bool
isHTTPS = do
  rq <- askRq
  let mscheme = getHeader "scheme" rq
  return $ mscheme == Just (BS.fromString "https")

getHostpart :: ServerMonad m => m String
getHostpart = do
  rq <- askRq
  let hostpart = maybe "scrive.com" BS.toString $ getHeader "host" rq
  let scheme = maybe "http" BS.toString $ getHeader "scheme" rq
  return $ scheme ++ "://" ++ hostpart

getSecureLink :: ServerMonad m => m String
getSecureLink = (++) "https://" `liftM` currentLinkBody

currentLink :: ServerMonad m => m String -- We use this since we can switch to HTTPS whenever we wan't
currentLink = do
  secure <- isHTTPS
  urlbody <- currentLinkBody
  return $ if secure
    then "https://" ++ urlbody
    else "http://"  ++ urlbody

currentLinkBody :: ServerMonad m => m String
currentLinkBody = do
  rq <- askRq
  let hostpart = maybe "scrive.com" BS.toString $ getHeader "host" rq
  let fixurl a1 a2 = if ("/" `isSuffixOf` a1 && "/" `isPrefixOf` a2)
                     then drop 1 a2
                     else a2
  return $ hostpart ++ fixurl hostpart (rqUri rq) ++ fixurl (rqUri rq) (rqURL rq)

querystring :: (ServerMonad m, HasRqData m, MonadIO m) => m String
querystring = qs `liftM` queryString lookPairs
  where
    qs qsPairs =
      let encodeString  = URL.encode . map (toEnum . ord)
          relevantPairs = [ (k, v) | (k, Right v) <- qsPairs ]
          empties       = [ encodeString k | (k, "") <- relevantPairs ]
          withValues    = [ encodeString k ++ "=" ++ encodeString v | (k, v) <- relevantPairs, length v > 0 ]
      in if Data.List.null relevantPairs
          then ""
          else "?" ++ intercalate "&" (empties ++ withValues)

pureString::String -> String
pureString s = unwords $ words $ filter (not . isControl) s

pairMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
pairMaybe (Just a) (Just b) = Just (a, b)
pairMaybe _ _ = Nothing

pairMaybe3 :: Maybe a -> Maybe b -> Maybe c -> Maybe (a, b, c)
pairMaybe3 (Just a) (Just b) (Just c) = Just (a, b, c)
pairMaybe3 _ _ _ = Nothing

maybeReadM::(Monad m,Read a,Functor m) =>  m (Maybe String) -> m (Maybe a)
maybeReadM c = join <$> fmap maybeRead <$> c

maybeRead::(Read a) => String -> Maybe a
maybeRead s = case reads s of
            [(v,"")] -> Just v
            _        -> Nothing

maybeReadIntM::(Monad m,Functor m) => m (Maybe String) -> m (Maybe Int)
maybeReadIntM c = join <$> fmap maybeReadInt <$> c

maybeReadInt::String -> Maybe Int
maybeReadInt s = case readDec s of
            [(v,"")] -> Just v
            _        -> Nothing

class URLAble a where
   encodeForURL::a -> String

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "Reading Left for Right"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Reading Right for Left"

toMaybe :: Either a b -> Maybe b
toMaybe (Right a) = Just a
toMaybe _ = Nothing

joinB :: Maybe Bool -> Bool
joinB (Just b) = b
joinB _ = False

mapJust :: (a -> Maybe b) -> [a] -> [b]
mapJust f l = catMaybes $ map f l

mapFst::(Functor f) => (a -> c) -> f (a,b)  -> f (c,b)
mapFst = fmap . first

mapSnd::(Functor f) => (b -> c)  -> f (a,b) -> f (a,c)
mapSnd = fmap . second

propagateFst :: (a,[b]) -> [(a,b)]
propagateFst (a,bs) = for bs (\b -> (a,b))

propagateMonad :: (Monad m)  => [(a, m b)] -> m [(a,b)]
propagateMonad ((a,mb):rest) = do
    b <- mb
    rest' <- propagateMonad rest
    return $ (a,b): rest'

propagateMonad _ = return []


-- Splits string over some substring
splitOver:: (Eq a) => [a] -> [a] -> [[a]]
splitOver = splitOver' []
    where
        splitOver' [] _ []  = []
        splitOver' c _ []  = [reverse c]
        splitOver' c a b@(bh:bt) =
            if (a `isPrefixOf` b)
             then (reverse c) : (splitOver' [] a (drop (length a) b) )
             else splitOver' (bh:c) a bt

(||^):: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(||^) f g a =  f a || g a

(&&^):: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&^) f g a =  f a && g a

-- To be extended
smartZip::[Maybe a] -> [b] -> [(a,b)]
smartZip ((Just a): as) (b:bs) = (a,b):(smartZip as bs)
smartZip (_: as) (_:bs) = smartZip as bs
smartZip _ _  = []


-- Check recursively time of modification of any file in directory
getRecursiveMTime :: [Char] -> IO ClockTime
getRecursiveMTime "."  = return $ TOD 0 0
getRecursiveMTime ".." = return $ TOD 0 0
getRecursiveMTime dir = do
     isDir <- doesDirectoryExist dir
     if not isDir
      then getRecursiveMTime dir
      else do
          files <- getDirectoryContents dir
          mts <- forM files $ \fn -> getModificationTime $ dir ++ "/" ++fn
          mt <- getModificationTime dir
          return $ maximum $ mt:mts

jsText :: String -> String
jsText  = filter (not . isControl)

instance (Enum a, Bounded a, Enum b, Bounded b) => Enum (a,b) where
    toEnum  l = let
                   block = length (allValues::[a])
                in (toEnum $ l `div` block,toEnum $ l `mod` block)
    fromEnum (a,b) = let
                        block = length (allValues::[a])
                in (fromEnum a * block) + (fromEnum b)

none :: (a -> Bool) -> [a] -> Bool
none f l = not $ any f l

-- | Simple logical inference operator (arrow)
(=>>) :: Bool -> Bool -> Bool
(=>>) a b = not a || b

-- | Higher order inference
(=>>^) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(=>>^) a b x = a x =>> b x

-- | Double Inference
(<<=>>) :: Bool -> Bool -> Bool
(<<=>>) = (==)

-- | Higher order double inference
(<<==>>^) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(<<==>>^) a b x = a x == b x

-- | Conditional choice operator
-- Use it like this: a <| condition |> b  is equivalent to if condition then a else b
-- http://zenzike.com/posts/2011-08-01-the-conditional-choice-operator
(|>) :: Bool -> a -> Maybe a
True  |> _ = Nothing
False |> y = Just y

(<|) :: a -> Maybe a -> a
x <| Nothing = x
_ <| Just y  = y

infixr 0 <|
infixr 0 |>

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith k ls = sortBy (\a b-> compare (k a) (k b)) ls

groupWith :: Eq b => (a -> b) -> [a] -> [[a]]
groupWith k ls = Data.List.groupBy (\a b -> k a == k b) ls

ignore :: Monad m => m a -> m ()
ignore a = a >> return ()

mapassoc :: (a -> b) -> [a] -> [(a, b)]
mapassoc f = map (id &&& f)

mapassocM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapassocM f = mapM (\a -> return . (a,) =<< f a)

basename :: String -> String
basename filename =
  case break (\x -> (x=='\\') || (x=='/')) filename of
    (_,(_:rest)) -> basename rest
    _            -> takeWhile ((/=) '.') filename

($^) :: Maybe (a -> a) -> a -> a
($^) Nothing  a = a
($^) (Just f) a = f a

($^^) :: [a -> a] -> a -> a
($^^) []  a = a
($^^) (f : fs) a = fs $^^ f a


toCSV :: [String] -> [[String]] -> BSL.ByteString -- Use bytestrings here for speed. Else you can get stack overflow
toCSV header ls =
   BSL.concat $ map csvline (header:ls)
    where csvline line = BSL.concat $ [BSL.fromString "\"", BSL.intercalate (BSL.fromString "\",\"") (map BSL.fromString line),BSL.fromString  "\"\n"]

{- Version of elem that as a value takes Maybe-}    
melem :: (Eq a) => Maybe a -> [a] -> Bool
melem Nothing   _  = False
melem (Just  e) es = elem e es

firstWithDefault :: (Monad m) => [m (Maybe a)] -> m a -> m a
firstWithDefault [] da = da
firstWithDefault (ma:mas) da = do
    a <- ma
    case a of
         Just a' -> return a'
         Nothing -> firstWithDefault mas da
         
-- changing an element in a list
chng :: [a] -> Int -> a -> [a]
chng ls i v = let (h, t) = splitAt i ls
              in h ++ [v] ++ (drop 1 t)

-- get the length with a filter
lengthWith :: (a -> Bool) -> [a] -> Int
lengthWith f l = length $ filter f l

optional :: (MonadPlus m) => m a -> m (Maybe a)
optional c = (liftM Just c) `mplus` (return Nothing)

urlDecodeVars :: String -> Maybe [(String, String)]
urlDecodeVars ('?':s) = urlDecodeVars s
urlDecodeVars s = makeKV (splitOver "&" s) []
  where makeKV [] a = Just a
        makeKV (kv:ks) a = case break (== '=') kv of
          (k, '=':v) -> makeKV ks ((urlDecode k, urlDecode v):a)
          (k, "") -> makeKV ks ((urlDecode k, ""):a)
          _ -> Nothing
          
containsAll :: Eq a => [a] -> [a] -> Bool
containsAll elems inList = foldl (\a e-> a && e `elem` inList) True elems

listsEqualNoOrder :: Eq a => [a] -> [a] -> Bool
listsEqualNoOrder a b = containsAll a b && containsAll b a

listDiff :: Eq a => [a] -> [a] -> ([a], [a], [a])
listDiff a b = ([x|x <- a, x `notElem` b], 
                [x|x <- a, x `elem`    b],
                [x|x <- b, x `notElem` a])
