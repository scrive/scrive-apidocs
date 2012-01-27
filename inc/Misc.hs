{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| Dump bin for things that do not fit anywhere else

I do not mind people sticking stuff in here. From time to time just
please go over this file, reorganize, pull better parts to other
modules.

Keep this one as unorganized dump.
-}
module Misc where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad.State
import Data.Char
import Data.Data
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Word
import Numeric (readDec)
import Happstack.Data.IxSet as IxSet
import Happstack.Server hiding (simpleHTTP,dir)
import Happstack.State
import Happstack.Util.Common hiding  (mapFst,mapSnd)
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import System.Random (randomRIO)
import System.Time
import qualified Log
import qualified Codec.Binary.Url as URL
import qualified Control.Exception as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS
import qualified GHC.Conc
import Data.Bits

-- We want this operators to bind strongly but weeker then . to do cond1 &&^ not . cond2
infixl 8  &&^
infixl 8  ||^
infixr 9 $^ 
infixr 9 $^^

selectFormAction :: (HasRqData m, MonadIO m,MonadPlus m,ServerMonad m) => [(String,m a)] -> m a
selectFormAction [] = mzero
selectFormAction ((button,action):rest) = do
  maybepressed <- getDataFn (look button)
  either (\_ -> selectFormAction rest) (\_ -> action) maybepressed

guardFormAction :: (HasRqData m, MonadIO m,ServerMonad m, MonadPlus m) => String -> m ()
guardFormAction button = do
  maybepressed <- getDataFn (look button)
  either (\_ -> mzero) (\_ -> return ()) maybepressed

concatChunks :: BSL.ByteString -> BS.ByteString
concatChunks = BS.concat . BSL.toChunks

-- | Get a unique index value in a set. Unique number is 31 bit in
-- this function.  First argument is set, second is index constructor.
--
-- See also 'getUnique64'.
getUnique
  :: (Indexable a,
      Typeable a,
      Ord a,
      Typeable k,
      Monad (t GHC.Conc.STM),
      MonadTrans t) =>
     IxSet a -> (Int -> k) -> Ev (t GHC.Conc.STM) k
getUnique ixset constr = do
  r <- getRandomR (0,0x7fffffff::Int)
  let v = constr r
  if IxSet.null (ixset @= v)
     then return v
     else getUnique ixset constr

-- | Get a unique index value in a set. Unique number is 31 bit in
-- this function.  First argument is set, second is index constructor.
--
-- See also 'getUnique64'.
getUnique64
  :: (Indexable a,
      Typeable a,
      Ord a,
      Typeable k,
      Monad (t GHC.Conc.STM),
      MonadTrans t) =>
     IxSet a -> (Int64 -> k) -> Ev (t GHC.Conc.STM) k
getUnique64 ixset constr = do
  r <- getRandomR (0,0x7fffffffffffffff::Int64)
  let v = constr r
  if IxSet.null (ixset @= v)
     then return v
     else getUnique64 ixset constr

-- | Generate random string of specified length that contains allowed chars
randomString :: MonadIO m => Int -> [Char] -> m String
randomString n allowed_chars = liftIO $
    sequence $ replicate n $ ((!!) allowed_chars <$> randomRIO (0, len))
    where
        len = length allowed_chars - 1

-- | Extract data from GET or POST request. Fail with 'mzero' if param
-- variable not present or when it cannot be read.
getDataFnM :: (HasRqData m, MonadIO m, ServerMonad m, MonadPlus m) => RqData a -> m a
getDataFnM fun = do
  m <- getDataFn fun
  either (\_ -> mzero) (return) m

-- | Since we sometimes want to get 'Maybe' and also we wont work with
-- newer versions of happstack here is.  This should be droped when
-- new version is globaly established.
getDataFn' :: (HasRqData m, MonadIO m, ServerMonad m) => RqData a -> m (Maybe a)
getDataFn' fun = do
  m <- getDataFn fun
  either (\_ -> return Nothing) (return . Just ) m

-- | This is a nice attempt at generating database queries directly
-- from URL parts.
pathdb
  :: (FromReqURI a,
      MonadPlus m,
      ServerMonad m,
      MonadIO m,
      QueryEvent a1 (Maybe t)) =>
     (a -> a1) -> (t -> m b) -> m b
pathdb getfn action = path $ \idd -> do
  m <- query $ getfn idd
  case m of
    Nothing -> mzero
    Just obj -> action obj

-- | Get param as strict ByteString instead of a lazy one.
getAsStrictBS :: (HasRqData f, MonadIO f, ServerMonad f, MonadPlus f, Functor f) =>
     String -> f BS.ByteString
getAsStrictBS name = fmap concatChunks (getDataFnM (lookBS name))

-- | Useful inside the 'RqData' monad.  Gets the named input parameter
-- (either from a @POST@ or a @GET@)
lookInputList :: String -> RqData [BSL.ByteString]
lookInputList name
    = do
         inputs <- (\(a, b, _) -> a ++ fromMaybe [] b) <$> askRqEnv
         let isname (xname,(Input value _ _)) | xname == name = [value]
             isname _ = []
         return [value | k <- inputs, eithervalue <- isname k, Right value <- [eithervalue]]

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


-- | Run action, record failure if any.
logErrorWithDefault :: IO (Either String a)  -- ^ action to run
                    -> b                     -- ^ default value in case action failed
                    -> (a -> IO b)           -- ^ action that uses value
                    -> IO b                  -- ^ result
logErrorWithDefault c d f = do
    c' <- c
    case c' of
        Right c'' ->  f c''
        Left err  ->  do
                Log.error err
                return d

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
    fromSafeEnum::(Integral b) =>  a -> b
    toSafeEnum::(Integral b) =>  b -> Maybe a

fromSafeEnumInt :: (SafeEnum a) => a -> Int
fromSafeEnumInt = fromSafeEnum

toSafeEnumInt :: (SafeEnum a) => Int -> Maybe a
toSafeEnumInt = toSafeEnum

-- | Just @flip map@.
for :: [a] -> (a -> b) -> [b]
for = flip map

isFieldSet :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> f Bool
isFieldSet name = isJust <$> getField name

getFields :: (HasRqData m, MonadIO m, ServerMonad m,Functor m) => String -> m [String]
getFields name = (map BSL.toString)  <$> (fromMaybe []) <$> getDataFn' (lookInputList name)

getField :: (HasRqData m, MonadIO m, ServerMonad m,Functor m) => String -> m (Maybe String)
getField name = listToMaybe . reverse <$> getFields name

getFieldBS :: (HasRqData m, MonadIO m, ServerMonad m,Functor m) => String -> m (Maybe BSL.ByteString)
getFieldBS name = getDataFn' (lookBS name)

getFieldUTF
  :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> f (Maybe BS.ByteString)
getFieldUTF name = (fmap BS.fromString) <$> getField name

getFieldWithDefault
  :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> String -> f String
getFieldWithDefault d name =   (fromMaybe d) <$> getField name

getFieldBSWithDefault
  :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) =>
     BSL.ByteString -> String -> f BSL.ByteString
getFieldBSWithDefault  d name = (fromMaybe d) <$> getFieldBS name

getFieldUTFWithDefault
  :: (HasRqData f, MonadIO f, Functor f, ServerMonad f) =>
     BS.ByteString -> String -> f BS.ByteString
getFieldUTFWithDefault  d name = (fromMaybe d) <$> getFieldUTF name

readField
  :: (HasRqData f, MonadIO f, Read a, Functor f, ServerMonad f) => String -> f (Maybe a)
readField name =  (join . (fmap readM)) <$> getField name

whenMaybe::(Functor m,Monad m) => Bool -> m a -> m (Maybe a)
whenMaybe True  c = fmap Just c
whenMaybe False _ = return Nothing

getFileField:: (HasRqData f, MonadIO f, Functor f, ServerMonad f) => String -> f (Maybe BS.ByteString)
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


mapIf::(a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf cond f = map (\a -> if (cond a) then f a else a)

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

-- HTTPS utils

isSecure::(ServerMonad m,Functor m) => m Bool
isSecure = do
     (Just (BS.fromString "http") /=) <$> (getHeaderM "scheme")

isHTTPS :: (ServerMonad m) => m Bool
isHTTPS = do
    rq <- askRq
    let mscheme = getHeader "scheme" rq
    return $ mscheme == Just (BS.fromString "https")

getHostpart :: (ServerMonad m, Functor m) => m String
getHostpart = do
  rq <- askRq
  let hostpart = maybe "skrivapa.se" BS.toString $ getHeader "host" rq
  let scheme = maybe "http" BS.toString $ getHeader "scheme" rq
  return $ scheme ++ "://" ++ hostpart

getSecureLink :: (ServerMonad m, Functor m) => m String
getSecureLink = (++) "https://" <$>  currentLinkBody


currentLink :: (ServerMonad m, Functor m) => m String -- We use this since we can switch to HTTPS whenever we wan't
currentLink = do
  secure <- isHTTPS
  urlbody   <- currentLinkBody
  if secure
    then return $ "https://" ++ urlbody
    else return $ "http://"  ++ urlbody

currentLinkBody :: (ServerMonad m, Functor m) => m String
currentLinkBody = do
  rq <- askRq
  let hostpart = maybe "skrivapa.se" BS.toString $ getHeader "host" rq
  let fixurl a1 a2 = if ("/" `isSuffixOf` a1 && "/" `isPrefixOf` a2)
                     then drop 1 a2
                     else a2
  return $ hostpart ++ fixurl hostpart (rqUri rq) ++ fixurl (rqUri rq) (rqURL rq)

para :: String -> String
para s = "<p>" ++ s ++ "</p>"

encodeString :: String -> String
encodeString = URL.encode . map (toEnum . ord)

qs :: [(String, Either a String)] -> String
qs qsPairs =
    let relevantPairs = [ (k, v) | (k, Right v) <- qsPairs ]
        empties       = [ encodeString k | (k, "") <- relevantPairs ]
        withValues    = [ encodeString k ++ "=" ++ encodeString v | (k, v) <- relevantPairs, length v > 0 ]
    in if Data.List.null relevantPairs
        then ""
        else "?" ++ intercalate "&" (empties ++ withValues)

querystring :: (ServerMonad m, HasRqData m, MonadIO m) => m String
querystring = do
    qsPairs <- queryString lookPairs
    return $ qs qsPairs

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

joinB:: Maybe Bool -> Bool
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

mapPair::(Functor f) => (a -> b) -> f (a,a)  -> f (b,b)
mapPair f = fmap (\(a1,a2) -> (f a1, f a2))

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

newtype IPAddress = IPAddress Word32
  deriving (Eq, Ord, Typeable, Serialize, Version)

unknownIPAddress :: IPAddress
unknownIPAddress = IPAddress 0

instance Show IPAddress where
  showsPrec p (IPAddress n) = showsPrec p n

instance Read IPAddress where
  readsPrec _ = map (first IPAddress) . readDec

-- oh boy, this is really network byte order!
formatIP :: IPAddress -> String
formatIP (IPAddress 0) = ""
-- formatIP 0x7f000001 = ""
formatIP (IPAddress x) =
              " (IP: " ++ show ((x `shiftR` 0) .&. 255) ++
                   "." ++ show ((x `shiftR` 8) .&. 255) ++
                   "." ++ show ((x `shiftR` 16) .&. 255) ++
                   "." ++ show ((x `shiftR` 24) .&. 255) ++ ")"

ignore :: Monad m => m a -> m ()
ignore acc = do
  _ <- acc
  return ()
  
pair :: a -> b -> (a, b)
pair a b = (a, b)

mapassoc :: (a -> b) -> [a] -> [(a, b)]
mapassoc f l = map (\a -> pair a $ f a) l

mapassocM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
mapassocM f l = mapM (\a -> (return . pair a) =<< f a) l

basename :: String -> String
basename filename =
  case break (\x -> (x=='\\') || (x=='/')) filename of
    (_,(_:rest)) -> basename rest
    _            -> takeWhile ((/=) '.') filename

indentLinesMore :: Int -> String -> String
indentLinesMore nspaces sublines =
  case lines sublines of
    (x:xs) -> unlines $ x : map (spaces ++) xs
    [] -> []
  where 
    spaces = replicate nspaces ' '

($^) :: Maybe (a -> a) -> a -> a
($^) Nothing  a = a
($^) (Just f) a = f a

($^^) :: [a -> a] -> a -> a
($^^) []  a = a
($^^) (f : fs) a = fs $^^ f a


toCSV :: [String] -> [[String]] -> String
toCSV header ls =
  concatMap csvline (header:ls)
    where csvline line = "\"" ++ intercalate "\",\"" line ++ "\"\n"

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
