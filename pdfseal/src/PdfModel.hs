
{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fbang-patterns #-}

module PdfModel where
import Prelude hiding (String)
import qualified Prelude as P
import Control.Monad.State.Strict
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import System.IO
import Control.Monad
import Control.Exception
import Numeric
import Data.Bits
import Data.Word
import Data.List
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as  BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Internal as BSB
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Matrix
import qualified ReadP as P
import qualified Data.IntSet as IntSet
import Control.Parallel.Strategies
import qualified Char
import qualified Prelude

import System.IO.MMap
import qualified Data.Binary.Builder as Bin
import Data.Monoid
import Text.Printf
import Data.Ratio
import Control.Monad.Writer.Class
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Control.Monad.State.Class
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
-- import Debug.Trace

import GHC.Num


type MyReadP a = P.ReadP a

data Document = Document
                   { documentHeader :: BS.ByteString                  -- header
                   , documentBodies :: [Body]                         -- bodies, from last to first
                   }

data Body = Body 
                   { bodyTrailer :: DictData
                   , bodyObjects :: IntMap.IntMap Entry   -- trailer and object map
                   }

data Entry = UsedEntry !Int !Indir                -- generation and indirect object
           | FreeEntry !Int                       -- next available generation
           deriving (Show)
data Indir = Indir Value (Maybe BSL.ByteString)   -- object value and associated encoded stream data
           deriving (Show)
           
type StringData = BS.ByteString
type ArrayData = [Value]
type DictData = [(BS.ByteString,Value)]
data Value = Boolean !Bool                        -- true or false
           | Null                                 -- null
           | Ref !RefID                           -- refid, bits 31..8 refno, 7..0 generation
           | Number !Double                       -- the number
           | String !Bool BS.ByteString           -- string, true is hex, then unencoded form
           | Name BS.ByteString                   -- encoded form of name
           | Operator BS.ByteString               -- encoded form of name
           | Comment BS.ByteString                -- encoded form of name
           | Array ArrayData                      -- array of values
           | Dict DictData                        -- a map from encoded names to values
           deriving (Eq,Ord)

{-
instance NFData Value where
    rnf (Array x) = seqList rnf x
    rnf (Dict x) = seqList rnf x
    rnf x = rwhnf x

instance NFData BS.ByteString
instance NFData Entry

instance NFData Indir where
    rnf (Indir value stream) = rnf value `seq` rwhnf stream

instance NFData Body where
    rnf (Body trailer objects) = rnf trailer `seq` seqList rnf (IntMap.toList objects)

instance NFData Document where
    rnf (Document a b) = rnf a `seq` rnf b
-}


class Binarize a where
    binarize :: a -> Bin.Builder
    binarizeList :: [a] -> Bin.Builder
    binarizeList x = mconcat (map binarize x)

instance Binarize a => Binarize [a] where
    binarize a = binarizeList a


class IsValue a where
    value :: a -> Value

instance IsValue Value where
    value x = x

instance IsValue RefID where
    value x = Ref x

instance IsValue () where
    value () = Null

instance IsValue Double where
    value x = Number x

instance IsValue Int where
    value x = Number (fromIntegral x)

instance IsValue Bool where
    value x = Boolean x

instance IsValue P.String where
    value x = Name (BSC.pack x)

instance IsValue Matrix where
    value (Matrix a b c d e f) = arrayn [ a, b, c, d, e, f ]

instance IsValue BS.ByteString where
    value x = Name x

instance (IsValue a) => IsValue ([(BS.ByteString,Value)] -> a) where
    value x = value (x [])

valuen :: Real x => x -> Value
valuen = Number . realToFrac

array x = Array . map value $ x

unpsname (Name x ) = x

string x = String False . unpsname . name $ x

hexstring x = String True . unpsname . name $ x

class IsName a where
    name :: a -> Value

instance IsName P.String where
    name x = Name (BSC.pack x)

instance IsName BS.ByteString where
    name x = Name x

-- Exponentiation with a cache for the most common numbers.
expt :: Int -> Integer
expt n = 10^n

floatToDigits2 :: Double -> ([Int], Int)
floatToDigits2 0 = ([0], 0)
floatToDigits2 x =
 let
  (f0, e0) = decodeFloat x
  (minExp0, _) = floatRange x
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent                                 
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (f, e) = 
   let n = minExp - e0 in
   if n > 0 then (f0 `div` (b^n), e0+n) else (f0, e0)
  (r, s, mUp, mDn) =
   if e >= 0 then
    let be = b^ e in
    if f == b^(p-1) then
      (f*be*b*2, 2*b, be*b, b)
    else
      (f*be*2, 2, be, be)
   else
    if e > minExp && f == b^(p-1) then
      (f*b*2, b^(-e+1)*2, b, 1)
    else
      (f*2, b^(-e)*2, 1, 1)
  k :: Int
  k =
   let
    k0 :: Int
    k0 =
     if b == 2 then
        -- logBase 10 2 is slightly bigger than 3/10 so
        -- the following will err on the low side.  Ignoring
        -- the fraction will make it err even more.
        -- Haskell promises that p-1 <= logBase b f < p.
        (p - 1 + e0) * 3 `div` 10
     else
        ceiling ((log (fromInteger (f+1)) +
                 fromIntegral (e) * log (fromInteger b)) /
                   log 10)
--WAS:            fromInt e * log (fromInteger b))

    fixup n =
      if n >= 0 then
        if r + mUp <= expt n * s then n else fixup (n+1)
      else
        if expt (-n) * (r + mUp) <= s then n else fixup (n+1)
   in
   fixup k0

  gen ds rn sN mUpN mDnN =
   let
    (dn, rn') = (rn * 10) `divMod` sN
    mUpN' = mUpN * 10
    mDnN' = mDnN * 10
   in
   case (rn' < mDnN', rn' + mUpN' > sN) of
    (True,  False) -> dn : ds
    (False, True)  -> dn+1 : ds
    (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
    (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'

  rds =
   if k >= 0 then
      gen [] r (s * expt k) mUp mDn
   else
     let bk = expt (-k) in
     gen [] (r * bk) s (mUp * bk) (mDn * bk)
 in
 (map fromIntegral (reverse rds), k)



ss [0] 0 = "0"
ss xs1 n
    | n<=0 = '.' : (take (-n) zeros ++ xs)
    | otherwise = let (a,b) = splitAt n xs
                      c = take n (a ++ zeros)
                  in case b of
                        [] -> c
                        _ -> c ++ "." ++ b
    where xs = map (\x -> Char.chr (x+Char.ord '0')) xs1
          zeros = repeat '0'

showFFloat2 x = result
    where (digits,exp) = floatToDigits2 x1
          (x1,result) = if x < 0
                           then (-x,'-' : ss digits exp)
                           else (x, ss digits exp)


instance Binarize Value where
    --binarize value = Strict.evalState (Strict.execWriterT (binarizeValue value)) False -- 62s
    -- binarize value = Lazy.execWriter (Strict.evalStateT (binarizeValue value) False) -- 63s
    binarize value = Strict.execWriter (Strict.evalStateT (binarizeValue value) False) -- 64s

maybeSpace = do
    s <- get
    if s
       then tell (bin_builder_char ' ')
       else return ()

--binarizeValue :: Value -> Control.Monad.Writer.Lazy.Writer Bin.Buffer
binarizeValue (Boolean False) = do
    maybeSpace
    tell $ bin_builder_string "false"
    put True
binarizeValue (Boolean True) = do
    maybeSpace
    tell $ bin_builder_string "true"
    put True
binarizeValue (Null) = do
    maybeSpace
    tell $ bin_builder_string "null"
    put True
binarizeValue (Ref x) = do
    maybeSpace
    tell $ bin_builder_string (showref x)
    put True
binarizeValue (Number x) = do
    maybeSpace
    tell $ bin_builder_string (showFFloat2 x)
    put True
binarizeValue (String False x) = do
    tell $ bin_builder_char '('
    tell $ bin_builder_bytestring x
    tell $ bin_builder_char ')'
    put False
binarizeValue (String True x) = do
    tell $ bin_builder_char '<'
    tell $ bin_builder_bytestring (hexencode x)
    tell $ bin_builder_char '>'
    put False
binarizeValue (Name x) = do
    tell $ bin_builder_char '/'
    tell $ bin_builder_bytestring x
    put True
binarizeValue (Array x) = do
    tell $ bin_builder_char '['
    put False
    mapM_ binarizeValue x
    put False
    tell $ bin_builder_char ']'
binarizeValue (Dict x) = do
    tell $ bin_builder_string "<<"
    put False
    binarizedictcontents x
    put False
    tell $ bin_builder_string ">>"
binarizeValue (Operator x) = do
    maybeSpace
    tell $ bin_builder_bytestring x
    put True


bin_builder_char x = Bin.singleton (BSB.c2w x)
bin_builder_string x =
    bin_builder_bytestring (BSC.pack x)
bin_builder_bytestring x =
    mconcat (map Bin.singleton (BS.unpack x))

hexencode x = BS.concatMap hex x
    where hex x = BS.pack [hex1 ((x `shiftR` 4) .&. 15),hex1 (x .&. 15)]
          hex1 a | a<10 = a + 48
                 | otherwise = a + 55

binarizedictcontents [] = return ()
binarizedictcontents ((k,v):xs)
    = do
        binarizepair (k,v)
        binarizedictcontents xs

binarizepair (a,Null) = return ()
binarizepair (a,b) = do
    tell $ bin_builder_char '/'
    tell $ bin_builder_bytestring a
    put True
    binarizeValue b
    return ()

instance Show Value where
    show x = BSLC.unpack (Bin.toLazyByteString (binarize x))


entry :: (IsName a,IsValue b) => a -> b -> (BS.ByteString,Value)
entry a b = (unpsname (name a), value b)

arrayn :: Real a => [a] -> Value
arrayn x = array (map realToFrac x :: [Double])

entryn :: (IsName a,Real b) => a -> b -> (BS.ByteString,Value)
entryn a b = entry a (realToFrac b :: Double)

-- entrya :: IsName a => a -> Double -> (BS.ByteString,Value)
entrya a b = entry a (array b)

entryna a b = entry a (arrayn b)

entrys a b = entry a (string b)

showpair (a,b) = "/" ++ BSC.unpack a ++ " " ++ show b

newtype RefID = RefID Int deriving (Eq,Ord)

instance Show RefID where
    show r = show (objno r) ++ " " ++ show (gener r) ++ " R"

refid :: Int -> Int -> RefID
refid a b = RefID ((a `shiftL` 8) .|. (b .&. 255))

gener (RefID a) = a .&. 255

objno (RefID a) = a `shiftR` 8


showref v = show (objno v) ++ " " ++ show k ++ " R"
    where k = if z==255 then 65535 else z
          z = gener v

dict x = Dict . map (\(a,b) -> (a, value b)) $ x

empty_dict = Dict []

documentF :: BS.ByteString -> Document
documentF version = Document version bodies
    where
        bodies = [Body ([]) (IntMap.singleton 0 (FreeEntry 65535))]

document :: BS.ByteString -> State Document ()
document version = put (documentF version)

upgradeF :: Document -> Document
upgradeF (Document ver bodies) = (Document ver (Body ([]) (IntMap.empty) : bodies))

upgrade :: State Document ()
upgrade = modify upgradeF

trailerF :: DictData -> Document -> Document
trailerF v (Document ver ((Body trailer objs):bodies)) = Document ver ((Body v objs):bodies)

trailer :: DictData -> State Document ()
trailer x = modify (trailerF x)

nextFreeIdF :: Document -> RefID
nextFreeIdF doc = refid x 0
    where
        bodies = documentBodies doc
        x = 1 + maximum (0 : map lastofbody bodies)
        lastofbody (Body trailer m) = maybe 0 (\((key,_),_) -> key) (IntMap.maxViewWithKey m)

addIndir :: Indir -> State Document RefID
addIndir object = do
    doc <- get
    let (ndoc,r) = addIndirF object doc
    put ndoc
    return r

addIndirF :: Indir -> Document -> (Document,RefID)
addIndirF object doc@(Document ver ((Body trailer objs):bodies)) = (ndoc,refid)
    where
        refid = nextFreeIdF doc
        value = UsedEntry (gener refid) object
        newbody = Body trailer (IntMap.insert (objno refid) value objs)
        ndoc = (Document ver (newbody:bodies))

setIndir :: RefID -> Indir -> State Document ()
setIndir refid indir = modify (setIndirF refid indir)

setIndirF :: RefID -> Indir -> Document -> Document
setIndirF refid object doc@(Document ver ((Body trailer objs):bodies)) = ndoc
    where
        key = objno refid
        value = UsedEntry (gener refid) object
        newbody = Body trailer (IntMap.insert key value objs)
        ndoc = (Document ver (newbody:bodies))

setObject :: RefID -> Value -> State Document ()
setObject refid value = setIndir refid (Indir value Nothing)

addObject :: Value -> State Document RefID
addObject text = addIndir (Indir text Nothing)

addStream :: Value -> BSL.ByteString -> State Document RefID
addStream value@(Dict dt) strm
    | Just x <- Prelude.lookup (BSC.pack "Length") dt =
        addIndir (Indir value (Just strm))
    | otherwise = 
        do rec s <- addIndir (Indir (Dict (entry "Length" cl:dt)) (Just strm))
               cl <- addIndir (Indir (Number 0) Nothing)
           return s
          
addStream value strm = error "Stream must begin with a dict"

lookup :: RefID -> Document -> Maybe Indir
lookup refid (Document _ bodies) = msum (map check bodies)
    where
        check (Body _ objmap) = case IntMap.lookup (objno refid) objmap of
                          Just (UsedEntry gen indir) -> if gen==gener refid
                                                           then Just indir
                                                           else Nothing
                          _ -> Nothing

--writeObject :: (BS.ByteString -> IO ())     -- write action
--            -> (IO Int)                     -- tell offset action
--            -> (Int,Map.Map Int (Int,Int))
--            -> (Int,Entry)                  -- (objno,entry)
--            -> IO () -- (Maybe (Int,(Int,Int)))   -- objno,gener,offset
writeObject write tell (lastlength,objnooffsetmap) ((objno,UsedEntry gener (Indir text stream))) = do
    pos <- tell
    write (BSC.pack (show objno ++ " " ++ show gener ++ " obj\n"))
    case text of
        Number x -> do -- oh my simplification
            write (BSC.pack (show lastlength))
        _ -> do
            mapM_ write (BSL.toChunks (Bin.toLazyByteString (binarize text)))
            return ()
    newlastlength <- case stream of
        Just x -> do
            write $ BSC.pack "\nstream\n"
            b <- tell
            mapM_ write (BSL.toChunks x)
            e <- tell
            write $ BSC.pack "\nendstream"
            return (e-b)
        _ -> return 0
    write $ BSC.pack "\nendobj\n"
    let newobjnoffsetmap = IntMap.insert objno pos objnooffsetmap
    return (newlastlength,newobjnoffsetmap)

writeObject write tell x ((objno,_)) = return x

createXRefTable :: IntMap.IntMap Int
                -> [(Int,Entry)]
                -> [(Int,[Either (Int,Int) (Int,Int)])]
createXRefTable offsetmap objects =
    group (map x objects)
    where
        x (objno,(UsedEntry gener _)) = let Just v = IntMap.lookup objno offsetmap in (objno,Right (v,gener))
        x (objno,(FreeEntry gener)) = (objno,Left (0,gener))
        group [] = []
        group (xs@((objno,_):_)) = let (a,b) = break br (zip xs [objno ..]) in (objno,map (snd . fst) a) : group (map fst b)
        br ((objno,entry),index) = index/=objno



binarizeXRefTable table = concatMap z table
    where
        z (off,entries) = show off ++ " " ++ show (length entries) ++ " \n" ++ concatMap binarizeXRefTableEntry entries
        binarizeXRefTableEntry (Right (a,b)) = entry a b 'n'
        binarizeXRefTableEntry (Left (a,b)) = entry a b 'f'
        entry a b f = last 10 ("0000000000" ++ show a) ++ " " ++ last 5 ("00000" ++ show b) ++ " " ++ [f] ++" \n"
        last n x = drop (length x - n) x


writeBody   :: (BS.ByteString -> IO ())     -- write action
            -> (IO Int)                     -- tell offset action
            -> (Int,Int)
            -> Body                         -- (objno,entry)
            -> IO (Int,Int)
writeBody write tell (highest,prevoffset) (Body trailer objects) = do
    (_,r) <- foldM (writeObject write tell) (0,IntMap.empty) (IntMap.toList objects)
    v <- tell
    write $ BSC.pack "xref\n"

    let x = createXRefTable r (IntMap.toList objects)
    write $ BSC.pack (binarizeXRefTable x)
    let Just ((k1,_),_) =  IntMap.maxViewWithKey objects
        k = (k1+1) `max` highest
    write $ BSC.pack "trailer\n"
    let trailernosizeprev = filter (\(x,_) -> x/=BSC.pack "Size" && x/=BSC.pack "Prev") trailer
    let trailersize = entryn "Size" k : trailernosizeprev
    let trailerfull = if prevoffset/=0
                           then entryn "Prev" prevoffset : trailersize
                           else trailersize
    mapM_ write (BSL.toChunks (Bin.toLazyByteString (binarize (Dict trailerfull))))
    write $ BSC.pack ("\nstartxref\n" ++ show v ++ "\n%%EOF\n")
    return (k,v)


writeDocument write tell (Document version bodies) = do
    write $ BSC.concat [BSC.pack "%", version, BSC.pack "\n"]
    foldM_ (writeBody write tell) (0,0) (reverse bodies)



writeFileX file document = do
    bracket (openBinaryFile file WriteMode)
            (hClose)
            (\handle -> writeDocument (BS.hPutStr handle) (liftM fromIntegral $ hTell handle) document)

importObjects :: Document -> [RefID] -> State Document [RefID]
importObjects doc refids = do
    rec
      let
        solve :: IntMap.IntMap Int -> [Int] -> State Document (IntMap.IntMap Int)
        solve visited [] = return visited
        solve visited (x:xs)
            | IntMap.member x visited = solve visited xs
            | otherwise = do
                let Just indir = PdfModel.lookup (RefID x) doc
                let (Indir val strm) = indir
                let (newvalue,newrefids) = both val
                let indir2 = if null newrefids
                             then indir
                             else Indir newvalue strm
                newrefid <- addIndir indir2
                let visited2 = IntMap.insert x (unRefID newrefid) visited
                solve visited2 (newrefids ++ xs)

        both v1@(Dict a) = let
                l = map (\(k,x) -> let (v,r) = both x in ((k,v),r)) a
                r = concatMap snd l
                v = if null r then v1 else Dict (map fst l)
                in (v,r)
        both v1@(Array a) = let
                l = map both a
                r = concatMap snd l
                v = if null r then v1 else Array (map fst l)
                in (v,r)
        both (Ref ref) = (Ref (RefID $ fromJust $ IntMap.lookup (unRefID ref) remap),[unRefID ref])
        {-
        both (Ref ref) = let
            -- this doesn't work, because it does not substitute references for value
            -- if there are no other references
            whenref = (Ref (RefID $ fromJust $ IntMap.lookup (unRefID ref) remap),[unRefID ref])
            whensimple = (value,[])
            Just indir = PdfModel.lookup ref doc
            (Indir value stream) = indir
            issimple (Dict _) = False
            issimple (Array _) = False
            issimple _ = True
            result = if issimple value then whensimple else whenref
            in result

        -}
        both x = (x,[])
        unRefID (RefID x) = x

      remap <- solve IntMap.empty (map unRefID refids)
    return (map (\k -> RefID $ fromJust $ IntMap.lookup k remap) (map unRefID refids))



ext (Dict d) r = Dict (r ++ d)

trailer_dict catalog = [ entry "Root" catalog ]

catalog_dict pages = dict [ entry "Type" "Catalog"
                                        , entry "Pages" pages
                                        ]

page_tree_item typex parent = dict [ entry "Type" typex, entry "Parent" parent ]

pages_dict parent count kids = page_tree_item "Pages" parent `ext` [ entryn "Count" count
                                                  , entry "Kids" (array kids)]
page_dict parent contents = page_tree_item "Page" parent `ext` [ entry "Contents" contents ]

standard_font_dict basefont = dict [ entry "Type" "Font"
                                            , entry "Subtype" "Type1"
                                            , entry "BaseFont" basefont
                                            ]
function_dict typex domain = dict [ entryn "FunctionType" typex, entryna "Domain" domain ]

function_type4_dict domain range = function_dict 4 domain `ext` [ entryna "Range" range
                                              ]
function_type2_dict domain c0 c1 n = function_dict 2 domain `ext` [ entryna "C0" c0
                                                                  , entryna "C1" c1
                                                                  , entryn "N" n
                                                                  ]

shading_dict typex colorspace = dict [ entryn "ShadingType" typex
                              , entry  "ColorSpace" colorspace
                              ]
shading_type1_dict colorspace domain function = shading_dict 1 colorspace `ext` [
                               entry "Domain" domain
                              , entry "Function" function
                              ]

shading_type2_dict colorspace bbox coords function = shading_dict 2 colorspace `ext` [ entryna "BBox" bbox
                              , entryna "Coords" coords
                              , entry "Function" function
                              ]

shading_type3_dict colorspace bbox coords function = shading_dict 3 colorspace `ext` [
                               entryna "BBox" bbox
                              , entryna "Coords" coords
                              , entry "Function" function
                              ]



-- content must be scaled to fit 100x142pt
place :: Double -> Double -> Int -> Int -> Int -> Int -> P.String -> P.String
place width height xcount ycount xpos ypos content =
    "q " ++ mshow xscale ++ " 0 0 " ++ mshow yscale ++ " " ++ mshow xmove ++ " " ++ mshow ymove ++ " cm\n" ++
    "0 0 100 142 re S\n" ++
    content ++
    "\nQ\n"
    where
        xscale = width/100/fromIntegral xcount
        yscale = height/142/fromIntegral ycount
        xmove = fromIntegral xpos*width/fromIntegral xcount
        ymove = fromIntegral ypos*height/fromIntegral ycount
        mshow x = showFFloat Nothing x ""


multipage parent width height xcount ycount contents = 
    mapM multipagehelper (groupN (xcount*ycount) contents)
    where
        multipagehelper contents = do
            rec page <- addObject (page_dict parent pagecontents)
                pagecontents <- addStream (empty_dict) (doconcat contents)
            return page
        doconcat contents = BSL.fromChunks (zipWith ($) [cnts x y | y <- [ycount-1,ycount-2..0], x <- [0..xcount-1]] contents)
        cnts x y text = BSC.pack $ place width height xcount ycount x y text
        groupN n [] = []
        groupN n list = let (a,b) = splitAt n list in a : groupN n b

indexed_array base count indexdata = array [ value "Indexed", value base, valuen count, value indexdata ]
separation_array name base func = array [ value "Separation", value name, value base, value func ]
devicen_array colorants base func = array [ value "DeviceN", array colorants, value base, value func ]




-- ----------------------------------------------------------------------------------------------------
-- Parser part

{-
 - 1. find header (to set origin offset)
 - 2. find last startxref
 - 3. parse bodys recursively
 -}

-- | Parse document from binary data
parse :: BS.ByteString -> Maybe Document
parse bin = do
    lstart <- find (BS.isPrefixOf (BSC.pack "startxref"))
                          (reverse (BS.tails l300))
    [(istart,_)] <- return (reads (drop 9 (BSC.unpack lstart)))
    {- bodys <- parseBodyList bin istart -}
    bodies <- parseBodyList bin istart
    return (Document (BSC.pack "PDF-1.4") bodies)
    where l = BS.length bin
          l300 = BS.drop (l-300) bin

-- parseBodyList :: IntSet.IntSet -> BinaryData -> Int -> Maybe [(Body,IntSet.IntSet)]
parseBodyList bin start = do
    both@(body) <- parseBody bin start
    let Body trailer _ = body
    rest <- case Prelude.lookup (BSC.pack "Prev") trailer of
                Just (Number offset) -> parseBodyList bin (round offset)
                _ -> return []
    return (body : rest)

-- | Parse document from file
parseFile :: FilePath -> IO (Maybe Document)
parseFile file = do
    -- cont <- BS.readFile file
    cont <- mmapFileByteString file Nothing
    return (PdfModel.parse cont)

-- accumulator version
sequence2       :: Monad m => [m a] -> m [a]
sequence2 ms = sequence' [] ms
    where
        sequence' vs [] = return (reverse vs)
        sequence' vs (m:ms) = m >>= (\v -> sequence' (v:vs) ms)

mapM2 f xs = sequence2 (map f xs)

-- the body should be the parsed body, kind of fixed point operator
xparse body bin = do
    parseOperatorEq "xref"
    p <- P.manyRev parseXRefSection
    parseOperatorEq "trailer"
    e <- parseDict
    let entries = IntMap.fromList (map (\(a,b,c) -> (a,c)) (concat p))
    return (Body e entries)
    where
        parseXRefSection = {-# SCC "xparse.parseXRefSection" #-} do
            objno <- parseNatural
            ncount <- parseNatural
            --P.count ncount (parseXRefEntry uniq nid)
            mapM2 parseXRefEntry [objno..objno+ncount-1]
        parseXRefEntry objno = {-# SCC "xparse.slot" #-} do
            offset <- parseNatural
            gener <- parseNatural
            nf <- parseRegular
            --mtrace "slot parsed"
            if nf==BSC.pack "f"
               then return (objno,0,FreeEntry gener)
               else if nf==BSC.pack "n"
                    then let
                             indir = parseIndir bin offset
                             usedEntry = UsedEntry gener indir
                         in return (objno,offset,usedEntry)
                    else P.pfail

        findLength :: Body -> Value -> Int
        findLength body@(Body trailer objects) (Dict d) = {-# SCC "xparse.findLength" #-}
            case Prelude.lookup (BSC.pack "Length") d of
                Just (Number v) -> (round v)
                Just (Ref v) -> case IntMap.lookup (fromIntegral $ objno v) objects of
                                    Just entry -> case entry of
                                        UsedEntry _ indir -> case indir of
                                            Indir value _ -> case value of
                                                Number v -> round v
                                                x -> error ("/Length not found case 5: " ++ show x)
                                            x -> error ("/Length not found case 6: " ++ show x)
                                        x -> error ("/Length not found case 4: " ++ show x)
                                    x -> error ("/Length not found case 1: " ++ show x)
                _ ->  error "/Length not found case 2"
        findLength _ _ =  error "/Length not found case 3"


        parseIndir :: BS.ByteString -> Int -> Indir
        parseIndir = {-# SCC "xparse.parseIndir" #-} parseIndirA

        {-
        parseIndirB :: a -> BS.ByteString -> Int -> Indir
        parseIndirB uniq bin off = let dat = BS.drop off bin in
            case execute dat of
                [obj] -> Indir obj Nothing
                [Number ntokens,obj] ->
                    let len = findLength body obj
                        dt = cut (off + round ntokens) len bin
                    in (Indir obj (Just (dt `seq` toWrite $ dt)))
                other -> error (show other ++ BSC.unpack (BS.take 100 dat))
        -}
        parseIndirA :: BS.ByteString -> Int -> Indir
        parseIndirA bin off =
            --case P.readP_to_S parseIndir1 (BS.unpack (BS.drop (fromIntegral off) bin)) of
            case head $ P.readP_to_S parseIndir1 (BS.drop (fromIntegral off) bin) of
                ((ntokens,Indir e Nothing),_) -> Indir e Nothing
                ((ntokens,Indir e (Just _)),_) ->
                    let len = findLength body e
                        dt = cut (off + ntokens) len bin
                        indir = Indir e (Just (BSL.fromChunks [dt]))
                    in indir
                _ -> error "did not parse correctly"
        cut off len = BS.take (fromIntegral len) . BS.drop (fromIntegral off)

        parseIndir1 :: MyReadP (Int,Indir)
        parseIndir1 = P.countsym $ do
            parseNatural -- should check if naturals match with reference
            parseNatural
            parseOperatorEq "obj"
            e <- parseValue
            (parseOperatorEq "endobj" >> return (Indir e Nothing)) `mplus` withStream e
        withStream e = do
            parseOperatorEq "stream"
            P.munch isspacenoteol
            (P.string (BS.pack [0x0d,0x0a]) >> return ())
                P.<++ (P.char 0x0d >> return ())
                P.<++ (P.char 0x0a >> return ())
                P.<++ (return ())
            return (Indir e (Just undefined))




-- | Parse body from bin starting xref at offset
parseBody bin startxref = case aparse of
    [(body,_)] -> Just (body)
    _ -> Nothing
    where
        aparse = P.readP_to_S (xparse body bin) xref
        [(body,_)] = aparse
        xref = BS.drop (fromIntegral startxref) bin


-- | Parses PDF element: dictionary, array, number, string, hexstring, operators.
parseValue :: MyReadP Value
parseValue = P.choice [ (P.<++) (parseRef >>= return . Ref) (parseRegular >>= return . mkop)
               , parseDict >>= return . Dict
               , parseArray >>= return . Array
               , parseString >>= return . String False
               , parseHexString >>= return . String True
               , parseName >>= return . Name
               -- , parse_execarray >>= return . ExecArray
               ]

-- | Parses nonnegative integer number given in decimal
parseNatural :: MyReadP Int
parseNatural = do
    s <- parseRegular
    r 0 (BS.unpack s)
    where r !n (x:xs) | x>=n0 && x<=n9 = r (n*10 + fromIntegral (x-n0)) xs
                     | otherwise = P.pfail
          r !n [] = return n
          n0 = 48
          n9 = 48 + 9

-- | Parse sequence of regular characters. This may constitute operator or number.
parseRegular = do
    parseSpace
    v <- P.munch1 isregular
    return v

parseNumber :: MyReadP Double
parseNumber = beforedot 0
    where
        c0 = BSB.c2w '0'
        c9 = BSB.c2w '9'
        dot = BSB.c2w '.'
        beforedot :: Integer -> MyReadP Double
        beforedot value = (do
                c <- P.satisfy (\c -> c >= c0 && c <= c9)
                beforedot (value*10 + fromIntegral (c - c0)))
            P.+++ (do
                P.char dot
                afterdot value 1)
            P.+++ (do
                v <- P.look
                if BSC.null v
                    then return (fromIntegral value)
                    else P.pfail)
        afterdot :: Integer -> Integer -> MyReadP Double
        afterdot value divider = (do
                c <- P.satisfy (\c -> c >= c0 && c <= c9)
                afterdot (value*10 + fromIntegral (c - c0)) (divider*10))
            P.+++ (do
                v <- P.look
                if BSC.null v
                    then return (fromIntegral value / fromIntegral divider)
                    else P.pfail)


mkop name
    | name==BSC.pack "true" = Boolean True
    | name==BSC.pack "false" = Boolean False
    | name==BSC.pack "null" = Null
    | [(flt,_)] <- P.readP_to_S parseNumber name = Number flt
    | otherwise = Operator name

-- | Parse operator given as 'name'.
parseOperatorEq :: [Char] -> MyReadP BS.ByteString
parseOperatorEq name = parseRegular >>= \x ->
     if x==BSC.pack name
         then return x
         else P.pfail

-- | Parse reference ID of the form n g R
parseRef :: MyReadP RefID
parseRef = do
    refno <- parseNatural
    gener <- parseNatural
    parseOperatorEq "R"
    return (refid (fromIntegral refno) (fromIntegral gener))


-- | Parse string delimited by ( ) or < > (hexstring).
parseString :: MyReadP BS.ByteString
parseString = do
    parseSpace
    P.char (BSB.c2w '(')
    x <- string_p1
    P.char (BSB.c2w ')')
    return (BS.concat x)
    where
        string_p1 = fmap reverse $ P.manyRev (P.choice [string_char_p, string_bs_p,
                                      string_string_p])
        string_char_p = P.munch1 (\x -> x/=BSB.c2w '(' &&
                                   x/= BSB.c2w '\\' &&
                                   x/=BSB.c2w ')')
        string_bs_p = P.choice [ P.string (o "\\f") >> return (o "\f")
                               , P.string (o "\\r") >> return (o "\r")
                               , P.string (o "\\n") >> return (o "\n")
                               , P.string (o "\\t") >> return (o "\t")
                               , P.char (BSB.c2w '\\') >>
                                    P.get >>= return . BS.singleton
                               ]
        string_string_p = do
             P.string (o "(")
             x <- string_p1
             P.string (o ")")
             return (BS.concat ([o "("] ++ x ++ [o ")"]))
        o = BSC.pack

parseHexString :: MyReadP BS.ByteString
parseHexString = do
    parseSpace
    P.char (c '<')
    x <- P.munch (\y -> ishexchar y || isspace y)
    P.char (c '>')
    return (BS.pack (hexdecode (filter ishexchar (BS.unpack x))))
    where
        hexdecode [] = []
        hexdecode [x] = [hexvalue x `shiftL` 4]
        hexdecode (a:b:xs) = ((hexvalue a `shiftL` 4) + hexvalue b)
                              : hexdecode xs
        hexvalue x | x>=c '0' && x<=c '9' = fromIntegral (x - c '0')
        hexvalue x | x>=c 'A' && x<=c 'F' = fromIntegral (10 + x - c 'A')
        hexvalue x | x>=c 'a' && x<=c 'f' = fromIntegral (10 + x - c 'a')
        c = BSB.c2w

-- | Parse array [ ]
parseArray :: MyReadP ArrayData
parseArray = do
    parseSpace
    P.char (BSB.c2w '[')
    e <- P.manyRev parseValue
    parseSpace
    P.char (BSB.c2w ']')
    return (reverse e)

-- | Parse dictionary << >>
parseDict :: MyReadP DictData
parseDict = do
    parseSpace
    P.string (BSC.pack "<<")
    e <- P.manyRev pair
    parseSpace
    P.string (BSC.pack ">>")
    return (e)
    where
        pair = liftM2 (,) parseName parseValue

-- | Skips whitespace characters
parseSpace :: MyReadP ()
{-
parseSpace = ((P.satisfy isspace >> parseSpace) P.+++
             (P.char (BSB.c2w '%') >> skipTillEOL >> parseSpace)) P.+++ return ()
    where skipTillEOL = (P.satisfy (not . iseol) >> skipTillEOL) P.+++ return ()
-}


parseSpace = P.munch isspace >>
                 ((P.char (BSB.c2w '%') >> P.munch (not . iseol) >> parseSpace) P.<++ return ())



-- | Parse PDF name. Name is in undecoded form, retaining all # characters.
parseName :: MyReadP BS.ByteString
parseName = do
    parseSpace
    P.char (BSB.c2w '/')
    s <- P.munch isregular
    return s


isdelim :: Word8 -> Bool
isdelim x =
    isdelimc (BSB.w2c x)
    where
        isdelimc '/' = True
        isdelimc '<' = True
        isdelimc '>' = True
        isdelimc '[' = True
        isdelimc ']' = True
        isdelimc '%' = True
        isdelimc '(' = True
        isdelimc ')' = True
        isdelimc '{' = True
        isdelimc '}' = True
        isdelimc _ = False



iseol :: Word8 -> Bool
iseol 0x0A = True
iseol 0x0D = True

isregular :: Word8 -> Bool
isregular x = not (isdelim x || isspace x)

isdigit :: Word8 -> Bool
isdigit x = (x>=BSB.c2w '0' && x<=BSB.c2w '9')

ishexchar :: Word8 -> Bool
ishexchar x = (x>=BSB.c2w '0' && x<=BSB.c2w '9') ||
              (x>=BSB.c2w 'A' && x<=BSB.c2w 'F') ||
              (x>=BSB.c2w 'a' && x<=BSB.c2w 'f')

isspace :: Word8 -> Bool
isspace 0x20 = True
isspace 0x0A = True
isspace 0x0D = True
isspace 0x09 = True
isspace 0x0C = True
isspace 0x00 = True
isspace _ = False

isspacenoteol :: Word8 -> Bool
isspacenoteol 0x20 = True
isspacenoteol 0x09 = True
isspacenoteol 0x0C = True
isspacenoteol 0x00 = True
isspacenoteol _ = False

