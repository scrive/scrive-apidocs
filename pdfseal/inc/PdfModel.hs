{-# LANGUAGE DoRec, PatternGuards, CPP, NoOverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module PdfModel where
import Prelude hiding (String)
import qualified Prelude as P
import Control.Monad.State.Strict
-- import "mtl" Control.Monad.ST
import System.IO
import Control.Exception
import Numeric
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as  BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Internal as BSB
import qualified Data.IntMap as IntMap
import Data.Maybe
import Matrix
import qualified ReadP as P
import qualified Data.Char as Char

import System.IO.MMap
import qualified Data.Binary.Builder as Bin
import Data.Monoid
import Control.Monad.Writer.Class
import qualified Control.Monad.Writer.Strict as Strict
import qualified Control.Monad.State.Strict as Strict
import Data.List (findIndex)

#ifdef _DEBUG
import Debug.Trace
#else
trace :: P.String -> a -> a
trace _ x = x
#endif

traceM :: (Monad m) => P.String -> m ()
traceM msg = trace msg (return ())


pdfStandardEncoding :: [(Char,Char)]
pdfStandardEncoding = map (\(a,b) -> (Char.chr a, Char.chr b)) [
    (0x0020,0x20), --       # SPACE # space
    (0x00A0,0x20), --       # NO-BREAK SPACE        # space
    (0x0021,0x21), --       # EXCLAMATION MARK      # exclam
    (0x0022,0x22), --       # QUOTATION MARK        # quotedbl
    (0x0023,0x23), --       # NUMBER SIGN   # numbersign
    (0x0024,0x24), --       # DOLLAR SIGN   # dollar
    (0x0025,0x25), --       # PERCENT SIGN  # percent
    (0x0026,0x26), --       # AMPERSAND     # ampersand
    (0x2019,0x27), --       # RIGHT SINGLE QUOTATION MARK   # quoteright
    (0x0028,0x28), --       # LEFT PARENTHESIS      # parenleft
    (0x0029,0x29), --       # RIGHT PARENTHESIS     # parenright
    (0x002A,0x2A), --       # ASTERISK      # asterisk
    (0x002B,0x2B), --       # PLUS SIGN     # plus
    (0x002C,0x2C), --       # COMMA # comma
    (0x002D,0x2D), --       # HYPHEN-MINUS  # hyphen
    (0x00AD,0x2D), --       # SOFT HYPHEN   # hyphen
    (0x002E,0x2E), --       # FULL STOP     # period
    (0x002F,0x2F), --       # SOLIDUS       # slash
    (0x0030,0x30), --       # DIGIT ZERO    # zero
    (0x0031,0x31), --       # DIGIT ONE     # one
    (0x0032,0x32), --       # DIGIT TWO     # two
    (0x0033,0x33), --       # DIGIT THREE   # three
    (0x0034,0x34), --       # DIGIT FOUR    # four
    (0x0035,0x35), --       # DIGIT FIVE    # five
    (0x0036,0x36), --       # DIGIT SIX     # six
    (0x0037,0x37), --       # DIGIT SEVEN   # seven
    (0x0038,0x38), --       # DIGIT EIGHT   # eight
    (0x0039,0x39), --       # DIGIT NINE    # nine
    (0x003A,0x3A), --       # COLON # colon
    (0x003B,0x3B), --       # SEMICOLON     # semicolon
    (0x003C,0x3C), --       # LESS-THAN SIGN        # less
    (0x003D,0x3D), --       # EQUALS SIGN   # equal
    (0x003E,0x3E), --       # GREATER-THAN SIGN     # greater
    (0x003F,0x3F), --       # QUESTION MARK # question
    (0x0040,0x40), --       # COMMERCIAL AT # at
    (0x0041,0x41), --       # LATIN CAPITAL LETTER A        # A
    (0x0042,0x42), --       # LATIN CAPITAL LETTER B        # B
    (0x0043,0x43), --       # LATIN CAPITAL LETTER C        # C
    (0x0044,0x44), --       # LATIN CAPITAL LETTER D        # D
    (0x0045,0x45), --       # LATIN CAPITAL LETTER E        # E
    (0x0046,0x46), --       # LATIN CAPITAL LETTER F        # F
    (0x0047,0x47), --       # LATIN CAPITAL LETTER G        # G
    (0x0048,0x48), --       # LATIN CAPITAL LETTER H        # H
    (0x0049,0x49), --       # LATIN CAPITAL LETTER I        # I
    (0x004A,0x4A), --       # LATIN CAPITAL LETTER J        # J
    (0x004B,0x4B), --       # LATIN CAPITAL LETTER K        # K
    (0x004C,0x4C), --       # LATIN CAPITAL LETTER L        # L
    (0x004D,0x4D), --       # LATIN CAPITAL LETTER M        # M
    (0x004E,0x4E), --       # LATIN CAPITAL LETTER N        # N
    (0x004F,0x4F), --       # LATIN CAPITAL LETTER O        # O
    (0x0050,0x50), --       # LATIN CAPITAL LETTER P        # P
    (0x0051,0x51), --       # LATIN CAPITAL LETTER Q        # Q
    (0x0052,0x52), --       # LATIN CAPITAL LETTER R        # R
    (0x0053,0x53), --       # LATIN CAPITAL LETTER S        # S
    (0x0054,0x54), --       # LATIN CAPITAL LETTER T        # T
    (0x0055,0x55), --       # LATIN CAPITAL LETTER U        # U
    (0x0056,0x56), --       # LATIN CAPITAL LETTER V        # V
    (0x0057,0x57), --       # LATIN CAPITAL LETTER W        # W
    (0x0058,0x58), --       # LATIN CAPITAL LETTER X        # X
    (0x0059,0x59), --       # LATIN CAPITAL LETTER Y        # Y
    (0x005A,0x5A), --       # LATIN CAPITAL LETTER Z        # Z
    (0x005B,0x5B), --       # LEFT SQUARE BRACKET   # bracketleft
    (0x005C,0x5C), --       # REVERSE SOLIDUS       # backslash
    (0x005D,0x5D), --       # RIGHT SQUARE BRACKET  # bracketright
    (0x005E,0x5E), --       # CIRCUMFLEX ACCENT     # asciicircum
    (0x005F,0x5F), --       # LOW LINE      # underscore
    (0x2018,0x60), --       # LEFT SINGLE QUOTATION MARK    # quoteleft
    (0x0061,0x61), --       # LATIN SMALL LETTER A  # a
    (0x0062,0x62), --       # LATIN SMALL LETTER B  # b
    (0x0063,0x63), --       # LATIN SMALL LETTER C  # c
    (0x0064,0x64), --       # LATIN SMALL LETTER D  # d
    (0x0065,0x65), --       # LATIN SMALL LETTER E  # e
    (0x0066,0x66), --       # LATIN SMALL LETTER F  # f
    (0x0067,0x67), --       # LATIN SMALL LETTER G  # g
    (0x0068,0x68), --       # LATIN SMALL LETTER H  # h
    (0x0069,0x69), --       # LATIN SMALL LETTER I  # i
    (0x006A,0x6A), --       # LATIN SMALL LETTER J  # j
    (0x006B,0x6B), --       # LATIN SMALL LETTER K  # k
    (0x006C,0x6C), --       # LATIN SMALL LETTER L  # l
    (0x006D,0x6D), --       # LATIN SMALL LETTER M  # m
    (0x006E,0x6E), --       # LATIN SMALL LETTER N  # n
    (0x006F,0x6F), --       # LATIN SMALL LETTER O  # o
    (0x0070,0x70), --       # LATIN SMALL LETTER P  # p
    (0x0071,0x71), --       # LATIN SMALL LETTER Q  # q
    (0x0072,0x72), --       # LATIN SMALL LETTER R  # r
    (0x0073,0x73), --       # LATIN SMALL LETTER S  # s
    (0x0074,0x74), --       # LATIN SMALL LETTER T  # t
    (0x0075,0x75), --       # LATIN SMALL LETTER U  # u
    (0x0076,0x76), --       # LATIN SMALL LETTER V  # v
    (0x0077,0x77), --       # LATIN SMALL LETTER W  # w
    (0x0078,0x78), --       # LATIN SMALL LETTER X  # x
    (0x0079,0x79), --       # LATIN SMALL LETTER Y  # y
    (0x007A,0x7A), --       # LATIN SMALL LETTER Z  # z
    (0x007B,0x7B), --       # LEFT CURLY BRACKET    # braceleft
    (0x007C,0x7C), --       # VERTICAL LINE # bar
    (0x007D,0x7D), --       # RIGHT CURLY BRACKET   # braceright
    (0x007E,0x7E), --       # TILDE # asciitilde
    (0x00A1,0xA1), --       # INVERTED EXCLAMATION MARK     # exclamdown
    (0x00A2,0xA2), --       # CENT SIGN     # cent
    (0x00A3,0xA3), --       # POUND SIGN    # sterling
    (0x2044,0xA4), --       # FRACTION SLASH        # fraction
    (0x2215,0xA4), --       # DIVISION SLASH        # fraction
    (0x00A5,0xA5), --       # YEN SIGN      # yen
    (0x0192,0xA6), --       # LATIN SMALL LETTER F WITH HOOK        # florin
    (0x00A7,0xA7), --       # SECTION SIGN  # section
    (0x00A4,0xA8), --       # CURRENCY SIGN # currency
    (0x0027,0xA9), --       # APOSTROPHE    # quotesingle
    (0x201C,0xAA), --       # LEFT DOUBLE QUOTATION MARK    # quotedblleft
    (0x00AB,0xAB), --       # LEFT-POINTING DOUBLE ANGLE QUOTATION MARK     # guillemotleft
    (0x2039,0xAC), --       # SINGLE LEFT-POINTING ANGLE QUOTATION MARK     # guilsinglleft
    (0x203A,0xAD), --       # SINGLE RIGHT-POINTING ANGLE QUOTATION MARK    # guilsinglright
    (0xFB01,0xAE), --       # LATIN SMALL LIGATURE FI       # fi
    (0xFB02,0xAF), --       # LATIN SMALL LIGATURE FL       # fl
    (0x2013,0xB1), --       # EN DASH       # endash
    (0x2020,0xB2), --       # DAGGER        # dagger
    (0x2021,0xB3), --       # DOUBLE DAGGER # daggerdbl
    (0x00B7,0xB4), --       # MIDDLE DOT    # periodcentered
    (0x2219,0xB4), --       # BULLET OPERATOR       # periodcentered
    (0x00B6,0xB6), --       # PILCROW SIGN  # paragraph
    (0x2022,0xB7), --       # BULLET        # bullet
    (0x201A,0xB8), --       # SINGLE LOW-9 QUOTATION MARK   # quotesinglbase
    (0x201E,0xB9), --       # DOUBLE LOW-9 QUOTATION MARK   # quotedblbase
    (0x201D,0xBA), --       # RIGHT DOUBLE QUOTATION MARK   # quotedblright
    (0x00BB,0xBB), --       # RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK    # guillemotright
    (0x2026,0xBC), --       # HORIZONTAL ELLIPSIS   # ellipsis
    (0x2030,0xBD), --       # PER MILLE SIGN        # perthousand
    (0x00BF,0xBF), --       # INVERTED QUESTION MARK        # questiondown
    (0x0060,0xC1), --       # GRAVE ACCENT  # grave
    (0x00B4,0xC2), --       # ACUTE ACCENT  # acute
    (0x02C6,0xC3), --       # MODIFIER LETTER CIRCUMFLEX ACCENT     # circumflex
    (0x02DC,0xC4), --       # SMALL TILDE   # tilde
    (0x00AF,0xC5), --       # MACRON        # macron
    (0x02C9,0xC5), --       # MODIFIER LETTER MACRON        # macron
    (0x02D8,0xC6), --       # BREVE # breve
    (0x02D9,0xC7), --       # DOT ABOVE     # dotaccent
    (0x00A8,0xC8), --       # DIAERESIS     # dieresis
    (0x02DA,0xCA), --       # RING ABOVE    # ring
    (0x00B8,0xCB), --       # CEDILLA       # cedilla
    (0x02DD,0xCD), --       # DOUBLE ACUTE ACCENT   # hungarumlaut
    (0x02DB,0xCE), --       # OGONEK        # ogonek
    (0x02C7,0xCF), --       # CARON # caron
    (0x2014,0xD0), --       # EM DASH       # emdash
    (0x00C6,0xE1), --       # LATIN CAPITAL LETTER AE       # AE
    (0x00AA,0xE3), --       # FEMININE ORDINAL INDICATOR    # ordfeminine
    (0x0141,0xE8), --       # LATIN CAPITAL LETTER L WITH STROKE    # Lslash
    (0x00D8,0xE9), --       # LATIN CAPITAL LETTER O WITH STROKE    # Oslash
    (0x0152,0xEA), --       # LATIN CAPITAL LIGATURE OE     # OE
    (0x00BA,0xEB), --       # MASCULINE ORDINAL INDICATOR   # ordmasculine
    (0x00E6,0xF1), --       # LATIN SMALL LETTER AE # ae
    (0x0131,0xF5), --       # LATIN SMALL LETTER DOTLESS I  # dotlessi
    (0x0142,0xF8), --       # LATIN SMALL LETTER L WITH STROKE      # lslash
    (0x00F8,0xF9), --       # LATIN SMALL LETTER O WITH STROKE      # oslash
    (0x0153,0xFA), --       # LATIN SMALL LIGATURE OE       # oe
    (0x00DF,0xFB), --       # LATIN SMALL LETTER SHARP S    # germandbls
    (0,0)]

-- this is cheating
-- FIXME: font encoding
winAnsiChars :: P.String
winAnsiChars = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ € ‚ƒ„…†‡ˆ‰Š‹Œ Ž  ‘’“”•–—˜™š›œ žŸ ¡¢£¤¥¦§¨©ª«¬?®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"

unicodeToWinAnsi :: Char -> Char
unicodeToWinAnsi x | x <= ' ' = ' '
                   | otherwise =
  case findIndex (==x) winAnsiChars of
      Just i -> Char.chr (i + 33)
      Nothing -> '?'

unicodeToStandardEncoding :: Char -> Char
unicodeToStandardEncoding x | x <= ' ' = ' '
                            | otherwise =
  case P.lookup x pdfStandardEncoding of
    Just c -> c
    Nothing -> '?'

type MyReadP a = P.ReadP a

data Document = Document
                   { documentHeader :: BS.ByteString                  -- header
                   , documentBodies :: [Body]                         -- bodies, from last to first
                   }
  deriving Show

data Body = Body
                   { bodyTrailer :: DictData
                   , bodyObjects :: IntMap.IntMap Entry   -- trailer and object map
                   }
  deriving Show

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

array :: IsValue a => [a] -> Value
array x = Array . map value $ x

unpsname :: Value -> BS.ByteString
unpsname (Name x ) = x
unpsname _ = error "Value is not a Name in unpsname"

class IsString a where
    string :: a -> Value

instance IsString BS.ByteString where
    string x = String False x

-- This instance is funny. If we see a char with value >255 we need to encode whole string as
-- double byte with a marker.
instance IsString P.String where
    string x = String False bs
      where bs = BSC.pack x

hexstring :: IsString a => a -> Value
hexstring x = String True v
  where
    String _ v = string x

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
        ceiling ((log (fromInteger (f+1) :: Double) +
                 fromIntegral (e) * log (fromInteger b :: Double)) /
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



ss :: [Int] -> Int -> [Char]
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

showFFloat2 :: Double -> [Char]
showFFloat2 x = result
    where (digits,exp') = floatToDigits2 x1
          (x1,result) = if x < 0
                           then (-x,'-' : ss digits exp')
                           else (x, ss digits exp')


instance Binarize Value where
    --binarize value = Strict.evalState (Strict.execWriterT (binarizeValue value)) False -- 62s
    -- binarize value = Lazy.execWriter (Strict.evalStateT (binarizeValue value) False) -- 63s
    binarize value' = Strict.execWriter (Strict.evalStateT (binarizeValue value') False) -- 64s

maybeSpace :: (MonadState Bool m, MonadWriter Bin.Builder m) => m ()
maybeSpace = do
    s <- get
    if s
       then tell (bin_builder_char ' ')
       else return ()

--binarizeValue :: Value -> Control.Monad.Writer.Lazy.Writer Bin.Buffer
binarizeValue :: (MonadWriter Bin.Builder m, MonadState Bool m) => Value -> m ()
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
    tell $ bin_builder_bytestring_escape x
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
binarizeValue (Comment x) = do
    tell $ bin_builder_char '%'
    tell $ bin_builder_bytestring x
    tell $ bin_builder_char '\n'
    put False


bin_builder_word8 :: Word8 -> Bin.Builder
bin_builder_word8 x = Bin.singleton x

bin_builder_char :: Char -> Bin.Builder
bin_builder_char x = bin_builder_word8 (BSB.c2w x)

bin_builder_word8_escape :: Word8 -> Bin.Builder
bin_builder_word8_escape x | x<32 || x >=127 || x == BSB.c2w '\\' || isdelim x
                             = mconcat [ Bin.singleton (BSB.c2w '\\')
                                       , Bin.singleton (((x `div` 64) `mod` 8)  + 48)
                                       , Bin.singleton (((x `div` 8) `mod` 8) + 48)
                                       , Bin.singleton ((x `mod` 8) + 48)
                                       ]
                    | otherwise = Bin.singleton x

bin_builder_word8_name_escape :: Word8 -> Bin.Builder
bin_builder_word8_name_escape x | x<32 || x >=127 || x == BSB.c2w '#' || not (isregular x)
                             = mconcat [ Bin.singleton (BSB.c2w '#')
                                       , Bin.fromByteString $ hexencode $ BS.singleton x
                                       ]
                    | otherwise = Bin.singleton x

bin_builder_char_escape :: Char -> Bin.Builder
bin_builder_char_escape x = bin_builder_word8_escape (BSB.c2w x)

bin_builder_string :: P.String -> Bin.Builder
bin_builder_string x =
    mconcat (map bin_builder_char x)

bin_builder_bytestring :: BS.ByteString -> Bin.Builder
bin_builder_bytestring x =
    mconcat (map bin_builder_word8 (BS.unpack x))

bin_builder_string_escape :: P.String -> Bin.Builder
bin_builder_string_escape x =
    mconcat (map bin_builder_char_escape x)

bin_builder_bytestring_escape :: BS.ByteString -> Bin.Builder
bin_builder_bytestring_escape x =
    mconcat (map bin_builder_word8_escape (BS.unpack x))

bin_builder_bytestring_name_escape :: BS.ByteString -> Bin.Builder
bin_builder_bytestring_name_escape x =
    mconcat (map bin_builder_word8_name_escape (BS.unpack x))

-- | Turn unencoded string into Name
eName :: BS.ByteString -> Value
eName = Name . mconcat . BSL.toChunks
      . Bin.toLazyByteString . bin_builder_bytestring_name_escape

-- | Turn Name into unencoded string
dName :: Value -> Maybe BS.ByteString
dName (Name s) = BSC.pack `fmap` decode (BSC.unpack s) where
  decode [] = return []
  decode ('#':a:b:r) = case readHex [a,b] of
                         [(c,"")] -> (toEnum c:) `fmap` decode r
                         _        -> Nothing
  decode (c:r) | c /= '#' = (c:) `fmap` decode r
  decode _ = Nothing
dName _ = Nothing

hexencode :: BS.ByteString -> BS.ByteString
hexencode = BS.concatMap hex
    where hex x = BS.pack [hex1 ((x `shiftR` 4) .&. 15),hex1 (x .&. 15)]
          hex1 a | a<10 = a + 48
                 | otherwise = a + 55

binarizedictcontents :: (MonadWriter Bin.Builder m, MonadState Bool m) => [(BS.ByteString, Value)] -> m ()
binarizedictcontents [] = return ()
binarizedictcontents ((k,v):xs)
    = do
        binarizepair (k,v)
        binarizedictcontents xs

binarizepair :: (MonadState Bool m, MonadWriter Bin.Builder m) => (BS.ByteString, Value) -> m ()
binarizepair (_,Null) = return ()
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
entrya :: (IsValue a1, IsName a) => a -> [a1] -> (BS.ByteString, Value)
entrya a b = entry a (array b)

entryna :: (Real a1, IsName a) => a -> [a1] -> (BS.ByteString, Value)
entryna a b = entry a (arrayn b)

entrys :: (IsName a, IsString b) => a -> b -> (BS.ByteString, Value)
entrys a b = entry a (string b)

showpair :: (Show t) => (BS.ByteString, t) -> [Char]
showpair (a,b) = "/" ++ BSC.unpack a ++ " " ++ show b

newtype RefID = RefID Int deriving (Eq,Ord)

instance Show RefID where
    show r = show (objno r) ++ " " ++ show (gener r) ++ " R"

refid :: Int -> Int -> RefID
refid a b = RefID ((a `shiftL` 8) .|. (b .&. 255))

gener :: RefID -> Int
gener (RefID a) = a .&. 255

objno :: RefID -> Int
objno (RefID a) = a `shiftR` 8


showref :: RefID -> [Char]
showref v = show (objno v) ++ " " ++ show k ++ " R"
    where k = if z==255 then 65535 else z
          z = gener v

dict :: (IsValue t) => [(BS.ByteString, t)] -> Value
dict x = Dict . map (\(a,b) -> (a, value b)) $ x

empty_dict :: Value
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
trailerF v (Document ver ((Body _ objs):bodies)) = Document ver ((Body v objs):bodies)
trailerF _ (Document _ []) = error "Document needs to have at least one body"

trailer :: DictData -> State Document ()
trailer x = modify (trailerF x)

nextFreeIdF :: Document -> RefID
nextFreeIdF doc = refid x 0
    where
        bodies = documentBodies doc
        x = 1 + maximum (0 : map lastofbody bodies)
        lastofbody (Body _ m) = maybe 0 (\((key,_),_) -> key) (IntMap.maxViewWithKey m)

addIndir :: Indir -> State Document RefID
addIndir object = do
    doc <- get
    let (ndoc,r) = addIndirF object doc
    put ndoc
    return r

addIndirF :: Indir -> Document -> (Document,RefID)
addIndirF object doc@(Document ver ((Body trailer' objs):bodies)) = (ndoc,refid')
    where
        refid' = nextFreeIdF doc
        value' = UsedEntry (gener refid') object
        newbody = Body trailer' (IntMap.insert (objno refid') value' objs)
        ndoc = (Document ver (newbody:bodies))
addIndirF _ _ = error "Document needs to have at least one body"

setIndir :: RefID -> Indir -> State Document ()
setIndir refid' indir = modify (setIndirF refid' indir)

setIndirF :: RefID -> Indir -> Document -> Document
setIndirF refid' object (Document ver ((Body trailer' objs):bodies)) = ndoc
    where
        key = objno refid'
        value' = UsedEntry (gener refid') object
        newbody = Body trailer' (IntMap.insert key value' objs)
        ndoc = (Document ver (newbody:bodies))
setIndirF _ _ _ = error "Document needs to have at least one body"

setObject :: RefID -> Value -> State Document ()
setObject refid' value' = setIndir refid' (Indir value' Nothing)

addObject :: Value -> State Document RefID
addObject text = addIndir (Indir text Nothing)

addStream :: Value -> BSL.ByteString -> State Document RefID
addStream value'@(Dict dt) strm
    | Just _ <- Prelude.lookup (BSC.pack "Length") dt =
        addIndir (Indir value' (Just strm))
    | otherwise =
        do rec s <- addIndir (Indir (Dict (entry "Length" cl:dt)) (Just strm))
               cl <- addIndir (Indir (Number 0) Nothing)
           return s

addStream _ _ = error "Stream must begin with a dict"

lookup :: RefID -> Document -> Maybe Indir
lookup refid' (Document _ bodies) = msum (map check bodies)
    where
        check (Body _ objmap) = case IntMap.lookup (objno refid') objmap of
                          Just (UsedEntry gen indir) -> if gen==gener refid'
                                                           then Just indir
                                                           else Nothing
                          _ -> Nothing

-- Lookup a key in a dictionary (possibly following a reference to find the dictionary)
lookupRef :: Document -> BS.ByteString -> Value -> Maybe Value
lookupRef _ k (Dict l) = Prelude.lookup k l
lookupRef d k (Ref n)  = do
  Indir (Dict l) _ <- PdfModel.lookup n d
  Prelude.lookup k l
lookupRef _ _ _ = Nothing

--writeObject :: (BS.ByteString -> IO ())     -- write action
--            -> (IO Int)                     -- tell offset action
--            -> (Int,Map.Map Int (Int,Int))
--            -> (Int,Entry)                  -- (objno,entry)
--            -> IO () -- (Maybe (Int,(Int,Int)))   -- objno,gener,offset
writeObject :: (Monad m, Num a, Show a) =>
               (BS.ByteString -> m ())
               -> m a
               -> (IntMap.IntMap a, IntMap.IntMap a)
               -> (IntMap.Key, Entry)
               -> m (IntMap.IntMap a, IntMap.IntMap a)
writeObject write tell' (lastlength,objnooffsetmap) ((objno1,UsedEntry gener' (Indir text stream))) = do
    pos <- tell'
    -- print (pos,objno1,gener')
    -- print text
    write (BSC.pack (show objno1 ++ " " ++ show gener' ++ " obj\n"))
    case text of
        Number x -> case IntMap.lookup objno1 lastlength of
                      Just v -> write (BSC.pack (show v))
                      Nothing -> write (BSC.pack (show x))
        _ -> do
            mapM_ write (BSL.toChunks (Bin.toLazyByteString (binarize text)))
            return ()
    newlastlength <- case stream of
        Just x -> do
            write $ BSC.pack "\nstream\n"
            b <- tell'
            mapM_ write (BSL.toChunks x)
            e <- tell'
            write $ BSC.pack "\nendstream"
            let l = (e-b)
            let Dict dictdata = text
            case Prelude.lookup (BSC.pack "Length") dictdata of
              Just (Ref r) -> return (IntMap.insert (objno r) l lastlength)
              _ -> return lastlength
        _ -> return lastlength
    write $ BSC.pack "\nendobj\n"
    let newobjnoffsetmap = IntMap.insert objno1 pos objnooffsetmap
    return (newlastlength,newobjnoffsetmap)

writeObject _ _ x ((_,_)) = return x

createXRefTable :: IntMap.IntMap Int
                -> [(Int,Entry)]
                -> [(Int,[Either (Int,Int) (Int,Int)])]
createXRefTable offsetmap objects =
    group (map x objects)
    where
        x (objno',(UsedEntry gener' _)) = let Just v = IntMap.lookup objno' offsetmap in (objno',Right (v,gener'))
        x (objno',(FreeEntry gener')) = (objno',Left (0,gener'))
        group [] = []
        group (xs@((objno',_):_)) = let (a,b) = break br (zip xs [objno' ..]) in (objno',map (snd . fst) a) : group (map fst b)
        br ((objno',_),index) = index/=objno'



binarizeXRefTable :: (Show t2, Show t1, Show t4, Show t3, Show t) => [(t, [Either (t1, t2) (t3, t4)])] -> P.String
binarizeXRefTable table = concatMap z table
    where
        z (off,entries) = show off ++ " " ++ show (length entries) ++ " \n" ++ concatMap binarizeXRefTableEntry entries
        binarizeXRefTableEntry (Right (a,b)) = entry' a b 'n'
        binarizeXRefTableEntry (Left (a,b)) = entry' a b 'f'
        entry' :: (Show a, Show b) => a -> b -> Char -> P.String
        entry' a b f = last' 10 ("0000000000" ++ show a) ++ " " ++ last' 5 ("00000" ++ show b) ++ " " ++ [f] ++" \n"
        last' n x = drop (length x - n) x


writeBody   :: (BS.ByteString -> IO ())     -- write action
            -> (IO Int)                     -- tell offset action
            -> (Int,Int)
            -> Body                         -- (objno,entry)
            -> IO (Int,Int)
writeBody write tell' (highest,prevoffset) (Body trailer' objects) = do
    (_,r) <- foldM (writeObject write tell') (IntMap.empty,IntMap.empty) (IntMap.toList objects)
    v <- tell'
    write $ BSC.pack "xref\n"

    let x = createXRefTable r (IntMap.toList objects)
    write $ BSC.pack (binarizeXRefTable x)
    let Just ((k1,_),_) =  IntMap.maxViewWithKey objects
        k = (k1+1) `max` highest
    write $ BSC.pack "trailer\n"
    let trailernosizeprev = filter (\(x',_) -> x'/=BSC.pack "Size" && x'/=BSC.pack "Prev") trailer'
    let trailersize = entryn "Size" k : trailernosizeprev
    let trailerfull = if prevoffset/=0
                           then entryn "Prev" prevoffset : trailersize
                           else trailersize
    mapM_ write (BSL.toChunks (Bin.toLazyByteString (binarize (Dict trailerfull))))
    write $ BSC.pack ("\nstartxref\n" ++ show v ++ "\n%%EOF\n")
    return (k,v)


writeDocument :: (BS.ByteString -> IO ()) -> IO Int -> Document -> IO ()
writeDocument write tell' (Document version bodies) = do
    write $ BSC.concat [BSC.pack "%", version, BSC.pack "\n"]
    foldM_ (writeBody write tell') (0,0) (reverse bodies)



writeFileX :: FilePath -> Document -> IO ()
writeFileX file document' = do
    bracket (openBinaryFile file WriteMode)
            (hClose)
            (\handle' -> writeDocument (\x -> BS.hPutStr handle' x >> hFlush handle')
                        (liftM fromIntegral $ hTell handle') document')

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
                l = map (\(k,x) -> let (v',r') = both x in ((k,v'),r')) a
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



ext :: Value -> [(BS.ByteString, Value)] -> Value
ext (Dict d) r = Dict (r ++ d)
ext _ _ = error "ext can be used only for Dict"

trailer_dict :: IsValue b => b -> [(BS.ByteString, Value)]
trailer_dict catalog = [ entry "Root" catalog ]

catalog_dict :: IsValue b => b -> Value
catalog_dict pages = dict [ entry "Type" "Catalog"
                                        , entry "Pages" pages
                                        ]

page_tree_item :: (IsValue b, IsValue b1) => b -> b1 -> Value
page_tree_item typex parent = dict [ entry "Type" typex, entry "Parent" parent ]

pages_dict :: (IsValue b, Real b1, IsValue a) => b -> b1 -> [a] -> Value
pages_dict parent count kids = page_tree_item "Pages" parent `ext` [ entryn "Count" count
                                                  , entry "Kids" (array kids)]

page_dict :: (IsValue b, IsValue b1) => b -> b1 -> Value
page_dict parent contents = page_tree_item "Page" parent `ext` [ entry "Contents" contents ]

standard_font_dict :: (IsValue b, IsValue c) => b -> c -> Value
standard_font_dict basefont encoding = dict [ entry "Type" "Font"
                                            , entry "Subtype" "Type1"
                                            , entry "BaseFont" basefont
                                            , entry "Encoding" encoding
                                            ]

function_dict :: (Real a1) => Int -> [a1] -> Value
function_dict typex domain = dict [ entryn "FunctionType" typex, entryna "Domain" domain ]

function_type4_dict :: (Real a1, Real a11) => [a1] -> [a11] -> Value
function_type4_dict domain range = function_dict 4 domain `ext` [ entryna "Range" range
                                              ]
function_type2_dict :: (Real a1, Real a11, Real a12, Real b) => [a1] -> [a11] -> [a12] -> b -> Value
function_type2_dict domain c0 c1 n = function_dict 2 domain `ext` [ entryna "C0" c0
                                                                  , entryna "C1" c1
                                                                  , entryn "N" n
                                                                  ]

shading_dict :: (IsValue b1) => Int -> b1 -> Value
shading_dict typex colorspace = dict [ entryn "ShadingType" typex
                              , entry  "ColorSpace" colorspace
                              ]

shading_type1_dict :: (IsValue b, IsValue b1, IsValue b2) => b -> b1 -> b2 -> Value
shading_type1_dict colorspace domain function = shading_dict 1 colorspace `ext` [
                               entry "Domain" domain
                              , entry "Function" function
                              ]

shading_type2_dict :: (IsValue b, Real a1, Real a11, IsValue b1) => b -> [a1] -> [a11] -> b1 -> Value
shading_type2_dict colorspace bbox coords function = shading_dict 2 colorspace `ext` [ entryna "BBox" bbox
                              , entryna "Coords" coords
                              , entry "Function" function
                              ]

shading_type3_dict :: (IsValue b, Real a1, Real a11, IsValue b1) => b -> [a1] -> [a11] -> b1 -> Value
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

multipage :: IsValue b => b -> Double -> Double -> Int -> Int -> [P.String] -> State Document [RefID]
multipage parent width height xcount ycount contents =
    mapM multipagehelper (groupN (xcount*ycount) contents)
    where
        multipagehelper contents' = do
            rec page <- addObject (page_dict parent pagecontents)
                pagecontents <- addStream (empty_dict) (doconcat contents')
            return page
        doconcat contents' = BSL.fromChunks (zipWith ($) [cnts x y | y <- [ycount-1,ycount-2..0], x <- [0..xcount-1]] contents')
        cnts x y text = BSC.pack $ place width height xcount ycount x y text
        groupN _ [] = []
        groupN n list = let (a,b) = splitAt n list in a : groupN n b

indexed_array :: (IsValue a, Real x, IsValue a1) => a -> x -> a1 -> Value
indexed_array base count indexdata = array [ value "Indexed", value base, valuen count, value indexdata ]

separation_array :: (IsValue a, IsValue a1, IsValue a2) => a -> a1 -> a2 -> Value
separation_array name' base func = array [ value "Separation", value name', value base, value func ]

devicen_array :: (IsValue a, IsValue a1, IsValue a2) => [a] -> a1 -> a2 -> Value
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
    traceM (show (take 20 startxreftext))
    [(istart,_)] <- return (readDec startxreftext)
    traceM (show istart)
    bodies <- parseBodyList bin istart
    return (Document (BSC.pack "PDF-1.4") bodies)
    where
      startxreftext = dropWhile (\x -> x <= ' ') $ BSC.unpack $ findLastStartXref bin
      startxref = BSC.pack "startxref"
      findLastStartXref bytes =
        case BS.breakSubstring startxref bytes of
          (h,t) | BS.null t -> h
                | otherwise -> findLastStartXref (BS.drop 9 t)



-- parseBodyList :: IntSet.IntSet -> BinaryData -> Int -> Maybe [(Body,IntSet.IntSet)]
parseBodyList :: BS.ByteString -> Int -> Maybe [Body]
parseBodyList bin start = do
    traceM ("Parse body at offset " ++ show start)
    body <- parseBody bin start
    let Body trailer' _ = body
    rest <- case Prelude.lookup (BSC.pack "Prev") trailer' of
                Just (Number offset') -> parseBodyList bin (round offset')
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
        sequence' vs (m:ms') = m >>= (\v -> sequence' (v:vs) ms')

mapM2 :: Monad m => (a -> m a1) -> [a] -> m [a1]
mapM2 f xs = sequence2 (map f xs)

-- the body should be the parsed body, kind of fixed point operator
xparse :: Body -> BS.ByteString -> P.ReadP Body
xparse body bin = do
    xref <- parseRegular
    when (xref/=BSC.pack "xref") $ do
                    traceM ("In place of 'xref' got " ++ show xref)
                    P.pfail

    traceM "Got past xref"
    p <- P.manyRev parseXRefSection
    traceM "Got past xref section"
    _ <- parseOperatorEq "trailer"
    traceM "Got past xref trailer token"
    e <- parseDict
    traceM ("Got past xref trailer dict " ++ show e)
    let entries = IntMap.fromList (map (\(a,_,c) -> (a,c)) ((0,0,FreeEntry 65535) : concat p))
    return (Body e entries)
    where
        parseXRefSection = {-# SCC "xparse.parseXRefSection" #-} do
            objno' <- parseNatural
            ncount <- parseNatural
            --P.count ncount (parseXRefEntry uniq nid)
            mapM2 parseXRefEntry [objno'..objno'+ncount-1]
        parseXRefEntry objno' = {-# SCC "xparse.slot" #-} do
            offset' <- parseNatural
            gener' <- parseNatural
            nf <- parseRegular
            --mtrace "slot parsed"
            if nf==BSC.pack "f"
               then return (objno',0,FreeEntry gener')
               else if nf==BSC.pack "n"
                    then let
                             (obj,gen,indir) = parseIndir bin offset'
                             --usedEntry = UsedEntry gener' indir
                             usedEntry = UsedEntry gen indir
                         in -- return (objno',offset',usedEntry)
                           return (obj,offset',usedEntry)
                    else P.pfail

        findLength :: Body -> Value -> Int
        findLength (Body _ objects) (Dict d) = {-# SCC "xparse.findLength" #-}
            case Prelude.lookup (BSC.pack "Length") d of
                Just (Number v) -> (round v)
                Just (Ref v) -> case IntMap.lookup (fromIntegral $ objno v) objects of
                                    Just entry' -> case entry' of
                                        UsedEntry _ indir -> case indir of
                                            Indir value' _ -> case value' of
                                                Number v' -> round v'
                                                x -> error ("/Length not found case 5: " ++ show x)
                                        x -> error ("/Length not found case 4: " ++ show x)
                                    x -> error ("/Length not found case 1: " ++ show x)
                _ ->  error "/Length not found case 2"
        findLength _ _ =  error "/Length not found case 3"


        parseIndir :: BS.ByteString -> Int -> (Int,Int,Indir)
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
        parseIndirA :: BS.ByteString -> Int -> (Int,Int,Indir)
        parseIndirA bin' off = -- trace ("Indir at offset " ++ show off) $
            --case P.readP_to_S parseIndir1 (BS.unpack (BS.drop (fromIntegral off) bin')) of
            let bin'' = BS.drop (fromIntegral off) bin' in
            case P.readP_to_S parseIndir1 bin'' of
                ((_,      thing@(_obj,_gen,Indir _e Nothing)),_):_ -> thing
                ((ntokens,(obj,gen,Indir e (Just _))),_):_ ->
                    let len = findLength body e
                        dt = cut ntokens len bin''
                        indir = (obj,gen,Indir e (Just (BSL.fromChunks [dt])))
                    in indir
                _ -> error $ "Cannot parse Indir at offset " ++ show off ++ ", beginning: " ++ take 300 (BSC.unpack bin'')
        cut off len = BS.take (fromIntegral len) . BS.drop (fromIntegral off)

        parseIndir1 :: MyReadP (Int,(Int,Int,Indir))
        parseIndir1 = P.countsym $ do
            obj <- parseNatural -- should check if naturals match with reference
            gen <- parseNatural
            _ <- parseOperatorEq "obj"
            e <- parseValue
            (parseOperatorEq "endobj" >> return (obj, gen, Indir e Nothing)) `mplus`
                             (withStream e >>= \indir -> return (obj,gen,indir))
        withStream e = do
            _ <- parseOperatorEq "stream"
            _ <- P.munch isspacenoteol
            (P.string (BS.pack [0x0d,0x0a]) >> return ())
                P.<++ (P.char 0x0d >> return ())
                P.<++ (P.char 0x0a >> return ())
                P.<++ (return ())
            return (Indir e (Just undefined))




-- | Parse body from bin starting xref at offset
parseBody :: Integral a => BS.ByteString -> a -> Maybe Body
parseBody bin startxref = case aparse of
    [(body',_)] -> Just body'
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
               , parseString >>= \raw -> let str = String False raw in traceM (show str) >> return str
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
parseRegular :: P.ReadP BS.ByteString
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
        beforedot value' = (do
                c <- P.satisfy (\c -> c >= c0 && c <= c9)
                beforedot (value'*10 + fromIntegral (c - c0)))
            P.+++ (do
                _ <- P.char dot
                afterdot value' 1)
            P.+++ (do
                v <- P.look
                if BSC.null v
                    then return (fromIntegral value')
                    else P.pfail)
        afterdot :: Integer -> Integer -> MyReadP Double
        afterdot value' divider = (do
                c <- P.satisfy (\c -> c >= c0 && c <= c9)
                afterdot (value'*10 + fromIntegral (c - c0)) (divider*10))
            P.+++ (do
                v <- P.look
                if BSC.null v
                    then return (fromIntegral value' / fromIntegral divider)
                    else P.pfail)


mkop :: BS.ByteString -> Value
mkop name'
    | name'==BSC.pack "true" = Boolean True
    | name'==BSC.pack "false" = Boolean False
    | name'==BSC.pack "null" = Null
    | [(flt,_)] <- P.readP_to_S parseNumber name' = Number flt
    | otherwise = Operator name'

-- | Parse operator given as 'name'.
parseOperatorEq :: [Char] -> MyReadP BS.ByteString
parseOperatorEq name' = parseRegular >>= \x ->
     if x==BSC.pack name'
         then return x
         else P.pfail

-- | Parse reference ID of the form n g R
parseRef :: MyReadP RefID
parseRef = do
    refno <- parseNatural
    gener' <- parseNatural
    _ <- parseOperatorEq "R"
    return (refid (fromIntegral refno) (fromIntegral gener'))


-- | Parse string delimited by ( ) or < > (hexstring).
parseString :: MyReadP BS.ByteString
parseString = do
    parseSpace
    _ <- P.char (BSB.c2w '(')
    x <- string_p1
    _ <- P.char (BSB.c2w ')')
    return (BS.concat x)
    where
        string_p1 = fmap reverse $ P.manyRev (string_char_p P.<++ string_bs_p P.<++ string_string_p)
        string_char_p = P.munch1 (\x -> x/=BSB.c2w '(' &&
                                   x/=BSB.c2w '\\' &&
                                   x/=BSB.c2w ')')

        string_bs_p = do
                _ <- P.char (o '\\')
                P.choice [ P.char (o 'f') >> return (BSC.singleton '\f')
                         , P.char (o 'r') >> return (BSC.singleton '\r')
                         , P.char (o 'n') >> return (BSC.singleton '\n')
                         , P.char (o 't') >> return (BSC.singleton '\t')
                         , P.char (o 'b') >> return (BSC.singleton '\b')
                           -- \\\n or \\\r or \\\r\n all result in \n
                         , P.char (o '\r') >>
                                  (P.char (o '\n') P.<++ return (o '\n')) >> return (BSC.singleton '\n')
                         , P.char (o '\n') >> return (BSC.singleton '\n')
                         , (do
                             o1 <- octDigit
                             o2 <- octDigit
                             o3 <- octDigit
                             return $ BS.singleton ((o1-48)*8*8 + (o2-48)*8 + (o3-48)))
                            P.<++
                           (do
                             o2 <- octDigit
                             o3 <- octDigit
                             return $ BS.singleton ((o2-48)*8 + (o3-48)))
                            P.<++
                           (do
                             o3 <- octDigit
                             return $ BS.singleton ((o3-48)))
                         ] P.<++ do
                              next <- P.get
                              traceM ("Backslash used before " ++ show (BSB.w2c next))
                              return (BS.singleton next)
        string_string_p = do
             _ <- P.char (o '(')
             x <- string_p1
             _ <- P.char (o ')')
             return (BS.concat ([BSC.singleton '('] ++ x ++ [BSC.singleton ')']))
        o = BSB.c2w
        octDigit = P.satisfy (\x -> BSB.c2w '0' <= x && x <= BSB.c2w '7')

parseHexString :: MyReadP BS.ByteString
parseHexString = do
    parseSpace
    _ <- P.char (c '<')
    x <- P.munch (\y -> ishexchar y || isspace y)
    _ <- P.char (c '>')
    return (BS.pack (hexdecode (filter ishexchar (BS.unpack x))))
    where
        hexdecode [] = []
        hexdecode [x] = [hexvalue x `shiftL` 4]
        hexdecode (a:b:xs) = ((hexvalue a `shiftL` 4) + hexvalue b)
                              : hexdecode xs
        hexvalue x | x>=c '0' && x<=c '9' = fromIntegral (x - c '0')
        hexvalue x | x>=c 'A' && x<=c 'F' = fromIntegral (10 + x - c 'A')
        hexvalue x | x>=c 'a' && x<=c 'f' = fromIntegral (10 + x - c 'a')
        hexvalue x = error ("hexvalue cannot know value of " ++ show x)
        c = BSB.c2w

-- | Parse array [ ]
parseArray :: MyReadP ArrayData
parseArray = do
    parseSpace
    _ <- P.char (BSB.c2w '[')
    e <- P.manyRev parseValue
    parseSpace
    _ <- P.char (BSB.c2w ']')
    return (reverse e)

-- | Parse dictionary << >>
parseDict :: MyReadP DictData
parseDict = do
    parseSpace
    _ <- P.string (BSC.pack "<<")
    e <- P.manyRev pair
    parseSpace
    _ <- P.string (BSC.pack ">>")
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
    _ <- P.char (BSB.c2w '/')
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
iseol _ = False

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

printIfParsedDifferent :: P.String -> P.String -> IO ()
printIfParsedDifferent ainput aoutput = do
  let input = BSC.pack ainput
      output = BSC.pack aoutput
  case P.readP_to_S parseString input of
    [(output',_)] -> do
      when (output/=output') $
           putStrLn $ "Parsed " ++ BSC.unpack input ++ " as " ++ show output' ++ " expected " ++ show output
    [] -> do
      putStrLn $ "Cannot at all parse " ++ BSC.unpack input
    xs -> do
      putStrLn $ "Many parses for " ++ BSC.unpack input ++ ": " ++ show (map fst xs)

testReadParseString :: IO ()
testReadParseString = do
  printIfParsedDifferent "()" ""
  printIfParsedDifferent "(abc)" "abc"
  printIfParsedDifferent "(())" "()"
  printIfParsedDifferent "(\\()" "("
  printIfParsedDifferent "(\\(\\)\\)\\))" "()))"
  printIfParsedDifferent "(\\n\\r\\t\\b\\f)" "\n\r\t\b\f"
  printIfParsedDifferent "(\\1q\\01q\\001q\\1)" "\x01q\x01q\x01q\x01"
  printIfParsedDifferent "(\\\rA\\\nA\\\r\nA)" "\nA\nA\nA"
  printIfParsedDifferent "(\\q\\h\\p\\v)" "qhpv"
