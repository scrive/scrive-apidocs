{- |
    The input validation rules.
-}
module InputValidation
    ( Result(..)
    , isGood
    , isBad
    , isEmpty
    , emptyOK
    , resultToMaybe
    , getOptionalField
    , getDefaultedField
    , getCriticalField
    , asValidEmail
    , asDirtyEmail
    , asDirtyPassword
    , asValidName
    , asValidCompanyName
    , asValidCompanyNumber
    , asValidNOBankIdPersonalNumber
    , asValidPersonalNumber
    , asValidSEBankIdPersonalNumber
    , asValidAddress
    , asValidPhone
    , asValidPhoneForSMS
    , asValidPhoneForNorwegianBankID
    , asValidPosition
    , asValidCheckBox
    , asValidID
    , asValidNumber
    , asValidDocID
    , asValidDocIDList
    , asValidAttachmentID
    , asValidAttachmentIDList
    , asValidUserID
    , asValidFieldValue
    , asValidInviteText
    , asValidIPAddressWithMaskList
    , asValidUserGroupID
    , asValidSwedishSSN
    , asValidNorwegianSSN
    , asValidDanishSSN
    , asValidFinnishSSN
    , asValidZip
    , asValidCity
    , asValidCountry
    , asWord32
    , isValidEmail
    , isValidPhoneForSMS
    , unjsonWithValidationOrEmpty
    , unjsonWithValidationOrEmptyText
) where

import Data.Char
import Data.Int
import Data.String.Utils hiding (maybeRead)
import Data.Word (Word32)
import Log
import Numeric
import Text.XML
import qualified Data.Semigroup as SG
import qualified Data.Text as T
import qualified Data.Text.ICU as Rx
import qualified Data.Text.Lazy as TL
import qualified Data.Unjson as UJ

import Attachment.AttachmentID
import Doc.DocumentID
import Happstack.Fields hiding (getFields)
import IPAddress
import Kontra
import User.Email
import User.Model
import UserGroup.Types
import Utils.String

{- |
    The input data.
-}
type Input = Maybe String

{- |
    If it goes wrong we get something that we can use to display a flash message.
    If it goes right we get the processed value.
    Alternatively the user could've entered nothing.
-}
data Result a = Good a
              | Bad
              | Empty
              deriving (Eq, Show, Functor)

instance Monoid a => SG.Semigroup (Result a) where
    Good a1 <> Good a2 = Good $ mappend a1 a2
    Empty   <> a       = a
    a       <> Empty   = a
    Bad     <> _       = Bad
    _       <> Bad     = Bad

instance Monoid a => Monoid (Result a) where
    mempty  = Empty
    mappend = (SG.<>)

instance Applicative Result where
  pure = Good
  Good f <*> Good v = Good (f v)
  Good _ <*> Bad    = Bad
  Good _ <*> Empty  = Empty
  Bad    <*> _      = Bad
  Empty  <*> _      = Empty

instance Monad Result where
  return = Good

  Good x >>= f = f x
  Bad >>= _ = Bad
  Empty >>= _ = Empty

isGood:: Result a -> Bool
isGood (Good _) = True
isGood _ = False

isBad:: Result a -> Bool
isBad Bad = True
isBad _ = False

isEmpty :: Result a -> Bool
isEmpty Empty = True
isEmpty _ = False

emptyOK :: Monoid a => (String -> Result a) -> (String -> Result a)
emptyOK _ ""  = Good mempty
emptyOK val s = val s

{- |
    Use this to get and validate most of the usual fields.  If the field
    is empty the user won't get told about it, you have to just act sensibly
    given a Nothing.
-}
getOptionalField :: Kontrakcja m => (String -> Result a) -> String -> m (Maybe a)
getOptionalField validate =
    getValidateAndHandle validate optionalFieldHandler

optionalFieldHandler :: Kontrakcja m => (Input, Result a) -> m (Maybe a)
optionalFieldHandler result =
    logIfBad result
    >>= asMaybe

{- |
    Use this to get a field that has a default value when Empty.
-}
getDefaultedField :: Kontrakcja m => a -> (String -> Result a) -> String -> m (Maybe a)
getDefaultedField d validate =
    getValidateAndHandle validate (defaultedFieldHandler d)

defaultedFieldHandler :: Kontrakcja m => a -> (Input, Result a) -> m (Maybe a)
defaultedFieldHandler d result =
    logIfBad result
    >>= withDefault d
    >>= asMaybe

{- |
    Use this to get a field that is absolutely required, probably
    because it is hidden. If this field is missing that means something
    bad has happened.
-}
getCriticalField :: Kontrakcja m => (String -> Result a) -> String -> m a
getCriticalField validate =
    getValidateAndHandle validate criticalFieldHandler

criticalFieldHandler :: Kontrakcja m => (Input, Result a) -> m a
criticalFieldHandler result =
    withRequiredFlash result
    >>= logIfBad
    >>= withFailure

{- |
    Gets a named field, validates it, and then handles the result.
-}
getValidateAndHandle :: Kontrakcja m => (String -> Result a) -> ((Input, Result a) -> m b) -> String -> m b
getValidateAndHandle validate handle fieldname = do
  result <- getAndValidate validate fieldname
  handle result

{- |
    Gets a named field and validates it.  It will return a pair of the input,
    and output.
-}
getAndValidate :: Kontrakcja m => (String -> Result a) -> String -> m (Input, Result a)
getAndValidate validate fieldname = do
  mrawvalue <- getField fieldname
  case mrawvalue of
    Nothing -> return $ (Nothing, Empty)
    (Just rawvalue) -> return $ (Just rawvalue, validate rawvalue)

{- |
    Puts any validation problem in the security log.
    This'll at least mean we've got a record of suspicious
    behaviour.
-}
logIfBad :: Kontrakcja m => (Input, Result a) -> m (Input, Result a)
logIfBad x@(input, Bad) = do
  logValidationBad input
  return x
logIfBad x = return x

logValidationBad :: Kontrakcja m => Input-> m ()
logValidationBad input = do
  ctx <- getContext
  logInfo "Input validation failed" $ object [
      "ip"    .= show (get ctxipnumber ctx)
    , "user"  .= maybe "unknown" (unEmail . useremail . userinfo)
                 (get ctxmaybeuser ctx)
    , "input" .= show input
    ]

{- |
    Interprets the Result as a Maybe,
    so you get a Just for sensible input,
    but a Nothing for invalid or empty input.
-}
asMaybe :: Kontrakcja m => (Input, Result a) -> m (Maybe a)
asMaybe (_,Good x) = return $ Just x
asMaybe _        = return Nothing


resultToMaybe :: Result a -> Maybe a
resultToMaybe (Good a) = Just a
resultToMaybe _ = Nothing

{- |
    If the result is Empty then this uses the
    given default value.
-}
withDefault :: Kontrakcja m => a -> (Input, Result a) -> m (Input, Result a)
withDefault d (input, Empty) = return $ (input, Good d)
withDefault _ x     = return x

{- |
    If the result is Empty then this turns it
    into a Bad result with an appropriate flash message.
-}
withRequiredFlash :: Kontrakcja m => (Input, Result a) -> m (Input, Result a)
withRequiredFlash (input, Empty) = return $ (input, Empty)
withRequiredFlash x     = return x


{- |
    You either get a value or a failure,
    that's it.  Which means this'll fail
    for invalid or empty input.
-}
withFailure :: Kontrakcja m => (Input, Result a) -> m a
withFailure (_,Good x) = return x
withFailure _        = internalError

checkFormatWithSensitive :: Bool -> T.Text -> String -> Result String
checkFormatWithSensitive caseSensitive format input | isValidFormat input = return input
                                                    | otherwise = Bad
  where isValidFormat = isJust . Rx.find (Rx.regex opts format) . T.pack
        opts = if caseSensitive then [] else [Rx.CaseInsensitive]

checkFormat :: T.Text -> String -> Result String
checkFormat = checkFormatWithSensitive True

-- | Creates a clean and validated email.
--
-- Validating emails is bizarrely hard.  These rules define a subset
-- of what the official rules (RFC) would allow you to have.  This is
-- because the official rules are very liberal, and email providers in
-- practise aren't that liberal.  So this is kind of a, hopefully,
-- practical compromise. So officially
--
-- > “Abc\@def”@example.com
--
-- is a valid email address, but really if a user put that in, it's
-- got to be wrong!
--
-- Rules:
--
--  * It must contain an \@ character
--
--  * Before the \@ character there must appear one of more of the
--  following: Full-stop ., Underscore _, Percentage %, Plus +, Hyphen
--  -, ASCII letters a-z and A-Z, digits 0-9
--
--  * After the \@ character you must have a full-stop .
--
--  * Between the \@ character and the full-stop you must have one of
--  the following: Full-stop ., Hyphen -, ASCII letters a-z and A-Z,
--  digits 0-9
--
--  * After the full-stop there must be two or more
--  characters and these can be: ASCII letters a-z and A-Z.
--
--  * Size: Up to 200 characters
--
-- Also emails are lowercased.
--
asValidEmail :: String -> Result String
asValidEmail input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 200
    >>= checkFormatWithSensitive False emailFormat
    >>= mkLowerCase
    where
      -- This must match PATTERN_EMAIL in frontend code
      emailFormat = "^[a-zA-Z0-9&._%+-]+@(\\p{L}|[0-9.-])+[.][a-z]{2,}$"

{- |
    Creates an email that hasn't been completely validated.  It still does handy things
    like trimming whitespace and lowercasing though.  This is useful in situations
    like checking a login.  Because it's dirty make sure you through it away,
    and don't store it.
-}
asDirtyEmail :: String -> Result String
asDirtyEmail input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= mkLowerCase


{- |
    Creates a dirty password.  This is useful when validating
    logins for example.  Because it's dirty you should make sure to throw it away,
    and not store it.
-}
asDirtyPassword :: String -> Result String
asDirtyPassword input = checkIfEmpty input

{- |
    Creates a clean and validated name (works for first or second)
    White list: Letters, digits, spaces, apostrophes and hyphens
    Size: Up to 100 chars
    Must match PATTERN_NAME in frontend code
-}
asValidName :: String -> Result String
asValidName input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkFormatWithSensitive False "^[- 0-9'\\p{L}]{0,100}$"

{- |
    Barely validates company names.
    Size: Up to 100 chars
    Must match CompanyNameValidation in frontend code
-}
asValidCompanyName :: String -> Result String
asValidCompanyName input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 100

{- |
    Creates a clean and validated company number.
    White list: Alphabetic characters, Numeric characters, spaces and hyphens
    Size: From 4 to 50 chars
    Regex must match PATTERN_COMPANY_NUMBER in frontend code
-}
asValidCompanyNumber :: String -> Result String
asValidCompanyNumber input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkFormatWithSensitive False "^[- 0-9\\p{L}]{4,50}$"

{- |
    Barely validates zip codes.
    Size: 1..20 chars
    Must match CompanyZipValidation in frontend code
-}
asValidZip :: String -> Result String
asValidZip input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 20

{- |
    Barely validates city names.
    Size: 1..60 chars
    Must match CompanyCityValidation in frontend code
-}
asValidCity :: String -> Result String
asValidCity input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 60

{- |
    Barely validates country names.
    Size: 1..60 chars
    Must match CompanyCountryValidation in frontend code
-}
asValidCountry :: String -> Result String
asValidCountry input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 60

{- |
    Validated swedish personal number.
    White list: Digits only
    Size: From 10 or 12 chars
-}
asValidSwedishSSN :: String -> Result String
asValidSwedishSSN input =
    filterOutCharacters [' ', '-', '+'] input
    >>= checkIfEmpty
    >>= checkLengthIs [10,12]
    >>= checkOnly [isDigit]

{- |
    Validated Norwegian personal number.
    White list: Digits only
    Size: 11 chars
-}
asValidNorwegianSSN :: String -> Result String
asValidNorwegianSSN input =
    filterOutCharacters [' ', '-'] input
    >>= checkIfEmpty
    >>= checkLengthIs [11]
    >>= checkOnly [isDigit]

{- |
    Validated danish CPR personal number.
    White list: Digits only
    Size: 10
-}
asValidDanishSSN :: String -> Result String
asValidDanishSSN input =
    filterOutCharacters [' ', '-'] input
    >>= checkIfEmpty
    >>= checkLengthIs [10]
    >>= checkOnly [isDigit]

{- |
    Validated Finnish personal number.
    White list: Digits in first part, special character separator, checksum character

    Format:
    Source: https://en.wikipedia.org/wiki/National_identification_number#Finland
    Size: 10
-}
asValidFinnishSSN :: String -> Result String
asValidFinnishSSN input =
    filterOutCharacters [' '] input
    >>= checkIfEmpty
    >>= checkLengthIs [11]
    >>= \case
      [d1,d2,m1,m2,y1,y2,sep,x1,x2,x3,checksum] ->
        fromMaybe Bad $ do
          (day :: Int64)  <- maybeRead [d1,d2]
          month <- maybeRead [m1,m2]
          combined_digits <- maybeRead [d1,d2,m1,m2,y1,y2,x1,x2,x3]
          let -- some alphabetic chars are missing to prevent confusion with digits
              checksum_chars = "0123456789ABCDEFHJKLMNPRSTUVWXY"
              computed_checksum = checksum_chars !! (combined_digits `mod` length checksum_chars)
          if (  1 <= day   && day   <= 31
             && 1 <= month && month <= 12
             && computed_checksum == toUpper checksum
             && (toUpper sep `elem` ['-', '+', 'A'])
             )
            then return . return $ [d1,d2,m1,m2,y1,y2,toUpper sep,x1,x2,x3,toUpper checksum]
            else return Bad
      _ -> Bad

{- |
    Creates a clean and validated personal number.
    White list: Digits, hyphens (to be stripped)
    Size: 10 or 12
-}
asValidSEBankIdPersonalNumber :: String -> Result String
asValidSEBankIdPersonalNumber input =
    stripAllWhitespace input
    >>= filterOutCharacters "-+"
    >>= checkOnly [isDigit]
    >>= (\xs -> if
             | length xs `elem` [10, 12] -> return xs
             | otherwise -> Bad)

{- |
    Creates a clean and validated personal number.
    White list: Digits, hyphens (to be stripped)
    Size: 11
-}
asValidNOBankIdPersonalNumber :: String -> Result String
asValidNOBankIdPersonalNumber input =
    stripAllWhitespace input
    >>= filterOutCharacters "-+"
    >>= checkOnly [isDigit]
    >>= (\xs -> if
             | length xs == 11 -> return xs
             | otherwise -> Bad)

{- |
    Creates a clean and validated personal number that should cover all personal number variants
    White list: Digits, hyphens, plus (to be stripped)
    Size: 10-12
    Must match PersonalNumberValidation in frontend code
-}
asValidPersonalNumber :: String -> Result String
asValidPersonalNumber input =
    stripAllWhitespace input
    >>= filterOutCharacters "-+"
    >>= checkOnly [isDigit]
    >>= (\xs -> if
             | length xs `elem` [10..12] -> return xs
             | otherwise -> Bad)

{- |
    Creates a clean and validated address.
    White list: Space, Apostrophe ', Open and close brackets (), Colon :, Comma , Forward slash //, Full-stop ., Hash #, Hyphen -, Alphabetic characters, Numeric characters
    Size: Up to 100 chars
    Regex must match PATTERN_ADDRESS in frontend code
-}
asValidAddress :: String -> Result String
asValidAddress input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkFormatWithSensitive False "^[- '():,/.#0-9\\p{L}]{0,100}$"

asValidUserGroupID :: String -> Result UserGroupID
asValidUserGroupID input = checkIfEmpty input
  >>= \xs -> case reads xs of
    [(val,[])] -> return val
    _ -> Bad

asValidIPAddressWithMaskList :: String -> Result [IPAddressWithMask]
asValidIPAddressWithMaskList input =
    stripWhitespace input
    >>= readAll2
    where readAll src = do
            (value,rest) <- reads src
            case dropWhile isSpace rest of
              ',' : r -> do
                 (values,m) <- readAll r
                 return (value : values, m)
              _ -> return ([value],rest)
          readAll2 src = case readAll src of
                           [(result,r)] | all isSpace r -> return result
                           [] | all isSpace src -> return []
                           _ -> Bad




{- |
    Creates a clean and validated company position.
    White list: Space, Ampersand &, Apostrophe ', Open and close brackets (), Colon :, Comma , Hyphen -, Alphabetic characters, Numeric characters
    Size: Up to 100 chars
    Regex must match PATTERN_POSITION in frontend code
-}
asValidPosition :: String -> Result String
asValidPosition input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkFormatWithSensitive False "^[- &'():,0-9\\p{L}]{0,100}$"

{- |
    Creates a Bool result for a check box, depending on whether it was set to "on" or "off".
-}
asValidCheckBox :: String -> Result Bool
asValidCheckBox input =
    checkIfEmpty input
    >>= mkLowerCase
    >>= parseOnOrOff
    where
          parseOnOrOff val | val == "on"  = return True
                           | val == "off" = return False
                           | otherwise = Bad


{- |
    Strip all whitespace, braces and hyphens
    Must start with a + and be followed by at least 9 digits
    Must match PhoneValidation in frontend code
-}
asValidPhone :: String -> Result String
asValidPhone input =
    stripAllWhitespace input
    >>= filterOutCharacters "-()"
    >>= checkFormat "^\\+[0-9]{9,}$"

asValidPhoneForSMS :: String -> Result String
asValidPhoneForSMS input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 20
    >>= checkLengthIsMin 6
    >>= checkOnly (isDigit : map (==) "+ -().")
    >>= filterOutCharacters "-(). "
    >>= (\str -> if take 1 str == "+"
                 then return str
                 else Bad)

asValidPhoneForNorwegianBankID :: String -> Result String
asValidPhoneForNorwegianBankID input =
    asValidPhoneForSMS input
    >>= checkLengthIs [11]
    >>= (\str -> if take 3 str == "+47"
                    then return str
                    else Bad)

{- |
    Gets a cleaned up doc id. Useful for validating
    you're not getting fed complete garbage from hidden fields,
    this makes sure the result parses as a Int64.
-}
asValidDocID :: String -> Result DocumentID
asValidDocID input =
    checkIfEmpty input
    >>= parseAsDocID
    where
          parseAsDocID xs =
            case reads xs of
              (val,[]):[] -> return val
              _ -> Bad

asWord32 :: String -> Result Word32
asWord32 input =
  checkIfEmpty input
  >>= parseAsWord32
  where parseAsWord32 xs =
          case reads xs of
            (val,[]):[] -> return val
            _ -> Bad

asValidDocIDList :: String -> Result [DocumentID]
asValidDocIDList input =
    checkIfEmpty input
    >>= parseList
    where
          parseList xs =
            case reads xs of
              (val,[]):[] -> return val
              _ -> Bad
{- |
    Gets a cleaned up doc id. Useful for validating
    you're not getting fed complete garbage from hidden fields,
    this makes sure the result parses as a Int64.
-}
asValidAttachmentID :: String -> Result AttachmentID
asValidAttachmentID input =
    checkIfEmpty input
    >>= parseAsAttachmentID
    where
          parseAsAttachmentID xs =
            case reads xs of
              (val,[]):[] -> return val
              _ -> Bad

asValidAttachmentIDList :: String -> Result [AttachmentID]
asValidAttachmentIDList input =
    checkIfEmpty input
    >>= parseList
    where
          parseList xs =
            case reads xs of
              (val,[]):[] -> return val
              _ -> Bad

{- |
    Gets a cleaned up user id. Useful for validating
    you're not getting fed complete garbage from hidden fields,
    this makes sure the result parses as a Int64.
-}
asValidUserID :: String -> Result UserID
asValidUserID input = checkIfEmpty input
  >>= \xs -> case reads xs of
    [(val,[])] -> return val
    _ -> Bad

{- |
    Checks that the input is a valid id, meaning it can be parsed
    as an int.  This is handy if you want to check that you're not
    getting fed rubbish in hidden fields.
-}
asValidID :: String -> Result String
asValidID input =
  asValidNumber input
    >>= useInput input
  where
    useInput xs (_::Int) = return xs

{- |
    Parses as a number.
-}
asValidNumber :: (Num a, Read a, Real a) => String -> Result a
asValidNumber input =
    checkIfEmpty input
    >>= parseAsNum

{- |
    Creates a clean and validated field value.
    White list: Alphabetic characters, Numeric characters, Punctuation characters & Symbol chars, and spaces.
    Size: Up to 200 chars
-}
asValidFieldValue :: String -> Result String
asValidFieldValue input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 200
    >>= checkOnly [isAlphaNum, isPunctuation, isSymbol, (==' ')]

-- | Cleans all HTML from message, and unescapes it. Api V1 accepting
-- HTML, but internally DB holds only pure text. Size: up to 800
-- chars.
asValidInviteText :: String -> Result String
asValidInviteText input =
    checkIfEmpty input
    >>= parseAndFixAsXml
    >>= return . strip . replace "\160" " "
  where
    parseAndFixAsXml :: String -> Result String
    parseAndFixAsXml xs = case parseText def xs' of
      (Right (Document { documentRoot = spanElt }))
         | (T.toLower . nameLocalName . elementName $ spanElt) == "span"
        -> Good . T.unpack . fixElems . elementNodes $ spanElt
      _ -> let xsWithFixedBRs = replace "<BR>" "<BR/>" $ replace "<br>" "<br/>" xs
           in if xsWithFixedBRs /= xs
              then parseAndFixAsXml xsWithFixedBRs
              else Good $ unescapeHTML $ justText xs
      where
        xs' = "<span>" <> TL.pack xs <> "</span>"

    fixElem :: Node -> T.Text
    fixElem (NodeElement (Element "div" _attrs cs)) = addNewline . fixElems $ cs
    fixElem (NodeElement (Element "p"   _attrs cs)) = addNewline . fixElems $ cs
    fixElem (NodeElement (Element "br"  _attrs cs)) = addNewline . fixElems $ cs
    fixElem (NodeElement (Element _name _attrs cs)) =              fixElems $ cs
    fixElem (NodeContent txt)                       = txt
    fixElem _                                       = ""

    addNewline  = (<> "\n")
    fixElems    = mconcat . map fixElem

    justText :: String -> String
    justText ('<':cs) = justText $ drop 1 $ dropWhile (/= '>') cs
    justText (c:cs)   = c : justText cs
    justText []       = []

isValidEmail :: String -> Bool
isValidEmail = isGood . asValidEmail

isValidPhoneForSMS :: String -> Bool
isValidPhoneForSMS = isGood . asValidPhoneForSMS

{- |
    Lower cases everything
-}
mkLowerCase :: String -> Result String
mkLowerCase = return . map toLower

{- |
    Parses a string as a Num
-}
parseAsNum :: (Num a, Read a, Real a) => String -> Result a
parseAsNum  xs =
    case readSigned readDec xs of
        (val,[]):[] -> return val
        _ -> Bad

{- |
    Checks that a string only contains the indicated types of characters.
    When there are invalid chars the flash message depends on what those chars were.
-}
checkOnly :: [Char -> Bool] -> String -> Result String
checkOnly ps str =
    case invalidChars of
        [] | null invalidChars -> return str
        _ | ' ' `elem` invalidChars -> Bad
        _ | null invalidPrintableChars -> Bad
        _ -> Bad
    where invalidChars = filter isInvalidChar str
          isInvalidChar c = all (\p -> not $ p c) ps
          invalidPrintableChars = filter (\c -> isAlphaNum c || isPunctuation c || isSymbol c) invalidChars
{- |
    Checks that a string meets the min length restriction.
-}
checkLengthIsMin :: Int -> String ->  Result String
checkLengthIsMin minlength xs
    | length xs < minlength =  Bad
    | otherwise = return xs

{- |
    Checks that a string doesn't exceed the max length restriction.
-}
checkLengthIsMax :: Int -> String -> Result String
checkLengthIsMax maxlength xs
    | length xs > maxlength =  Bad
    | otherwise = return xs

{- |
    Checks that a string has lenght from list.
-}
checkLengthIs :: [Int] -> String -> Result String
checkLengthIs lengths xs
    | length xs `elem` lengths =  Good xs
    | otherwise = Bad

{- |
    Checks if the input is empty, assigning it value Empty.
-}
checkIfEmpty :: String -> Result String
checkIfEmpty [] = Empty
checkIfEmpty xs = Good xs


filterOutCharacters :: String -> String -> Result String
filterOutCharacters pattern =
    return . filter (\x -> not $ elem x pattern)

{- |
    Strips leading and trailing whitespace
-}
stripWhitespace :: String -> Result String
stripWhitespace =
    return . stripLeadingWhitespace . stripTrailingWhitespace
    where stripLeadingWhitespace = dropWhile isSpace
          stripTrailingWhitespace = reverse . stripLeadingWhitespace . reverse

{- |
    Strips all (even from the middle) whitespace
-}
stripAllWhitespace :: String -> Result String
stripAllWhitespace = return . filter (not . isSpace)

{- |
    Helper to allow input validation via unjson
-}
unjsonWithValidationOrEmpty
    :: (String -> InputValidation.Result String)
    -> UJ.UnjsonDef String
unjsonWithValidationOrEmpty validation =
    UJ.unjsonInvmapR (convertResult . validation) id UJ.unjsonDef
  where
    convertResult (InputValidation.Good s) = return s
    convertResult (InputValidation.Empty)  = return ""
    convertResult (InputValidation.Bad)    = fail "not valid"

{- |
    Helper to allow input validation via unjson
-}
unjsonWithValidationOrEmptyText
    :: (String -> InputValidation.Result String)
    -> UJ.UnjsonDef T.Text
unjsonWithValidationOrEmptyText validation =
    UJ.unjsonInvmapR (convertResult . validation') id UJ.unjsonDef
  where
    convertResult (InputValidation.Good s) = return s
    convertResult (InputValidation.Empty)  = return ""
    convertResult (InputValidation.Bad)    = fail "not valid"
    validation' s = T.pack <$> validation (T.unpack s)
