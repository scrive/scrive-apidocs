{- |
    The input validation rules.
-}
module InputValidation
    ( Result(..)
    , isGood
    , isBad
    , isEmpty
    , resultToMaybe
    , getOptionalField
    , getDefaultedField
    , getCriticalField
    , asValidEmail
    , asDirtyEmail
    , asValidPassword
    , asDirtyPassword
    , asValidName
    , asValidCompanyName
    , asValidCompanyNumber
    , asValidSEBankIdPersonalNumber
    , asValidAddress
    , asValidPhone
    , asValidPhoneForSMS
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
) where

import Data.Char
import Data.String.Utils
import Log
import Numeric
import Text.HTML.TagSoup.Entity
import Text.Regex.TDFA ((=~))
import Text.XML.HaXml(render)
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty(content)
import Text.XML.HaXml.Types

import Attachment.AttachmentID
import Doc.DocumentID
import Happstack.Fields hiding (getFields)
import IPAddress
import Kontra
import KontraPrelude
import User.Email
import User.Model
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

instance Monoid a => Monoid (Result a) where
    mappend (Good a1) (Good a2) = Good $ mappend a1 a2
    mappend Empty a = a
    mappend a Empty = a
    mappend Bad _ = Bad
    mappend _ Bad  = Bad
    mempty = Empty

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
    because it is hidden.  If this field is missing that means something
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
  Context{ctxmaybeuser, ctxipnumber} <- getContext
  logInfo "Input validation failed" $ object [
      "ip" .= show ctxipnumber
    , "user" .= maybe "unknown" (unEmail . useremail . userinfo) ctxmaybeuser
    , "input" .= show input
    ]
  return x
logIfBad x = return x

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
    >>= checkFormat
    >>= mkLowerCase
    where
          checkFormat :: String -> Result String
          checkFormat email | isValidFormat email = return email
                            | otherwise = Bad
          isValidFormat :: String -> Bool
          isValidFormat = (=~ ("^[[:alnum:]._%+-]+@[[:alnum:].-]+[.][[:alpha:]]{2,}$":: String))

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
    Creates a clean and validated password.
    White list: Alphabetic characters, Digits [0-9], Punctuation characters, and Symbols (such as $) or spaces (to accommodate people who like to use passphrases)
    Rules: Must contain at least 2 Alphabetic characters, and 2 Digits [0-9]
    Size: At least 8 chars.  No more than 250 chars.
-}
asValidPassword :: String -> Result String
asValidPassword input =
    checkIfEmpty input
    >>= checkLengthIsMin 8
    >>= checkLengthIsMax 250
    >>= checkOnly [isAlpha, isDigit, isPunctuation, isSymbol]
    >>= checkContains1AlphaAndDigit
    where
          checkContains1AlphaAndDigit :: String -> Result String
          checkContains1AlphaAndDigit pwd
              | length (filter isAlpha pwd) >= 1 && length (filter isDigit pwd) >=1 = return pwd
              | otherwise = Bad


{- |
    Creates a dirty password.  This is useful when validating
    logins for example.  Because it's dirty you should make sure to throw it away,
    and not store it.
-}
asDirtyPassword :: String -> Result String
asDirtyPassword input = checkIfEmpty input

{- |
    Creates a clean and validated name (works for first or second)
    White list: Space, Apostrophe ', Hyphen -, Alphabetic characters
    Size: Up to 100 chars
-}
asValidName :: String -> Result String
asValidName input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 100
    >>= checkOnly (isAlpha : map (==) " \'-")


{- |
    Creates a clean and validated company name.
    White list: Space, Ampersand &, Apostrophe \', At \@, Open and close brackets (), Colon :, Comma , Exclamation !, Full-stop ., Hyphen -, Question mark ?, Alphabetic characters, Numeric characters
    Size: Up to 100 chars
-}
asValidCompanyName :: String -> Result String
asValidCompanyName input =
    (Good $ map (\ch -> if ch == '\t' then ' ' else ch) input)
    >>= stripWhitespace
    >>= checkIfEmpty
    >>= checkLengthIsMax 100
    >>= checkOnly (isAlphaNum : map (==) " &\'@():,!.-?")

{- |
    Creates a clean and validated company number.
    White list: Alphabetic characters, Numeric characters, punctuation
    Size: From 4 to 50 chars
-}
asValidCompanyNumber :: String -> Result String
asValidCompanyNumber input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 50
    >>= checkOnly [isDigit, (`elem` ['a'..'z']), (`elem` ['A'..'Z']), (=='-')]

{- |
    Creates a clean and validated personal number.
    White list: Digits, hyphens (to be stripped)
    Size: 10 or 12
-}
asValidSEBankIdPersonalNumber :: String -> Result String
asValidSEBankIdPersonalNumber input =
    stripAllWhitespace input
    >>= filterOutCharacters "-"
    >>= checkOnly [isDigit]
    >>= (\xs -> if
             | length xs `elem` [10, 12] -> return xs
             | otherwise -> Bad)

{- |
    Creates a clean and validated address.
    White list: Space, Apostrophe ', Open and close brackets (), Colon :, Comma , Forward slash //, Full-stop ., Hash #, Hyphen -, Alphabetic characters, Numeric characters
    Size: Up to 200 chars
-}
asValidAddress :: String -> Result String
asValidAddress input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 200
    >>= checkOnly (isAlphaNum : map (==) " \'():,/.#-")

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
-}
asValidPosition :: String -> Result String
asValidPosition input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 200
    >>= checkOnly (isAlphaNum : map (==) " &():,-")

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
    No idea how this validator should look like
-}
asValidPhone :: String -> Result String
asValidPhone input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 100
    >>= checkOnly (isAlphaNum : map (==) " +-()")

asValidPhoneForSMS :: String -> Result String
asValidPhoneForSMS input =
    filterOutCharacters " -()." input
    >>= checkIfEmpty
    >>= checkLengthIsMax 20
    >>= checkLengthIsMin 6
    >>= checkOnly (isDigit : map (==) "+")
    >>= (\str -> if take 1 str == "+"
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

{- |
    Cleans all HTML from message, and unescapes it. Api V1 accepting HTML, but internally DB holds only pure text.
    Size:  up to 800 chars
-}
asValidInviteText :: String -> Result String
asValidInviteText input =
    checkIfEmpty input
    >>= parseAndFixAsXml
    >>= return . strip . replace "\160" " "
    where
          parseAndFixAsXml :: String -> Result String
          parseAndFixAsXml xs =
            case xmlParse' "asValidInviteText" $ "<span>" ++ xs ++ "</span>" of
              (Right (Document _ _ (Elem _ _ cs) _)) -> Good $ (concatMap fixContent cs)
              _ -> let xsWithFixedBRs = replace "<BR>" "<BR/>" $ replace "<br>" "<br/>" xs
                   in if xsWithFixedBRs /= xs
                         then parseAndFixAsXml xsWithFixedBRs
                         else Good $ unescapeHTML $ justText xs
          fixContent :: Content Posn -> String
          fixContent (CElem (Elem (N "div") _ cs) _) = (concatMap fixContent cs) ++ " \n"
          fixContent (CElem (Elem (N "p") _ cs) _)   = (concatMap fixContent cs) ++ " \n"
          fixContent (CElem (Elem (N "br") _ cs) _)  = (concatMap fixContent cs) ++ " \n"
          fixContent (CElem (Elem (N _) _ cs) _)     = (concatMap fixContent cs)
          fixContent x@(CString _ _ _)               = render $ content x
          fixContent (CRef (RefEntity ent) _)        = fromMaybe "" $ lookupEntity ent
          fixContent (CRef (RefChar i) _)            = [chr i]
          fixContent _ = ""
          justText ('<':cs) = justText $ drop 1 $ dropWhile (/= '>') cs
          justText (c:cs) = c : justText cs
          justText [] = []


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
