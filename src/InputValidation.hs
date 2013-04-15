{- |
    The input validation rules.
-}
module InputValidation
    ( Result(..)
    , isGood
    , isBad
    , isEmpty
    , checkIfEmpty
    , getOptionalField
    , getOptionalFieldList
    , getDefaultedField
    , getDefaultedFieldList
    , getRequiredField
    , getCriticalField
    , getCriticalFieldList
    , getValidateAndHandle
    , getValidateAndHandleList
    , getAndValidate
    , getAndValidateList
    , withDefault
    , withRequiredFlash
    , withFailure
    , asValidEmail
    , asDirtyEmail
    , asValidPassword
    , asDirtyPassword
    , asValidName
    , asValidCompanyName
    , asValidCompanyNumber
    , asValidAddress
    , asValidPhone
    , asValidPosition
    , asValidCheckBox
    , asValidID
    , asValidNumber
    , asValidDocID
    , asValidAttachmentID
    , asValidUserID
    , asValidFieldValue
    , asValidInviteText
    , asValidIPAddressWithMaskList
) where

import Control.Applicative
import Control.Monad()
import Control.Monad.Error
import qualified Data.ByteString.Lazy.UTF8 as BSL
import Data.Char
import Text.Regex.TDFA ((=~))
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Types

import Kontra
import Doc.DocumentID
import Attachment.AttachmentID
import Numeric
import IPAddress
import qualified Log (security)
import Happstack.Fields hiding (getFields)
import User.Model
import Data.Monoid

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

instance Monoid a => Monoid (Result a) where
    mappend (Good a1) (Good a2) = Good $ mappend a1 a2
    mappend Empty a = a
    mappend a Empty = a
    mappend Bad _ = Bad
    mappend _ Bad  = Bad
    mempty = Empty

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

getOptionalFieldList :: Kontrakcja m => (String -> Result a) -> String -> m [Maybe a]
getOptionalFieldList validate =
    getValidateAndHandleList validate optionalFieldHandler

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

getDefaultedFieldList :: Kontrakcja m => a -> (String -> Result a) -> String -> m [Maybe a]
getDefaultedFieldList d validate =
    getValidateAndHandleList validate (defaultedFieldHandler d)

defaultedFieldHandler :: Kontrakcja m => a -> (Input, Result a) -> m (Maybe a)
defaultedFieldHandler d result =
    logIfBad result
    >>= withDefault d
    >>= asMaybe

{- |
    Use this to get a field that requires input from the user.
    If there is not input then it will display a flash message warning them.
-}
getRequiredField :: Kontrakcja m => (String -> Result a) -> String -> m (Maybe a)
getRequiredField validate =
    getValidateAndHandle validate requiredFieldHandler

requiredFieldHandler :: Kontrakcja m => (Input, Result a) -> m (Maybe a)
requiredFieldHandler result =
    withRequiredFlash result
    >>= logIfBad
    >>= asMaybe

{- |
    Use this to get a field that is absolutely required, probably
    because it is hidden.  If this field is missing that means something
    bad has happened.
-}
getCriticalField :: Kontrakcja m => (String -> Result a) -> String -> m a
getCriticalField validate =
    getValidateAndHandle validate criticalFieldHandler

getCriticalFieldList :: Kontrakcja m => (String -> Result a) -> String -> m [a]
getCriticalFieldList validate =
    getValidateAndHandleList validate criticalFieldHandler

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

getValidateAndHandleList :: Kontrakcja m => (String -> Result a) -> ((Input, Result a) -> m b) -> String -> m [b]
getValidateAndHandleList validate handle fieldname = do
  results <- getAndValidateList validate fieldname
  mapM handle results

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

getAndValidateList :: Kontrakcja m => (String -> Result a) -> String -> m [(Input, Result a)]
getAndValidateList validate fieldname = do
  rawvalues <- map BSL.toString <$> getDataFnM (lookInputList fieldname)
  return $ zip (map Just rawvalues) (map validate rawvalues)


{- |
    Puts any validation problem in the security log.
    This'll at least mean we've got a record of suspicious
    behaviour.
-}
logIfBad :: Kontrakcja m => (Input, Result a) -> m (Input, Result a)
logIfBad x@(input, Bad) = do
  Context{ctxmaybeuser, ctxipnumber} <- getContext
  let username :: String
      username = maybe "unknown" (unEmail . useremail . userinfo) ctxmaybeuser
      logtext = "ip " ++ show ctxipnumber ++
               " user " ++ username ++
               " invalid input: " ++
               " raw value info [" ++ show input ++ "]"
  _ <- liftIO $ Log.security logtext
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
--  * After the full-stop there must be either two, three or four
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
          isValidFormat = (=~ ("^[[:alnum:]._%+-]+@[[:alnum:].-]+[.][[:alpha:]]{2,4}$":: String))

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
    Creates a clean and validated company or individual number.
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
    Cleans and validates the invite text html.
    Oh goodness!  This one's going to be hard, because the text is actually meant to be html, and will deliberately be displayed as html.  It would be unusual for an email client to execute JavaScript, but there's still ample opportunity for placing any old html in there.  Maybe this would be a good way to do some phishing, for example.  Malicious users could use this to spam people, potentially with quite official looking links designed to trick them into handing over their passwords.
    Rules:
    Must parse as XML
    The only XML tags allowed are: br, p, strong, em, ul, li, ol, and span.
    The only attribute allowed is the style attribute on a span tag.
    This style attribute is only allowed to have the value “text-decoration: underline;” or “text-decoration: line-through;”.
    XML comments are allowed, because you tend to get from tinymce when people paste from Word.  I can get tinymce to
    filter all the tags nicely, but have failed here.  When I have time I'll either figure it out, or filter here.
    Size:  up to 800 chars
-}
asValidInviteText :: String -> Result String
asValidInviteText input =
    checkIfEmpty input
    >>= parseAsXml
    >>= checkContent
    >>= ignoreXml input
    where
          parseAsXml :: String -> Result (Content Posn)
          parseAsXml xs =
            case xmlParse' "asValidInviteText" $ "<p>" ++ xs ++ "</p>" of
              (Right (Document _ _ root _)) -> return $ CElem root undefined
              _ -> Bad
          checkContent :: Content Posn -> Result (Content Posn)
          checkContent x@(CElem (Elem (N name) atts cnt) _)
              | isValidElemName name
                && all (isValidAtt name) atts
                && all isValidChild cnt = return x
              | otherwise = Bad
          checkContent x@(CString _ _ _) = return x
          checkContent x@(CRef _ _) = return x
          checkContent x@(CMisc (Comment _) _) = return x
          checkContent _ = Bad
          isValidChild :: Content Posn -> Bool
          isValidChild c = not $ isBad $ checkContent c
          isValidElemName :: Name -> Bool
          isValidElemName n = n `elem` ["br", "em", "li", "ol", "p", "span", "strong", "ul"]
          isValidAtt :: Name -> Attribute -> Bool
          isValidAtt elemname (attname, attvalue) =
              "span"==elemname
              && attname==N "style"
              && isValidAttValue attvalue
          isValidAttValue :: AttValue -> Bool
          isValidAttValue (AttValue (Left v:[])) =
              v `elem` ["text-decoration: underline;",
                        "text-decoration: line-through;"]
          isValidAttValue _ = False
          ignoreXml :: String -> Content Posn -> Result String
          ignoreXml c _ = return c

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

{- |
    Strips leading and trailing whitespace
-}
stripWhitespace :: String -> Result String
stripWhitespace =
    return . stripLeadingWhitespace . stripTrailingWhitespace
    where stripLeadingWhitespace = dropWhile isSpace
          stripTrailingWhitespace = reverse . stripLeadingWhitespace . reverse

