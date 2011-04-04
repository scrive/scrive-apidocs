{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates
-fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
{- | 
    The input validation rules.
-}
module InputValidation
    ( ValidationMessage
    , Result(..)
    , getOptionalField
    , getOptionalFieldList
    , getDefaultedField
    , getDefaultedFieldList
    , getRequiredField
    , getRequiredFieldList
    , getCriticalField
    , getCriticalFieldList
    , getValidateAndHandle
    , getValidateAndHandleList
    , getAndValidate
    , getAndValidateList
    , logIfBad
    , flashValidationMessage
    , asMaybe
    , withDefault
    , withRequiredFlash
    , withFailure
    , withFailureIfBad
    , asValidEmail
    , asDirtyEmail
    , asValidPassword
    , asDirtyPassword
    , asValidName
    , asValidCompanyName
    , asValidCompanyNumber
    , asValidAddress
    , asValidPosition
    , asValidCheckBox
    , asValidDaysToSign
    , asValidID
    , asValidNumber
    , asValidDocID
    , asValidFieldName
    , asValidFieldValue
    , asValidPlace
    , asValidInviteText) where

import Control.Applicative
import Control.Monad()
import Control.Monad.Error
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import Data.Char
import Data.Int
import Data.Maybe
import Text.Regex.TDFA ((=~))
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Types

import Kontra
import AppLogger as Log (security)
import Misc hiding (getFields)
import Templates.Templates

{- |
    If there's a problem this will create the appropriate FlashMessage.
-}
type ValidationMessage = KontrakcjaTemplates -> IO FlashMessage

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
                | Bad ValidationMessage
                | Empty

instance Monad Result where
  return = Good
  
  Good x >>= f = f x
  Bad msg >>= _ = Bad msg
  Empty >>= _ = Empty

{- |
    Use this to get and validate most of the usual fields.  If the field
    is empty the user won't get told about it, you have to just act sensibly
    given a Nothing.
-}
getOptionalField :: (String -> Result a) -> String -> Kontra (Maybe a)
getOptionalField validate = 
    getValidateAndHandle validate optionalFieldHandler

getOptionalFieldList :: (String -> Result a) -> String -> Kontra [Maybe a]
getOptionalFieldList validate =
    getValidateAndHandleList validate optionalFieldHandler

optionalFieldHandler :: (Input, Result a) -> Kontra (Maybe a)
optionalFieldHandler result =
    logIfBad result
    >>= flashValidationMessage
    >>= asMaybe

{- |
    Use this to get a field that has a default value when Empty.
-}
getDefaultedField :: a -> (String -> Result a) -> String -> Kontra (Maybe a)
getDefaultedField d validate = 
    getValidateAndHandle validate (defaultedFieldHandler d)

getDefaultedFieldList :: a -> (String -> Result a) -> String -> Kontra [Maybe a]
getDefaultedFieldList d validate =
    getValidateAndHandleList validate (defaultedFieldHandler d)

defaultedFieldHandler :: a -> (Input, Result a) -> Kontra (Maybe a)
defaultedFieldHandler d result =
    logIfBad result
    >>= flashValidationMessage
    >>= withDefault d
    >>= asMaybe

{- |
    Use this to get a field that requires input from the user.
    If there is not input then it will display a flash message warning them.
-}
getRequiredField :: (String -> Result a) -> String -> Kontra (Maybe a)
getRequiredField validate = 
    getValidateAndHandle validate requiredFieldHandler

getRequiredFieldList :: (String -> Result a) -> String -> Kontra [Maybe a]
getRequiredFieldList validate =
    getValidateAndHandleList validate requiredFieldHandler

requiredFieldHandler :: (Input, Result a) -> Kontra (Maybe a)
requiredFieldHandler result =
    withRequiredFlash result
    >>= logIfBad
    >>= flashValidationMessage
    >>= asMaybe

{- |
    Use this to get a field that is absolutely required, probably
    because it is hidden.  If this field is missing that means something
    bad has happened.
-}
getCriticalField :: (String -> Result a) -> String -> Kontra a
getCriticalField validate = 
    getValidateAndHandle validate criticalFieldHandler

getCriticalFieldList :: (String -> Result a) -> String -> Kontra [a]
getCriticalFieldList validate =
    getValidateAndHandleList validate criticalFieldHandler

criticalFieldHandler :: (Input, Result a) -> Kontra a
criticalFieldHandler result =
    withRequiredFlash result
    >>= logIfBad
    >>= flashValidationMessage
    >>= withFailure

{- |
    Gets a named field, validates it, and then handles the result.
-}
getValidateAndHandle :: (String -> Result a) -> ((Input, Result a) -> Kontra b) -> String -> Kontra b
getValidateAndHandle validate handle fieldname = do
  result <- getAndValidate validate fieldname
  handle result

getValidateAndHandleList :: (String -> Result a) -> ((Input, Result a) -> Kontra b) -> String -> Kontra [b]
getValidateAndHandleList validate handle fieldname = do
  results <- getAndValidateList validate fieldname
  mapM handle results

{- |
    Gets a named field and validates it.  It will return a pair of the input,
    and output.
-}
getAndValidate :: (String -> Result a) -> String -> Kontra (Input, Result a)
getAndValidate validate fieldname = do
  mrawvalue <- getField fieldname
  case mrawvalue of
    Nothing -> return $ (Nothing, Empty)
    (Just rawvalue) -> return $ (Just rawvalue, validate rawvalue)

getAndValidateList :: (String -> Result a) -> String -> Kontra [(Input, Result a)]
getAndValidateList validate fieldname = do
  rawvalues <- getFields fieldname
  return $ zip (map Just rawvalues) (map validate rawvalues)

getFields :: String -> Kontra [String]
getFields fieldname = do
  values <- getDataFnM $ lookInputList fieldname
  return $ map (BS.toString . concatChunks) values

{- |
    Handles any validation message by adding
    as a flash message.  The result returned is the same
    as the one given, I thought it might be handy to include
    just in case.
-}
flashValidationMessage :: (Input, Result a) -> Kontra (Input, Result a)
flashValidationMessage x@(_, Bad flashmsg) = do
  Context{ctxtemplates,ctxflashmessages} <- get
  msg <- liftIO $ flashmsg ctxtemplates
  when (msg `notElem` ctxflashmessages) $ addFlashMsg msg
  return x
flashValidationMessage x = return x

{- |
    Puts any validation problem in the security log.
    This'll at least mean we've got a record of suspicious
    behaviour.
-}
logIfBad :: (Input, Result a) -> Kontra (Input, Result a)
logIfBad x@(input, Bad flashmsg) = do
  Context{ctxmaybeuser,ctxipnumber,ctxtemplates} <- get
  flash <- liftIO $ flashmsg ctxtemplates
  let username :: String
      username = maybe "unknown" (BS.toString . unEmail . useremail . userinfo) ctxmaybeuser
      logtext = "ip " ++ (show ctxipnumber) ++
               " user " ++ username ++
               " invalid input: " ++
               " flash [" ++ (snd $ unFlashMessage flash) ++ "]" ++
               " raw value [" ++ (show input) ++ "]"
  _ <- liftIO $ Log.security logtext
  return x
logIfBad x = return x

{- |
    Interprets the Result as a Maybe,
    so you get a Just for sensible input,
    but a Nothing for invalid or empty input.
-} 
asMaybe :: (Input, Result a) -> Kontra (Maybe a)
asMaybe (_,Good x) = return $ Just x
asMaybe _        = return Nothing

{- |
    If the result is Empty then this uses the
    given default value.  
-}
withDefault :: a -> (Input, Result a) -> Kontra (Input, Result a)
withDefault d (input, Empty) = return $ (input, Good d)
withDefault _ x     = return x

{- |
    If the result is Empty then this turns it
    into a Bad result with an appropriate flash message.
-}
withRequiredFlash :: (Input, Result a) -> Kontra (Input, Result a)
withRequiredFlash (input, Empty) = return $ (input, Bad flashMessageMissingRequiredField)
withRequiredFlash x     = return x

flashMessageMissingRequiredField :: ValidationMessage
flashMessageMissingRequiredField = 
    flashMessage "flashMessageMissingRequiredField" Nothing

{- |
    You either get a value or a failure,
    that's it.  Which means this'll fail
    for invalid or empty input.
-}
withFailure :: (Input, Result a) -> Kontra a
withFailure (_,Good x) = return x
withFailure _        = mzero

{- |
    You get a failure for bad input.
-}
withFailureIfBad :: (Input, Result a) -> Kontra (Maybe a)
withFailureIfBad (_,Good x) = return $ Just x
withFailureIfBad (_,Bad _) = mzero
withFailureIfBad (_,Empty) = return Nothing

{- |
    Creates a clean and validated email.
    Validating emails is bizarrely hard.  These rules define a subset of what the official rules (RFC) would allow you to have.  This is because the official rules are very liberal, and email providers in practise aren't that liberal.  So this is kind of a, hopefully, practical compromise. So officially “Abc\@def”@example.com is a valid email address, but really if a user put that in, it's got to be wrong!
    Rules:
    * It must contain an @ character
    * Before the @ character there must appear one of more of the following: Full-stop ., Underscore _, Percentage %, Plus +, Hyphen -, ASCII letters a-z and A-Z, digits 0-9, and nordic chars æ, Æ, ä, Ä, ø, Ø, ö, Ö, å, Å
    * After the @ character you must have a full-stop .
    * Between the @ character and the full-stop you must have one of the following: Full-stop ., Hyphen -, ASCII letters a-z and A-Z, digits 0-9.
    * After the full-stop there must be either two, three or four characters and these can be: ASCII letters a-z and A-Z.
    * Size: Up to 200 characters
  Also emails are lowercased.  
-}
asValidEmail :: String -> Result BS.ByteString
asValidEmail input = 
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 200 fieldtemplate
    >>= checkFormat
    >>= mkLowerCase
    >>= mkByteString
    where fieldtemplate = "emailFieldName"
          checkFormat :: String -> Result String
          checkFormat email | isValidFormat email = return email
                            | otherwise = Bad $ flashMessageInvalidFormat fieldtemplate
          isValidFormat :: String -> Bool
          isValidFormat = (=~ "^[æÆäÄøØöÖåÅ[:alnum:]._%+-]+@[[:alnum:].-]+[.][[:alpha:]]{2,4}$")

{- |
    Creates an email that hasn't been completely validated.  It still does handy things
    like trimming whitespace and lowercasing though.  This is useful in situations
    like checking a login.  Because it's dirty make sure you through it away,
    and don't store it.
-}
asDirtyEmail :: String -> Result BS.ByteString
asDirtyEmail input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= mkLowerCase
    >>= mkByteString

{- |
    Creates a clean and validated password.
    White list: Alphabetic characters, Digits [0-9], Punctuation characters, and Symbols (such as $) or spaces (to accommodate people who like to use passphrases)
    Rules: Must contain at least one Alphabetic character, and one Digit [0-9]
    Size: At least 8 chars.  No more than 250 chars.
-}
asValidPassword :: String -> Result BS.ByteString
asValidPassword input =
    checkIfEmpty input
    >>= checkLengthIsMin 8 fieldtemplate
    >>= checkLengthIsMax 250 fieldtemplate
    >>= checkOnly [isAlpha, isDigit, isPunctuation, isSymbol] fieldtemplate
    >>= checkContainsAlphaAndDigit
    >>= mkByteString
    where fieldtemplate = "passwordFieldName"
          checkContainsAlphaAndDigit :: String -> Result String
          checkContainsAlphaAndDigit pwd 
              | any isAlpha pwd && any isDigit pwd = return pwd
              | otherwise = Bad $ flashMessageNeedsLetterAndDigit fieldtemplate

flashMessageNeedsLetterAndDigit :: String -> ValidationMessage
flashMessageNeedsLetterAndDigit fieldtemplate =
    flashMessageWithFieldName fieldtemplate "flashMessageNeedsLetterAndDigit" Nothing

{- |
    Creates a dirty password.  This is useful when validating
    logins for example.  Because it's dirty you should make sure to throw it away,
    and not store it.
-}
asDirtyPassword :: String -> Result BS.ByteString
asDirtyPassword input =
    checkIfEmpty input
    >>= mkByteString

{- |
    Creates a clean and validated name (works for first or second)
    White list: Space, Apostrophe ', Hyphen -, Alphabetic characters
    Size: Up to 100 chars
-}
asValidName :: String -> Result BS.ByteString 
asValidName input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 100 fieldtemplate
    >>= checkOnly (isAlpha : map (==) " \'-") fieldtemplate
    >>= mkByteString
    where fieldtemplate = "nameFieldName"

{- |
    Creates a clean and validated company name.
    White list: Space, Ampersand &, Apostrophe ', At @, Open and close brackets (), Colon :, Comma , Exclamation !, Full-stop ., Hyphen -, Question mark ?, Alphabetic characters, Numeric characters
    Size: Up to 100 chars
-}
asValidCompanyName :: String -> Result BS.ByteString
asValidCompanyName input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 100 fieldtemplate
    >>= checkOnly (isAlphaNum : map (==) " &\'@():,!.-?") fieldtemplate
    >>= mkByteString
    where fieldtemplate = "companyNameFieldName"

{- |
    Creates a clean and validated company or individual number.
    White list: Alphabetic characters, Numeric characters, punctuation 
    Size: From 4 to 50 chars
-}
asValidCompanyNumber :: String -> Result BS.ByteString
asValidCompanyNumber input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMin 4 fieldtemplate
    >>= checkLengthIsMax 50 fieldtemplate
    >>= checkOnly [isDigit, (`elem` ['a'..'z']), (`elem` ['A'..'Z']), (=='-')] fieldtemplate
    >>= mkByteString
    where fieldtemplate = "companyNumberFieldName"     

{- |
    Creates a clean and validated address.
    White list: Space, Apostrophe ', Open and close brackets (), Colon :, Comma , Forward slash //, Full-stop ., Hash #, Hyphen -, Alphabetic characters, Numeric characters
    Size: Up to 200 chars
-}
asValidAddress :: String -> Result BS.ByteString
asValidAddress input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 200 fieldtemplate
    >>= checkOnly (isAlphaNum : map (==) " \'():,/.#-") fieldtemplate
    >>= mkByteString
    where fieldtemplate = "addressFieldName"

{- |
    Creates a clean and validated company position.
    White list: Space, Ampersand &, Apostrophe ', Open and close brackets (), Colon :, Comma , Hyphen -, Alphabetic characters, Numeric characters
    Size: Up to 100 chars
-}
asValidPosition :: String -> Result BS.ByteString
asValidPosition input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 200 fieldtemplate
    >>= checkOnly (isAlphaNum : map (==) " &():,-") fieldtemplate
    >>= mkByteString
    where fieldtemplate = "positionFieldName"

{- |
    Creates a Bool result for a check box, depending on whether it was set to "on" or "off".
-}
asValidCheckBox :: String -> Result Bool
asValidCheckBox input =
    checkIfEmpty input
    >>= mkLowerCase
    >>= parseOnOrOff
    where fieldtemplate = "checkBoxFieldName"
          parseOnOrOff val | val == "on"  = return True
                           | val == "off" = return False
                           | otherwise = Bad $ flashMessageInvalidFormat fieldtemplate

{-|
    Gets the cleaned up number of days to sign.
    This'll make sure you get a number in the range 1-99.
-}
asValidDaysToSign :: String -> Result Int
asValidDaysToSign input = 
    stripWhitespace input
    >>= checkIfEmpty
    >>= parseAsInt fieldtemplate
    >>= checkWithinLowerBound 1 fieldtemplate
    >>= checkWithinUpperBound 99 fieldtemplate
    where fieldtemplate = "daysToSignFieldName"

{- |
    Gets a cleaned up doc id. Useful for validating
    you're not getting fed complete garbage from hidden fields,
    this makes sure the result parses as a Int64.
-}
asValidDocID :: String -> Result Int64
asValidDocID input =
    checkIfEmpty input
    >>= parseAsInt64
    where fieldtemplate = "idFieldName"
          parseAsInt64 :: String -> Result Int64
          parseAsInt64 xs =
            case reads xs of
              (val,[]):[] -> return val
              _ -> Bad $ flashMessageNotAValidInteger fieldtemplate


{- |
    Checks that the input is a valid id, meaning it can be parsed
    as an int.  This is handy if you want to check that you're not
    getting fed rubbish in hidden fields.
-}
asValidID :: String -> Result BS.ByteString
asValidID input =
    checkIfEmpty input
    >>= checkCanParseAsInt
    >>= mkByteString
    where fieldtemplate = "idFieldName"
          checkCanParseAsInt :: String -> Result String
          checkCanParseAsInt xs = 
            case parseAsInt xs fieldtemplate of
              (Good _) -> Good xs
              (Bad msg) -> Bad msg
              Empty -> Empty

{- |
    Parses as a int.
-}
asValidNumber :: String -> Result Int
asValidNumber input =
    checkIfEmpty input
    >>= parseAsInt fieldtemplate
    where fieldtemplate = "idFieldName"

{-|
   Creates a cleaned up place.  Which is just a positive int.
-}
asValidPlace :: String -> Result Int
asValidPlace input =
    checkIfEmpty input
    >>= parseAsInt fieldtemplate
    >>= checkWithinLowerBound 0 fieldtemplate
    where fieldtemplate = "placeFieldName"

{- |
    Creates a clean and validated field name
    White list: Space, Hyphen -, Alphabetic characters, Numeric characters
    Size: Up to 50 chars
-}
asValidFieldName :: String -> Result BS.ByteString
asValidFieldName input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 50 fieldtemplate
    >>= checkOnly (isAlphaNum : map (==) " -") fieldtemplate
    >>= mkByteString
    where fieldtemplate = "fieldNameFieldName"

{- |
    Creates a clean and validated field value.
    White list: Alphabetic characters, Numeric characters, Punctuation characters & Symbol chars, and spaces.
    Size: Up to 200 chars
-}
asValidFieldValue :: String -> Result BS.ByteString
asValidFieldValue input =
    stripWhitespace input
    >>= checkIfEmpty
    >>= checkLengthIsMax 200 fieldtemplate
    >>= checkOnly [isAlphaNum, isPunctuation, isSymbol, (==' ')] fieldtemplate
    >>= mkByteString
    where fieldtemplate = "fieldValueFieldName"

{- |
    Cleans and validates the invite text html.
    Oh goodness!  This one's going to be hard, because the text is actually meant to be html, and will deliberately be displayed as html.  It would be unusual for an email client to execute JavaScript, but there's still ample opportunity for placing any old html in there.  Maybe this would be a good way to do some phishing, for example.  Malicious users could use this to spam people, potentially with quite official looking links designed to trick them into handing over their passwords.
    Rules:
    Must parse as XML
    The only XML tags allowed are: br, p, strong, em, ul, li, ol, and span.
    The only attribute allowed is the style attribute on a span tag.
    This style attribute is only allowed to have the value “text-decoration: underline;” or “text-decoration: line-through;”.
    XML comments are not allowed.
    Size:  up to 800 chars
-}
asValidInviteText :: String -> Result BS.ByteString
asValidInviteText input =
    checkIfEmpty input
    >>= parseAsXml
    >>= checkContent
    >>= ignoreXml input
    >>= mkByteString
    where fieldtemplate = "inviteTextFieldName"
          bad = Bad $ flashMessageInvalidFormat fieldtemplate
          parseAsXml :: String -> Result (Content Posn)
          parseAsXml xs = 
            case xmlParse' "asValidInviteText" $ "<p>" ++ xs ++ "</p>" of
              (Right (Document _ _ root _)) -> return $ CElem root undefined
              _ -> bad
          checkContent :: Content Posn -> Result (Content Posn)
          checkContent x@(CElem (Elem name atts cnt) _)
              | isValidElemName name 
                && all (isValidAtt name) atts 
                && all isValidChild cnt = return x
              | otherwise = bad
          checkContent x@(CString _ _ _) = return x 
          checkContent x@(CRef _ _) = return x
          checkContent _ = bad
          isValidChild :: Content Posn -> Bool
          isValidChild c =
            case checkContent c of
              (Bad _) -> False
              _ -> True
          isValidElemName :: Name -> Bool
          isValidElemName n = n `elem` ["br", "em", "li", "ol", "p", "span", "strong", "ul"]
          isValidAtt :: Name -> Attribute -> Bool
          isValidAtt elemname (attname, attvalue) = 
              "span"==elemname 
              && attname=="style" 
              && isValidAttValue attvalue
          isValidAttValue :: AttValue -> Bool
          isValidAttValue (AttValue (Left v:[])) = 
              v `elem` ["text-decoration: underline;", 
                        "text-decoration: line-through;"]
          isValidAttValue _ = False
          ignoreXml :: String -> Content Posn -> Result String
          ignoreXml c _ = return c
{- |
    Formats strings as ByteStrings
-}
mkByteString :: String -> Result BS.ByteString
mkByteString = return . BS.fromString

{- |
    Lower cases everything
-}
mkLowerCase :: String -> Result String
mkLowerCase = return . map toLower 

{- |
    Checks the lower bound of a number
-}
checkWithinLowerBound :: (Num a, Ord a, Show a) => a -> String -> a -> Result a
checkWithinLowerBound lowerbound fieldtemplate val
    | val>=lowerbound = return val
    | otherwise = Bad $ flashMessageNumberBelowMinimum fieldtemplate lowerbound

flashMessageNumberBelowMinimum :: (Num a, Show a) => String -> a -> ValidationMessage
flashMessageNumberBelowMinimum fieldtemplate lowerbound = 
    flashMessageWithFieldName fieldtemplate "flashMessageNumberBelowMinimum"  . Just $ field "min" (show lowerbound)

{- |
    Checks the upper bound of a number
-}
checkWithinUpperBound :: (Num a, Ord a, Show a) => a -> String -> a -> Result a
checkWithinUpperBound upperbound fieldtemplate val
    | val<=upperbound = return val
    | otherwise = Bad $ flashMessageNumberAboveMaximum fieldtemplate upperbound

flashMessageNumberAboveMaximum :: (Num a, Show a) => String -> a -> ValidationMessage
flashMessageNumberAboveMaximum fieldtemplate upperbound = 
    flashMessageWithFieldName fieldtemplate "flashMessageNumberAboveMaximum" . Just $ field "max" (show upperbound)

{- |
    Parses a string as an int
-}
parseAsInt :: String -> String -> Result Int
parseAsInt fieldtemplate xs =
    case reads xs of
        (val,[]):[] -> return val
        _ -> Bad $ flashMessageNotAValidInteger fieldtemplate

flashMessageNotAValidInteger :: String -> ValidationMessage
flashMessageNotAValidInteger fieldtemplate =
    flashMessageWithFieldName fieldtemplate "flashMessageNotAValidInteger" Nothing

{- |
    Checks that a string only contains the indicated types of characters.
    When there are invalid chars the flash message depends on what those chars were.
-}
checkOnly :: [Char -> Bool] -> String -> String -> Result String
checkOnly ps fieldtemplate str = 
    case invalidChars of
        [] | null invalidChars -> return str
        _ | ' ' `elem` invalidChars -> Bad $ flashMessageInvalidSpaceInInput fieldtemplate
        _ | null invalidPrintableChars -> Bad $ flashMessageInvalidUnprintableCharsInInput fieldtemplate
        _ -> Bad $ flashMessageInvalidPrintableCharsInInput fieldtemplate invalidPrintableChars
    where invalidChars = filter isInvalidChar str
          isInvalidChar c = all (\p -> not $ p c) ps
          invalidPrintableChars = filter (\c -> isAlphaNum c || isPunctuation c || isSymbol c) invalidChars

flashMessageInvalidSpaceInInput :: String -> ValidationMessage
flashMessageInvalidSpaceInInput fieldtemplate =
    flashMessageWithFieldName fieldtemplate "flashMessageInvalidSpaceInInput" Nothing

flashMessageInvalidUnprintableCharsInInput :: String -> ValidationMessage
flashMessageInvalidUnprintableCharsInInput fieldtemplate = 
    flashMessageWithFieldName fieldtemplate "flashMessageInvalidUnprintableCharsInInput" Nothing

flashMessageInvalidPrintableCharsInInput :: String -> String -> ValidationMessage
flashMessageInvalidPrintableCharsInInput fieldtemplate invalidchars = 
    flashMessageWithFieldName fieldtemplate "flashMessageInvalidPrintableCharsInInput" . Just $ field "invalidchar" (head invalidchars)

{- |
    Checks that a string meets the min length restriction.
-}
checkLengthIsMin :: Int -> String -> String -> Result String
checkLengthIsMin minlength fieldtemplate xs 
    | length xs < minlength =  Bad $ flashMessageInputLessThanMinLength fieldtemplate minlength
    | otherwise = return xs

flashMessageInputLessThanMinLength :: String -> Int -> ValidationMessage
flashMessageInputLessThanMinLength fieldtemplate minlength = 
    flashMessageWithFieldName fieldtemplate "flashMessageInputLessThanMinLength" . Just $ field "minlength" minlength

{- |
    Checks that a string doesn't exceed the max length restriction.
-}
checkLengthIsMax :: Int -> String -> String -> Result String
checkLengthIsMax maxlength fieldtemplate xs 
    | length xs > maxlength =  Bad $ flashMessageInputExceedsMaxLength fieldtemplate maxlength
    | otherwise = return xs

flashMessageInputExceedsMaxLength :: String -> Int -> ValidationMessage
flashMessageInputExceedsMaxLength fieldtemplate maxlength = 
    flashMessageWithFieldName fieldtemplate "flashMessageInputExceedsMaxLength" . Just $ field "maxlength" maxlength

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

{- |
    This isn't very specific, so try not to use!
-}
flashMessageInvalidFormat :: String -> ValidationMessage
flashMessageInvalidFormat fieldtemplate =
    flashMessageWithFieldName fieldtemplate "flashMessageInvalidFormat" Nothing

flashMessageWithFieldName :: String -> String -> Maybe Fields -> ValidationMessage
flashMessageWithFieldName fieldtemplate templatename mfields templates = do
   fieldname <- renderTemplate templates fieldtemplate ()
   let fields = do
       when (isJust mfields) (fromJust mfields)
       field "fieldnametitlecase" (titleCase fieldname)
       field "fieldnamelowercase" (lowerCase fieldname)
   flashMessage templatename (Just fields) templates
   where titleCase (x:xs) = toUpper x : lowerCase xs
         titleCase [] = []
         lowerCase = map toLower

flashMessage :: String -> Maybe Fields -> ValidationMessage
flashMessage templatename mfields templates =
    toFlashMsg OperationFailed <$>
      renderTemplate templates templatename
        (when (isJust mfields) (fromJust mfields))
