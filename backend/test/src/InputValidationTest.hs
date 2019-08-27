module InputValidationTest (inputValidationTests) where

import Data.Char
import Data.Int
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assert)
import Test.QuickCheck ((==>), Arbitrary(..), Property, oneof, property)
import Test.QuickCheck.Instances.Text ()
import qualified Data.Text as T
import qualified Data.Text.ICU as Rx

import InputValidation
import TestingUtil (ArbitraryUnicode(..))
import TestKontra

inputValidationTests :: TestEnvSt -> Test
inputValidationTests _ = testGroup "InputValidation"
    [
      testGroup "asValidEmail"
        [ testCase "bad examples fail" testValidEmailExampleFails
        , testCase "good examples pass" testValidEmailExamplePasses
        , testCase "lower cases" testValidEmailLowercases
        , testCase "strips surrounding whitespace" testValidEmailStripsWhitespace
        , testCase "null is counted as empty" testValidEmailNullIsEmpty
        , testProperty "whitespace only is counted as empty" propValidEmailWhitespaceIsEmpty ]
    , testGroup "asDirtyEmail"
        [ testProperty "lower cases" $
            propDirtyEmailLowercases . withArbitraryUnicode
        , testProperty "strips surrounding whitespace" $
            propDirtyEmailStripsWhitespace
        , testCase "null is counted as empty" testDirtyEmailNullIsEmpty
        , testProperty "whitespace only is counted as empty" propDirtyEmailWhitespaceIsEmpty ]
    , testGroup "asDirtyPassword"
        [ testCase "null is counted as empty" testDirtyPasswordNullIsEmpty ]
    , testGroup "asValidName"
        [ testProperty "strips surrounding whitespace" propValidNameStripsWhitespace
        , testCase "null is counted as empty" testValidNameNullIsEmpty
        , testProperty "whitespace only is counted as empty" propValidNameWhitespaceIsEmpty
        , testProperty "good examples pass" propValidNameGoodExamples ]
    , testGroup "asValidCompanyName"
        [ testProperty "strips surrounding whitespace" propValidCompanyNameStripsWhitespace
        , testCase "null is counted as empty" testValidCompanyNameNullIsEmpty
        , testProperty "whitespace only is counted as empty" propValidCompanyNameWhitespaceIsEmpty
        , testProperty "good examples pass" propValidCompanyNameGoodExamples ]
    , testGroup "asValidCompanyNumber"
        [ testProperty "strips surrounding whitespace" propValidCompanyNumberStripsWhitespace
        , testCase "null is counted as empty" testValidCompanyNumberNullIsEmpty
        , testProperty "whitespace only is counted as empty" $
            propValidCompanyNumberWhitespaceIsEmpty
        , testProperty "can only contain hyphen, digits [0-9] or letters" $
            propValidCompanyNumberRestrictsChars . withArbitraryUnicode
        , testProperty "good examples pass" propValidCompanyNumberGoodExamples ]
    , testGroup "asValidAddress"
        [ testProperty "strips surrounding whitespace" propValidAddressStripsWhitespace
        , testCase "null is counted as empty" testValidAddressNullIsEmpty
        , testProperty "whitespace only is counted as empty" propValidAddressWhitespaceIsEmpty
        , testProperty "can only contain alphanumeric, spaces and chars '():,/.#-"$
            propValidAddressRestrictsChars . withArbitraryUnicode
        , testProperty "good examples pass" propValidAddressGoodExamples ]
    , testGroup "asValidPosition"
        [ testProperty "strips surrounding whitespace" propValidPositionStripsWhitespace
        , testCase "null is counted as empty" testValidPositionNullIsEmpty
        , testProperty "whitespace only is counted as empty" propValidPositionWhitespaceIsEmpty
        , testProperty "can only contain alphanumeric, spaces and chars &'():,-" $
            propValidPositionRestrictsChars . withArbitraryUnicode
        , testProperty "good examples pass" propValidPositionGoodExamples ]
    , testGroup "asValidCheckBox"
        [ testCase "on/ON is true" testValidCheckBoxOnIsTrue
        , testCase "off/OFF is true" testValidCheckBoxOffIsFalse
        , testCase "null is counted as empty" testValidCheckBoxNullIsEmpty
        , testCase "not on/off is fail" testValidCheckBoxBadIfNotOnOrOff ]
    , testGroup "asValidDocID"
        [ testCase "null is counted as empty" testValidDocIDNullIsEmpty
        --, testProperty "must be an int64" propValidDocIDMustBeInt64
        , testProperty "good examples pass" propValidDocIDGoodExamples ]
    , testGroup "asValidID"
        [ testCase "null is counted as empty" testValidIDNullIsEmpty
        --, testProperty "must be an int" propValidIDMustBeInt
        , testProperty "good examples pass" propValidIDGoodExamples ]
    , testGroup "asValidFieldValue"
        [ testProperty "strips surrounding whitespace" propValidFieldValueStripsWhitespace
        , testCase "null is counted as empty" testValidFieldValueNullIsEmpty
        , testProperty "whitespace only is counted as empty" propValidFieldValueWhitespaceIsEmpty
        , testProperty "can only contain alphanumeric, punctuation or symbol or space" $
            propValidFieldValueRestrictsChars . withArbitraryUnicode
        , testProperty "good examples pass" propValidFieldValueGoodExamples ]
    , testGroup "asValidInviteText"
        [ testCase "null is counted as empty" testValidInviteTextNullIsEmpty
        , testCase "bad examples are fixed" testValidInviteTextBadExamplesAreFixed
        , testCase "good examples pass" testValidInviteTextGoodExamples ]
    ]

testValidEmailExampleFails :: Assertion
testValidEmailExampleFails = do
    let results = map asValidEmail
                  [ "@aaa.com"
                  , "a£@aaa.com"
                  , "aaa.cOm"
                  , "a@aaA_.com"
                  , "a@.com"
                  , "a@abC."
                  , "a@Abc._om"
                  , "a@abc.a"
                  , "a@abc.a2"
                  , "12@122@sdfsw@"
                  , "żółw@a.com"
                  , "ok@ok.nótók"
                  ]
    assert $ all isBad results

goodEmailExamples :: [Text]
goodEmailExamples = [ "1a2B3_4%5+6.7-zZ@abc.com"
                    , "abc@1a2B3c4.5-.com"
                    , "abc@ABC.com"
                    , "ABC@ABC.XYZ"
                    , "abc@abc.au"
                    , "abc@abc.abcd"
                    , "abc@abc.travel"
                    , "ok@ók.ok"
                    ]

testValidEmailExamplePasses :: Assertion
testValidEmailExamplePasses = do
    let results = map asValidEmail goodEmailExamples
    assert $ all isGood results

testValidEmailLowercases :: Assertion
testValidEmailLowercases = do
    let results = map asValidEmail goodEmailExamples
    assert $ all (isLowerCase . fromGood) results

testValidEmailStripsWhitespace :: Assertion
testValidEmailStripsWhitespace = do
    let results = map (asValidEmail . surroundWithWhitespace) goodEmailExamples
    assert $ all (not . isWhitespace . fromGood) results
    where surroundWithWhitespace :: Text -> Text
          surroundWithWhitespace xs = "\t\n " <> xs <> "\n\t "

testValidEmailNullIsEmpty :: Assertion
testValidEmailNullIsEmpty = testNullIsEmpty asDirtyEmail

propValidEmailWhitespaceIsEmpty :: [WhitespaceChar] -> Property
propValidEmailWhitespaceIsEmpty = propWhitespaceIsEmpty asValidEmail

propDirtyEmailLowercases :: Text -> Property
propDirtyEmailLowercases xs =
    (not (isLowerCase xs) && not (isEmptyInput xs))
    ==> (isLowerCase . fromGood . asDirtyEmail $ xs)

propDirtyEmailStripsWhitespace :: [WhitespaceChar] -> ArbitraryUnicode -> Property
propDirtyEmailStripsWhitespace ws s = propStripWhitespace asDirtyEmail ws $ withArbitraryUnicode s

testDirtyEmailNullIsEmpty :: Assertion
testDirtyEmailNullIsEmpty = testNullIsEmpty asDirtyEmail

propDirtyEmailWhitespaceIsEmpty :: [WhitespaceChar] -> Property
propDirtyEmailWhitespaceIsEmpty = propWhitespaceIsEmpty asDirtyEmail


newtype PasswordChar = PasswordChar { pc :: Char } deriving Show

instance Arbitrary PasswordChar where
    arbitrary = oneof . map (return . PasswordChar) $ "aAż*$12"

testDirtyPasswordNullIsEmpty :: Assertion
testDirtyPasswordNullIsEmpty = testNullIsEmpty asDirtyPassword

propValidNameStripsWhitespace :: [WhitespaceChar] -> [NameChar] -> Property
propValidNameStripsWhitespace ws ns =
    let xs = map nc ns in
    propStripWhitespace asValidName ws (T.pack xs)

testValidNameNullIsEmpty :: Assertion
testValidNameNullIsEmpty = testNullIsEmpty asValidName

propValidNameWhitespaceIsEmpty :: [WhitespaceChar] -> Property
propValidNameWhitespaceIsEmpty = propWhitespaceIsEmpty asValidName

propValidNameGoodExamples :: [NameChar] -> Property
propValidNameGoodExamples ns =
    let xs = map nc ns in
    length xs > 0
      && length xs < 50
      && not (isWhitespace $ T.pack xs)
     ==> isGood $ asValidName $ T.pack xs

newtype NameChar = NameChar { nc :: Char } deriving Show

instance Arbitrary NameChar where
    arbitrary = oneof . map (return . NameChar) $ "aAż 2-"

propValidCompanyNameStripsWhitespace :: [WhitespaceChar] -> [CompanyNameChar] -> Property
propValidCompanyNameStripsWhitespace ws ns =
    let xs = map cnc ns in
    propStripWhitespace asValidCompanyName ws $ T.pack xs

testValidCompanyNameNullIsEmpty :: Assertion
testValidCompanyNameNullIsEmpty = testNullIsEmpty asValidCompanyName

propValidCompanyNameWhitespaceIsEmpty :: [WhitespaceChar] -> Property
propValidCompanyNameWhitespaceIsEmpty = propWhitespaceIsEmpty asValidCompanyName

propValidCompanyNameGoodExamples :: [CompanyNameChar] -> Property
propValidCompanyNameGoodExamples ns =
    let xs = map cnc ns in
    length xs > 0
      && length xs < 100
      && not (isWhitespace $ T.pack xs)
     ==> isGood $ asValidCompanyName $ T.pack xs

newtype CompanyNameChar = CompanyNameChar { cnc :: Char } deriving Show

instance Arbitrary CompanyNameChar where
    arbitrary = oneof . map (return . CompanyNameChar) $ "aAż29 &\'@():,!.-?"

propValidCompanyNumberStripsWhitespace :: [WhitespaceChar] -> [CompanyNumberChar] -> Property
propValidCompanyNumberStripsWhitespace ws ns =
    let xs = map cn ns in
    propStripWhitespace asValidCompanyNumber ws $ T.pack xs

testValidCompanyNumberNullIsEmpty :: Assertion
testValidCompanyNumberNullIsEmpty = testNullIsEmpty asValidCompanyNumber

propValidCompanyNumberWhitespaceIsEmpty :: [WhitespaceChar] -> Property
propValidCompanyNumberWhitespaceIsEmpty = propWhitespaceIsEmpty asValidCompanyNumber

propValidCompanyNumberRestrictsChars :: Text -> Property
propValidCompanyNumberRestrictsChars =
   propJustAllowed asValidCompanyNumber [isUnicodeChar, isAlphaNum, isDigit, (`elem` ['-', ' '])]

propValidCompanyNumberGoodExamples :: [CompanyNumberChar] -> Property
propValidCompanyNumberGoodExamples ns =
    let xs = map cn ns in
    length xs > 4
      && length xs < 15
      && not (isWhitespace $ T.pack xs)
     ==> isGood $ asValidCompanyNumber $ T.pack xs

newtype CompanyNumberChar = CompanyNumberChar { cn :: Char } deriving Show

instance Arbitrary CompanyNumberChar where
    arbitrary = oneof . map (return . CompanyNumberChar) $ "aA12-"

propValidAddressStripsWhitespace :: [WhitespaceChar] -> [AddressChar] -> Property
propValidAddressStripsWhitespace ws as =
    let xs = map ac as in
    propStripWhitespace asValidAddress ws $ T.pack xs

testValidAddressNullIsEmpty :: Assertion
testValidAddressNullIsEmpty = testNullIsEmpty asValidAddress

propValidAddressWhitespaceIsEmpty :: [WhitespaceChar] -> Property
propValidAddressWhitespaceIsEmpty = propWhitespaceIsEmpty asValidAddress

propValidAddressRestrictsChars :: Text -> Property
propValidAddressRestrictsChars =
   propJustAllowed asValidAddress [isUnicodeChar, isAlphaNum, (`elem` (' ': " '():,/.#-"))]

propValidAddressGoodExamples :: [AddressChar] -> Property
propValidAddressGoodExamples as =
    let xs = map ac as in
    length xs > 0
      && length xs < 100
      && not (isWhitespace $ T.pack xs)
     ==> isGood $ asValidAddress $ T.pack xs

newtype AddressChar = AddressChar { ac :: Char } deriving Show

instance Arbitrary AddressChar where
    arbitrary = oneof . map (return . AddressChar) $ "aAż29 \'():,/.#-"

propValidPositionStripsWhitespace :: [WhitespaceChar] -> [PositionChar] -> Property
propValidPositionStripsWhitespace ws ps =
    let xs = map pnc ps in
    propStripWhitespace asValidPosition ws $ T.pack xs

testValidPositionNullIsEmpty :: Assertion
testValidPositionNullIsEmpty = testNullIsEmpty asValidPosition

propValidPositionWhitespaceIsEmpty :: [WhitespaceChar] -> Property
propValidPositionWhitespaceIsEmpty = propWhitespaceIsEmpty asValidPosition

propValidPositionRestrictsChars :: Text -> Property
propValidPositionRestrictsChars =
   propJustAllowed asValidPosition (isAlpha : isDigit : map (==) " &'():,-")

propValidPositionGoodExamples :: [PositionChar] -> Property
propValidPositionGoodExamples ps =
    let xs = map pnc ps in
    length xs > 0
      && length xs < 200
      && not (isWhitespace $ T.pack xs)
     ==> isGood $ asValidPosition $ T.pack xs

newtype PositionChar = PositionChar { pnc :: Char } deriving Show

instance Arbitrary PositionChar where
    arbitrary = oneof . map (return . PositionChar) $ "aAż29 &():,-"

testValidCheckBoxOnIsTrue :: Assertion
testValidCheckBoxOnIsTrue = do
    assert . fromGood . asValidCheckBox $ "on"
    assert . fromGood . asValidCheckBox $ "ON"

testValidCheckBoxOffIsFalse :: Assertion
testValidCheckBoxOffIsFalse = do
    assert . not . fromGood . asValidCheckBox $ "off"
    assert . not . fromGood . asValidCheckBox $ "OFF"

testValidCheckBoxNullIsEmpty :: Assertion
testValidCheckBoxNullIsEmpty = testNullIsEmpty asValidCheckBox

testValidCheckBoxBadIfNotOnOrOff :: Assertion
testValidCheckBoxBadIfNotOnOrOff = do
    assert . isBad . asValidCheckBox $ " on "
    assert . isBad . asValidCheckBox $ " off "
    assert . isBad . asValidCheckBox $ "other"
    assert . isBad . asValidCheckBox $ " "

testValidDocIDNullIsEmpty :: Assertion
testValidDocIDNullIsEmpty = testNullIsEmpty asValidDocID

propValidDocIDGoodExamples :: Int64 -> Property
propValidDocIDGoodExamples n =
    True ==> isGood . asValidDocID $ showt n

testValidIDNullIsEmpty :: Assertion
testValidIDNullIsEmpty = testNullIsEmpty asValidID

propValidIDGoodExamples :: Int -> Property
propValidIDGoodExamples n = property (isGood . asValidID $ showt n)

propValidFieldValueStripsWhitespace :: [WhitespaceChar] -> [FieldValueChar] -> Property
propValidFieldValueStripsWhitespace ws fs =
    let xs = map fvc fs in
    propStripWhitespace asValidFieldValue ws $ T.pack xs

testValidFieldValueNullIsEmpty :: Assertion
testValidFieldValueNullIsEmpty = testNullIsEmpty asValidFieldValue

propValidFieldValueWhitespaceIsEmpty :: [WhitespaceChar] -> Property
propValidFieldValueWhitespaceIsEmpty = propWhitespaceIsEmpty asValidFieldValue

propValidFieldValueRestrictsChars :: Text -> Property
propValidFieldValueRestrictsChars =
   propJustAllowed asValidFieldValue [isUnicodeChar, isAlphaNum, isPunctuation, isSymbol, (==' ')]

propValidFieldValueGoodExamples :: [FieldValueChar] -> Property
propValidFieldValueGoodExamples fs =
    let xs = map fvc fs in
    length xs > 0
      && length xs < 200
      && not (isWhitespace $ T.pack xs)
     ==> isGood $ asValidFieldValue $ T.pack xs

newtype FieldValueChar = FieldValueChar { fvc :: Char } deriving Show

instance Arbitrary FieldValueChar where
    arbitrary = oneof . map (return . FieldValueChar) $ "aAż29 $'*-"
testValidInviteTextNullIsEmpty :: Assertion
testValidInviteTextNullIsEmpty = testNullIsEmpty asValidInviteText

testValidInviteTextBadExamplesAreFixed :: Assertion
testValidInviteTextBadExamplesAreFixed = do
  let e1 = asValidInviteText "<p><a href='aaaa'>blah</a></p>"
  assert $ isGood e1 &&
    (T.filter (\c ->
      not $ isSpace c || isControl c) $ fromGood $ e1) == "blah"
  let e2 = asValidInviteText "<p><BR></p>"
  assert $ isGood e2 &&
    (T.filter (\c ->
      not $ isSpace c || isControl c) $ fromGood $ e2) == ""
  let e3 = asValidInviteText "<p --A >aaaa<>"
  assert $ isGood e3 && fromGood e3 == "aaaa"

testValidInviteTextGoodExamples :: Assertion
testValidInviteTextGoodExamples = do
    let goodexamples = [ "<p>blah</p>"
                       , "<span>blah</span>"
                       , "<p><!-- comment --></p>"
                       , "<ol>blah</ol>"
                       , "<ul>blah</ul>"
                       , "<em>blah</em>"
                       , "<li>blah</li>"
                       , "<p><br/><br /></p>"
                       , "<p>&nbsp;</p>"
                       , "<strong>blah</strong>"
                       , "<span style=\"text-decoration: underline;\">underline</span>"
                       , "<span style=\"text-decoration: line-through;\">strikethrough</span>"
                       , "<p>Hej You,</p><p><strong>bold</strong></p><p><em>italics</em></p><p><span style=\"text-decoration: underline;\">underline</span></p><p><span style=\"text-decoration: line-through;\">strikethrough</span></p>"
                       ]
    assert $ all (isGood . asValidInviteText) goodexamples

propJustAllowed :: (Text -> Result Text) -> [Char -> Bool] -> Text -> Property
propJustAllowed f ps xs =
    isTrimmed xs
      && T.any isInvalidChar xs
    ==> isBad $ f xs
    where isInvalidChar c = all (\p -> not $ p c) ps

propStripWhitespace :: (Text -> Result Text) -> [WhitespaceChar] -> Text -> Property
propStripWhitespace f ws xs =
    let padding = T.pack $ map wc ws
        result = f (padding <> xs <> padding) in
    isGood result
    ==> isTrimmed . fromGood $ result

propWhitespaceIsEmpty :: (Text -> Result a) -> [WhitespaceChar] -> Property
propWhitespaceIsEmpty f xs =
    length xs > 0
    ==> isEmpty . f . T.pack . map wc $ xs

newtype WhitespaceChar = WhitespaceChar { wc :: Char } deriving Show

instance Arbitrary WhitespaceChar where
    arbitrary = oneof . map (return . WhitespaceChar) $ " \n\t"

testNullIsEmpty :: (Text -> Result a) -> Assertion
testNullIsEmpty f = assert $ isEmpty . f $ ""

isTrimmed :: Text -> Bool
isTrimmed xs = not (startsWithWhitespace xs) && not (endsWithWhitespace xs)

startsWithWhitespace :: Text -> Bool
startsWithWhitespace t = case T.unpack t of
  [] -> False
  (x:_) -> isSpace x

endsWithWhitespace :: Text -> Bool
endsWithWhitespace = startsWithWhitespace . T.reverse

isUnicodeChar :: Char -> Bool
isUnicodeChar = isJust . Rx.find (Rx.regex [] "\\p{L}") . T.pack . (:[])

isWhitespace :: Text -> Bool
isWhitespace = T.all isSpace

isEmptyInput :: Text -> Bool
isEmptyInput xs = isWhitespace xs || T.null xs

isLowerCase :: Text -> Bool
isLowerCase xs = T.toLower xs == xs

-- TODO: remove this.
fromGood:: Result a -> a
fromGood (Good a) = a
fromGood _ = unexpectedError "Trying to get good from bad"
