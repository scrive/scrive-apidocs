module Doc.API.V2.JSON.SigningData (
    ssdToJson
  , matchSignatoryName
  , matchSignatoryBirthday
  , matchName
  , IdinMatchResult (..)
  , BirthdayMatchResult (..)
 ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Algorithm.Diff (Diff, getGroupedDiff)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Unjson
import qualified Data.Algorithm.Diff as Diff
import qualified Data.ByteString.Base64 as B64
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Doc.API.V2.JSON.Misc
import Doc.Types.SignatoryField
import Doc.Types.SignatoryLink
import Doc.Types.SigningData
import EID.EIDService.Types
import EID.Signature.Model
import Util.HasSomeUserInfo

data IdinMatchResult
  = Match
  | Mismatch !Text
  | Misspelled
  deriving (Eq)

data BirthdayMatchResult
  = SameDate
  | MismatchDate !Text
  deriving (Eq)

instance Show IdinMatchResult where
  show Match        = "match"
  show (Mismatch _) = "mismatch"
  show Misspelled   = "misspelled"

instance Show BirthdayMatchResult where
  show SameDate         = "match"
  show (MismatchDate _) = "mismatch"

ssdToJson :: Bool -> SignatoryLink -> SignatorySigningData -> Value
ssdToJson hidePN signatory SignatorySigningData {..} =
  object
    $  ["has_signed" .= ssdHasSigned, "provider" .= encAuthMethod]
    <> providerSpecificData
  where
    encAuthMethod =
      maybe "legacy_provider" (unjsonToJSON unjsonAuthenticationToSignMethod)
        $ case ssdData of
            Left  authToSignMethod                 -> Just authToSignMethod
            Right (CGISEBankIDSignature_        _) -> Just SEBankIDAuthenticationToSign
            Right (NetsNOBankIDSignature_       _) -> Just NOBankIDAuthenticationToSign
            Right (NetsDKNemIDSignature_ _) -> Just LegacyDKNemIDAuthenticationToSign
            Right (EIDServiceIDINSignature_     _) -> Just IDINAuthenticationToSign
            Right (EIDServiceFITupasSignature_  _) -> Just FITupasAuthenticationToSign
            Right (EIDServiceNOBankIDSignature_ _) -> Just NOBankIDAuthenticationToSign
            Right (EIDServiceOnfidoSignature_ EIDServiceOnfidoSignature {..}) ->
              case eidServiceOnfidoSigMethod of
                OnfidoDocumentCheck -> Just OnfidoDocumentCheckAuthenticationToSign
                OnfidoDocumentAndPhotoCheck ->
                  Just OnfidoDocumentAndPhotoCheckAuthenticationToSign
            Right (EIDServiceSEBankIDSignature_  _) -> Just SEBankIDAuthenticationToSign
            Right (EIDServiceVerimiQesSignature_ _) -> Just VerimiQesAuthenticationToSign
            -- TODO maybe it makes sense to actually use this for all (Right _) patterns?
            Right (EIDServiceDKNemIDSignature_ _) ->
              Just $ signatorylinkauthenticationtosignmethod signatory
            Right (LegacyBankIDSignature_       _) -> Nothing
            Right (LegacyTeliaSignature_        _) -> Nothing
            Right (LegacyNordeaSignature_       _) -> Nothing
            Right (LegacyMobileBankIDSignature_ _) -> Nothing

    encB64 = T.decodeUtf8 . B64.encode

    providerSpecificData :: [Pair]
    providerSpecificData = case ssdData of
      Left _ -> []
      Right (CGISEBankIDSignature_ CGISEBankIDSignature {..}) ->
        [ "se_bankid_data" .= object
            (  [ "signatory_name" .= cgisebidsSignatoryName
               , "signed_text" .= cgisebidsSignedText
               , "signature" .= encB64 cgisebidsSignature
               , "ocsp_response" .= encB64 cgisebidsOcspResponse
               , "signatory_ip" .= cgisebidsSignatoryIP
               ]
            <> if hidePN
                 then []
                 else ["personal_number" .= cgisebidsSignatoryPersonalNumber]
            )
        ]
      Right (NetsNOBankIDSignature_ NetsNOBankIDSignature {..}) ->
        [ "no_bankid_data" .= object
            [ "signatory_name" .= netsnoSignatoryName
            , "signed_text" .= netsnoSignedText
            , "sdo" .= netsnoB64SDO
            , "signatory_pid" .= netsnoSignatoryPID
            ]
        ]
      Right (NetsDKNemIDSignature_ NetsDKNemIDSignature {..}) ->
        [ "dk_nemid_data" .= object
            (  [ "signatory_name" .= netsdkSignatoryName
               , "signed_text" .= netsdkSignedText
               , "sdo" .= netsdkB64SDO
               , "signatory_ip" .= netsdkSignatoryIP
               ]
            <> if hidePN
                 then []
                 else ["signatory_personal_number" .= netsdkSignatorySSN]
            )
        ]
      Right (EIDServiceIDINSignature_ details) ->
        idinSigningDataToJson hidePN signatory details
      Right (EIDServiceFITupasSignature_ EIDServiceFITupasSignature {..}) ->
        [ "fi_tupas_data" .= object
            (["signatory_name" .= eidServiceFITupasSigSignatoryName] <> if hidePN
              then []
              else
                [ "signatory_date_of_birth" .= eidServiceFITupasSigDateOfBirth
                , "signatory_personal_number" .= eidServiceFITupasSigPersonalNumber
                ]
            )
        ]
      Right (EIDServiceNOBankIDSignature_ EIDServiceNOBankIDSignature {..}) ->
        [ "no_bankid_data" .= object
            (["signatory_name" .= eidServiceNOBankIDSigSignatoryName] <> if hidePN
              then []
              else
                [ "signatory_date_of_birth" .= eidServiceNOBankIDSigDateOfBirth
                , "signatory_personal_number" .= eidServiceNOBankIDSigPersonalNumber
                , "signatory_mobile" .= eidServiceNOBankIDSigPhoneNumber
                , "signature" .= eidServiceNOBankIDSigCertificate
                ]
            )
        ]
      Right (EIDServiceOnfidoSignature_ EIDServiceOnfidoSignature {..}) ->
        [ "onfido_data" .= object
            (  [ "signatory_name" .= eidServiceOnfidoSigSignatoryName
               , "report" .= eidServiceOnfidoSigMethod
               ]
            <> if hidePN
                 then []
                 else ["signatory_date_of_birth" .= eidServiceOnfidoSigDateOfBirth]
            )
        ]
      Right (EIDServiceSEBankIDSignature_ EIDServiceSEBankIDSignature {..}) ->
        [ "se_bankid_data" .= object
            (  [ "signatory_name" .= eidServiceSEBankIDSigSignatoryName
               , "signature" .= encB64 eidServiceSEBankIDSigSignature
               , "ocsp_response" .= encB64 eidServiceSEBankIDSigOcspResponse
               , "signatory_ip" .= eidServiceSEBankIDSigIP
               ]
            <> if hidePN
                 then []
                 else ["personal_number" .= eidServiceSEBankIDSigPersonalNumber]
            )
        ]
      Right (EIDServiceDKNemIDSignature_ EIDServiceDKNemIDSignature {..}) ->
        [ "dk_nemid_data" .= object
            (  [ "signatory_name" .= eidServiceDKNemIDSignatoryName
               , "signed_text" .= eidServiceDKNemIDSignedText
               , "sdo" .= eidServiceDKNemIDB64SDO
               , "signatory_ip" .= eidServiceDKNemIDSignatoryIP
               ]
            <> if hidePN
                 then []
                 else
                   [ "signatory_personal_number"
                       .= eidServiceDKNemIDSignatoryPersonalNumber
                   ]
            )
        ]
      Right (EIDServiceVerimiQesSignature_ EIDServiceVerimiQesSignature {..}) ->
        [ "verimi_data" .= object
            [ "signatory_name" .= eidServiceVerimiSigName
            , "signatory_mobile" .= eidServiceVerimiSigVerifiedPhone
            , "signatory_email" .= eidServiceVerimiSigVerifiedEmail
            ]
        ]
      Right (LegacyBankIDSignature_       _) -> []
      Right (LegacyTeliaSignature_        _) -> []
      Right (LegacyNordeaSignature_       _) -> []
      Right (LegacyMobileBankIDSignature_ _) -> []

idinSigningDataToJson :: Bool -> SignatoryLink -> EIDServiceNLIDINSignature -> [Pair]
idinSigningDataToJson hidePN signatory details@EIDServiceNLIDINSignature {..} =
  ["nl_idin_data" .= object idinData]
  where
    idinData :: [Pair]
    idinData =
      [ "signatory_name" .= unEIDServiceIDINSigSignatoryName
        , "signatory_customer_id" .= unEIDServiceIDINSigCustomerID
        , "signatory_name_match" .= showt nameMatchResult
        , "signatory_date_of_birth_match" .= showt birthdayMatchResult
        , "signatory_name_and_date_of_birth_match" .= showt nameBirthdayMatchResult
        ]
        <> birthdayField
        <> matchErrorField

    nameMatchResult :: IdinMatchResult
    nameMatchResult = matchSignatoryName signatory details

    birthdayMatchResult :: BirthdayMatchResult
    birthdayMatchResult = matchSignatoryBirthday signatory details

    nameBirthdayMatchResult :: IdinMatchResult
    nameBirthdayMatchResult = case birthdayMatchResult of
      MismatchDate err -> Mismatch err
      SameDate         -> nameMatchResult

    birthdayField :: [Pair]
    birthdayField =
      if hidePN then [] else ["signatory_date_of_birth" .= unEIDServiceIDINSigDateOfBirth]

    matchErrorField :: [Pair]
    matchErrorField = case nameBirthdayMatchResult of
      Mismatch err -> ["mismatch_error" .= err]
      _            -> []

splitFirstSpace :: Text -> (Text, Text)
splitFirstSpace str = case T.words str of
  (x : xs) -> (x, T.unwords xs)
  []       -> ("", "")

normalizeName :: Text -> Text
normalizeName = T.toLower . T.replace "." ""

joinText :: Text -> Text -> Text
joinText "" "" = ""
joinText "" b  = b
joinText a  "" = a
joinText a  b  = a <> " " <> b

-- Compare the name registered in the signatory link against
-- the name returned from IDIN authentication. IDIN returns
-- the initials of the first name, combined with the legal
-- last name and prefix (tussenvoegsel).
--   Match - if name matches exactly
--   Misspelled - if there is 1~2 letters difference in the last name
--   Mismatch - if the initials don't match or if more misspellings in the last name.
matchSignatoryName :: SignatoryLink -> EIDServiceNLIDINSignature -> IdinMatchResult
matchSignatoryName signatory details = matchSignatoryName' slFullName
                                                           eidInitials
                                                           eidLastName
  where
    slFirstName                = normalizeName $ getFirstName signatory
    slLastName                 = normalizeName $ getLastName signatory
    slFullName                 = joinText slFirstName slLastName
    eidFullName                = normalizeName $ unEIDServiceIDINSigSignatoryName details
    (eidInitials, eidLastName) = splitFirstSpace eidFullName

matchSignatoryBirthday
  :: SignatoryLink -> EIDServiceNLIDINSignature -> BirthdayMatchResult
matchSignatoryBirthday signatory details = case (mDate1, mDate2) of
  (Just date1, Just date2) -> if date1 == date2
    then SameDate
    else
      MismatchDate
      $  "birth dates does not match. expected "
      <> showt date2
      <> ", got "
      <> showt date1
  (Nothing, Nothing) -> MismatchDate "failed to parse birth dates"
  (Nothing, _      ) -> MismatchDate "failed to parse input birth date"
  (_      , Nothing) -> MismatchDate "failed to parse birth date from iDIN"
  where
    mDate1 :: Maybe Day
    mDate1 = findSignatoryBirthDate signatory

    mDate2 :: Maybe Day
    mDate2 = parseDate $ unEIDServiceIDINSigDateOfBirth details

matchSignatoryName' :: Text -> Text -> Text -> IdinMatchResult
matchSignatoryName' slFullName eidInitials eidLastName
  | slFullName == eidFullName = Match
  | slInitials == eidInitials = matchName eidLastName slRestName
  | T.length slInitials <= 1 = case matchName eidLastName slRestName of
    Match        -> Misspelled
    Misspelled   -> Misspelled
    Mismatch err -> Mismatch err
  | otherwise = case
      matchSignatoryName' slFullName (T.dropEnd 1 eidInitials) eidLastName
    of
      Match        -> Misspelled
      Misspelled   -> Misspelled
      Mismatch err -> Mismatch err
  where
    slNameWords = T.words slFullName

    eidInitialsCount :: Int
    eidInitialsCount = T.length eidInitials

    -- Fuzzy match the initials from IDIN by taking the words
    -- from full name, so that misplaced first/last names still matches
    slInitials :: Text
    slInitials = T.concat . map (T.take 1) $ L.take eidInitialsCount slNameWords

    slRestName :: Text
    slRestName  = T.unwords $ L.drop eidInitialsCount slNameWords

    eidFullName = eidInitials <> " " <> eidLastName

matchName :: Text -> Text -> IdinMatchResult
matchName s1 s2 | s1 == s2      = Match
                | distance == 0 = Match
                | distance <= 2 = Misspelled
                | otherwise     = Mismatch "last name does not match"
  where distance = stringDistance (T.unpack s1) (T.unpack s2)

-- Use diff algorithm to compare how many letters
-- difference in two strings. Missing or additional
-- one letter have distance of 1, while replacement
-- of one letter have distance of 2 (substraction + addition).
stringDistance :: String -> String -> Int
stringDistance a b = distance
  where
    diffs :: [Diff String]
    diffs = getGroupedDiff a b

    patchSize :: Diff String -> Int
    patchSize (Diff.First  s) = length s
    patchSize (Diff.Second s) = length s
    patchSize (Diff.Both _ _) = 0

    distance :: Int
    distance = sum . map patchSize $ diffs

parseDate :: Text -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" . T.unpack

findSignatoryBirthDate :: SignatoryLink -> Maybe Day
findSignatoryBirthDate = L.firstJust matchField . signatoryfields
  where
    matchField :: SignatoryField -> Maybe Day
    matchField (SignatoryTextField textField) | stfName textField == "DOB" =
      parseDate $ stfValue textField
    matchField _ = Nothing
