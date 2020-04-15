module Doc.API.V2.JSON.SigningData (
    ssdToJson
  , matchSignatoryName
  , matchName
  , NameMatchResult (..)
 ) where

import Data.Aeson
import Data.Algorithm.Diff (Diff, getGroupedDiff)
import Data.Unjson
import qualified Data.Algorithm.Diff as Diff
import qualified Data.ByteString.Base64 as B64
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Doc.API.V2.JSON.Misc
import Doc.Types.SignatoryLink
import Doc.Types.SigningData
import EID.EIDService.Types
import EID.Signature.Model
import Util.HasSomeUserInfo

data NameMatchResult
  = Match
  | Mismatch
  | Misspelled
  deriving (Eq)

instance Show NameMatchResult where
  show Match      = "match"
  show Mismatch   = "mismatch"
  show Misspelled = "misspelled"

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
            Right (NetsDKNemIDSignature_        _) -> Just DKNemIDAuthenticationToSign
            Right (EIDServiceIDINSignature_     _) -> Just IDINAuthenticationToSign
            Right (EIDServiceFITupasSignature_  _) -> Just FITupasAuthenticationToSign
            Right (LegacyBankIDSignature_       _) -> Nothing
            Right (LegacyTeliaSignature_        _) -> Nothing
            Right (LegacyNordeaSignature_       _) -> Nothing
            Right (LegacyMobileBankIDSignature_ _) -> Nothing

    encB64               = T.decodeUtf8 . B64.encode

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
      Right (EIDServiceIDINSignature_ (EIDServiceNLIDINSignature details@NLIDINEIDServiceCompletionData {..}))
        -> [ "nl_idin_data" .= object
               (  [ "signatory_name" .= eiditdName
                  , "signatory_customer_id" .= eiditdCustomerID
                  , "signatory_name_match" .= showt (matchSignatoryName signatory details)
                  ]
               <> if hidePN then [] else ["signatory_date_of_birth" .= eiditdBirthDate]
               )
           ]
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
      Right (LegacyBankIDSignature_       _) -> []
      Right (LegacyTeliaSignature_        _) -> []
      Right (LegacyNordeaSignature_       _) -> []
      Right (LegacyMobileBankIDSignature_ _) -> []

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
matchSignatoryName :: SignatoryLink -> NLIDINEIDServiceCompletionData -> NameMatchResult
matchSignatoryName signatory details = matchSignatoryName' slFullName
                                                           eidInitials
                                                           eidLastName
  where
    slFirstName                = normalizeName $ getFirstName signatory
    slLastName                 = normalizeName $ getLastName signatory
    slFullName                 = joinText slFirstName slLastName
    eidFullName                = normalizeName $ eiditdName details
    (eidInitials, eidLastName) = splitFirstSpace eidFullName

matchSignatoryName' :: Text -> Text -> Text -> NameMatchResult
matchSignatoryName' slFullName eidInitials eidLastName
  | slFullName == eidFullName = Match
  | slInitials == eidInitials = matchName eidLastName slRestName
  | T.length slInitials <= 1 = Mismatch
  | otherwise = case
      matchSignatoryName' slFullName (T.dropEnd 1 eidInitials) eidLastName
    of
      Match      -> Misspelled
      Misspelled -> Misspelled
      Mismatch   -> Mismatch
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

matchName :: Text -> Text -> NameMatchResult
matchName s1 s2 | s1 == s2      = Match
                | distance == 0 = Match
                | distance <= 2 = Misspelled
                | otherwise     = Mismatch
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
