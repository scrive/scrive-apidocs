module Doc.API.V2.JSON.SigningData (
    ssdToJson
 ) where

import Data.Aeson
import Data.Unjson
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T

import Doc.API.V2.JSON.Misc
import Doc.Types.SignatoryLink
import Doc.Types.SigningData
import EID.EIDService.Types
import EID.Signature.Model

ssdToJson :: Bool -> SignatorySigningData -> Value
ssdToJson hidePN SignatorySigningData {..} =
  object
    $  ["has_signed" .= ssdHasSigned, "provider" .= encAuthMethod]
    <> providerSpecificData
  where
    encAuthMethod =
      fromMaybe "legacy_provider"
        $   unjsonToJSON unjsonAuthenticationToSignMethod
        <$> case ssdData of
              Left  authToSignMethod                 -> Just authToSignMethod
              Right (CGISEBankIDSignature_        _) -> Just SEBankIDAuthenticationToSign
              Right (NetsNOBankIDSignature_       _) -> Just NOBankIDAuthenticationToSign
              Right (NetsDKNemIDSignature_        _) -> Just DKNemIDAuthenticationToSign
              Right (EIDServiceIDINSignature_     _) -> Just IDINAuthenticationToSign
              Right (LegacyBankIDSignature_       _) -> Nothing
              Right (LegacyTeliaSignature_        _) -> Nothing
              Right (LegacyNordeaSignature_       _) -> Nothing
              Right (LegacyMobileBankIDSignature_ _) -> Nothing

    encB64               = T.decodeUtf8 . B64.encode

    providerSpecificData = case ssdData of
      Left _ -> []
      Right (CGISEBankIDSignature_ (CGISEBankIDSignature {..})) ->
        [ "se_bankid_data"
            .= (  object
               $  [ "signatory_name" .= cgisebidsSignatoryName
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
      Right (NetsNOBankIDSignature_ (NetsNOBankIDSignature {..})) ->
        [ "no_bankid_data"
            .= ( object
               $ [ "signatory_name" .= netsnoSignatoryName
                 , "signed_text" .= netsnoSignedText
                 , "sdo" .= netsnoB64SDO
                 , "signatory_pid" .= netsnoSignatoryPID
                 ]
               )
        ]
      Right (NetsDKNemIDSignature_ (NetsDKNemIDSignature {..})) ->
        [ "dk_nemid_data"
            .= (  object
               $  [ "signatory_name" .= netsdkSignatoryName
                  , "signed_text" .= netsdkSignedText
                  , "sdo" .= netsdkB64SDO
                  , "signatory_ip" .= netsdkSignatoryIP
                  ]
               <> if hidePN
                    then []
                    else ["signatory_personal_number" .= netsdkSignatorySSN]
               )
        ]
      Right (EIDServiceIDINSignature_ (EIDServiceIDINSignature (CompleteIDINEIDServiceTransactionData {..})))
        -> [ "nl_idin_data"
               .= (  object
                  $  [ "signatory_name" .= eiditdName
                     , "signatory_verified_email" .= eiditdVerifiedEmail
                     , "signatory_customer_id" .= eiditdCustomerID
                     ]
                  <> if hidePN
                       then []
                       else ["signatory_date_of_birth" .= eiditdBirthDate]
                  )
           ]
      Right (LegacyBankIDSignature_       _) -> []
      Right (LegacyTeliaSignature_        _) -> []
      Right (LegacyNordeaSignature_       _) -> []
      Right (LegacyMobileBankIDSignature_ _) -> []
