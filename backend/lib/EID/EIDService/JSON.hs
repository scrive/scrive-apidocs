module EID.EIDService.JSON (
    encodeNewTransactionRequest
  , extractEIDServiceURL
  , decodeCompleteTransaction
  ) where

import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.HashMap.Lazy
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import EID.EIDService.Types

encodeNewTransactionRequest :: EIDServiceProviderParams -> Encoding
encodeNewTransactionRequest espp =
  pairs
    $  ("method" .= ("auth" :: String))
    <> ("provider" .= provider)
    <> ("redirectUrl" .= esppRedirectURL espp)
    <> providerParameters
  where
    provider = toEIDServiceName espp
    makeProviderParams vals =
      pair "providerParameters" . pairs . pair provider $ pairs vals
    providerParameters = case espp of
      EIDServiceProviderParamsVerimi{} -> mempty
      EIDServiceProviderParamsNLIDIN{} -> makeProviderParams ("requestBirthdate" .= True)
      EIDServiceProviderParamsDKNemID {..} ->
        makeProviderParams $ ("limitedClientMode" .= True) <> ("uiLocale" .= esppUILocale)
      EIDServiceProviderParamsNOBankID {..} ->
        let phonePair = maybe mempty ("phoneNumber" .=) esppPhoneNumber
        in  makeProviderParams $ ("personalNumber" .= esppPersonalNumber) <> phonePair
      EIDServiceProviderParamsFITupas{} -> mempty

newtype EIDServiceURL = EIDServiceURL Text

instance FromJSON EIDServiceURL where
  parseJSON outer =
    EIDServiceURL
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" extractAuthObj
          >>= withObject "object" (.: "authUrl")
          )
    where
      -- We have to do this because the JSON object key varies depending on provider
      extractAuthObj pInfoHM = do
        let onlyAuthKeys k _ = "Auth" `T.isSuffixOf` k
        case keys $ filterWithKey onlyAuthKeys pInfoHM of
          [authKey] -> pInfoHM .: authKey
          [] -> fail "JSON structure is invalid - no <provider>Auth object"
          _ -> fail "JSON structure is invalid - more than one <provider>Auth object"

extractEIDServiceURL :: BSL.ByteString -> Maybe Text
extractEIDServiceURL = fmap (\(EIDServiceURL t) -> t) . decode

newtype CompleteTransaction = CompleteTransaction CompleteEIDServiceTransaction

instance FromJSON CompleteTransaction where
  parseJSON outer = CompleteTransaction <$> do
    status <- parseJSON outer
    withObject "object" (.: "provider") outer >>= \case
      "verimi"    -> CompleteVerimiEIDServiceTransaction status <$> parseCDJSON outer
      "nlIDIN"    -> CompleteNLIDINEIDServiceTransaction status <$> parseCDJSON outer
      "dkNemID"   -> CompleteDKNemIDEIDServiceTransaction status <$> parseCDJSON outer
      "noBankID"  -> CompleteNOBankIDEIDServiceTransaction status <$> parseCDJSON outer
      "fiTupas"   -> CompleteFITupasEIDServiceTransaction status <$> parseCDJSON outer
      (_ :: Text) -> fail "Unrecognised provider"

decodeCompleteTransaction :: BSL.ByteString -> Maybe CompleteEIDServiceTransaction
decodeCompleteTransaction = fmap (\(CompleteTransaction t) -> t) . decode

class FromCompletionDataJSON a where
  parseCDJSON :: Value -> Parser (Maybe a)

newtype VerimiCompletionData = VerimiCompletionData VerimiEIDServiceCompletionData

instance FromJSON VerimiCompletionData where
  parseJSON outer = do
    let providerAuth = toEIDServiceName EIDServiceTransactionProviderVerimi <> "Auth"
    VerimiCompletionData
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: providerAuth)
          >>= withObject "object" (.: "completionData")
          >>= withObject
                "object"
                (\o -> do
                  emailVerified <- o .: "emailVerified"
                  unless emailVerified $ fail "Email is not verified"
                  VerimiEIDServiceCompletionData <$> (o .: "name") <*> (o .: "email")
                )
          )

instance FromCompletionDataJSON VerimiEIDServiceCompletionData where
  parseCDJSON val = do
    mcd <- parseJSON val <|> return Nothing
    return $ (\(VerimiCompletionData t) -> t) <$> mcd

newtype NLIDINCompletionData = NLIDINCompletionData NLIDINEIDServiceCompletionData

instance FromJSON NLIDINCompletionData where
  parseJSON outer = do
    let providerAuth = toEIDServiceName EIDServiceTransactionProviderNLIDIN <> "Auth"
    NLIDINCompletionData
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: providerAuth)
          >>= withObject "object" (.: "completionData")
          >>= withObject
                "object"
                (\o -> do
                  initials       <- o .:? "initials" .!= ""
                  mTussenvoegsel <- o .:? "legalLastNamePrefix"
                  surname        <- o .:? "legalLastName" .!= ""
                  dob            <- o .:? "birthDate" .!= ""
                  customerID     <- o .:? "customerId" .!= ""
                  let name = case mTussenvoegsel of
                        Just tussenvoegsel ->
                          initials <> " " <> tussenvoegsel <> " " <> surname
                        Nothing -> initials <> " " <> surname
                  return NLIDINEIDServiceCompletionData { eiditdName       = name
                                                        , eiditdBirthDate  = dob
                                                        , eiditdCustomerID = customerID
                                                        }
                )
          )

instance FromCompletionDataJSON NLIDINEIDServiceCompletionData where
  parseCDJSON val = do
    mcd <- parseJSON val <|> return Nothing
    return $ (\(NLIDINCompletionData t) -> t) <$> mcd

newtype DKNemIDCompletionData = DKNemIDCompletionData DKNemIDEIDServiceCompletionData

instance FromJSON DKNemIDCompletionData where
  parseJSON outer = do
    let providerName = toEIDServiceName EIDServiceTransactionProviderDKNemID
        providerAuth = providerName <> "Auth"
    ip <-
      withObject "object" (.: "providerParameters") outer
      >>= withObject "object" (.: "auth")
      >>= withObject "object" (.: providerName)
      >>= withObject "object" (.: "method")
      >>= resolveInternalProvider
    DKNemIDCompletionData
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: providerAuth)
          >>= withObject "object" (.: "completionData")
          >>= withObject
                "object"
                (\o -> do
                  pid <- o .: "pid"
                  ssn <- o .: "ssn"
                  let dob = dateOfBirthFromDKPersonalNumber ssn
                  (cer, dn) <- o .: "certificateData" >>= withObject
                    "object"
                    (\cd -> (,) <$> cd .: "certificate" <*> cd .: "distinguishedName")
                  return DKNemIDEIDServiceCompletionData { eidnidInternalProvider  = ip
                                                         , eidnidSSN               = ssn
                                                         , eidnidBirthDate         = dob
                                                         , eidnidCertificate       = cer
                                                         , eidnidDistinguishedName = dn
                                                         , eidnidPid               = pid
                                                         }
                )
          )
    where
      resolveInternalProvider mt = case mt of
        -- TODO: decide if we want to distinguish between personal keycard and employee
        -- keycard - this would break backward data compability, but maybe doesn't hurt us
        -- that much? - KJ
        Nothing                -> return EIDServiceNemIDKeyCard
        Just "PersonalKeycard" -> return EIDServiceNemIDKeyCard
        Just "EmployeeKeycard" -> return EIDServiceNemIDKeyCard
        Just "EmployeeKeyFile" -> return EIDServiceNemIDKeyFile
        Just t -> fail $ "Unknown internal provider returned from EID service: " <> t

instance FromCompletionDataJSON DKNemIDEIDServiceCompletionData where
  parseCDJSON val = do
    mcd <- parseJSON val <|> return Nothing
    return $ (\(DKNemIDCompletionData t) -> t) <$> mcd

newtype NOBankIDCompletionData = NOBankIDCompletionData NOBankIDEIDServiceCompletionData

instance FromJSON NOBankIDCompletionData where
  parseJSON outer = do
    let providerName = toEIDServiceName EIDServiceTransactionProviderNOBankID
        providerAuth = providerName <> "Auth"
    NOBankIDCompletionData
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: providerAuth)
          >>= withObject "object" (.: "completionData")
          >>= withObject
                "object"
                (\o -> do
                  pid              <- o .: "pid"
                  usedMobileBankID <- o .: "usedMobileBankID"
                  mphoneNumber     <- o .:? "phoneNumber"
                  idn              <- o .: "issuerDN"
                  (mdob, mname)    <- o .: "profileData" >>= withObject
                    "object"
                    (\cd -> (,) <$> cd .:? "birthdate" <*> cd .:? "name")
                  (mcer, dn) <- o .: "certificateData" >>= withObject
                    "object"
                    (\cd -> (,) <$> cd .:? "certificate" <*> cd .: "distinguishedName")
                  return NOBankIDEIDServiceCompletionData
                    { eidnobidInternalProvider        = if usedMobileBankID
                                                          then EIDServiceNOBankIDMobile
                                                          else EIDServiceNOBankIDStandard
                    , eidnobidName                    = mname
                    , eidnobidBirthDate               = mdob
                    , eidnobidDistinguishedName       = dn
                    , eidnobidIssuerDistinguishedName = idn
                    , eidnobidCertificate             = mcer
                    , eidnobidPhoneNumber             = mphoneNumber
                    , eidnobidPid                     = pid
                    }
                )
          )

instance FromCompletionDataJSON NOBankIDEIDServiceCompletionData where
  parseCDJSON val = do
    mcd <- parseJSON val <|> return Nothing
    return $ (\(NOBankIDCompletionData t) -> t) <$> mcd

newtype FITupasCompletionData = FITupasCompletionData FITupasEIDServiceCompletionData

instance FromJSON FITupasCompletionData where
  parseJSON outer = do
    let providerAuth = toEIDServiceName EIDServiceTransactionProviderFITupas <> "Auth"
    FITupasCompletionData
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: providerAuth)
          >>= withObject "object" (.: "completionData")
          >>= withObject
                "object"
                (\o -> do
                  mbank       <- o .:? "bank"
                  mpid        <- o .:? "pid"
                  mssn        <- o .:? "ssn"
                  (dob, name) <- o .: "profileData" >>= withObject
                    "object"
                    (\cd -> (,) <$> cd .: "birthdate" <*> cd .: "name")
                  dn <- o .: "certificateData" >>= withObject "object"
                                                              (.: "distinguishedName")
                  return FITupasEIDServiceCompletionData
                    { eidtupasName              = name
                    , eidtupasBirthDate         = dob
                    , eidtupasDistinguishedName = dn
                    , eidtupasBank              = mbank
                    , eidtupasPid               = mpid
                    , eidtupasSSN               = mssn
                    }
                )
          )

instance FromCompletionDataJSON FITupasEIDServiceCompletionData where
  parseCDJSON val = do
    mcd <- parseJSON val <|> return Nothing
    return $ (\(FITupasCompletionData t) -> t) <$> mcd
