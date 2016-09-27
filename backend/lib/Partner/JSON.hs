module Partner.JSON (
    UserForUpdate(..)
  , unjsonUserForUpdate
  , userInfoFromUserForUpdate
  , userToUserForUpdate
  , CompanyForUpdate(..)
  , unjsonCompanyForUpdate
  , updateCompanyInfoWithCompanyForUpdate
  , companyToCompanyForUpdate
  ) where

import Data.Default
import Data.Unjson

import Company.Model
import InputValidation
import KontraPrelude
import User.Email (Email(..))
import User.Model

data UserForUpdate = UserForUpdate {
      ufuId :: String
    , ufuEmail :: Email
    , ufuFirstName :: String
    , ufuLastName :: String
    , ufuPersonalNumber :: String
    , ufuPhone :: String
    , ufuCompanyPosition :: String
    , ufuLang :: Lang
    , ufuHasAcceptedTOS :: Bool
    }

instance Default UserForUpdate where
    def = UserForUpdate {
            ufuId = ""
          , ufuEmail = Email ""
          , ufuFirstName = ""
          , ufuLastName = ""
          , ufuPersonalNumber = ""
          , ufuPhone = ""
          , ufuCompanyPosition = ""
          , ufuLang = LANG_EN
          , ufuHasAcceptedTOS = False
          }

userInfoFromUserForUpdate :: UserForUpdate -> UserInfo
userInfoFromUserForUpdate UserForUpdate{..} =
  UserInfo { userfstname         = ufuFirstName
           , usersndname         = ufuLastName
           , userpersonalnumber  = ufuPersonalNumber
           , usercompanyposition = ufuCompanyPosition
           , userphone           = ufuPhone
           , useremail           = ufuEmail
           }

unjsonUserForUpdate :: UnjsonDef UserForUpdate
unjsonUserForUpdate = objectOf $ pure def
        <*   fieldReadonly "id" ufuId "The user's ID"
        <**> (fieldBy "email" ufuEmail "A user's email"
              (unjsonEmail)
              <**> (pure $ \email ufu -> ufu { ufuEmail = email }))
        <**> (fieldBy "first_name" ufuFirstName "A user's name"
              (unjsonWithValidationOrEmpty asValidName)
              <**> (pure $ \fname ufu -> ufu { ufuFirstName = fname }))
        <**> (fieldBy "last_name" ufuLastName "A user's last name"
              (unjsonWithValidationOrEmpty asValidName)
              <**> (pure $ \lname ufu -> ufu { ufuLastName = lname }))
        <**> (fieldBy "personal_number" ufuPersonalNumber "A user's personal number"
              (unjsonWithValidationOrEmpty asValidName)
              <**> (pure $ \pnumber ufu -> ufu { ufuPersonalNumber = pnumber }))
        <**> (fieldBy "phone" ufuPhone "A user's phone"
              (unjsonWithValidationOrEmpty asValidPhone)
              <**> (pure $ \phone ufu -> ufu { ufuPhone = phone }))
        <**> (fieldBy "company_position" ufuCompanyPosition "A user's company position"
              (unjsonWithValidationOrEmpty asValidPosition)
              <**> (pure $ \comppos ufu -> ufu { ufuCompanyPosition = comppos }))
        <**> (fieldBy "lang" ufuLang "A user's language"
              (unjsonLang)
              <**> (pure $ \lang ufu -> ufu { ufuLang = lang }))
        <**> (field "has_accepted_tos" ufuHasAcceptedTOS "Has the user accepted the TOS?"
              <**> (pure $ \tos ufu -> ufu { ufuHasAcceptedTOS = tos }))

userToUserForUpdate :: User -> UserForUpdate
userToUserForUpdate user =
  UserForUpdate {
    ufuId = show $ userid user
  , ufuEmail = useremail . userinfo $ user
  , ufuFirstName = userfstname . userinfo $ user
  , ufuLastName = usersndname . userinfo $ user
  , ufuPersonalNumber = userpersonalnumber . userinfo $ user
  , ufuPhone = userphone . userinfo $ user
  , ufuCompanyPosition = usercompanyposition . userinfo $ user
  , ufuLang = lang $ usersettings $ user
  , ufuHasAcceptedTOS = maybe False (const True) (userhasacceptedtermsofservice user)
  }

data CompanyForUpdate = CompanyForUpdate
    {
      cfuCompanyId :: String
    , cfuCompanyName :: String
    , cfuCompanyNumber :: String
    , cfuCompanyAddress :: String
    , cfuCompanyZip :: String
    , cfuCompanyCity :: String
    , cfuCompanyCountry :: String
    }

instance Default CompanyForUpdate where
    def = CompanyForUpdate
          {
            cfuCompanyId = ""
          , cfuCompanyName = ""
          , cfuCompanyNumber = ""
          , cfuCompanyAddress = ""
          , cfuCompanyZip = ""
          , cfuCompanyCity = ""
          , cfuCompanyCountry = ""
          }

unjsonCompanyForUpdate :: UnjsonDef CompanyForUpdate
unjsonCompanyForUpdate = objectOf $ pure def
        <*   fieldReadonly "id" cfuCompanyId "The company ID"
        <**> (fieldBy "name" cfuCompanyName "Company name"
              (unjsonWithValidationOrEmpty asValidCompanyName)
              <**> (pure $ \cn cfu -> cfu { cfuCompanyName = cn }))
        <**> (fieldBy "number" cfuCompanyNumber "Company number"
              (unjsonWithValidationOrEmpty asValidCompanyNumber)
              <**> (pure $ \cnum cfu -> cfu { cfuCompanyNumber = cnum }))
        <**> (fieldBy "address" cfuCompanyAddress "Company address"
              (unjsonWithValidationOrEmpty asValidAddress)
              <**> (pure $ \ca cfu -> cfu { cfuCompanyAddress = ca }))
        <**> (field "zip" cfuCompanyZip "Company zip"
              <**> (pure $ \cz cfu -> cfu { cfuCompanyZip = cz }))
        <**> (fieldBy "city" cfuCompanyCity "Company city"
              (unjsonWithValidationOrEmpty asValidName)
              <**> (pure $ \cci cfu -> cfu { cfuCompanyCity = cci }))
        <**> (fieldBy "country" cfuCompanyCountry "Company country"
              (unjsonWithValidationOrEmpty asValidName)
              <**> (pure $ \cco cfu -> cfu { cfuCompanyCountry = cco }))

updateCompanyInfoWithCompanyForUpdate :: CompanyInfo -> CompanyForUpdate -> CompanyInfo
updateCompanyInfoWithCompanyForUpdate companyInfo CompanyForUpdate{..} =
  companyInfo { companyname = cfuCompanyName
              , companynumber = cfuCompanyNumber
              , companyaddress = cfuCompanyAddress
              , companyzip = cfuCompanyZip
              , companycity = cfuCompanyCity
              , companycountry = cfuCompanyCountry
              }

companyToCompanyForUpdate :: Company -> CompanyForUpdate
companyToCompanyForUpdate company = CompanyForUpdate {
    cfuCompanyId = show $ companyid company
  , cfuCompanyName = companyname cInfo
  , cfuCompanyNumber = companynumber cInfo
  , cfuCompanyAddress = companyaddress cInfo
  , cfuCompanyZip = companyzip cInfo
  , cfuCompanyCity = companycity cInfo
  , cfuCompanyCountry = companycountry cInfo
  }
  where cInfo = companyinfo company

-------------------------------------------------------------------------------
-- Utils                                                                    ---
-------------------------------------------------------------------------------

unjsonLang :: UnjsonDef Lang
unjsonLang = unjsonInvmapR ((maybe (fail "value is not valid language code") return) . langFromCode) codeFromLang unjsonDef

unjsonEmail :: UnjsonDef Email
unjsonEmail = unjsonInvmapR ((maybe (fail "not valid email address") (return . Email)) . resultToMaybe . asValidEmail) unEmail unjsonDef

unjsonWithValidationOrEmpty :: (String -> InputValidation.Result String) -> UnjsonDef String
unjsonWithValidationOrEmpty validation = unjsonInvmapR (convertResult . validation) id unjsonDef
  where
    convertResult (InputValidation.Good s)   = return s
    convertResult (InputValidation.Empty)    = return ""
    convertResult (InputValidation.Bad)      = fail "not valid"

