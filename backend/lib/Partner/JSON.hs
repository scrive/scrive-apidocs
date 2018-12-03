module Partner.JSON (
    UserForUpdate(..)
  , unjsonUserForUpdate
  , userInfoFromUserForUpdate
  , userToUserForUpdate
  , unjsonUsersForUpdate

  , UserGroupForUpdate
  , unjsonUserGroupForUpdate
  , unjsonUserGroupsForUpdate
  , userGroupToUserGroupForUpdate
  , updateUserGroupWithUserGroupForUpdate
  ) where

import Data.Default
import Data.Text (Text, pack, unpack)
import Data.Unjson

import InputValidation
import User.Email (Email(..))
import User.Model
import UserGroup.Data

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
              (unjsonWithValidationOrEmpty $ emptyOK asValidPersonalNumber)
              <**> (pure $ \pnumber ufu -> ufu { ufuPersonalNumber = pnumber }))
        <**> (fieldBy "phone" ufuPhone "A user's phone"
              (unjsonWithValidationOrEmpty $ emptyOK asValidPhone)
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

unjsonUsersForUpdate :: UnjsonDef [UserForUpdate]
unjsonUsersForUpdate = objectOf $
  fieldBy "users"
  id
  "List of users"
  (arrayOf unjsonUserForUpdate)

data UserGroupForUpdate = UserGroupForUpdate
    {
      uguUserGroupID      :: Text
    , uguUserGroupName    :: Text
    , uguUserGroupNumber  :: Text
    , uguUserGroupAddress :: Text
    , uguUserGroupZip     :: Text
    , uguUserGroupCity    :: Text
    , uguUserGroupCountry :: Text
    } deriving (Show)

instance Default UserGroupForUpdate where
    def = UserGroupForUpdate
          {
            uguUserGroupID = ""
          , uguUserGroupName = ""
          , uguUserGroupNumber = ""
          , uguUserGroupAddress = ""
          , uguUserGroupZip = ""
          , uguUserGroupCity = ""
          , uguUserGroupCountry = ""
          }

unjsonUserGroupForUpdate :: UnjsonDef UserGroupForUpdate
unjsonUserGroupForUpdate = objectOf $ pure def
        <*   fieldReadonly "id" uguUserGroupID "The company ID"
        <**> (fieldBy "name" uguUserGroupName "Company name"
              (unjsonWithValidationOrEmptyText (\s -> pack <$> (asValidCompanyName . unpack $ s)))
              <**> (pure $ \cn cfu -> cfu { uguUserGroupName = cn }))
        <**> (fieldBy "number" uguUserGroupNumber "Company number"
              (unjsonWithValidationOrEmptyText (\s -> pack <$> (asValidCompanyNumber . unpack $ s)))
              <**> (pure $ \cnum cfu -> cfu { uguUserGroupNumber = cnum }))
        <**> (fieldBy "address" uguUserGroupAddress "Company address"
              (unjsonWithValidationOrEmptyText (\s -> pack <$> (asValidAddress . unpack $ s)))
              <**> (pure $ \ca cfu -> cfu { uguUserGroupAddress = ca }))
        <**> (field "zip" uguUserGroupZip "Company zip"
              <**> (pure $ \cz cfu -> cfu { uguUserGroupZip = cz }))
        <**> (fieldBy "city" uguUserGroupCity "Company city"
              (unjsonWithValidationOrEmptyText (\s -> pack <$> (asValidCity . unpack $ s)))
              <**> (pure $ \cci cfu -> cfu { uguUserGroupCity = cci }))
        <**> (fieldBy "country" uguUserGroupCountry "Company country"
              (unjsonWithValidationOrEmptyText (\s -> pack <$> (asValidCountry . unpack $ s)))
              <**> (pure $ \cco cfu -> cfu { uguUserGroupCountry = cco }))

unjsonUserGroupsForUpdate :: UnjsonDef [UserGroupForUpdate]
unjsonUserGroupsForUpdate = objectOf $
  fieldBy "companies"
  id
  "List of companies"
  (arrayOf unjsonUserGroupForUpdate)

userGroupToUserGroupForUpdate :: UserGroup -> UserGroupForUpdate
userGroupToUserGroupForUpdate ug
  = UserGroupForUpdate
      {
        uguUserGroupID      = pack . show $ get ugID ug -- unsafeUserGroupID
      , uguUserGroupName    = get ugName ug
      , uguUserGroupNumber  = get ugaCompanyNumber ugAddress'
      , uguUserGroupAddress = get ugaAddress ugAddress'
      , uguUserGroupZip     = get ugaZip ugAddress'
      , uguUserGroupCity    = get ugaCity ugAddress'
      , uguUserGroupCountry = get ugaCountry ugAddress'
      }
  where ugAddress' = get ugAddress ug

updateUserGroupWithUserGroupForUpdate :: UserGroup -> UserGroupForUpdate -> UserGroup
updateUserGroupWithUserGroupForUpdate ug UserGroupForUpdate{..} =
  set ugName uguUserGroupName . set ugAddress muga $ ug
  where muga = set ugaCompanyNumber uguUserGroupNumber .
               set ugaAddress       uguUserGroupAddress .
               set ugaZip           uguUserGroupZip .
               set ugaCity          uguUserGroupCity .
               set ugaCountry       uguUserGroupCountry $
               get ugAddress ug

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

-- @note unify the Text vs. String approach when removing or tidying up old partners API.
unjsonWithValidationOrEmptyText :: (Text -> InputValidation.Result Text) -> UnjsonDef Text
unjsonWithValidationOrEmptyText validation = unjsonInvmapR (convertResult . validation) id unjsonDef
  where
    convertResult (InputValidation.Good s)   = return s
    convertResult (InputValidation.Empty)    = return ""
    convertResult (InputValidation.Bad)      = fail "not valid"
