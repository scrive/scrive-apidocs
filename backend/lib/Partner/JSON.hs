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

import Data.Unjson

import InputValidation
import User.Email (Email(..))
import User.Model
import UserGroup.Types
import qualified UserGroup.Internal as I

data UserForUpdate = UserForUpdate {
      ufuId :: Text
    , ufuEmail :: Email
    , ufuFirstName :: Text
    , ufuLastName :: Text
    , ufuPersonalNumber :: Text
    , ufuPhone :: Text
    , ufuCompanyPosition :: Text
    , ufuLang :: Lang
    , ufuHasAcceptedTOS :: Bool
    }

defaultUserForUpdate :: UserForUpdate
defaultUserForUpdate = UserForUpdate { ufuId              = ""
                                     , ufuEmail           = Email ""
                                     , ufuFirstName       = ""
                                     , ufuLastName        = ""
                                     , ufuPersonalNumber  = ""
                                     , ufuPhone           = ""
                                     , ufuCompanyPosition = ""
                                     , ufuLang            = LANG_EN
                                     , ufuHasAcceptedTOS  = False
                                     }

userInfoFromUserForUpdate :: UserForUpdate -> UserInfo
userInfoFromUserForUpdate UserForUpdate {..} = UserInfo
  { userfstname         = ufuFirstName
  , usersndname         = ufuLastName
  , userpersonalnumber  = ufuPersonalNumber
  , usercompanyposition = ufuCompanyPosition
  , userphone           = ufuPhone
  , useremail           = ufuEmail
  }

unjsonUserForUpdate :: UnjsonDef UserForUpdate
unjsonUserForUpdate =
  objectOf
    $    pure defaultUserForUpdate
    <*   fieldReadonly "id" ufuId "The user's ID"
    <**> (    fieldBy "email" ufuEmail "A user's email" (unjsonEmail)
         <**> (pure $ \email ufu -> ufu { ufuEmail = email })
         )
    <**> (    fieldBy "first_name"
                      ufuFirstName
                      "A user's name"
                      (unjsonWithValidationOrEmpty asValidName)
         <**> (pure $ \fname ufu -> ufu { ufuFirstName = fname })
         )
    <**> (    fieldBy "last_name"
                      ufuLastName
                      "A user's last name"
                      (unjsonWithValidationOrEmpty asValidName)
         <**> (pure $ \lname ufu -> ufu { ufuLastName = lname })
         )
    <**> (    fieldBy "personal_number"
                      ufuPersonalNumber
                      "A user's personal number"
                      (unjsonWithValidationOrEmpty $ emptyOK asValidPersonalNumber)
         <**> (pure $ \pnumber ufu -> ufu { ufuPersonalNumber = pnumber })
         )
    <**> (    fieldBy "phone"
                      ufuPhone
                      "A user's phone"
                      (unjsonWithValidationOrEmpty $ emptyOK asValidPhone)
         <**> (pure $ \phone ufu -> ufu { ufuPhone = phone })
         )
    <**> (    fieldBy "company_position"
                      ufuCompanyPosition
                      "A user's company position"
                      (unjsonWithValidationOrEmpty asValidPosition)
         <**> (pure $ \comppos ufu -> ufu { ufuCompanyPosition = comppos })
         )
    <**> (    fieldBy "lang" ufuLang "A user's language" (unjsonLang)
         <**> (pure $ \lang ufu -> ufu { ufuLang = lang })
         )
    <**> (    field "has_accepted_tos" ufuHasAcceptedTOS "Has the user accepted the TOS?"
         <**> (pure $ \tos ufu -> ufu { ufuHasAcceptedTOS = tos })
         )

userToUserForUpdate :: User -> UserForUpdate
userToUserForUpdate user = UserForUpdate
  { ufuId              = showt $ userid user
  , ufuEmail           = useremail . userinfo $ user
  , ufuFirstName       = userfstname . userinfo $ user
  , ufuLastName        = usersndname . userinfo $ user
  , ufuPersonalNumber  = userpersonalnumber . userinfo $ user
  , ufuPhone           = userphone . userinfo $ user
  , ufuCompanyPosition = usercompanyposition . userinfo $ user
  , ufuLang            = lang $ usersettings $ user
  , ufuHasAcceptedTOS  = maybe False (const True) (userhasacceptedtermsofservice user)
  }

unjsonUsersForUpdate :: UnjsonDef [UserForUpdate]
unjsonUsersForUpdate =
  objectOf $ fieldBy "users" identity "List of users" (arrayOf unjsonUserForUpdate)

data UserGroupForUpdate = UserGroupForUpdate
  { uguUserGroupID      :: Text
  , uguUserGroupName    :: Text
  , uguUserGroupNumber  :: Text
  , uguUserGroupAddress :: Text
  , uguUserGroupZip     :: Text
  , uguUserGroupCity    :: Text
  , uguUserGroupCountry :: Text
  } deriving (Show)

defaultUserGroupForUpdate :: UserGroupForUpdate
defaultUserGroupForUpdate = UserGroupForUpdate { uguUserGroupID      = ""
                                               , uguUserGroupName    = ""
                                               , uguUserGroupNumber  = ""
                                               , uguUserGroupAddress = ""
                                               , uguUserGroupZip     = ""
                                               , uguUserGroupCity    = ""
                                               , uguUserGroupCountry = ""
                                               }

unjsonUserGroupForUpdate :: UnjsonDef UserGroupForUpdate
unjsonUserGroupForUpdate =
  objectOf
    $    pure defaultUserGroupForUpdate
    <*   fieldReadonly "id" uguUserGroupID "The company ID"
    <**> (    fieldBy "name"
                      uguUserGroupName
                      "Company name"
                      (unjsonWithValidationOrEmptyText asValidCompanyName)
         <**> (pure $ \cn cfu -> cfu { uguUserGroupName = cn })
         )
    <**> (    fieldBy "number"
                      uguUserGroupNumber
                      "Company number"
                      (unjsonWithValidationOrEmptyText asValidCompanyNumber)
         <**> (pure $ \cnum cfu -> cfu { uguUserGroupNumber = cnum })
         )
    <**> (    fieldBy "address"
                      uguUserGroupAddress
                      "Company address"
                      (unjsonWithValidationOrEmptyText asValidAddress)
         <**> (pure $ \ca cfu -> cfu { uguUserGroupAddress = ca })
         )
    <**> (    field "zip" uguUserGroupZip "Company zip"
         <**> (pure $ \cz cfu -> cfu { uguUserGroupZip = cz })
         )
    <**> (    fieldBy "city"
                      uguUserGroupCity
                      "Company city"
                      (unjsonWithValidationOrEmptyText asValidCity)
         <**> (pure $ \cci cfu -> cfu { uguUserGroupCity = cci })
         )
    <**> (    fieldBy "country"
                      uguUserGroupCountry
                      "Company country"
                      (unjsonWithValidationOrEmptyText asValidCountry)
         <**> (pure $ \cco cfu -> cfu { uguUserGroupCountry = cco })
         )

unjsonUserGroupsForUpdate :: UnjsonDef [UserGroupForUpdate]
unjsonUserGroupsForUpdate = objectOf
  $ fieldBy "companies" identity "List of companies" (arrayOf unjsonUserGroupForUpdate)

-- This is intended for PartnerAPI only. It uses inherited data and the caller
-- does not know about UserGroups or inheritance
userGroupToUserGroupForUpdate :: UserGroupWithParents -> UserGroupForUpdate
userGroupToUserGroupForUpdate ugwp =
  let ug         = ugwpUG ugwp
      ug_address = ugwpAddress ugwp
  in  UserGroupForUpdate { uguUserGroupID      = showt $ ugID ug
                         , uguUserGroupName    = ugName ug
                         , uguUserGroupNumber  = ug_address ^. #ugaCompanyNumber
                         , uguUserGroupAddress = ug_address ^. #ugaAddress
                         , uguUserGroupZip     = ug_address ^. #ugaZip
                         , uguUserGroupCity    = ug_address ^. #ugaCity
                         , uguUserGroupCountry = ug_address ^. #ugaCountry
                         }

-- This is intended for PartnerAPI only. We compare the set values with
-- inherited data and update only if there is any change.
updateUserGroupWithUserGroupForUpdate
  :: UserGroupWithParents -> UserGroupForUpdate -> UserGroup
updateUserGroupWithUserGroupForUpdate ugwp UserGroupForUpdate {..} =
  let ug          = ugwpUG ugwp
      old_address = ugwpAddress ugwp
      new_address = I.UserGroupAddress { ugaCompanyNumber = uguUserGroupNumber
                                       , ugaEntityName    = uguUserGroupName
                                       , ugaAddress       = uguUserGroupAddress
                                       , ugaZip           = uguUserGroupZip
                                       , ugaCity          = uguUserGroupCity
                                       , ugaCountry       = uguUserGroupCountry
                                       }
      -- don't stop inheriting address, unless it has been changed
      updateAddress = case new_address == old_address of
        True  -> identity
        False -> set #ugAddress $ Just new_address
  in  set #ugName uguUserGroupName . updateAddress $ ug

-------------------------------------------------------------------------------
-- Utils                                                                    ---
-------------------------------------------------------------------------------

unjsonLang :: UnjsonDef Lang
unjsonLang = unjsonInvmapR
  ((maybe (fail "value is not valid language code") return) . langFromCode)
  codeFromLang
  unjsonDef

unjsonEmail :: UnjsonDef Email
unjsonEmail = unjsonInvmapR
  ( (maybe (fail "not valid email address") (return . Email))
  . resultToMaybe
  . asValidEmail
  )
  unEmail
  unjsonDef
