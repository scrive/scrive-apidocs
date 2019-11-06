module Partner.JSON
  ( I.UserForUpdate
  , unjsonUserForUpdate
  , userInfoFromUserForUpdate
  , userToUserForUpdate
  , unjsonUsersForUpdate

  , I.UserGroupForUpdate
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
import qualified Partner.JSON.Internal as I
import qualified UserGroup.Internal as I

defaultUserForUpdate :: I.UserForUpdate
defaultUserForUpdate = I.UserForUpdate { id              = ""
                                       , email           = Email ""
                                       , firstName       = ""
                                       , lastName        = ""
                                       , personalNumber  = ""
                                       , phone           = ""
                                       , companyPosition = ""
                                       , lang            = LANG_EN
                                       , hasAcceptedTOS  = False
                                       }

userInfoFromUserForUpdate :: I.UserForUpdate -> UserInfo
userInfoFromUserForUpdate I.UserForUpdate {..} = UserInfo
  { userfstname         = firstName
  , usersndname         = lastName
  , userpersonalnumber  = personalNumber
  , usercompanyposition = companyPosition
  , userphone           = phone
  , useremail           = email
  }

unjsonUserForUpdate :: UnjsonDef I.UserForUpdate
unjsonUserForUpdate =
  objectOf
    $    pure defaultUserForUpdate
    <*   fieldReadonly "id" (^. #id) "The user's ID"
    <**> (fieldBy "email" (^. #email) "A user's email" unjsonEmail <**> pure (#email .~))
    <**> (    fieldBy "first_name"
                      (^. #firstName)
                      "A user's name"
                      (unjsonWithValidationOrEmpty asValidName)
         <**> pure (#firstName .~)
         )
    <**> (    fieldBy "last_name"
                      (^. #lastName)
                      "A user's last name"
                      (unjsonWithValidationOrEmpty asValidName)
         <**> pure (#lastName .~)
         )
    <**> (    fieldBy "personal_number"
                      (^. #personalNumber)
                      "A user's personal number"
                      (unjsonWithValidationOrEmpty $ emptyOK asValidPersonalNumber)
         <**> pure (#personalNumber .~)
         )
    <**> (    fieldBy "phone"
                      (^. #phone)
                      "A user's phone"
                      (unjsonWithValidationOrEmpty $ emptyOK asValidPhone)
         <**> pure (#phone .~)
         )
    <**> (    fieldBy "company_position"
                      (^. #companyPosition)
                      "A user's company position"
                      (unjsonWithValidationOrEmpty asValidPosition)
         <**> pure (#companyPosition .~)
         )
    <**> (fieldBy "lang" (^. #lang) "A user's language" (unjsonLang) <**> pure (#lang .~))
    <**> (field "has_accepted_tos" (^. #hasAcceptedTOS) "Has the user accepted the TOS?"
         <**> pure (#hasAcceptedTOS .~)
         )

userToUserForUpdate :: User -> I.UserForUpdate
userToUserForUpdate user = I.UserForUpdate
  { id              = showt $ userid user
  , email           = useremail . userinfo $ user
  , firstName       = userfstname . userinfo $ user
  , lastName        = usersndname . userinfo $ user
  , personalNumber  = userpersonalnumber . userinfo $ user
  , phone           = userphone . userinfo $ user
  , companyPosition = usercompanyposition . userinfo $ user
  , lang            = lang $ usersettings $ user
  , hasAcceptedTOS  = maybe False (const True) (userhasacceptedtermsofservice user)
  }

unjsonUsersForUpdate :: UnjsonDef [I.UserForUpdate]
unjsonUsersForUpdate =
  objectOf $ fieldBy "users" identity "List of users" (arrayOf unjsonUserForUpdate)

----------------------------------------

defaultUserGroupForUpdate :: I.UserGroupForUpdate
defaultUserGroupForUpdate = I.UserGroupForUpdate { id            = ""
                                                 , entityName    = ""
                                                 , companyNumber = ""
                                                 , address       = ""
                                                 , zipCode       = ""
                                                 , city          = ""
                                                 , country       = ""
                                                 }

unjsonUserGroupForUpdate :: UnjsonDef I.UserGroupForUpdate
unjsonUserGroupForUpdate =
  objectOf
    $    pure defaultUserGroupForUpdate
    <*   fieldReadonly "id" (^. #id) "The company ID"
    <**> (    fieldBy "name"
                      (^. #entityName)
                      "Company name"
                      (unjsonWithValidationOrEmptyText asValidCompanyName)
         <**> pure (#entityName .~)
         )
    <**> (    fieldBy "number"
                      (^. #companyNumber)
                      "Company number"
                      (unjsonWithValidationOrEmptyText asValidCompanyNumber)
         <**> pure (#companyNumber .~)
         )
    <**> (    fieldBy "address"
                      (^. #address)
                      "Company address"
                      (unjsonWithValidationOrEmptyText asValidAddress)
         <**> pure (#address .~)
         )
    <**> (field "zip" (^. #zipCode) "Company zip" <**> pure (#zipCode .~))
    <**> (    fieldBy "city"
                      (^. #city)
                      "Company city"
                      (unjsonWithValidationOrEmptyText asValidCity)
         <**> pure (#city .~)
         )
    <**> (    fieldBy "country"
                      (^. #country)
                      "Company country"
                      (unjsonWithValidationOrEmptyText asValidCountry)
         <**> pure (#country .~)
         )

unjsonUserGroupsForUpdate :: UnjsonDef [I.UserGroupForUpdate]
unjsonUserGroupsForUpdate = objectOf
  $ fieldBy "companies" identity "List of companies" (arrayOf unjsonUserGroupForUpdate)

-- This is intended for PartnerAPI only. It uses inherited data and the caller
-- does not know about UserGroups or inheritance
userGroupToUserGroupForUpdate :: UserGroupWithParents -> I.UserGroupForUpdate
userGroupToUserGroupForUpdate ugwp =
  let ug         = ugwpUG ugwp
      ug_address = ugwpAddress ugwp
  in  I.UserGroupForUpdate { id            = showt $ ug ^. #id
                           , entityName    = ug ^. #name
                           , companyNumber = ug_address ^. #companyNumber
                           , address       = ug_address ^. #address
                           , zipCode       = ug_address ^. #zipCode
                           , city          = ug_address ^. #city
                           , country       = ug_address ^. #country
                           }

-- This is intended for PartnerAPI only. We compare the set values with
-- inherited data and update only if there is any change.
updateUserGroupWithUserGroupForUpdate
  :: UserGroupWithParents -> I.UserGroupForUpdate -> UserGroup
updateUserGroupWithUserGroupForUpdate ugwp I.UserGroupForUpdate {..} =
  let ug          = ugwpUG ugwp
      old_address = ugwpAddress ugwp
      new_address = I.UserGroupAddress { companyNumber = companyNumber
                                       , entityName    = entityName
                                       , address       = address
                                       , zipCode       = zipCode
                                       , city          = city
                                       , country       = country
                                       }
      -- don't stop inheriting address, unless it has been changed
      updateAddress = case new_address == old_address of
        True  -> identity
        False -> set #address $ Just new_address
  in  set #name entityName . updateAddress $ ug

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
