module UserGroup.Types
  ( InvoicingType(..)
  , UserGroup(UserGroup)
  , fetchUserGroup
  , defaultUserGroup
  , defaultChildUserGroup
  , ugInvoicingType
  , ugPaymentPlan
  , ugwpOnlyParents
  , ugwpPaymentPlan
  , ugwpPaymentPlanWithID
  , ugwpSettings
  , ugwpSettingsWithID
  , ugwpAddress
  , ugwpAddressWithID
  , ugwpFeatures
  , ugwpFeaturesWithID
  , ugwpToList
  , ugwpUG
  , ugwpUI
  , ugwpUIWithID
  , ugwpRoot
  , ugwpAddChild
  , ugrFromUG
  , ugwcToList
  , UserGroupID
  , emptyUserGroupID
  , unsafeUserGroupID
  , fromUserGroupID
  , UserGroupSettings(UserGroupSettings)
  , defaultUserGroupSettings
  , UserGroupSSOConfiguration(UserGroupSSOConfiguration)
  , UserGroupAddress(UserGroupAddress)
  , defaultUserGroupAddress
  , UserGroupUI(UserGroupUI)
  , defaultUserGroupUI
  , UserGroupInvoicing(..)
  , UserGroupWithParents
  , UserGroupRoot(UserGroupRoot)
  , ugFromUGRoot
  , UserGroupWithChildren(UserGroupWithChildren)
  , Tag(Tag)
  , SEBankIDSigningProviderOverride(..)
  ) where

import qualified Data.Set as S

import DataRetentionPolicy
import DB
import DigitalSignatureMethod
import FeatureFlags.Model
import Folder.FolderID
import PadApplication.Types
import SMS.Types
import Tag
import UserGroup.FolderListCallsTransition
import UserGroup.Internal
import UserGroup.Types.PaymentPlan

-- INVOICING

ugInvoicingType :: UserGroup -> InvoicingType
ugInvoicingType ug = case ug ^. #invoicing of
  None         -> InvoicingTypeNone
  (BillItem _) -> InvoicingTypeBillItem
  (Invoice  _) -> InvoicingTypeInvoice

ugPaymentPlan :: UserGroup -> Maybe PaymentPlan
ugPaymentPlan ug = case ug ^. #invoicing of
  None           -> Nothing
  (BillItem mpp) -> mpp
  (Invoice  pp ) -> Just pp

-- USER GROUP

defaultUserGroup :: UserGroup
defaultUserGroup = ugFromUGRoot $ UserGroupRoot { id           = emptyUserGroupID
                                                , name         = ""
                                                , homeFolderID = Nothing
                                                , settings     = defaultUserGroupSettings
                                                , paymentPlan  = FreePlan
                                                , address      = defaultUserGroupAddress
                                                , ui           = defaultUserGroupUI
                                                , features     = defaultFeatures FreePlan
                                                , internalTags = S.empty
                                                , externalTags = S.empty
                                                }

defaultChildUserGroup :: UserGroup
defaultChildUserGroup = UserGroup { id            = emptyUserGroupID
                                  , parentGroupID = Nothing
                                  , name          = ""
                                  , homeFolderID  = Nothing
                                  , settings      = Nothing
                                  , invoicing     = None
                                  , address       = Nothing
                                  , ui            = Nothing
                                  , features      = Nothing
                                  , internalTags  = S.empty
                                  , externalTags  = S.empty
                                  }


fetchUserGroup
  :: ( UserGroupID
     , Maybe UserGroupID
     , Text
     , Maybe FolderID
     , Composite UserGroupInvoicing
     , Maybe (Composite UserGroupSettings)
     , Maybe (Composite UserGroupAddress)
     , Maybe (Composite UserGroupUI)
     , Maybe (Composite FeatureFlags)  -- for admins
     , Maybe (Composite FeatureFlags)
     , CompositeArray1 Tag
     , CompositeArray1 Tag
     )  -- for regular users
  -> UserGroup
fetchUserGroup (id, parentGroupID, name, homeFolderID, cinvoicing, cinfos, caddresses, cuis, cAdminFeatureFlags, cRegularFeatureFlags, CompositeArray1 iTags, CompositeArray1 eTags)
  = UserGroup
    { settings     = unComposite <$> cinfos
    , invoicing    = unComposite cinvoicing
    , address      = unComposite <$> caddresses
    , ui           = unComposite <$> cuis
    , features     = Features
      <$> (unComposite <$> cAdminFeatureFlags)
      <*> (unComposite <$> cRegularFeatureFlags)
    , internalTags = S.fromList iTags
    , externalTags = S.fromList eTags
    , ..
    }

-- USER GROUP ROOT

ugrFromUG :: UserGroup -> Maybe UserGroupRoot
ugrFromUG ug = do
  -- the root of usergroup tree must have Invoice, Settings, Address, UI
  -- and Feature Flags
  paymentPlan <- case ug ^. #invoicing of
    None        -> Nothing
    BillItem _  -> Nothing         -- the root of usergroup tree must have:
    Invoice  pp -> Just pp         --   Invoice
  settings <- ug ^. #settings  --   Settings
  address  <- ug ^. #address   --   Address
  features <- ug ^. #features  --   Features
  ui       <- ug ^. #ui        --   UI (aka user group branding)
  return $ UserGroupRoot
    { id           = ug ^. #id
    , name         = ug ^. #name
    , homeFolderID = ug ^. #homeFolderID
    , internalTags = ug ^. #internalTags
    , externalTags = ug ^. #externalTags
    , ..
    }

ugFromUGRoot :: UserGroupRoot -> UserGroup
ugFromUGRoot ugr = UserGroup { id            = ugr ^. #id
                             , name          = ugr ^. #name
                             , homeFolderID  = ugr ^. #homeFolderID
                             , parentGroupID = Nothing
                             , invoicing     = Invoice $ ugr ^. #paymentPlan
                             , settings      = Just $ ugr ^. #settings
                             , address       = Just $ ugr ^. #address
                             , ui            = Just $ ugr ^. #ui
                             , features      = Just $ ugr ^. #features
                             , internalTags  = ugr ^. #internalTags
                             , externalTags  = ugr ^. #externalTags
                             }

-- USER GROUP WITH PARENTS

ugwpOnlyParents :: UserGroupWithParents -> Maybe UserGroupWithParents
ugwpOnlyParents (_   , []              ) = Nothing -- root is leaf, nothing would be left
ugwpOnlyParents (root, _ : parents_tail) = Just (root, parents_tail)

ugwpInherit
  :: (UserGroupRoot -> a)
  -> (UserGroup -> Maybe a)
  -> UserGroupWithParents
  -> (UserGroupID, a)
ugwpInherit ugrProperty ugProperty (ug_root, ug_children_path) =
  fromMaybe (ug_root ^. #id, ugrProperty ug_root)
    . listToMaybe
    . mapMaybe makeIDPropertyTuple
    $ ug_children_path
  where
    makeIDPropertyTuple ug = case ugProperty ug of
      Nothing   -> Nothing
      Just prop -> Just (ug ^. #id, prop)

ugwpPaymentPlan :: UserGroupWithParents -> PaymentPlan
ugwpPaymentPlan = snd . ugwpInherit (^. #paymentPlan) ugPaymentPlan

ugwpPaymentPlanWithID :: UserGroupWithParents -> (UserGroupID, PaymentPlan)
ugwpPaymentPlanWithID = ugwpInherit (^. #paymentPlan) ugPaymentPlan

ugwpSettings :: UserGroupWithParents -> UserGroupSettings
ugwpSettings = snd . ugwpInherit (^. #settings) (^. #settings)

ugwpSettingsWithID :: UserGroupWithParents -> (UserGroupID, UserGroupSettings)
ugwpSettingsWithID = ugwpInherit (^. #settings) (^. #settings)

ugwpUI :: UserGroupWithParents -> UserGroupUI
ugwpUI = snd . ugwpUIWithID

ugwpUIWithID :: UserGroupWithParents -> (UserGroupID, UserGroupUI)
ugwpUIWithID = ugwpInherit (^. #ui) (^. #ui)

ugwpAddress :: UserGroupWithParents -> UserGroupAddress
ugwpAddress = snd . ugwpInherit (^. #address) (^. #address)

ugwpAddressWithID :: UserGroupWithParents -> (UserGroupID, UserGroupAddress)
ugwpAddressWithID = ugwpInherit (^. #address) (^. #address)

ugwpFeatures :: UserGroupWithParents -> Features
ugwpFeatures = snd . ugwpInherit (^. #features) (^. #features)

ugwpFeaturesWithID :: UserGroupWithParents -> (UserGroupID, Features)
ugwpFeaturesWithID = ugwpInherit (^. #features) (^. #features)

ugwpToList :: UserGroupWithParents -> [UserGroup]
ugwpToList (ug_root, ug_children_path) = ug_children_path ++ [ugFromUGRoot ug_root]

ugwpUG :: UserGroupWithParents -> UserGroup
ugwpUG (root, []    ) = ugFromUGRoot root
ugwpUG (_   , ug : _) = ug

ugwpRoot :: UserGroupWithParents -> UserGroup
ugwpRoot (root, _) = ugFromUGRoot root

ugwpAddChild :: UserGroup -> UserGroupWithParents -> UserGroupWithParents
ugwpAddChild ug (root, children_path) = (root, ug : children_path)

ugwcToList :: [UserGroupWithChildren] -> [UserGroup]
ugwcToList ugwcs =
  concat . for ugwcs $ \ugwc -> ugwc ^. #group : ugwcToList (ugwc ^. #children)

-- USER GROUP INFO

defaultUserGroupSettings :: UserGroupSettings
defaultUserGroupSettings = UserGroupSettings
  { ipAddressMaskList          = []
  , dataRetentionPolicy        = defaultDataRetentionPolicy
  , cgiDisplayName             = Nothing
  , cgiServiceID               = Nothing
  , smsProvider                = SMSDefault
  , padAppMode                 = ListView
  , padEarchiveEnabled         = True
  , forceHidePN                = False
  , legalText                  = False
  , requireBPIDForNewDoc       = False
  , sendTimeoutNotification    = False
  , useFolderListCalls         = defaultUseFolderListCalls
  , totpIsMandatory            = False
  , sessionTimeoutSecs         = Nothing
  , portalUrl                  = Nothing
  , eidServiceToken            = Nothing
  , digitalSignatureMethod     = Guardtime
  , documentSessionTimeoutSecs = Nothing
  , hasPostSignview            = True
  , ssoConfig                  = Nothing
  , addMetadataToPDFs          = False
  , eidUseForSEView            = False
  , appFrontend                = False
  , seBankIDSigningOverride    = Nothing
  , padesCredentialsLabel      = Nothing
  }

-- UI

defaultUserGroupUI :: UserGroupUI
defaultUserGroupUI = UserGroupUI { mailTheme     = Nothing
                                 , signviewTheme = Nothing
                                 , serviceTheme  = Nothing
                                 , browserTitle  = Nothing
                                 , smsOriginator = Nothing
                                 , favicon       = Nothing
                                 }

-- ADDRESS

defaultUserGroupAddress :: UserGroupAddress
defaultUserGroupAddress = UserGroupAddress { companyNumber = ""
                                           , entityName    = ""
                                           , address       = ""
                                           , zipCode       = ""
                                           , city          = ""
                                           , country       = ""
                                           }
