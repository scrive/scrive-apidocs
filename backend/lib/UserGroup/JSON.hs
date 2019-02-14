{-# LANGUAGE TemplateHaskell #-}
module UserGroup.JSON (
    unjsonUserGroup
  , unjsonUserGroupSettings
  , unjsonUserGroupInvoicing
  , unjsonUserGroupAddress
  , unjsonUserGroupUI
) where

import Data.ByteString
import Data.Either.Combinators (rightToMaybe)
import Data.Unjson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC

import DataRetentionPolicy
import IPAddress (IPAddressWithMask)
import PadApplication.Types (PadAppMode, padAppModeFromText, padAppModeText)
import SMS.Types (SMSProvider, codeFromSMSProvider, smsProviderFromCode)
import UserGroup.Types
import Utils.TH

unjsonUserGroup :: UnjsonDef UserGroup
unjsonUserGroup = objectOf $ pure defaultUserGroup
  -- I'd like to prevent the possibility of ugid being provided
  -- but this does not error if ugid is provided (it's only ignored)
  <* (fieldReadonly "id" _ugID "UserGroup id")
  <**> (fieldBy "name" _ugName "UserGroup name" unjsonDef
    <**> (pure $ \name ug -> ug { _ugName = name }))
  <**> (fieldOptBy "parent_id" _ugParentGroupID "Parent UserGroup ID" unjsonDef
    <**> (pure $ \name ug -> ug { _ugParentGroupID = name }))
  <**> (fieldOptBy "settings" _ugSettings "UserGroup Settings" unjsonUserGroupSettings
    <**> (pure $ \settings ug -> ug { _ugSettings = settings }))
  <**> (fieldOptBy "address" _ugAddress "UserGroup address" unjsonUserGroupAddress
    <**> (pure $ \address ug -> ug { _ugAddress = address }))
  <**> (fieldBy "ui" _ugUI "UserGroup ui" unjsonUserGroupUI
    <**> (pure $ \ui ug -> ug { _ugUI = ui }))

unjsonUserGroupSettings :: UnjsonDef UserGroupSettings
unjsonUserGroupSettings = objectOf $ pure defaultUserGroupSettings
  <**> (fieldBy "ip_address_mask_list" _ugsIPAddressMaskList "UserGroup settings ip_address_mask_list" unjsonIPAddressWithMask
    <**> (pure $ \ipAddressMaskList ug -> ug { _ugsIPAddressMaskList = ipAddressMaskList }))
  <**> (fieldBy "data_retention_policy" _ugsDataRetentionPolicy "UserGroup settings data_retention_policy" unjsonDataRetentionPolicy
    <**> (pure $ \dataRetentionPolicy ug -> ug { _ugsDataRetentionPolicy = dataRetentionPolicy }))
  <**> (fieldOptBy "cgi_display_name" _ugsCGIDisplayName "UserGroup settings cgi_display_name" unjsonDef
    <**> (pure $ \cgiDisplayName ug -> ug { _ugsCGIDisplayName = cgiDisplayName }))
  <**> (fieldOptBy "cgi_service_id" _ugsCGIServiceID "UserGroup settings cgi_service_id" unjsonDef
    <**> (pure $ \cgiServiceID ug -> ug { _ugsCGIServiceID = cgiServiceID }))
  <**> (fieldBy "sms_provider" _ugsSMSProvider "UserGroup settings sms_provider" unjsonSMSProvider
    <**> (pure $ \smsProvider ug -> ug { _ugsSMSProvider = smsProvider }))
  <**> (fieldBy "pad_app_mode" _ugsPadAppMode "UserGroup settings pad_app_mode" unjsonPadAppMode
    <**> (pure $ \padAppMode ug -> ug { _ugsPadAppMode = padAppMode }))
  <**> (fieldBy "pad_earchive_enabled" _ugsPadEarchiveEnabled "UserGroup settings pad_earchive_enabled" unjsonDef
    <**> (pure $ \padEarchiveEnabled ug -> ug { _ugsPadEarchiveEnabled = padEarchiveEnabled }))
  -- I'd like to prevent the possibility of legalText being provided
  -- but this does not error if legalText is provided (it's only ignored)
  <* (fieldReadonly "legal_text" _ugsLegalText "UserGroup settings legal_text")

-- This still uses the Show/Read instances because it's a kind of number.
-- Any attempt to parse IP addresses would just be a reimplementation of these instances.
unjsonIPAddressWithMask :: UnjsonDef [IPAddressWithMask]
unjsonIPAddressWithMask = arrayOf $ unjsonInvmapR
  ((maybe (fail "Can't parse IPAddressWithMask") return) . maybeRead) show unjsonDef

unjsonSMSProvider :: UnjsonDef SMSProvider
unjsonSMSProvider = unjsonInvmapR
  ((maybe (fail "Can't parse SMSProvider") return) . smsProviderFromCode)
  codeFromSMSProvider
  unjsonDef

unjsonPadAppMode :: UnjsonDef PadAppMode
unjsonPadAppMode = unjsonInvmapR
  ((maybe (fail "Can't parse PadAppMode") return) . padAppModeFromText)
  padAppModeText
  unjsonDef

unjsonUserGroupInvoicing :: UnjsonDef UserGroupInvoicing
unjsonUserGroupInvoicing = disjointUnionOf "invoicing" [
    ("none", $(isConstr 'None), pure None)
  , ("bill_item", $(isConstr 'BillItem), BillItem
    <$> fieldOpt "payment_plan"
        (\(BillItem mPaymentPlan) -> mPaymentPlan)
        "Optional payment plan for the bill item")
  , ("invoice", $(isConstr 'Invoice), Invoice
    <$> field "payment_plan"
        (\(Invoice paymentPlan) -> paymentPlan)
        "Payment plan for the invoice")
  ]

unjsonUserGroupAddress :: UnjsonDef UserGroupAddress
unjsonUserGroupAddress = objectOf $ pure defaultUserGroupAddress
  <**> (fieldBy "company_number" _ugaCompanyNumber "UserGroup address companyNumber" unjsonDef
    <**> (pure $ \companyNumber ug -> ug { _ugaCompanyNumber = companyNumber }))
  <**> (fieldBy "address" _ugaAddress "UserGroup address address" unjsonDef
    <**> (pure $ \address ug -> ug { _ugaAddress = address }))
  <**> (fieldBy "zip" _ugaZip "UserGroup address zip" unjsonDef
    <**> (pure $ \zipCode ug -> ug { _ugaZip = zipCode }))
  <**> (fieldBy "city" _ugaCity "UserGroup address city" unjsonDef
    <**> (pure $ \city ug -> ug { _ugaCity = city }))
  <**> (fieldBy "country" _ugaCountry "UserGroup address country" unjsonDef
    <**> (pure $ \country ug -> ug { _ugaCountry = country }))

unjsonUserGroupUI :: UnjsonDef UserGroupUI
unjsonUserGroupUI = objectOf $ pure defaultUserGroupUI
  <**> (fieldOptBy "mail_theme" _uguiMailTheme "UserGroup UI Mail Theme" unjsonDef
    <**> (pure $ \mailTheme ug -> ug { _uguiMailTheme = mailTheme }))
  <**> (fieldOptBy "signview_theme" _uguiSignviewTheme "UserGroup UI Signview Theme" unjsonDef
    <**> (pure $ \signviewTheme ug -> ug { _uguiSignviewTheme = signviewTheme }))
  <**> (fieldOptBy "service_theme" _uguiServiceTheme "UserGroup UI Service Theme" unjsonDef
    <**> (pure $ \serviceTheme ug -> ug { _uguiServiceTheme = serviceTheme }))
  <**> (fieldOptBy "browser_title" _uguiBrowserTitle "UserGroup UI Browser Title" unjsonDef
    <**> (pure $ \browserTitle ug -> ug { _uguiBrowserTitle = browserTitle }))
  <**> (fieldOptBy "sms_originator" _uguiSmsOriginator "UserGroup UI SMS Originator" unjsonDef
    <**> (pure $ \smsOriginator ug -> ug { _uguiSmsOriginator = smsOriginator }))
  <**> (fieldOptBy "favicon" _uguiFavicon "UserGroup UI Favicon" unjsonFavicon
    <**> (pure $ \favicon ug -> ug { _uguiFavicon = favicon }))

unjsonFavicon :: UnjsonDef ByteString
unjsonFavicon = unjsonInvmapR
  ((maybe (fail "Can't parse favicon ByteString") return) . base64ToMaybeBS)
  (BSC.unpack . Base64.encode)
  unjsonDef
    where
      base64ToMaybeBS :: String -> Maybe ByteString
      base64ToMaybeBS = rightToMaybe . Base64.decode . BSC.pack
