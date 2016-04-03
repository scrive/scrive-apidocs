module Doc.Data.SignatoryFieldUtils (
      FieldValue(..)
    , FieldIdentity(..)
    , getTextValueOfField
    , getFieldByIdentity
    , fieldType
    , fieldIdentity
    , fieldTypeFromFieldIdentity
    , fieldTextValue
    , fieldFileValue
    , fieldBoolValue
    , fieldIsObligatory
    , fieldShouldBeFilledBySender
    , fieldPlacements
    , fieldID
    , fieldsAreAlmoustEqual
    , fieldsListsAreAlmoustEqual
  ) where

import Doc.Data.SignatoryField
import Doc.SignatoryFieldID
import File.FileID
import KontraPrelude

data FieldValue
  = StringFV String
  | BoolFV Bool
  | FileFV (Maybe FileID)
    deriving (Eq, Ord, Show)

data FieldIdentity
  = NameFI NameOrder
  | CompanyFI
  | PersonalNumberFI
  | CompanyNumberFI
  | EmailFI
  | MobileFI
  | TextFI String
  | SignatureFI String
  | CheckboxFI String
    deriving (Eq, Ord, Show)

fieldType :: SignatoryField -> FieldType
fieldType = fieldTypeFromFieldIdentity . fieldIdentity

fieldTypeFromFieldIdentity :: FieldIdentity -> FieldType
fieldTypeFromFieldIdentity (NameFI _)       = NameFT
fieldTypeFromFieldIdentity CompanyFI        = CompanyFT
fieldTypeFromFieldIdentity PersonalNumberFI = PersonalNumberFT
fieldTypeFromFieldIdentity CompanyNumberFI  = CompanyNumberFT
fieldTypeFromFieldIdentity EmailFI          = EmailFT
fieldTypeFromFieldIdentity MobileFI         = MobileFT
fieldTypeFromFieldIdentity (TextFI _)       = TextFT
fieldTypeFromFieldIdentity (SignatureFI _)  = SignatureFT
fieldTypeFromFieldIdentity (CheckboxFI _)   = CheckboxFT

fieldIdentity :: SignatoryField -> FieldIdentity
fieldIdentity (SignatoryNameField f)           = NameFI (snfNameOrder f)
fieldIdentity (SignatoryCompanyField _)        = CompanyFI
fieldIdentity (SignatoryPersonalNumberField _) = PersonalNumberFI
fieldIdentity (SignatoryCompanyNumberField _)  = CompanyNumberFI
fieldIdentity (SignatoryEmailField _)          = EmailFI
fieldIdentity (SignatoryMobileField _)         = MobileFI
fieldIdentity (SignatoryTextField f)           = TextFI (stfName f)
fieldIdentity (SignatoryCheckboxField f)       = CheckboxFI (schfName f)
fieldIdentity (SignatorySignatureField f)      = SignatureFI (ssfName f)

getFieldByIdentity :: FieldIdentity -> [SignatoryField] -> Maybe SignatoryField
getFieldByIdentity fi sfs = find (\sf -> fieldIdentity sf == fi) sfs


-- General utils for fields
getTextValueOfField ::  FieldIdentity -> [SignatoryField] -> String
getTextValueOfField fi sfs =
  case (getFieldByIdentity fi sfs) of
    Just f  -> fromMaybe "" (fieldTextValue f)
    Nothing -> ""

fieldTextValue :: SignatoryField -> Maybe String
fieldTextValue (SignatoryNameField f)           = Just $ snfValue f
fieldTextValue (SignatoryCompanyField f)        = Just $ scfValue f
fieldTextValue (SignatoryPersonalNumberField f) = Just $ spnfValue f
fieldTextValue (SignatoryCompanyNumberField f)  = Just $ scnfValue f
fieldTextValue (SignatoryEmailField f)          = Just $ sefValue f
fieldTextValue (SignatoryMobileField f)         = Just $ smfValue f
fieldTextValue (SignatoryTextField f)           = Just $ stfValue f
fieldTextValue (SignatoryCheckboxField _)       = Nothing
fieldTextValue (SignatorySignatureField _)      = Nothing

fieldFileValue :: SignatoryField -> Maybe FileID
fieldFileValue (SignatoryNameField _)           = Nothing
fieldFileValue (SignatoryCompanyField _)        = Nothing
fieldFileValue (SignatoryPersonalNumberField _) = Nothing
fieldFileValue (SignatoryCompanyNumberField _)  = Nothing
fieldFileValue (SignatoryEmailField _)          = Nothing
fieldFileValue (SignatoryMobileField _)         = Nothing
fieldFileValue (SignatoryTextField _)           = Nothing
fieldFileValue (SignatoryCheckboxField _)       = Nothing
fieldFileValue (SignatorySignatureField f)      = ssfValue f

fieldBoolValue :: SignatoryField -> Maybe Bool
fieldBoolValue (SignatoryNameField _)           = Nothing
fieldBoolValue (SignatoryCompanyField _)        = Nothing
fieldBoolValue (SignatoryPersonalNumberField _) = Nothing
fieldBoolValue (SignatoryCompanyNumberField _)  = Nothing
fieldBoolValue (SignatoryEmailField _)          = Nothing
fieldBoolValue (SignatoryMobileField _)         = Nothing
fieldBoolValue (SignatoryTextField _)           = Nothing
fieldBoolValue (SignatoryCheckboxField f)       = Just $ schfValue f
fieldBoolValue (SignatorySignatureField _)      = Nothing


fieldIsObligatory :: SignatoryField -> Bool
fieldIsObligatory (SignatoryNameField f)           = snfObligatory f
fieldIsObligatory (SignatoryCompanyField f)        = scfObligatory f
fieldIsObligatory (SignatoryPersonalNumberField f) = spnfObligatory f
fieldIsObligatory (SignatoryCompanyNumberField f)  = scnfObligatory f
fieldIsObligatory (SignatoryEmailField f)          = sefObligatory f
fieldIsObligatory (SignatoryMobileField f)         = smfObligatory f
fieldIsObligatory (SignatoryTextField f)           = stfObligatory f
fieldIsObligatory (SignatoryCheckboxField f)       = schfObligatory f
fieldIsObligatory (SignatorySignatureField f)      = ssfObligatory f

fieldShouldBeFilledBySender :: SignatoryField -> Bool
fieldShouldBeFilledBySender (SignatoryNameField f)           = snfShouldBeFilledBySender f
fieldShouldBeFilledBySender (SignatoryCompanyField f)        = scfShouldBeFilledBySender f
fieldShouldBeFilledBySender (SignatoryPersonalNumberField f) = spnfShouldBeFilledBySender f
fieldShouldBeFilledBySender (SignatoryCompanyNumberField f)  = scnfShouldBeFilledBySender f
fieldShouldBeFilledBySender (SignatoryEmailField f)          = sefShouldBeFilledBySender f
fieldShouldBeFilledBySender (SignatoryMobileField f)         = smfShouldBeFilledBySender f
fieldShouldBeFilledBySender (SignatoryTextField f)           = stfShouldBeFilledBySender f
fieldShouldBeFilledBySender (SignatoryCheckboxField f)       = schfShouldBeFilledBySender f
fieldShouldBeFilledBySender (SignatorySignatureField f)      = ssfShouldBeFilledBySender f

fieldPlacements :: SignatoryField -> [FieldPlacement]
fieldPlacements (SignatoryNameField f)           = snfPlacements f
fieldPlacements (SignatoryCompanyField f)        = scfPlacements f
fieldPlacements (SignatoryPersonalNumberField f) = spnfPlacements f
fieldPlacements (SignatoryCompanyNumberField f)  = scnfPlacements f
fieldPlacements (SignatoryEmailField f)          = sefPlacements f
fieldPlacements (SignatoryMobileField f)         = smfPlacements f
fieldPlacements (SignatoryTextField f)           = stfPlacements f
fieldPlacements (SignatoryCheckboxField f)       = schfPlacements f
fieldPlacements (SignatorySignatureField f)      = ssfPlacements f

fieldID :: SignatoryField -> SignatoryFieldID
fieldID (SignatoryNameField f)           = snfID f
fieldID (SignatoryCompanyField f)        = scfID f
fieldID (SignatoryPersonalNumberField f) = spnfID f
fieldID (SignatoryCompanyNumberField f)  = scnfID f
fieldID (SignatoryEmailField f)          = sefID f
fieldID (SignatoryMobileField f)         = smfID f
fieldID (SignatoryTextField f)           = stfID f
fieldID (SignatoryCheckboxField f)       = schfID f
fieldID (SignatorySignatureField f)      = ssfID f

-- fieldsAreAlmoustEqual compares properties of fields that can be changed by user of scrive system.
-- It ignores fieldID and filledByAuthor properties - since they are set automatically and are not controled by user.
fieldsAreAlmoustEqual :: SignatoryField -> SignatoryField -> Bool
fieldsAreAlmoustEqual a b = and [
      fieldIdentity a == fieldIdentity b
    , fieldTextValue a == fieldTextValue b
    , fieldFileValue a == fieldFileValue b
    , fieldBoolValue a == fieldBoolValue b
    , fieldIsObligatory a == fieldIsObligatory b
    , fieldShouldBeFilledBySender a == fieldShouldBeFilledBySender b
    , fieldPlacements a == fieldPlacements b
    ]


--  fieldsAreAlmoustEqual for Lists. It will work only for lists that are ordered
fieldsListsAreAlmoustEqual :: [SignatoryField] -> [SignatoryField] -> Bool
fieldsListsAreAlmoustEqual (f:fs) (f':fs') = fieldsAreAlmoustEqual f f' && fieldsListsAreAlmoustEqual fs fs'
fieldsListsAreAlmoustEqual [] [] = True
fieldsListsAreAlmoustEqual _ _  = False
