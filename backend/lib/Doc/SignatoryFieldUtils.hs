module Doc.SignatoryFieldUtils (
      FieldValue(..)
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
    , fieldEditableBySignatory
    , fieldPlacements
    , fieldID
    , fieldRadioGroupValues
    , fieldsAreAlmostEqual
    , fieldsListsAreAlmostEqual
    , fieldTextCustomValidation
  ) where

import Doc.SignatoryFieldID
import Doc.Types.SignatoryField
import File.FileID

data FieldValue
  = StringFV Text
  | BoolFV Bool
  | FileFV (Maybe FileID)
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
fieldTypeFromFieldIdentity (RadioGroupFI _) = RadioGroupFT


-- General utils for fields

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
fieldFileValue (SignatoryRadioGroupField _)     = Nothing

fieldBoolValue :: SignatoryField -> Maybe Bool
fieldBoolValue (SignatoryNameField _)          = Nothing
fieldBoolValue (SignatoryCompanyField _)       = Nothing
fieldBoolValue (SignatoryPersonalNumberField _)= Nothing
fieldBoolValue (SignatoryCompanyNumberField _) = Nothing
fieldBoolValue (SignatoryEmailField _)         = Nothing
fieldBoolValue (SignatoryMobileField _)        = Nothing
fieldBoolValue (SignatoryTextField _)          = Nothing
fieldBoolValue (SignatoryCheckboxField f)      = Just $ schfValue f
fieldBoolValue (SignatorySignatureField _)     = Nothing
fieldBoolValue (SignatoryRadioGroupField _)    = Nothing


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
fieldIsObligatory (SignatoryRadioGroupField _)     = True

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
fieldShouldBeFilledBySender (SignatoryRadioGroupField _)     = False

fieldEditableBySignatory :: SignatoryField -> Maybe Bool
fieldEditableBySignatory (SignatoryEmailField f)    = Just $ sefEditableBySignatory f
fieldEditableBySignatory (SignatoryMobileField f)   = Just $ smfEditableBySignatory f
fieldEditableBySignatory _                          = Nothing

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
fieldPlacements (SignatoryRadioGroupField f)     = srgfPlacements f

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
fieldID (SignatoryRadioGroupField f)     = srgfID f

fieldRadioGroupValues :: SignatoryField -> Maybe [Text]
fieldRadioGroupValues (SignatoryNameField _)           = Nothing
fieldRadioGroupValues (SignatoryCompanyField _)        = Nothing
fieldRadioGroupValues (SignatoryPersonalNumberField _) = Nothing
fieldRadioGroupValues (SignatoryCompanyNumberField _)  = Nothing
fieldRadioGroupValues (SignatoryEmailField _)          = Nothing
fieldRadioGroupValues (SignatoryMobileField _)         = Nothing
fieldRadioGroupValues (SignatoryTextField _)           = Nothing
fieldRadioGroupValues (SignatoryCheckboxField _)       = Nothing
fieldRadioGroupValues (SignatorySignatureField _)      = Nothing
fieldRadioGroupValues (SignatoryRadioGroupField f)     = Just $ srgfValues f

fieldTextCustomValidation :: SignatoryField -> Maybe TextCustomValidation
fieldTextCustomValidation (SignatoryNameField _)           = Nothing
fieldTextCustomValidation (SignatoryCompanyField _)        = Nothing
fieldTextCustomValidation (SignatoryPersonalNumberField _) = Nothing
fieldTextCustomValidation (SignatoryCompanyNumberField _)  = Nothing
fieldTextCustomValidation (SignatoryEmailField _)          = Nothing
fieldTextCustomValidation (SignatoryMobileField _)         = Nothing
fieldTextCustomValidation (SignatoryTextField f)           = stfCustomValidation f
fieldTextCustomValidation (SignatoryCheckboxField _)       = Nothing
fieldTextCustomValidation (SignatorySignatureField _)      = Nothing
fieldTextCustomValidation (SignatoryRadioGroupField _)     = Nothing

-- fieldsAreAlmostEqual compares properties of fields that can be changed by user of scrive system.
-- It ignores fieldID and filledByAuthor properties - since they are set automatically and are not controled by user.
fieldsAreAlmostEqual :: SignatoryField -> SignatoryField -> Bool
fieldsAreAlmostEqual a b = and [
      fieldIdentity a == fieldIdentity b
    , fieldTextValue a == fieldTextValue b
    , fieldFileValue a == fieldFileValue b
    , fieldBoolValue a == fieldBoolValue b
    , fieldIsObligatory a == fieldIsObligatory b
    , fieldShouldBeFilledBySender a == fieldShouldBeFilledBySender b
    , fieldPlacements a == fieldPlacements b
    , fieldRadioGroupValues a == fieldRadioGroupValues b
    ]


--  fieldsAreAlmostEqual for Lists. It will work only for lists that are ordered
fieldsListsAreAlmostEqual :: [SignatoryField] -> [SignatoryField] -> Bool
fieldsListsAreAlmostEqual fs fs' = all id $ zipWith fieldsAreAlmostEqual fs fs'
