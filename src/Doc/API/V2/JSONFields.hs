{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.JSONFields (unjsonSignatoryFields) where


import Doc.DocStateData
import KontraPrelude
import Data.Unjson
import Data.Text
import Control.Applicative.Free
import Doc.API.V2.UnjsonUtils
import Doc.API.V2.JSONMisc()
import Doc.SignatoryFieldID


unjsonSignatoryFields :: UnjsonDef [SignatoryField]
unjsonSignatoryFields = arrayOf unjsonSignatoryField

unjsonSignatoryField :: UnjsonDef SignatoryField
unjsonSignatoryField = disjointUnionOf "type" [
    (fieldTypeToText NameFT, (\f -> fieldType f == NameFT), (SignatoryNameField <$> unjsonNameField))
  , (fieldTypeToText CompanyFT, (\f -> fieldType f == CompanyFT), (SignatoryCompanyField <$> unjsonCompanyField))
  , (fieldTypeToText PersonalNumberFT, (\f -> fieldType f == PersonalNumberFT), (SignatoryPersonalNumberField <$> unjsonPersonalNumberField))
  , (fieldTypeToText CompanyNumberFT, (\f -> fieldType f == CompanyNumberFT), (SignatoryCompanyNumberField <$> unjsonCompanyNumberField))
  , (fieldTypeToText EmailFT, (\f -> fieldType f == EmailFT), (SignatoryEmailField <$> unjsonEmailField))
  , (fieldTypeToText MobileFT, (\f -> fieldType f == MobileFT), (SignatoryMobileField <$> unjsonMobileField))
  , (fieldTypeToText TextFT, (\f -> fieldType f == TextFT), (SignatoryTextField <$> unjsonTextField))
  , (fieldTypeToText CheckboxFT, (\f -> fieldType f == CheckboxFT), (SignatoryCheckboxField <$> unjsonCheckboxField))
  , (fieldTypeToText SignatureFT, (\f -> fieldType f == SignatureFT), (SignatorySignatureField <$> unjsonSignatureField))
  ]


unjsonNameField :: Ap (FieldDef SignatoryField) SignatoryNameField
unjsonNameField = pure (\no v ob sfbs ps -> NameField (unsafeSignatoryFieldID 0) no v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> field "order" (unsafeFromNameField snfNameOrder) "Order of name field"
  <*> fieldDef "value" "" (unsafeFromNameField snfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromNameField snfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromNameField snfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromNameField snfPlacements) "Placements"
  where
    unsafeFromNameField :: (SignatoryNameField -> a) -> SignatoryField -> a
    unsafeFromNameField f (SignatoryNameField a) = f a
    unsafeFromNameField _ _ = $unexpectedError "unsafeFromNameField"

unjsonCompanyField :: Ap (FieldDef SignatoryField) SignatoryCompanyField
unjsonCompanyField = pure (\v ob sfbs ps -> CompanyField (unsafeSignatoryFieldID 0) v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromCompanyField scfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromCompanyField scfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromCompanyField scfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromCompanyField scfPlacements) "Placements"
  where
    unsafeFromCompanyField :: (SignatoryCompanyField -> a) -> SignatoryField -> a
    unsafeFromCompanyField f (SignatoryCompanyField a) = f a
    unsafeFromCompanyField _ _ = $unexpectedError "unsafeFromCompanyField"

unjsonPersonalNumberField :: Ap (FieldDef SignatoryField) SignatoryPersonalNumberField
unjsonPersonalNumberField = pure (\v ob sfbs ps-> PersonalNumberField (unsafeSignatoryFieldID 0) v  ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromPersonalNumberField spnfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromPersonalNumberField spnfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromPersonalNumberField spnfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromPersonalNumberField spnfPlacements) "Placements"
  where
    unsafeFromPersonalNumberField :: (SignatoryPersonalNumberField -> a) -> SignatoryField -> a
    unsafeFromPersonalNumberField f (SignatoryPersonalNumberField a) = f a
    unsafeFromPersonalNumberField _ _ = $unexpectedError "unsafeFromPersonalNumberField"

unjsonCompanyNumberField :: Ap (FieldDef SignatoryField) SignatoryCompanyNumberField
unjsonCompanyNumberField = pure (\v  ob sfbs ps -> CompanyNumberField (unsafeSignatoryFieldID 0) v  ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromCompanyNumberField scnfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromCompanyNumberField scnfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromCompanyNumberField scnfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromCompanyNumberField scnfPlacements) "Placements"
  where
    unsafeFromCompanyNumberField :: (SignatoryCompanyNumberField -> a) -> SignatoryField -> a
    unsafeFromCompanyNumberField f (SignatoryCompanyNumberField a) = f a
    unsafeFromCompanyNumberField _ _ = $unexpectedError "unsafeFromCompanyNumberField"


unjsonEmailField :: Ap (FieldDef SignatoryField) SignatoryEmailField
unjsonEmailField = pure (\v  ob sfbs ps -> EmailField (unsafeSignatoryFieldID 0) v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromEmailField sefValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromEmailField sefObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromEmailField sefShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromEmailField sefPlacements) "Placements"
  where
    unsafeFromEmailField :: (SignatoryEmailField -> a) -> SignatoryField -> a
    unsafeFromEmailField f (SignatoryEmailField a) = f a
    unsafeFromEmailField _ _ = $unexpectedError "unsafeFromEmailField"

unjsonMobileField :: Ap (FieldDef SignatoryField) SignatoryMobileField
unjsonMobileField = pure (\v ob sfbs ps -> MobileField (unsafeSignatoryFieldID 0) v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> fieldDef "value" "" (unsafeFromMobileField smfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromMobileField smfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromMobileField smfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromMobileField smfPlacements) "Placements"

  where
    unsafeFromMobileField :: (SignatoryMobileField -> a) -> SignatoryField -> a
    unsafeFromMobileField f (SignatoryMobileField a) = f a
    unsafeFromMobileField _ _ = $unexpectedError "unsafeFromMobileField"


unjsonTextField :: Ap (FieldDef SignatoryField) SignatoryTextField
unjsonTextField  = pure (\n v  ob sfbs ps -> TextField  (unsafeSignatoryFieldID 0) n (v == "") v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> field "name"  (unsafeFromTextField  stfName) "Name of the field"
  <*> fieldDef "value" "" (unsafeFromTextField  stfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromTextField stfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromTextField stfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromTextField stfPlacements) "Placements"
  where
    unsafeFromTextField  :: (SignatoryTextField  -> a) -> SignatoryField -> a
    unsafeFromTextField  f (SignatoryTextField  a) = f a
    unsafeFromTextField  _ _ = $unexpectedError "unsafeFromTextField "

unjsonCheckboxField :: Ap (FieldDef SignatoryField) SignatoryCheckboxField
unjsonCheckboxField  = pure (\n v ob sfbs ps -> CheckboxField  (unsafeSignatoryFieldID 0) n v ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> field "name"  (unsafeFromCheckboxField  schfName)  "Name of the field"
  <*> fieldDef "is_checked" False (unsafeFromCheckboxField  schfValue) "Value of the field"
  <*> fieldDef "is_obligatory" True (unsafeFromCheckboxField schfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromCheckboxField schfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromCheckboxField schfPlacements) "Placements"
  where
    unsafeFromCheckboxField  :: (SignatoryCheckboxField  -> a) -> SignatoryField -> a
    unsafeFromCheckboxField  f (SignatoryCheckboxField  a) = f a
    unsafeFromCheckboxField  _ _ = $unexpectedError "unsafeFromCheckboxField "

unjsonSignatureField :: Ap (FieldDef SignatoryField) SignatorySignatureField
unjsonSignatureField  = pure (\n ob sfbs ps -> SignatureField  (unsafeSignatoryFieldID 0) n Nothing ob sfbs ps)
  <*  fieldReadonly "type" fieldType "Type of a field"
  <*> field "name"  (unsafeFromSignatureField  ssfName)  "Value of the field"
  <*  fieldReadonlyBy "signature" (unsafeFromSignatureField  ssfValue) "Uploaded file" unjsonDefWithNull
  <*> fieldDef "is_obligatory" True (unsafeFromSignatureField ssfObligatory) "If is oligatory"
  <*> fieldDef "should_be_filled_by_sender" False (unsafeFromSignatureField ssfShouldBeFilledBySender) "If should be filled by sender"
  <*> fieldDef "placements" [] (unsafeFromSignatureField ssfPlacements) "Placements"
  where
    unsafeFromSignatureField  :: (SignatorySignatureField  -> a) -> SignatoryField -> a
    unsafeFromSignatureField  f (SignatorySignatureField  a) = f a
    unsafeFromSignatureField  _ _ = $unexpectedError "unsafeFromSignatureField "

-- Unjson for FieldType. fieldTypeToText on "type" field is used to build a disjointUnion.

instance Unjson FieldType where
  unjsonDef = unjsonEnum "FieldType (Readonly)" (\_ -> Nothing) fieldTypeToText

fieldTypeToText :: FieldType -> Text
fieldTypeToText NameFT = "name"
fieldTypeToText CompanyFT = "company"
fieldTypeToText PersonalNumberFT = "ssn"
fieldTypeToText CompanyNumberFT = "comanyno"
fieldTypeToText EmailFT = "email"
fieldTypeToText MobileFT = "mobile"
fieldTypeToText TextFT = "text"
fieldTypeToText SignatureFT = "signature"
fieldTypeToText CheckboxFT = "checkbox"


-- Unjson for field placements and anchors
instance Unjson FieldPlacement where
  unjsonDef = unsonFieldPlacement

unsonFieldPlacement :: UnjsonDef FieldPlacement
unsonFieldPlacement =  objectOf $ pure FieldPlacement
  <*> field "xrel" placementxrel "Relative x position"
  <*> field "yrel" placementyrel "Relative y position"
  <*> field "wrel" placementwrel "Relative width"
  <*> field "hrel" placementhrel "Relative height"
  <*> field "fsrel" placementfsrel "Relative font size"
  <*> field "page" placementpage "Page of palcement"
  <*> fieldOpt "tip" placementtipside "Should arrow point on field from left or right"
  <*> fieldDef "anchors" [] placementanchors "Should arrow point on field from left or right"

instance Unjson PlacementAnchor where
  unjsonDef = unsonPlacementAnchor

unsonPlacementAnchor :: UnjsonDef PlacementAnchor
unsonPlacementAnchor = objectOf $ pure PlacementAnchor
  <*> field "text" placementanchortext "Text to match with anchor"
  <*> field "index" placementanchorindex "Relative x position"
  <*> fieldOpt "pages" placementanchorpages "Relative x position"

instance Unjson TipSide where
  unjsonDef = unjsonEnumBy "TipSide" [
      (LeftTip, "left")
    , (RightTip, "right")
    ]


