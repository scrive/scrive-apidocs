define(['legacy_code'], function() {

window.FieldPlacementGlobal = {};

/* Borders for all placements */
FieldPlacementGlobal.placementBorder = 2;

FieldPlacementGlobal.signatoryCSSClass = function(sig) {
  if (sig)
    return "signatory-field-" + ((sig.participantIndex() -1 ) % 6 + 1);
  else
    return "";
};

FieldPlacementGlobal.updateSignatoryCSSClass = function(el,sig) {
  el.removeClass("signatory-field-1")
    .removeClass("signatory-field-2")
    .removeClass("signatory-field-3")
    .removeClass("signatory-field-4")
    .removeClass("signatory-field-5")
    .removeClass("signatory-field-6");
  if (sig) {
    el.addClass(FieldPlacementGlobal.signatoryCSSClass(sig));
  }
};

/* Margins for text placements. Such placements have some internal margin and we need to adjust it*/
FieldPlacementGlobal.textPlacementHorSpace = 7;
FieldPlacementGlobal.textPlacementExtraLineHeight = 4;
FieldPlacementGlobal.textPlacementVerSpace = 5;
FieldPlacementGlobal.textPlacementSpacingString = FieldPlacementGlobal.textPlacementVerSpace + 'px ' + FieldPlacementGlobal.textPlacementHorSpace + 'px';
FieldPlacementGlobal.textPlacementXOffset = FieldPlacementGlobal.placementBorder + FieldPlacementGlobal.textPlacementHorSpace;
FieldPlacementGlobal.textPlacementYOffset = FieldPlacementGlobal.placementBorder + FieldPlacementGlobal.textPlacementVerSpace + FieldPlacementGlobal.textPlacementExtraLineHeight;

/* Font sizes for text placements */
FieldPlacementGlobal.fontSizeSmall  = 12;
FieldPlacementGlobal.fontSizeNormal = 16;
FieldPlacementGlobal.fontSizeLarge  = 20;
FieldPlacementGlobal.fontSizeHuge   = 24;

/* Margins for checkbox placement. */
FieldPlacementGlobal.checkboxSprite = 10;
FieldPlacementGlobal.checkboxSpriteBorder = 1;
FieldPlacementGlobal.checkboxPlacementMargin = FieldPlacementGlobal.placementBorder + 1;

/* Margins for signature placement. */
FieldPlacementGlobal.signaturePlacementTopMargin = FieldPlacementGlobal.placementBorder + 1;
FieldPlacementGlobal.signaturePlacementLeftMargin = FieldPlacementGlobal.placementBorder + 1;

/* Offsets for type setter placements */
FieldPlacementGlobal.textTypeSetterArrowOffset = 18;
FieldPlacementGlobal.checkboxTypeSetterHorizontalOffset = 32;
FieldPlacementGlobal.checkboxTypeSetterVerticalOffset = -22;
FieldPlacementGlobal.signatureTypeSetterHorizontalOffset = 18;
FieldPlacementGlobal.signatureTypeSetterVerticalOffset = -19;

});
