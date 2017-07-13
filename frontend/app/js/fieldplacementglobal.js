var FieldPlacementGlobal = require("./fieldplacementglobal.js").FieldPlacementGlobal;


var FieldPlacementGlobal = exports.FieldPlacementGlobal = {};

/* Borders for all placements */
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
FieldPlacementGlobal.textPlacementXOffset = 2 + FieldPlacementGlobal.textPlacementHorSpace;
FieldPlacementGlobal.textPlacementYOffset = 2 + FieldPlacementGlobal.textPlacementVerSpace + FieldPlacementGlobal.textPlacementExtraLineHeight;

/* Font sizes for text placements */
FieldPlacementGlobal.fontSizeSmall  = 12;
FieldPlacementGlobal.fontSizeNormal = 16;
FieldPlacementGlobal.fontSizeLarge  = 20;
FieldPlacementGlobal.fontSizeHuge   = 24;

/* Margins for checkbox placement. */
FieldPlacementGlobal.checkboxSprite = 10;
FieldPlacementGlobal.checkboxSpriteBorder = 1;

FieldPlacementGlobal.checkboxPlacementDDTopOffset  = 1;
FieldPlacementGlobal.checkboxPlacementDDLeftOffset = 1;

/* D&D for signatures needs to be adjusted. */
FieldPlacementGlobal.signaturePlacementDDTopOffset = 1;
FieldPlacementGlobal.signaturePlacementDDLeftOffset = 1;
FieldPlacementGlobal.signatureBorderWidth = 2;

/* Offsets for type setter placements */
FieldPlacementGlobal.textTypeSetterVerticalOffset = -19;
FieldPlacementGlobal.textTypeSetterArrowOffset = 18;
FieldPlacementGlobal.checkboxTypeSetterHorizontalOffset = 19;
FieldPlacementGlobal.checkboxTypeSetterVerticalOffset = -22;
FieldPlacementGlobal.signatureTypeSetterHorizontalOffset = 18;
FieldPlacementGlobal.signatureTypeSetterVerticalOffset = -19;

/* Standard placement sizes */
FieldPlacementGlobal.signatureWidth = 260;
FieldPlacementGlobal.signatureHeight = 102;

/* Checkbox sizes */
FieldPlacementGlobal.smallCheckboxRatio = 0.011538
FieldPlacementGlobal.mediumCheckboxRatio = 0.021153;
FieldPlacementGlobal.largeCheckboxRatio = 0.0423076;
FieldPlacementGlobal.checkboxClickableAreaWidth = 44;
FieldPlacementGlobal.checkboxClickableAreaHeight = 44;


/* Other defaults */
FieldPlacementGlobal.defaultCheckboxWRel = FieldPlacementGlobal.smallCheckboxRatio;
FieldPlacementGlobal.designviewPageWidth = 950;

/* Radiobutton constants */
FieldPlacementGlobal.smallRadiobuttonRatio = 0.014736;
FieldPlacementGlobal.mediumRadiobuttonRatio = 0.021052;
FieldPlacementGlobal.largeRadiobuttonRatio = 0.025263;
FieldPlacementGlobal.defaultRadiobuttonWRel = FieldPlacementGlobal.mediumRadiobuttonRatio;
FieldPlacementGlobal.radioGroupPlacementDDTopOffset  = 0;
FieldPlacementGlobal.radioGroupPlacementDDLeftOffset = 0;
FieldPlacementGlobal.radioGroupBorderWidth = 1;
FieldPlacementGlobal.radioButtonWrapperPadding = 3;
FieldPlacementGlobal.radioButtonClickableAreaWidth = 44;
FieldPlacementGlobal.radioButtonClickableAreaHeight = 44;
