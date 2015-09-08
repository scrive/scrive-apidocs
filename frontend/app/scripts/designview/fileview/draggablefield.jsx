define(["legacy_code", "designview/fileview/signatureplacementviewwithoutplacement",
        "designview/fileview/checkboxplacementview", "designview/fileview/textplacementview",
        "designview/editdocument/placeondocument"],
  function (legacy_code, SignaturePlacementViewWithoutPlacement, CheckboxPlacementView, TextPlacementView,
    placeOnDocument) {

var createFieldPlacementView = function (args) {
  if (args.model.isSignature()) {
    return new SignaturePlacementViewWithoutPlacement(args);
  } else if (args.model.isCheckbox()) {
    return new CheckboxPlacementView(args);
  } else {
    return new TextPlacementView(args);
  }
};

return function (dragHandler, fieldOrPlacementFN, widthFunction, heightFunction,
                 cursorNormalize, fontSize, onFieldAdded, onDragStart) {
  var droppedInside = false;
  var helper;
  var field;
  var placement;
  var verticaloffset = 0;
  var fieldOrPlacement;
  var disabled = false;
  var initFP = function () {
      if (typeof fieldOrPlacementFN === "function") {
        fieldOrPlacement = fieldOrPlacementFN();
      } else {
        fieldOrPlacement = fieldOrPlacementFN;
      }

      if (fieldOrPlacement !== undefined && fieldOrPlacement.field !== undefined) {
        placement = fieldOrPlacement;
        field = placement.field();
      } else {
        placement = undefined;
        field = fieldOrPlacement;
      }

      if (field.isFake()) {
        verticaloffset = -1;
      } else if (field.isText()) {
        verticaloffset = -1;
      } else if (field.isSignature()) {
        verticaloffset = 0;
      }
  };

  dragHandler.mousedown(function () {
    initFP();
    disabled = false;
    if (onDragStart) {
      if (onDragStart(field)) {
        dragHandler.draggable("enable");
        return true;
      } else {
        dragHandler.draggable("disable");
        disabled = true;
        return false;
      }
    }
  });

  var initHelper = function (event) {
    helper = createFieldPlacementView({
      model: field,
      height: heightFunction != undefined ? heightFunction() : undefined,
      width: widthFunction != undefined ? widthFunction() : undefined,
      dragging: true,
      fontSize: fontSize
    }).el;
    return helper;
  };

  if (!dragHandler.hasClass("placedfield")) {
    dragHandler.click(function () {
      if (!disabled) {
        initHelper();
        placeOnDocument(dragHandler, field, onDrop);
      }
    });
  }

  var onDrop = function (page, x, y, w, h) {
    if (field.isText() || field.isFake()) {
      x += FieldPlacementGlobal.textPlacementXOffset;
      y += FieldPlacementGlobal.textPlacementYOffset;
    } else if (field.isCheckbox()) {
      x += FieldPlacementGlobal.checkboxPlacementMargin;
      y += FieldPlacementGlobal.checkboxPlacementMargin;
    } else if (field.isSignature()) {
      x += FieldPlacementGlobal.signaturePlacementLeftMargin;
      y += FieldPlacementGlobal.signaturePlacementTopMargin;
    }
    droppedInside = true;
    var signatory = field.signatory();
    if (!_.find(signatory.fields(), function (f) { return f == field; })) {
      if (onFieldAdded != undefined) {
        onFieldAdded(field);
      }
      signatory.addField(field);
    }

    field.setSignatory(signatory);

    var fontSizeText = $(helper).css("font-size");
    var fontSize = parseFloat(fontSizeText) || 16;

    if (placement != undefined) {
      if (placement.page() == page.number()) {
        placement.set({xrel: x / w,
                        yrel: y / h,
                        wrel: $(helper).width() / w,
                        hrel: $(helper).height() / h
                      });
      }        else {
        /*
         * Placement has been moved from page to another
         * page. For now we just remove and re-add
         * placement. Refactor this later to in place
         * update.
         */
        mixpanel.track("Drag field to new page", {fieldname:field.name(),
                                                  signatory:field.signatory().signIndex(),
                                                  documentid:field.signatory().document().documentid()});
        placement.remove();
        var newPlacement = new FieldPlacement({
          page: page.number(),
          fileid: page.file().fileid(),
          field: field,
          xrel: x / w,
          yrel: y / h,
          wrel: $(helper).width() / w,
          hrel: $(helper).height() / h,
          fsrel: fontSize / w,
          tip: placement.tip(),
          step: placement.step()
        });
        field.addPlacement(newPlacement);
      }
    }      else {
      _.each(field.signatory().document().signatories(), function (s) {
        _.each(s.fields(), function (f) {
          _.each(f.placements(), function (p) {
            if (p.typeSetter != undefined && p.withTypeSetter()) {
                p.typeSetter.clear();
            }
          });
        });
      });
      mixpanel.track("Drag field", {
        documentid:field.signatory().document().documentid()
      });
      var newPlacement = new FieldPlacement({
        page: page.number(),
        fileid: page.file().fileid(),
        field: field,
        xrel: x / w,
        yrel: y / h,
        wrel: $(helper).width() / w,
        hrel: $(helper).height() / h,
        fsrel: fontSize / w,
        withTypeSetter: true,
        step: (field.isFake() ? "signatory" : "edit")
      });
      field.addPlacement(newPlacement);
      signatory.trigger("drag:checkbox");
    }
  };

  dragHandler.draggable({
    appendTo: ".design-view-frame",
    scroll: false,
    cursorAt: cursorNormalize ? {top:7, left:7} : undefined,
    helper: initHelper,
    start: function (event, ui) {
      if ($("html")[0].scrollWidth <= $(window).width()) {
        // there"s no horizontal scrollbar, so dragging away should not create one
        $("html").css("overflow-x", "hidden");
      }
      if (placement != undefined) {
        if (placement.typeSetter != undefined) {
          placement.typeSetter.clear();
          placement.typeSetter = undefined;
        }
      }

      if (dragHandler.hasClass("placedfield")) {
        dragHandler.hide();
      }
      if (field.signatory().document().mainfile() != undefined) {
        var xAxisOffset = 0;
        var yAxisOffset = 0;
        if (field.isText() || field.isFake()) {
          xAxisOffset = FieldPlacementGlobal.textPlacementXOffset;
          yAxisOffset = FieldPlacementGlobal.textPlacementYOffset;
        }
        field.signatory().document().mainfile().view.showCoordinateAxes(ui.helper,
          verticaloffset, xAxisOffset, yAxisOffset);
      }
    },
    stop: function () {
      $("html").css("overflow-x", "auto");
      if (placement != undefined && !droppedInside) {
        placement.remove();
        placement.removeField();
      } else if (dragHandler.hasClass("placedfield")) {
        dragHandler.show();
      }
      if (field.signatory().document().mainfile() != undefined) {
        field.signatory().document().mainfile().view.hideCoordinateAxes();
      }
      droppedInside = false;
    },
    drag: function (event, ui) {
      if (field.signatory().document().mainfile() != undefined) {
        var xAxisOffset = 0;
        var yAxisOffset = 0;
        if (field.isText() || field.isFake()) {
          xAxisOffset = FieldPlacementGlobal.textPlacementXOffset;
          yAxisOffset = FieldPlacementGlobal.textPlacementYOffset;
        }
        field.signatory().document().mainfile().view.showCoordinateAxes(ui.helper,
          verticaloffset, xAxisOffset, yAxisOffset);
        field.signatory().document().mainfile().view.moveCoordinateAxes(ui.helper,
          verticaloffset, xAxisOffset, yAxisOffset);
      }
    },
    onDrop: onDrop
  });
};

});
