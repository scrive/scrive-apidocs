define(["legacy_code", "Backbone", "React", "designview/typesetters/texttypesetterview",
        "designview/fileview/textplacementview", "designview/fileview/draggablefield"],
  function (legacy_code, Backbone, React, TextTypeSetterView, TextPlacementView, draggableField) {

return Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render", "clear", "closeTypeSetter", "updateErrorBackground", "fixWHRel", "listenToField", "updatePosition");
    var view = this;
    var placement = this.model;
    var field =  placement.field();
    var signatory = field ? field.signatory() : placement.signatory();
    var doc = signatory.document();

    this.listenTo(placement, "removed", this.clear);
    this.listenTo(placement, "change:field change:signatory change:withTypeSetter change:fsrel", this.render);
    this.listenTo(placement, "change:xrel change:yrel change:wrel change:hrel", this.updatePosition);
    this.listenTo(placement, "clean", this.closeTypeSetter);
    this.listenTo(placement, "change:field", this.listenToField);
    this.listenTo(placement, "change", this.updateErrorBackground);
    this.listenTo(field, "change:name", this.render);
    this.listenTo(field, "change", this.updateErrorBackground);
    this.listenTo(field, "change", this.fixWHRel);
    this.listenTo(doc, "change:signatories", this.render);


    this.model.view = this;
    this.render();
  },

  firstRender: true,

  fontSize: function () {
    var parent = $(this.el).parent();
    if (parent.length > 0) { return Math.floor(this.model.fsrel() * parent.width()); }
    return 16;
  },

  listenToField: function (model, oldField) {
    var field = model.field();
    this.stopListening(oldField);
    this.listenTo(field, "change:name", this.render);
    this.listenTo(field, "change", this.updateErrorBackground);
    this.listenTo(field, "change", this.fixWHRel);
  },

  updatePosition: function () {
    /*
     * There is a series of these updatePosition functions, all
     * the same.  We need to round position down to nearest
     * integer. We need to ceil size because if we do not then we
     * end up with place not big enough to fit text and it will
     * wrap or be cropped.
     */
    var placement = this.model;
    var place = $(this.el);
    var parent = place.parent();
    if (parent.length > 0) {
      /*
       * We set size only when we have parent. If drag was just
       * started then drag helper does not have parent and will
       * use default size for itself.
       */
      var parentWidth = parent.width();
      var parentHeight = parent.height();
      place.css({
        left: Math.floor(placement.xrel() * parentWidth + 1.5) - FieldPlacementGlobal.textPlacementXOffset,
        top: Math.floor(placement.yrel() * parentHeight + 1.5) - FieldPlacementGlobal.textPlacementYOffset,
        fontSize: Math.floor(placement.fsrel() * parentWidth)
      });
    }
  },

  clear: function () {
    var placement = this.model;
    var field =  placement.field();
    var signatory = field ? field.signatory() : placement.signatory();
    this.off();
    $(this.el).remove();
    this.stopListening();
  },

  shouldFocusEditor: function () {
    return this._focusEditor;
  },

  setShouldFocusEditor: function (bool) {
    this._focusEditor = bool;
  },

  hasTypeSetter: function () {
    return this.model.typeSetter != undefined;
  },

  addTypeSetter: function () {
    var placement = this.model;
    var field = placement.field();
    if (!this.hasTypeSetter() && $.contains(document.body, this.el)) {
      var typeSetterDiv = $("<div />");
      placement.typeSetter = React.render(React.createElement(TextTypeSetterView, {
               model: placement, element: this.el
             }), typeSetterDiv[0]);
      $("body").append(typeSetterDiv);

      placement.typeSetter && placement.typeSetter.place();
      setTimeout(function () {
      }, 0);
    }
  },

  closeTypeSetter: function () {
    var placement = this.model;
    if (this.hasTypeSetter()) {
      placement.typeSetter.clear();
    }
  },

  updateTypeSetter: function () {
    var placement = this.model;
    if (this.hasTypeSetter()) {
      placement.typeSetter.update();
    }
  },

  updateErrorBackground: function () {
    var placement = this.model;
    var field = placement.field();
    var document = field.signatory().document();
    $(this.el).toggleClass("invalid", field && !field.isValid(true));
  },

  editor: function () {
    var view = this;
    var placement = view.model;
    var field = placement.field();
    var className = "text-field-placement-setter-field-editor " +
      FieldPlacementGlobal.signatoryCSSClass(this.model.signatory());

    var input = new InfoTextInput({
      cssClass: className,
      infotext: field.nicename(),
      style: "font-size:" + this.fontSize() + "px ;" +
             "line-height: " + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) +  "px;" +
             "height:" + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px; " +
             "padding:" + FieldPlacementGlobal.textPlacementSpacingString + ";",
      inputStyle: "font-size:" + this.fontSize() + "px ; " +
                   "line-height: " + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px; " +
                   "height:" + (this.fontSize() + FieldPlacementGlobal.textPlacementExtraLineHeight) + "px; " +
                   "vertical-align: top;",
      value: field.value(),
      suppressSpace: (field.name() == "fstname"),
      onChange: function (val) {
        field.setValue(val.trim());
      },
      onEnter: function (val) {
        view.closeTypeSetter();
        view.model.cleanTypeSetter();
        view.render();
      }
    });

    return input;
  },

  fixWHRel: function () {
    // This will silently update wrel and hrel. Should be called after all changes to w/h are applied. Not 100% reliable
    var check = $(this.el).width() && $(this.el).width() > 0 &&
      $(this.el).height() && $(this.el).height() > 0;

    if (check) {
      this.model.fixWHRel($(this.el).width(), $(this.el).height());
    }
  },

  render: function () {
    var self = this;
    var placement = this.model;
    var field =  placement.field();
    var signatory = placement.signatory() || field.signatory();
    var document = signatory.document();
    var place = $(this.el);
    var placewrapper = $("<div class='placedfield-placement-wrapper'>");

    place.addClass("placedfield");
    place.addClass("js-" + field.type());
    this.updateErrorBackground();

    place.css("cursor", "pointer");

    this.updatePosition();

    place.empty();

    if (self.hasTypeSetter() && !field.isCsvField() && !field.isAuthorUnchangeableField()) {
      var editor = this.editor();
      place.append(editor.el());
      if (self.shouldFocusEditor()) {
        editor.focus();
        self.setShouldFocusEditor(false);
      }
      self.updateTypeSetter();
    } else {
      place.append(placewrapper.append(new TextPlacementView({model: field, fontSize: this.fontSize()}).el));
    }

    place.unbind("click");
    if (document.allowsDD()) {
      draggableField(place, placement, undefined, undefined, false, this.fontSize());
      place.click(function () {
        if (!self.hasTypeSetter()) {
          self.addTypeSetter();
          self.setShouldFocusEditor(!field.isCsvField() && !field.isAuthorUnchangeableField());
          self.render();
        }
        return false;
      });
    }

    if (placement.withTypeSetter() && this.firstRender) {
      this.addTypeSetter();
    }

    this.fixWHRel();
    this.firstRender = false;
    return this;
  }
});

});
