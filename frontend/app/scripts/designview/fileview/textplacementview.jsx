define(["legacy_code", "Backbone", "legacy_code"], function (legacy_code, Backbone) {

return Backbone.View.extend({
  initialize: function (args) {
    _.bindAll(this, "render", "clear");
    var view = this;
    this.fontSize = args.fontSize;
    if (this.model) {
      this.model.bind("removed", this.clear);
    }
    this.dragging = args.dragging;
    this.render();
  },

  clear: function () {
    this.off();
    $(this.el).remove();
    if (this.model) {
      this.model.unbind("removed", this.clear);
    }

  },

  updateSignatoryCSSClass: function () {
    FieldPlacementGlobal.updateSignatoryCSSClass($(this.el), !this.model.isFake() && this.model.signatory());
  },

  render: function () {
    var field = this.model;
    var box = $(this.el);
    box.addClass("placedfieldvalue value");
    box.toggleClass("invalid", field && !field.isValid(true));
    var boxForText = box;
    if (this.dragging) {
      boxForText = $("<div class='placedfield-placement-wrapper-text' />");
      boxForText.css("padding", FieldPlacementGlobal.textPlacementSpacingString);
      box.append($("<div class='placedfield-placement-wrapper' />").append(boxForText));
    } else {
      box.css("padding", FieldPlacementGlobal.textPlacementSpacingString);
    }

    if (field) {
      boxForText.text(field.nicetext());
      field.bind("change", function () {
        boxForText.text(field.nicetext());
      });
    } else {
      boxForText.text("unset field");
    }

    if (this.fontSize) {
      box.css("font-size", this.fontSize + "px");
      box.css("line-height", this.fontSize +
        FieldPlacementGlobal.textPlacementExtraLineHeight + "px");
    }

    this.updateSignatoryCSSClass();
  }
});

});
