define(["legacy_code", "Backbone", "React"], function (legacy_code, Backbone, React) {

return Backbone.View.extend({
  initialize: function (args) {
    this.listenTo(this.model, "change", this.updatePosition);
    this.render();
  },

  fontSize: function () {
    var parent = $(this.el).parent();
    if (parent.length > 0) { return Math.floor(this.model.fsrel() * parent.width()); }
    return 16;
  },

  updatePosition: function () {
    var placement = this.model;
    var place = $(this.el);
    var parent = place.parent();
    if (parent.length > 0) {
      var parentWidth = parent.width();
      var parentHeight = parent.height();
      place.css({
        left: Math.floor(placement.xrel() * parentWidth + 1.5) - FieldPlacementGlobal.textPlacementXOffset,
        top: Math.floor(placement.yrel() * parentHeight + 1.5) - FieldPlacementGlobal.textPlacementYOffset,
        fontSize: Math.floor(placement.fsrel() * parentWidth)
      });
    }
  },

  render: function () {
    var field = this.model.field();
    var signatory = this.model.signatory() || field.signatory();
    var doc = signatory.document();
    var fontSize = this.fontSize();
    var wrapper = $("<div class='placedfield-placement-wrapper'>");
    var box = $("<div>");

    this.$el.addClass("placedfield");

    this.$el.empty();

    box.css("padding", FieldPlacementGlobal.textPlacementSpacingString);

    if (field) {
      box.text(field.nicetext());
    } else {
      box.text("unset field");
    }

    if (fontSize) {
      box.css("font-size", this.fontSize + "px");
      box.css("line-height", this.fontSize + FieldPlacementGlobal.textPlacementExtraLineHeight + "px");
    }

    wrapper.append(box);

    this.$el.append(wrapper);

    if (!field.value()) {
      this.$el.css("display", "none");
    }
  }
});

});
