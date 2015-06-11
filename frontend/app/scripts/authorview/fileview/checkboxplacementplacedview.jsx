define(["Backbone", "React", "legacy_code"], function (Backbone, React) {

return Backbone.View.extend({
  initialize: function (args) {
    this.listenTo(this.model, "change", this.updatePosition);
    this.render();
  },

  updatePosition: function () {
    var placement = this.model;
    var place = $(this.el);
    var parent = place.parent();
    if (parent.length > 0) {
      var parentWidth = parent.width();
      var parentHeight = parent.height();
      place.css({
        left: Math.round(placement.xrel() * parentWidth) - FieldPlacementGlobal.placementBorder,
        top: Math.round(placement.yrel() * parentHeight) - FieldPlacementGlobal.placementBorder,
        width: Math.round(placement.wrel() * parentWidth),
        height: Math.round(placement.hrel() * parentHeight),
        fontSize: placement.fsrel() * parentWidth
      });
    }
  },

  render: function () {
    var field = this.model.field();
    var doc = field.signatory().document();
    var box = $("<div>");

    this.$el.addClass("placedfield");

    box.addClass("placedcheckbox-noactive");
    box.toggleClass("checked", field.value() !== "");

    this.$el.empty();
    this.$el.append(box);
  }
});

});
