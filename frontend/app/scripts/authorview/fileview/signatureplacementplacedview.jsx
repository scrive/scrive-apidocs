define(["legacy_code", "Backbone", "React"], function (legacy_code, Backbone, React) {

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
        left: Math.round(placement.xrel() * parentWidth),
        top: Math.round(placement.yrel() * parentHeight),
        fontSize: placement.fsrel() * parentWidth
      });
      if (this.signaturePlacement !== undefined) {
        this.signaturePlacement.updateSize(placement.wrel() * parentWidth, placement.hrel() * parentHeight);
      }
    }
  },

  render: function () {
    var field = this.model.field();
    var signatory = field.signatory();
    var doc = signatory.document();
    var box = $("<div>");
    var img = $("<img alt=''/>");
    var width = this.model.wrel() * this.$el.parent().width();
    var height = this.model.hrel() * this.$el.parent().height();

    this.$el.empty();

    this.$el.addClass("placedfield");
    this.$el.toggleClass("empty-signature", field.value() === "");

    box.empty();

    if (field.value()) {
      box.css("width", width);
      box.css("height", height);
      img.attr("src", field.value());
      img.css("width", width);
      img.attr("width", width);
      img.css("height", height);
      img.attr("height", height);
      box.append(img);
    } else {
      this.$el.css("display", "none");
    }

    this.$el.append(box);
  }
});

});
